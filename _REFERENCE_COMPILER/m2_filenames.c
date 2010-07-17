/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_filenames.c
 *  Portable filename handling implementation
 *
 *  Author: Benjamin Kowarsch
 *
 *  Copyright (C) 2010 B.Kowarsch. All rights reserved.
 *
 *  License:
 *
 *  Permission is hereby granted to review and test this software for the sole
 *  purpose of supporting the effort by the licensor  to implement a reference
 *  compiler for  Modula-2 R10.  It is not permissible under any circumstances
 *  to  use the software  for the purpose  of creating derivative languages or 
 *  dialects.  This permission is valid until 31 December 2010, 24:00h GMT.
 *
 *  Future licensing:
 *
 *  The licensor undertakes  to release  this software  under a BSD-style open
 *  source license  AFTER  the M2R10 language definition has been finalised.
 *  
 */

// TO DO: Add code to check well formedness of directory strings

#include "m2_filenames.h"

#include "ASCII.h"
#include "hash.h"
#include "alloc.h"
#include <limits.h>
#include <unistd.h>


// ---------------------------------------------------------------------------
// Maximum pathname length
// ---------------------------------------------------------------------------
// On some operating systems,  MAX_PATH may not be defined.

#ifdef MAX_PATH // in limits.h
#define M2_MAX_PATH_LENGTH MAX_PATH
#elif
#warning MAX_PATH is undefined, using default value of 511
#define M2_MAX_PATH_LENGTH 511
#endif


// ---------------------------------------------------------------------------
// Maximum filename length
// ---------------------------------------------------------------------------
// On some operating systems,  MAX_NAME may not be defined.

#ifdef MAX_NAME // in limits.h
#define M2_MAX_FILENAME_LENGTH MAX_NAME
#elif
#warning MAX_NAME is undefined, using default value of 127
#define M2_MAX_FILENAME_LENGTH 127
#endif


// ---------------------------------------------------------------------------
// Check for presence of getcwd() or _getcwd()
// ---------------------------------------------------------------------------
// On some operating systems,  getcwd() may be called _getcwd() instead 

#if !defined(getcwd) && defined(_getcwd)
#define getcwd _getcwd
#elif
#error function getcwd() is undefined, this should have been in unistd.h
#endif


// ---------------------------------------------------------------------------
// Default filenaming scheme
// ---------------------------------------------------------------------------
//
// If any of  ISO9660, POSIX, WINDOWS or WIN, MSDOS or DOS, OPENVMS or VMS are
// defined  in the  compile  time  environment  (usually via CFLAGS)  then the
// default filenaming is set accordingly.  If  none of the symbols are defined
// then the default filenaming is set to POSIX_FILENAMING.

static const m2_filenaming_t _default_filenaming =
#if defined(ISO9660)
    ISO9660_FILENAMING ;
#elif defined(POSIX)
    POSIX_FILENAMING ;
#elif defined(WINDOWS) || defined(WIN)
    MSDOS_FILENAMING ;
#elif defined(MSDOS) || defined(DOS)
    MSDOS_FILENAMING ;
#elif defined(OPENVMS) || defined(VMS)
    OPENVMS_FILENAMING ;
#else
#warning OS environment undefined, default filenaming set to POSIX_FILENAMING
    POSIX_FILENAMING ;
#endif


// ---------------------------------------------------------------------------
// Hash values of filename extensions for input files
// ---------------------------------------------------------------------------

#define M2_EXT_DEF_UPPER  0x21C12F85 /* DEF */
#define M2_EXT_DEF_CAMEL  0x21E13785 /* Def */
#define M2_EXT_DEF_LOWER  0x31A327A5 /* def */
#define M2_EXT_MOD_UPPER  0x2639BD82 /* MOD */
#define M2_EXT_MOD_CAMEL  0x2659C582 /* Mod */
#define M2_EXT_MOD_LOWER  0x361BB5A2 /* mod */


// ---------------------------------------------------------------------------
// File extension strings
// ---------------------------------------------------------------------------

static const char *_ext_string[] = {
    // dummy entry
    "\0",
    // source file extensions
    "def\0", "mod\0",
    // output file extensions
    "ast\0", "sym\0", "h\0", "c\0", "llvm\0"
}; /* _ext_string */

#define _MAX_EXTSTR_SIZE 5


// ---------------------------------------------------------------------------
// Delimiter set type
// ---------------------------------------------------------------------------

typedef struct /* delimiter_set */ {
    char for_filename;
    char for_file_extension;
    char for_file_version;
} delimiter_set;


// ---------------------------------------------------------------------------
// Delimiter sets
// ---------------------------------------------------------------------------

static const delimiter_set _delimiter[] = {
    // dummy entry
    { CSTRING_TERMINATOR, CSTRING_TERMINATOR, CSTRING_TERMINATOR },
    // delimiters for POSIX
    { '/', '.', CSTRING_TERMINATOR },
    // delimiters for ISO9660
    { '/', '.', ';' },
    // delimiters for MSDOS
    { '\\', '.', CSTRING_TERMINATOR },
    // delimiters for OPENVMS
    { ']', '.', ';' }
}; /* _delimiter */


// ---------------------------------------------------------------------------
// Filename descriptor
// ---------------------------------------------------------------------------

typedef struct /* m2_filename_s */ {
    char *directory;
    char *filename;
    char *extension;
    cardinal path_length;
    m2_file_type_t file_type;
    m2_filenaming_t filenaming;
} m2_filename_s;


// ---------------------------------------------------------------------------
// Buffer for pathname of working directory
// ---------------------------------------------------------------------------

static char *_working_directory[M2_MAX_PATH_LENGTH + 1] = EMPTY_STRING;


// ---------------------------------------------------------------------------
// function:  m2_working_directory()
// ---------------------------------------------------------------------------
//
// Obtains the  current working directory pathname  of the current process and
// returns an  immutable pointer  to the pathname string.  Subsequent calls to
// this function will always return the  same string  as the working directory
// is not supposed to change during the process' lifetime.

const char *m2_working_directory() {
    if (_working_directory[0] == CSTRING_TERMINATOR) {
        return
            (const char *) getcwd(_working_directory, M2_MAX_PATH_LENGTH + 1);
    }
    else {
        return _working_directory;
    } // end if
} // end m2_working_directory


// ---------------------------------------------------------------------------
// function:  m2_is_valid_filename_string( filename )
// ---------------------------------------------------------------------------
//
// Returns true  if <filename> is a valid Modula-2 filename,  returns false if
// it is not.  Modula-2 filenames must always be valid Modula-2 identifiers.
//
// filename := ( letter | "_" | "$" ) ( letter | digit | "_" | "$" )*

bool m2_is_valid_filename_string(const char *filename) {
    int index;

    if ((IS_NOT_LETTER(filename[0])) &&
        (filename[0] != UNDERSCORE) && (filename[0] != DOLLAR))
        return false;
    
    index = 1;
    while (filename[index] != CSTRING_TERMINATOR) {
        if ((IS_NOT_ALPHANUM(filename[index])) &&
            (filename[0] != UNDERSCORE) && (filename[0] != DOLLAR))
            return false;
        index++;
    } // end while
        
    return true;
} // end m2_is_valid_filename_string


// --------------------------------------------------------------------------
// function:  m2_outfile_type_for( target, sourcefile_type )
// --------------------------------------------------------------------------
//
// Returns  the  output  file  type  associated  with  <sourcefile_type>  for
// <target>.  Returns FILE_TYPE_UNKNOWN if no associated file type exists.

m2_file_type_t m2_outfile_type_for(m2_target_t target,
                                m2_file_type_t sourcefile_type) {
    switch (target) {
            
        // when using C99 source code generator
        case M2_TARGET_C99 :
            switch(sourcefile_type) {
                case FILE_TYPE_MOD :
                    return FILE_TYPE_C;
                case FILE_TYPE_DEF :
                    return FILE_TYPE_H;
                default :
                    return FILE_TYPE_UNKNOWN;
            } // end switch
            
        // when using LLVM source code generator
        case M2_TARGET_LLVM :
            switch(sourcefile_type) {
                case FILE_TYPE_MOD :
                    return FILE_TYPE_LLVM;
                case FILE_TYPE_DEF :
                    return FILE_TYPE_SYM;
                default :
                    return FILE_TYPE_UNKNOWN;
            } // end switch
            
            // no other targets are available
        default :
            return FILE_TYPE_UNKNOWN;
    } // end switch
} // end m2_outfile_type_for


// ---------------------------------------------------------------------------
// function:  m2_new_filename( dir, name, file_type, filenaming, status )
// ---------------------------------------------------------------------------
//
// Returns a newly allocated filename descriptor,  initialised with the passed
// in parameters.  The caller is responsible  for deallocating the descriptor.
// Returns NULL if  any  of <filename> and <file_type> is invalid.
//
// The status of the operation  is passed back in <status>,  unless  NULL  was
// passed in for <status>.

m2_filename_t *m2_new_filename(const char *directory,
                               const char *filename,
                           m2_file_type_t file_type,
                          m2_filenaming_t filenaming,
                     m2_filename_status_t *status) {
    m2_filename_s *new_filename;
    int size, index = 0, total_length = 0;
    char *fn_delimiter, *ext_delimiter, *ver_delimiter;
    
    // bail out if filename is invalid
    if ((filename == NULL) ||
        (m2_is_valid_filename_string(filename) == false)) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_FILENAME);
        return NULL;
    } // end if
    
    // bail out if file type is invalid
    if ((file_type == FILE_TYPE_UNKNOWN) ||
        (file_type >= INVALID_FILENAME)) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_FILETYPE);
        return NULL;
    } // end if
    
    // use default if filenaming value is not specific or invalid
    if ((filenaming == DEFAULT_FILENAMING) ||
        (filenaming >= NUMBER_OF_FILENAMING_SCHEMES))
        filenaming = _default_filenaming;
    
    // determine delimiters
    fn_delimiter = _delimiter[filenaming].for_filename;
    ver_delimiter = _delimiter[filenaming].for_file_version;
    ext_delimiter = _delimiter[filenaming].for_file_extension;
    
    // allocate memory for filename descriptor
    new_filename = ALLOCATE(sizeof(m2_filename_t));
    
    // bail out if allocation failed
    if (new_filename == NULL) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // determine the allocation size of the directory string
    size = 0;
    while (directory[size] != CSTRING_TERMINATOR)
        size++;
    
    // allocate memory for directory string
    new_filename->directory = ALLOCATE(size);
    
    // bail out if allocation failed
    if (new_filename->directory == NULL) {
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // copy the directory string
    index = 0;
    while (index < size) {
        new_filename->directory[index] = directory[index];
        index++;
    } // end while
        
    // accumulated length
    total_length = index;
    
    // terminate the directory string
    new_filename->directory[index] = CSTRING_TERMINATOR;
    
    
    // determine the allocation size of the filename string
    size = 0;
    while (filename[size] != CSTRING_TERMINATOR)
        size++;
    
    // allocate memory for the filename string
    new_filename->filename = ALLOCATE(size);
    
    // bail out if allocation failed
    if (new_filename->filename == NULL) {
        DEALLOCATE(new_filename->directory);
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // copy the filename string
    index = 0;
    while (index < size) {
        new_filename->filename[index] = filename[index];
        index++;
    } // end while
    
    // accumulated length
    total_length = total_length + index;
    
    // terminate the filename string
    new_filename->filename[index] = CSTRING_TERMINATOR;

    // allocate memory for the file extension string
    new_filename->extension = ALLOCATE(_MAX_EXTSTR_SIZE);
    
    // bail out if allocation failed
    if (new_filename->extension == NULL) {
        DEALLOCATE(new_filename->directory);
        DEALLOCATE(new_filename->filename);
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // copy the file extension string
    index = 0;
    while (_ext_string[file_type][index] != CSTRING_TERMINATOR) {
        new_filename->extension[index] = _ext_string[file_type][index];
        index++;
    } // end while
    
    // accumulated length
    total_length = total_length + index;

    // terminate the file extension string
    new_filename->extension[index] = CSTRING_TERMINATOR;
    
    // calculate and set the path name length
    new_filename->path_length = total_length + 1;
    
    // set the file type and filenaming scheme
    new_filename->file_type = file_type;
    new_filename->filenaming = filenaming;
    
    ASSIGN_BY_REF(status, M2_FILENAME_STATUS_SUCCESS);
    return (m2_filename_t) new_filename;
} // end m2_new_filename


// ---------------------------------------------------------------------------
// function:  m2_new_filename_from_path( path, filenaming )
// ---------------------------------------------------------------------------
//
// Returns a newly allocated filename descriptor,  initialised from path using
// filenaming scheme <filenaming>.  The caller is responsible for deallocating
// the descriptor.  The function tests the path for the presence of input file
// extensions and sets the filename descriptor's file_type field as follows:
//
// o  if  the  filename  consists  of nothing but the file extension  then the
//    descriptor's file_type field is set to INVALID_FILENAME.
//
// o  if the file extension is "DEF",  "Def"  or "def"  then  the descriptor's
//    file_type field is set to FILE_TYPE_DEF.
//
// o  if the file extension is "MOD",  "Mod"  or "mod"  then  the descriptor's
//    file_type field is set to FILE_TYPE_MOD.
//
// o  if the file extension matches none of the above file extensions then the
//    descriptor's file_type field is set to FILE_TYPE_UNKNOWN.
//
// For  any missing component  in <path>  an empty string  is  assigned to the
// string field representing  the missing component in the descriptor.  If all
// components are missing in <path>,  no descriptor will be created  and  NULL
// will be returned.
//
// The status of the operation  is passed back in <status>,  unless  NULL  was
// passed in for <status>.

m2_filename_t m2_new_filename_from_path(const char *path,
                                   m2_filenaming_t filenaming,
                              m2_filename_status_t *status) {
    m2_filename_s *new_filename;
    int index, path_index, fn_index, ext_index;
    int end_of_path, length, total_length;
    cardinal ext_hash = HASH_INITIAL;
    char *fn_delimiter, *ext_delimiter, *ver_delimiter;
    
    // bail out if there is no path string
    if ((path == NULL) || (path[0] == CSTRING_TERMINATOR)) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_PATHNAME);
        return NULL;
    } // end if

    // use default if filenaming value is not specific or invalid
    if ((filenaming == DEFAULT_FILENAMING) ||
        (filenaming >= NUMBER_OF_FILENAMING_SCHEMES))
        filenaming = _default_filenaming;
    
    // determine delimiters
    fn_delimiter = _delimiter[filenaming].for_filename;
    ver_delimiter = _delimiter[filenaming].for_file_version;
    ext_delimiter = _delimiter[filenaming].for_file_extension;
        
    // find end of path
    index = 0;
    fn_index = -1;
    ext_index = -1;
    while ((path[index] != CSTRING_TERMINATOR) &&
           (path[index] != ver_delimiter)) {
        if (path[index] == fn_delimiter)
            fn_index = index;
        if (path[index] == ext_delimiter)
            ext_index = index;
        index++;
    } // end while
    index--;
    end_of_path = index;
    
    if (end_of_path >= M2_MAX_PATH_LENGTH) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_PATHNAME_TOO_LONG);
        return NULL;
    } // end if
    
    // disregard ext delimiters in directory
    if (ext_index < fn_index)
        ext_index = -1;
    
    // length of directory string
    length = fn_index + 1;
    total_length = length;
    
    // allocate memory for directory string
    new_filename->directory = ALLOCATE(length + 1);
    
    // bail out if allocation failed
    if (new_filename->directory == NULL) {
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
        
    index = 0;
    if (length > 0) {
        // copy the directory string
        while (index < size) {
            new_filename->directory[index] = path[index];
            index++;
        } // end while
    } // end if
    
    // terminate the directory string
    new_filename->directory[index] = CSTRING_TERMINATOR;
    
    
    // length of filename string
    if (ext_index < 0) {
        length = end_of_path - fn_index;
    }
    else {
        length = ext_index - fn_index - 1;
    } // end if
    total_length = total_length + length;

    
    // allocate memory for filename string
    new_filename->filename = ALLOCATE(length + 1);
    
    // bail out if allocation failed
    if (new_filename->filename == NULL) {
        DEALLOCATE(new_filename->directory);
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    index = 0;
    path_index = fn_index + 1;
    if (length > 0) {
        // copy the filename string
        while (index < size) {
            new_filename->filename[index] = path[path_index];
            index++;
            path_index++;
        } // end while
    } // end if
    
    // terminate the filename string
    new_filename->filename[index] = CSTRING_TERMINATOR;
    
    // length of file extension string
    if (ext_index < 0) {
        length = 0;
    }
    else {
        length = end_of_path - ext_index - 1;
    } // end if
    total_length = total_length + length;
    
    // allocate memory for file extension string
    new_filename->extension = ALLOCATE(length + 1);
    
    // bail out if allocation failed
    if (new_filename->filename == NULL) {
        DEALLOCATE(new_filename->directory);
        DEALLOCATE(new_filename->filename);
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    index = 0;
    path_index = ext_index + 1;
    if (length > 0) {
        // copy the file extension string
        while (index < length) {
            new_filename->extension[index] = path[path_index];
            ext_hash = HASH_NEXT_CHAR(ext_hash, path[path_index]);
            index++;
            path_index++;
        } // end while
        ext_hash = HASH_FINAL(ext_hash);
    } // end if
    
    // terminate the file extension string
    new_filename->extension[index] = CSTRING_TERMINATOR;
    
    // calculate and set the path name length
    new_filename->path_length = total_length + 1;
    
    // determine and set the file type
    if ((new_filename->filename[0] == CSTRING_TERMINATOR) ||
        (new_filename->extension[0] == CSTRING_TERMINATOR))
        new_filename->file_type = INVALID_FILENAME;
    else if ((ext_hash == M2_EXT_DEF_UPPER) ||
             (ext_hash == M2_EXT_DEF_CAMEL) ||
             (ext_hash == M2_EXT_DEF_LOWER)) {
        new_filename->file_type = FILE_TYPE_DEF;
    }
    else if ((ext_hash == M2_EXT_MOD_UPPER) ||
             (ext_hash == M2_EXT_MOD_CAMEL) ||
             (ext_hash == M2_EXT_MOD_LOWER)) {
        new_filename->file_type = FILE_TYPE_MOD;
    }
    else {
        new_filename->file_type = FILE_TYPE_UNKNOWN;
    } // end if
    
    // set the filenaming scheme
    new_filename->filenaming = filenaming;
    
    ASSIGN_BY_REF(status, M2_FILENAME_STATUS_SUCCESS);
    return (m2_filename_t) new_filename;
} // end m2_new_filename_from_path


// ---------------------------------------------------------------------------
// function:  m2_directory_string( filename )
// ---------------------------------------------------------------------------
//
// Returns  an  immutable pointer  to the  directory string  field of filename
// descriptor <filename>.  Returns NULL if the descriptor is NULL.

const char *m2_directory_string(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return NULL;
    
    return (const char *) this_filename->directory;
} // end m2_directory_string


// ---------------------------------------------------------------------------
// function:  m2_filename_string( filename )
// ---------------------------------------------------------------------------
//
// Returns  an  immutable pointer  to the  filename string  field  of filename
// descriptor <filename>.  Returns NULL if the descriptor is NULL.

const char *m2_filename_string(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return NULL;
    
    return (const char *) this_filename->filename;
} // end m2_filename_string


// ---------------------------------------------------------------------------
// function:  m2_file_ext_string( filename )
// ---------------------------------------------------------------------------
//
// Returns  an  immutable pointer  to the  file_ext string  field  of filename
// descriptor <filename>.  Returns NULL if the descriptor is NULL.

const char *m2_file_ext_string(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return NULL;
    
    return (const char *) this_filename->extension;
} // end m2_file_ext_string


// ---------------------------------------------------------------------------
// function:  m2_file_type( filename )
// ---------------------------------------------------------------------------
//
// Returns the value of the file_type field of filename descriptor <filename>.
// Returns FILE_TYPE_UNKNOWN if the descriptor is NULL.

m2_file_type_t m2_file_type(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return FILE_TYPE_UNKNOWN;
    
    return this_filename->file_type;
} // end m2_file_type


// ---------------------------------------------------------------------------
// function:  m2_filenaming( filename )
// ---------------------------------------------------------------------------
//
// Returns the  filenaming scheme  of filename descriptor <filename>.  Returns
// DEFAULT_FILENAMING if the descriptor is NULL.

m2_filenaming_t m2_filenaming(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    if (filename == NULL)
        return DEFAULT_FILENAMING;
    
    return this_filename->filenaming;
} // end m2_filenaming


// ---------------------------------------------------------------------------
// function:  m2_path_from_filename( filename, status )
// ---------------------------------------------------------------------------
//
// Returns a newly allocated C string with a pathname composed from the fields
// in   filename   descriptor   <filename>.  The  caller  is  responsible  for
// deallocating the returned C string.
//
// The status of the operation  is passed back in <status>,  unless  NULL  was
// passed in for <status>.

const char m2_path_from_filename(m2_filename_t filename,
                          m2_filename_status_t *status) {
    
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    int index, path_index = 0;
    char *path;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_REFERENCE);
        return NULL;
    } // end if
    
    // allocate memory for path string
    path = ALLOCATE(this_filename->path_length + 1);
    
    // bail out if allocation failed
    if (path = NULL) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // copy the directory string
    index = 0;
    while (this_filename->directory[index] != CSTRING_TERMINATOR) {
        path[path_index] = this_filename->directory[index];
        path_index++;
        index++;
    } // end while;

    // append the filename string
    index = 0;
    while (this_filename->filename[index] != CSTRING_TERMINATOR) {
        path[path_index] = this_filename->filename[index];
        path_index++;
        index++;
    } // end while;
    
    // append the file extension delimiter
    path[path_index] =
        _delimiter[this_filename->filenaming].for_file_extension;
    path_index++;
    
    // append the file extension string
    index = 0;
    while (this_filename->extension[index] != CSTRING_TERMINATOR) {
        path[path_index] = this_filename->extension[index];
        path_index++;
        index++;
    } // end while;
    
    // terminate the path string
    path[path_index] = CSTRING_TERMINATOR;
    
    ASSIGN_BY_REF(status, M2_FILENAME_STATUS_SUCCESS);
    return (const char *) path;
} // end m2_path_from_filename


// ---------------------------------------------------------------------------
// function:  m2_dispose_filename( filename )
// ---------------------------------------------------------------------------
//
// Deallocates filename descriptor <filename> and all of its components.

void m2_dispose_filename(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return;

    DEALLOCATE(this_filename->directory);
    DEALLOCATE(this_filename->filename);
    DEALLOCATE(this_filename->extension);
    DEALLOCATE(this_filename);
} // end m2_dispose_filename


// END OF FILE
