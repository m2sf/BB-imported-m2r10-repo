/*! Modula-2 R10 Compiler (m2r10c)
 *
 *  @file  m2_filenames.c
 *  @brief Portable filename handling implementation
 *
 *  @b Author: Benjamin Kowarsch
 *
 *  @b Copyright: (C) 2010 B.Kowarsch. All rights reserved.
 *
 *  @b License:
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
// On some operating systems PATH_MAX may not be defined.

#ifdef PATH_MAX // in limits.h
#define M2_MAX_PATH_LENGTH PATH_MAX
#else
#warning PATH_MAX is undefined, using default value of 511
#define M2_MAX_PATH_LENGTH 511
#endif


// ---------------------------------------------------------------------------
// Maximum filename length
// ---------------------------------------------------------------------------
// On some operating systems NAME_MAX may not be defined.

#ifdef NAME_MAX // in limits.h
#define M2_MAX_FILENAME_LENGTH NAME_MAX
#else
#warning NAME_MAX is undefined, using default value of 127
#define M2_MAX_FILENAME_LENGTH 127
#endif


// ---------------------------------------------------------------------------
// Check for presence of getcwd() or _getcwd()
// ---------------------------------------------------------------------------
// On some operating systems,  getcwd() may be called _getcwd() instead 

// FIX ME

#if false
#if !defined(getcwd) && defined(_getcwd)
#define getcwd _getcwd
#else
#error function getcwd() is undefined, this should have been in unistd.h
#endif
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

#define DELIMITER_LENGTH 1

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
    uint16_t path_length;
    uint16_t directory_length;
    uint16_t filename_length;
    uint16_t extension_length;
    m2_file_type_t file_type;
    m2_filenaming_t filenaming;
} m2_filename_s;


// ---------------------------------------------------------------------------
// Buffer for pathname of working directory
// ---------------------------------------------------------------------------

static char _working_directory[M2_MAX_PATH_LENGTH + 1] = EMPTY_STRING;


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
        (filename[0] != UNDERSCORE) && (filename[0] != DOLLAR)) {
        return false;
    } // end if
    
    index = 1;
    while (filename[index] != CSTRING_TERMINATOR) {
        if ((IS_NOT_ALPHANUM(filename[index])) &&
            (filename[index] != UNDERSCORE) && (filename[index] != DOLLAR)) {
            return false;
        } // end if
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

m2_filename_t m2_new_filename(const char *directory,
                              const char *filename,
                              m2_file_type_t file_type,
                              m2_filenaming_t filenaming,
                              m2_filename_status_t *status) {
    m2_filename_s *new_filename;
    int size, index = 0, total_length = 0;
    char fn_delimiter, ext_delimiter, ver_delimiter;
    
    // bail out if directory is invalid
    if (directory == NULL) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_DIRECTORY);
        return NULL;
    } // end if
    
    // bail out if filename is invalid
    if (filename == NULL) {
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
        (filenaming >= NUMBER_OF_FILENAMING_SCHEMES)) {
        filenaming = _default_filenaming;
    } // end if
    
    // determine delimiters
    fn_delimiter = _delimiter[filenaming].for_filename;
    ver_delimiter = _delimiter[filenaming].for_file_version;
    ext_delimiter = _delimiter[filenaming].for_file_extension;
    
    // allocate memory for filename descriptor
    new_filename = ALLOCATE(sizeof(m2_filename_s));
    
    // bail out if allocation failed
    if (new_filename == NULL) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // determine the allocation size of the directory string
    size = 0;
    while (directory[size] != CSTRING_TERMINATOR)
        size++;
    
    // remember directory string length
    new_filename->directory_length = size;
    
    // allocate memory for directory string
    new_filename->directory = ALLOCATE(size + 2);
    
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
    
    // add a trailing delimiter if a directory without one is present
    if ((index != 0) && (directory[index - 1] != fn_delimiter)) {
        new_filename->directory[index] = fn_delimiter;
        new_filename->directory_length++;
        index++;
    }
    
    // accumulated length
    total_length = index;
    
    // terminate the directory string
    new_filename->directory[index] = CSTRING_TERMINATOR;
    
    
    // determine the allocation size of the filename string
    size = 0;
    while ((filename[size] != CSTRING_TERMINATOR) &&
           (filename[size] != ver_delimiter))
        size++;
    
    // remember filename string length
    new_filename->filename_length = size;
    
    // allocate memory for the filename string
    new_filename->filename = ALLOCATE(size + 1);
    
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
    
    // bail out if filename is invalid
    if (m2_is_valid_filename_string(new_filename->filename) == false) {
        DEALLOCATE(new_filename->directory);
        DEALLOCATE(new_filename->filename);
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_FILENAME);
        return NULL;
    } // end if
    
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
    
    // remember file extension string length
    new_filename->extension_length = index;
    
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
// function:  m2_new_filename_from_filename( filename, file_type, status )
// ---------------------------------------------------------------------------
//
// Returns a newly allocated filename descriptor whose file extension and file
// type are determined by <file_type> and whose remaining paramters are copied
// from <filename>.  The caller is responsible for  deallocating  the returned
// descriptor.  Returns NULL if any of <filename> and <file_type> is invalid.
//
// The status of the operation  is passed back in <status>,  unless  NULL  was
// passed in for <status>.

m2_filename_t m2_new_filename_from_filename(m2_filename_t filename,
                                            m2_file_type_t file_type,
                                            m2_filename_status_t *status) {
    m2_filename_s *new_filename;
    m2_filename_s *from_filename = (m2_filename_s *) filename;
    int size, index = 0;
    char ch;
    
    // bail out if filename is invalid
    if (filename == NULL) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_REFERENCE);
        return NULL;
    } // end if
    
    // bail out if file type is invalid
    if ((file_type == FILE_TYPE_UNKNOWN) ||
        (file_type >= INVALID_FILENAME)) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_FILETYPE);
        return NULL;
    } // end if
    
    // allocate memory for filename descriptor
    new_filename = ALLOCATE(sizeof(m2_filename_s));
    
    // bail out if allocation failed
    if (new_filename == NULL) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // determine allocation size of directory string
    size = from_filename->directory_length;
    
    // allocate memory for directory string
    new_filename->directory = ALLOCATE(size + 2);
    
    // bail out if allocation failed
    if (new_filename->directory == NULL) {
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // copy the directory string
    index = 0;
    repeat {
        ch = from_filename->directory[index];
        new_filename->directory[index] = ch;
        index++;
    } until (ch == CSTRING_TERMINATOR);
    
    // determine the allocation size of the filename string
    size = from_filename->filename_length;
    
    // allocate memory for the filename string
    new_filename->filename = ALLOCATE(size + 1);
    
    // bail out if allocation failed
    if (new_filename->filename == NULL) {
        DEALLOCATE(new_filename->directory);
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
    // copy the filename string
    index = 0;
    repeat {
        ch = from_filename->filename[index];
        new_filename->filename[index] = ch;
        index++;
    } until (ch == CSTRING_TERMINATOR);
    
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
    
    // remember file extension string length
    new_filename->extension_length = index;
    
    // calculate and set the path name length
    new_filename->path_length = from_filename->path_length -
                                from_filename->extension_length +
                                new_filename->extension_length;
    
    // set the file type and filenaming scheme
    new_filename->file_type = file_type;
    new_filename->filenaming = from_filename->filenaming;
    
    ASSIGN_BY_REF(status, M2_FILENAME_STATUS_SUCCESS);
    return (m2_filename_t) new_filename;
} // end m2_new_filename_from_filename


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
    char fn_delimiter, ext_delimiter, ver_delimiter;
    
    // bail out if there is no path string
    if ((path == NULL) || (path[0] == CSTRING_TERMINATOR)) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_PATHNAME);
        return NULL;
    } // end if

    // use default if filenaming value is not specific or invalid
    if ((filenaming == DEFAULT_FILENAMING) ||
        (filenaming >= NUMBER_OF_FILENAMING_SCHEMES)) {
        filenaming = _default_filenaming;
    } // end if
    
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
    
    // allocate memory for filename descriptor
    new_filename = ALLOCATE(sizeof(m2_filename_s));
    
    // bail out if allocation failed
    if (new_filename == NULL) {
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_ALLOCATION_FAILED);
        return NULL;
    } // end if
    
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
    // copy the directory string
    while (index < length) {
        new_filename->directory[index] = path[index];
        index++;
    } // end while
    
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
    
    // copy the filename string
    index = 0;
    path_index = fn_index + 1;
    while (index < length) {
        new_filename->filename[index] = path[path_index];
        index++;
        path_index++;
    } // end while
    
    // terminate the filename string
    new_filename->filename[index] = CSTRING_TERMINATOR;
    
    // bail out if filename is invalid
    if ((length > 0) &&
        (m2_is_valid_filename_string(new_filename->filename) == false)) {
        DEALLOCATE(new_filename->directory);
        DEALLOCATE(new_filename->filename);
        DEALLOCATE(new_filename);
        ASSIGN_BY_REF(status, M2_FILENAME_STATUS_INVALID_FILENAME);
        return NULL;
    } // end if
    
    // length of file extension string
    if (ext_index < 0) {
        length = 0;
    }
    else {
        length = end_of_path - ext_index;
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
        (new_filename->extension[0] == CSTRING_TERMINATOR)) {
        new_filename->file_type = INVALID_FILENAME;
    }
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
// function:  m2_path_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns  the  length  of the  full path  of filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_path_string_length(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return 0;
    
    return (cardinal) this_filename->path_length;
} // m2_path_string_length


// ---------------------------------------------------------------------------
// function:  m2_copy_path_string( filename, target )
// ---------------------------------------------------------------------------
//
// Copies the full path of filename descriptor <filename>  to the char pointer
// passed in <target>.  The full path consists of the directory path, filename
// and file extension.  It is  the responsibility  of the caller  to make sure
// that <target> points to a memory buffer  large enough to hold the full path
// including its null terminator.
//
// If NULL is passed in <filename> a null terminator is copied to <target>.
// If NULL is passed in <target> the function returns without action.

void m2_copy_path_string(m2_filename_t filename, char *target) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    cardinal index, target_index;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL) {
        target[0] = CSTRING_TERMINATOR;
        return;
    } // end if
    
    target_index = 0;

    // copy the directory string
    index = 0;
    while (this_filename->directory[index] != CSTRING_TERMINATOR) {
        target[target_index] = this_filename->directory[index];
        target_index++;
        index++;
    } // end while;
    
    // append the basename string
    index = 0;
    while (this_filename->filename[index] != CSTRING_TERMINATOR) {
        target[target_index] = this_filename->filename[index];
        target_index++;
        index++;
    } // end while;
    
    // append the file extension delimiter
    target[target_index] =
    _delimiter[this_filename->filenaming].for_file_extension;
    target_index++;
    
    // append the file extension string
    index = 0;
    while (this_filename->extension[index] != CSTRING_TERMINATOR) {
        target[target_index] = this_filename->extension[index];
        target_index++;
        index++;
    } // end while;
    
    // terminate the string
    target[target_index] = CSTRING_TERMINATOR;
    
    return;
} // end m2_copy_path_string


// ---------------------------------------------------------------------------
// function:  m2_directory_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns the length of the directory path of filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_directory_string_length(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return 0;
    
    return (cardinal) this_filename->directory_length;
} // m2_directory_string_length


// ---------------------------------------------------------------------------
// function:  m2_copy_directory_string( filename, target )
// ---------------------------------------------------------------------------
//
// Copies  the  directory path  of filename descriptor <filename>  to the char
// pointer  passed in <target>.  The  directory  path  consists  of  the  path
// without filename and extension.  It is the responsibility of the caller  to
// make sure  that <target> points to a memory buffer large enough to hold the
// directory path including its null terminator.
//
// If NULL is passed in <filename> a null terminator is copied to <target>.
// If NULL is passed in <target> the function returns without action.

void m2_copy_directory_string(m2_filename_t filename, char *target) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    cardinal index;

    // bail out if filename descriptor is NULL
    if (filename == NULL) {
        target[0] = CSTRING_TERMINATOR;
        return;
    } // end if
    
    // copy the directory string
    index = 0;
    while (this_filename->directory[index] != CSTRING_TERMINATOR) {
        target[index] = this_filename->directory[index];
        index++;
    } // end while;
    
    // terminate the string
    target[index] = CSTRING_TERMINATOR;
    
    return;
} // end m2_copy_directory_string


// ---------------------------------------------------------------------------
// function:  m2_filename_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns the length  of the full filename of filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_filename_string_length(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    cardinal length;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return 0;
    
    length = this_filename->filename_length +
             this_filename->extension_length + DELIMITER_LENGTH;

    return length;
} // m2_filename_string_length


// ---------------------------------------------------------------------------
// function:  m2_copy_filename_string( filename, target )
// ---------------------------------------------------------------------------
//
// Copies  the  full filename  of filename descriptor <filename>  to  the char
// pointer passed in <target>.  The  full filename  consists  of  the basename
// followed by the file extension delimiter and the file extension.  It is the
// responsibility of the caller to make sure  that <target> points to a memory
// buffer large enough to hold the filename including its null terminator.
//
// If NULL is passed in <filename> a null terminator is copied to <target>.
// If NULL is passed in <target> the function returns without action.

void m2_copy_filename_string(m2_filename_t filename, char *target) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    cardinal index, target_index;

    // bail out if filename descriptor is NULL
    if (filename == NULL) {
        target[0] = CSTRING_TERMINATOR;
        return;
    } // end if
    
    target_index = 0;
    
    // copy the basename string
    index = 0;
    while (this_filename->filename[index] != CSTRING_TERMINATOR) {
        target[target_index] = this_filename->filename[index];
        target_index++;
        index++;
    } // end while;
    
    // append the file extension delimiter
    target[target_index] =
        _delimiter[this_filename->filenaming].for_file_extension;
    target_index++;
    
    // append the file extension string
    index = 0;
    while (this_filename->extension[index] != CSTRING_TERMINATOR) {
        target[target_index] = this_filename->extension[index];
        target_index++;
        index++;
    } // end while;

    // terminate the string
    target[target_index] = CSTRING_TERMINATOR;
    
    return;
} // end m2_copy_filename_string


// ---------------------------------------------------------------------------
// function:  m2_basename_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns  the  length  of the  basename  of  filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_basename_string_length(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return 0;
    
    return (cardinal) this_filename->filename_length;
} // m2_basename_string_length


// ---------------------------------------------------------------------------
// function:  m2_copy_basename_string( filename, target )
// ---------------------------------------------------------------------------
//
// Copies the  basename of filename descriptor <filename>  to the char pointer
// passed in <target>.  The  basename  consists  of the  filename  without the
// directory path,  without file extension  and without delimiters.  It is the
// responsibility of the caller to make sure  that <target> points to a memory
// buffer large enough to hold the basename including its null terminator.
//
// If NULL is passed in <filename> a null terminator is copied to <target>.
// If NULL is passed in <target> the function returns without action.

void m2_copy_basename_string(m2_filename_t filename, char *target) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    cardinal index;

    // bail out if filename descriptor is NULL
    if (filename == NULL) {
        target[0] = CSTRING_TERMINATOR;
        return;
    } // end if

    // copy the basename string
    index = 0;
    while (this_filename->filename[index] != CSTRING_TERMINATOR) {
        target[index] = this_filename->filename[index];
        index++;
    } // end while;
    
    // terminate the string
    target[index] = CSTRING_TERMINATOR;
    
    return;
} // end m2_copy_basename_string


// ---------------------------------------------------------------------------
// function:  m2_file_ext_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns the length of the file extension of filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_file_ext_string_length(m2_filename_t filename) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return 0;
    
    return (cardinal) this_filename->extension_length;
} // m2_extension_string_length    


// ---------------------------------------------------------------------------
// function:  m2_copy_file_ext_string( filename, target )
// ---------------------------------------------------------------------------
//
// Copies  the  file extension  of filename descriptor <filename>  to the char
// pointer passed in <target>.  It is the responsibility of the caller to make
// sure  that <target> points to a memory buffer large enough to hold the file
// extenson including its null terminator.
//
// If NULL is passed in <filename> a null terminator is copied to <target>.
// If NULL is passed in <target> the function returns without action.

void m2_copy_file_ext_string(m2_filename_t filename, char *target) {
    m2_filename_s *this_filename = (m2_filename_s *) filename;
    cardinal index;

    // bail out if filename descriptor is NULL
    if (filename == NULL) {
        target[0] = CSTRING_TERMINATOR;
        return;
    } // end if

    // copy the file extension string
    index = 0;
    while (this_filename->extension[index] != CSTRING_TERMINATOR) {
        target[index] = this_filename->extension[index];
        index++;
    } // end while;
    
    // terminate the string
    target[index] = CSTRING_TERMINATOR;
    
    return;
} // end m2_copy_file_ext_string


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
    
    // bail out if filename descriptor is NULL
    if (filename == NULL)
        return DEFAULT_FILENAMING;
    
    return this_filename->filenaming;
} // end m2_filenaming


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
