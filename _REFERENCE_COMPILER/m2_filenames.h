/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_filenames.h
 *  Portable filename handling interface
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

#ifndef M2_FILENAMES_H
#define M2_FILENAMES_H


#include "common.h"


// --------------------------------------------------------------------------
// Status codes
// --------------------------------------------------------------------------

typedef /* m2_filename_status_t */ enum {
    M2_FILENAME_STATUS_SUCCESS,
    M2_FILENAME_STATUS_ALLOCATION_FAILED,
    M2_FILENAME_STATUS_INVALID_REFERENCE,
    M2_FILENAME_STATUS_INVALID_DIRECTORY,
    M2_FILENAME_STATUS_INVALID_FILENAME,
    M2_FILENAME_STATUS_INVALID_FILETYPE,
    M2_FILENAME_STATUS_INVALID_PATHNAME,
    M2_FILENAME_STATUS_PATHNAME_TOO_LONG,
    M2_FILENAME_STATUS_FILENAME_TOO_LONG,
} m2_filename_status_t;


// ---------------------------------------------------------------------------
// Filenaming schemes
// ---------------------------------------------------------------------------

typedef enum /* m2_filenaming_t */ {
    DEFAULT_FILENAMING = 0,
    POSIX_FILENAMING,
    ISO9660_FILENAMING,
    MSDOS_FILENAMING,
    OPENVMS_FILENAMING,
    NUMBER_OF_FILENAMING_SCHEMES
} m2_filenaming_t;


// ---------------------------------------------------------------------------
// File types
// ---------------------------------------------------------------------------

typedef enum /* m2_file_type_t */ {
    FILE_TYPE_UNKNOWN = 0,
    FILE_TYPE_DEF,
    FILE_TYPE_MOD,
    FILE_TYPE_AST,
    FILE_TYPE_SYM,
    FILE_TYPE_H,
    FILE_TYPE_C,
    FILE_TYPE_LLVM,
    INVALID_FILENAME
} m2_file_type_t;


// ---------------------------------------------------------------------------
// Source types
// ---------------------------------------------------------------------------

typedef enum /* m2_source_type_t */ {
    SOURCE_TYPE_UNKNOWN = FILE_TYPE_UNKNOWN,
    SOURCE_TYPE_DEF = FILE_TYPE_DEF,
    SOURCE_TYPE_MOD = FILE_TYPE_MOD
} m2_source_type_t;


// ---------------------------------------------------------------------------
// Target types
// ---------------------------------------------------------------------------

typedef enum /* m2_target_t */ {
    M2_TARGET_C99,
    M2_TARGET_LLVM
} m2_target_t;


// --------------------------------------------------------------------------
// Opaque filename descriptor handle type
// --------------------------------------------------------------------------
//
// WARNING: Objects of this opaque type should only be accessed through this
// public interface.  DO NOT EVER attempt to bypass the public interface.
//
// The internal data structure of this opaque type is HIDDEN  and  MAY CHANGE
// at any time WITHOUT NOTICE. Accessing the internal data structure directly
// other than through the  functions  in this public interface is  UNSAFE and
// may result in an inconsistent program state or a crash.

typedef opaque_t m2_filename_t;


// ---------------------------------------------------------------------------
// function:  m2_working_directory()
// ---------------------------------------------------------------------------
//
// Obtains the  current working directory pathname  of the current process and
// returns an  immutable pointer  to the pathname string.  Subsequent calls to
// this function will always return the  same string  as the working directory
// is not supposed to change during the process' lifetime.

const char *m2_working_directory();


// ---------------------------------------------------------------------------
// function:  m2_is_valid_filename_string( filename )
// ---------------------------------------------------------------------------
//
// Returns true  if <filename> is a valid Modula-2 filename,  returns false if
// it is not.  Modula-2 filenames must always be valid Modula-2 identifiers.
//
// filename := ( letter | "_" | "$" ) ( letter | digit | "_" | "$" )*

bool m2_is_valid_filename_string(const char *filename);


// --------------------------------------------------------------------------
// function:  m2_outfile_type_for( target, sourcefile_type )
// --------------------------------------------------------------------------
//
// Returns  the  output  file  type  associated  with  <sourcefile_type>  for
// <target>.  Returns FILE_TYPE_UNKNOWN if no associated file type exists.

m2_file_type_t m2_outfile_type_for(m2_target_t target,
                                m2_file_type_t sourcefile_type);


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
                    m2_filename_status_t *status);


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
                              m2_filename_status_t *status);


// ---------------------------------------------------------------------------
// function:  m2_path_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns  the  length  of the  full path  of filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_path_string_length(m2_filename_t filename);


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

void m2_copy_path_string(m2_filename_t filename, char *target);


// ---------------------------------------------------------------------------
// function:  m2_directory_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns the length of the directory path of filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_directory_string_length(m2_filename_t filename);


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

void m2_copy_directory_string(m2_filename_t filename, char *target);


// ---------------------------------------------------------------------------
// function:  m2_filename_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns the length  of the full filename of filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_filename_string_length(m2_filename_t filename);


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

void m2_copy_filename_string(m2_filename_t filename, char *target);


// ---------------------------------------------------------------------------
// function:  m2_basename_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns  the  length  of the  basename  of  filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_basename_string_length(m2_filename_t filename);


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

void m2_copy_basename_string(m2_filename_t filename, char *target);


// ---------------------------------------------------------------------------
// function:  m2_file_ext_string_length( filename )
// ---------------------------------------------------------------------------
//
// Returns the length of the file extension of filename descriptor <filename>.
// Returns zero if the descriptor is NULL.

fmacro cardinal m2_file_ext_string_length(m2_filename_t filename);


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

void m2_copy_file_ext_string(m2_filename_t filename, char *target);


// ---------------------------------------------------------------------------
// function:  m2_file_type( filename )
// ---------------------------------------------------------------------------
//
// Returns the value of the file_type field of filename descriptor <filename>.
// Returns FILE_TYPE_UNKNOWN if the descriptor is NULL.

m2_file_type_t m2_file_type(m2_filename_t filename);


// ---------------------------------------------------------------------------
// function:  m2_filenaming( filename )
// ---------------------------------------------------------------------------
//
// Returns the  filenaming scheme  of filename descriptor <filename>.  Returns
// DEFAULT_FILENAMING if the descriptor is NULL.

m2_filenaming_t m2_filenaming(m2_filename_t filename);


// ---------------------------------------------------------------------------
// function:  m2_dispose_filename( filename )
// ---------------------------------------------------------------------------
//
// Deallocates filename descriptor <filename> and all of its components.

void m2_dispose_filename(m2_filename_t filename);


#endif /* M2_FILENAMES_H */

// END OF FILE
