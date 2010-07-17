/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_table_of_pragma_names.h
 *  Master table of pragma names
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

// This file represents the  PRAGMA NAME MASTER TABLE  for the M2R10 compiler,
// used  to derive other tables  which contain pragma names,  thereby  keeping
// those tables in synchronisation.  Consequently,  any changes, additions  or
// removals of pragma names  must  be made in  this  file,  not anywhere else.
// Failing to do so  will result  in an  incorrect compiler  as tables will no
// longer be synchronised.
//
// How to use this master table:
//
// The  following  code  will  generate  a declaration  for  an enumeration of
// pragma identifiers prefixed with PRAGMA_NAME ...
//
//  #define _add_pragma_name(_suffix, _hash) PRAGMA_NAME ## _suffix,
//  enum m2_pragma_name_t {
//  #include "m2_pragma_name_table.h"
//  } /* m2_pragma_name_t */;
//  #undef _add_pragma_name
//
// Arguments for the _add_pragma_name macro:
//
//  1st: pragma name suffix, must be preceeded by an underscore
//  2nd: storage hash value for the pragma name, unsigned integer

_add_pragma_ident( _IF,       0x0049123D )
_add_pragma_ident( _ELSIF,    0x47E3AD09 )
_add_pragma_ident( _ELSE,     0x3015F279 )
_add_pragma_ident( _ENDIF,    0x1D8E65F8 )
_add_pragma_ident( _ENCODING, 0x78784A93 )
_add_pragma_ident( _INFO,     0x6B20514E )
_add_pragma_ident( _WARN,     0x7024F526 )
_add_pragma_ident( _ERROR,    0x5E948388 )
_add_pragma_ident( _FATAL,    0x0C7B2164 )
_add_pragma_ident( _ALIGN,    0x33439585 )
_add_pragma_ident( _FOREIGN,  0x57B56914 )
_add_pragma_ident( _MAKE,     0x1ED9CBEE )
_add_pragma_ident( _INLINE,   0x78F32539 )
_add_pragma_ident( _NOINLINE, 0x3F96E73A )
_add_pragma_ident( _VOLATILE, 0x1BDD4B5C )

// END OF FILE