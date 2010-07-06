/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_builtin_ident_table.h
 *  Master table of built-in identifiers
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

// This  file  represents the  BUILT-IN IDENTIFIER MASTER TABLE  for the M2R10
// compiler,  which is used  to derive  other tables which contain identifiers
// and  it thereby  keeps those tables  in synchronisation.  Consequently, any
// changes, additions or removals of built-in identifiers must be made in this
// file, not anywhere else.  Failing to do so  will  result  in  an  incorrect
// compiler as tables will no longer be synchronised.
//
// How to use this master table:
//
// The  following  code  will  generate  a declaration  for  an enumeration of
// predefined identifiers prefixed with IDENT ...
//
//  #define _add_ident(_suffix, _hash) IDENT ## _suffix,
//  enum m2_builtin_ident_t {
//  #include "m2_builtin_ident_table.h"
//  } /* m2_builtin_ident_t */;
//  #undef _add_ident
//
// Arguments for the _add_ident macro:
//
//  1st: identifier suffix, must be preceeded by an underscore
//  2nd: hash value for the symbol table, integer

// Built-in constants

_add_ident( _NIL,        0x26B1CB91 )
_add_ident( _TRUE,       0x6CF28B2E )
_add_ident( _FALSE,      0x089CA9C3 )

// Built-in types

_add_ident( _BOOLEAN,    0x76F607C8 )
_add_ident( _BITSET,     0x0338C055 )
_add_ident( _LONGBITSET, 0x63385B51 )
_add_ident( _CHAR,       0x50FE0E96 )
_add_ident( _UNICHAR,    0x0D2BF7E6 )
_add_ident( _OCTET,      0x4CDC1AEF )
_add_ident( _CARDINAL,   0x1EA3C640 )
_add_ident( _LONGCARD,   0x6630FF2C )
_add_ident( _INTEGER,    0x4AE471DE )
_add_ident( _LONGINT,    0x2A1A2753 )
_add_ident( _REAL,       0x096A1B3E )
_add_ident( _LONGREAL,   0x21FE743A )

// Built-in procedures

_add_ident( _NEW,        0x26ADCAA0 )
_add_ident( _DISPOSE,    0x6D0CFBBF )
_add_ident( _READ,       0x096A1B36 )
_add_ident( _WRITE,      0x20C9A73F )
_add_ident( _WRITEF,     0x38DF28C7 )

// Built-in functions

_add_ident( _ABS,        0x20440052 )
_add_ident( _NEG,        0x26ADCA90 )
_add_ident( _ODD,        0x272AD9CF )
_add_ident( _PRED,       0x32C74441 )
_add_ident( _SUCC,       0x3FD3E482 )
_add_ident( _ORD,        0x2738DD41 )
_add_ident( _CHR,        0x214620CD )
_add_ident( _COUNT,      0x4CA0818F )
_add_ident( _SIZE,       0x3A023021 )
_add_ident( _HIGH,       0x3A243342 )
_add_ident( _LENGTH,     0x344075E6 )
_add_ident( _NEXTV,      0x68049F23 )
_add_ident( _TMIN,       0x6A703ABE )
_add_ident( _TMAX,       0x6A6838D0 )
_add_ident( _TSIZE,      0x03915C75 )

// Built-in compiler macros

_add_ident( _MIN,        0x2633BC12 )
_add_ident( _MAX,        0x262BBA24 )

// END OF FILE