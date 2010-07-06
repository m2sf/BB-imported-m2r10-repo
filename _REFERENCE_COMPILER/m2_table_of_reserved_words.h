/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_reserved_word_table.h
 *  Master table of reserved words
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

// This file represents the RESERVED WORD MASTER TABLE for the M2R10 compiler,
// used to derive other tables which contain  reserved words,  thereby keeping
// those tables  in  synchronisation.  Consequently, any changes, additions or
// removals of keywords must be made in this file, not anywhere else.  Failing
// to do so will result in an incorrect compiler as tables will no longer be 
// synchronised.
//
// How to use this master table:
//
// The  following  code  will  generate  a declaration  for  an enumeration of
// reserved word identifiers prefixed with RESERVED_WORD ...
//
//  #define _add_reserved_word(_suffix, _hash) RESERVED_WORD ## _suffix,
//  enum m2_reserved_word_t {
//  #include "m2_reserved_word_table.h"
//  } /* m2_reserved_word_t */;
//  #undef _add_reserved_word
//
// Arguments for the _add_reserved_word macro:
//
//  1st: reserved word identifier suffix, must be preceeded by an underscore
//  2nd: storage hash value for the reserved word, must be an unsigned integer

_add_reserved_word( _ALIAS,          0x333D9410 )
_add_reserved_word( _AND,            0x20500337 )
_add_reserved_word( _ARRAY,          0x4ED50419 )
_add_reserved_word( _ASSOCIATIVE,    0x5728E251 )
_add_reserved_word( _BEGIN,          0x308620C9 )
_add_reserved_word( _BY,             0x00421097 )
_add_reserved_word( _CASE,           0x4D9DA670 )
_add_reserved_word( _CAST,           0x4D9DA67F )
_add_reserved_word( _CONST,          0x49331643 )
_add_reserved_word( _DEFINITION,     0x327B22B3 )
_add_reserved_word( _DESCENDING,     0x18FFF5F8 )
_add_reserved_word( _DIV,            0x21C53091 )
_add_reserved_word( _DO,             0x0044110B )
_add_reserved_word( _ELSE,           0x3015F279 )
_add_reserved_word( _ELSIF,          0x47E3AD09 )
_add_reserved_word( _END,            0x2248413B )
_add_reserved_word( _EXIT,           0x35F4AA1E )
_add_reserved_word( _FOR,            0x22C75109 )
_add_reserved_word( _FROM,           0x618D1F4A )
_add_reserved_word( _IF,             0x0049123D )
_add_reserved_word( _IMPLEMENTATION, 0x2781BF72 )
_add_reserved_word( _IMPORT,         0x721A6745 )
_add_reserved_word( _IN,             0x00491245 )
_add_reserved_word( _LOOP,           0x773BD544 )
_add_reserved_word( _MOD,            0x2639BD82 )
_add_reserved_word( _MODULE,         0x27413CCC )
_add_reserved_word( _NOT,            0x26B7CD13 )
_add_reserved_word( _OF,             0x004F13B7 )
_add_reserved_word( _OPAQUE,         0x0A02B041 )
_add_reserved_word( _OR,             0x004F13C3 )
_add_reserved_word( _POINTER,        0x7A3963DD )
_add_reserved_word( _PROCEDURE,      0x6D45A513 )
_add_reserved_word( _PROTOTYPE,      0x77D719C2 )
_add_reserved_word( _RECORD,         0x3D6E40B1 )
_add_reserved_word( _REPEAT,         0x15493B3B )
_add_reserved_word( _RETURN,         0x57567A70 )
_add_reserved_word( _SET,            0x29241822 )
_add_reserved_word( _THEN,           0x67F5EC3D )
_add_reserved_word( _TO,             0x005414FB )
_add_reserved_word( _TYPE,           0x705FF67A )
_add_reserved_word( _UNTIL,          0x64354E1E )
_add_reserved_word( _VAR,            0x2A9A45A7 )
_add_reserved_word( _VARIADIC,       0x1376B35F )
_add_reserved_word( _WHILE,          0x4F7D7DD1 )

// END OF FILE