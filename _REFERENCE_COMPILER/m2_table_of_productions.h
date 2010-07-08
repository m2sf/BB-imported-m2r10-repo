/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2_table_of_productions.h
 *  Master table of EBNF productions
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

// This  file  represents  the  EBNF PRODUCTION MASTER TABLE   for  the  M2R10
// compiler, used to generate data structures containing productions and their
// FIRST and FOLLOW sets, thereby keeping those structures in synchronisation.
// Consequently,  any changes,  additions or removals of productions and their
// respective FIRST and FOLLOW sets must be made in  this  file,  not anywhere
// else.  Failing to do so  will  result in an incorrect compiler  as  related
// data structures will no longer be synchronised.
//
// NB:  The  FIRST and FOLLOW set data  in this file  has  been  automatically
// generated using the utility gen_first_and_follow_sets.c
//
//
// How to use this master table:
//
// The  following  code  will  generate  a declaration  for  an enumeration of
// all productions prefixed with p_ ...
//
//  #define _add_production(_production, \
//      _first0,  _first1,  _first2,  _first3, \
//      _follow0, _follow1, _follow2, _follow3 ) p_ ## _production,
//
//  typedef enum m2_production_t {
//  #include "m2_table_of_productions.h"
//  M2_NUMBER_OF_PRODUCTIONS
//  } /* m2_production_t */;
//  #undef _add_production
//
//
// The following code will generate an array of tokensets initialised with the
// FIRST sets of all productions ...
//
//  #define _add_production(_production, \
//      _first0,  _first1,  _first2,  _first3, \
//      _follow0, _follow1, _follow2, _follow3 ) \
//      { _first0, _first1, _first2, _first3 },
//
//  m2_tokenset_a FIRST[] = {
//  #include "m2_table_of_productions.h"
//  { 0, 0, 0, 0 }
//  } /* FIRST */;
//  #undef _add_production
//
//
// The following code will generate an array of tokensets initialised with the
// FOLLOW sets of all productions ...
//
//  #define _add_production(_production, \
//      _first0,  _first1,  _first2,  _first3, \
//      _follow0, _follow1, _follow2, _follow3 ) \
//      { _follow0, _follow1, _follow2, _follow3 },
//
//  m2_tokenset_a FOLLOW[] = {
//  #include "m2_table_of_productions.h"
//  { 0, 0, 0, 0 }
//  } /* FOLLOW */;
//  #undef _add_production
//
//
// Arguments for the _add_production macro:
//
//  1st: production name, must be preceeded by an underscore
//
//  2nd: 32-bit unsigned integer representing bits  0 to  31 of FIRST set
//  3rd: 32-bit unsigned integer representing bits 32 to  63 of FIRST set
//  4th: 32-bit unsigned integer representing bits 64 to  95 of FIRST set
//  5th: 32-bit unsigned integer representing bits 96 to 127 of FIRST set
//
//  6th: 32-bit unsigned integer representing bits  0 to  31 of FOLLOW set
//  7th: 32-bit unsigned integer representing bits 32 to  63 of FOLLOW set
//  8th: 32-bit unsigned integer representing bits 64 to  95 of FOLLOW set
//  9th: 32-bit unsigned integer representing bits 96 to 127 of FOLLOW set


// Productions

// #1
_add_production( _compilation_unit,
                0x00200420, 0x40000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000000, 0x00008000, 0 )  // FOLLOW set

// #2
_add_production( _prototype,
                0x00000000, 0x40000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000000, 0x00008000, 0 )  // FOLLOW set

// #3
_add_production( _program_module,
                0x00000020, 0x00000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000000, 0x00008000, 0 )  // FOLLOW set

// #4
_add_production( _definition_of_module,
                0x00200000, 0x00000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000000, 0x00008000, 0 )  // FOLLOW set

// #5
_add_production( _implementation_of_module,
                0x00000400, 0x00000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000000, 0x00008000, 0 )  // FOLLOW set

// #6
_add_production( _required_binding,
                0x00000000, 0x80000000, 0x00000000, 0,   // FIRST set
                0x00408000, 0x80000000, 0x00000000, 0 )  // FOLLOW set

// #7
_add_production( _bindable_operator,
                0x00082140, 0x0000644A, 0xC01C0000, 0,   // FIRST set
                0x00000000, 0x00000000, 0x00800000, 0 )  // FOLLOW set

// #8
_add_production( _import_list,
                0x00001200, 0x00000000, 0x00000000, 0,   // FIRST set
                0x04408000, 0x80A00000, 0x00000000, 0 )  // FOLLOW set

// #9
_add_production( _block,
                0x04400000, 0x80A00000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00040000, 0x00000000, 0 )  // FOLLOW set

// #10
_add_production( _declaration,
                0x00400000, 0x80A00000, 0x00000000, 0,   // FIRST set
                0x04008000, 0x00000000, 0x00000000, 0 )  // FOLLOW set

// #11
_add_production( _definition,
                0x00400000, 0x80A00000, 0x00000000, 0,   // FIRST set
                0x00008000, 0x00000000, 0x00000000, 0 )  // FOLLOW set

// #12
_add_production( _const_declaration,
                0x00000000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #13
_add_production( _type,
                0x58000001, 0xA4040000, 0x05000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #14
_add_production( _range,
                0x00000000, 0x00000000, 0x01000000, 0,   // FIRST set
                0x00000008, 0x00000000, 0x00000000, 0 )  // FOLLOW set

// #15
_add_production( _enumeration_type,
                0x00000000, 0x00000000, 0x04000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #16
_add_production( _array_type,
                0x18000000, 0x00000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #17
_add_production( _record_type,
                0x00000000, 0x20000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #18
_add_production( _field_list_sequence,
                0x00000000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x00008000, 0x00000000, 0x00000000, 0 )  // FOLLOW set

// #19
_add_production( _field_list,
                0x00000000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x00008000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #20
_add_production( _set_type,
                0x00000000, 0x04000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #21
_add_production( _pointer_type,
                0x00000001, 0x00000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #22
_add_production( _procedure_type,
                0x00000000, 0x80000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #23
_add_production( _formal_type_list,
                0x10C00000, 0x00340000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000024, 0x00000000, 0 )  // FOLLOW set

// #24
_add_production( _formal_type,
                0x10C00000, 0x00340000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000824, 0x00000000, 0 )  // FOLLOW set

// #25
_add_production( _attributed_formal_type,
                0x10C00000, 0x00240000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000824, 0x00000000, 0 )  // FOLLOW set

// #26
_add_production( _simple_formal_type,
                0x10800000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000824, 0x00000000, 0 )  // FOLLOW set

// #27
_add_production( _variadic_formal_type,
                0x00000000, 0x00100000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000824, 0x00000000, 0 )  // FOLLOW set

// #28
_add_production( _variable_declaration,
                0x00000000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #29
_add_production( _procedure_declaration,
                0x00000000, 0x80000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #30
_add_production( _procedure_header,
                0x00000000, 0x80000000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000004, 0x00000000, 0 )  // FOLLOW set

// #31
_add_production( _formal_param_list,
                0x00400000, 0x00340000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00000000, 0x02000000, 0 )  // FOLLOW set

// #32
_add_production( _formal_params,
                0x00400000, 0x00340000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00100004, 0x02000000, 0 )  // FOLLOW set

// #33
_add_production( _simple_formal_params,
                0x00000000, 0x00100000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00100004, 0x02000000, 0 )  // FOLLOW set

// #34
_add_production( _variadic_formal_params,
                0x00400000, 0x00240000, 0x00000000, 0,   // FIRST set
                0x00000000, 0x00100004, 0x02000000, 0 )  // FOLLOW set

// #35
_add_production( _statement,
                0x01006880, 0x180C0000, 0x00000000, 0,   // FIRST set
                0x00028000, 0x00400004, 0x08000000, 0 )  // FOLLOW set

// #36
_add_production( _statement_sequence,
                0x01006880, 0x180C0000, 0x00000000, 0,   // FIRST set
                0x00038000, 0x00400000, 0x08000000, 0 )  // FOLLOW set

// #37
_add_production( _assignment_or_procedure_call,
                0x00000000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x00028000, 0x00400004, 0x08000000, 0 )  // FOLLOW set

// #38
_add_production( _if_statement,
                0x00000800, 0x00000000, 0x00000000, 0,   // FIRST set
                0x00028000, 0x00400004, 0x08000000, 0 )  // FOLLOW set

// #39
_add_production( _case_statement,
                0x01000000, 0x00000000, 0x00000000, 0,   // FIRST set
                0x00028000, 0x00400004, 0x08000000, 0 )  // FOLLOW set

// #40
_add_production( _case,
                0x00800010, 0x00072400, 0x04400000, 0,   // FIRST set
                0x00028000, 0x00000000, 0x08000000, 0 )  // FOLLOW set

// #41
_add_production( _case_labels,
                0x00800010, 0x00072400, 0x04400000, 0,   // FIRST set
                0x00000000, 0x00000820, 0x00000000, 0 )  // FOLLOW set

// #42
_add_production( _while_statement,
                0x00000000, 0x00080000, 0x00000000, 0,   // FIRST set
                0x00028000, 0x00400004, 0x08000000, 0 )  // FOLLOW set

// #43
_add_production( _repeat_statement,
                0x00000000, 0x10000000, 0x00000000, 0,   // FIRST set
                0x00028000, 0x00400004, 0x08000000, 0 )  // FOLLOW set

// #44
_add_production( _loop_statement,
                0x00000080, 0x00000000, 0x00000000, 0,   // FIRST set
                0x00028000, 0x00400004, 0x08000000, 0 )  // FOLLOW set

// #45
_add_production( _for_statement,
                0x00002000, 0x00000000, 0x00000000, 0,   // FIRST set
                0x00028000, 0x00400004, 0x08000000, 0 )  // FOLLOW set

// #46
_add_production( _const_expression,
                0x00800010, 0x00072400, 0x04400000, 0,   // FIRST set
                0x02040008, 0x000008A4, 0x02E00000, 0 )  // FOLLOW set

// #47
_add_production( _relation,
                0x00000100, 0x00008003, 0xE0000000, 0,   // FIRST set
                0x00000010, 0x00072400, 0x04000000, 0 )  // FOLLOW set

// #48
_add_production( _simple_const_expr,
                0x00800010, 0x00072400, 0x04400000, 0,   // FIRST set
                0x00000100, 0x00008013, 0xE0000000, 0 )  // FOLLOW set

// #49
_add_production( _add_operator,
                0x00000002, 0x00002400, 0x00000000, 0,   // FIRST set
                0x00000010, 0x00070000, 0x04000000, 0 )  // FOLLOW set

// #50
_add_production( _const_term,
                0x00800010, 0x00070000, 0x04400000, 0,   // FIRST set
                0x00000102, 0x0000A413, 0xE0000000, 0 )  // FOLLOW set

// #51
_add_production( _mul_operator,
                0x20080040, 0x00004040, 0x00000000, 0,   // FIRST set
                0x00000010, 0x00070000, 0x04000000, 0 )  // FOLLOW set

// #52
_add_production( _const_factor,
                0x00800010, 0x00070000, 0x04400000, 0,   // FIRST set
                0x20080142, 0x0000E453, 0xE0000000, 0 )  // FOLLOW set

// #53
_add_production( _designator,
                0x00000000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x00008000, 0x0000120C, 0x04000000, 0 )  // FOLLOW set

// #54
_add_production( _designator_tail,
                0x00000000, 0x00000000, 0x11000000, 0,   // FIRST set
                0x00008000, 0x0000120C, 0x04000000, 0 )  // FOLLOW set

// #55
_add_production( _expression_list,
                0x00800010, 0x00072400, 0x04400000, 0,   // FIRST set
                0x00000000, 0x00000000, 0x02800000, 0 )  // FOLLOW set

// #56
_add_production( _expression,
                0x00800010, 0x00072400, 0x04400000, 0,   // FIRST set
                0x00078008, 0x03400804, 0x0A800000, 0 )  // FOLLOW set

// #57
_add_production( _simple_expression,
                0x00800010, 0x00072400, 0x04400000, 0,  // FIRST set
                0x00078018, 0x03472C14, 0x0A800000, 0 )  // FOLLOW set

// #58
_add_production( _term,
                0x00800010, 0x00070000, 0x04400000, 0,   // FIRST set
                0x0007801A, 0x03472C14, 0x0A800000, 0 )  // FOLLOW set

// #59
_add_production( _factor,
                0x00800010, 0x00070000, 0x04400000, 0,   // FIRST set
                0x0007801A, 0x03472C14, 0x0A800000, 0 )  // FOLLOW set

// #60
_add_production( _designator_or_procedure_call,
                0x00000000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x0007801A, 0x03472C14, 0x0A800000, 0 )  // FOLLOW set

// #61
_add_production( _actual_parameters,
                0x00000000, 0x00000000, 0x04000000, 0,   // FIRST set
                0x0007801A, 0x03472C14, 0x0A800000, 0 )  // FOLLOW set

// #62
_add_production( _const_structured_value,
                0x00000000, 0x00000000, 0x00400000, 0,   // FIRST set
                0x00028000, 0x00400804, 0x08200000, 0 )  // FOLLOW set

// #63
_add_production( _const_value_component,
                0x00800010, 0x00072400, 0x04400000, 0,   // FIRST set
                0x00000000, 0x00000800, 0x00200000, 0 )  // FOLLOW set

// #64
_add_production( _structured_value,
                0x00000000, 0x00000000, 0x00400000, 0,  // FIRST set
                0x00028000, 0x00400804, 0x08200000, 0 )  // FOLLOW set

// #65
_add_production( _value_component,
                0x00800010, 0x00072400, 0x04400000, 0,   // FIRST set
                0x00000000, 0x00000800, 0x00200000, 0 )  // FOLLOW set

// #66
_add_production( _qualident,
                0x00000000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x200F814A, 0x0340FE5F, 0xFF800000, 0 )  // FOLLOW set

// #67
_add_production( _ident_list,
                0x00000000, 0x00040000, 0x00000000, 0,   // FIRST set
                0x00008000, 0x00000024, 0x02000000, 0 )  // FOLLOW set

// END OF FILE