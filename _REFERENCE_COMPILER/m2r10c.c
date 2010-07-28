/* Modula-2 R10 Compiler (m2r10c)
 *
 *  m2r10c.c
 *  Main program (driver)
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

#include <stdio.h>
#include <getopt.h>
#include "KVS.h"
#include "ASCII.h"
#include "m2_build_params.h"
#include "m2_errmsg.h"
#include "m2_filenames.h"
#include "m2_fileio.h"
#include "m2_parser.h"
#include "m2_ast.h"
#include "m2_code_generator.h"


// ---------------------------------------------------------------------------
// Interim declarations due to yet unimplemented module(s)
// ---------------------------------------------------------------------------


// ---------------------------------------------------------------------------
// Application info
// ---------------------------------------------------------------------------

#define APP_NAME      "Modula-2 R10 Compiler (m2r10c)"
#define APP_VERSION   "Version 0.0" " Build " BUILD_VERSION
#define APP_COPYRIGHT "(C) 2010 B.Kowarsch. All rights reserved."


// ---------------------------------------------------------------------------
// Exit codes
// ---------------------------------------------------------------------------

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif


// ---------------------------------------------------------------------------
// Option macros
// ---------------------------------------------------------------------------

#define OPT_ENDMARKER -1
#define OPT_SYNTAXCHECK 'c'
#define OPT_DUMPAST 'a'
#define OPT_DUMPSYMTAB 's'
#define OPT_TARGET 't'
#define OPT_VERSION 'V'
#define OPT_USAGE 'h'

#define OPT_LIST "achst:V"


// ---------------------------------------------------------------------------
// Long options
// ---------------------------------------------------------------------------

static struct option lopts_all[] = {
    { "syntaxcheck", 0, 0, OPT_SYNTAXCHECK },
    { "dumpast",     0, 0, OPT_DUMPAST     },
    { "dumpsymtab",  0, 0, OPT_DUMPSYMTAB  },
    { "target",      0, 0, OPT_TARGET      },
    { "help",        0, 0, OPT_USAGE       },
    { "version",     0, 0, OPT_VERSION     },
    { 0, 0, 0, 0 }
}; // end lopts_all


// ---------------------------------------------------------------------------
// function:  show_usage()
// ---------------------------------------------------------------------------
//
// Prints usage info to stdout.

static void show_usage() {
    printf("Usage\n\n"
           "  m2r10c [ debug-option | target_option tgt] sourcefile\n\n"
           "Obtaining information:\n"
           "  m2r10c usage-info-option | version-info-option\n\n"
           "Target options:\n"
           " -t, --target  tgt : generate code for specified target"
           "                     available targets are c99 and llvm\n"
           "Debug options:\n"
           " -c, --syntaxcheck : perform syntax check only\n\n"
           " -a, --dumpast     : build and print AST only\n"
           " -s, --dumpsymtab  : build and print symbol table only\n"
           "Info options:\n"
           " -h, --help        : print usage info\n"
           " -V, --version     : print version and copyright notice\n"
           "\n");
} // end show_usage


// ---------------------------------------------------------------------------
// function:  show_version()
// ---------------------------------------------------------------------------
//
// Prints software version and copyright notice to stdout.

static void show_version() {
    printf("%s %s\n%s\n\n", APP_NAME, APP_VERSION, APP_COPYRIGHT);
} // end show_version


// ---------------------------------------------------------------------------
// function:  show_error( error )
// ---------------------------------------------------------------------------
//
// Prints error message for <error> to stderr.

static void show_error(m2_err_t error) {
    printf(error_message(error));
    printf("\n\n");
} // end show_error


// --------------------------------------------------------------------------
// function:  main()
// --------------------------------------------------------------------------
//
// Get command line arguments, open source file and feed parser.

int main (int argc, const char * argv[]) {
    int opt, index = 0;
    char *source_path;
    FILE *sourcefile, *outfile;
    bool syntax_check_only = false;
    m2_file_type_t outfile_type;
    m2_target_t target = M2_TARGET_C99;
    m2_filename_t source_filename, output_filename;
    m2_filename_status_t fn_status;
    m2_parser_status_t p_status;
    m2_codegen_status_t cg_status;
    m2_fileio_status_t fio_status;
    kvs_table_t lexeme_table;
    m2_parser_t parser; m2_ast_node_t ast; m2_codegen_t cgen;
    
    // get command line options and arguments
    opt = getopt_long(argc, (char **) argv, OPT_LIST, lopts_all, &index);
    
    switch (opt) {
        case OPT_DUMPAST :
            show_error(ERR_OPT_NOT_IMPLEMENTED);
            return EXIT_FAILURE;
        case OPT_SYNTAXCHECK :
            syntax_check_only = true;
            break;
        case OPT_DUMPSYMTAB :
            show_error(ERR_OPT_NOT_IMPLEMENTED);
            return EXIT_FAILURE;
        case OPT_TARGET :
            show_error(ERR_OPT_NOT_IMPLEMENTED);
            return EXIT_FAILURE;
            break;
        case OPT_USAGE :
            show_usage();
            return EXIT_SUCCESS;
        case OPT_VERSION :
            show_version();
            return EXIT_SUCCESS;
        case OPT_ENDMARKER :
            if (argc > 0)
                break;
            show_version();
            show_usage();
            return EXIT_SUCCESS;
        default :
            show_error(ERR_OPT_INVALID);
            show_usage();
            return EXIT_FAILURE;
    } // end switch
    
    if (argc == 0) {
        show_error(ERR_SRC_NOT_SPECIFIED); // missing sourcefile argument
        return EXIT_FAILURE;
    }
    else if (argc > 1) {
        show_error(ERR_TOO_MANY_ARGS); // extra arguments ingored
    } // end if
    
    // get source path argument
    source_path = (char *) argv[1];
    
    // get source filename descriptor from source path
    source_filename =
      m2_new_filename_from_path(source_path, DEFAULT_FILENAMING, &fn_status);
    
    if (fn_status != M2_FILENAME_STATUS_SUCCESS) {
        show_error(fn_status);
        return EXIT_FAILURE;
    } // end if
    
    // get matching outfile type for target and source file type
    outfile_type =
      m2_outfile_type_for(target, m2_file_type(source_filename));
    
    if (outfile_type == FILE_TYPE_UNKNOWN) {
        // no outfile associated with with source for target
        show_error(ERR_OPT_INVALID);
        return EXIT_FAILURE;
    } // end if
    
    // open sourcefile
    sourcefile = m2_open_sourcefile(source_filename, &fio_status);
    
    if (fio_status != M2_FILEIO_STATUS_SUCCESS) {
        show_error(fio_status);
        return EXIT_FAILURE;
    } // end if
    
    // init parser
    parser = m2_new_parser(sourcefile,
                           lexeme_table, NULL, NULL, NULL, &p_status);
    
    if (p_status != M2_PARSER_STATUS_SUCCESS) {
        show_error(p_status);
        return EXIT_FAILURE;
    } // end if
    
    // create new AST root
    ast = m2_ast_new_node(0, 0, NULL);
    
    if (ast == NULL) {
        show_error(ERR_ALLOCATION_FAILED);
        return EXIT_FAILURE;
    } // end if
    
    // invoke parser
    m2_parse_file(parser, &p_status);
    
    if (p_status != M2_PARSER_STATUS_SUCCESS) {
        show_error(p_status);
        return EXIT_FAILURE;
    } // end if
    
    // get output filename descriptor from source file components
    output_filename = m2_new_filename(m2_directory_string(source_filename),
                      m2_filename_string(source_filename),
                      outfile_type, DEFAULT_FILENAMING, &fn_status);
    
    if (fn_status != M2_FILENAME_STATUS_SUCCESS) {
        show_error(fn_status);
        return EXIT_FAILURE;
    } // end if    
    
    // create and open output file
    outfile = m2_new_outfile(output_filename, &fio_status);
    
    if (fio_status != M2_FILEIO_STATUS_SUCCESS) {
        show_error(fio_status);
        return EXIT_FAILURE;
    } // end if
    
    // invoke code generator
    cgen = m2_new_codegen(outfile, NULL, NULL, NULL, NULL, &cg_status);
    
    if (cg_status != M2_CODEGEN_STATUS_SUCCESS) {
        show_error(cg_status);
        return EXIT_FAILURE;
    } // end if
    
    // clean up
    m2_close_file(sourcefile, NULL);
    m2_dispose_parser(parser, NULL);
    
    m2_close_file(outfile, NULL);
    m2_dispose_codegen(cgen, &cg_status);
    
    m2_dispose_filename(sourcefile);
    m2_dispose_filename(outfile);
    
    return EXIT_SUCCESS;
} // main

// END OF FILE
