/*
 *  test_lexer.c
 *  m2c
 *
 */

// ---------------------------------------------------------------------------
// imports
// ---------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include "KVS.h"
#include "m2_lexer.h"
#include "m2_symbol_table.h"

// ---------------------------------------------------------------------------
// forward declarations
// ---------------------------------------------------------------------------

 void print_symbol(m2_lexer_t lexer, kvs_table_t symtab, cardinal lexkey, m2_token_t token );


// ---------------------------------------------------------------------------
// function:  main(argc, argv)
// ---------------------------------------------------------------------------
//
// Reads  one  argument  from  the command line,  interprets the argument as a
// pathname to  an  Modula-2 source file,  opens the file,  performs
// a lexical analysis and prints tokens, lexemes and attributes to stdout.

int main (int argc, const char **argv) {
    const char *pathname;
    kvs_table_t symtab;
    m2_lexer_t lexer;
    m2_token_t token;
//    symbol_s symbol;
    cardinal lexkey; 
    
    m2_lexer_status_t status;
    FILE *filetotest;

    if (argc <= 1) {
        printf("usage: test_lexer sourcefile\n");
        exit(1);
    } // end if

    pathname = argv[1];

    printf("lexical analysis for file %s\n", pathname);
    
    filetotest = fopen(pathname,"r");	
    if (filetotest == (FILE *)0) {
        printf (" not possible to open the file: %s", pathname);
        exit(1);
        }
        
    // create new symbol table
    symtab = kvs_new_table(0,NULL);

    // create a new lexer instance
    lexer = m2_new_lexer(filetotest, symtab, &status);

    token = 0;

    // read until end of file
    while (token != TOKEN_EOF_MARKER) {

        token = m2_lexer_getsym(lexer, &lexkey, &status);

        print_symbol(lexer, symtab, lexkey, token);

    } // end while

    m2_dispose_lexer(lexer, &status);

    return 0;
} // end main


// ---------------------------------------------------------------------------
// function:  print_symbol(symtab, sym)
// ---------------------------------------------------------------------------
//
// Prints token, lexeme and attributes of symbol <sym> to stdout.

void print_symbol(m2_lexer_t lexer, kvs_table_t symtab, cardinal lexkey, m2_token_t token) {
//    m2_token_t token = symbol_token(sym);
    cardinal len;
    cardinal xpos;
    cardinal ypos;
    char *lexeme;
    bool null_terminated;
    kvs_status_t *status;
    
    
    m2_lexer_getpos(lexer, &xpos, &ypos, NULL);
    printf("Token: %s at line: %i col: %i\n",
            m2_token_name(token),
            xpos, ypos);

    if (token == TOKEN_EOF_MARKER) {
        printf("\n");
    }
    if (token == TOKEN_ILLEGAL_CHARACTER) {
        printf(" offending character: '%c'\n",
                m2_offending_char(lexer, &xpos, &ypos,NULL));
   }
//    else /* all other tokens */ {
//        kvs_get_entry(symtab, 1, lexkey, &len, &null_terminated, status);
//        if (lexeme != NULL)
//            printf(" lexeme: %s\n", lexeme);
//
//        len = 0;
//        printf(" flags:\n");
//
//        if (symbol_flag(sym, is_terminal)) {
//            printf("terminal");
//            len++;
//        } // end if
//
//        if (symbol_flag(sym, is_reserved_word)) {
//            if (len > 0)
//                printf(", ");
//
//            printf("reserved word");
//        } // end if
//
//        if (symbol_flag(sym, is_builtin_ident)) {
//            if (len > 0)
//                printf(", ");
//            printf("builtin");
//        } // end if
//
//       if (symbol_flag(sym, is_literal)) {
//            if (len > 0)
//                printf(", ");
//            printf("literal");
//        } // end if
//
//        if (symbol_flag(sym, is_operator)) {
//            if (len > 0)
//                printf(", ");
//            printf("operator");
//        } // end if
//
//        if (symbol_flag(sym, is_unary_operator)) {
//            if (len > 0)
//                printf(", ");
//            printf("unary operator");
//        } // end if
//
//        if (symbol_flag(sym, is_relational_operator)) {
//            if (len > 0)
//                printf(", ");
//            printf("relational operator");
//        } // end if
//
//        if (symbol_flag(sym, is_first_order_operator)) {
//            if (len > 0)
//                printf(", ");
//            printf("first order operator");
//        } // end if
//
//        if (symbol_flag(sym, is_second_order_operator)) {
//            if (len > 0)
//                printf(", ");
//            printf("second order operator");
//        } // end if
//
//        if (symbol_flag(sym, is_malformed_literal)) {
//            if (len > 0)
//                printf(", ");
//            printf("malformed literal");
//        } // end if
//
//        if (symbol_flag(sym, excess_chars_truncated)) {
//            if (len > 0)
//                printf(", ");
//            printf("excess characters truncated");
//       } // end if
//
//        if (symbol_flag(sym, has_non_7bit_ascii_chars)) {
//            if (len > 0)
//                printf(", ");
//            printf("has non 7bit ascii characters");
//        } // end if
//
//        if (symbol_flag(sym, has_one_or_more_underscores)) {
//            if (len > 0)
//                printf(", ");
//            printf("has underscores");
//        } // end if
//
//        if (symbol_flag(sym, has_one_or_more_dollar_signs)) {
//            if (len > 0)
//                printf(", ");
//            printf("has dollar signs");
//        } // end if
//
//        if (symbol_flag(sym, may_collide_with_priv_objc_name)) {
//            if (len > 0)
//                printf(", ");
//            printf("may collide with private objc name");
//        } // end if
//
//        printf("\n");
//
//    } // end if
//
//    if ((token == TOKEN_OPENING_PARENTHESIS) ||
//        (token == TOKEN_CLOSING_PARENTHESIS)) {
//        printf(" parenthesis nesting level: %i\n",
//                symbol_paren_nesting_level(sym));
//    }
//    else if ((token == TOKEN_OPENING_BRACKET) ||
//        (token == TOKEN_CLOSING_BRACKET)) {
//        printf(" bracket nesting level: %i\n",
//                symbol_bracket_nesting_level(sym));
//    }
//    else if ((token == TOKEN_OPENING_BRACE) ||
//        (token == TOKEN_CLOSING_BRACE)) {
//        printf(" brace nesting level: %i\n",
//                symbol_brace_nesting_level(sym));
//    } // end if

    return;
} 
// end print_symbol

// END OF FILE
