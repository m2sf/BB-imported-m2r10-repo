/*  Identifiers and macros for testing 7-bit-ASCII characters
 *
 *  ASCII.h
 *
 *  This file ("ASCII.h") was released into the public domain
 *  by Sunrise Telephone Systems KK, Tokyo, Japan.
 *
 */


#ifndef ASCII_H
#define ASCII_H


// ---------------------------------------------------------------------------
// Identifiers for ASCII control characters
// ---------------------------------------------------------------------------

typedef /* ASCII_CONTROL_CHAR */ enum {
    ASCII_NUL	= '\0',
    ASCII_SOH	= '\001',
    ASCII_STX	= '\002',
    ASCII_ETX	= '\003',
    ASCII_EOT	= '\004',
    ASCII_ENQ	= '\005',
    ASCII_ACK	= '\006',
    ASCII_BEL	= '\007',
    ASCII_BS	= '\010',
    ASCII_HT	= '\011',
    ASCII_LF	= '\012',
    ASCII_VT	= '\013',
    ASCII_FF	= '\014',
    ASCII_CR	= '\015',
    ASCII_SO	= '\016',
    ASCII_SI	= '\017',
    ASCII_DLE	= '\020',
    ASCII_DC1	= '\021',
    ASCII_DC2	= '\022',
    ASCII_DC3	= '\023',
    ASCII_DC4	= '\024',
    ASCII_NAK	= '\025',
    ASCII_SYN	= '\026',
    ASCII_ETB	= '\027',
    ASCII_CAN	= '\030',
    ASCII_EM	= '\031',
    ASCII_SUB	= '\032',
    ASCII_ESC	= '\033',
    ASCII_FS	= '\034',
    ASCII_GS	= '\035',
    ASCII_RS	= '\036',
    ASCII_US	= '\037',
    ASCII_DEL	= '\177'
} ASCII_CONTROL_CHAR;

// Aliases
#define TAB                 ASCII_HT
#define ASCII_TAB           ASCII_HT
#define TABULATOR           ASCII_HT
#define EOL                 ASCII_LF
#define CSTRING_TERMINATOR  ASCII_NUL
#define END_OF_STRING       ASCII_NUL
#define NEWLINE             ASCII_LF
#define LINEFEED            ASCII_LF
#define LINE_FEED           ASCII_LF
#define CARRIAGE_RETURN     ASCII_CR
#define XON                 ASCII_DC1
#define ASCII_XON           ASCII_DC1
#define XOFF                ASCII_DC3
#define ASCII_XOFF          ASCII_DC3
#define ESCAPE              ASCII_ESC


// ---------------------------------------------------------------------------
// Identifiers for whitespace character
// ---------------------------------------------------------------------------

#define ASCII_SP        '\040'
#define WHITESPACE      ASCII_SP


// ---------------------------------------------------------------------------
// Identifiers for ASCII printable characters
// ---------------------------------------------------------------------------

typedef /* ASCII_PRINTABLE_CHAR */ enum {
    EXCLAMATION         = '!',
    DOUBLE_QUOTE        = '"',
    NUMBER_SIGN         = '#',
    DOLLAR              = '$',
    PERCENT             = '%',
    AMPERSAND           = '&',
    SINGLE_QUOTE        = '\'',
    OPENING_PARENTHESIS = '(',
    CLOSING_PARENTHESIS = ')',
    ASTERISK            = '*',
    PLUS                = '+',
    COMMA               = ',',
    MINUS               = '-',
    DOT                 = '.',
    FORWARD_SLASH       = '/',
    DIGIT_ZERO          = '0',
    DIGIT_ONE           = '1',
    DIGIT_TWO           = '2',
    DIGIT_THREE         = '3',
    DIGIT_FOUR          = '4',
    DIGIT_FIVE          = '5',
    DIGIT_SIX           = '6',
    DIGIT_SEVEN         = '7',
    DIGIT_EIGHT         = '8',
    DIGIT_NINE          = '9',
    COLON               = ':',
    SEMICOLON           = ';',
    LESS_THAN           = '<',
    EQUAL_SIGN          = '=',
    GREATER_THAN        = '>',
    QUESTION_MARK       = '?',
    AT_SIGN             = '@',
    UPPERCASE_A         = 'A',
    UPPERCASE_B         = 'B',
    UPPERCASE_C         = 'C',
    UPPERCASE_D         = 'D',
    UPPERCASE_E         = 'E',
    UPPERCASE_F         = 'F',
    UPPERCASE_G         = 'G',
    UPPERCASE_H         = 'H',
    UPPERCASE_I         = 'I',
    UPPERCASE_J         = 'J',
    UPPERCASE_K         = 'K',
    UPPERCASE_L         = 'L',
    UPPERCASE_M         = 'M',
    UPPERCASE_N         = 'N',
    UPPERCASE_O         = 'O',
    UPPERCASE_P         = 'P',
    UPPERCASE_Q         = 'Q',
    UPPERCASE_R         = 'R',
    UPPERCASE_S         = 'S',
    UPPERCASE_T         = 'T',
    UPPERCASE_U         = 'U',
    UPPERCASE_V         = 'V',
    UPPERCASE_W         = 'W',
    UPPERCASE_X         = 'X',
    UPPERCASE_Y         = 'Y',
    UPPERCASE_Z         = 'Z',
    OPENING_BRACKET     = '[',
    BACK_SLASH          = '\\',
    CLOSING_BRACKET     = ']',
    CARET               = '^',
    UNDERSCORE          = '_',
    BACK_QUOTE          = '`',
    LOWERCASE_A         = 'a',
    LOWERCASE_B         = 'b',
    LOWERCASE_C         = 'c',
    LOWERCASE_D         = 'd',
    LOWERCASE_E         = 'e',
    LOWERCASE_F         = 'f',
    LOWERCASE_G         = 'g',
    LOWERCASE_H         = 'h',
    LOWERCASE_I         = 'i',
    LOWERCASE_J         = 'j',
    LOWERCASE_K         = 'k',
    LOWERCASE_L         = 'l',
    LOWERCASE_M         = 'm',
    LOWERCASE_N         = 'n',
    LOWERCASE_O         = 'o',
    LOWERCASE_P         = 'p',
    LOWERCASE_Q         = 'q',
    LOWERCASE_R         = 'r',
    LOWERCASE_S         = 's',
    LOWERCASE_T         = 't',
    LOWERCASE_U         = 'u',
    LOWERCASE_V         = 'v',
    LOWERCASE_W         = 'w',
    LOWERCASE_X         = 'x',
    LOWERCASE_Y         = 'y',	
    LOWERCASE_Z         = 'z',	
    OPENING_BRACE       = '{',
    VERTICAL_BAR        = '|',
    CLOSING_BRACE       = '}',
    TILDE               = '~'
} ASCII_PRINTABLE_CHAR;


// Aliases
#define HASH                     NUMBER_SIGN
#define OCTOTHORPE               NUMBER_SIGN
#define APOSTROPHE               SINGLE_QUOTE
#define SINGLEQUOTE              SINGLE_QUOTE
#define DASH                     MINUS
#define HYPHEN                   MINUS
#define DOUBLEQUOTE              DOUBLE_QUOTE
#define OPENING_PAREN            OPENING_PARENTHESIS
#define CLOSING_PAREN            CLOSING_PARENTHESIS
#define SLASH                    FORWARD_SLASH
#define BACKSLASH                BACK_SLASH
#define BACKQUOTE                BACK_QUOTE
#define OPENING_ANGULAR_BRACKET  LESS_THAN
#define CLOSING_ANGULAR_BRACKET  GREATER_THAN


// ---------------------------------------------------------------------------
// Macros
// ---------------------------------------------------------------------------

// test for 7-bit
#define IS_7BIT_ASCII(x) \
    ((x & 0x80) == 0)
#define IS_NOT_7BIT_ASCII(x) \
    ((x & 0x80) != 0)

// test control chars
#define IS_CONTROL(x) \
    ((x < 32) || (x == 127))
#define IS_NOT_CONTROL(x) \
    ((x > 31) && (x != 127))

// test spaces and tabs
#define IS_WHITESPACE_OR_TAB(x)	\
    ((x == 32) || (x == 9))
#define IS_NOT_WHITESPACE_NOR_TAB(x) \
    ((x != 32) && (x != 9))

// test if printable 7-bit
#define IS_PRINTABLE_7BIT_ASCII(x) \
    ((IS_7BIT_ASCII(x)) && (IS_NOT_CONTROL(x)))

// test booleans
#define IS_BOOLEAN(x) \
    ((x == 0) || (x == 1) || \
     (x == 'y') || (x == 'n') || (x == 't') || (x == 'f'))

// test digits
#define IS_BINARY(x) \
    ((x == '0') || (x == '1'))
#define IS_NOT_BINARY(x) \
    ((x > '1') || (x < '0'))
#define IS_0_TO_3(x) \
    ((x >= '0') && (x <= '3'))
#define IS_NOT_0_TO_3(x) \
    ((x < '0') || (x > '3'))
#define IS_DIGIT(x) \
    ((x >= '0') && (x <= '9'))
#define IS_NOT_DIGIT(x) \
    ((x < '0') || (x > '9'))
#define IS_OCTAL(x) \
    ((x >= '0') && (x <= '7'))
#define IS_NOT_OCTAL(x) \
    ((x < '0') || (x > '7'))
#define IS_HEX(x) \
    (((x >= '0') && (x <= '9')) || \
     ((x >= 'a') && (x <= 'f')) || ((x >= 'A') && (x <= 'F')))
#define IS_NOT_HEX(x) \
    ((x < '0') || (x > 'f') || \
     ((x > '9') && (x < 'A')) || ((x > 'F') && (x < 'a')))
#define IS_LOWERHEX(x) \
    (((x >= '0') && (x <= '9')) || ((x >= 'a') && (x <= 'f')))
#define IS_NOT_LOWERHEX(x) \
    ((x < '0') || (x > 'f') || ((x > '9') && (x < 'a')))
#define IS_UPPERHEX(x) \
    (((x >= '0') && (x <= '9')) || ((x >= 'A') && (x <= 'F')))
#define IS_NOT_UPPERHEX(x) \
    ((x < '0') || (x > 'F') || ((x > '9') && (x < 'A')))
#define IS_A_TO_F(x) \
    ((x >= 'A') && (x <= 'F'))
#define IS_NOT_A_TO_F(x) \
    ((x < 'A') || (x > 'F'))
#define IS_a_TO_f(x) \
    ((x >= 'a') && (x <= 'f'))
#define IS_NOT_a_TO_f(x) \
    ((x < 'a') || (x > 'f'))
#define IS_UPPERCASE_A_TO_F IS_A_TO_F
#define IS_NOT_UPPERCASE_A_TO_F IS_NOT_A_TO_F
#define IS_LOWERCASE_A_TO_F IS_a_TO_f
#define IS_NOT_LOWERCASE_A_TO_F IS_NOT_a_TO_f

// test letters
#define IS_LOWERCASE(x) \
    ((x >= 'a') && (x <= 'z'))
#define IS_NOT_LOWERCASE(x) \
    (x < 'a') || (x > 'z')
#define IS_UPPERCASE(x) \
    ((x >= 'A') && (x <= 'Z'))
#define IS_NOT_UPPERCASE(x) \
    (x < 'A') || (x > 'Z')
#define IS_LETTER(x) \
    (((x >= 'a') && (x <= 'z')) || ((x >= 'A') && (x <= 'Z')))
#define IS_NOT_LETTER(x) \
    ((x < 'A') || (x > 'z') || ((x > 'Z') && (x < 'a')))

// test DTMF
#define IS_DTMF(x) \
    (((x >= '0') && (x <= '9')) || (x == '*') || (x == '#'))
#define IS_NOT_DTMF(x) \
    ((x > '9') || ((x < '0') && (x != '*') && (x != '#')))

// test alphanumeric
#define IS_ALPHANUM(x) \
    (((x >= '0') && (x <= '9')) || \
     ((x >= 'a') && (x <= 'z')) || ((x >= 'A') && (x <= 'Z')))
#define IS_NOT_ALPHANUM(x) \
    ((x < '0') || (x > 'z') || \
     ((x > '9') && (x < 'A')) || ((x > 'Z') && (x < 'a')))

// tests for identifiers
#define IS_UNDERSCORE_OR_ALPHANUM(x) \
    ((x == UNDERSCORE) || (IS_ALPHANUM(x))
#define IS_NOT_UNDERSCORE_NOR_ALPHANUM(x) \
    ((x < '0') || (x > 'z') || ((x > '9') && (x < 'A')) || ((x > 'Z') && \
     (x < UNDERSCORE)) || (x == BACK_QUOTE))

#endif /* ASCII_H */

// END OF FILE