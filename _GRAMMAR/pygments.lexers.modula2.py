# -*- coding: utf-8 -*-
"""
    pygments.lexers.modula2
    ~~~~~~~~~~~~~~~~~~~~~~~

    Multi-Dialect Lexer for Modula-2.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import RegexLexer, include
from pygments.util import get_bool_opt, get_list_opt
from pygments.token import Text, Comment, Operator, Keyword, Name, \
    String, Number, Punctuation, Error

__all__ = ['Modula2Lexer']


# Multi-Dialect Modula-2 Lexer
class Modula2Lexer(RegexLexer):
    """
    For `Modula-2 <http://www.modula2.org/>`_ source code.
    
    The Modula-2 lexer supports several dialects.  By default, it operates in
    fallback mode, recognising the *combined* literals, punctuation symbols
    and operators of all supported dialects, and the *combined* reserved words
    and builtins of PIM Modula-2, ISO Modula-2 and Modula-2 R10, while not
    differentiating between library defined identifiers.
    
    To select a specific dialect, a dialect option may be passed
    or a dialect tag may be embedded into a source file.
    
    Dialect Options:
    
    `m2pim`
        Select PIM Modula-2 dialect.
    `m2iso`
        Select ISO Modula-2 dialect.
    `m2r10`
        Select Modula-2 R10 dialect.
    `objm2`
        Select Objective Modula-2 dialect.
    
    The PIM and ISO dialect options may be qualified with a language extension.
    
    Language Extensions:

    `+gm2`
        Select GNU Modula-2 extensions, available with m2pim and m2iso.
    `+mocka`
        Select MOCKA extensions, available with m2pim only.
    `+aglet`
        Select Aglet Modula-2 extensions, available with m2iso only.
    `+gpm`
        Select Gardens Point Modula-2 extensions, available with m2iso.
    `+p1`
        Select p1 Modula-2 extensions, available with m2iso only.
    `+sbu`
        Select Stony Brook Modula-2 extensions, available with m2iso only.
    `+xds`
        Select XDS Modula-2 extensions, available with m2iso only.


    Passing a Dialect Option via Unix Commandline Interface
    
    Dialect options may be passed to the lexer using the `dialect` key.
    Only one such option should be passed. If multiple dialect options are
    passed, the first valid option is used, any subsequent options are ignored.
    
    Examples:
        
    `$ pygmentize -O full,dialect=m2iso -f html -o /path/to/output /path/to/input`
        Use ISO dialect to render input to HTML output
    `$ pygmentize -O full,dialect=m2iso+p1 -f rtf -o /path/to/output /path/to/input`
        Use ISO dialect with p1 extensions to render input to RTF output

    
    Embedding a Dialect Option within a source file
    
    A dialect option may be embedded in a source file in form of a dialect
    tag, a specially formatted comment that specifies a dialect option.
    
    Dialect Tag EBNF:
        
    dialectTag :
        OpeningCommentDelim Prefix dialectOption ClosingCommentDelim ;
    
    dialectOption :
        baseDialect ( '+' languageExtension )? '
    
    baseDialect : 'm2pim' | 'm2iso' | 'm2r10' | 'objm2' ;
    
    languageExtension :
        'gm2' | 'mocka' | 'aglet' | 'gpm' | 'p1' |'sbu' | 'xds' ;

    Prefix : '!' ;

    OpeningCommentDelim : '(*' ;

    ClosingCommentDelim : '*)' ;
    
    No whitespace is permitted between the tokens of a dialect tag.
    
    In the event that a source file contains multiple dialect tags, the first
    tag that contains a valid dialect option will be used and any subsequent
    dialect tags will be ignored.  Ideally, a dialect tag should be placed
    at the beginning of a source file.
    
    An embedded dialect tag overrides a dialect option set via command line.
    
    Examples:
        
    `(*!m2r10*) DEFINITION MODULE Foobar; ...`
        Use Modula2 R10 dialect to render this source file.
    `(*!m2pim+gm2*) DEFINITION MODULE Bazbam; ...`
        Use PIM dialect with GNU extensions to render this source file.
    
    
    Algol Publication Mode:
    
    In Algol publication mode, source text is rendered for publication of
    algorithms in scientific papers and academic texts, following the format
    of the Revised Algol-60 Language Report.  It is activated by passing
    one of two corresponding styles as an option:
        
    `algol`
        render reserved words lowercase underline boldface
        and builtins lowercase boldface italic
    `algol_nu`
        render reserved words lowercase boldface (no underlining)
        and builtins lowercase boldface italic
    
    The lexer automatically performs the required lowercase conversion when
    this mode is activated.
    
    Example:
    
    `$ pygmentize -O full,style=algol -f latex -o /path/to/output /path/to/input`
        Render input file in Algol publication mode to LaTeX output.
    
    
    Rendering Mode of First Class ADT Identifiers:
    
    The rendering of standard library first class ADT identifiers is controlled
    by option flag "treat_stdlib_adts_as_builtins".
    
    When this option is turned on, standard library ADT identifiers are rendered
    as builtins.  When it is turned off, they are rendered as ordinary library
    identifiers.
    
    `treat_stdlib_adts_as_builtins` (default: On)

    The option is useful for dialects that support ADTs as first class objects
    and provide ADTs in the standard library that would otherwise be built-in.
    
    At present, only Modula-2 R10 supports library ADTs as first class objects
    and therefore, no ADT identifiers are defined for any other dialects.
    
    Example:
    
    `$ pygmentize -O full,dialect=m2r10,treat_stdlib_adts_as_builtins=Off ...`
        Render standard library ADTs as ordinary library types.   
    
    
    ISO Modula-2 Specific Lexical Synonyms
    
    Uses of ! and @ as synonyms for | and ^ are rendered as lexical errors.
    
    When Modula-2 was first published, 6-bit character sets had *long* been
    obsolete.  No Modula-2 compiler ever supported any 6-bit character set
    platform.  EBCDIC character sets support either vertical bar or broken
    line characters and caret or upwards pointing arrow characters.
    There was never any need for the ISO M2 working group to define these
    synonyms nor has there ever been any need to use them.  Their use is
    discouraged.  Both synonyms will thus be rendered as lexical errors.
    
    .. versionadded:: 1.3
    """

#  M e t a d a t a

    name = 'Modula-2'
    aliases = ['modula2', 'm2']
    filenames = ['*.def', '*.mod']
    mimetypes = ['text/x-modula2']

    flags = re.MULTILINE | re.DOTALL
    
#  T o k e n s   F o r   A l l   D i a l e c t s

#  This lexer recognises lexemes for all supported dialects and tokenises them.
#  Tokens that are not supported for the targeted dialect are then marked as
#  error tokens after the initial tokenisation. This has the advantage that
#  additional dialects can be more easily added. Furthermore, unrecognised
#  lexemes are marked in the output as errors in their entirety, not simply
#  the first one or two unrecognised characters.

    tokens = {
        'whitespace': [
            (r'\n+', Text),  # blank lines
            (r'\s+', Text),  # whitespace
        ],
        'dialecttags': [
            # PIM Dialect Tag
            (r'\(\*!m2pim\*\)', Comment.Special.DialectTag), 
            # ISO Dialect Tag
            (r'\(\*!m2iso\*\)', Comment.Special.DialectTag), 
            # M2R10 Dialect Tag
            (r'\(\*!m2r10\*\)', Comment.Special.DialectTag), 
            # ObjM2 Dialect Tag
            (r'\(\*!objm2\*\)', Comment.Special.DialectTag), 
            # PIM + GNU Extensions Dialect Tag
            (r'\(\*!m2pim\+gm2\*\)', Comment.Special.DialectTag), 
            # PIM + MOCKA Extensions Dialect Tag
            (r'\(\*!m2pim\+mocka\*\)', Comment.Special.DialectTag), 
            # ISO + Aglet Extensions Dialect Tag
            (r'\(\*!m2iso\+aglet\*\)', Comment.Special.DialectTag), 
            # ISO + GNU Extensions Dialect Tag
            (r'\(\*!m2iso\+gm2\*\)', Comment.Special.DialectTag), 
            # ISO + Gardens Point Extensions Dialect Tag
            (r'\(\*!m2pim\+gpm\*\)', Comment.Special.DialectTag), 
            # ISO + p1 Extensions Dialect Tag
            (r'\(\*!m2iso\+p1\*\)', Comment.Special.DialectTag), 
            # ISO + Stony Brook Extensions Dialect Tag
            (r'\(\*!m2iso\+sbu\*\)', Comment.Special.DialectTag), 
            # ISO + XDS Extensions Dialect Tag
            (r'\(\*!m2iso\+xds\*\)', Comment.Special.DialectTag), 
            # Insert Dialect Into Comment
            (r'\(\*\?.*?\?\*\)', Comment.Special.DialectMacro),
        ],
        'identifiers': [
            # VMS names, with leading %
            (r'%[$%_a-zA-Z0-9]*', Name.VMS),
            # VMS names, with middle or trailing %
            (r'[$%_a-zA-Z][$%_a-zA-Z0-9]*%[$%_a-zA-Z0-9]*', Name.VMS),
            
            # POSIX names, with leading $
            (r'[$][$_a-zA-Z0-9]*', Name.Posix),
            # POSIX names, with middle or trailing $
            (r'[$_a-zA-Z][$_a-zA-Z0-9]*[$][$_a-zA-Z0-9]*', Name.Posix),
            
            # Alpha-Numeric names, with leading _
            (r'_[_a-zA-Z0-9]*', Name.AlphanumAndLowline),
            
            # Alpha-Numeric names, with non-consecutive middle _
            (r'[a-zA-Z][a-zA-Z0-9]*' # leader \
             r'(_[a-zA-Z0-9]+)+', Name.AlphanumAndMiddleLowline),
            
            # Alpha-Numeric names, with middle or trailing _
            (r'[_a-zA-Z][_a-zA-Z0-9]*' # leader \
             r'_[_a-zA-Z0-9]*', Name.AlphanumAndLowline),
            
            # Alpha-Numeric names, without $, % or _
            (r'[a-zA-Z][a-zA-Z0-9]*', Name.Alphanum),
            
            # M2 R10 Template Engine placeholders
            (r'##[a-zA-Z][a-zA-Z0-9]*##', Name.Placeholder),
        ],
        'prefixed_number_literals': [
            # Base-2, whole number
            (r'0b[01]+(\'[01]+)*', Number.Bin.Prefixed),
            
            # Base-16, whole number
            (r'0[ux][0-9A-F]+(\'[0-9A-F]+)*', Number.Hex.Prefixed),
        ],
        'non_affixed_number_literals': [
            # Base-10, real number with exponent
            (r'[0-9]+' # integral part \
             r'\.[0-9]+' # fractional part \
             r'E[+-]?[0-9]+', # exponent \
             Number.Float.NonAffixed.Unformatted.Exponent.UpperE),
            
            # Base-10, same as above with lowercase e
            (r'[0-9]+' # integral part \
             r'\.[0-9]+' # fractional part \
             r'e[+-]?[0-9]+', # exponent \
             Number.Float.NonAffixed.Unformatted.Exponent.LowerE),
            
            # Same as above, with digit separators
            (r'[0-9]+(\'[0-9]+)*' # integral part \
             r'\.[0-9]+(\'[0-9]+)*' # fractional part \
             r'e[+-]?[0-9]+(\'[0-9]+)*', # exponent \
             Number.Float.NonAffixed.DigitGrouped),
            
            # Base-10, real number without exponent
            (r'[0-9]+' # integral part \
             r'\.[0-9]+', # fractional part \
             Number.Float.NonAffixed.Unformatted.NoExponent),
            
            # same as above, with digit separators
            (r'[0-9]+(\'[0-9]+)*' # integral part \
             r'\.[0-9]+(\'[0-9]+)*', # fractional part \
             Number.Float.NonAffixed.DigitGrouped),
            
            # Base-10, whole number
            (r'[0-9]+(\'[0-9]+)+', Number.Integer.NonAffixed.DigitGrouped),
            # Same as above, without digit separators
            (r'[0-9]+', Number.Integer.NonAffixed.Unformatted),
        ],
        'suffixed_number_literals': [
            # Base-8, whole number
            (r'[0-7]+B', Number.Oct.Suffixed),
            
            # Base-8, character code
            (r'[0-7]+C', Number.Oct.Suffixed),
            
            # Base-16, number
            (r'[0-9A-F]+H', Number.Hex.Suffixed),
            
            # Base-10, real number
            (r'[0-9]+\.[0-9]+([eE][+-]?[0-9]+)*\$', Number.Float.DollarSuffixed),
        ],
        'string_literals': [
            (r"'[^']*'", String),  # single quoted string
            (r'"[^"]*"', String),  # double quoted string
        ],
        'schroedinger_digraphs': [
            # digraphs that could be either operators or punctuation
            # depending on context that cannot be determined by regex
            
            # diamond
            #  operator synonym in PIM and ISO
            #  punctuation in M2R10 and ObjM2
            (r'<>', Schroedinger),
        ],
        'schroedinger_unigraphs': [
            # unigraphs that could be either operators or punctuation
            # depending on context that cannot be determined by regex
            
            # tilde
            #  operator synonym in PIM and ISO
            #  punctuation in M2R10 and ObjM2
            (r'~', Schroedinger),
        ],
        'digraph_punctuation': [
            # Assignment Symbol
            (r':=', Punctuation),
            
            # Range Constructor
            (r'\.\.', Punctuation),
            
            # Ascender/Increment and Descender/Decrement
            (r'\+\+', Punctuation), # M2R10 + ObjM2
            (r'--', Punctuation), # M2R10 + ObjM2
            
            # Chevron Brackets
            (r'<<', Punctuation), # M2R10 + ObjM2
            (r'>>', Punctuation), # M2R10 + ObjM2
            
            # Blueprint Punctuation
            (r':,', Punctuation), # M2R10 + ObjM2
            (r':*', Punctuation), # M2R10 + ObjM2
            (r':#', Punctuation), # M2R10 + ObjM2
            (r'->', Punctuation), # M2R10 + ObjM2
            (r'><', Punctuation), # M2R10 + ObjM2
            
            # Template Punctuation
            (r'##', Punctuation), # M2R10 + ObjM2
            (r'@@', Punctuation), # M2R10 + ObjM2
            (r'<#', Punctuation), # M2R10 + ObjM2
            (r'#>', Punctuation), # M2R10 + ObjM2
            
            # Synonym Braces
            (r'\(\.', Punctuation), # PIM + ISO
            (r'\.\)', Punctuation), # PIM + ISO
            
            # Synonym Brackets
            (r'\(:', Punctuation), # PIM + ISO
            (r':\)', Punctuation), # PIM + ISO
        ],
        'unigraph_punctuation': [
            # Common Punctuation
            (r'[\(\)\[\]{},.:;\|]', Punctuation),
            
            # Unqualified Import/Alias Wildcard *
            (r'\*(?=;)', Punctuation),
            (r'\*(?= ;)', Punctuation),
            
            # Re-Export Suffix +
            (r'\+(?=([,;]))', Punctuation),
            (r'\+(?= ;)', Punctuation),
            
            # Treat Characters Within Sequence "[+/-]" as Punctuation
            (r'\+(?=/-)', Punctuation),
            (r'/(?=-])', Punctuation),
            (r'-(?=])', Punctuation),
            
            # Pragma value query prefix
            (r'\?', Punctuation),
            
            # extension RW/name prefix
            (r'@', Punctuation),
        ],
        'digraph_operators': [
            # Exponentiation Operator
            (r'\*\*', Operator),
            
            # Relational Operators
            (r'<=', Operator),
            (r'>=', Operator),
            
            # Identity Operator
            (r'==', Operator), # M2R10 + ObjM2
            
            # Type Conversion Operator
            (r'::', Operator), # M2R10 + ObjM2
        ],
        'unigraph_operators': [
            # Arithmetic Operators
            (r'[+-/]', Operator),
            (r'\*', Operator),
            
            # ISO 80000-2 compliant Set Difference Operator
            (r'\\', Operator), # M2R10 + ObjM2 
            
            # Relational Operators
            (r'[=#<>]', Operator),
            
            # Ampersand Operator
            (r'&', Operator),
            
            # Dereferencing Operator
            (r'\^', Operator),
            
            # Smalltalk Message Prefix
            (r'`', Operator), # ObjM2
        ],
        'special_comments': [
            # Copyright Comment
            (r'\(\* Copyright .*?\*\)', Comment.Special.Copyright),
            (r'\(\* \([cC]\) *[12][0-9]{3} .*?\*\)', Comment.Special.Copyright),
            
            # Title Comment
            (r'\(\*# .*? #\*\)', Comment.Special.Title),
            
            # Headline H1 Comment
            (r'\(\*= .*? =\*\)', Comment.Special.Headline.One),
            
            # Headline H2 Comment
            (r'\(\*- .*? -\*\)', Comment.Special.Headline.Two),
            
            # Headline H3 Comment
            (r'\(\*_ .*? _\*\)', Comment.Special.Headline.Three),
            
            # HeaderDoc Comment
            (r'\(\*! .*?\*\)', Comment.Special.HeaderDoc),
            
            # Doxygen Comment
            (r'^![<!>].*?\n', Comment.Single.Doxygen),
        ],
        'comments': [
            # Pascal Style Block Comment
            (r'\(\*([^$].*?)\*\)', Comment.Multiline.PascalStyle),
            
            # C Style Block Comment
            (r'/\*(.*?)\*/', Comment.Multiline.CStyle),
            
            # BCPL Style Single Line Comment
            (r'^//.*?\n', Comment.Single.BcplStyle.StartOfLineOnly),
            (r'(?<=;)//.*?\n', Comment.Single.BcplStyle),
            (r'(?<=; )//.*?\n', Comment.Single.BcplStyle),
            (r'(?<=;\t)//.*?\n', Comment.Single.BcplStyle),
            (r'(?<=;\t\t)//.*?\n', Comment.Single.BcplStyle),
            (r'(?<=;\t\t\t)//.*?\n', Comment.Single.BcplStyle),
            (r'(?<=;\t\t\t\t)//.*?\n', Comment.Single.BcplStyle),

            # Fortran Style Single Line Comment
            (r'^!.*?\n', Comment.Single.FortranStyle.StartOfLineOnly),
            (r'(?<=;)!.*?\n', Comment.Single.FortranStyle),
            (r'(?<=; )!.*?\n', Comment.Single.FortranStyle),
            (r'(?<=;\t)!.*?\n', Comment.Single.FortranStyle),
            (r'(?<=;\t\t)!.*?\n', Comment.Single.FortranStyle),
            (r'(?<=;\t\t\t)!.*?\n', Comment.Single.FortranStyle),
            (r'(?<=;\t\t\t\t)!.*?\n', Comment.Single.FortranStyle),

            # Ada Style Single Line Comment
            (r'^--.*?\n', Comment.Single.AdaStyle.StartOfLineOnly),
            (r'(?<=;)--.*?\n', Comment.Single.AdaStyle),
            (r'(?<=; )--.*?\n', Comment.Single.AdaStyle),
            (r'(?<=;\t)--.*?\n', Comment.Single.AdaStyle),
            (r'(?<=;\t\t)--.*?\n', Comment.Single.AdaStyle),
            (r'(?<=;\t\t\t)--.*?\n', Comment.Single.AdaStyle),
            (r'(?<=;\t\t\t\t)--.*?\n', Comment.Single.AdaStyle),
        ],
        'pragmas': [
            # ISO Style Pragmas
            (r'<\*.*?\*>', Comment.Preproc.IsoStyle),
            
            # Pascal Style Pragmas
            (r'\(\*\$.*?\*\)', Comment.Preproc.PascalStyle),
            
            # Stony Brook Conditional Compilation Pragmas
            (r'%(IF|THEN|ELSIF|ELSE|END|NOT|AND|OR)', Comment.Preproc.StonyBrook),
        ],
        'root': [
            # * * * The order of inclusion is semantically significant * * *
            # In general, the order should be from longer to shorter matches.
            # That is to say, of two possible matches with common start
            # symbols, the longer of the two should be tried first.
            include('whitespace'),
            include('dialecttags'),
            include('pragmas'),
            include('special_comments'),
            include('comments'),
            include('identifiers'),
            include('suffixed_number_literals'),
            include('prefixed_number_literals'),
            include('non_affixed_number_literals'),
            include('string_literals'),
            include('schroedinger_digraphs'),
            include('digraph_punctuation'),
            include('digraph_operators'),
            include('schroedinger_unigraphs'),
            include('unigraph_punctuation'),
            include('unigraph_operators'),
        ]
    }
    
#  C o m m o n   D a t a s e t s
    
    # Common Punctuation Dataset
    common_punctuation = (
        ',', '.', ':', ';', '|', '..', ':=', '(', ')', '[', ']', '{', '}',
    )

    # Common Operators Dataset
    common_operators = (
        '+', '-', '*', '/', '&', '=', '#', '<', '>', '<=', '>=', '^',
    )

    # Common Comments and Pragmas Dataset
    common_comments_and_pragmas = (
        Comment.Multiline.PascalStyle,
        Comment.Special.DialectTag,
        Comment.Special.DialectMacro,
        Comment.Special.Copyright,
        Comment.Special.Title,
        Comment.Special.Headline,
        Comment.Special.HeaderDoc,
    )
    
    # Common Literals Dataset
    common_literals = (
        Number.Integer.NonAffixed.Unformatted,
        Number.Float.NonAffixed.Unformatted.NoExponent,
        Number.Float.NonAffixed.Unformatted.Exponent.LowerE,
    )
    
    # Common name recognition Dataset
    common_name_recognition = (
        Name.Alphanum,
    )
    
    # Common Reserved Words Dataset
    common_reserved_words = (
        # 37 common reserved words
        'AND', 'ARRAY', 'BEGIN', 'BY', 'CASE', 'CONST', 'DEFINITION', 'DIV',
        'DO', 'ELSE', 'ELSIF', 'END', 'EXIT', 'FOR', 'FROM', 'IF',
        'IMPLEMENTATION', 'IMPORT', 'IN', 'LOOP', 'MOD', 'MODULE', 'NOT',
        'OF', 'OR', 'POINTER', 'PROCEDURE', 'RECORD', 'REPEAT', 'RETURN',
        'SET', 'THEN', 'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE',
    )

    # Common Builtins Dataset
    common_builtins = (
        # 16 common builtins
        'ABS', 'BOOLEAN', 'CARDINAL', 'CHAR', 'CHR', 'FALSE', 'INTEGER',
        'LONGINT', 'LONGREAL', 'MAX', 'MIN', 'NIL', 'ODD', 'ORD', 'REAL', 
        'TRUE',
    )
    
    # Common Pseudo-Module Builtins Dataset
    common_pseudo_builtins = (
        # 3 common pseudo builtins
        'ADDRESS', 'WORD', 'ADR'
    )
    
#  P I M   M o d u l a - 2   D a t a s e t s

    # PIM Modula-2 punctuation in addition to the common set
    pim_additional_punctuation = (
        '(.', '.)', '(:', ':)',
    )

    # PIM Modula-2 operators in addition to the common set
    pim_additional_operators = (
        '~', '<>',
    )
    
    # PIM Modula-2 literals in addition to the common set
    pim_additional_literals = (
        Number.Oct.Suffixed, Number.Hex.Suffixed,
        Number.Float.NonAffixed.Unformatted.Exponent.UpperE,
    )
    
    # PIM Modula-2 comments and pragmas in addition to the common set
    pim_additional_comments_and_pragmas = (
        Comment.Preproc.PascalStyle,
    )
    
    # PIM Modula-2 name recognition in addition to the common set
    pim_additional_name_recognition = (
        # None
    )
    
    # PIM Modula-2 reserved words in addition to the common set
    pim_additional_reserved_words = (
        # 3 additional reserved words
        'EXPORT', 'QUALIFIED', 'WITH',
    )
    
    # PIM Modula-2 builtins in addition to the common set
    pim_additional_builtins = (
        # 16 additional builtins
        'BITSET', 'CAP', 'DEC', 'DISPOSE', 'EXCL', 'FLOAT', 'HALT', 'HIGH',
        'INC', 'INCL', 'NEW', 'NIL', 'PROC', 'SIZE', 'TRUNC', 'VAL',
    )
    
    # PIM Modula-2 pseudo-builtins in addition to the common set
    pim_additional_pseudo_builtins = (
        # 6 additional pseudo builtins
        'SYSTEM', 'PROCESS', 'TSIZE', 'NEWPROCESS', 'IOTRANSFER', 'TRANSFER',
    )
    
#  I S O   M o d u l a - 2   D a t a s e t s
    
    # ISO Modula-2 punctuation in addition to the common set
    iso_additional_punctuation = (
        '(.', '.)', '(:', ':)',
    )

    # ISO Modula-2 operators in addition to the common set
    iso_additional_operators = (
        '~', '<>',
    )
    
    # ISO Modula-2 literals in addition to the common set
    iso_additional_literals = (
        Number.Oct.Suffixed, Number.Hex.Suffixed,
        Number.Float.NonAffixed.Unformatted.Exponent.UpperE,
    )
    
    # ISO Modula-2 comments and pragmas in addition to the common set
    iso_additional_comments_and_pragmas = (
        Comment.Preproc.IsoStyle,
    )
    
    # ISO Modula-2 name recognition in addition to the common set
    iso_additional_name_recognition = (
        Name.AlphanumAndLowline, Name.AlphanumAndMiddleLowline,
    )
    
    # ISO Modula-2 reserved words in addition to the common set
    iso_additional_reserved_words = (
        # 9 additional reserved words (ISO 10514-1)
        'EXCEPT', 'EXPORT', 'FINALLY', 'FORWARD', 'PACKEDSET', 'QUALIFIED',
        'REM', 'RETRY', 'WITH',
        # 10 additional reserved words (ISO 10514-2 & ISO 10514-3)
        'ABSTRACT', 'AS', 'CLASS', 'GUARD', 'INHERIT', 'OVERRIDE', 'READONLY',
        'REVEAL', 'TRACED', 'UNSAFEGUARDED',
    )
    
    # ISO Modula-2 builtins in addition to the common set
    iso_additional_builtins = (
        # 26 additional builtins (ISO 10514-1)
        'BITSET', 'CAP', 'CMPLX', 'COMPLEX', 'DEC', 'DISPOSE', 'EXCL', 'FLOAT',
        'HALT', 'HIGH', 'IM', 'INC', 'INCL', 'INT', 'INTERRUPTIBLE',  'LENGTH',
        'LFLOAT', 'LONGCOMPLEX', 'NEW', 'PROC', 'PROTECTION', 'RE', 'SIZE',
        'TRUNC', 'UNINTERRUBTIBLE', 'VAL',
        # 5 additional builtins (ISO 10514-2 & ISO 10514-3)
        'CREATE', 'DESTROY', 'EMPTY', 'ISMEMBER', 'SELF', 
    )

    # ISO Modula-2 pseudo-builtins in addition to the common set
    iso_additional_pseudo_builtins = (
        # 14 additional builtins (SYSTEM)
        'SYSTEM', 'BITSPERLOC', 'BYTE', 'LOCSPERBYTE', 'LOCSPERWORD', 'LOC',
        'ADDADR', 'SUBADR', 'DIFADR', 'MAKEADR',
        'ROTATE', 'SHIFT', 'CAST', 'TSIZE',
        # 13 additional builtins (COROUTINES)
        'COROUTINES', 'ATTACH', 'COROUTINE', 'CURRENT', 'DETACH', 'HANDLER',
        'INTERRUPTSOURCE', 'IOTRANSFER', 'IsATTACHED', 'LISTEN',
        'NEWCOROUTINE', 'PROT', 'TRANSFER',  
        # 9 additional builtins (EXCEPTIONS)
        'EXCEPTIONS', 'AllocateSource', 'CurrentNumber', 'ExceptionNumber',
        'ExceptionSource', 'GetMessage', 'IsCurrentSource',
        'IsExceptionalExecution', 'RAISE',
        # 3 additional builtins (TERMINATION)
        'TERMINATION', 'IsTerminating', 'HasHalted',
        # 4 additional builtins (M2EXCEPTION)
        'M2EXCEPTION', 'M2Exceptions', 'M2Exception', 'IsM2Exception',
        'indexException', 'rangeException', 'caseSelectException',
        'invalidLocation', 'functionException', 'wholeValueException',
        'wholeDivException', 'realValueException', 'realDivException',
        'complexValueException', 'complexDivException', 'protException',
        'sysException', 'coException', 'exException',
    )
    
#  M o d u l a - 2   R 1 0   D a t a s e t s
    
    # Modula-2 R10 punctuation in addition to the common set
    m2r10_additional_punctuation = (
        # common
        '+', '*', '~',
        # definition specific
        '<<', '>>',
        # implementation specific
        '++', '--',
        # blueprint specific
        ':,', ':*', ':#', '->', '<>', '><',
        # template specific
        '##', '@@', '<#', '#>'
        # pragma specific
        '?',
    )

    # Modula-2 R10 operators in addition to the common set
    m2r10_additional_operators = (
        '\\', '==', '::',
    )

    # Modula-2 R10 literals in addition to the common set
    m2r10_additional_literals = (
        Number.Bin.Prefixed, Number.Hex.Prefixed,
        Number.Integer.NonAffixed,
        Number.Float.NonAffixed.DigitGrouped,
    )
    
    # Modula-2 R10 comments and pragmas in addition to the common set
    m2r10_additional_comments_and_pragmas = (
        Comment.Single.BcplStyle.StartOfLineOnly, Comment.Single.Doxygen,
        Comment.Single.FortranStyle.StartOfLineOnly,
        Comment.Multiline.CStyle,
        Comment.Preproc.IsoStyle,
    )
    
    # Modula-2 R10 name recognition in addition to the common set
    m2r10_additional_name_recognition = (
        Name.AlphanumAndLowline, Name.AlphanumAndMiddleLowline,
        Name.Posix, Name.VMS, Name.Placeholder,
    )
    
    # Modula-2 R10 reserved words in addition to the common set
    m2r10_additional_reserved_words = (
        # 12 additional reserved words
        'ALIAS', 'ARGLIST', 'BLUEPRINT', 'COPY', 'GENLIB', 'NEW',
        'NONE', 'OPAQUE', 'REFERENTIAL', 'RELEASE', 'RETAIN', 'YIELD',
    )

    # Modula-2 R10 builtins in addition to the common set
    m2r10_additional_builtins = (
        # 29 additional builtins
        'APPEND', 'COUNT', 'COROUTINE', 'EMPTY', 'EXISTS', 'FIRST', 'INSERT',
        'LAST', 'LENGTH', 'LONGCARD', 'OCTET', 'PTR', 'PRED', 'READ', 'READNEW',
        'REMOVE', 'RETRIEVE', 'SORT', 'SORTNEW', 'SUCC', 'TLIMIT', 'TMAX',
        'TMIN', 'TODO', 'TRUE', 'TSIZE', 'UNICHAR', 'WRITE', 'WRITEF',
    )
    
    # Modula-2 R10 pseudo-builtins in addition to the common set
    m2r10_additional_pseudo_builtins = (
        # 4 additional builtins (primitives)
        'SEEK', 'STORE', 'SUBSET', 'VALUE',
        # 12 additional builtins (TPROPERTIES)
        'TPROPERTIES', 'PROPERTY', 'LITERAL', 'TPROPERTY', 'TLITERAL',
        'TBUILTIN', 'TDYN', 'TFLAGS', 'TORDERED', 'TREFC', 'TSCALAR', 'TSORTED',
        # 4 additional builtins (CONVERSION)
        'CONVERSION', 'TSXFSIZE', 'SXF', 'VAL',
        # 35 additional builtins (UNSAFE)
        'UNSAFE', 'BYTE', 'CAST', 'INTRINSIC', 'AVAIL', 'ADD', 'SUB', 'ADDC',
        'SUBC', 'FETCHADD', 'FETCHSUB', 'SHL', 'SHR', 'ASHR', 'ROTL', 'ROTR',
        'ROTLC', 'ROTRC', 'BWNOT', 'BWAND', 'BWOR', 'BWXOR', 'BWNAND', 'BWNOR',
        'SETBIT', 'TESTBIT', 'LSBIT', 'MSBIT', 'CSBITS', 'BAIL', 'HALT',
        'TODO', 'FFI', 'ADDR', 'VARGLIST', 'VARGC',
        # 11 additional builtins (ATOMIC)
        'ATOMIC', 'INTRINSIC', 'AVAIL', 'SWAP', 'CAS', 'INC', 'DEC', 'BWAND',
        'BWNAND', 'BWOR', 'BWXOR',
        # 7 additional builtins (COMPILER)
        'COMPILER', 'DEBUG', 'MODNAME', 'PROCNAME', 'LINENUM', 'DEFAULT',
        'HASH',
        # 7 additional builtins (ASSEMBLER)
        'ASSEMBLER', 'ASM', 'REG', 'REGISTER', 'SETREG', 'GETREG', 'CODE',
    )
    
#  O b j e c t i v e   M o d u l a - 2   D a t a s e t s
    
    # ObjM2 punctuation in addition to Modula-2 R10
    objm2_additional_punctuation = (
        '@',
    )

    # ObjM2 operators in addition to Modula-2 R10
    objm2_additional_operators = (
        '`',
    )

    # ObjM2 literals in addition to Modula-2 R10
    objm2_additional_literals = (
        # None
    )
    
    # ObjM2 comments and pragmas in addition to Modula-2 R10
    objm2_additional_comments_and_pragmas = (
        # None
    )
    
    # ObjM2 Modula-2 name recognition in addition to Modula-2 R10
    objm2_additional_name_recognition = (
        # None
    )
    
    # ObjM2 reserved words in addition to Modula-2 R10
    objm2_additional_reserved_words = (
        # 16 additional reserved words
        'BYCOPY', 'BYREF', 'CLASS', 'CONTINUE', 'CRITICAL', 'INOUT', 'METHOD',
        'ON', 'OPTIONAL', 'OUT', 'PRIVATE', 'PROTECTED', 'PROTOCOL', 'PUBLIC',
        'SUPER', 'TRY',
    )

    # ObjM2 builtins in addition to Modula-2 R10
    objm2_additional_builtins = (
        # 3 additional builtins
        'OBJECT', 'NO', 'YES',
    )

    # ObjM2 pseudo-module builtins in addition to Modula-2 R10
    objm2_additional_pseudo_builtins = (
        # None
    )

#  G N U   M o d u l a - 2   D a t a s e t s
    
    # GM2 punctuation in addition to PIM or ISO
    gm2_additional_punctuation = (
        # None
    )

    # GM2 operators in addition to PIM or ISO
    gm2_additional_operators = (
        # None
    )

    # GM2 literals in addition to PIM Modula-2
    gm2_additional_literals = (
        # None
    )
    
    # GM2 comments and pragmas in addition to PIM or ISO
    gm2_additional_comments_and_pragmas = (
        # None
    )
    
    # GM2 name recognition in addition to PIM or ISO
    gm2_additional_name_recognition = (
        Name.AlphanumAndLowline, Name.Posix,
    )
    
    # GM2 reserved words in addition to PIM or ISO
    gm2_additional_reserved_words = (
        # 10 additional reserved words
        'ASM', '__ATTRIBUTE__', '__BUILTIN__', '__COLUMN__', '__DATE__',
        '__FILE__', '__FUNCTION__', '__LINE__', '__MODULE__', 'VOLATILE',
    )

    # GM2 builtins in addition to PIM or ISO
    gm2_additional_builtins = (
        # 21 additional builtins
        'BITSET8', 'BITSET16', 'BITSET32', 'CARDINAL8', 'CARDINAL16',
        'CARDINAL32', 'CARDINAL64', 'COMPLEX32', 'COMPLEX64', 'COMPLEX96',
        'COMPLEX128', 'INTEGER8', 'INTEGER16', 'INTEGER32', 'INTEGER64',
        'REAL8', 'REAL16', 'REAL32', 'REAL96', 'REAL128', 'THROW',
    )

    # GM2 pseudo-module builtins in addition to PIM
    gm2_additional_pseudo_builtins = (
        # 1 additional pseudo-builtin
        'BYTE', 
    )
        
#  M O C K A   M o d u l a - 2   D a t a s e t s
    
    # MOCKA punctuation in addition to PIM Modula-2
    mocka_additional_punctuation = (
        # None
    )

    # MOCKA operators in addition to PIM Modula-2
    mocka_additional_operators = (
        # None
    )

    # MOCKA literals in addition to PIM Modula-2
    mocka_additional_literals = (
        # None
    )
    
    # MOCKA comments and pragmas in addition to PIM Modula-2
    mocka_additional_comments_and_pragmas = (
        # None
    )
    
    # MOCKA name recognition in addition to PIM Modula-2
    mocka_additional_name_recognition = (
        Name.AlphanumAndLowline, Name.Posix,
    )
    
    # MOCKA reserved words in addition to PIM Modula-2
    mocka_additional_reserved_words = (
        # 1 additional reserved word
        'FOREIGN',
    )

    # MOCKA builtins in addition to PIM Modula-2
    mocka_additional_builtins = (
        # 21 additional builtins
        'LONGCARD', 'SHORTCARD', 'SHORTINT',
    )

    # MOCKA pseudo-module builtins in addition to PIM Modula-2
    mocka_additional_pseudo_builtins = (
        # 1 additional pseudo-builtin
        'BYTE', 
    )
    
#  A g l e t   M o d u l a - 2   D a t a s e t s
    
    # Aglet punctuation in addition to ISO Modula-2
    aglet_additional_punctuation = (
        # None
    )

    # Aglet operators in addition to ISO Modula-2
    aglet_additional_operators = (
        # None
    )

    # Aglet literals in addition to ISO Modula-2
    aglet_additional_literals = (
        # None
    )
    
    # Aglet comments and pragmas in addition to ISO Modula-2
    aglet_additional_comments_and_pragmas = (
        # None
    )
    
    # Aglet name recognition in addition to ISO Modula-2
    aglet_additional_name_recognition = (
        Name.Posix,
    )
    
    # Aglet reserved words in addition to ISO Modula-2
    aglet_additional_reserved_words = (
        # None
    )

    # Aglet builtins in addition to ISO Modula-2
    aglet_additional_builtins = (
        # 9 additional builtins
        'BITSET8', 'BITSET16', 'BITSET32', 'CARDINAL8', 'CARDINAL16',
        'CARDINAL32', 'INTEGER8', 'INTEGER16', 'INTEGER32',
    )

    # Aglet pseudo-module builtins in addition to ISO Modula-2
    aglet_additional_pseudo_builtins = (
        # None
    )
	
#  G a r d e n s   P o i n t   M o d u l a - 2   D a t a s e t s
    
    # GPM punctuation in addition to ISO Modula-2
    gpm_additional_punctuation = (
        # None
    )

    # GPM operators in addition to ISO Modula-2
    gpm_additional_operators = (
        # None
    )

    # GPM literals in addition to ISO Modula-2
    gpm_additional_literals = (
        # None
    )
    
    # GPM comments and pragmas in addition to ISO Modula-2
    gpm_additional_comments_and_pragmas = (
        # None
    )
    
    # GPM name recognition in addition to *Common Set*
    gpm_additional_name_recognition = (
        # GPM does not permit leading or trailing or consecutive lowlines
        Name.AlphanumAndMiddleLowline,
    )
    
    # GPM reserved words in addition to ISO Modula-2
    gpm_additional_reserved_words = (
        # 1 additional reserved word
        'FOREIGN',
    )

    # GPM builtins in addition to ISO Modula-2
    gpm_additional_builtins = (
        # 3 additional builtins
        'ABORT', 'SFLOAT', 'SHORTREAL',
    )

    # GPM pseudo-module builtins in addition to ISO Modula-2
    gpm_additional_pseudo_builtins = (
        # 4 additional builtins
        'BIN', 'BYTE', 'NEWPROCESS', 'SAL',
    )
    
#  p 1   M o d u l a - 2   D a t a s e t s
    
    # p1 punctuation in addition to ISO Modula-2
    p1_additional_punctuation = (
        # None
    )

    # p1 operators in addition to ISO Modula-2
    p1_additional_operators = (
        # None
    )

    # p1 literals in addition to ISO Modula-2
    p1_additional_literals = (
        Number.Float.DollarSuffixed,
    )
    
    # p1 comments and pragmas in addition to ISO Modula-2
    p1_additional_comments_and_pragmas = (
        # None
    )
    
    # p1 name recognition in addition to ISO Modula-2
    p1_additional_name_recognition = (
        Name.Posix,
    )
    
    # p1 reserved words in addition to ISO Modula-2
    p1_additional_reserved_words = (
        # None
    )

    # p1 builtins in addition to ISO Modula-2
    p1_additional_builtins = (
        # None
    )

    # p1 pseudo-module builtins in addition to ISO Modula-2
    p1_additional_pseudo_builtins = (
        # 1 additional builtin
        'BCD',
    )
    
#  S t o n y   B r o o k   M o d u l a - 2   D a t a s e t s
    
    # Stony Brook punctuation in addition to ISO Modula-2
    sbu_additional_punctuation = (
        '*',
    )

    # Stony Brook operators in addition to ISO Modula-2
    sbu_additional_operators = (
        # None
    )

    # Stony Brook literals in addition to ISO Modula-2
    sbu_additional_literals = (
        # None
    )
    
    # Stony Brook comments and pragmas in addition to ISO Modula-2
    sbu_additional_comments_and_pragmas = (
        Comment.Preproc.StonyBrook,
    )
    
    # Stony Brook name recognition in addition to ISO Modula-2
    sbu_additional_name_recognition = (
        # None
    )
    
    # Stony Brook reserved words in addition to ISO Modula-2
    sbu_additional_reserved_words = (
        'BIG', 'BITFIELDS', 'BREAK', 'CONTINUE', 'EXCEPT', 'FUNC', 'INOUT',
        'OUT', 'MACRO', 'SMALL',
    )

    # Stony Brook builtins in addition to ISO Modula-2
    sbu_additional_builtins = (
        # 30 additional builtins
        'ACHAR', 'BAND', 'BNOT', 'BOOL8', 'BOOL16', 'BOOL32', 'BOR', 'BYTEBOOL',
        'CARDINAL8', 'CARDINAL16', 'CARDINAL32', 'CARDINAL64', 'DWORDBOOL',
        'INTEGER8', 'INTEGER16', 'INTEGER32', 'INTEGER64', 'LONGCARD',
        'NILPROC', 'ROL', 'ROR', 'SHL', 'SHORTCARD', 'SHORTINT', 'SHR',
        'UCHAR', 'WORDBOOL'
    )
    
    # Stony Brook pseudo-module builtins in addition to ISO Modula-2
    sbu_additional_pseudo_builtins = (
        # 33 additional builtins (SYSTEM)
        'CPUCOUNT', 'EXITCODE', 'BuildNumber', 'DebuggerPresent', 'OFFS',
        'UNREFERENCED_PARAMETER', 'SOURCEFILE', 'SOURCELINE', 'ASSERT',
        'ISASSERT', 'EXCEPTADDR', 'EXCEPT_INFO', 'SetUnhandledExceptionProc',
        'AttachDebugger', 'AttachDebuggerOpt', 'DoNotAttach', 'AttachExternal',
        'AttachAll', 'OutputDebugMessage', 'EnableCallTrace', 'OutputCallTrace',
        'TrapAccessViolations', 'VA_START', 'VA_ARG', 'CLONE', 'FIXME',
        'SWAPENDIAN', 'BIGENDIAN', 'LITTLEENDIAN', 'ATOMIC_CMPXCHG',
        'ATOMIC_XCHG', 'ATOMIC_ADD', 'MEMORY_FENCE',
    )
    
#  X D S   M o d u l a - 2   D a t a s e t s
    
    # XDS punctuation in addition to ISO Modula-2
    xds_additional_punctuation = (
        # None
    )

    # XDS operators in addition to ISO Modula-2
    xds_additional_operators = (
        # None
    )

    # XDS literals in addition to ISO Modula-2
    xds_additional_literals = (
        # None
    )
    
    # XDS comments and pragmas in addition to ISO Modula-2
    xds_additional_comments_and_pragmas = (
        Comment.Single.AdaStyle,
    )
    
    # XDS name recognition in addition to ISO Modula-2
    xds_additional_name_recognition = (
        Name.Posix,
    )
    
    # XDS reserved words in addition to ISO Modula-2
    xds_additional_reserved_words = (
        # 1 additional reserved word
        'SEQ',
    )

    # XDS builtins in addition to ISO Modula-2
    xds_additional_builtins = (
        # 9 additional builtins
        'ASH', 'ASSERT', 'DIFFADR_TYPE', 'ENTIER', 'INDEX', 'LEN',
        'LONGCARD', 'SHORTCARD', 'SHORTINT',
    )
    
    # XDS pseudo-module builtins in addition to ISO Modula-2
    xds_additional_pseudo_builtins = (
        # 22 additional builtins (SYSTEM)
        'PROCESS', 'NEWPROCESS', 'BOOL8', 'BOOL16', 'BOOL32', 'CARD8',
        'CARD16', 'CARD32', 'INT8', 'INT16', 'INT32', 'REF', 'MOVE',
        'FILL', 'GET', 'PUT', 'CC', 'int', 'unsigned', 'size_t', 'void',
        # 3 additional builtins (COMPILER)
        'COMPILER', 'OPTION', 'EQUATION',
    )

#  P I M   S t a n d a r d   L i b r a r y   D a t a s e t s

    # PIM Modula-2 Standard Library Modules Dataset
    pim_stdlib_module_identifiers = (
        'Terminal', 'FileSystem', 'InOut', 'RealInOut', 'MathLib0', 'Storage',
    )

    # PIM Modula-2 Standard Library Types Dataset
    pim_stdlib_type_identifiers = (
        'Flag', 'FlagSet', 'Response', 'Command', 'Lock', 'Permission',
        'MediumType', 'File', 'FileProc', 'DirectoryProc', 'FileCommand',
        'DirectoryCommand',
    )

    # PIM Modula-2 Standard Library Procedures Dataset
    pim_stdlib_proc_identifiers = (
        'Read', 'BusyRead', 'ReadAgain', 'Write', 'WriteString', 'WriteLn',
        'Create', 'Lookup', 'Close', 'Delete', 'Rename', 'SetRead', 'SetWrite',
        'SetModify', 'SetOpen', 'Doio', 'SetPos', 'GetPos', 'Length', 'Reset',
        'Again', 'ReadWord', 'WriteWord', 'ReadChar', 'WriteChar',
        'CreateMedium', 'DeleteMedium', 'AssignName', 'DeassignName',
        'ReadMedium', 'LookupMedium', 'OpenInput', 'OpenOutput', 'CloseInput',
        'CloseOutput', 'ReadString', 'ReadInt', 'ReadCard', 'ReadWrd',
        'WriteInt', 'WriteCard', 'WriteOct', 'WriteHex', 'WriteWrd',
        'ReadReal', 'WriteReal', 'WriteFixPt', 'WriteRealOct', 'sqrt', 'exp',
        'ln', 'sin', 'cos', 'arctan', 'entier','ALLOCATE', 'DEALLOCATE',   
    )

    # PIM Modula-2 Standard Library Variables Dataset
    pim_stdlib_var_identifiers = (
        'Done', 'termCH', 'in', 'out'
    )

    # PIM Modula-2 Standard Library Constants Dataset
    pim_stdlib_const_identifiers = (
        'EOL',
    )
    
#  I S O   S t a n d a r d   L i b r a r y   D a t a s e t s
    
    # ISO Modula-2 Standard Library Modules Dataset
    iso_stdlib_module_identifiers = (
        # TO DO
    )
    
    # ISO Modula-2 Standard Library Types Dataset
    iso_stdlib_type_identifiers = (
        # TO DO
    )
    
    # ISO Modula-2 Standard Library Procedures Dataset
    iso_stdlib_proc_identifiers = (
        # TO DO
    )
    
    # ISO Modula-2 Standard Library Variables Dataset
    iso_stdlib_var_identifiers = (
        # TO DO
    )
    
    # ISO Modula-2 Standard Library Constants Dataset
    iso_stdlib_const_identifiers = (
        # TO DO
    )
    
#  M 2   R 1 0   S t a n d a r d   L i b r a r y   D a t a s e t s
    
    # Modula-2 R10 Standard Library ADTs Dataset
    m2r10_stdlib_adt_identifiers = (
        'BCD', 'LONGBCD', 'BITSET', 'SHORTBITSET', 'LONGBITSET',
        'LONGLONGBITSET', 'COMPLEX', 'LONGCOMPLEX', 'SHORTCARD', 'LONGLONGCARD',
        'SHORTINT', 'LONGLONGINT', 'POSINT', 'SHORTPOSINT', 'LONGPOSINT',
        'LONGLONGPOSINT', 'BITSET8', 'BITSET16', 'BITSET32', 'BITSET64',
        'BITSET128', 'BS8', 'BS16', 'BS32', 'BS64', 'BS128', 'CARDINAL8',
        'CARDINAL16', 'CARDINAL32', 'CARDINAL64', 'CARDINAL128', 'CARD8',
        'CARD16', 'CARD32', 'CARD64', 'CARD128', 'INTEGER8', 'INTEGER16',
        'INTEGER32', 'INTEGER64', 'INTEGER128', 'INT8', 'INT16', 'INT32',
        'INT64', 'INT128', 'STRING', 'UNISTRING',
    )

    # Modula-2 R10 Standard Library Blueprints Dataset
    m2r10_stdlib_blueprint_identifiers = (
        'ProtoRoot', 'ProtoComputational', 'ProtoNumeric', 'ProtoScalar',
        'ProtoNonScalar', 'ProtoCardinal', 'ProtoInteger', 'ProtoReal',
        'ProtoComplex', 'ProtoVector', 'ProtoTuple', 'ProtoCompArray',
        'ProtoCollection', 'ProtoStaticArray', 'ProtoStaticSet',
        'ProtoStaticString', 'ProtoArray', 'ProtoString', 'ProtoSet',
        'ProtoMultiSet', 'ProtoDictionary', 'ProtoMultiDict', 'ProtoExtension',
        'ProtoIO', 'ProtoCardMath', 'ProtoIntMath', 'ProtoRealMath',
    )

    # Modula-2 R10 Standard Library Modules Dataset
    m2r10_stdlib_module_identifiers = (
        'ASCII', 'BooleanIO', 'CharIO', 'UnicharIO', 'OctetIO',
        'CardinalIO', 'LongCardIO', 'IntegerIO', 'LongIntIO', 'RealIO',
        'LongRealIO', 'BCDIO', 'LongBCDIO', 'CardMath', 'LongCardMath',
        'IntMath', 'LongIntMath', 'RealMath', 'LongRealMath', 'BCDMath',
        'LongBCDMath', 'FileIO', 'FileSystem', 'Storage', 'IOSupport',       
    )
    
    # Modula-2 R10 Standard Library Types Dataset
    m2r10_stdlib_type_identifiers = (
        'File', 'Status', 
        # TO BE COMPLETED
    )
    
    # Modula-2 R10 Standard Library Procedures Dataset
    m2r10_stdlib_proc_identifiers = (
        'ALLOCATE', 'DEALLOCATE', 'SIZE', 
        # TO BE COMPLETED
    )
    
    # Modula-2 R10 Standard Library Variables Dataset
    m2r10_stdlib_var_identifiers = (
        'stdIn', 'stdOut', 'stdErr',
    )

    # Modula-2 R10 Standard Library Constants Dataset
    m2r10_stdlib_const_identifiers = (
        'pi', 'tau', 
    )
    
#  D i a l e c t s
    
    # Dialect modes
    dialects = (
        'unknown',
        'm2pim', 'm2iso', 'm2r10', 'objm2', 'm2pim+gm2', 'm2pim+mocka',
        'm2iso+aglet', 'm2iso+gm2', 'm2iso+gpm', 'm2iso+p1', 'm2iso+sbu',
        'm2iso+xds',
    )
    
#   D a t a b a s e s
    
    # Human Readable Dialect Name Database
    dialect_str = {
        'unknown' : 'unknown',
        'm2pim' : 'PIM Modula-2',
        'm2iso' : 'ISO Modula-2',
        'm2r10' : 'Modula-2 R10',
        'objm2' : 'Objective Modula-2',
        'm2pim+gm2' : 'PIM Modula-2 with GNU Extensions',
        'm2pim+mocka' : 'PIM Modula-2 with MOCKA Extensions',
        'm2iso+aglet' : 'ISO Modula-2 with Aglet Extensions',
        'm2iso+gm2' : 'ISO Modula-2 with GNU Extensions',
        'm2iso+gpm' : 'ISO Modula-2 with Gardens Point Extensions',
        'm2iso+p1' : 'ISO Modula-2 with p1 Extensions',
        'm2iso+sbu' : 'ISO Modula-2 with Stony Brook Extensions',
        'm2iso+xds' : 'ISO Modula-2 with XDS Extensions',
    }
        
    # Punctuation Database
    punctuation_db = {
        # Punctuation for unknown dialect
        'unknown' : (
            common_punctuation,
            pim_additional_punctuation,
            iso_additional_punctuation,
            m2r10_additional_punctuation,
        ),

        # Punctuation for PIM Modula-2
        'm2pim' : (
            common_punctuation,
            pim_additional_punctuation,
        ),

        # Punctuation for ISO Modula-2
        'm2iso' : (
            common_punctuation,
            iso_additional_punctuation,
        ),

        # Punctuation for Modula-2 R10
        'm2r10' : (
            common_punctuation,
            m2r10_additional_punctuation,
        ),

        # Punctuation for Objective Modula-2
        'objm2' : (
            common_punctuation,
            m2r10_additional_punctuation,
            objm2_additional_punctuation,
        ),

        # Punctuation for GNU Modula-2 Extensions to PIM
        'm2pim+gm2' : (
            common_punctuation,
            pim_additional_punctuation,
            gm2_additional_punctuation,
        ),

        # Punctuation for MOCKA Modula-2 Extensions to PIM
        'm2pim+mocka' : (
            common_punctuation,
            pim_additional_punctuation,
            mocka_additional_punctuation,
        ),

        # Punctuation for Aglet Modula-2 Extensions to ISO
        'm2iso+aglet' : (
            common_punctuation,
            iso_additional_punctuation,
            aglet_additional_punctuation,
        ),

        # Punctuation for GNU Modula-2 Extensions to ISO
        'm2iso+gm2' : (
            common_punctuation,
            iso_additional_punctuation,
            gm2_additional_punctuation,
        ),

        # Punctuation for Gardens Point Modula-2 Extensions to ISO
        'm2iso+gpm' : (
            common_punctuation,
            iso_additional_punctuation,
            gpm_additional_punctuation,
        ),

        # Punctuation for p1 Modula-2 Extensions to ISO
        'm2iso+p1' : (
            common_punctuation,
            iso_additional_punctuation,
            p1_additional_punctuation,
        ),

        # Punctuation for Stony Brook Modula-2 Extensions to ISO
        'm2iso+sbu' : (
            common_punctuation,
            iso_additional_punctuation,
            sbu_additional_punctuation,
        ),
        
        # Punctuation for XDS Modula-2 Extensions to ISO
        'm2iso+xds' : (
            common_punctuation,
            iso_additional_punctuation,
            xds_additional_punctuation,
        ),
    }
    
    # Operators Database
    operators_db = {
        # Operators for unknown dialect
        'unknown' : (
            common_operators,
            pim_additional_operators,
            iso_additional_operators,
            m2r10_additional_operators,
        ),

        # Operators for PIM Modula-2
        'm2pim' : (
            common_operators,
            pim_additional_operators,
        ),

        # Operators for ISO Modula-2
        'm2iso' : (
            common_operators,
            iso_additional_operators,
        ),

        # Operators for Modula-2 R10
        'm2r10' : (
            common_operators,
            m2r10_additional_operators,
        ),

        # Operators for Objective Modula-2
        'objm2' : (
            common_operators,
            m2r10_additional_operators,
            objm2_additional_operators,
        ),

        # Operators for GNU Modula-2 Extensions to PIM
        'm2pim+gm2' : (
            common_operators,
            pim_additional_operators,
            gm2_additional_operators,
        ),

        # Operators for MOCKA Modula-2 Extensions to PIM
        'm2pim+mocka' : (
            common_operators,
            pim_additional_operators,
            mocka_additional_operators,
        ),

        # Operators for Aglet Modula-2 Extensions to ISO
        'm2iso+aglet' : (
            common_operators,
            iso_additional_operators,
            aglet_additional_operators,
        ),

        # Operators for GNU Modula-2 Extensions to ISO
        'm2iso+gm2' : (
            common_operators,
            iso_additional_operators,
            gm2_additional_operators,
        ),

        # Operators for Gardens Point Modula-2 Extensions to ISO
        'm2iso+gpm' : (
            common_operators,
            iso_additional_operators,
            gpm_additional_operators,
        ),

        # Operators for p1 Modula-2 Extensions to ISO
        'm2iso+p1' : (
            common_operators,
            iso_additional_operators,
            p1_additional_operators,
        ),

        # Operators for Stony Brook Modula-2 Extensions to ISO
        'm2iso+sbu' : (
            common_operators,
            iso_additional_operators,
            sbu_additional_operators,
        ),

        # Operators for XDS Modula-2 Extensions to ISO
        'm2iso+xds' : (
            common_operators,
            iso_additional_operators,
            xds_additional_operators,
        ),
    }
    
    # Literals Database
    literals_db = {
        # Literals for unknown dialect
        'unknown' : (
            common_literals,
            pim_additional_literals,
            iso_additional_literals,
            m2r10_additional_literals,
        ),

        # Literals for PIM Modula-2
        'm2pim' : (
            common_literals,
            pim_additional_literals,
        ),

        # Literals for ISO Modula-2
        'm2iso' : (
            common_literals,
            iso_additional_literals,
        ),

        # Literals for Modula-2 R10
        'm2r10' : (
            common_literals,
            m2r10_additional_literals,
        ),

        # Literals for Objective Modula-2
        'objm2' : (
            common_literals,
            m2r10_additional_literals,
            objm2_additional_literals,
        ),

        # Literals for GNU Modula-2 Extensions to PIM
        'm2pim+gm2' : (
            common_literals,
            pim_additional_literals,
            gm2_additional_literals,
        ),

        # Literals for MOCKA Modula-2 Extensions to PIM
        'm2pim+mocka' : (
            common_literals,
            pim_additional_literals,
            mocka_additional_literals,
        ),

        # Literals for Aglet Modula-2 Extensions to ISO
        'm2iso+aglet' : (
            common_literals,
            iso_additional_literals,
            aglet_additional_literals,
        ),

        # Literals for GNU Modula-2 Extensions to ISO
        'm2iso+gm2' : (
            common_literals,
            iso_additional_literals,
            gm2_additional_literals,
        ),

        # Literals for Gardens Point Modula-2 Extensions to ISO
        'm2iso+gpm' : (
            common_literals,
            iso_additional_literals,
            gpm_additional_literals,
        ),

        # Literals for p1 Modula-2 Extensions to ISO
        'm2iso+p1' : (
            common_literals,
            iso_additional_literals,
            p1_additional_literals,
        ),

        # Literals for Stony Brook Modula-2 Extensions to ISO
        'm2iso+sbu' : (
            common_literals,
            iso_additional_literals,
            sbu_additional_literals,
        ),

        # Literals for XDS Modula-2 Extensions to ISO
        'm2iso+xds' : (
            common_literals,
            iso_additional_literals,
            xds_additional_literals,
        ),
    }
    
    # Comments and Pragmas Database
    comments_and_pragmas_db = {
        # Comments and Pragmas for unknown dialect
        'unknown' : (
            common_comments_and_pragmas,
            pim_additional_comments_and_pragmas,
            iso_additional_comments_and_pragmas,
            m2r10_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for PIM Modula-2
        'm2pim' : (
            common_comments_and_pragmas,
            pim_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for ISO Modula-2
        'm2iso' : (
            common_comments_and_pragmas,
            iso_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for Modula-2 R10
        'm2r10' : (
            common_comments_and_pragmas,
            m2r10_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for Objective Modula-2
        'objm2' : (
            common_comments_and_pragmas,
            m2r10_additional_comments_and_pragmas,
            objm2_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for GNU Modula-2 Extensions to PIM
        'm2pim+gm2' : (
            common_comments_and_pragmas,
            pim_additional_comments_and_pragmas,
            gm2_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for MOCKA Modula-2 Extensions to PIM
        'm2pim+mocka' : (
            common_comments_and_pragmas,
            pim_additional_comments_and_pragmas,
            mocka_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for Aglet Modula-2 Extensions to ISO
        'm2iso+aglet' : (
            common_comments_and_pragmas,
            iso_additional_comments_and_pragmas,
            aglet_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for GNU Modula-2 Extensions to ISO
        'm2iso+gm2' : (
            common_comments_and_pragmas,
            iso_additional_comments_and_pragmas,
            gm2_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for Gardens Point Modula-2 Extensions to ISO
        'm2iso+gpm' : (
            common_comments_and_pragmas,
            iso_additional_comments_and_pragmas,
            gpm_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for p1 Modula-2 Extensions to ISO
        'm2iso+p1' : (
            common_comments_and_pragmas,
            iso_additional_comments_and_pragmas,
            p1_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for Stony Brook Modula-2 Extensions to ISO
        'm2iso+sbu' : (
            common_comments_and_pragmas,
            iso_additional_comments_and_pragmas,
            sbu_additional_comments_and_pragmas,
        ),

        # Comments and Pragmas for XDS Modula-2 Extensions to ISO
        'm2iso+xds' : (
            common_comments_and_pragmas,
            iso_additional_comments_and_pragmas,
            xds_additional_comments_and_pragmas,
        ),
    }
    
    # Name Recognition Database
    name_recognition_db = {
        # Name Recognition for unknown dialect
        'unknown' : (
            common_name_recognition,
            pim_additional_name_recognition,
            iso_additional_name_recognition,
            m2r10_additional_name_recognition,
        ),

        # Name Recognition for PIM Modula-2
        'm2pim' : (
            common_name_recognition,
            pim_additional_name_recognition,
        ),

        # Name Recognition for ISO Modula-2
        'm2iso' : (
            common_name_recognition,
            iso_additional_name_recognition,
        ),

        # Name Recognition for Modula-2 R10
        'm2r10' : (
            common_name_recognition,
            m2r10_additional_name_recognition,
        ),

        # Name Recognition for Objective Modula-2
        'objm2' : (
            common_name_recognition,
            m2r10_additional_name_recognition,
            objm2_additional_name_recognition,
        ),

        # Name Recognition for GNU Modula-2 Extensions to PIM
        'm2pim+gm2' : (
            common_name_recognition,
            pim_additional_name_recognition,
            gm2_additional_name_recognition,
        ),

        # Name Recognition for MOCKA Modula-2 Extensions to PIM
        'm2pim+mocka' : (
            common_name_recognition,
            pim_additional_name_recognition,
            mocka_additional_name_recognition,
        ),

        # Name Recognition for Aglet Modula-2 Extensions to ISO
        'm2iso+aglet' : (
            common_name_recognition,
            iso_additional_name_recognition,
            aglet_additional_name_recognition,
        ),

        # Name Recognition for GNU Modula-2 Extensions to ISO
        'm2iso+gm2' : (
            common_name_recognition,
            iso_additional_name_recognition,
            gm2_additional_name_recognition,
        ),

        # Name Recognition for Gardens Point Modula-2 Extensions to ISO
        'm2iso+gpm' : (
            # GPM only permits a subset of ISO names
            common_name_recognition,
            gpm_additional_name_recognition,
        ),

        # Name Recognition for p1 Modula-2 Extensions to ISO
        'm2iso+p1' : (
            common_name_recognition,
            iso_additional_name_recognition,
            p1_additional_name_recognition,
        ),

        # Name Recognition for Stony Brook Modula-2 Extensions to ISO
        'm2iso+sbu' : (
            common_name_recognition,
            iso_additional_name_recognition,
            sbu_additional_name_recognition,
        ),

        # Name Recognition for XDS Modula-2 Extensions to ISO
        'm2iso+xds' : (
            common_name_recognition,
            iso_additional_name_recognition,
            xds_additional_name_recognition,
        ),
    }

    # Reserved Words Database
    reserved_words_db = {
        # Reserved words for unknown dialect
        'unknown' : (
            common_reserved_words,
            pim_additional_reserved_words,
            iso_additional_reserved_words,
            m2r10_additional_reserved_words,
        ),

        # Reserved words for PIM Modula-2
        'm2pim' : (
            common_reserved_words,
            pim_additional_reserved_words,
        ),

        # Reserved words for ISO Modula-2
        'm2iso' : (
            common_reserved_words,
            iso_additional_reserved_words,
        ),

        # Reserved words for Modula-2 R10
        'm2r10' : (
            common_reserved_words,
            m2r10_additional_reserved_words,
        ),

        # Reserved words for Objective Modula-2
        'objm2' : (
            common_reserved_words,
            m2r10_additional_reserved_words,
            objm2_additional_reserved_words,
        ),

        # Reserved words for GNU Modula-2 Extensions to PIM
        'm2pim+gm2' : (
            common_reserved_words,
            pim_additional_reserved_words,
            gm2_additional_reserved_words,
        ),

        # Reserved words for MOCKA Modula-2 Extensions to PIM
        'm2pim+mocka' : (
            common_reserved_words,
            pim_additional_reserved_words,
            mocka_additional_reserved_words,
        ),

        # Reserved words for Aglet Modula-2 Extensions to ISO
        'm2iso+aglet' : (
            common_reserved_words,
            iso_additional_reserved_words,
            aglet_additional_reserved_words,
        ),

        # Reserved words for GNU Modula-2 Extensions to ISO
        'm2iso+gm2' : (
            common_reserved_words,
            iso_additional_reserved_words,
            gm2_additional_reserved_words,
        ),

        # Reserved words for Gardens Point Modula-2 Extensions to ISO
        'm2iso+gpm' : (
            common_reserved_words,
            iso_additional_reserved_words,
            gpm_additional_reserved_words,
        ),

        # Reserved words for p1 Modula-2 Extensions to ISO
        'm2iso+p1' : (
            common_reserved_words,
            iso_additional_reserved_words,
            p1_additional_reserved_words,
        ),

        # Reserved words for Stony Brook Modula-2 Extensions to ISO
        'm2iso+sbu' : (
            common_reserved_words,
            iso_additional_reserved_words,
            sbu_additional_reserved_words,
        ),

        # Reserved words for XDS Modula-2 Extensions to ISO
        'm2iso+xds' : (
            common_reserved_words,
            iso_additional_reserved_words,
            xds_additional_reserved_words,
        ),
    }

    # Builtins Database
    builtins_db = {
        # Builtins for unknown dialect
        'unknown' : (
            common_builtins,
            pim_additional_builtins,
            iso_additional_builtins,
            m2r10_additional_builtins,
        ),

        # Builtins for PIM Modula-2
        'm2pim' : (
            common_builtins,
            pim_additional_builtins,
        ),

        # Builtins for ISO Modula-2
        'm2iso' : (
            common_builtins,
            iso_additional_builtins,
        ),

        # Builtins for Modula-2 R10
        'm2r10' : (
            common_builtins,
            m2r10_additional_builtins,
        ),

        # Builtins for Objective Modula-2
        'objm2' : (
            common_builtins,
            m2r10_additional_builtins,
            objm2_additional_builtins,
        ),

        # Builtins for GNU Modula-2 Extensions to PIM
        'm2pim+gm2' : (
            common_builtins,
            pim_additional_builtins,
            gm2_additional_builtins,
        ),

        # Builtins for MOCKA Modula-2 Extensions to PIM
        'm2pim+mocka' : (
            common_builtins,
            pim_additional_builtins,
            mocka_additional_builtins,
        ),

        # Builtins for Aglet Modula-2 Extensions to ISO
        'm2iso+aglet' : (
            common_builtins,
            iso_additional_builtins,
            aglet_additional_builtins,
        ),

        # Builtins for GNU Modula-2 Extensions to ISO
        'm2iso+gm2' : (
            common_builtins,
            iso_additional_builtins,
            gm2_additional_builtins,
        ),

        # Builtins for Gardens Point Modula-2 Extensions to ISO
        'm2iso+gpm' : (
            common_builtins,
            iso_additional_builtins,
            gpm_additional_builtins,
        ),

        # Builtins for p1 Modula-2 Extensions to ISO
        'm2iso+p1' : (
            common_builtins,
            iso_additional_builtins,
            p1_additional_builtins,
        ),

        # Builtins for Stony Brook Modula-2 Extensions to ISO
        'm2iso+sbu' : (
            common_builtins,
            iso_additional_builtins,
            sbu_additional_builtins,
        ),

        # Builtins for XDS Modula-2 Extensions to ISO
        'm2iso+xds' : (
            common_builtins,
            iso_additional_builtins,
            xds_additional_builtins,
        ),
    }
    
    # Pseudo-Module Builtins Database
    pseudo_builtins_db = {
        # Builtins for unknown dialect
        'unknown' : (
            common_pseudo_builtins,
            pim_additional_pseudo_builtins,
            iso_additional_pseudo_builtins,
            m2r10_additional_pseudo_builtins,
        ),

        # Builtins for PIM Modula-2
        'm2pim' : (
            common_pseudo_builtins,
            pim_additional_pseudo_builtins,
        ),

        # Builtins for ISO Modula-2
        'm2iso' : (
            common_pseudo_builtins,
            iso_additional_pseudo_builtins,
        ),

        # Builtins for ISO Modula-2
        'm2r10' : (
            common_pseudo_builtins,
            m2r10_additional_pseudo_builtins,
        ),

        # Builtins for Objective Modula-2
        'objm2' : (
            common_pseudo_builtins,
            m2r10_additional_pseudo_builtins,
            objm2_additional_pseudo_builtins,
        ),

        # Builtins for GNU Modula-2 Extensions to PIM
        'm2pim+gm2' : (
            common_pseudo_builtins,
            pim_additional_pseudo_builtins,
            gm2_additional_pseudo_builtins,
        ),

        # Builtins for MOCKA Modula-2 Extensions to PIM
        'm2pim+mocka' : (
            common_pseudo_builtins,
            pim_additional_pseudo_builtins,
            mocka_additional_pseudo_builtins,
        ),

        # Builtins for Aglet Modula-2 Extensions to ISO
        'm2iso+aglet' : (
            common_pseudo_builtins,
            iso_additional_pseudo_builtins,
            aglet_additional_pseudo_builtins,
        ),

        # Builtins for GNU Modula-2 Extensions to ISO
        'm2iso+gm2' : (
            common_pseudo_builtins,
            iso_additional_pseudo_builtins,
            gm2_additional_pseudo_builtins,
        ),

        # Builtins for Gardens Point Modula-2 Extensions to ISO
        'm2iso+gpm' : (
            common_pseudo_builtins,
            iso_additional_pseudo_builtins,
            gpm_additional_pseudo_builtins,
        ),

        # Builtins for p1 Modula-2 Extensions to ISO
        'm2iso+p1' : (
            common_pseudo_builtins,
            iso_additional_pseudo_builtins,
            p1_additional_pseudo_builtins,
        ),

        # Builtins for Stony Brook Modula-2 Extensions to ISO
        'm2iso+sbu' : (
            common_pseudo_builtins,
            iso_additional_pseudo_builtins,
            sbu_additional_pseudo_builtins,
        ),

        # Builtins for XDS Modula-2 Extensions to ISO
        'm2iso+xds' : (
            common_pseudo_builtins,
            iso_additional_pseudo_builtins,
            xds_additional_pseudo_builtins,
        ),
    }
    
    # Standard Library ADTs Database
    stdlib_adts_db = {
        # Empty entry for unknown dialect
        'unknown' : (
            # LEAVE THIS EMPTY
        ),
        # Standard Library ADTs for PIM Modula-2
        'm2pim' : (
            # No first class library types
        ),

        # Standard Library ADTs for ISO Modula-2
        'm2iso' : (
            # No first class library types
        ),

        # Standard Library ADTs for Modula-2 R10
        'm2r10' : (
            m2r10_stdlib_adt_identifiers,
        ),

        # Standard Library ADTs for Objective Modula-2
        'objm2' : (
            m2r10_stdlib_adt_identifiers,
        ),
        
        # Standard Library ADTs for GNU Modula-2 (PIM)
        'm2pim+gm2' : (
            # No first class library types
        ),
        
        # Standard Library ADTs for MOCKA Modula-2
        'm2pim+mocka' : (
            # No first class library types
        ),
        
        # Standard Library ADTs for Aglet Modula-2
        'm2iso+aglet' : (
            # No first class library types
        ),
        
        # Standard Library ADTs for GNU Modula-2 (ISO)
        'm2iso+gm2' : (
            # No first class library types
        ),
        
        # Standard Library ADTs for Gardens Point Modula-2
        'm2iso+gpm' : (
            # No first class library types
        ),
        
        # Standard Library ADTs for p1 Modula-2
        'm2iso+p1' : (
            # No first class library types
        ),
        
        # Standard Library ADTs for Stony Brook Modula-2
        'm2iso+sbu' : (
            # No first class library types
        ),
        
        # Standard Library ADTs for XDS Modula-2
        'm2iso+xds' : (
            # No first class library types
        ),
    }
     
    # Standard Library Modules Database
    stdlib_modules_db = {
        # Empty entry for unknown dialect
        'unknown' : (
            # LEAVE THIS EMPTY
        ),
        # Standard Library Modules for PIM Modula-2
        'm2pim' : (
            pim_stdlib_module_identifiers,
        ),

        # Standard Library Modules for ISO Modula-2
        'm2iso' : (
            iso_stdlib_module_identifiers,
        ),

        # Standard Library Modules for Modula-2 R10
        'm2r10' : (
            m2r10_stdlib_blueprint_identifiers,
            m2r10_stdlib_module_identifiers,
            m2r10_stdlib_adt_identifiers,
        ),

        # Standard Library Modules for Objective Modula-2
        'objm2' : (
            m2r10_stdlib_blueprint_identifiers,
            m2r10_stdlib_module_identifiers,
        ),
        
        # Standard Library Modules for GNU Modula-2 (PIM)
        'm2pim+gm2' : (
            pim_stdlib_module_identifiers,
        ),
        
        # Standard Library Modules for MOCKA Modula-2
        'm2pim+mocka' : (
            pim_stdlib_module_identifiers,
        ),
        
        # Standard Library Modules for Aglet Modula-2
        'm2iso+aglet' : (
            iso_stdlib_module_identifiers,
        ),
        
        # Standard Library Modules for GNU Modula-2 (ISO)
        'm2iso+gm2' : (
            iso_stdlib_module_identifiers,
        ),
        
        # Standard Library Modules for Gardens Point Modula-2
        'm2iso+gpm' : (
            iso_stdlib_module_identifiers,
        ),
        
        # Standard Library Modules for p1 Modula-2
        'm2iso+p1' : (
            iso_stdlib_module_identifiers,
        ),
        
        # Standard Library Modules for Stony Brook Modula-2
        'm2iso+sbu' : (
            iso_stdlib_module_identifiers,
        ),
        
        # Standard Library Modules for XDS Modula-2
        'm2iso+xds' : (
            iso_stdlib_module_identifiers,
        ),
    }
     
    # Standard Library Types Database
    stdlib_types_db = {
        # Empty entry for unknown dialect
        'unknown' : (
            # LEAVE THIS EMPTY
        ),
        # Standard Library Types for PIM Modula-2
        'm2pim' : (
            pim_stdlib_type_identifiers,
        ),

        # Standard Library Types for ISO Modula-2
        'm2iso' : (
            iso_stdlib_type_identifiers,
        ),

        # Standard Library Types for Modula-2 R10
        'm2r10' : (
            m2r10_stdlib_type_identifiers,
        ),

        # Standard Library Types for Objective Modula-2
        'objm2' : (
            m2r10_stdlib_type_identifiers,
        ),
        
        # Standard Library Types for GNU Modula-2 (PIM)
        'm2pim+gm2' : (
            pim_stdlib_type_identifiers,
        ),
        
        # Standard Library Types for MOCKA Modula-2
        'm2pim+mocka' : (
            pim_stdlib_type_identifiers,
        ),
        
        # Standard Library Types for Aglet Modula-2
        'm2iso+aglet' : (
            iso_stdlib_type_identifiers,
        ),
        
        # Standard Library Types for GNU Modula-2 (ISO)
        'm2iso+gm2' : (
            iso_stdlib_type_identifiers,
        ),
        
        # Standard Library Types for Gardens Point Modula-2
        'm2iso+gpm' : (
            iso_stdlib_type_identifiers,
        ),
        
        # Standard Library Types for p1 Modula-2
        'm2iso+p1' : (
            iso_stdlib_type_identifiers,
        ),
        
        # Standard Library Types for Stony Brook Modula-2
        'm2iso+sbu' : (
            iso_stdlib_type_identifiers,
        ),
        
        # Standard Library Types for XDS Modula-2
        'm2iso+xds' : (
            iso_stdlib_type_identifiers,
        ),
    }

    # Standard Library Procedures Database
    stdlib_procedures_db = {
        # Empty entry for unknown dialect
        'unknown' : (
            # LEAVE THIS EMPTY
        ),
        # Standard Library Procedures for PIM Modula-2
        'm2pim' : (
            pim_stdlib_proc_identifiers,
        ),

        # Standard Library Procedures for ISO Modula-2
        'm2iso' : (
            iso_stdlib_proc_identifiers,
        ),

        # Standard Library Procedures for Modula-2 R10
        'm2r10' : (
            m2r10_stdlib_proc_identifiers,
        ),

        # Standard Library Procedures for Objective Modula-2
        'objm2' : (
            m2r10_stdlib_proc_identifiers,
        ),
        
        # Standard Library Procedures for GNU Modula-2 (PIM)
        'm2pim+gm2' : (
            pim_stdlib_proc_identifiers,
        ),
        
        # Standard Library Procedures for MOCKA Modula-2
        'm2pim+mocka' : (
            pim_stdlib_proc_identifiers,
        ),
        
        # Standard Library Procedures for Aglet Modula-2
        'm2iso+aglet' : (
            iso_stdlib_proc_identifiers,
        ),
        
        # Standard Library Procedures for GNU Modula-2 (ISO)
        'm2iso+gm2' : (
            iso_stdlib_proc_identifiers,
        ),
        
        # Standard Library Procedures for Gardens Point Modula-2
        'm2iso+gpm' : (
            iso_stdlib_proc_identifiers,
        ),
        
        # Standard Library Procedures for p1 Modula-2
        'm2iso+p1' : (
            iso_stdlib_proc_identifiers,
        ),
        
        # Standard Library Procedures for Stony Brook Modula-2
        'm2iso+sbu' : (
            iso_stdlib_proc_identifiers,
        ),
        
        # Standard Library Procedures for XDS Modula-2
        'm2iso+xds' : (
            iso_stdlib_proc_identifiers,
        ),
    }
   
    # Standard Library Variables Database
    stdlib_variables_db = {
        # Empty entry for unknown dialect
        'unknown' : (
            # LEAVE THIS EMPTY
        ),
        # Standard Library Variables for PIM Modula-2
        'm2pim' : (
            pim_stdlib_var_identifiers,
        ),

        # Standard Library Variables for ISO Modula-2
        'm2iso' : (
            iso_stdlib_var_identifiers,
        ),

        # Standard Library Variables for Modula-2 R10
        'm2r10' : (
            m2r10_stdlib_var_identifiers,
        ),

        # Standard Library Variables for Objective Modula-2
        'objm2' : (
            m2r10_stdlib_var_identifiers,
        ),
        
        # Standard Library Variables for GNU Modula-2 (PIM)
        'm2pim+gm2' : (
            pim_stdlib_var_identifiers,
        ),
        
        # Standard Library Variables for MOCKA Modula-2
        'm2pim+mocka' : (
            pim_stdlib_var_identifiers,
        ),
        
        # Standard Library Variables for Aglet Modula-2
        'm2iso+aglet' : (
            iso_stdlib_var_identifiers,
        ),
        
        # Standard Library Variables for GNU Modula-2 (ISO)
        'm2iso+gm2' : (
            iso_stdlib_var_identifiers,
        ),
        
        # Standard Library Variables for Gardens Point Modula-2
        'm2iso+gpm' : (
            iso_stdlib_var_identifiers,
        ),
        
        # Standard Library Variables for p1 Modula-2
        'm2iso+p1' : (
            iso_stdlib_var_identifiers,
        ),
        
        # Standard Library Variables for Stony Brook Modula-2
        'm2iso+sbu' : (
            iso_stdlib_var_identifiers,
        ),
        
        # Standard Library Variables for XDS Modula-2
        'm2iso+xds' : (
            iso_stdlib_var_identifiers,
        ),
    }
   
    # Standard Library Constants Database
    stdlib_constants_db = {
        # Empty entry for unknown dialect
        'unknown' : (
            # LEAVE THIS EMPTY
        ),
        # Standard Library Constants for PIM Modula-2
        'm2pim' : (
            pim_stdlib_const_identifiers,
        ),

        # Standard Library Constants for ISO Modula-2
        'm2iso' : (
            iso_stdlib_const_identifiers,
        ),

        # Standard Library Constants for Modula-2 R10
        'm2r10' : (
            m2r10_stdlib_const_identifiers,
        ),

        # Standard Library Constants for Objective Modula-2
        'objm2' : (
            m2r10_stdlib_const_identifiers,
        ),
        
        # Standard Library Constants for GNU Modula-2 (PIM)
        'm2pim+gm2' : (
            pim_stdlib_const_identifiers,
        ),
        
        # Standard Library Constants for MOCKA Modula-2
        'm2pim+mocka' : (
            pim_stdlib_const_identifiers,
        ),
        
        # Standard Library Constants for Aglet Modula-2
        'm2iso+aglet' : (
            iso_stdlib_const_identifiers,
        ),
        
        # Standard Library Constants for GNU Modula-2 (ISO)
        'm2iso+gm2' : (
            iso_stdlib_const_identifiers,
        ),
        
        # Standard Library Constants for Gardens Point Modula-2
        'm2iso+gpm' : (
            iso_stdlib_const_identifiers,
        ),
        
        # Standard Library Constants for p1 Modula-2
        'm2iso+p1' : (
            iso_stdlib_const_identifiers,
        ),
        
        # Standard Library Constants for Stony Brook Modula-2
        'm2iso+sbu' : (
            iso_stdlib_const_identifiers,
        ),
        
        # Standard Library Constants for XDS Modula-2
        'm2iso+xds' : (
            iso_stdlib_const_identifiers,
        ),
    }
    
#   M e t h o d s
    
    # initialise a lexer instance
    def __init__(self, **options):
        #
        # Alias for unknown dialect
        global UNKNOWN
        UNKNOWN = self.dialects[0]
        #
        # check dialect options
        #
        dialects = get_list_opt(options, 'dialect', [])
        #
        for dialect_option in dialects:
            if dialect_option in self.dialects[1:len(self.dialects)]:
                # valid dialect option found
                self.set_dialect(dialect_option)
                break
        #
        # Fallback Mode (DEFAULT)
        else:
              # no valid dialect option
              self.set_dialect(UNKNOWN)
        #
        self.dialect_set_by_tag = False
        #
        # check style options
        #
        styles = get_list_opt(options, 'style', [])
        #
        # use lowercase mode for Algol style
        if 'algol' in styles or 'algol_nu' in styles:
            self.algol_publication_mode = True
        else:
            self.algol_publication_mode = False
        #
        # Check option flags 
        #
        self.treat_stdlib_adts_as_builtins = \
          get_bool_opt(options, 'treat_stdlib_adts_as_builtins', True)
        #
        # call superclass initialiser
        RegexLexer.__init__(self, **options)
    
    # Set lexer to a specified dialect
    def set_dialect(self, dialect_id):
        #
        #print 'entered set_dialect with arg: ', dialect_id
        #
        # check dialect name against known dialects
        if dialect_id not in self.dialects:
            dialect = UNKNOWN # default
        else:
            dialect = dialect_id
        #
        # compose punctuation set
        punctuation_set = set()
        # add each list of punctuation symbols for this dialect
        for list in self.punctuation_db[dialect]:
            punctuation_set.update(set(list))
        #
        # compose operators set
        operators_set = set()
        # add each list of operators for this dialect
        for list in self.operators_db[dialect]:
            operators_set.update(set(list))
        #
        # compose literals set
        literals_set = set()
        # add each list of literals for this dialect
        for list in self.literals_db[dialect]:
            literals_set.update(set(list))
        #
        # compose comments and pragmas set
        comments_and_pragmas_set = set()
        # add each list of comments and pragmas for this dialect
        for list in self.comments_and_pragmas_db[dialect]:
            comments_and_pragmas_set.update(set(list))
        #
        # compose name recognition set
        name_recognition_set = set()
        # add each list of name recognitions for this dialect
        for list in self.name_recognition_db[dialect]:
            name_recognition_set.update(set(list))
        #
        # compose reserved words set
        reswords_set = set()
        # add each list of reserved words for this dialect
        for list in self.reserved_words_db[dialect]:
            reswords_set.update(set(list))
        #
        # compose builtins set
        builtins_set = set()
        # add each list of builtins for this dialect excluding reserved words
        for list in self.builtins_db[dialect]:
            builtins_set.update(set(list).difference(reswords_set))
        #
        # compose pseudo-builtins set
        pseudo_builtins_set = set()
        # add each list of builtins for this dialect excluding reserved words
        for list in self.pseudo_builtins_db[dialect]:
            pseudo_builtins_set.update(set(list).difference(reswords_set))
        #
        # compose ADTs set
        adts_set = set()
        # add each list of ADTs for this dialect excluding reserved words
        for list in self.stdlib_adts_db[dialect]:
            adts_set.update(set(list).difference(reswords_set))
        #
        # compose modules set
        modules_set = set()
        # add each list of builtins for this dialect excluding builtins
        for list in self.stdlib_modules_db[dialect]:
            modules_set.update(set(list).difference(builtins_set))
        #
        # compose types set
        types_set = set()
        # add each list of types for this dialect excluding builtins
        for list in self.stdlib_types_db[dialect]:
            types_set.update(set(list).difference(builtins_set))
        #
        # compose procedures set
        procedures_set = set()
        # add each list of procedures for this dialect excluding builtins
        for list in self.stdlib_procedures_db[dialect]:
            procedures_set.update(set(list).difference(builtins_set))
        #
        # compose variables set
        variables_set = set()
        # add each list of variables for this dialect excluding builtins
        for list in self.stdlib_variables_db[dialect]:
            variables_set.update(set(list).difference(builtins_set))
        #
        # compose constants set
        constants_set = set()
        # add each list of constants for this dialect excluding builtins
        for list in self.stdlib_constants_db[dialect]:
            constants_set.update(set(list).difference(builtins_set))
        #
        # update lexer state
        self.dialect = dialect
        self.punctuation = punctuation_set
        self.operators = operators_set
        self.literals = literals_set
        self.comments_and_pragmas = comments_and_pragmas_set
        self.name_recognition = name_recognition_set
        self.reserved_words = reswords_set
        self.builtins = builtins_set    
        self.pseudo_builtins = pseudo_builtins_set    
        self.adts = adts_set    
        self.modules = modules_set    
        self.types = types_set    
        self.procedures = procedures_set    
        self.variables = variables_set    
        self.constants = constants_set
        #
        #if __debug__:
        #    print 'exiting set_dialect'
        #    print ' self.dialect: ', self.dialect
        #    print ' self.punctuation: ', self.punctuation
        #    print ' self.operators: ', self.operators
        #    print ' self.literals: ', self.literals
        #    print ' self.comments_and_pragmas: ', self.comments_and_pragmas
        #    print ' self.name_recognition: ', self.name_recognition
        #    print ' self.reserved_words: ', self.reserved_words
        #    print ' self.builtins: ', self.builtins
        #    print ' self.pseudo_builtins: ', self.pseudo_builtins
        #    print ' self.adts: ', self.adts
        #    print ' self.modules: ', self.modules
        #    print ' self.types: ', self.types
        #    print ' self.procedures: ', self.procedures
        #    print ' self.variables: ', self.variables
        #    print ' self.types: ', self.types
        #    print ' self.constants: ', self.constants
    
    # return human readable name for dialect
    def get_dialect_str(self):
        return self.dialect_str[self.dialect]
    
    # Extracts a dialect name from a dialect tag comment string  and checks
    # the extracted name against known dialects.  If a match is found,  the
    # matching name is returned, otherwise dialect id 'unknown' is returned
    def get_dialect_from_dialect_tag(self, dialect_tag):
        #
        #print 'entered get_dialect_from_dialect_tag with arg: ', dialect_tag
        #
        # constants
        left_tag_delim = '(*!'
        right_tag_delim = '*)'
        left_tag_delim_len = len(left_tag_delim)
        right_tag_delim_len = len(right_tag_delim)
        indicator_start = left_tag_delim_len
        indicator_end = -(right_tag_delim_len)
        #
        # check comment string for dialect indicator
        if len(dialect_tag) > (left_tag_delim_len + right_tag_delim_len) \
          and dialect_tag.startswith(left_tag_delim) \
          and dialect_tag.endswith(right_tag_delim):
            #
            #print 'dialect tag found'
            #
            # extract dialect indicator
            indicator = dialect_tag[indicator_start:indicator_end]
            #
            #print 'extracted: ', indicator
            #
            # check against known dialects
            for index in range(1, len(self.dialects)):
                #print 'dialects[', index, ']: ', self.dialects[index]
                #
                if indicator == self.dialects[index]:
                    #print 'matching dialect found'
                    #
                    # indicator matches known dialect
                    return indicator
            else:
                # indicator does not match any dialect
                return UNKNOWN # default
        else:
            # invalid indicator string
            return UNKNOWN # default
    
    # intercept the token stream, modify token attributes and return them
    def get_tokens_unprocessed(self, text):
        for index, token, value in RegexLexer.get_tokens_unprocessed(self, text):
            #
            # check for dialect tag if dialect has not been set by tag
            if not self.dialect_set_by_tag and token in Comment.Special.DialectTag:
                # print "dialect tag found", value, token
                indicated_dialect = self.get_dialect_from_dialect_tag(value)
                if indicated_dialect != UNKNOWN:
                    # token is a dialect indicator
                    # reset lexer to use indicated dialect
                    self.set_dialect(indicated_dialect)
                    self.dialect_set_by_tag = True
            #
            # first check tokens that may either be operators or punctuation
            if token is Schroedinger:
                #print "lexeme = ", value, " : token = ", token
                if self.dialect == UNKNOWN:
                    if value not in self.operators \
                      and value not in self.punctuation:
                        token = Error
                else: # self.dialect is known
                    if value in self.operators:
                        token = Operator
                    elif value in self.punctuation:
                        token = Punctuation
            #
            # check punctuation, mark unsupported punctuation as errors
            if token is Punctuation:
                #print "lexeme = ", value, " : token = ", token
                if value not in self.punctuation:
                    token = Error
            #
            # check operators, mark unsupported operators as errors and
            # substitute operator lexemes in algol publication mode
            elif token is Operator:
                #print "lexeme = ", value, " : token = ", token
                if value not in self.operators:
                    token = Error
                if self.algol_publication_mode:
                    if value == '#':
                        value = u'≠'
                    elif value == '<=':
                        value = u'≤'
                    elif value == '>=':
                        value = u'≥'
                    elif value == '==':
                        value = u'≡'
                    elif value == '~':
                        value = u'¬'
            #
            elif token in Number:
                #
                if self.algol_publication_mode:
                    #print "lexeme = ", value, " : token = ", token
                    if token in Number.Oct:
                        value = value.replace('B', u'₈', 1)
                    elif token in Number.Hex:
                        value = value.replace('H', u'₁₆', 1)
                    elif token in Number.Float:
                        value = value.replace('E', u'₁₀', 1)
                #
                for list in self.literals:
                    if token in list:
                        # token found in list of supported literals
                        break
                else:
                    # token not found in list of supported literals
                    token = Error
                #
                # formatters appear to be broken for custom tokens,
                # for now, restore tokens to their nearest builtin values
                if token in Number.Float:
                    token = Number.Float
                elif token in Number:
                    token = Number.Integer
            #
            # check for reserved words, predefined and stdlib identifiers
            elif token in Name:
                
                for list in self.name_recognition:
                    if token in list:
                        # token conforms to supported name recognition rules
                        break
                else:
                    # token not conform to supported name recognition rules
                    token = Error
                                
                if value in self.reserved_words:
                    token = Keyword.Reserved
                    if self.algol_publication_mode:
                        if value.isalnum() and value.isupper():
                            value = value.lower()
                #
                elif value in self.builtins:
                    token = Name.Builtin
                    if self.algol_publication_mode:
                        if value.isalnum() and value.isupper():
                            value = value.lower()
                #
                elif value in self.pseudo_builtins:
                    token = Name.Builtin.Pseudo
                    if self.algol_publication_mode:
                        if value.isupper():
                            value = value.lower()
                #
                elif value in self.adts:
                    if not self.treat_stdlib_adts_as_builtins:
                        token = Name.Namespace
                    else:
                        token = Name.Builtin.Pseudo
                        if self.algol_publication_mode:
                            if value.isalnum() and value.isupper():
                                value = value.lower()
                #
                elif value in self.modules:
                    token = Name.Namespace
                #
                elif value in self.types:
                    token = Name.Class
                #
                elif value in self.procedures:
                    token = Name.Function
                #
                elif value in self.variables:
                    token = Name.Variable
                #
                elif value in self.constants:
                    token = Name.Constant
            #
            elif token in Comment:
                #print "lexeme = ", value[0:15], "... : token = ", token
                #
                for list in self.comments_and_pragmas:
                    if token in list:
                        # token found in list of supported comments and pragmas
                        break
                else:
                    # token not found in list of supported comments and pragmas
                    if token is Comment.Preproc.PascalStyle:
                        token = Comment.Multiline.PascalStyle
                    else:
                        token = Error
                #
                # expand dialect comment macro
                if token is Comment.Special.DialectMacro:
                    value = value.replace('~dialect~', self.get_dialect_str(), 1)
                    value = value.replace('~algol-mode~', \
                      str(self.algol_publication_mode), 1)
                    value = value.replace('(*?', '(*', 1)
                    value = value.replace('?*)', '*)', 1)
                #
                # replace special comment delimiters with plain delimiters
                if token in Comment.Special.Title:
                    value = value.replace('(*#', '(*', 1)
                    value = value.replace('#*)', '*)', 1)
                if token in Comment.Special.Headline.One:
                    value = value.replace('(*=', '(*', 1)
                    value = value.replace('=*)', '*)', 1)
                if token in Comment.Special.Headline.Two:
                    value = value.replace('(*-', '(*', 1)
                    value = value.replace('-*)', '*)', 1)
                if token in Comment.Special.Headline.Three:
                    value = value.replace('(*_', '(*', 1)
                    value = value.replace('_*)', '*)', 1)

                # formatters appear to be broken for custom tokens,
                # for now, restore tokens to their nearest builtin values
                if token in Comment.Single:
                    token = Comment.Single
                elif token in Comment.Multiline:
                    token = Comment.Multiline
                elif token in Comment.Preproc:
                    token = Comment.Preproc
                elif token in Comment.Special:
                    token = Comment.Special
            #
            # return result
            yield index, token, value

# end of modula2.py