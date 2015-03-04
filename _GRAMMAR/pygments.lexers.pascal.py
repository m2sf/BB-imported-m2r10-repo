# -*- coding: utf-8 -*-
"""
    pygments.lexers.pascal
    ~~~~~~~~~~~~~~~~~~~~~~

    Lexers for Pascal family languages.

    :copyright: Copyright 2006-2015 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments.lexer import Lexer, RegexLexer, include, bygroups, words, \
    using, this, default
from pygments.util import get_bool_opt, get_list_opt
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Error
from pygments.scanner import Scanner
from pygments.filters import KeywordCaseFilter

__all__ = ['DelphiLexer', 'Modula2Lexer', 'AdaLexer']


class DelphiLexer(Lexer):
    """
    For `Delphi <http://www.borland.com/delphi/>`_ (Borland Object Pascal),
    Turbo Pascal and Free Pascal source code.

    Additional options accepted:

    `turbopascal`
        Highlight Turbo Pascal specific keywords (default: ``True``).
    `delphi`
        Highlight Borland Delphi specific keywords (default: ``True``).
    `freepascal`
        Highlight Free Pascal specific keywords (default: ``True``).
    `units`
        A list of units that should be considered builtin, supported are
        ``System``, ``SysUtils``, ``Classes`` and ``Math``.
        Default is to consider all of them builtin.
    """
    name = 'Delphi'
    aliases = ['delphi', 'pas', 'pascal', 'objectpascal']
    filenames = ['*.pas']
    mimetypes = ['text/x-pascal']

    TURBO_PASCAL_KEYWORDS = (
        'absolute', 'and', 'array', 'asm', 'begin', 'break', 'case',
        'const', 'constructor', 'continue', 'destructor', 'div', 'do',
        'downto', 'else', 'end', 'file', 'for', 'function', 'goto',
        'if', 'implementation', 'in', 'inherited', 'inline', 'interface',
        'label', 'mod', 'nil', 'not', 'object', 'of', 'on', 'operator',
        'or', 'packed', 'procedure', 'program', 'record', 'reintroduce',
        'repeat', 'self', 'set', 'shl', 'shr', 'string', 'then', 'to',
        'type', 'unit', 'until', 'uses', 'var', 'while', 'with', 'xor'
    )

    DELPHI_KEYWORDS = (
        'as', 'class', 'except', 'exports', 'finalization', 'finally',
        'initialization', 'is', 'library', 'on', 'property', 'raise',
        'threadvar', 'try'
    )

    FREE_PASCAL_KEYWORDS = (
        'dispose', 'exit', 'false', 'new', 'true'
    )

    BLOCK_KEYWORDS = set((
        'begin', 'class', 'const', 'constructor', 'destructor', 'end',
        'finalization', 'function', 'implementation', 'initialization',
        'label', 'library', 'operator', 'procedure', 'program', 'property',
        'record', 'threadvar', 'type', 'unit', 'uses', 'var'
    ))

    FUNCTION_MODIFIERS = set((
        'alias', 'cdecl', 'export', 'inline', 'interrupt', 'nostackframe',
        'pascal', 'register', 'safecall', 'softfloat', 'stdcall',
        'varargs', 'name', 'dynamic', 'near', 'virtual', 'external',
        'override', 'assembler'
    ))

    # XXX: those aren't global. but currently we know no way for defining
    #      them just for the type context.
    DIRECTIVES = set((
        'absolute', 'abstract', 'assembler', 'cppdecl', 'default', 'far',
        'far16', 'forward', 'index', 'oldfpccall', 'private', 'protected',
        'published', 'public'
    ))

    BUILTIN_TYPES = set((
        'ansichar', 'ansistring', 'bool', 'boolean', 'byte', 'bytebool',
        'cardinal', 'char', 'comp', 'currency', 'double', 'dword',
        'extended', 'int64', 'integer', 'iunknown', 'longbool', 'longint',
        'longword', 'pansichar', 'pansistring', 'pbool', 'pboolean',
        'pbyte', 'pbytearray', 'pcardinal', 'pchar', 'pcomp', 'pcurrency',
        'pdate', 'pdatetime', 'pdouble', 'pdword', 'pextended', 'phandle',
        'pint64', 'pinteger', 'plongint', 'plongword', 'pointer',
        'ppointer', 'pshortint', 'pshortstring', 'psingle', 'psmallint',
        'pstring', 'pvariant', 'pwidechar', 'pwidestring', 'pword',
        'pwordarray', 'pwordbool', 'real', 'real48', 'shortint',
        'shortstring', 'single', 'smallint', 'string', 'tclass', 'tdate',
        'tdatetime', 'textfile', 'thandle', 'tobject', 'ttime', 'variant',
        'widechar', 'widestring', 'word', 'wordbool'
    ))

    BUILTIN_UNITS = {
        'System': (
            'abs', 'acquireexceptionobject', 'addr', 'ansitoutf8',
            'append', 'arctan', 'assert', 'assigned', 'assignfile',
            'beginthread', 'blockread', 'blockwrite', 'break', 'chdir',
            'chr', 'close', 'closefile', 'comptocurrency', 'comptodouble',
            'concat', 'continue', 'copy', 'cos', 'dec', 'delete',
            'dispose', 'doubletocomp', 'endthread', 'enummodules',
            'enumresourcemodules', 'eof', 'eoln', 'erase', 'exceptaddr',
            'exceptobject', 'exclude', 'exit', 'exp', 'filepos', 'filesize',
            'fillchar', 'finalize', 'findclasshinstance', 'findhinstance',
            'findresourcehinstance', 'flush', 'frac', 'freemem',
            'get8087cw', 'getdir', 'getlasterror', 'getmem',
            'getmemorymanager', 'getmodulefilename', 'getvariantmanager',
            'halt', 'hi', 'high', 'inc', 'include', 'initialize', 'insert',
            'int', 'ioresult', 'ismemorymanagerset', 'isvariantmanagerset',
            'length', 'ln', 'lo', 'low', 'mkdir', 'move', 'new', 'odd',
            'olestrtostring', 'olestrtostrvar', 'ord', 'paramcount',
            'paramstr', 'pi', 'pos', 'pred', 'ptr', 'pucs4chars', 'random',
            'randomize', 'read', 'readln', 'reallocmem',
            'releaseexceptionobject', 'rename', 'reset', 'rewrite', 'rmdir',
            'round', 'runerror', 'seek', 'seekeof', 'seekeoln',
            'set8087cw', 'setlength', 'setlinebreakstyle',
            'setmemorymanager', 'setstring', 'settextbuf',
            'setvariantmanager', 'sin', 'sizeof', 'slice', 'sqr', 'sqrt',
            'str', 'stringofchar', 'stringtoolestr', 'stringtowidechar',
            'succ', 'swap', 'trunc', 'truncate', 'typeinfo',
            'ucs4stringtowidestring', 'unicodetoutf8', 'uniquestring',
            'upcase', 'utf8decode', 'utf8encode', 'utf8toansi',
            'utf8tounicode', 'val', 'vararrayredim', 'varclear',
            'widecharlentostring', 'widecharlentostrvar',
            'widechartostring', 'widechartostrvar',
            'widestringtoucs4string', 'write', 'writeln'
        ),
        'SysUtils': (
            'abort', 'addexitproc', 'addterminateproc', 'adjustlinebreaks',
            'allocmem', 'ansicomparefilename', 'ansicomparestr',
            'ansicomparetext', 'ansidequotedstr', 'ansiextractquotedstr',
            'ansilastchar', 'ansilowercase', 'ansilowercasefilename',
            'ansipos', 'ansiquotedstr', 'ansisamestr', 'ansisametext',
            'ansistrcomp', 'ansistricomp', 'ansistrlastchar', 'ansistrlcomp',
            'ansistrlicomp', 'ansistrlower', 'ansistrpos', 'ansistrrscan',
            'ansistrscan', 'ansistrupper', 'ansiuppercase',
            'ansiuppercasefilename', 'appendstr', 'assignstr', 'beep',
            'booltostr', 'bytetocharindex', 'bytetocharlen', 'bytetype',
            'callterminateprocs', 'changefileext', 'charlength',
            'chartobyteindex', 'chartobytelen', 'comparemem', 'comparestr',
            'comparetext', 'createdir', 'createguid', 'currentyear',
            'currtostr', 'currtostrf', 'date', 'datetimetofiledate',
            'datetimetostr', 'datetimetostring', 'datetimetosystemtime',
            'datetimetotimestamp', 'datetostr', 'dayofweek', 'decodedate',
            'decodedatefully', 'decodetime', 'deletefile', 'directoryexists',
            'diskfree', 'disksize', 'disposestr', 'encodedate', 'encodetime',
            'exceptionerrormessage', 'excludetrailingbackslash',
            'excludetrailingpathdelimiter', 'expandfilename',
            'expandfilenamecase', 'expanduncfilename', 'extractfiledir',
            'extractfiledrive', 'extractfileext', 'extractfilename',
            'extractfilepath', 'extractrelativepath', 'extractshortpathname',
            'fileage', 'fileclose', 'filecreate', 'filedatetodatetime',
            'fileexists', 'filegetattr', 'filegetdate', 'fileisreadonly',
            'fileopen', 'fileread', 'filesearch', 'fileseek', 'filesetattr',
            'filesetdate', 'filesetreadonly', 'filewrite', 'finalizepackage',
            'findclose', 'findcmdlineswitch', 'findfirst', 'findnext',
            'floattocurr', 'floattodatetime', 'floattodecimal', 'floattostr',
            'floattostrf', 'floattotext', 'floattotextfmt', 'fmtloadstr',
            'fmtstr', 'forcedirectories', 'format', 'formatbuf', 'formatcurr',
            'formatdatetime', 'formatfloat', 'freeandnil', 'getcurrentdir',
            'getenvironmentvariable', 'getfileversion', 'getformatsettings',
            'getlocaleformatsettings', 'getmodulename', 'getpackagedescription',
            'getpackageinfo', 'gettime', 'guidtostring', 'incamonth',
            'includetrailingbackslash', 'includetrailingpathdelimiter',
            'incmonth', 'initializepackage', 'interlockeddecrement',
            'interlockedexchange', 'interlockedexchangeadd',
            'interlockedincrement', 'inttohex', 'inttostr', 'isdelimiter',
            'isequalguid', 'isleapyear', 'ispathdelimiter', 'isvalidident',
            'languages', 'lastdelimiter', 'loadpackage', 'loadstr',
            'lowercase', 'msecstotimestamp', 'newstr', 'nextcharindex', 'now',
            'outofmemoryerror', 'quotedstr', 'raiselastoserror',
            'raiselastwin32error', 'removedir', 'renamefile', 'replacedate',
            'replacetime', 'safeloadlibrary', 'samefilename', 'sametext',
            'setcurrentdir', 'showexception', 'sleep', 'stralloc', 'strbufsize',
            'strbytetype', 'strcat', 'strcharlength', 'strcomp', 'strcopy',
            'strdispose', 'strecopy', 'strend', 'strfmt', 'stricomp',
            'stringreplace', 'stringtoguid', 'strlcat', 'strlcomp', 'strlcopy',
            'strlen', 'strlfmt', 'strlicomp', 'strlower', 'strmove', 'strnew',
            'strnextchar', 'strpas', 'strpcopy', 'strplcopy', 'strpos',
            'strrscan', 'strscan', 'strtobool', 'strtobooldef', 'strtocurr',
            'strtocurrdef', 'strtodate', 'strtodatedef', 'strtodatetime',
            'strtodatetimedef', 'strtofloat', 'strtofloatdef', 'strtoint',
            'strtoint64', 'strtoint64def', 'strtointdef', 'strtotime',
            'strtotimedef', 'strupper', 'supports', 'syserrormessage',
            'systemtimetodatetime', 'texttofloat', 'time', 'timestamptodatetime',
            'timestamptomsecs', 'timetostr', 'trim', 'trimleft', 'trimright',
            'tryencodedate', 'tryencodetime', 'tryfloattocurr', 'tryfloattodatetime',
            'trystrtobool', 'trystrtocurr', 'trystrtodate', 'trystrtodatetime',
            'trystrtofloat', 'trystrtoint', 'trystrtoint64', 'trystrtotime',
            'unloadpackage', 'uppercase', 'widecomparestr', 'widecomparetext',
            'widefmtstr', 'wideformat', 'wideformatbuf', 'widelowercase',
            'widesamestr', 'widesametext', 'wideuppercase', 'win32check',
            'wraptext'
        ),
        'Classes': (
            'activateclassgroup', 'allocatehwnd', 'bintohex', 'checksynchronize',
            'collectionsequal', 'countgenerations', 'deallocatehwnd', 'equalrect',
            'extractstrings', 'findclass', 'findglobalcomponent', 'getclass',
            'groupdescendantswith', 'hextobin', 'identtoint',
            'initinheritedcomponent', 'inttoident', 'invalidpoint',
            'isuniqueglobalcomponentname', 'linestart', 'objectbinarytotext',
            'objectresourcetotext', 'objecttexttobinary', 'objecttexttoresource',
            'pointsequal', 'readcomponentres', 'readcomponentresex',
            'readcomponentresfile', 'rect', 'registerclass', 'registerclassalias',
            'registerclasses', 'registercomponents', 'registerintegerconsts',
            'registernoicon', 'registernonactivex', 'smallpoint', 'startclassgroup',
            'teststreamformat', 'unregisterclass', 'unregisterclasses',
            'unregisterintegerconsts', 'unregistermoduleclasses',
            'writecomponentresfile'
        ),
        'Math': (
            'arccos', 'arccosh', 'arccot', 'arccoth', 'arccsc', 'arccsch', 'arcsec',
            'arcsech', 'arcsin', 'arcsinh', 'arctan2', 'arctanh', 'ceil',
            'comparevalue', 'cosecant', 'cosh', 'cot', 'cotan', 'coth', 'csc',
            'csch', 'cycletodeg', 'cycletograd', 'cycletorad', 'degtocycle',
            'degtograd', 'degtorad', 'divmod', 'doubledecliningbalance',
            'ensurerange', 'floor', 'frexp', 'futurevalue', 'getexceptionmask',
            'getprecisionmode', 'getroundmode', 'gradtocycle', 'gradtodeg',
            'gradtorad', 'hypot', 'inrange', 'interestpayment', 'interestrate',
            'internalrateofreturn', 'intpower', 'isinfinite', 'isnan', 'iszero',
            'ldexp', 'lnxp1', 'log10', 'log2', 'logn', 'max', 'maxintvalue',
            'maxvalue', 'mean', 'meanandstddev', 'min', 'minintvalue', 'minvalue',
            'momentskewkurtosis', 'netpresentvalue', 'norm', 'numberofperiods',
            'payment', 'periodpayment', 'poly', 'popnstddev', 'popnvariance',
            'power', 'presentvalue', 'radtocycle', 'radtodeg', 'radtograd',
            'randg', 'randomrange', 'roundto', 'samevalue', 'sec', 'secant',
            'sech', 'setexceptionmask', 'setprecisionmode', 'setroundmode',
            'sign', 'simpleroundto', 'sincos', 'sinh', 'slndepreciation', 'stddev',
            'sum', 'sumint', 'sumofsquares', 'sumsandsquares', 'syddepreciation',
            'tan', 'tanh', 'totalvariance', 'variance'
        )
    }

    ASM_REGISTERS = set((
        'ah', 'al', 'ax', 'bh', 'bl', 'bp', 'bx', 'ch', 'cl', 'cr0',
        'cr1', 'cr2', 'cr3', 'cr4', 'cs', 'cx', 'dh', 'di', 'dl', 'dr0',
        'dr1', 'dr2', 'dr3', 'dr4', 'dr5', 'dr6', 'dr7', 'ds', 'dx',
        'eax', 'ebp', 'ebx', 'ecx', 'edi', 'edx', 'es', 'esi', 'esp',
        'fs', 'gs', 'mm0', 'mm1', 'mm2', 'mm3', 'mm4', 'mm5', 'mm6',
        'mm7', 'si', 'sp', 'ss', 'st0', 'st1', 'st2', 'st3', 'st4', 'st5',
        'st6', 'st7', 'xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5',
        'xmm6', 'xmm7'
    ))

    ASM_INSTRUCTIONS = set((
        'aaa', 'aad', 'aam', 'aas', 'adc', 'add', 'and', 'arpl', 'bound',
        'bsf', 'bsr', 'bswap', 'bt', 'btc', 'btr', 'bts', 'call', 'cbw',
        'cdq', 'clc', 'cld', 'cli', 'clts', 'cmc', 'cmova', 'cmovae',
        'cmovb', 'cmovbe', 'cmovc', 'cmovcxz', 'cmove', 'cmovg',
        'cmovge', 'cmovl', 'cmovle', 'cmovna', 'cmovnae', 'cmovnb',
        'cmovnbe', 'cmovnc', 'cmovne', 'cmovng', 'cmovnge', 'cmovnl',
        'cmovnle', 'cmovno', 'cmovnp', 'cmovns', 'cmovnz', 'cmovo',
        'cmovp', 'cmovpe', 'cmovpo', 'cmovs', 'cmovz', 'cmp', 'cmpsb',
        'cmpsd', 'cmpsw', 'cmpxchg', 'cmpxchg486', 'cmpxchg8b', 'cpuid',
        'cwd', 'cwde', 'daa', 'das', 'dec', 'div', 'emms', 'enter', 'hlt',
        'ibts', 'icebp', 'idiv', 'imul', 'in', 'inc', 'insb', 'insd',
        'insw', 'int', 'int01', 'int03', 'int1', 'int3', 'into', 'invd',
        'invlpg', 'iret', 'iretd', 'iretw', 'ja', 'jae', 'jb', 'jbe',
        'jc', 'jcxz', 'jcxz', 'je', 'jecxz', 'jg', 'jge', 'jl', 'jle',
        'jmp', 'jna', 'jnae', 'jnb', 'jnbe', 'jnc', 'jne', 'jng', 'jnge',
        'jnl', 'jnle', 'jno', 'jnp', 'jns', 'jnz', 'jo', 'jp', 'jpe',
        'jpo', 'js', 'jz', 'lahf', 'lar', 'lcall', 'lds', 'lea', 'leave',
        'les', 'lfs', 'lgdt', 'lgs', 'lidt', 'ljmp', 'lldt', 'lmsw',
        'loadall', 'loadall286', 'lock', 'lodsb', 'lodsd', 'lodsw',
        'loop', 'loope', 'loopne', 'loopnz', 'loopz', 'lsl', 'lss', 'ltr',
        'mov', 'movd', 'movq', 'movsb', 'movsd', 'movsw', 'movsx',
        'movzx', 'mul', 'neg', 'nop', 'not', 'or', 'out', 'outsb', 'outsd',
        'outsw', 'pop', 'popa', 'popad', 'popaw', 'popf', 'popfd', 'popfw',
        'push', 'pusha', 'pushad', 'pushaw', 'pushf', 'pushfd', 'pushfw',
        'rcl', 'rcr', 'rdmsr', 'rdpmc', 'rdshr', 'rdtsc', 'rep', 'repe',
        'repne', 'repnz', 'repz', 'ret', 'retf', 'retn', 'rol', 'ror',
        'rsdc', 'rsldt', 'rsm', 'sahf', 'sal', 'salc', 'sar', 'sbb',
        'scasb', 'scasd', 'scasw', 'seta', 'setae', 'setb', 'setbe',
        'setc', 'setcxz', 'sete', 'setg', 'setge', 'setl', 'setle',
        'setna', 'setnae', 'setnb', 'setnbe', 'setnc', 'setne', 'setng',
        'setnge', 'setnl', 'setnle', 'setno', 'setnp', 'setns', 'setnz',
        'seto', 'setp', 'setpe', 'setpo', 'sets', 'setz', 'sgdt', 'shl',
        'shld', 'shr', 'shrd', 'sidt', 'sldt', 'smi', 'smint', 'smintold',
        'smsw', 'stc', 'std', 'sti', 'stosb', 'stosd', 'stosw', 'str',
        'sub', 'svdc', 'svldt', 'svts', 'syscall', 'sysenter', 'sysexit',
        'sysret', 'test', 'ud1', 'ud2', 'umov', 'verr', 'verw', 'wait',
        'wbinvd', 'wrmsr', 'wrshr', 'xadd', 'xbts', 'xchg', 'xlat',
        'xlatb', 'xor'
    ))

    def __init__(self, **options):
        Lexer.__init__(self, **options)
        self.keywords = set()
        if get_bool_opt(options, 'turbopascal', True):
            self.keywords.update(self.TURBO_PASCAL_KEYWORDS)
        if get_bool_opt(options, 'delphi', True):
            self.keywords.update(self.DELPHI_KEYWORDS)
        if get_bool_opt(options, 'freepascal', True):
            self.keywords.update(self.FREE_PASCAL_KEYWORDS)
        self.builtins = set()
        for unit in get_list_opt(options, 'units', list(self.BUILTIN_UNITS)):
            self.builtins.update(self.BUILTIN_UNITS[unit])

    def get_tokens_unprocessed(self, text):
        scanner = Scanner(text, re.DOTALL | re.MULTILINE | re.IGNORECASE)
        stack = ['initial']
        in_function_block = False
        in_property_block = False
        was_dot = False
        next_token_is_function = False
        next_token_is_property = False
        collect_labels = False
        block_labels = set()
        brace_balance = [0, 0]

        while not scanner.eos:
            token = Error

            if stack[-1] == 'initial':
                if scanner.scan(r'\s+'):
                    token = Text
                elif scanner.scan(r'\{.*?\}|\(\*.*?\*\)'):
                    if scanner.match.startswith('$'):
                        token = Comment.Preproc
                    else:
                        token = Comment.Multiline
                elif scanner.scan(r'//.*?$'):
                    token = Comment.Single
                elif scanner.scan(r'[-+*\/=<>:;,.@\^]'):
                    token = Operator
                    # stop label highlighting on next ";"
                    if collect_labels and scanner.match == ';':
                        collect_labels = False
                elif scanner.scan(r'[\(\)\[\]]+'):
                    token = Punctuation
                    # abort function naming ``foo = Function(...)``
                    next_token_is_function = False
                    # if we are in a function block we count the open
                    # braces because ootherwise it's impossible to
                    # determine the end of the modifier context
                    if in_function_block or in_property_block:
                        if scanner.match == '(':
                            brace_balance[0] += 1
                        elif scanner.match == ')':
                            brace_balance[0] -= 1
                        elif scanner.match == '[':
                            brace_balance[1] += 1
                        elif scanner.match == ']':
                            brace_balance[1] -= 1
                elif scanner.scan(r'[A-Za-z_][A-Za-z_0-9]*'):
                    lowercase_name = scanner.match.lower()
                    if lowercase_name == 'result':
                        token = Name.Builtin.Pseudo
                    elif lowercase_name in self.keywords:
                        token = Keyword
                        # if we are in a special block and a
                        # block ending keyword occours (and the parenthesis
                        # is balanced) we end the current block context
                        if (in_function_block or in_property_block) and \
                           lowercase_name in self.BLOCK_KEYWORDS and \
                           brace_balance[0] <= 0 and \
                           brace_balance[1] <= 0:
                            in_function_block = False
                            in_property_block = False
                            brace_balance = [0, 0]
                            block_labels = set()
                        if lowercase_name in ('label', 'goto'):
                            collect_labels = True
                        elif lowercase_name == 'asm':
                            stack.append('asm')
                        elif lowercase_name == 'property':
                            in_property_block = True
                            next_token_is_property = True
                        elif lowercase_name in ('procedure', 'operator',
                                                'function', 'constructor',
                                                'destructor'):
                            in_function_block = True
                            next_token_is_function = True
                    # we are in a function block and the current name
                    # is in the set of registered modifiers. highlight
                    # it as pseudo keyword
                    elif in_function_block and \
                            lowercase_name in self.FUNCTION_MODIFIERS:
                        token = Keyword.Pseudo
                    # if we are in a property highlight some more
                    # modifiers
                    elif in_property_block and \
                            lowercase_name in ('read', 'write'):
                        token = Keyword.Pseudo
                        next_token_is_function = True
                    # if the last iteration set next_token_is_function
                    # to true we now want this name highlighted as
                    # function. so do that and reset the state
                    elif next_token_is_function:
                        # Look if the next token is a dot. If yes it's
                        # not a function, but a class name and the
                        # part after the dot a function name
                        if scanner.test(r'\s*\.\s*'):
                            token = Name.Class
                        # it's not a dot, our job is done
                        else:
                            token = Name.Function
                            next_token_is_function = False
                    # same for properties
                    elif next_token_is_property:
                        token = Name.Property
                        next_token_is_property = False
                    # Highlight this token as label and add it
                    # to the list of known labels
                    elif collect_labels:
                        token = Name.Label
                        block_labels.add(scanner.match.lower())
                    # name is in list of known labels
                    elif lowercase_name in block_labels:
                        token = Name.Label
                    elif lowercase_name in self.BUILTIN_TYPES:
                        token = Keyword.Type
                    elif lowercase_name in self.DIRECTIVES:
                        token = Keyword.Pseudo
                    # builtins are just builtins if the token
                    # before isn't a dot
                    elif not was_dot and lowercase_name in self.builtins:
                        token = Name.Builtin
                    else:
                        token = Name
                elif scanner.scan(r"'"):
                    token = String
                    stack.append('string')
                elif scanner.scan(r'\#(\d+|\$[0-9A-Fa-f]+)'):
                    token = String.Char
                elif scanner.scan(r'\$[0-9A-Fa-f]+'):
                    token = Number.Hex
                elif scanner.scan(r'\d+(?![eE]|\.[^.])'):
                    token = Number.Integer
                elif scanner.scan(r'\d+(\.\d+([eE][+-]?\d+)?|[eE][+-]?\d+)'):
                    token = Number.Float
                else:
                    # if the stack depth is deeper than once, pop
                    if len(stack) > 1:
                        stack.pop()
                    scanner.get_char()

            elif stack[-1] == 'string':
                if scanner.scan(r"''"):
                    token = String.Escape
                elif scanner.scan(r"'"):
                    token = String
                    stack.pop()
                elif scanner.scan(r"[^']*"):
                    token = String
                else:
                    scanner.get_char()
                    stack.pop()

            elif stack[-1] == 'asm':
                if scanner.scan(r'\s+'):
                    token = Text
                elif scanner.scan(r'end'):
                    token = Keyword
                    stack.pop()
                elif scanner.scan(r'\{.*?\}|\(\*.*?\*\)'):
                    if scanner.match.startswith('$'):
                        token = Comment.Preproc
                    else:
                        token = Comment.Multiline
                elif scanner.scan(r'//.*?$'):
                    token = Comment.Single
                elif scanner.scan(r"'"):
                    token = String
                    stack.append('string')
                elif scanner.scan(r'@@[A-Za-z_][A-Za-z_0-9]*'):
                    token = Name.Label
                elif scanner.scan(r'[A-Za-z_][A-Za-z_0-9]*'):
                    lowercase_name = scanner.match.lower()
                    if lowercase_name in self.ASM_INSTRUCTIONS:
                        token = Keyword
                    elif lowercase_name in self.ASM_REGISTERS:
                        token = Name.Builtin
                    else:
                        token = Name
                elif scanner.scan(r'[-+*\/=<>:;,.@\^]+'):
                    token = Operator
                elif scanner.scan(r'[\(\)\[\]]+'):
                    token = Punctuation
                elif scanner.scan(r'\$[0-9A-Fa-f]+'):
                    token = Number.Hex
                elif scanner.scan(r'\d+(?![eE]|\.[^.])'):
                    token = Number.Integer
                elif scanner.scan(r'\d+(\.\d+([eE][+-]?\d+)?|[eE][+-]?\d+)'):
                    token = Number.Float
                else:
                    scanner.get_char()
                    stack.pop()

            # save the dot!!!11
            if scanner.match.strip():
                was_dot = scanner.match == '.'
            yield scanner.start_pos, token, scanner.match or ''


# Multi-Dialect Modula-2 Lexer
class Modula2Lexer(RegexLexer):
    """
    For `Modula-2 <http://www.modula2.org/>`_ source code.
    
    The Modula-2 lexer supports several dialects.  By default, it operates in
    fallback mode, recognising the *combined* reserved words and builtins of
    the PIM, ISO and R10 dialects.
    
    To select a specific dialect one of several dialect options may be passed
    or embedded into a source file.
    
    Dialect Options:
    
    `m2pim`
        Select PIM Modula-2 dialect (default: False).
    `m2iso`
        Select ISO Modula-2 dialect (default: False).
    `m2r10`
        Select Modula-2 R10 dialect (default: False).
    `objm2`
        Select Objective Modula-2 dialect (default: False).
    `m2iso+aglet`
        Select Aglet Modula-2 extended ISO dialect (default: False).
    `m2pim+gm2`
        Select GNU Modula-2 extended PIM dialect (default: False).
    `m2iso+p1`
        Select p1 Modula-2 extended ISO dialect (default: False).
    `m2iso+xds`
        Select XDS Modula-2 extended ISO dialect (default: False).

    Passing a Dialect Option via Unix Commandline Interface
    
    The example below shows how to invoke the lexer from a Unix command line
    to render a Modula-2 source file to HTML output, using the ISO dialect:
    
    `$ pygmentize -O full,m2iso=True -f html -o /path/to/output /path/to/input`

    
    Embedding a Dialect Option:
    
    A dialect option may be embedded in a source file in form of a dialect
    tag, a specially formatted comment that specifies a dialect option.
    
    Dialect Tag EBNF:
        
    dialectTag :
        OpeningCommentDelim Prefix dialectOption ClosingCommentDelim ;
    
    dialectOption :
        'm2pim' | 'm2iso' | 'm2r10' | 'objm2' |
        'm2iso+aglet' | 'm2pim+gm2' | 'm2iso+p1' | 'm2iso+xds' ;

    Prefix : '!' ;

    OpeningCommentDelim : '(*' ;

    ClosingCommentDelim : '*)' ;
    
    No whitespace is permitted between the tokens of a dialect tag.
    
    In the event that a source file contains multiple dialect tags, the first
    tag that contains a valid dialect option will be recognised and any
    subsequent tags will be ignored.  Ideally, a dialect tag is placed
    at the beginning of a source file.
    
    An embedded dialect tag overrides a dialect option set via command line.

    The example below shows how to embed a dialect tag for the ISO dialect:
    
    `(*!m2iso*) (* Copyright (C) 2015, The Flintstones Inc. *)
    DEFINITION MODULE Jurassic;
    ...`
    
    
    Algol Presentation Style Option:
    
    The Modula-2 lexer supports Algol presentation style.  In this style
    reserved words are rendered lowercase boldface and builtins are
    rendered lowercase boldface italic.
    
    The example below shows how to invoke the lexer from a Unix command line
    to render a Modula-2 source file in Algol presentation style:
    
    `$ pygmentize -O full,style=algol -f html -o /path/to/output /path/to/input`
    
    .. versionadded:: 1.3
    """
    name = 'Modula-2'
    aliases = ['modula2', 'm2']
    filenames = ['*.def', '*.mod']
    mimetypes = ['text/x-modula2']

    flags = re.MULTILINE | re.DOTALL
    
    tokens = {
        'whitespace': [
         (r'\n+', Text),  # blank lines
         (r'\s+', Text),  # whitespace
        ],
        'dialecttags': [
        # PIM Dialect Tag
         (r'\(\*!m2pim\*\)', Comment.Preproc), 
        # ISO Dialect Tag
         (r'\(\*!m2iso\*\)', Comment.Preproc), 
        # M2R10 Dialect Tag
         (r'\(\*!m2r10\*\)', Comment.Preproc), 
        # ObjM2 Dialect Tag
         (r'\(\*!objm2\*\)', Comment.Preproc), 
        # Aglet Extensions Dialect Tag
         (r'\(\*!m2iso\+aglet\*\)', Comment.Preproc), 
        # GNU Extensions Dialect Tag
         (r'\(\*!m2pim\+gm2\*\)', Comment.Preproc), 
        # p1 Extensions Dialect Tag
         (r'\(\*!m2iso\+p1\*\)', Comment.Preproc), 
        # XDS Extensions Dialect Tag
         (r'\(\*!m2iso\+xds\*\)', Comment.Preproc), 
        ],
        'identifiers': [
            (r'([a-zA-Z_$][\w$]*)', Name),
        ],
        'numliterals': [
        #
        # Prefixed literals (M2R10 + ObjM2)
        #
        # Base-2, with optional digit group separators
          (r'0b[01]+(\'[01]+)*', Number.Bin),
        # Base-16, with optional digit group separators
          (r'0[ux][0-9A-F]+(\'[0-9A-F]+)*', Number.Hex),
        #
        # Plain literals (M2R10 + ObjM2)
        #
        # Base-10 whole and real numbers,
        # with mandatory integral part,
        # followed by optional fractional part with optional exponent,
        # and each individual part with optional digit group separators
          (r'[0-9]+(\'[0-9]+)*(\.[0-9]+(\'[0-9]+)?(e[+-]?[0-9]+(\'[0-9]+)*)?)?', Number.Float),
        #
        # Plain literals (PIM + ISO)
        #
        # Base-10, whole and real numbers,
        # with mandatory integral part,
        # followed by optional fractional part with optional exponent
          (r'[0-9]+(\.[0-9]+(E[+-]?[0-9]+)?)?', Number.Float),
        #
        # Suffixed literals (PIM + ISO)
        #
        # Base-8, number
          (r'[0-7]+B', Number.Oct),
        # Base-8, character code
          (r'[0-7]+C', Number.Oct),
        # Base-16, number
          (r'[0-9A-F]+H', Number.Hex),
        ],
        'strings': [
            (r"'(\\\\|\\'|[^'])*'", String),  # single quoted string
            (r'"(\\\\|\\"|[^"])*"', String),  # double quoted string
        ],
        'operators': [
        #
        # Operators Common To PIM, ISO, M2R10 and ObjM2
        #
        # Arithmetic Operators
          (r'[+-]', Operator),
          (r'[*/]', Operator),
        # Relational Operators
          (r'[=#<>]', Operator),
        # Less-Or-Equal, Subset
          (r'<=', Operator),
        # Greater-Or-Equal, Superset
          (r'>=', Operator),
        # Assignment Symbol
          (r':=', Operator),
        # Range Symbol
          (r'\.\.', Operator),
        # Dereferencing Operator
          (r'\^', Operator),
        # Tilde Symbol (varying semantics)
          (r'~', Operator),
        #
        # Operators Common To PIM and ISO
        #
        # Logical Conjunction synonym
          (r'&', Operator),
        # Inequality synonym
          (r'<>', Operator),
        #
        # Operators Common To ISO, M2R10 and ObjM2
        #
        # At Symbol (varying semantics)
          (r'@', Operator),
        #
        # Operators Common To M2R10 and ObjM2
        #
        # Array Concatenation Operator
          (r'\+>', Operator),
        # Dot Product Operator
          (r'\*\.', Operator),
        # Set Difference Operator
          (r'\\', Operator),
        # Type Conversion Operator
          (r'::', Operator),
        # Identity Operator
          (r'==', Operator),
        # Postfix Increment Mutator
          (r'\+\+', Operator),
        # Postfix Decrement Mutator
          (r'--', Operator),
        # Undetermined Property Prefix
          (r'\?', Operator),
        #
        # Operators Unique To ObjM2
        #
        # Smalltalk Message Prefix
          (r'`', Operator),
        ],
        'punctuation': [
        # Punctuation Common to PIM, ISO, M2R10 and ObjM2
            (r'[\(\)\[\]{},.:;\|]', Punctuation),
        # Punctuation Common to M2R10 and ObjM2
          (r'->', Punctuation),
        # Punctuation Unique to ISO
          (r'!', Punctuation),
        ],
        'comments': [
        # Single Line Comment (M2R10 and ObjM2)
          (r'^//.*?\n', Comment.Single),
        # Block Comment (PIM, ISO, M2R10 and ObjM2)
          (r'\(\*([^$].*?)\*\)', Comment.Multiline),
        # Template Block Comment (M2R10 and ObjM2)
          (r'/\*(.*?)\*/', Comment.Multiline),
        ],
        'pragmas': [
        # Pragmas Common to ISO, M2R10 and ObjM2
          (r'<\*(.*?)\*>', Comment.Preproc),
        # Pragmas Unique to PIM
          (r'\(\*\$(.*?)\*\)', Comment.Preproc),
        ],
        'root': [
            include('whitespace'),
            include('dialecttags'),
            include('comments'),
            include('pragmas'),
            include('identifiers'),
            include('numliterals'),
            include('strings'),
            include('operators'),
            include('punctuation'),
        ]
    }

    # Common Reserved Words Dataset
    common_reserved_words = [
        # 37 common reserved words
        'AND', 'ARRAY', 'BEGIN', 'BY', 'CASE', 'CONST', 'DEFINITION', 'DIV',
        'DO', 'ELSE', 'ELSIF', 'END', 'EXIT', 'FOR', 'FROM', 'IF',
        'IMPLEMENTATION', 'IMPORT', 'IN', 'LOOP', 'MOD', 'MODULE', 'NOT',
        'OF', 'OR', 'POINTER', 'PROCEDURE', 'RECORD', 'REPEAT', 'RETURN',
        'SET', 'THEN', 'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE',
    ]

    # Common Builtins Dataset
    common_builtins = [
        # 17 common builtins
        'ABS', 'BOOLEAN', 'CARDINAL', 'CHAR', 'CHR', 'FALSE', 'INTEGER',
        'LONGINT', 'LONGREAL', 'MAX', 'MIN', 'NIL', 'ODD', 'ORD', 'REAL', 
        'TRUE', 'VAL',
    ]
    
    # PIM Modula-2 Additional Reserved Words Dataset
    pim_additional_reserved_words = [
        # 3 additional reserved words
        'EXPORT', 'QUALIFIED', 'WITH',
    ]
    
    # PIM Modula-2 Additional Builtins Dataset
    pim_additional_builtins = [
        # 15 additional builtins
        'BITSET', 'CAP', 'DEC', 'DISPOSE', 'EXCL', 'FLOAT', 'HALT', 'HIGH',
        'INC', 'INCL', 'NEW', 'NIL', 'PROC', 'SIZE', 'TRUNC',
    ]
    
    # ISO Modula-2 Additional Reserved Words Dataset
    iso_additional_reserved_words = [
        # 20 additional reserved words
        'ABSTRACT', 'AS', 'CLASS', 'EXCEPT', 'EXPORT', 'FINALLY', 'FORWARD',
        'GUARD', 'INHERIT', 'OVERRIDE', 'PACKEDSET', 'QUALIFIED', 'READONLY',
        'REM', 'RETRY', 'REVEAL', 'TRACED', 'UNSAFEGUARDED', 'WITH',
    ]
    
    # ISO Modula-2 Additional Builtins Dataset
    iso_additional_builtins = [
        # 30 additional builtins
        'CREATE', 'BITSET', 'CAP', 'CMPLX', 'COMPLEX', 'DEC', 'DESTROY',
        'DISPOSE', 'EMPTY', 'EXCL', 'FLOAT', 'HALT', 'HIGH', 'IM', 'INC',
        'INCL', 'INT', 'INTERRUPTIBLE', 'ISMEMBER', 'LENGTH', 'LFLOAT',
        'LONGCOMPLEX', 'NEW', 'PROC', 'PROTECTION', 'RE', 'SELF', 'SIZE',
        'TRUNC', 'UNINTERRUBTIBLE',
    ]
    
    # Modula-2 R10 reserved words in addition to the common set
    m2r10_additional_reserved_words = [
        # 12 additional reserved words
        'ALIAS', 'ARGLIST', 'BLUEPRINT', 'COPY', 'GENLIB', 'INDETERMINATE',
        'NEW', 'NONE', 'OPAQUE', 'REFERENTIAL', 'RELEASE', 'RETAIN',
    ]

    # Modula-2 R10 builtins in addition to the common set
    m2r10_additional_builtins = [
        # 41 additional builtins
        'ADDRESS', 'BYTE', 'CAST', 'CARDINAL', 'COUNT', 'EMPTY', 'EXISTS',
        'INSERT', 'LENGTH', 'LONGCARD', 'OCTET', 'PTR', 'PRED', 'READ',
        'READNEW', 'REMOVE', 'RETRIEVE', 'SORT', 'STORE', 'SUBSET', 'SUCC',
        'SXF', 'TBASE', 'TBUILTIN', 'TDYN', 'TLIMIT', 'TLITERAL', 'TMAX',
        'TMAXEXP', 'TMIN', 'TMINEXP', 'TNIL', 'TPRECISION', 'TPROPERTIES',
        'TREFC', 'TRUE', 'TSIZE', 'UNICHAR', 'UNSAFE', 'WRITE', 'WRITEF',
    ]
    
    # Objective Modula-2 Extensions
    # reserved words in addition to Modula-2 R10
    objm2_additional_reserved_words = [
        # 16 additional reserved words
        'BYCOPY', 'BYREF', 'CLASS', 'CONTINUE', 'CRITICAL', 'INOUT', 'METHOD',
        'ON', 'OPTIONAL', 'OUT', 'PRIVATE', 'PROTECTED', 'PROTOCOL', 'PUBLIC',
        'SUPER', 'TRY',
    ]

    # Objective Modula-2 Extensions
    # builtins in addition to Modula-2 R10
    objm2_additional_builtins = [
        # 3 additional builtins
        'OBJECT', 'NO', 'YES',
    ]

    # Aglet Extensions
    # reserved words in addition to ISO Modula-2
    aglet_additional_reserved_words = [
        # No additional reserved words
    ]

    # Aglet Extensions
    # builtins in addition to ISO Modula-2
    aglet_additional_builtins = [
        # 9 additional builtins
        'BITSET8', 'BITSET16', 'BITSET32', 'CARDINAL8', 'CARDINAL16',
        'CARDINAL32', 'INTEGER8', 'INTEGER16', 'INTEGER32',
    ]

    # GNU Extensions
    # reserved words in addition to PIM Modula-2
    gm2_additional_reserved_words = [
        # 10 additional reserved words
        'ASM', '__ATTRIBUTE__', '__BUILTIN__', '__COLUMN__', '__DATE__',
        '__FILE__', '__FUNCTION__', '__LINE__', '__MODULE__', 'VOLATILE',
    ]

    # GNU Extensions
    # builtins in addition to PIM Modula-2
    gm2_additional_builtins = [
        # 21 additional builtins
        'BITSET8', 'BITSET16', 'BITSET32', 'CARDINAL8', 'CARDINAL16',
        'CARDINAL32', 'CARDINAL64', 'COMPLEX32', 'COMPLEX64', 'COMPLEX96',
        'COMPLEX128', 'INTEGER8', 'INTEGER16', 'INTEGER32', 'INTEGER64',
        'REAL8', 'REAL16', 'REAL32', 'REAL96', 'REAL128', 'THROW',
    ]

    # p1 Extensions
    # reserved words in addition to ISO Modula-2
    p1_additional_reserved_words = [
        # No additional reserved words
    ]

    # p1 Extensions
    # builtins in addition to ISO Modula-2
    p1_additional_builtins = [
        # 3 additional builtins
        'BCD', 'LONGBCD', 'EMPTY',
    ]

    # XDS Extensions
    # reserved words in addition to ISO Modula-2
    xds_additional_reserved_words = [
        # 1 additional reserved word
        'SEQ',
    ]

    # XDS Extensions
    # builtins in addition to ISO Modula-2
    xds_additional_builtins = [
        # 18 additional builtins
        'ASH', 'ASSERT', 'BOOL8', 'BOOL16', 'BOOL32', 'CARD8', 'CARD16',
        'CARD32', 'DIFFADR_TYPE', 'ENTIER', 'INDEX', 'INT8', 'INT16',
        'INT32', 'LEN', 'LONGCARD', 'SHORTCARD', 'SHORTINT',
    ]
    
    # Dialect modes
    dialects = (
        'unknown',
        'm2pim', 'm2iso', 'm2r10', 'objm2',
        'm2iso+aglet', 'm2pim+gm2', 'm2iso+p1', 'm2iso+xds',
    )
        
    # Reserved Words Database
    reserved_words_db = {
        # Reserved words for unknown dialect
        'unknown' : [
            common_reserved_words,
            pim_additional_reserved_words,
            iso_additional_reserved_words,
            m2r10_additional_reserved_words,
        ],

        # Reserved words for PIM Modula-2
        'm2pim' : [
            common_reserved_words,
            pim_additional_reserved_words,
        ],

        # Reserved words for Modula-2 R10
        'm2iso' : [
            common_reserved_words,
            iso_additional_reserved_words,
        ],

        # Reserved words for ISO Modula-2
        'm2r10' : [
            common_reserved_words,
            m2r10_additional_reserved_words,
        ],

        # Reserved words for Objective Modula-2
        'objm2' : [
            common_reserved_words,
            m2r10_additional_reserved_words,
            objm2_additional_reserved_words,
        ],

        # Reserved words for Aglet Modula-2 Extensions
        'm2iso+aglet' : [
            common_reserved_words,
            iso_additional_reserved_words,
            aglet_additional_reserved_words,
        ],

        # Reserved words for GNU Modula-2 Extensions
        'm2pim+gm2' : [
            common_reserved_words,
            pim_additional_reserved_words,
            gm2_additional_reserved_words,
        ],

        # Reserved words for p1 Modula-2 Extensions
        'm2iso+p1' : [
            common_reserved_words,
            iso_additional_reserved_words,
            p1_additional_reserved_words,
        ],

        # Reserved words for XDS Modula-2 Extensions
        'm2iso+xds' : [
            common_reserved_words,
            iso_additional_reserved_words,
            xds_additional_reserved_words,
        ],
    }

    # Builtins Database
    builtins_db = {
        # Builtins for unknown dialect
        'unknown' : [
            common_builtins,
            pim_additional_builtins,
            iso_additional_builtins,
            m2r10_additional_builtins,
        ],

        # Builtins for PIM Modula-2
        'm2pim' : [
            common_builtins,
            pim_additional_builtins,
        ],

        # Builtins for ISO Modula-2
        'm2iso' : [
            common_builtins,
            iso_additional_builtins,
        ],

        # Builtins for ISO Modula-2
        'm2r10' : [
            common_builtins,
            m2r10_additional_builtins,
        ],

        # Builtins for Objective Modula-2
        'objm2' : [
            common_builtins,
            m2r10_additional_builtins,
            objm2_additional_builtins,
        ],

        # Builtins for Aglet Modula-2 Extensions
        'm2iso+aglet' : [
            common_builtins,
            iso_additional_builtins,
            aglet_additional_builtins,
        ],

        # Builtins for GNU Modula-2 Extensions
        'm2pim+gm2' : [
            common_builtins,
            pim_additional_builtins,
            gm2_additional_builtins,
        ],

        # Builtins for p1 Modula-2 Extensions
        'm2iso+p1' : [
            common_builtins,
            iso_additional_builtins,
            p1_additional_builtins,
        ],

        # Builtins for XDS Modula-2 Extensions
        'm2iso+xds' : [
            common_builtins,
            iso_additional_builtins,
            xds_additional_builtins,
        ],
    }
    
    # initialise a lexer instance
    def __init__(self, **options):
        #
        # Alias for unknown dialect
        global UNKNOWN
        UNKNOWN = self.dialects[0]
        #
        # check dialect options and set dialect
        #
        # PIM Modula-2
        if get_bool_opt(options, 'm2pim', False):
            self.set_dialect('m2pim')
        #
        # ISO Modula-2
        elif get_bool_opt(options, 'm2iso', False):
            self.set_dialect('m2iso')
        #
        # Modula-2 R10
        elif get_bool_opt(options, 'm2r10', False):
            self.set_dialect('m2r10')
        #
        # Objective Modula-2
        elif get_bool_opt(options, 'objm2', False):
            self.set_dialect('m2r10')
        #
        # Aglet Extensions to ISO
        elif get_bool_opt(options, 'm2iso+aglet', False):
            self.set_dialect('m2iso+aglet')
        #
        # GNU Extensions to PIM
        elif get_bool_opt(options, 'm2pim+gm2', False):
            self.set_dialect('m2pim+gm2')
        #
        # p1 Extensions to ISO
        elif get_bool_opt(options, 'm2iso+p1', False):
            self.set_dialect('m2iso+p1')
        #
        # XDS Extensions to ISO
        elif get_bool_opt(options, 'm2iso+xds', False):
            self.set_dialect('m2iso+xds')
        #
        # Fallback Mode (DEFAULT)
        else:
            self.set_dialect(UNKNOWN)
        #
        self.dialect_set_by_tag = False
        #
        # check style options
        #
        styles = get_list_opt(options, 'style', [])
        #
        # use lowercase mode for algol style
        if 'algol' in styles:
            self.lowercase_presentation = True
        else:
            self.lowercase_presentation = False
        #
        # call superclass initialiser
        RegexLexer.__init__(self, **options)
    
    # Set lexer to a specified dialect
    def set_dialect(self, dialect_id):
        #
        #if __debug__:
        #    print 'entered set_dialect with arg: ', dialect_id
        #
        # check dialect name against known dialects
        if dialect_id not in self.dialects:
            dialect = UNKNOWN # default
        else:
            dialect = dialect_id
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
        # update lexer state
        self.dialect = dialect
        self.reserved_words = reswords_set
        self.builtins = builtins_set    
        #
        #if __debug__:
        #    print 'exiting set_dialect with self.dialect: ', self.dialect
    
    # Extracts a dialect name from a dialect tag comment string  and checks
    # the extracted name against known dialects.  If a match is found,  the
    # matching name is returned, otherwise dialect id 'unknown' is returned
    def get_dialect_from_dialect_tag(self, dialect_tag):
        #
        #if __debug__:
        #    print 'entered get_dialect_from_dialect_tag with arg: ', dialect_tag
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
            #if __debug__:
            #    print 'dialect tag found'
            #
            # extract dialect indicator
            indicator = dialect_tag[indicator_start:indicator_end]
            #
            #if __debug__:
            #    print 'extracted: ', indicator
            #
            # check against known dialects
            for index in range(1, len(self.dialects)):
                #
                #if __debug__:
                #    print 'dialects[', index, ']: ', self.dialects[index]
                #
                if indicator == self.dialects[index]:
                    #
                    #if __debug__:
                    #    print 'matching dialect found'
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
            if not self.dialect_set_by_tag and token == Comment.Preproc:
                indicated_dialect = self.get_dialect_from_dialect_tag(value)
                if indicated_dialect != UNKNOWN:
                    # token is a dialect indicator
                    # reset reserved words and builtins
                    self.set_dialect(indicated_dialect)
                    self.dialect_set_by_tag = True
            #
            # check for reserved words and predefined identifiers
            if token is Name:
                if value in self.reserved_words:
                    token = Keyword.Reserved
                    if self.lowercase_presentation:
                        value = value.lower()
                #
                elif value in self.builtins:
                    token = Name.Builtin
                    if self.lowercase_presentation:
                        value = value.lower()
            # return result
            yield index, token, value


class AdaLexer(RegexLexer):
    """
    For Ada source code.

    .. versionadded:: 1.3
    """

    name = 'Ada'
    aliases = ['ada', 'ada95', 'ada2005']
    filenames = ['*.adb', '*.ads', '*.ada']
    mimetypes = ['text/x-ada']

    flags = re.MULTILINE | re.IGNORECASE

    tokens = {
        'root': [
            (r'[^\S\n]+', Text),
            (r'--.*?\n', Comment.Single),
            (r'[^\S\n]+', Text),
            (r'function|procedure|entry', Keyword.Declaration, 'subprogram'),
            (r'(subtype|type)(\s+)(\w+)',
             bygroups(Keyword.Declaration, Text, Keyword.Type), 'type_def'),
            (r'task|protected', Keyword.Declaration),
            (r'(subtype)(\s+)', bygroups(Keyword.Declaration, Text)),
            (r'(end)(\s+)', bygroups(Keyword.Reserved, Text), 'end'),
            (r'(pragma)(\s+)(\w+)', bygroups(Keyword.Reserved, Text,
                                             Comment.Preproc)),
            (r'(true|false|null)\b', Keyword.Constant),
            (words((
                'Address', 'Byte', 'Boolean', 'Character', 'Controlled', 'Count', 'Cursor',
                'Duration', 'File_Mode', 'File_Type', 'Float', 'Generator', 'Integer', 'Long_Float',
                'Long_Integer', 'Long_Long_Float', 'Long_Long_Integer', 'Natural', 'Positive',
                'Reference_Type', 'Short_Float', 'Short_Integer', 'Short_Short_Float',
                'Short_Short_Integer', 'String', 'Wide_Character', 'Wide_String'), suffix=r'\b'),
             Keyword.Type),
            (r'(and(\s+then)?|in|mod|not|or(\s+else)|rem)\b', Operator.Word),
            (r'generic|private', Keyword.Declaration),
            (r'package', Keyword.Declaration, 'package'),
            (r'array\b', Keyword.Reserved, 'array_def'),
            (r'(with|use)(\s+)', bygroups(Keyword.Namespace, Text), 'import'),
            (r'(\w+)(\s*)(:)(\s*)(constant)',
             bygroups(Name.Constant, Text, Punctuation, Text,
                      Keyword.Reserved)),
            (r'<<\w+>>', Name.Label),
            (r'(\w+)(\s*)(:)(\s*)(declare|begin|loop|for|while)',
             bygroups(Name.Label, Text, Punctuation, Text, Keyword.Reserved)),
            (words((
                'abort', 'abs', 'abstract', 'accept', 'access', 'aliased', 'all',
                'array', 'at', 'begin', 'body', 'case', 'constant', 'declare',
                'delay', 'delta', 'digits', 'do', 'else', 'elsif', 'end', 'entry',
                'exception', 'exit', 'interface', 'for', 'goto', 'if', 'is', 'limited',
                'loop', 'new', 'null', 'of', 'or', 'others', 'out', 'overriding',
                'pragma', 'protected', 'raise', 'range', 'record', 'renames', 'requeue',
                'return', 'reverse', 'select', 'separate', 'subtype', 'synchronized',
                'task', 'tagged', 'terminate', 'then', 'type', 'until', 'when',
                'while', 'xor'), prefix=r'\b', suffix=r'\b'),
             Keyword.Reserved),
            (r'"[^"]*"', String),
            include('attribute'),
            include('numbers'),
            (r"'[^']'", String.Character),
            (r'(\w+)(\s*|[(,])', bygroups(Name, using(this))),
            (r"(<>|=>|:=|[()|:;,.'])", Punctuation),
            (r'[*<>+=/&-]', Operator),
            (r'\n+', Text),
        ],
        'numbers': [
            (r'[0-9_]+#[0-9a-f]+#', Number.Hex),
            (r'[0-9_]+\.[0-9_]*', Number.Float),
            (r'[0-9_]+', Number.Integer),
        ],
        'attribute': [
            (r"(')(\w+)", bygroups(Punctuation, Name.Attribute)),
        ],
        'subprogram': [
            (r'\(', Punctuation, ('#pop', 'formal_part')),
            (r';', Punctuation, '#pop'),
            (r'is\b', Keyword.Reserved, '#pop'),
            (r'"[^"]+"|\w+', Name.Function),
            include('root'),
        ],
        'end': [
            ('(if|case|record|loop|select)', Keyword.Reserved),
            ('"[^"]+"|[\w.]+', Name.Function),
            ('\s+', Text),
            (';', Punctuation, '#pop'),
        ],
        'type_def': [
            (r';', Punctuation, '#pop'),
            (r'\(', Punctuation, 'formal_part'),
            (r'with|and|use', Keyword.Reserved),
            (r'array\b', Keyword.Reserved, ('#pop', 'array_def')),
            (r'record\b', Keyword.Reserved, ('record_def')),
            (r'(null record)(;)', bygroups(Keyword.Reserved, Punctuation), '#pop'),
            include('root'),
        ],
        'array_def': [
            (r';', Punctuation, '#pop'),
            (r'(\w+)(\s+)(range)', bygroups(Keyword.Type, Text, Keyword.Reserved)),
            include('root'),
        ],
        'record_def': [
            (r'end record', Keyword.Reserved, '#pop'),
            include('root'),
        ],
        'import': [
            (r'[\w.]+', Name.Namespace, '#pop'),
            default('#pop'),
        ],
        'formal_part': [
            (r'\)', Punctuation, '#pop'),
            (r'\w+', Name.Variable),
            (r',|:[^=]', Punctuation),
            (r'(in|not|null|out|access)\b', Keyword.Reserved),
            include('root'),
        ],
        'package': [
            ('body', Keyword.Declaration),
            ('is\s+new|renames', Keyword.Reserved),
            ('is', Keyword.Reserved, '#pop'),
            (';', Punctuation, '#pop'),
            ('\(', Punctuation, 'package_instantiation'),
            ('([\w.]+)', Name.Class),
            include('root'),
        ],
        'package_instantiation': [
            (r'("[^"]+"|\w+)(\s+)(=>)', bygroups(Name.Variable, Text, Punctuation)),
            (r'[\w.\'"]', Text),
            (r'\)', Punctuation, '#pop'),
            include('root'),
        ],
    }