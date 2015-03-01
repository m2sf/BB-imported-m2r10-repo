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


class Modula2Lexer(RegexLexer):
    """
    For `Modula-2 <http://www.modula2.org/>`_ source code.
    
    By default, the Modula-2 lexer recognises the combined reserved words and
    builtins of the PIM, ISO and M2R10 dialects.  To limit the lexer to a
    specific dialect, one the following options must be set to True:

    `pim` or `m2pim`
        Select PIM Modula-2 dialect (default: False).
    `iso` or `m2iso`
        Select ISO Modula-2 dialect (default: False).
    `r10` or `m2r10`
        Select Modula-2 R10 dialect (default: False).
    `objm2`
        Select Objective Modula-2 dialect (default: False).
    
    An additional option is available when PIM is selected:
    
    `gm2ext`
        Also highlight GNU extensions to PIM (default: False).

    The example below shows how to invoke the lexer from a Unix command line
    to render a Modula-2 source file to HTML output, using the ISO dialect:
    
    $ ./pygmentize -O full,iso=True -f html -o /path/to/output /path/to/input
    
    .. versionadded:: 1.3
    """
    name = 'Modula-2'
    aliases = ['modula2', 'm2']
    filenames = ['*.def', '*.mod']
    mimetypes = ['text/x-modula2']

    flags = re.MULTILINE | re.DOTALL

    tokens = {
        'magicbang': [
        # PIM Dialect Indicator
         (r'\(\*!m2pim\*\)', Comment.Preproc), 
        # ISO Dialect Indicator
         (r'\(\*!m2iso\*\)', Comment.Preproc), 
        # M2R10 Dialect Indicator
         (r'\(\*!m2r10\*\)', Comment.Preproc), 
        # ObjM2 Dialect Indicator
         (r'\(\*!objm2\*\)', Comment.Preproc), 
        # GNU Extensions Dialect Indicator
         (r'\(\*!gm2ext\*\)', Comment.Preproc), 
        ],
        'whitespace': [
         (r'\n+', Text),  # blank lines
         (r'\s+', Text),  # whitespace
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
            include('magicbang'),
            include('comments'),
            include('pragmas'),
            include('identifiers'),
            include('numliterals'),
            include('strings'),
            include('operators'),
            include('punctuation'),
        ]
    }

    fallback_mode_reserved_words = [
        # 58 reserved words from PIM, ISO and R10
        'ALIAS', 'AND', 'ARGLIST', 'ARRAY', 'BEGIN', 'BLUEPRINT', 'BY', 'CASE',
        'CONST', 'COPY', 'DEFINITION', 'DIV', 'DO', 'ELSE', 'ELSIF', 'END',
        'EXCEPT', 'EXIT', 'EXPORT', 'FINALLY', 'FOR', 'FORWARD', 'FROM',
        'GENLIB', 'IF', 'IMPLEMENTATION', 'IMPORT', 'IN', 'INDETERMINATE',
        'LOOP', 'MOD', 'MODULE', 'NEW', 'NONE', 'NOT', 'OF', 'OR', 'OPAQUE',
        'PACKEDSET', 'POINTER', 'PROCEDURE', 'QUALIFIED', 'RECORD',
        'REFERENTIAL', 'RELEASE', 'REM', 'REPEAT', 'RETAIN', 'RETRY', 'RETURN',
        'SET', 'THEN', 'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE', 'WITH',
    ]

    fallback_mode_highlight_identifiers = [
        # 79 builtins from PIM, ISO and R10
        'ABS', 'ADDRESS', 'BITSET', 'BOOLEAN', 'BYTE', 'CAP', 'CARDINAL',
        'CAST', 'CHAR', 'CHR', 'CMPLX', 'COMPLEX', 'COUNT', 'DEC', 'DISPOSE',
        'EMPTY', 'EXCL', 'EXISTS', 'FALSE', 'FLOAT', 'HALT', 'HIGH', 'IM',
        'INC', 'INCL', 'INSERT', 'INT', 'INTEGER', 'INTERRUPTIBLE', 'LENGTH',
        'LFLOAT', 'LONGCARD', 'LONGCOMPLEX', 'LONGINT', 'LONGREAL', 'MAX',
        'MIN', 'NIL', 'OCTET', 'ODD', 'ORD', 'PROC', 'PROTECTION', 'PRED',
        'PTR', 'RE', 'READ', 'READNEW', 'REAL', 'REMOVE', 'RETRIEVE', 'SIZE',
        'SORT', 'STORE', 'SUBSET', 'SUCC', 'SXF', 'TBASE', 'TBUILTIN', 'TDYN',
        'TLIMIT', 'TLITERAL', 'TMAX', 'TMAXEXP', 'TMIN', 'TMINEXP', 'TNIL',
        'TPRECISION', 'TPROPERTIES', 'TREFC', 'TRUE', 'TRUNC', 'TSIZE',
        'UNICHAR', 'UNINTERRUPTIBLE', 'UNSAFE', 'VAL', 'WRITE', 'WRITEF',
    ]

    pim_reserved_words = [
        # 40 reserved words
        'AND', 'ARRAY', 'BEGIN', 'BY', 'CASE', 'CONST', 'DEFINITION',
        'DIV', 'DO', 'ELSE', 'ELSIF', 'END', 'EXIT', 'EXPORT', 'FOR',
        'FROM', 'IF', 'IMPLEMENTATION', 'IMPORT', 'IN', 'LOOP', 'MOD',
        'MODULE', 'NOT', 'OF', 'OR', 'POINTER', 'PROCEDURE', 'QUALIFIED',
        'RECORD', 'REPEAT', 'RETURN', 'SET', 'THEN', 'TO', 'TYPE',
        'UNTIL', 'VAR', 'WHILE', 'WITH',
    ]

    pim_standard_identifiers = [
        # 31 pervasives
        'ABS', 'BITSET', 'BOOLEAN', 'CAP', 'CARDINAL', 'CHAR', 'CHR', 'DEC',
        'DISPOSE', 'EXCL', 'FALSE', 'FLOAT', 'HALT', 'HIGH', 'INC', 'INCL',
        'INTEGER', 'LONGINT', 'LONGREAL', 'MAX', 'MIN', 'NEW', 'NIL', 'ODD',
        'ORD', 'PROC', 'REAL', 'SIZE', 'TRUE', 'TRUNC', 'VAL',
    ]

    iso_reserved_words = [
        # 46 reserved words
        'AND', 'ARRAY', 'BEGIN', 'BY', 'CASE', 'CONST', 'DEFINITION', 'DIV',
        'DO', 'ELSE', 'ELSIF', 'END', 'EXCEPT', 'EXIT', 'EXPORT', 'FINALLY',
        'FOR', 'FORWARD', 'FROM', 'IF', 'IMPLEMENTATION', 'IMPORT', 'IN',
        'LOOP', 'MOD', 'MODULE', 'NOT', 'OF', 'OR', 'PACKEDSET', 'POINTER',
        'PROCEDURE', 'QUALIFIED', 'RECORD', 'REPEAT', 'REM', 'RETRY',
        'RETURN', 'SET', 'THEN', 'TO', 'TYPE', 'UNTIL', 'VAR', 'WHILE',
        'WITH',
    ]

    iso_pervasives = [
        # 42 pervasives
        'ABS', 'BITSET', 'BOOLEAN', 'CAP', 'CARDINAL', 'CHAR', 'CHR', 'CMPLX',
        'COMPLEX', 'DEC', 'DISPOSE', 'EXCL', 'FALSE', 'FLOAT', 'HALT', 'HIGH',
        'IM', 'INC', 'INCL', 'INT', 'INTEGER', 'INTERRUPTIBLE', 'LENGTH',
        'LFLOAT', 'LONGCOMPLEX', 'LONGINT', 'LONGREAL', 'MAX', 'MIN', 'NEW',
        'NIL', 'ODD', 'ORD', 'PROC', 'PROTECTION', 'RE', 'REAL', 'SIZE',
        'TRUE', 'TRUNC', 'UNINTERRUBTIBLE', 'VAL',
    ]

    m2r10_reserved_words = [
        # 49 reserved words
        'ALIAS', 'AND', 'ARGLIST', 'ARRAY', 'BEGIN', 'BLUEPRINT', 'BY', 'CASE',
        'CONST', 'COPY', 'DEFINITION', 'DIV', 'DO', 'ELSE', 'ELSIF', 'END',
        'EXIT', 'FOR', 'FROM', 'GENLIB', 'IF', 'IMPLEMENTATION', 'IMPORT',
        'IN', 'INDETERMINATE', 'LOOP', 'MOD', 'MODULE', 'NEW', 'NONE', 'NOT',
        'OF', 'OPAQUE', 'OR', 'POINTER', 'PROCEDURE', 'RECORD', 'REFERENTIAL',
        'RELEASE', 'REPEAT', 'RETAIN', 'RETURN', 'SET', 'THEN', 'TO', 'TYPE',
        'UNTIL', 'VAR', 'WHILE',
    ]

    m2r10_reserved_identifiers = [
        # 56 reserved identifiers
        'ABS', 'ADDRESS', 'BOOLEAN', 'BYTE', 'CAST', 'CARDINAL', 'CHAR', 'CHR',
        'COUNT', 'EMPTY', 'EXISTS', 'FALSE', 'INSERT', 'INTEGER', 'LENGTH',
        'LONGCARD', 'LONGINT', 'LONGREAL', 'MAX', 'MIN', 'NIL', 'OCTET', 'ODD',
        'ORD', 'PTR', 'PRED', 'READ', 'READNEW', 'REAL', 'REMOVE', 'RETRIEVE',
        'SORT', 'STORE', 'SUBSET', 'SUCC', 'SXF', 'TBASE', 'TBUILTIN', 'TDYN',
        'TLIMIT', 'TLITERAL', 'TMAX', 'TMAXEXP', 'TMIN', 'TMINEXP', 'TNIL',
        'TPRECISION', 'TPROPERTIES', 'TREFC', 'TRUE', 'TSIZE', 'UNICHAR',
        'UNSAFE', 'VAL', 'WRITE', 'WRITEF',
    ]

    objm2_reserved_words = [
        # OO extensions to Modula-2 R10, 16 reserved words
        'BYCOPY', 'BYREF', 'CLASS', 'CONTINUE', 'CRITICAL', 'INOUT', 'METHOD',
        'ON', 'OPTIONAL', 'OUT', 'PRIVATE', 'PROTECTED', 'PROTOCOL', 'PUBLIC',
        'SUPER', 'TRY',
    ]

    objm2_reserved_identifiers = [
        # OO extensions to Modula-2 R10, 3 reserved identifiers
        'OBJECT', 'NO', 'YES',
    ]

    gnu_reserved_words = [
        # GNU extensions to PIM, 10 reserved words
        'ASM', '__ATTRIBUTE__', '__BUILTIN__', '__COLUMN__', '__DATE__',
        '__FILE__', '__FUNCTION__', '__LINE__', '__MODULE__', 'VOLATILE',
    ]

    gnu_builtin_identifiers = [
        # GNU extensions to PIM, 21 identifiers
        'BITSET8', 'BITSET16', 'BITSET32', 'CARDINAL8', 'CARDINAL16',
        'CARDINAL32', 'CARDINAL64', 'COMPLEX32', 'COMPLEX64', 'COMPLEX96',
        'COMPLEX128', 'INTEGER8', 'INTEGER16', 'INTEGER32', 'INTEGER64',
        'REAL8', 'REAL16', 'REAL32', 'REAL96', 'REAL128', 'THROW',
    ]

    def __init__(self, **options):
        self.reserved_words = set()
        self.builtins = set()

        # PIM Modula-2
        if get_bool_opt(options, 'pim', False) or \
             get_bool_opt(options, 'm2pim', False):
            self.reserved_words.update(self.pim_reserved_words)
            self.builtins.update(self.pim_standard_identifiers)
            if get_bool_opt(options, 'gm2ext', False):
                self.reserved_words.update(self.pim_reserved_words)
                self.reserved_words.update(self.gnu_reserved_words)
                self.builtins.update(self.pim_standard_identifiers)
                self.builtins.update(self.gnu_builtin_identifiers)

        # ISO Modula-2
        elif get_bool_opt(options, 'iso', False) or \
             get_bool_opt(options, 'm2iso', False):
            self.reserved_words.update(self.iso_reserved_words)
            self.builtins.update(self.iso_pervasives)

        # Modula-2 R10
        elif get_bool_opt(options, 'r10', False) or \
             get_bool_opt(options, 'm2r10', False):
            self.reserved_words.update(self.m2r10_reserved_words)
            self.builtins.update(self.m2r10_reserved_identifiers)

        # Objective Modula-2
        elif get_bool_opt(options, 'objm2', False):
            self.reserved_words.update(self.m2r10_reserved_words)
            self.reserved_words.update(self.objm2_reserved_words)
            self.builtins.update(self.m2r10_reserved_identifiers)
            self.builtins.update(self.objm2_reserved_identifiers)
        # Fallback Mode (DEFAULT)
        else:
            self.reserved_words.update(self.fallback_mode_reserved_words)
            self.builtins.update(self.fallback_mode_highlight_identifiers)
        # initialise
        RegexLexer.__init__(self, **options)

    def get_tokens_unprocessed(self, text):
        for index, token, value in RegexLexer.get_tokens_unprocessed(self, text):
            # check for reserved words and predefined identifiers
            if token is Name:
                if value in self.reserved_words:
                    token = Keyword.Reserved
                elif value in self.builtins:
                    token = Name.Builtin
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