(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE M2Lexer;

(* Lexer for Modula-2 R10 Bootstrap Compiler *)

IMPORT Files, M2Params, M2Source;

FROM ISO646 IMPORT TAB, SPACE, NEWLINE,
  BACKSLASH, SINGLEQUOTE, DOUBLEQUOTE, isDigit;

FROM M2Tokens IMPORT Token, TokenValue;

FROM M2Source IMPORT Source, getChar, consumeChar, lookaheadChar,
  lookahead2, markLexeme, copyLexeme, getLineAndColumn, eof;


(* Lexer Descriptor *)

TYPE Lexer = POINTER TO LexerDescriptor;

TYPE LexerDescriptor = RECORD
  source : Source;
  status : Status;
  nextSymbol : Symbol;
  errorCount : CARDINAL;
END;


(* Operations *)

(* ---------------------------------------------------------------------------
 * procedure new ( lexer, filename, status )
 *  creates a new lexer instance, associated with filename
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE new ( VAR lexer : Lexer; filename : Filename; VAR s : Status );
 
BEGIN
 
  (* lexer must not have been initialised *)
  IF lexer # NIL THEN
    status := alreadyInitialised;
    RETURN
  END;
  
  (* allocate a lexer instance *)
  NEW(lexer);
  IF lexer = NIL THEN
    status := unableToAllocate;
    RETURN
  END;
  
  (* initialise source *)
  M2Source.new(lexer^.source, filename, sourceStatus);
  IF sourceStatus # M2Source.success THEN
  (* TO DO: status *)
    RETURN
  END;
  
  (* initialise error count and status *)
  errorCount := 0;
  status := success;
  
  (* read the first symbol to be returned *)
  consumeSym(lexer);
  
  RETURN
END new;


(* ---------------------------------------------------------------------------
 * procedure getSym ( lexer, symbol, lookaheadSymbol )
 *  passes and consumes current lookahead symbol, passes new lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE getSym ( lexer : Lexer; VAR sym, next : Symbol );

BEGIN
  
  (* nextSymbol holds current lookahead, pass it back in sym *)
  sym := lexer^.nextSymbol;
  
  (* consume the current and read the new lookahead symbol *)
  consumeSym(lexer);
  
  (* nextSymbol holds new lookahead, pass it back in next *)
  next := lexer^.nextSymbol;
  
  RETURN
END getSym;


(* ---------------------------------------------------------------------------
 * procedure consumeSym ( lexer )
 *  consumes current lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE consumeSym ( lexer : Lexer );

VAR
  ch, next, la2 : CHAR;
  sym : Symbol;

BEGIN
  (* ensure source is valid *)
  IF lexer = NIL THEN
  (* TO DO: report and handle error *)
    RETURN
  END;
  
  (* all decisions are based on lookahead *)
  next := lookaheadChar(lexer^.source);
  
  (* skip any whitespace, tab and new line *)
  WHILE NOT eof(lexer^.source) AND
    ((next = SPACE) OR (next = TAB) OR (next = NEWLINE)) DO
    getChar(source, ch, next)
  END; (* WHILE *)
  
  (* TO DO *)
  getLineAndColumn(lexer^.source, sym.line, sym.column);

  (* check for end-of-file *)
  IF eof(lexer^.source) THEN
    sym.token := eof;
    sym.lexeme := 0
  
  (* check for reserved word or identifier *)
  ELSIF isLetter(next) OR (next = "_") OR (next = "$") THEN
    markLexeme(source, sym.line, sym.column);
    matchResWordOrIdent(source, sym.token);
    copyLexeme(lexer^.source, lexer^.dict, sym.lexeme)

  (* check for numeric literal *)
  ELSIF (next >= "0") AND (next <= "9") THEN 
    markLexeme(lexer^.source, sym.line, sym.column);
    matchNumericLiteral(lexer^.source, sym.token);
    copyLexeme(lexer^.source, lexer^.dict, sym.lexeme)

  (* check for quoted literal *)
  ELSIF (next = SINGLEQUOTE) OR (next = DOUBLEQUOTE) THEN
    markLexeme(lexer^.source, sym.line, sym.column);
    matchQuotedLiteral(lexer^.source, sym.token);
    copyLexeme(lexer^.source, lexer^.dict, sym.lexeme)
      
  (* check for any other symbol *)
  ELSE
    CASE next OF
    
    (* next symbol is line comment *)
      "!" :
        markLexeme(lexer^.source, sym.line, sym.column);
        matchLineComment(lexer^.source, sym.token);
        copyLexeme(lexer^.source, dict, sym.lexeme)
    
    (* next symbol is "#" *)
    | "#" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := notEqual;
        sym.lexeme := lexemeForToken(notEqual)
    
    (* next symbol is "(" or block comment *)
    | "(" :
        IF lookahead2(lexer^.source) = "*" THEN (* found block comment *)
          markLexeme(lexer^.source, sym.line, sym.column);
          matchBlockComment(lexer^.source, sym.token);
          copyLexeme(lexer^.source, lexer^.dict, sym.lexeme)
        
        ELSE (* found "(" *)
          consumeChar(lexer^.source);
          getLineAndColumn(lexer^.source, sym.line, sym.column);
          sym.token := lParen;
          sym.lexeme := lexemeForToken(lParen)
          
        END (* "(" and block comment *)
    
    (* next symbol is ")" *)
    | ")" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.value := rParen;
        sym.lexeme := lexemeForToken(rParen)
    
    (* next symbol is "*" or "*." *)
    | "*" :
        getChar(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "." THEN (* found "*" *)
          sym.token := asterisk;
          sym.lexeme := lexemeForToken(asterisk)
        
        ELSE (* found "*." *)
          consumeChar(lexer^.source);
          sym.token := asterDot;
          sym.lexeme := lexemeForToken(asterDot)
        
        END (* "*" or "*." *)
    
    (* next symbol is "+" or "++" *)
    | "+" :
        getChar(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "+" THEN (* found "+" *)
          sym.token := plus;
          sym.lexeme := lexemeForToken(plus)
        
        ELSE (* found "++" *)
          consumeChar(lexer^.source);
          sym.token := plusPlus;
          sym.lexeme := lexemeForToken(plusPlus)
        
        END (* "+" and "++" *)
      
    (* next symbol is "," *)
    | "," :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := comma;
        sym.lexeme := lexemeForToken(comma)
    
    (* next symbol is "-", "--" or "->" *)
    | "-" :
        getChar(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next = "-" THEN (* found "--" *)
          consumeChar(lexer^.source);
          sym.token := minusMinus;
          sym.lexeme := lexemeForToken(minusMinus)
        
        ELSIF next = ">" THEN (* found "->" *)
          consumeChar(lexer^.source);
          sym.token := rArrow;
          sym.lexeme := lexemeForToken(rArrow)
        
        ELSE (* found "-" *)
          sym.token := minus;
          sym.lexeme := lexemeForToken(minus)
        
        END (* "-", "--" or "->" *)
    
    (* next symbol is "." or ".." *)
    | "." :
        getChar(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "." THEN
          sym.token := dot;
          sym.lexeme := lexemeForToken(dotDot)
        
        ELSE (* found ".." *)
          consumeChar(lexer^.source);
          sym.token := dotDot;
          sym.lexeme := lexemeForToken(dotDot)
        
        END (* "." and ".." *)
      
    (* next symbol is "/" *)
    | "/" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := realDiv;
        sym.lexeme := lexemeForToken(realDiv)
    
    (* next symbol is ":", ":=" or "::" *)
    | ":" :
        getChar(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next = "=" THEN (* found ":=" *)
          consumeChar(lexer^.source);
          sym.token := assign;
          sym.lexeme := lexemeForToken(assign)
        
        ELSIF next = ":" THEN (* found "::" *)
          consumeChar(lexer^.source);
          sym.token := typeConv;
          sym.lexeme := lexemeForToken(typeConv)
        
        ELSE (* found ":" *)
          sym.token := colon;
          sym.lexeme := lexemeForToken(colon)
        
        END (* ":", ":=" and "::" *)
    
    (* next symbol is ";" *)
    | ";" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := semicolon;
        sym.lexeme := lexemeForToken(semicolon)
    
    (* next symbol is "<", "<=", chevron text or pragma *)
    | "<" :
        la2 := lookahead2(lexer^.source);
        
        IF la2 = ">" THEN (* found "<<" *)
          markLexeme(lexer^.source, sym.line, sym.column);
          matchChevronText(lexer^.source, sym.token);
          copyLexeme(lexer^.source, lexer^.dict, sym.lexeme)
        
        ELSIF la2 = "*" THEN (* found "<*" *)
          markLexeme(lexer^.source, sym.line, sym.column);
          matchPragma(lexer^.source, sym.token);
          copyLexeme(lexer^.source, lexer^.dict, sym.lexeme)
        
        ELSE (* "<" or "<=" *)
          getChar(lexer^.source, ch, next);
          getLineAndColumn(lexer^.source, sym.line, sym.column);
                  
          IF next = "=" THEN (* found "<=" *)
            consumeChar(source);
            sym.token := lessEq;
            sym.lexeme := lexemeForToken(lessEq)
            
          ELSE (* found "<" *)
            sym.token := less;
            sym.lexeme := lexemeForToken(less)
          
          END (* "<" or "<=" *)
          
        END (* chevron text or pragma *)
    
    (* next symbol is "=" or "==" *)
    | "=" :
        getChar(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "=" THEN (* found "=" *)
          sym.token := equal;
          sym.lexeme := lexemeForToken(equal)
        
        ELSE (* found "==" *)
          consumeChar(lexer^.source);
          sym.token := identity;
          sym.lexeme := lexemeForToken(identity)
        
        END (* "=" or "==" *)
    
    (* next symbol is ">" and ">=" *)
    | ">" :
        getChar(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "=" THEN (* found ">=" *)
          consumeChar(lexer^.source);
          sym.token := greaterEq;
          sym.lexeme := lexemeForToken(greaterEq)
        
        ELSE (* found ">" *)
          sym.token := identity;
          sym.lexeme := lexemeForToken(identity)
        
        END (* "=" or "==" *)
    
    (* next symbol is "?" *)
    | "?" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := qMark;
        sym.lexeme := lexemeForToken(qMark)
    
    (* next symbol is "[" *)
    | "[" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := lBracket;
        sym.lexeme := lexemeForToken(lBracket)
    
    (* next symbol is backslash *)
    | BACKSLASH :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := setDiff;
        sym.lexeme := lexemeForToken(setDiff)
    
    (* next symbol is "]" *)
    | "]" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := rBracket;
        sym.lexeme := lexemeForToken(rBracket)
    
    (* next symbol is "^" *)
    | "^" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := deref;
        sym.lexeme := lexemeForToken(deref)
    
    (* next symbol is "{" *)
    | "{" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := lBrace;
        sym.lexeme := lexemeForToken(lBrace)
    
    (* next symbol is "|" *)
    | "|" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := verticalBar;
        sym.lexeme := lexemeForToken(verticalBar)
    
    (* next symbol is "}" *)
    | "}" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := rBrace;
        sym.lexeme := lexemeForToken(rBrace)
    
    (* next symbol is "~" *)
    | "~" :
        consumeChar(lexer^.source);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        sym.token := tilde;
        sym.lexeme := lexemeForToken(tilde)
    
    (* next symbol is invalid *)
    ELSE
      markLexeme(lexer^.source, sym.line, sym.column);
      consumeChar(lexer^.source);
      sym.token := invalid;
      copyLexeme(lexer^.source, lexer^.dict, sym.lexeme);
      INC(lexer^.errorCount)
      
    END; (* CASE *)
  
  END (* IF *);

  lexer^.nextSymbol := sym;
  
  RETURN
END consumeSym;


(* ---------------------------------------------------------------------------
 * procedure lookaheadSym ( lexer ) : Symbol
 *  returns current lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE lookaheadSym ( lexer : Lexer ) : Symbol; (* PURE *)

BEGIN
  
  RETURN lexer^.nextSymbol
  
END lookaheadSym;


(* ---------------------------------------------------------------------------
 * procedure errorCount ( lexer ) : CARDINAL
 *  returns current lexical error count
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE errorCount ( lexer : Lexer ) : CARDINAL; (* PURE *)
 (* Returns the lexer's accumulated error count. *)

BEGIN
  
  RETURN lexer^.errorCount
  
END errorCount;


(* ---------------------------------------------------------------------------
 * procedure getStatus ( lexer, status )
 *  passes back status of last operation
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE getStatus ( lexer : Lexer; VAR s : Status );

BEGIN

  IF lexer = NIL THEN
    s := notInitialised
  ELSE
    s := lexer^.status
  END

END getStatus;


(* ---------------------------------------------------------------------------
 * procedure release ( lexer )
 *  releases lexer instance
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) lexer must not be NIL
 *
 * post-conditions:
 *  (1) lexer is deallocated
 *  (2) NIL is passed back in lexer
 *
 * error-conditions:
 *  (1) reference to lexer remains unmodified
 * ---------------------------------------------------------------------------
 *)
PROCEDURE release ( VAR lexer : Lexer );

VAR
  sourceStatus : M2Source.Status;
  
BEGIN

  (* lexer must not be NIL *)
  IF lexer = NIL THEN
    RETURN
  END;
  
  (* release source *)
  M2Source.release(lexer^.source, sourceStatus);
  
  (* TO DO: check for and handle error(s) *)
  
  (* release lexer *)
  DISPOSE(lexer);
  lexer := NIL;
  
  RETURN
END release;


(* Private Operations *)

(* ---------------------------------------------------------------------------
 * procedure matchResWordOrIdent ( s, t, diag )
 *  matches the input in s to a reserved word or identifier
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the RW or identifier.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the RW or identifier whose first character was the
 *      lookahead of s upon entry into the procedure.
 *  (2) if the input represents a reserved word or dual-use identifier,
 *       its token value is passed back in token.
 *      if the input represents any other identifier,
 *       token value identifier is passed back in token.
 *
 * error-conditions:
 *  (1) identifier consists entirely of non-alphanumeric characters
 *       TO DO
 *  (2) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchResWordOrIdent
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  allChars, upperChars, nonStdChars : CARDINAL;
  
BEGIN
  
  allChars := 0;
  upperChars := 0;
  nonStdChars := 0;
  
  REPEAT
    getChar(s, ch, next);
    INC(allChars);
    
    IF (ch >= "A") AND (ch <= "Z") THEN
      INC(upperChars)
    END;
    
    IF (ch = "_") OR (ch = "$") THEN
      INC(nonStdChars)
    END

  UNTIL eof(s) OR NOT isIdentChar(next);
  
  IF allChars = upperChars THEN (* possibly reserved word found *)
    (* TO DO : check for reserved word *)
    
  ELSIF allChars = nonStdChars THEN (* illegal identifier found *)
    M2LexDiag.new(diag, illegalIdent, 0, 0, "")
    
  END
  
END matchResWordOrIdent;


(* ---------------------------------------------------------------------------
 * procedure matchNumericLiteral ( s, t, diag )
 *  matches the input in s to numeric literal syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first digit of the literal.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last digit
 *      of the literal whose first digit was the lookahead of s upon entry
 *      into the procedure.
 *  (2) if the numeric literal represents a whole number,
 *       token value wholeNumber is passed back in token.
 *      if the numeric literal represents a character code,
 *       token value quotedChar is passed back in token.
 *      if the numeric literal represents a real number,
 *       token value realNumber is passed back in token.
 *
 * error-conditions:
 *  (1) missing digit after prefix
 *       TO DO
 *  (2) missing fractional part after decimal point
 *       TO DO
 *  (3) missing exponent part after exponent prefix
 *       TO DO
 *  (4) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchNumericLiteral
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

BEGIN
  
  getChar(s, ch, next);
  IF ch = "0" THEN
          
    CASE next OF
      "'" : (* decimal number *)
    | "." : (* real number or range *)
    | "b" : (* base-2 whole number *)
    | "u" : (* base-16 character code *)
    | "x" : (* base-16 whole number *)
    
    ELSE (* single digit zero *)
      (* TO DO *)
    
    END; (* CASE *)
      
  ELSE
  
  END
  (* TO DO *)

END matchNumericLiteral;


(* ---------------------------------------------------------------------------
 * procedure matchQuotedLiteral ( s, t, diag )
 *  matches the input in s to quoted literal syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening quotation mark of the literal.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing
 *      quotation mark that closes the literal whose opening quotation mark
 *      was the lookahead of s upon entry into the procedure.
 *  (2) if the quoted literal represents the empty string or a single
 *      character, token value quotedChar is passed back in token.
 *      Otherwise, token value quotedString is passed back in token.
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) unescaped backslash encountered
 *       TO DO
 *  (4) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchQuotedLiteral
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

VAR
  delimiter, ch, next : CHAR;
  len : CARDINAL;

BEGIN

  (* TO DO : update, following change to M2Source *)
  
  len := 0;
  getChar(s, delimiter, next);
  
  WHILE NOT eof(s) AND (next # delimiter) AND isPrintable(next) DO
    
    IF next # BACKSLASH THEN
      getChar(s, ch, next);
      INC(len)
      
    ELSE (* backslash *)
      matchEscapeSequence(s, success);
      
      IF NOT success THEN (* unescaped backslash found *)
        (* TO DO : handle error *)
        
      END
    END
  END; (* WHILE *)
  
  IF next = delimiter THEN
    consumeChar(s);
    INC(len)
    
  ELSE (* illegal character in string literal found *)
    (* TO DO : handle error *)
    
  END;
  
  IF len <= 1 THEN
    token := quotedChar
    
  ELSE (* len > 1 *)
    token := quotedString
    
  END
  
END matchQuotedLiteral;


(* ---------------------------------------------------------------------------
 * procedure matchLineComment ( s, t, diag )
 *  matches the input in s to line comment syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening exclamation point of a line comment.
 *
 * post-conditions:
 *  (1) if the comment is terminated by end-of-line:
 *       lookahead of s is the new-line character that closes the line comment
 *       whose opening exclamation point was the lookahead of s upon entry
 *       into the procedure, or
 *      if the comment is terminated by end-of-file:
 *       the last character in input s has been consumed.
 *  (2) token value lineComment is passed back in token
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 *  (2) maximum comment length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchLineComment
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  
BEGIN

  REPEAT
    getChar(s, ch, next)
  UNTIL eof(s) OR (next = NEWLINE);
  
  token := lineComment

END matchLineComment;


(* ---------------------------------------------------------------------------
 * procedure matchBlockComment ( s, t, diag )
 *  matches the input in s to block comment syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the opening parenthesis of a block comment.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the closing
 *      parenthesis that closes the block comment whose opening parenthesis
 *      was the lookahead of s upon entry into the procedure.
 *  (2) token value blockComment is passed back in token
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum comment length exceeded
 *       TO DO
 *  (4) maximum nesting level exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchBlockComment
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

VAR
  ch, next : CHAR;
  nestLevel : CARDINAL;
  
BEGIN
  
  nestLevel := 1;
  
  WHILE NOT eof(s) AND (nestLevel > 0) DO
    getChar(s, ch, next);
    
    IF (ch = "*") AND (next = ")") THEN
      consumeChar(s);
      DEC(nestLevel)
    
    ELSIF (ch = "(") AND (next = "*") THEN
      consumeChar(s);
      INC(nestLevel)
      
    END;
    
    consumeChar(s)
    
  END; (* WHILE *)
  
  (* TO DO *)

END matchBlockComment;


(* ---------------------------------------------------------------------------
 * procedure matchChevronText ( s, t, diag )
 *  matches the input in s to chevron text syntax
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the opening chevron.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the closing chevron that closes the chevron text whose
 *      opening delimiter was the lookahead of s upon entry into the procedure.
 *  (2) token value chevronText is passed back in token
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchChevronText
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

BEGIN

  (* TO DO *)

END matchChevronText;


(* ---------------------------------------------------------------------------
 * procedure matchPragma ( s, t, diag )
 *  matches the input in s to "<*" any legal characters "*>"
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is the first character of the opening pragma delimiter.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last
 *      character of the closing delimiter that closes the pragma whose
 *      opening delimiter was the lookahead of s upon entry into the procedure.
 *  (2) token value pragma is passed back in token
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchPragma
  ( s : Source; VAR t : TokenValue; VAR diag : Diagnostic );

BEGIN

  (* TO DO *)

END matchPragma;


END M2Lexer.