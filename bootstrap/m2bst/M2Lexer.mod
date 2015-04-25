(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE M2Lexer;

(* Lexer for Modula-2 R10 Bootstrap Compiler *)

IMPORT ASCII, M2Params;

FROM M2Tokens IMPORT Token, TokenValue;

FROM M2Source IMPORT Source, open, read, consume, lookahead, lookahead2,
  markLexeme, copyLexeme, getLineAndColumn, eof, close;


(* Lexer Descriptor *)

TYPE Lexer = POINTER TO LexerDescriptor;

TYPE LexerDescriptor = RECORD
  source : Source;
  status : Status;
  nextSymbol : Symbol;
  errorCount : CARDINAL;
END;


(* Operations *)

PROCEDURE new ( VAR lexer : Lexer; filename : Filename; VAR s : Status );
 (* Create newly allocated and initialised lexer instance associated with
    source file filename.  Passes back the status of the operation in s. *)
 
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
  
  open(lexer^.source, filename, sourceStatus);
  IF sourceStatus # M2Source.success THEN
  (* TO DO: status *)
    RETURN
  END;
  
  (* get the first symbol to be returned *)
  readSymbol(source, lexer^.nextSymbol);
  
  errorCount := 0;
  status := success;
  RETURN
  
END new;


PROCEDURE getSym ( lexer : Lexer; VAR sym, next : Symbol );
(* Passes back the current lookahead symbol in current and consumes it.
   Passes back the new lookahead symbol in next without consuming it. *)

BEGIN

  sym := lexer^.nextSymbol;
  readSymbol(source, next);
  lexer^.nextSymbol := next;

END getSym;


PROCEDURE consumeSym ( lexer : Lexer );
(* Consumes the current lookahead symbol. *)

BEGIN

  readSymbol(source, next);
  lexer^.nextSymbol := next;

END consumeSym;


PROCEDURE lookaheadSym ( lexer : Lexer ) : Symbol;
(* Returns the current lookahead symbol without consuming it. *)

BEGIN
  
  RETURN lexer^.nextSymbol
  
END lookaheadSym;


PROCEDURE errorCount ( lexer : Lexer ) : CARDINAL;
 (* Returns the lexer's accumulated error count. *)

BEGIN
  
  RETURN lexer^.errorCount
  
END errorCount;


PROCEDURE getStatus ( lexer : Lexer; VAR s : Status );
 (* Passes back status of last operation in s. *)

BEGIN

  IF lexer = NIL THEN
    s := notInitialised
  ELSE
    s := lexer^.status
  END

END getStatus;


PROCEDURE release ( VAR lexer : Lexer );
 (* Release lexer instance. Passes back NIL in lexer if successful. *)

BEGIN

  (* TO DO *)
  
END release;


(* Private Operations *)

(* ---------------------------------------------------------------------------
 * procedure readSymbol ( source, symbol )
 *  reads the next available symbol from source
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is at start of source or symbol or ignore character.
 *
 * post-conditions:
 *  (1) symbol is passed back in symbol.
 *  (2) lookahead of s is the character immediately following the last
 *      character of the symbol passed back.
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE readSymbol ( s : Source; VAR sym : Symbol );
(* Passes back the current lookahead symbol in current and consumes it.
   Passes the new lookahead symbol in next without consuming it. *)

VAR
  ch, next, la2 : CHAR;

(* TO DO : work out how to reference/pass lexeme dictionary *)

BEGIN
  (* ensure source is valid *)
  IF source = NIL THEN
  (* TO DO: report and handle error *)
    RETURN invalid;
  END;
  
  (* all decisions are based on lookahead *)
  next := lookahead(source);
  
  (* skip any whitespace, tab and new line *)
  WHILE NOT eof(source) AND isIgnoreChar(next) DO
    read(source, ch, next)
  END; (* WHILE *)
  
  (* TO DO *)
  getLineAndColumn(source, sym.line, sym.column);

  (* check for end-of-file *)
  IF eof(source) THEN
    sym.token := eof;
    sym.lexeme := 0
  
  (* check for reserved word or identifier *)
  ELSIF isLetter(next) OR (next = "_") OR (next = "$") THEN
    markLexeme(source, sym.line, sym.column);
    matchResWordOrIdent(source, sym.token);
    copyLexeme(source, lexer^.dict, sym.lexeme)

  (* check for numeric literal *)
  ELSIF isDigit(next) THEN 
    markLexeme(source, sym.line, sym.column);
    matchNumericLiteral(source, sym.token);
    copyLexeme(source, lexer^.dict, sym.lexeme)

  (* check for quoted literal *)
  ELSIF (next = SingleQuote) OR (next = DoubleQuote) THEN
    markLexeme(source, sym.line, sym.column);
    matchQuotedLiteral(source, sym.token);
    copyLexeme(source, lexer^.dict, sym.lexeme)
      
  (* check for any other symbol *)
  ELSE
    CASE next OF
    
    (* next symbol is line comment *)
    | "!" :
        markLexeme(source, sym.line, sym.column);
        matchLineComment(source, sym.token);
        copyLexeme(source, dict, sym.lexeme)
    
    (* next symbol is "#" *)
    | "#" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := notEqual;
        sym.lexeme := lexemeForToken(notEqual)
    
    (* next symbol is "(" or block comment *)
    | "(" :
        IF lookahead2(source) = "*" THEN (* found block comment *)
          markLexeme(source, sym.line, sym.column);
          matchBlockComment(source, sym.token);
          copyLexeme(source, dict, sym.lexeme)
        
        ELSE (* found "(" *)
          consume(source);
          getLineAndColumn(source, sym.line, sym.column);
          sym.token := lParen;
          sym.lexeme := lexemeForToken(lParen)
          
        END (* "(" and block comment *)
    
    (* next symbol is ")" *)
    | ")" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.value := rParen;
        sym.lexeme := lexemeForToken(rParen)
    
    (* next symbol is "*" or "*." *)
    | "*" :
        read(source, ch, next);
        getLineAndColumn(source, sym.line, sym.column);
        
        IF next # "." THEN (* found "*" *)
          sym.token := asterisk;
          sym.lexeme := lexemeForToken(asterisk)
        
        ELSE (* found "*." *)
          consume(source);
          sym.token := asterDot;
          sym.lexeme := lexemeForToken(asterDot)
        
        END (* "*" or "*." *)
    
    (* next symbol is "+" or "++" *)
    | "+" :
        read(lexer^.source, ch, next);
        getLineAndColumn(source, sym.line, sym.column);
        
        IF next # "+" THEN (* found "+" *)
          sym.token := plus;
          sym.lexeme := lexemeForToken(plus)
        
        ELSE (* found "++" *)
          consume(source);
          sym.token := plusPlus;
          sym.lexeme := lexemeForToken(plusPlus)
        
        END (* "+" and "++" *)
      
    (* next symbol is "," *)
    | "," :
        consume(lexer^.source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := comma;
        sym.lexeme := lexemeForToken(comma)
    
    (* next symbol is "-", "--" or "->" *)
    | "-" :
        read(lexer^.source, ch, next);
        getLineAndColumn(source, sym.line, sym.column);
        
        IF next = "-" THEN (* found "--" *)
          consume(lexer^.source);
          sym.token := minusMinus;
          sym.lexeme := lexemeForToken(minusMinus)
        
        ELSIF next = ">" THEN (* found "->" *)
          consume(source);
          sym.token := rArrow;
          sym.lexeme := lexemeForToken(rArrow)
        
        ELSE (* found "-" *)
          sym.token := minus;
          sym.lexeme := lexemeForToken(minus)
        
        END (* "-", "--" or "->" *)
    
    (* next symbol is "." or ".." *)
      "." :
        read(source, ch, next);
        getLineAndColumn(source, sym.line, sym.column);
        
        IF next # "." THEN
          sym.token := dot;
          sym.lexeme := lexemeForToken(dotDot)
        
        ELSE (* found ".." *)
          consume(source);
          sym.token := dotDot;
          sym.lexeme := lexemeForToken(dotDot)
        
        END (* "." and ".." *)
      
    (* next symbol is "/" *)
    | "/" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := realDiv;
        sym.lexeme := lexemeForToken(realDiv)
    
    (* next symbol is ":", ":=" or "::" *)
    | ":" :
        read(source, ch, next);
        getLineAndColumn(source, sym.line, sym.column);
        
        IF next = "=" THEN (* found ":=" *)
          consume(source);
          sym.token := assign;
          sym.lexeme := lexemeForToken(assign)
        
        ELSIF next = ":" THEN (* found "::" *)
          consume(source);
          sym.token := typeConv;
          sym.lexeme := lexemeForToken(typeConv)
        
        ELSE (* found ":" *)
          sym.token := colon;
          sym.lexeme := lexemeForToken(colon)
        
        END (* ":", ":=" and "::" *)
    
    (* next symbol is ";" *)
    | ";" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := semicolon;
        sym.lexeme := lexemeForToken(semicolon)
    
    (* next symbol is "<", "<=", chevron text or pragma *)
    | "<" :
        la2 := lookahead2(source);
        
        IF la2 = ">" THEN (* found "<<" *)
          markLexeme(source, sym.line, sym.column);
          matchChevronText(source, sym.token);
          copyLexeme(source, dict, sym.lexeme)
        
        ELSIF la2 = "*" THEN (* found "<*" *)
          markLexeme(source, sym.line, sym.column);
          matchPragma(source, sym.token);
          copyLexeme(source, dict, sym.lexeme)
        
        ELSE (* "<" or "<=" *)
          read(source, ch, next);
          getLineAndColumn(source, sym.line, sym.column);
                  
          IF next = "=" THEN (* found "<=" *)
            consume(source);
            sym.token := lessEq;
            sym.lexeme := lexemeForToken(lessEq)
            
          ELSE (* found "<" *)
            sym.token := less;
            sym.lexeme := lexemeForToken(less)
          
          END (* "<" or "<=" *)
          
        END (* chevron text or pragma *)
    
    (* next symbol is "=" or "==" *)
    | "=" :
        read(source, ch, next);
        getLineAndColumn(source, sym.line, sym.column);
        
        IF next # "=" THEN (* found "=" *)
          sym.token := equal;
          sym.lexeme := lexemeForToken(equal)
        
        ELSE (* found "==" *)
          consume(source);
          sym.token := identity;
          sym.lexeme := lexemeForToken(identity)
        
        END (* "=" or "==" *)
    
    (* next symbol is ">" and ">=" *)
    | ">" :
        read(source, ch, next);
        getLineAndColumn(lexer^.source, sym.line, sym.column);
        
        IF next # "=" THEN (* found ">=" *)
          consume(source);
          sym.token := greaterEq;
          sym.lexeme := lexemeForToken(greaterEq)
        
        ELSE (* found ">" *)
          sym.token := identity;
          sym.lexeme := lexemeForToken(identity)
        
        END (* "=" or "==" *)
    
    (* next symbol is "?" *)
    | "?" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := qMark;
        sym.lexeme := lexemeForToken(qMark)
    
    (* next symbol is "[" *)
    | "[" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := lBracket;
        sym.lexeme := lexemeForToken(lBracket)
    
    (* next symbol is backslash *)
    | BACKSLASH :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := setDiff;
        sym.lexeme := lexemeForToken(setDiff)
    
    (* next symbol is "]" *)
    | "]" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := rBracket;
        sym.lexeme := lexemeForToken(rBracket)
    
    (* next symbol is "^" *)
    | "^" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := deref;
        sym.lexeme := lexemeForToken(deref)
    
    (* next symbol is "{" *)
    | "{" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := lBrace;
        sym.lexeme := lexemeForToken(lBrace)
    
    (* next symbol is "|" *)
    | "|" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := verticalBar;
        sym.lexeme := lexemeForToken(verticalBar)
    
    (* next symbol is "}" *)
    | "}" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := rBrace;
        sym.lexeme := lexemeForToken(rBrace)
    
    (* next symbol is "~" *)
    | "~" :
        consume(source);
        getLineAndColumn(source, sym.line, sym.column);
        sym.token := tilde;
        sym.lexeme := lexemeForToken(tilde)
    
    (* next symbol is invalid *)
    ELSE
      markLexeme(source, sym.line, sym.column);
      consume(source);
      sym.token := invalid;
      copyLexeme(source, lexer^.dict, sym.lexeme)
      
    END; (* CASE *)
  
  END (* IF *);
    
  RETURN
END readSymbol;


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
    read(s, ch, next);
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
  
  read(s, ch, next);
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
  read(s, delimiter, next);
  
  WHILE NOT eof(s) AND (next # delimiter) AND isPrintable(next) DO
    
    IF next # BACKSLASH THEN
      read(s, ch, next);
      INC(len)
      
    ELSE (* backslash *)
      matchEscapeSequence(s, success);
      
      IF NOT success THEN (* unescaped backslash found *)
        (* TO DO : handle error *)
        
      END
    END
  END; (* WHILE *)
  
  IF next = delimiter THEN
    consume(s);
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
    read(s, ch, next)
  UNTIL eof(s) OR (next = ASCII.LF);
  
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
    read(s, ch, next);
    
    IF (ch = "*") AND (next = ")") THEN
      consume(s);
      DEC(nestLevel)
    
    ELSIF (ch = "(") AND (next = "*") THEN
      consume(s);
      INC(nestLevel)
      
    END;
    
    consume(s)
    
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