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
  unconsumed : BOOLEAN;
  token : Token;
END;


(* Return Status *)

TYPE Status =
  ( success,
    unableToAllocate,
    illegalSymbolFound,
    unescapedBackslash,
    illegalCharInCharOrString,
    endOfLineInCharOrString,
    lexemeCapacityExceded,
    commentNestingLimitExceded,
    prematureEndOfFile );


(* Operations *)

PROCEDURE new ( VAR lexer : Lexer );
 (* Create mew lexer instance.  Passes back NIL if unsuccessful. *)
 
 BEGIN
 
   (* TO DO *)
 
 END new;


PROCEDURE nextToken ( lexer : Lexer ) : Token;
 (* Returns next token in input stream, does not consume the token. *)

VAR
  ch, next, la2 : CHAR;
  token : Token;
  
BEGIN
  (* ensure lexer is valid *)
  IF lexer = NIL THEN
  (* TO DO: report and handle error *)
    RETURN invalid;
  END;
  
  IF lexer^.unconsumed THEN
    RETURN lexer^.token
  END;
  
  (* all decisions are based on lookahead *)
  next := lookahead(lexer^.source);
  
  (* skip any whitespace, tab and new line *)
  WHILE NOT eof(lexer^.source) AND isIgnoreChar(next) DO
    read(lexer^.source, ch, next)
  END; (* WHILE *)
  
  (* TO DO *)
  getLineAndColumn(lexer^.source, token.line, token.column);

  (* check for end-of-file *)
  IF eof(lexer^.source) THEN
    token.value := eof;
    token.lexeme := 0
  
  (* check for reserved word or identifier *)
  ELSIF isLetter(next) OR (next = "_") OR (next = "$") THEN
    markLexeme(lexer^.source, token.line, token.column);
    matchResWordOrIdent(lexer^.source, token.value);
    copyLexeme(lexer^.source, lexer^.dict, token.lexeme)

  (* check for numeric literal *)
  ELSIF isDigit(next) THEN 
    markLexeme(lexer^.source, token.line, token.column);
    matchNumericLiteral(lexer^.source, token.value);
    copyLexeme(lexer^.source, lexer^.dict, token.lexeme)

  (* check for quoted literal *)
  ELSIF (next = SingleQuote) OR (next = DoubleQuote) THEN
    markLexeme(lexer^.source, token.line, token.column);
    matchQuotedLiteral(lexer^.source, token.value);
    copyLexeme(lexer^.source, lexer^.dict, token.lexeme)
      
  (* check for any other symbol *)
  ELSE
    CASE next OF
    
    (* next symbol is line comment *)
    | "!" :
        markLexeme(source, token.line, token.column);
        matchLineComment(source, token.value);
        copyLexeme(source, dict, token.lexeme)
    
    (* next symbol is "#" *)
    | "#" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := notEqual;
        token.lexeme := lexemeForToken(notEqual)
    
    (* next symbol is "(" or block comment *)
    | "(" :
        IF lookahead2(lexer^.source) = "*" THEN (* found block comment *)
          markLexeme(source, token.line, token.column);
          matchBlockComment(source, token.value);
          copyLexeme(source, dict, token.lexeme)
        
        ELSE (* found "(" *)
          consume(lexer^.source);
          getLineAndColumn(source, token.line, token.column);
          token.value := lParen;
          token.lexeme := lexemeForToken(lParen)
          
        END (* "(" and block comment *)
    
    (* next symbol is ")" *)
    | ")" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := rParen;
        token.lexeme := lexemeForToken(rParen)
    
    (* next symbol is "*" or "*." *)
    | "*" :
        read(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, token.line, token.column);
        
        IF next # "." THEN (* found "*" *)
          token.value := asterisk;
          token.lexeme := lexemeForToken(asterisk)
        
        ELSE (* found "*." *)
          consume(lexer^.source);
          token.value := asterDot;
          token.lexeme := lexemeForToken(asterDot)
        
        END (* "*" or "*." *)
    
    (* next symbol is "+" or "++" *)
    | "+" :
        read(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, token.line, token.column);
        
        IF next # "+" THEN (* found "+" *)
          token.value := plus;
          token.lexeme := lexemeForToken(plus)
        
        ELSE (* found "++" *)
          consume(lexer^.source);
          token.value := plusPlus;
          token.lexeme := lexemeForToken(plusPlus)
        
        END (* "+" and "++" *)
      
    (* next symbol is "," *)
    | "," :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := comma;
        token.lexeme := lexemeForToken(comma)
    
    (* next symbol is "-", "--" or "->" *)
    | "-" :
        read(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, token.line, token.column);
        
        IF next = "-" THEN (* found "--" *)
          consume(lexer^.source);
          token.value := minusMinus;
          token.lexeme := lexemeForToken(minusMinus)
        
        ELSIF next = ">" THEN (* found "->" *)
          consume(lexer^.source);
          token.value := rArrow;
          token.lexeme := lexemeForToken(rArrow)
        
        ELSE (* found "-" *)
          token.value := minus;
          token.lexeme := lexemeForToken(minus)
        
        END (* "-", "--" or "->" *)
    
    (* next symbol is "." or ".." *)
      "." :
        read(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, token.line, token.column);
        
        IF next # "." THEN
          token.value := dot;
          token.lexeme := lexemeForToken(dotDot)
        
        ELSE (* found ".." *)
          consume(lexer^.source);
          token.value := dotDot;
          token.lexeme := lexemeForToken(dotDot)
        
        END (* "." and ".." *)
      
    (* next symbol is "/" *)
    | "/" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := realDiv;
        token.lexeme := lexemeForToken(realDiv)
    
    (* next symbol is ":", ":=" or "::" *)
    | ":" :
        read(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, token.line, token.column);
        
        IF next = "=" THEN (* found ":=" *)
          consume(lexer^.source);
          token.value := assign;
          token.lexeme := lexemeForToken(assign)
        
        ELSIF next = ":" THEN (* found "::" *)
          consume(lexer^.source);
          token.value := typeConv;
          token.lexeme := lexemeForToken(typeConv)
        
        ELSE (* found ":" *)
          token.value := colon;
          token.lexeme := lexemeForToken(colon)
        
        END (* ":", ":=" and "::" *)
    
    (* next symbol is ";" *)
    | ";" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := semicolon;
        token.lexeme := lexemeForToken(semicolon)
    
    (* next symbol is "<", "<=", chevron text or pragma *)
    | "<" :
        la2 := lookahead2(lexer^.source);
        
        IF la2 = ">" THEN (* found "<<" *)
          markLexeme(source, token.line, token.column);
          matchChevronText(source, token.value);
          copyLexeme(source, dict, token.lexeme)
        
        ELSIF la2 = "*" THEN (* found "<*" *)
          markLexeme(source, token.line, token.column);
          matchPragma(source, token.value);
          copyLexeme(source, dict, token.lexeme)
        
        ELSE (* "<" or "<=" *)
          read(lexer^.source, ch, next);
          getLineAndColumn(lexer^.source, token.line, token.column);
                  
          IF next = "=" THEN (* found "<=" *)
            consume(lexer^.source);
            token.value := lessEq;
            token.lexeme := lexemeForToken(lessEq)
            
          ELSE (* found "<" *)
            token.value := less;
            token.lexeme := lexemeForToken(less)
          
          END (* "<" or "<=" *)
          
        END (* chevron text or pragma *)
    
    (* next symbol is "=" or "==" *)
    | "=" :
        read(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, token.line, token.column);
        
        IF next # "=" THEN (* found "=" *)
          token.value := equal;
          token.lexeme := lexemeForToken(equal)
        
        ELSE (* found "==" *)
          consume(lexer^.source);
          token.value := identity;
          token.lexeme := lexemeForToken(identity)
        
        END (* "=" or "==" *)
    
    (* next symbol is ">" and ">=" *)
    | ">" :
        read(lexer^.source, ch, next);
        getLineAndColumn(lexer^.source, token.line, token.column);
        
        IF next # "=" THEN (* found ">=" *)
          consume(lexer^.source);
          token.value := greaterEq;
          token.lexeme := lexemeForToken(greaterEq)
        
        ELSE (* found ">" *)
          token.value := identity;
          token.lexeme := lexemeForToken(identity)
        
        END (* "=" or "==" *)
    
    (* next symbol is "?" *)
    | "?" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := qMark;
        token.lexeme := lexemeForToken(qMark)
    
    (* next symbol is "[" *)
    | "[" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := lBracket;
        token.lexeme := lexemeForToken(lBracket)
    
    (* next symbol is backslash *)
    | BACKSLASH :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := setDiff;
        token.lexeme := lexemeForToken(setDiff)
    
    (* next symbol is "]" *)
    | "]" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := rBracket;
        token.lexeme := lexemeForToken(rBracket)
    
    (* next symbol is "^" *)
    | "^" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := deref;
        token.lexeme := lexemeForToken(deref)
    
    (* next symbol is "{" *)
    | "{" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := lBrace;
        token.lexeme := lexemeForToken(lBrace)
    
    (* next symbol is "|" *)
    | "|" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := verticalBar;
        token.lexeme := lexemeForToken(verticalBar)
    
    (* next symbol is "}" *)
    | "}" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := rBrace;
        token.lexeme := lexemeForToken(rBrace)
    
    (* next symbol is "~" *)
    | "~" :
        consume(lexer^.source);
        getLineAndColumn(lexer^.source, token.line, token.column);
        token.value := tilde;
        token.lexeme := lexemeForToken(tilde)
    
    (* next symbol is invalid *)
    ELSE
      markLexeme(source, token.line, token.column);
      consume(lexer^.source);
      token.value := invalid;
      copyLexeme(source, lexer^.dict, token.lexeme)
      
    END; (* CASE *)
  
  END (* IF *);
  
  (* store lookahead *)
  lexer^.token := token;
  lexer^.unconsumed := TRUE;
  
  RETURN token
END nextToken;


PROCEDURE consumeToken ( lexer : Lexer );
 (* Consumes the next token. *)
BEGIN
  
  IF lexer^.unconsumed THEN
    lexer^.unconsumed := FALSE
  END
  
END consumeToken;


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
 (* Release lexer instance.  Passes back NIL if successful. *)

BEGIN

  (* TO DO *)
  
END release;


(* Private Operations *)

(* ---------------------------------------------------------------------------
 * procedure matchResWordOrIdent ( s, token )
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
PROCEDURE matchResWordOrIdent ( s : Source; VAR token : Token );

VAR
  ch, next : CHAR;
  allChars, nonStdChars : CARDINAL;
  
BEGIN
  
  allChars := 0;
  nonStdChars := 0;
  
  REPEAT
    read(s, ch, next);
    INC(allChars);
    
    IF (ch = "_") OR (ch = "$") THEN
      INC(nonStdChars)
    END

  UNTIL eof(s) OR NOT isIdentChar(next);
  
  IF allChars = nonStdChars THEN (* illegal identifier found *)
    (* TO DO : handle error *)
  END;
  
  (* TO DO : check for reserved word *)
  
END matchResWordOrIdent;


(* ---------------------------------------------------------------------------
 * procedure matchNumericLiteral ( s, token )
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
PROCEDURE matchNumericLiteral ( s : Source; VAR token : Token );

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
    
    END; (* CASE *)
      
  ELSE
  
  END
  (* TO DO *)

END matchNumericLiteral;


(* ---------------------------------------------------------------------------
 * procedure matchQuotedLiteral ( s, token )
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
PROCEDURE matchQuotedLiteral ( s : Source; VAR token : Token );

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
 * procedure matchLineComment ( s, token )
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
PROCEDURE matchLineComment ( s : Source; VAR token : Token );

VAR
  ch, next : CHAR;
  
BEGIN

  REPEAT
    read(s, ch, next)
  UNTIL eof(s) OR (next = ASCII.LF);
  
  token := lineComment

END matchLineComment;


(* ---------------------------------------------------------------------------
 * procedure matchBlockComment ( s, token )
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
PROCEDURE matchBlockComment ( s : Source; VAR token : Token );

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
 * procedure matchChevronText ( s, token )
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
PROCEDURE matchChevronText ( s : Source; VAR token : Token );

BEGIN

  (* TO DO *)

END matchChevronText;


(* ---------------------------------------------------------------------------
 * procedure matchPragma ( s, token )
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
PROCEDURE matchPragma ( s : Source; VAR token : Token );

BEGIN

  (* TO DO *)

END matchPragma;


END M2Lexer.