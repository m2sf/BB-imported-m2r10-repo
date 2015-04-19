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
  
  IF lexer^.uncomsumed THEN
    lexer^.unconsumed := FALSE
  END
  
END consumeToken;


PROCEDURE getStatus ( lexer : Lexer; VAR s : Status; VAR line, col : CARDINAL );
 (* Passes status, line and column of current token back. *)

BEGIN

  (* TO DO *)

END getStatus;


PROCEDURE release ( VAR lexer : Lexer );
 (* Release lexer instance.  Passes NIL back if successful. *)

BEGIN

  (* TO DO *)
  
END release;


(* Private Operations *)

PROCEDURE matchResWordOrIdent
  ( s : Source; VAR token : Token; VAR lexeme : Lexeme );

BEGIN

  (* TO DO *)

END matchResWordOrIdent;


PROCEDURE matchNumericLiteral
  ( s : Source; VAR token : Token; VAR lexeme : Lexeme );

BEGIN

  (* TO DO *)

END matchNumericLiteral;


PROCEDURE matchQuotedLiteral
  ( s : Source; VAR token : Token; VAR lexeme : Lexeme );

VAR
  ch, delimiter : CHAR;
  len : CARDINAL;

BEGIN

  (* TO DO : update, following change to M2Source *)
  
  len := 0;
  M2FileIO.read(lexer^.infile, delimiter);
  M2FileIO.lookahead(lexer^.infile, ch);
  
  WHILE NOT M2FileIO.eof(lexer^.infile) AND (ch # delimiter) DO
    
    len := len + 1
  END; (* WHILE *)

  M2FileIO.read(lexer^.infile, delimiter);
  
  IF len <= 1 THEN
    RETURN quotedChar
  ELSE (* len > 1 *)
    RETURN quotedString
  END
  
END matchQuotedLiteral;


PROCEDURE matchLineComment
  ( s : Source; VAR token : Token; VAR lexeme : Lexeme );

BEGIN

  (* TO DO *)

END matchLineComment;


PROCEDURE matchBlockComment
  ( s : Source; VAR token : Token; VAR lexeme : Lexeme );

BEGIN

  (* TO DO *)

END matchBlockComment;


PROCEDURE matchChevronText
  ( s : Source; VAR token : Token; VAR lexeme : Lexeme );

BEGIN

  (* TO DO *)

END matchChevronText;


PROCEDURE matchPragma
  ( s : Source; VAR token : Token; VAR lexeme : Lexeme );

BEGIN

  (* TO DO *)

END matchPragma;


END M2Lexer.