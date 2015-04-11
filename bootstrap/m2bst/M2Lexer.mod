(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE M2Lexer;

(* Lexer for Modula-2 R10 Bootstrap Compiler *)

IMPORT ASCII, M2FileIO;

(* Lexer Descriptor *)

TYPE Lexer = POINTER TO LexerDescriptor;

TYPE LexerDescriptor = RECORD
  infile : M2FileIO.M2File;
  (* TO DO *)
END;


(* Return Status 

TYPE Status =
  ( success,
    unableToAllocate,
    illegalSymbolFound,
    unescapedBackslash,
    illegalCharInCharOrString,
    endOfLineInCharOrString,
    lexemeCapacityExceded,
    commentNestingLimitExceded,
    prematureEndOfFile ); *)


(* Operations *)

PROCEDURE new ( VAR lexer : Lexer );
 (* Create mew lexer instance. Passes NIL back if unsuccessful. *)


PROCEDURE nextToken ( lexer : Lexer ) : Token;
 (* Returns next token in input stream, does not consume the token. *)
VAR
  ch : CHAR;
BEGIN
  
  (* ensure lexer is valid *)
  IF lexer = NIL THEN
  (* TO DO: report and handle error *)
    RETURN invalid;
  END;
  
  IF (* token already available *) THEN
    (* return token *)
  END;
  
  (* peek at the next character *)
  M2FileIO.lookahead(lexer^.infile, ch);
  
  (* skip all whitespace, TAB and EOL characters *)
  WHILE NOT M2FileIO.eof(lexer^.infile) AND
   (ch = ASCII.SPACE OR ch = ASCII.TAB OR ch = ASCII.EOL) DO
   
    (* skip the current character *)
    M2FileIO.read(lexer^.infile, ch);
    
    (* peek at the next character *)
    M2FileIO.lookahead(lexer^.infile, ch);

  END; (* WHILE *)
  
  (* remember position at the start of the symbol *)
  M2FileIO.getPos(lexer^.infile, lexer^.line, lexer^.col);
  
  (* check for end-of-file *)
  IF M2FileIO.eof(lexer^.infile) THEN
    thisToken := eof
  
  (* check for reserved word or identifier *)
  ELSIF isLetter(ch) OR ch = "_" OR ch = "$" THEN
  
  (* check for numeric literal *)
  ELSIF isDigit(ch) THEN 
  
  (* check for quoted literal *)
  ELSIF ch = "'" OR ch = "'" THEN
  
  (* check for any other token *)
  ELSE
    CASE ch OF
    
    (* check for . and .. *)
      "." :
      M2FileIO.read(lexer^.infile, ch);
      M2FileIO.lookahead(lexer^.infile, ch);
      IF ch # "." THEN
        thisToken = dot
      ELSE (* found ".." *)
        thisToken = dotDot
      END
      
    (* check for , *)
    | "," :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = comma
    
    (* check for : and :: *)
    | ":" :
      M2FileIO.read(lexer^.infile, ch);
      M2FileIO.lookahead(lexer^.infile, ch);
      IF ch # ":" THEN
        thisToken = colon
      ELSE (* found "::" *)
        thisToken = typeConv
      END
    
    (* check for ; *)
    | ";" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = semicolon
    
    (* check for | *)
    | "|" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = verticalBar
    
    (* check for ^ *)
    | "^" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = deref
    
    (* check for ? *)
    | "?" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = qMark
    
    (* check for ( and block comment *)
    | "(" :
    
    (* check for ) *)
    | ")" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = rParen
    
    (* check for [ *)
    | "[" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = lBracket
    
    (* check for ] *)
    | "]" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = rBracket
    
    (* check for { *)
    | "{" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = lBrace
    
    (* check for } *)
    | "}" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = rBrace
    
    (* check for + and ++ *)
    | "+" :
      M2FileIO.read(lexer^.infile, ch);
      M2FileIO.lookahead(lexer^.infile, ch);
      IF ch # "+" THEN
        thisToken = plus
      ELSE (* found "++" *)
        thisToken = plusPlus
      END
    
    (* check for -, -- and -> *)
    | "-" :
      M2FileIO.read(lexer^.infile, ch);
      M2FileIO.lookahead(lexer^.infile, ch);
      IF ch # "-" THEN
        thisToken = minus
      ELSE (* found "--" *)
        thisToken = minusMinus
      END
    
    (* check for * and *. *)
    | "*" :
      M2FileIO.read(lexer^.infile, ch);
      M2FileIO.lookahead(lexer^.infile, ch);
      IF ch # "." THEN
        thisToken = asterisk
      ELSE (* found "*." *)
        thisToken = asterDot
      END
    
    (* check for / *)
    | "/" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = realDiv
    
    (* check for \ *)
    | BACKSLASH :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = setDiff
    
    (* check for ~ *)
    | "~" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = arrayCat
    
    (* check for =, == and => *)
    | "=" :
    
    (* check for # *)
    | "#" :
      M2FileIO.read(lexer^.infile, ch);
      thisToken = notEqual
    
    (* check for <, <= and pragma *)
    | "<" :
    
    (* check for > and >= *)
    | ">" :
      M2FileIO.read(lexer^.infile, ch);
      M2FileIO.lookahead(lexer^.infile, ch);
      IF ch # "=" THEN
        thisToken = equal
      ELSE (* found ">=" *)
        thisToken = greaterOrEq
      END
    
    (* check for line comment *)
    | "!" :
      (* TO DO: handle line comment *)
    
    (* any other symbol is illegal *)
    ELSE
      thisToken = invalid
    END; (* CASE *)
  
  END (* IF *);
  
  RETURN thisToken  
END nextToken;


PROCEDURE consumeToken ( lexer : Lexer; VAR lexeme : ARRAY OF CHAR );
 (* Consumes the next token and passes its lexeme back. *)
BEGIN
  (* TO DO *)
END consumeToken;


PROCEDURE getStatus ( lexer : Lexer; VAR s : Status; VAR line, col : CARDINAL );
 (* Passes status, line and column of current token back. *)
BEGIN
  (* TO DO *)
END getStatus;


PROCEDURE release ( VAR lexer : Lexer );
 (* Release lexer instance. Passes NIL back if successful. *)
BEGIN
  (* TO DO *)
END release;


END M2Lexer.