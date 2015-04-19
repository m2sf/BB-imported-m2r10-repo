(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE M2Source;

(* Modula-2 Source File Reader. *)

FROM M2Filenames IMPORT Filename;

FROM M2LexTab IMPORT LexDict, DictHandle;


TYPE Source (* = OPAQUE *);

TYPE Status =
  ( success,
    invalidFileType,
    maxFileSizeExceeded,
    allocationFailed );

(* ---------------------------------------------------------------------------
 *  Definitions
 *
 *  start position :
 *    the position of the first character in the source.
 *
 *  end position :
 *    the position of the last character in the source.
 *
 *  current position :
 *    the position of the character to be read next.
 *
 *  lookahead position :
 *    the position immediately following the current position.
 *
 *  second lookahead position :
 *    the position immediately following the lookahead position.
 *
 *  marked position :
 *    a position recorded as the start of a lexeme.
 *
 *  current character :
 *    the character at the current position,
 *    it is ASCII.NUL if eof is set.
 *
 *  lookahead character :
 *    the character at the lookahead position,
 *    it is ASCII.NUL if its position > end position or if eof is set.
 *
 *  second lookahead character :
 *    the character at the second lookahead position,
 *    it is ASCII.NUL if its position > end position or if eof is set.
 *
 *  marked lexeme :
 *    a character sequence that starts at the marked position
 *    and ends at the current position, including the current position.
 *
 *  character consumption :
 *    a character is consumed by advancing the current position
 *    to the character's lookahead position or by setting eof.
 *
 *  end-of-line marker:
 *    an ASCII.LF,
 *    or a sequence consisting of an ASCII.CR followed by an ASCII.LF,
 *    or a sole ASCII.CR that is not immediately followed by ASCII.LF.
 *
 *    The lookahead position of an end-of-line marker is the position
 *    following the last character of the end-of-line marker.
 *
 *  end-of-file flag:
 *    abbreviated as eof flag, is a boolean value that is set when
 *    the character at the end position has been consumed.
 *
 * ---------------------------------------------------------------------------
 *)


PROCEDURE open
  ( VAR s : Source; name : Filename; VAR status : Status );
(* Passes back a newly allocated source instance associated with name in s.
   The associated file is opened for reading and the current position is set
   to the start position.  Passes back NIL in s if unsuccessful.
   The status of the operation is passed back in status. *)

VAR
  source : Source;
  
BEGIN
  
  IF s # NIL THEN
    status := allocTargetNotNil
    
  ELSE
    NEW(source);
    FileIO.open(source^.file, name);
    FileIO.ReadNBytes(source^.file, s^.buffer, bytesRead);
    
    (* TO DO : check for errors and handle them *)
    
    source^.index := 0;
    source^.maxIndex := bytesRead-1;
    
    source^.line := 1;
    source^.col := 1;
  
    status := success;
    s := source
  END
  
END open;


PROCEDURE read ( s : Source; VAR current, next : CHAR );
(* Passes back the current character in current and consumes it.  Passes the
   lookahead character in next.  Does not consume the lookahead character. *)

BEGIN
  
  IF s^.eof THEN
    ch := ASCII.NUL;
    next := ASCII.NUL;
    RETURN
    
  END;
  
  IF source^.index = M2Params.MaxInFileSize THEN
    
  END;
  
  thisChar := source^.buffer[source^.index];
  
  (* check for line feed *)
  IF thisChar = ASCII.LF THEN
    ch := thisChar;
    INC(source^.index);
    INC(source^.line);
    source^.column := 1
  
  (* check for carriage return *)
  ELSIF thisChar = ASCII.CR THEN
    ch := ASCII.LF;
    INC(source^.index);
    INC(source^.line);
    source^.column := 1;
    
    (* skip LF immediately following CR *)
    IF source^.buffer[source^.index] = ASCII.LF THEN
      INC(source^.index)
    END
  
  (* any other characters *)
  ELSE
    ch := thisChar;
    INC(source^.index);
    INC(source^.column)
    
  END;
  
  RETURN
END readChar;
  

PROCEDURE consume ( s : source );
(* Consumes the current character of s. *)

BEGIN

(* TO DO *)

END consume;


PROCEDURE currChar ( s : Source ) : CHAR;
(* Returns the current character of s and consumes it. *)

VAR
  ch : CHAR;

BEGIN
  (* check if current character lies beyond eof *)  
  IF s^.eof OR (s^.index > MaxIndex) THEN
    ch := ASCII.NUL
    
  ELSE (* current character lies within buffer *)
    ch := s^.buffer[s^.index];
    
    (* translate ASCII.CR to ASCII.LF *)
    IF ch = ASCII.CR THEN
      ch := ASCII.LF;
      
      IF (s^.index+1 <= MaxIndex) AND (s^.buffer[s^.index+1] = ASCII.LF) THEN
      
      (* TO DO *)
      
      END
      
    END
  END;
  
  RETURN ch
END currChar;


PROCEDURE lookahead ( s : Source ) : CHAR;
(* Returns the lookahead character of s.
   Does not consume any character and does not set eof. *)

VAR
  ch, next : CHAR;
  
BEGIN
  (* check if lookahead lies beyond eof *)  
  IF s^.eof OR (s^.index >= MaxIndex) THEN
    next := ASCII.NUL
    
  ELSE (* current position lies within buffer *)
    ch := s^.buffer[s^.index];
    
    (* lookahead may still lie beyond eof ... *)
    next := s^.buffer[s^.index+1]; (* tentative *)
  
    (* ... if CR+LF sequence is at current position ... *)
    IF (ch = ASCII.CR) AND (next = ASCII.LF) THEN
      
      (* ... and lies at the end of the buffer *)
      IF s^.index+2 > MaxIndex THEN
        next := ASCII.NUL
        
      ELSE (* lookahead lies within buffer *)
        next := s^.buffer[s^.index+2]
        
        (* translate ASCII.CR to ASCII.LF *)
        IF next = ASCII.CR THEN
          next := ASCII.LF
          
        END
      END
    END
  END;
    
  RETURN next
END lookahead;


PROCEDURE lookahead2 ( s : Source ) : CHAR;
(* Returns the second lookahead character of s.
   Does not consume any character and does not set eof. *)
   
VAR
  ch, next, la2 : CHAR;
  
BEGIN
  ch := s^.buffer[s^.index];
  next := s^.buffer[s^.index+1];
  
  (* skip LF immediately following CR *)
  IF (ch = ASCII.CR) AND (next = ASCII.LF) THEN
    la2 := s^.buffer[s^.index+2]
  
  ELSIF next = ASCII.CR THEN
    la2 := ASCII.LF
  
  ELSE
    la2 := next
    
  END;
  
  RETURN la2
END lookahead2;


PROCEDURE markLexeme ( s : Source; VAR line, col : CARDINAL );
(* Marks the lookahead position in s as the start of the marked lexeme.
   Passes back lookahead position line and column counters in line and col. *)

BEGIN

  (* TO DO *)

END markLexeme;


PROCEDURE copyLexeme ( s : Source; dict : LexDict; VAR handle : DictHandle );
(* Adds the marked lexeme in s to lexeme dictionary dict, passes its access
   handle back in handle and clears the lexeme marker.  If no lexeme marker
   has been set, no content is copied and zero is passed back in handle. *)

BEGIN

  (* TO DO *)

END copyLexeme;


PROCEDURE getLineAndColumn ( s : Source; VAR line, col : CARDINAL );
(* Passes back the current line and column counters of s in line and col. *)

BEGIN
  line := s^.line;
  col := s^.column;
  
  RETURN
END getLineAndColumn;


PROCEDURE eof ( s : Source ) : BOOLEAN;
(* Returns TRUE if the last character in s has been consumed, else FALSE. *)

BEGIN

  (* TO DO *)

END eof;


PROCEDURE close ( VAR s : Source; VAR status : Status );
(* Closes the file associated with s and deallocates s.  Passes back NIL in s
   if successful.  The status of the operation is passed back in status. *)

BEGIN

  (* TO DO *)

END close;


(* Private Operations *)

PROCEDURE incrLine ( source : Source );

BEGIN
  
  IF source^.index < source^.maxIndex THEN
    INC(source^.index);
    INC(source^.line);
    source^.column := 1
    
  ELSE
    source^.eof := TRUE
    
  END;
  
  RETURN
END incrLine;


PROCEDURE incrColumn ( source : Source );
(* Increments source's current reading position and column counter.
   If reading position is at end of source, source's eof flag is set. *)

BEGIN

  IF source^.index < source^.maxIndex THEN
    INC(source^.index);
    INC(source^.column)
    
  ELSE
    source^.eof := TRUE
    
  END;
  
  RETURN
END incrColumn;


END M2Source.