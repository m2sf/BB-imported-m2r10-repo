(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE M2Source;

(* Modula-2 Source File Reader *)

IMPORT ISO646, FileIO;

FROM M2Params IMPORT MaxSourceFileSize;

FROM M2LexTab IMPORT store;


TYPE Source = POINTER TO SourceDescriptor;

TYPE SourceDescriptor = RECORD
  index,                   (* index of lookahead position in source buffer *)
  endPos,                  (* index of end position in source buffer *)
  lexPos : CARDINAL;       (* index of marked position in source buffer *)
  buffer : SourceBuffer;   (* source buffer with entire source *)
  line : LineCounter;      (* line counter for lookahead position *)
  column : ColumnCounter   (* column counter for lookahead position *)
END;

TYPE SourceBuffer = ARRAY [0..MaxSourceFileSize] OF CHAR;
(* always to be terminated by ASCII.NUL, therefore max index = size + 1 *)


(* ---------------------------------------------------------------------------
 *  Definitions
 *
 *  start position :
 *    the position of the first character in the source.
 *
 *  end position :
 *    the position of the last character in the source.
 *
 *  lookahead position :
 *    the position of the character to be consumed next.
 *
 *  second lookahead position :
 *    the position immediately following the lookahead position.
 *
 *  marked position :
 *    a position recorded as the start of a lexeme.
 *    it is end position + 1 if no marker has been set.
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
 *    a character sequence that starts at the marked position (inclusively)
 *    and ends at the lookahead position (exclusively).
 *
 *  character consumption :
 *    a character is consumed by advancing the lookahead position
 *    to the character's second lookahead position or by setting eof.
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


(* Operations *)

(* ---------------------------------------------------------------------------
 * procedure new ( source, filename, status )
 *  creates a new source instance, associated with filename
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
PROCEDURE new
  ( VAR s : Source; filename : Filename; VAR status : Status );
(* Passes back a newly allocated source instance associated with name in s.
   The associated file is opened for reading and the lookahead position is
   set to the start position.  Passes back NIL in s if unsuccessful.
   The status of the operation is passed back in status. *)

VAR
  file : FileIO.File;
  source : Source;
  bytesRead : CARDINAL;
  
BEGIN
  
  (* source must be NIL *)
  IF s # NIL THEN
    status := allocTargetNotNil;
    RETURN
  END;
    
  IF FileSizeOf(filename) > MaxSourceFileSize THEN
    status := sourceExceedsMaxFileSize;
    RETURN
  END;
  
  (* allocate source instance *)
  NEW(source);
  
  (* read source file contents into buffer *)
  FileIO.open(file, filename);
  FileIO.ReadNBytes(file, source^.buffer, bytesRead);
  FileIO.close(file);
    
  (* TO DO : check for and handle file IO errors *)
  
  (* set start and end position *)
  source^.index := 0;
  source^.endPos := bytesRead-1;
  
  (* clear lexeme marker by setting it beyond end position *)
  source^.lexPos := source^.endPos+1;
  
  (* terminate buffer *)
  source^.buffer[source^.endPos+1] := ISO646.NUL;
  
  (* initialise line and column counters *)
  source^.line := 1; source^.column := 1;
  
  (* pass back status and source *)
  status := success;
  s := source;

  RETURN  
END new;


(* ---------------------------------------------------------------------------
 * procedure getChar ( source, ch, next )
 *  consumes current lookahead character, passes back new lookahead character
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
PROCEDURE getChar ( s : Source; VAR ch, next : CHAR );
(* Passes back the lookahead character in ch and consumes it.
   Passes back the new lookahead character in next without consuming it. *)

BEGIN

  (* pass and consume lookahead character *)
  ch := s^.buffer[s^.index];
  consumeChar(s);
  
  (* pass LF instead of CR *)
  IF ch = ISO646.CR THEN
    ch := ISO646.LF
  END;

  (* pass new lookahead character *)
  next := s^.buffer[s^.index];
  
  (* pass LF instead of CR *)
  IF next := ISO646.CR THEN
    next := ISO646.LF
  END;
  
  RETURN
END getChar;
  

(* ---------------------------------------------------------------------------
 * procedure consumeChar ( source )
 *  consumes current lookahead character
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
PROCEDURE consumeChar ( s : source );

VAR
  ch : CHAR;

BEGIN
  
  (* remember the lookahead character *)
  ch := s^.buffer[s^.index];
  
  (* ... and consume it *)
  IF s^.index <= s^.endPos THEN
    INC(s^.index)
    
  END;
  
  (* check for new line *)
  IF (* new line *) (ch = ISO646.LF) OR (ch = ISO646.CR) THEN
    (* update line and column counters *)
    INC(s^.line); s^.column := 1;
    
    (* check for CR LF sequence *)
    IF (ch = ISO646.CR) AND (s^.buffer[s^.index] = ISO646.LF) THEN
      (* consume trailing LF *)
      INC(s^.index)
      
    END
    
  ELSE (* no new line *)
    (* update column counter only *)
    INC(s^.column)
    
  END
    
  RETURN
END consumeChar;


(* ---------------------------------------------------------------------------
 * procedure lookaheadChar ( source ) : CHAR
 *  returns current lookahead character
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
PROCEDURE lookaheadChar ( s : Source ) : CHAR;
(* Returns the lookahead character of s.
   Does not consume any character and does not set eof. *)

VAR
  next : CHAR;
  
BEGIN

  (* get lookahead character *)
  next := s^.buffer[s^.index];
  
  (* return LF instead of CR *)
  IF next = ISO646.CR THEN
    next := ISO646.LF
  END;
    
  RETURN next
END lookaheadChar;


(* ---------------------------------------------------------------------------
 * procedure lookahead2 ( source ) : CHAR
 *  returns second lookahead character
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
PROCEDURE lookahead2 ( s : Source ) : CHAR;
(* Returns the second lookahead character of s.
   Does not consume any character and does not set eof. *)
   
VAR
  next, la2 : CHAR;
  
BEGIN
  
  (* return ASCII.NUL if lookahead is last character or beyond eof *)
  IF s^.index >= s^.endPos THEN
    RETURN ISO646.NUL
  END;
  
  (* get lookahead and tentative second lookahead *)
  next := s^.buffer[s^.index];
  la2 := s^.buffer[s^.index+1];
  
  (* check if lookahead is CR LF sequence *)
  IF (next = ISO646.CR) AND (la2 = ISO646.LF) THEN
  
    (* return ASCII.NUL if CR LF is at the very end of source *)
    IF s^.index+1 >= s^.endPos THEN
      RETURN ISO646.NUL
    END;
    
    (* otherwise second lookahead is character after CR LF sequence  *)
    la2 := s^.buffer[s^.index+2]
  END
  
  (* return LF instead of CR *)
  IF la2 = ISO646.CR THEN
    la2 := ISO646.LF
  END;
  
  RETURN la2
END lookahead2;


(* ---------------------------------------------------------------------------
 * procedure markLexeme ( source, line, col )
 *  marks current lookahead position as the start of a lexeme
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
PROCEDURE markLexeme ( s : Source; VAR line, col : CARDINAL );
(* Marks the lookahead position in s as the start of the marked lexeme.
   Passes back lookahead position line and column counters in line and col. *)

BEGIN

  s^.lexPos := s^.index;
  line := s^.line;
  col := s^.column;

  RETURN
END markLexeme;


(* ---------------------------------------------------------------------------
 * procedure copyLexeme ( source, dict, handle )
 *  adds a marked lexeme to lexeme dictionary dict
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
PROCEDURE copyLexeme ( s : Source; dict : LexDict; VAR handle : DictHandle );
(* Adds the marked lexeme in s to lexeme dictionary dict, passes its access
   handle back in handle and clears the lexeme marker.  If no lexeme marker
   has been set, no content is copied and zero is passed back in handle. *)

VAR
  length : CARDINAL;
  
BEGIN
  
  (* return zero handle if no lexeme marker is set *)
  IF s^.lexPos >= s^.index THEN
    handle := 0;
    RETURN
  END;
  
  (* store marked lexeme in lexeme dictionary *)
  length := s^.index - s^.lexPos;
  store(dict, s^.buffer, s^.lexPos, length, handle);
  
  (* clear lexeme marker by setting it beyond end position *)
  s^.lexPos := s^.endPos+1;
  
  RETURN
END copyLexeme;


(* ---------------------------------------------------------------------------
 * procedure getLineAndColumn ( source, line, column )
 *  passes back line and column counters for current lookahead position
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
PROCEDURE getLineAndColumn ( s : Source; VAR line, col : CARDINAL );
(* Passes back the current line and column counters of s in line and col. *)

BEGIN

  line := s^.line;
  col := s^.column;

  RETURN
END getLineAndColumn;


(* ---------------------------------------------------------------------------
 * procedure eof ( source ) : BOOLEAN
 *  returns TRUE if last character in source has been consumed, else FALSE
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
PROCEDURE eof ( s : Source ) : BOOLEAN;

BEGIN
  (* eof is set if lookahead position is greater than end position *)
  RETURN s^.index > s^.endPos
END eof;


(* ---------------------------------------------------------------------------
 * procedure release ( source )
 *  releases source instance
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) source must not be NIL
 *
 * post-conditions:
 *  (1) lexer is deallocated
 *  (2) NIL is passed back in source
 *
 * error-conditions:
 *  (1) reference to source remains unmodified
 * ---------------------------------------------------------------------------
 *)
PROCEDURE release ( VAR s : Source; VAR status : Status );

BEGIN

  IF s = NIL THEN
    status := invalidReference;
    RETURN
  END;
  
  DISPOSE(s);
  status := success;
  s := NIL;
  
  RETURN
END release;


END M2Source.