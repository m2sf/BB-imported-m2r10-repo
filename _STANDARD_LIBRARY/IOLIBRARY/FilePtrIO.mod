(* (C) 2009-2016 by B.Kowarsch & R.Sutcliffe. All rights reserved. *)

IMPLEMENTATION MODULE FilePtrIO;

(* Driver for File IO on File Pointers *)

IMPORT UNSAFE, FileDescIO, PtrSet;

(* File accessor and file status *)

TYPE File = POINTER TO Descriptor; (* implementation defined file accessor *)
    
TYPE Descriptor = RECORD
  fd : FileDescIO.File;
  special : BOOLEAN;
  name : DString;
  mode : FileMode;
  maxPos,
  bufPos : Pos;
  status : IOStatus;
  bufSize,
  index : IOSIZE;
  buffer : UNSAFE.ADDRESS
END;


(* Default File Buffer *)

TYPE DefaultBufferPtr = POINTER TO DefaultBuffer;

TYPE DefaultBuffer = BARE ARRAY DefaultBufferSize OF OCTET;


(* File Table *)

VAR fileTable : PtrSet;


(* stdin, stdout, stderr and nullfile *)
VAR dfltInFile, dfltOutFile, dfltErrFile, nullFile : File;


(* Introspection *)

PROCEDURE openFileCount : LONGCARD;
BEGIN
  RETURN PtrSet.Count(fileTable)
END openFileCount;

PROCEDURE isValidAccessor ( file : File ) : BOOLEAN;
(* Returns TRUE if <file> is a valid file accessor, otherwise FALSE. *)
BEGIN
  IF file = NIL THEN
    RETURN FALSE
  ELSE
    RETURN PtrSet.isPresent(fileTable, file)
  END
END isValidAccessor;

PROCEDURE isSpecialFile ( file : File ) : BOOLEAN;
(* Returns TRUE if file is associated with a special file, otherwise FALSE. *)
BEGIN
  IF file = NIL THEN
    RETURN FALSE
  END
    RETURN file^.special
  END
END isSpecialFile;

PROCEDURE isValidPos ( file : File; pos : Pos ) : BOOLEAN;
(* Returns TRUE if <pos> is a valid file position for <file>,
   otherwise FALSE. *)
BEGIN
  IF file = NIL THEN
    RETURN FALSE
  ELSE
    RETURN pos <= file^.maxPos
  END
END isValidPos;


(* Opening and Closing Files *)

PROCEDURE Open
  ( NEW file       : File;
    CONST filename : ARRAY OF CHAR;
    mode           : FileMode;
    VAR status     : IOStatus );
(* Opens the file <filename> in file mode <mode>.  If sucessful passes a file
   accessor back in <file>,  otherwise passes NIL back in <file>.  Sets the
   file position depending on <mode>.  The status of the operation is passed
   back in <status>. *)
VAR
  fd : FileDescIO.File;
  newFile : File;
  newBuf : DefaultBufferPtr;
  bufSize : IOSIZE;
  bufAddr : UNSAFE.ADDRESS;
BEGIN
  (* request a file descriptor *)
  FileDescIO.Open(fd, filename, mode, status);
  
  (* on failure bail out *)
  IF status.failed THEN
    RETURN
  END;
  
  (* allocate default buffer *)
  NEW newBuf;
  
  (* on failure bail out *)
  IF newFile = NIL THEN
    FileDescIO.Close(fd, status);
    status := { TRUE, IOStatus.AllocationFailed };
    RETURN
  END;
  
  bufSize := DefaultBufferSize;
  bufAddr := UNSAFE.ADR(newBuf);
    
  (* allocate a new file structure *)
  NewFileWithDescriptor(newFile, filename, mode, bufSize, bufAddr, fd);
  
  (* on failure bail out *)
  IF newFile = NIL THEN
    RELEASE newBuf;
    FileDescIO.Close(fd, status);
    status := { TRUE, IOStatus.AllocationFailed };
    RETURN
  END;
  
  (* register new file in file table *)
  PtrSet.Insert(fileTable, newFile);
  
  file := newFile;
  RETURN
END Open;

PROCEDURE OpenWithBuffer
  ( NEW file         : File;
    CONST filename   : ARRAY OF CHAR;
    mode             : FileMode;
    VAR buffer       : ARRAY OF OCTET;
    VAR status       : IOStatus );
(* Opens the file <filename> in file mode <mode> using <buffer> as a custom
   file buffer.  If successful passes a file accessor back in <file>,  other-
   wise passes NIL back in <file>.  Sets the file position depending on <mode>.
   The status is passed back in <status>. *)
VAR
  fd : FileDescIO.File;
  newFile : File;
  bufSize : IOSIZE;
  bufAddr : UNSAFE.ADDRESS;
BEGIN
  (* make sure buffer is large enough *)
  IF COUNT(buffer) < MinBufferSize THEN
    status := IOStatus.InsufficientBufferSize;
    RETURN
  END;
  
  (* request a file descriptor *)
  FileDescIO.Open(fd, filename, mode, status);
  
  (* on failure bail out *)
  IF status.failed THEN
    RETURN
  END;
    
  (* custom buffer *)
  bufSize := COUNT(buffer);
  bufAddr := UNSAFE.ADR(buffer);
  
  (* allocate a new file structure *)
  NewFileWithDescriptor(newFile, filename, mode, bufSize, bufAddr, fd);
  
  (* on failure bail out *)
  IF newFile = NIL THEN
    RELEASE buffer;
    FileDescIO.Close(fd, status);
    status := { TRUE, IOStatus.AllocationFailed };
    RETURN
  END;
  
  (* register new file in file table *)
  PtrSet.Insert(fileTable, newFile);
  
  file := newFile;
  RETURN
END OpenWithBuffer;

PROCEDURE ReOpen ( file : File; mode : FileMode );
(* Flushes the file associated with file accessor <file> and changes its mode
   to <mode>. Sets the file position depending on <mode>. *)
BEGIN
  (* TO DO *)
END ReOpen;

(* Opening or reopening a file in a mode whose append flag is not set causes
   the file position to be set to the beginning of the file.  Opening or re-
   opening a file in a mode whose append flag is set causes the file position
   to be set to the end of the file.  An attempt to open a file that is a di-
   rectory fails with status code mayNotOpenDirectory. *)

PROCEDURE Close ( VAR file : File; VAR status : IOStatus );
(* Performs Flush on <file>, closes the associated file and passes NIL back
   in <file>.  The status of the operation is passed back  in <status>. *)
BEGIN
  IF FileTable.isPresent(fileTable, file) THEN
    PtrSet.Remove(fileTable, file);
    Flush(file);
    FileDescIO.Close(file^.fd, status);
    ClearBuffer(file);
    DeallocBuffer(file);
    RELEASE(file);
    status := {FALSE, IOStatus.Success}
  ELSE
    status := {TRUE, IOStatus.InvalidFile}
  END
END Close;


(* Querying File Parameters *)

PROCEDURE modeOf ( file : File ) : FileMode;
(* Returns the file mode of file accessor <file>. *)
BEGIN
  IF file = NIL THEN
    RETURN {}
  ELSE
    RETURN file^.mode
  END
END modeOf;

PROCEDURE statusOf ( file : File ) : IOStatus;
(* Returns the status of the most recent operation for <file>. *)
BEGIN
  IF File = NIL THEN
    RETURN { TRUE, IOStatus.InvalidFile }
  ELSE
    RETURN file^.status
  END
END statusOf;

PROCEDURE nameLen ( file : File ) : CARDINAL;
(* Returns the length of the filename associated with <file>. *)
BEGIN
  IF file = NIL THEN
    RETURN 0
  ELSE
    RETURN file^.name^.len
  END
END nameLen;

PROCEDURE GetName ( file : File; VAR filename : ARRAY OF CHAR );
(* Passes the name  of the file  associated with  file accessor <file> back in
   <filename>.  If the name  exceeds the capacity  of the filename array,  the 
   operation fails  with status NameTooLong,  passing an empty string  back in
   <filename>. *)
BEGIN
  IF file = NIL THEN
    filename := ""
  ELSIF file^.name^.len >= CAPACITY(filename) THEN
    status := { TRUE, IOStatus.NameTooLong };
    filename := ""
  ELSE
    COPY filename := file^.name^.str;
  END
END GetName;

(* File position operations *)

PROCEDURE eof ( file : File ) : BOOLEAN;
(* Returns TRUE if the end of the file associated with file accessor <file>
   has been reached, otherwise FALSE. *)
BEGIN
  IF file = NIL THEN
    RETURN FALSE
  ELSE
    file^.status := { FALSE, IOStatus.Succes };
    RETURN file^.eof
  END
END eof;

PROCEDURE currentPos ( file : File ) : FilePos;
(* Returns the current read/write position for file accessor <file> where a
   value of TMIN(FilePos) represents the beginning of a file. *)
BEGIN
  IF file = NIL THEN
    file^.status := { TRUE, IOStatus.InvalidFile };
    RETURN 0
  ELSIF file^.special THEN
    file^.status := { TRUE, IOStatus.OperationNotSupported };
    RETURN 0
  ELSE
    file^.status := { TRUE, IOStatus.Success };
    RETURN file^.bufPos + file^.index
  END
END currentPos;

PROCEDURE lastValidSetPos ( file : File ) : FilePos;
(* Returns the most advanced position that may be passed to SetPos for file
   accessor <file>.  The return value depends on length and mode of <file>
   
   (1) if <file> is in append mode, the function returns zero and sets the
       file's status to OperationNotSupported.
   (2) if <file> is empty, then TMIN(FilePos) is returned.
   (3) if <file> is not empty and has its write flag set, then the position
       of the last octet in <file> plus one is returned.
   (4) if <file> is not empty and has its read but not its write flag set,
       then the position of the last octet in <file> is returned. *)
BEGIN
  IF file = NIL THEN
    RETURN 0
  ELSIF FileMode.Append IN file^.mode THEN
    file^.status := { TRUE, IOStatus.OperationNotSupported }
    RETURN 0
  ELSIF FileMode.Write IN file^.mode THEN
    RETURN maxPos + 1
  ELSIF FileMode.Read IN file^.mode THEN
    RETURN maxPos
  ELSE
    (* if we get here, the file's mode is corrupt *)
    (* TO DO : mitigation *)
    RETURN 0
  END
END lastValidSetPos;

PROCEDURE SetPos ( file : File; pos : Pos );
(* Sets the  read/write position  for  file accessor <file>  to <pos>.  For
   any empty file, <pos> must be zero. For any non-empty file in read mode,
   <pos> must be  less than  LastValidWritePos.  For any  non-empty file in
   write mode, <pos> must be  less than or equal  to LastValidWritePos. *)
BEGIN
  IF file = NIL THEN
    RETURN
  END;
  
  IF pos > lastValidSetPos(file) THEN
    file^.status := { TRUE, IOStatus.InvalidPos };
    RETURN
  END;
  
  IF (pos < file^.bufPos) OR (pos > file^.bufPos + file^.bufSize) THEN
    (* target position lies outside of current buffer *)
    FlushBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
    LoadBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
    file^.index := 0;
  ELSE
    file^.index := pos - file^.bufPos
  END;
  
  file^.status := { FALSE, IOStatus.Success };
  RETURN
END SetPos;

PROCEDURE Advance ( file : File; offset : Pos );
(* Advances the read/write position for file accessor <file> by <offset>. *)
VAR
  targetPos : Pos;
BEGIN
  IF file = NIL THEN
    RETURN
  END;
  
  targetPos := file^.bufPos + index + 1;
  IF targetPos > lastValidSetPos(file) THEN
    file^.status := { TRUE, IOStatus.InvalidPos };
    RETURN
  END;
  
  IF targetPos > file^.bufPos + file^.bufSize THEN
    (* target position lies outside of current buffer *)
    FlushBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
    LoadBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
    file^.index := 0;
  ELSE
    file^.index := targetPos - file^.bufPos
  END

  file^.status := { FALSE, IOStatus.Success };
  RETURN
END Advance;

PROCEDURE Rewind ( file : File );
(* Sets the read/write position for file accessor <file>  to the  beginning
   of the file and resets its end-of-file status. *)
BEGIN
  IF file = NIL THEN
    RETURN
  END;
  
  IF file^.bufPos > 0 THEN
    (* target position lies outside of current buffer *)
    FlushBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
    LoadBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
  END;
  
  file^.index := 0;
  file^.eof := FALSE;
  file^.status := { FALSE, IOStatus.Success };
  RETURN
END Rewind;

(* Any attempt to call  LastValidWritePos, SetPos, Advance or Rewind  on  a
   file accessor  whose file mode  has the append flag set  shall fail with
   status OperationNotSupported.  Any attempt to set a file's read position
   past the end of the file  shall fail  with status  AccessBeyondEOF.  Any
   attempt to set a file's write position past LastValidWritePos shall fail
   with status AccessBeyondEOF. *)


(* Read and write operations *)

PROCEDURE Read ( file : File; VAR data : OCTET );
(* Reads one octet of data at the current position of <file>, passes it back
   in <data> and advances the read/write position of <file> by one. *)
BEGIN
  IF file = NIL THEN
    RETURN
  END;
  
  data := file^.buffer[file^.index];
  
  IF file^.index < file^.bufSize - 1 THEN
    file^.index++
  ELSE
    (* end of buffer reached, flush and reload *)
    FlushBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
    LoadBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
    file^.index := 0;
  END;

  file^.status := { FALSE, IOStatus.Success };
  RETURN
END Read;

PROCEDURE ReadBlock
  ( file           : File;
    VAR data       : ARRAY OF OCTET );
(* Reads a block of data  at the current position of <file>,  passes it back
   in <data> and advances the read/write position of <file> by the number of
   octets read. *)
VAR
  octetsRead, maxIndex : IOSIZE;
BEGIN
  IF file = NIL THEN
    RETURN
  END;
  
  octetsRead := 0;
  WHILE (octetsRead < COUNT(array)) AND (NOT file^.eof) DO
  
    IF file^.index = file^.bufSize THEN
      FlushBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
      LoadBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
      file^.index := 0
    END;
    
    maxIndex := file^.bufSize - file^.index - 1;
    FOR index IN [0..maxIndex] DO
      file^.buffer[file^.index] := data[index];
      octetsRead++;
      file^.index++
    END; (* FOR *)    
  END; (* WHILE *)
    
  file^.status := { FALSE, IOStatus.Success };
  RETURN
END ReadBlock;

PROCEDURE insertReady ( file : File ) : BOOLEAN;
(* Returns FALSE if the insert buffer of <chan> is full, otherwise TRUE. *)
BEGIN
  (* TO DO *)
END insertReady;

PROCEDURE Insert ( stream : File; data : OCTET );
(* Inserts octet <data> into input stream <stream> to be read by the next read
   operation on <stream>.  Inserted octets are stored in a FIFO buffer of size
   InsertBufferSize.  The minimum size of the insert buffer is two octets. *)
BEGIN
  (* TO DO *)
END Insert;

(* Any attempt to read or lookahead read from a file whose read flag is not
   set shall fail with status OperationNotSupported. *)

PROCEDURE Write ( file : File; data : OCTET );
(* Performs a write or append operation depending on the mode of <file>.
   In write mode, the procedure writes one octet in <data> to the current
   position of <file>. In append mode, the procedure atomically sets the
   current read/write position of <file> to the end of the file and appends
   one octet in <data> to the end of the file. In either mode, the current
   read/write position of <file> is advanced by one after the data has been
   written. *)
BEGIN
  IF file = NIL THEN
    RETURN
  END;
  
  file^.buffer[file^.index] := data;
  
  IF file^.index < file^.bufSize - 1 THEN
    file^.index++
  ELSE
    (* end of buffer reached, flush and reload *)
    FlushBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
    LoadBuffer(file^.buffer, file^.fd, file^.bufPos, file^.bufSize);
    file^.index := 0;
  END;

  file^.status := { FALSE, IOStatus.Success };
  RETURN
END Write;

PROCEDURE WriteBlock
  ( file              : File;
    data              : ARRAY OF OCTET;
    VAR octetsWritten : IOSIZE );
(* Performs a write or append operation depending on the mode of <file>.
   In write mode, the procedure writes the block of data passed in <data>
   starting at the current read/write position of <file>. In append mode, the
   procedure atomically sets the current read/write position of <file> to the
   end of the file and appends the block of data passed in <data> to the end
   of the file. In either mode, the current read/write position of <file> is
   advanced by the number of octets written and the number of octets written
   is passed back in <octetsWritten> after the data has been written. *)
BEGIN
  (* TO DO *)
END WriteBlock;

PROCEDURE isFlushable ( file : File ) : BOOLEAN;
(* Returns TRUE if <file> is flushable, otherwise FALSE. *)
BEGIN
  RETURN FileMode.Write IN file^.mode AND NOT isSpecialFile(file);
END isFlushable;

PROCEDURE Flush ( file : File );
(* Writes unwritten data in any buffer of <file> to its associated file. *)
BEGIN
  (* TO DO *)
END Flush;

(* Any attempt to write to or flush a file whose write flag is not set shall
   fail with status operationNotSupported. *)


(* Implementation defined accessors to special files *)

(* Special files  are automatically opened when module FileIO is initialised.
   The file mode of a special file is either read  or write.   EOF always re-
   turns FALSE and CurrentPos always returns TMIN(FilePos) for special files.
   Any attempt to open a special file will fail with status alreadyOpen.  Any
   attempt to call ReOpen, NameLen, GetName, LastValidSetPos, SetPos, Rewind,
   Advance, Lookahead, LA2, ReadBlock, Flush or Close  will fail with  status
   operationNotSupported.  NameLen  and LastValidSetPos will return zero  and
   GetName will pass an empty string. *)

PROCEDURE defaultInFile : File;
(* Returns a file accessor associated with stdin. *)
BEGIN
  RETURN dfltInFile
END defaultInFile;

PROCEDURE defaultOutFile : File;
(* Returns a file accessor associated with stdout. *)
BEGIN
  RETURN dfltOutFile
END defaultOutFile;

PROCEDURE defaultErrFile : File;
(* Returns a file accessor associated with stderr. *)
BEGIN
  RETURN dfltErrFile
END defaultErrFile;

PROCEDURE defaultNullFile : File;
(* Returns the file accessor associated with the null device, open in write
   mode.  Write operations on this file have no effect. *)
BEGIN
  RETURN nullFile
END defaultNullFile;

BEGIN
  (* TO DO *)
  (* create file structure for dfltInFile, init with fd for stdin *)
  (* create file structure for dfltOutFile, init with fd for stdout *)
  (* create file structure for dfltErrFile, init with fd for stderr *)
  (* create file structure for dfltNullFile, inti with new fd for /dev/null *)  
END FilePtrIO.