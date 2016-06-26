(* (C) 2009-2016 by B.Kowarsch & R.Sutcliffe. All rights reserved. *)

IMPLEMENTATION MODULE FilePtrIO;

(* Driver for File IO on File Pointers *)

IMPORT UNSAFE, FileDescIO, FileTable;

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

TYPE DefaultBuffer = POINTER TO DefaultBufferArray;

TYPE DefaultBufferArray = BARE ARRAY DefaultBufferSize OF OCTET;


(* File Table *)

VAR fileTable : FileTable;


(* stdin, stdout, stderr and nullfile *)
VAR dfltInFile, dfltOutFile, dfltErrFile, nullFile : File;


(* Introspection *)

PROCEDURE isValidAccessor ( file : File ) : BOOLEAN;
(* Returns TRUE if <file> is a valid file accessor, otherwise FALSE. *)
BEGIN
  RETURN FileTable.isPresent(fileTable, file)
END isValidAccessor;

PROCEDURE isSpecialFile ( file : File ) : BOOLEAN;
(* Returns TRUE if file is associated with a special file, otherwise FALSE. *)
BEGIN
  RETURN file^.special
END isSpecialFile;

PROCEDURE isValidPos ( file : File; pos : Pos ) : BOOLEAN;
(* Returns TRUE if <pos> is a valid file position for <file>,
   otherwise FALSE. *)
BEGIN
  IF FileMode.Write IN file^.mode THEN
    RETURN pos + 1 <= file^.maxPos
  ELSE
    RETURN pos <= file^.maxPos
  END
END isValidPos;


(* Opening files *)

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
  newBuf : DefaultBuffer;
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
  FileTable.Insert(fileTable, newFile);
  
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
  FileTable.Insert(fileTable, newFile);
  
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


(* Querying File Mode, Status and Filename *)

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
  IF file # NIL AND file^.name^.len < CAPACITY(filename) THEN
    COPY filename := file^.name^.str;
  ELSE
    filename := "";
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
    (* mitigate *)
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
  
  IF pos > lastValidSetPos THEN
    status := IOStatus.InvalidPos;
    RETURN
  END;
  
  (* TO DO *)
  IF (* pos is outside of current buffer *) THEN
    (* load the requested file portion into the buffer
       and update buffer start marker bufOffs accordingly *)
  END;
  
  file^.currPos := pos;
  RETURN
END SetPos;

PROCEDURE Advance ( file : File; offset : Pos );
(* Advances the read/write position for file accessor <file> by <offset>. *)
BEGIN
  IF file^.currPos + file^.bufOffs >= file^.bufSize THEN
  (* new position is outside of current buffer *)
  (* load file portion into buffer and update buffer start marker *)
  ELSE
    file^.currPos++
  END
END Advance;

PROCEDURE Rewind ( file : File );
(* Sets the read/write position for file accessor <file>  to the  beginning
   of the file and resets its end-of-file status. *)
BEGIN
  IF file^.bufOffs > 0 THEN
  (* new position is outside of current buffer *)
  (* load file portion into buffer and update buffer start marker *)
  END;
  file^.currPos := 0;
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
  data := file^.currAddr^;
  IF file^.currAddr < file^.lastBufAddr THEN
    file^.currAddr++
  ELSE
    LoadBuffer(file^.fd, firstBufAddr, lastBufAddr);
    file^.currAddr := firstBufAddr
  END
END Read;

PROCEDURE Lookahead ( file : File; VAR data : OCTET );
(* Reads the first lookahead octet of <file>, passes it back in <data>, but
   does NOT advance the read/write position of <file>.  If the lookahead
   octet lies beyond the end of the file then no data is passed back and
   the file status is set to AccessBeyondEOF. *)
BEGIN
  (* TO DO *)
END LookAhead;

PROCEDURE LA2 ( file : File; VAR data : OCTET );
(* Reads the second lookahead octet of <file>, passes it back in <data>, but
   does NOT advance the read/write position of <file>.  If the second look-
   ahead octet lies beyond the end of the file then no data is passed back
   and the file status is set to AccessBeyondEOF. *)

PROCEDURE ReadBlock
  ( file           : File;
    VAR data       : ARRAY OF OCTET;
    VAR octetsRead : IOSIZE );
(* Reads a block of data at the current position of <file>. Passes the block
   of data back in <data> and the number of octets read in <octetsRead>.
   The read/write position of <file> is advanced accordingly. *)
BEGIN
  (* TO DO *)
END ReadBlock;

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
  file^.currAddr^ := data;
  IF file^.currAddr < file^.lastBufAddr THEN
    file^.currAddr++
  ELSE
    FlushAndLoad(file^.fd, firstBufAddr, lastBufAddr);
    file^.currAddr := firstBufAddr
  END
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

PROCEDURE Flush ( file : File );
(* Writes unwritten data in any buffer of <file> to its associated file. *)
BEGIN
  (* TO DO *)
END Flush;

(* Any attempt to write to or flush a file whose write flag is not set shall
   fail with status operationNotSupported. *)


(* Closing files *)

PROCEDURE Close ( VAR file : File; VAR status : IOStatus );
(* Performs Flush on <file>, closes the associated file and passes NIL back
   in <file>.  The status of the operation is passed back  in <status>. *)
BEGIN
  IF FileTable.isPresent(fileTable, file) THEN
    FileTable.Remove(fileTable, file);
    Flush(file);
    FileDescIO.Close(file^.fd, status);
    ClearBuffer(file);
    DeallocBuffer(file);
    status := { FALSE, IOStatus.Success }
  ELSE
    status := { TRUE, IOStatus.InvalidFile }
  END
END Close;

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