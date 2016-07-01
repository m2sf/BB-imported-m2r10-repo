(* (C) 2016 by B.Kowarsch & R.Sutcliffe. All rights reserved. *)

IMPLEMENTATION MODULE FileIO;

(* File Specific Channel Based IO *)

(* Introspection *)

PROCEDURE isFileChannel ( chan : ChanIO.Channel ) : BOOLEAN;
(* Returns TRUE if <chan> is a valid file channel, else FALSE. *)
BEGIN
  CASE chan^ OF
  | Descriptor :
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END
END isFileChannel;

PROCEDURE isValidFile ( file : File ) : BOOLEAN;
(* Returns TRUE if the file accessor of <file> is valid, else FALSE. *)
BEGIN
  RETURN FilePtrIO.isValidFile(file^.handle)
END isValidFile;

PROCEDURE isSpecialFile ( file : File ) : BOOLEAN;
(* Returns TRUE if <file> is associated with a special file, else FALSE. *)
BEGIN
  RETURN FilePtrIO.isSpecialFile(file^.handle)
END isSpecialFile;

PROCEDURE isValidPos ( file : File; pos : Pos ) : BOOLEAN;
(* Returns TRUE if <pos> is a valid position for <file>, else FALSE. *)
BEGIN
  RETURN FilePtrIO.isValidPos(file^.handle)
END isValidPos;


(* Opening and Re-opening a File *)

(* Opening or reopening a file in a mode  whose append flag is not set  causes
   the file position  to be set  to the beginning of the file.  Opening or re-
   opening a file in a mode whose append flag is set  causes the file position
   to be set  to the end of the file.  An attempt to open a file that is a di-
   rectory fails with status code MayNotOpenDirectory. *)

PROCEDURE Open
  ( NEW file       : File;
    CONST filename : ARRAY OF CHAR;
    mode           : FileMode;
    VAR status     : IOStatus );
(* Opens file <filename> in file mode <mode>.  On success, a  newly allocated
   and initialised file channel is passed back in <file>.  On failure, NIL is
   passed back in <file>.  A status code is passed back in <status>. *)
BEGIN
  FilePtrIO.Open(file^.handle, filename, mode, status)
END Open;

PROCEDURE OpenWithBuffer
  ( NEW file         : File;
    CONST filename   : ARRAY OF CHAR;
    mode             : FileMode;
    VAR buffer       : BARE ARRAY OF OCTET;
    VAR status       : IOStatus );
(* Opens file <filename>  in file mode <mode>  using <buffer> as a custom file
   buffer.   On success,  a newly allocated  and initialised  file channel  is
   passed back in <file>.  On failure, NIL is passed back in <file>.  A status
   code is passed back in <status>. *)
BEGIN
  FilePtrIO.OpenWithBuffer(file^.handle, filename, mode, buffer, status)
END OpenWithBuffer;

PROCEDURE ReOpen ( file : File; mode : FileMode );
(* Flushes <file> and changes its access mode to <mode>.
   The file position is set depending on <mode>. *)
BEGIN
  FilePtrIO.ReOpen(file^.handle, mode)
END ReOpen;


(* Querying Mode, Status and Filename *)

PROCEDURE modeOf ( file : File ) : FileMode;
(* Returns the access mode of <file>. *)
BEGIN
  RETURN FilePtrIO.modeOf(file^.handle)
END modeOf;

PROCEDURE statusOf ( file : File ) : IOStatus;
(* Returns the status of the most recent operation on <file>. *)
BEGIN
  RETURN FilePtrIO.statusOf(file^.handle)
END statusOf;

PROCEDURE nameLen ( file : File ) : CARDINAL;
(* Returns the length of the filename associated with <file>. *)
BEGIN
  RETURN FilePtrIO.nameLen(file^.handle)
END nameLen;

PROCEDURE GetName ( file : File; VAR filename : ARRAY OF CHAR );
(* Passes the name of the file  associated with <file> back in <filename>.  If
   the name exceeds the capacity of <filename> the operation fails with status
   code NameTooLong and an empty string is passed back in <filename>. *)
BEGIN
  FilePtrIO.GetName(file^.handle, filename)
END GetName;


(* Fie Positioning *)

PROCEDURE currentPos ( file : File ) : Pos;
(* Returns the current read/write position of <file>. *)
BEGIN
  RETURN FilePtrIO.currentPos(file^.handle)
END currentPos;

PROCEDURE lastValidSetPos ( file : File ) : Pos;
(* Returns the most advanced position that may be passed to SetPos for <file>.
   The return value depends on the size and access mode of <file> as follows:
   
   (1) if <file> is in append mode, the function returns zero and sets the
       status of <file> to OperationNotSupported.
   (2) if <file> is empty, then TMIN(FilePos) is returned.
   (3) if <file> is not empty and has its write flag set, then the position
       of the last octet in <file> plus one is returned.
   (4) if <file> is not empty and has its read but not its write flag set,
       then the position of the last octet in <file> is returned. *)
BEGIN
  RETURN FilePtrIO.lastValidSetPos(file^.handle)
END lastValidSetPos;

PROCEDURE SetPos ( file : File; pos : Pos );
(* Sets the  read/write position of <chan> to <pos>. For any empty file, <pos>
   must be zero.  For any non-empty file in read mode, <pos> must be less than
   LastValidWritePos. For any non-empty file in write mode, <pos> must be less
   than or equal to LastValidWritePos. *)
BEGIN
  FilePtrIO.SetPos(file^.handle, pos)
END SetPos;

PROCEDURE Advance ( file : File; offset : Pos );
(* Advances the read/write position of <file> by <offset>. *)
BEGIN
  FilePtrIO.Advance(file^.handle, offset)
END Advance;

PROCEDURE Rewind ( file : File );
(* Sets the read/write position of <file> to the beginning of the file and re-
   sets its end-of-file status. *)
BEGIN
  FilePtrIO.Rewind(file^.handle)
END Rewind;

(* Any attempt to call  LastValidWritePos, SetPos, Advance or Rewind on a file
   channel whose access mode  has the append flag set  shall fail  with status
   OperationNotSupported.  Any attempt  to set  a file channel's read position
   past  the  end  of the file  shall fail  with  status AccessBeyondEOF.  Any
   attempt to set a file channel's write position past LastValidWritePos shall
   fail with status AccessBeyondEOF. *)


(* I/O operations *)

PROCEDURE eof ( file : File ) : BOOLEAN;
(* Returns TRUE if the end of <file> has been reached, otherwise FALSE. *)
BEGIN
  RETURN FilePtrIO.eof(file^.handle)
END eof;

PROCEDURE Read ( file : File; VAR data : OCTET );
(* Reads one octet of data  at the current position of <file>,  passes it back
   in <data> and advances the current read/write position of <file> by one. *)
BEGIN
  FilePtrIO(file^.handle, data)
END Read;

PROCEDURE Lookahead ( file : File; VAR data : OCTET );
(* Reads the first lookahead octet  of <file>,  passes it back in <data>,  but
   does NOT advance the read/write position of <file>.  If the lookahead octet
   lies beyond the end  of the file  then no data is passed back  and the file
   status is set to AccessBeyondEOF. *)
BEGIN
  FilePtrIO.Lookahead(file^.handle, data)
END Lookahead;

PROCEDURE LA2 ( file : File; VAR data : OCTET );
(* Reads the second lookahead octet of <file>,  passes it back in <data>,  but
   does  NOT advance  the read/write position  of <file>.  If the second look-
   ahead octet lies beyond the end of the file then no data is passed back and
   the file status is set to AccessBeyondEOF. *)
BEGIN
  FilePtrIO.LA2(file^.handle, data)
END LA2;

PROCEDURE ReadBlock
  ( file : File; VAR data : ARRAY OF OCTET; VAR octetsRead : IOSIZE );
(* Reads a block of data  at the current position of <file>.  Passes the block
   of data back in <data>  and the number of octets read in <octetsRead>.  The
   current read/write position of <file> is advanced by <octetsRead>. *)
BEGIN
  FilePtrIO.ReadBlock(file^.handle, data, octetsRead)
END ReadBlock;

(* Any attempt  to read or lookahead read  from a file channel whose read flag
   is not set shall fail with status OperationNotSupported. *)

PROCEDURE Write ( file : File; data : OCTET );
(* Performs a write or append operation depending on the access mode of <file>
   and advances the current read/write position by one after the data has been
   written. In write mode <data> is written to the current position of <file>.
   In append mode the procedure  atomically  sets the current read/write posi-
   tion to the end of the file and appends <data> to the end of <file>. *)
BEGIN
  FilePtrIO.Write(file^.handle, data)
END Write;

PROCEDURE WriteBlock
  ( file : File; data : BARE ARRAY OF OCTET; VAR octetsWritten : IOSIZE );
(* Performs a write or append operation depending on the access mode of <file>
   and passes the actual number of octets written in <octetsWritten>. The cur-
   rent read/write position is advanced accordingly. In write mode, the proce-
   dure writes <data> at the current read/write position of <chan>.  In append
   mode,  it atomically sets the current read/write position to the end of the
   file and appends <data> to the end of <file>. *)
BEGIN
  FilePtrIO.WriteBlock(file^.handle, data, octetsWritten)
END WriteBlock;

PROCEDURE Flush ( file : File );
(* Writes unwritten data in any buffer of <chan> to its associated file. *)
BEGIN
  FilePtrIO.Flush(file^.handle)
END Flush;

(* Any attempt  to write to  or flush a file whose write flag is not set shall
   fail with status OperationNotSupported. *)


(* Channel Method Implementations *)

PROCEDURE dataReady ( chan : ChanIO.Channel ) : BOOLEAN;
(* Channel method implementation equivalent to NOT eof(). *)
BEGIN
  CASE chan^ OF
  | Descriptor : RETURN (NOT FilePtrIO.eof(chan^.handle))
  ELSE
    (* signal IO error or raise runtime fault *)
  END
END dataReady;

PROCEDURE GetOctet
  ( chan : ChanIO.Channel; VAR data : OCTET; VAR status : IOStatus );
(* Channel method implementation corresponding to Read(). *)
BEGIN
  CASE chan^ OF
  | Descriptor :
    FilePtrIO.Read(chan^.handle, data);
    status := FilePtrIO.statusOf(chan^.handle)
  ELSE
    (* signal IO error or raise runtime fault *)
  END
END GetOctet;

PROCEDURE GetBlock
  ( chan : ChanIO.Channel; VAR data : ARRAY OF OCTET; VAR status : IOStatus );
(* Channel method implementation corresponding to ReadBlock(). *)
VAR count : IOSIZE;
BEGIN
  CASE chan^ OF
  | Descriptor :
    FilePtrIO.ReadBlock(chan^.handle, data, count);
    status := FilePtrIO.statusOf(chan^.handle)
  ELSE
    (* signal IO error or raise runtime fault *)
  END
END GetBlock;

PROCEDURE PutOctet
  ( chan : ChanIO.Channel; data : OCTET; VAR status : IOStatus );
(* Channel method implementation corresponding to Write(). *)
BEGIN
  CASE chan^ OF
  | Descriptor :
    FilePtrIO.Write(chan^.handle, data);
    status := FilePtrIO.statusOf(chan^.handle)
  ELSE
    (* signal IO error or raise runtime fault *)
  END
END PutOctet;

PROCEDURE PutBlock
  ( chan              : ChanIO.Channel;
    data              : ARRAY OF OCTET;
    VAR octetsWritten : IOSIZE;
    VAR status        : IOStatus );
(* Channel method implementation corresponding to WriteBlock(). *)
BEGIN
  CASE chan^ OF
  | Descriptor :
    FilePtrIO.WriteBlock(chan^.handle, data, octetsWritten);
    status := FilePtrIO.statusOf(chan^.handle)
  ELSE
    (* signal IO error or raise runtime fault *)
  END
END PutBlock;

PROCEDURE SyncData ( chan : ChanIO.Channel; VAR status : IOStatus );
(* Channel method implementation corresponding to Flush(). *)
BEGIN
  CASE chan^ OF
  | Descriptor :
    FilePtrIO.Flush(chan^.handle);
    status := FilePtrIO.statusOf(chan^.handle)
  ELSE
    (* signal IO error or raise runtime fault *)
  END
END SyncData;

(* Closing a File *)

PROCEDURE Close ( VAR file : File; VAR status : IOStatus );
(* Performs Flush on <file>,  closes  the associated file,  deallocates <file>
   and passes NIL back in <file>. A status code is passed back in <status>. *)
BEGIN
  FilePtrIO.Close(file^.handle, status)
END Close;


(* File Channels for Special Files *)

(* Special files stdin, stdout and stderr are automatically opened when a pro-
   cess is created.  Associated file channels are allocated when module FileIO
   is initialised.  The functions below return these associated file channels.
   The access mode of stdin is RO,  that of stdout and stderr is WO.  Function
   dataReady always returns TRUE for stdin. Function currentPos always returns
   TMIN(FilePos) for any special file. Any attempt to open a special file will
   fail with status AlreadyOpen. Any attempt to call ReOpen, nameLen, GetName,
   lastValidSetPos, SetPos, Rewind, Advance, Lookahead, LA2, ReadBlock,  Flush
   or Close will fail with status OperationNotSupported. Functions nameLen and
   lastValidSetPos will return zero and GetName will pass an empty string. *)
   
PROCEDURE defaultInFile : File;
(* Returns a file channel associated with special file stdin. *)
BEGIN
  RETURN FilePtrIO.defaultInFile
END defaultInFile;

PROCEDURE defaultOutFile : File;
(* Returns a file channel associated with special file stdout. *)
BEGIN
  RETURN FilePtrIO.defaultOutFile
END defaultOutFile;

PROCEDURE defaultErrFile : File;
(* Returns a file channel associated with special file stderr. *)
BEGIN
  RETURN FilePtrIO.defaultErrFile
END defaultErrFile;

PROCEDURE defaultNullFile : File;
(* Returns a file channel associated with the null device, open in write mode.
   Write operations on this file channel have no effect. *)
BEGIN
  RETURN FilePtrIO.defaultNullFile
END defaultNullFile;

END FileIO.