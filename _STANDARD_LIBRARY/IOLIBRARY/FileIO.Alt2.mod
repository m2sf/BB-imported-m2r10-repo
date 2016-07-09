(* (C) 2016 by B.Kowarsch & R.Sutcliffe. All rights reserved. *)

IMPLEMENTATION MODULE FileIO;

(* File Specific Channel Based IO *)

(* File Channel Operations Vector *)

VAR opVector : ChanIO.Operations;

(* Inspection *)

PROCEDURE isFileChannel ( chan : ChanIO.Channel ) : BOOLEAN;
(* Returns TRUE if <chan> is a valid file channel, else FALSE. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    RETURN TRUE
  ELSE
    RETURN FALSE
  END
END isFileChannel;

PROCEDURE isValidFile ( file : File ) : BOOLEAN;
(* Returns TRUE if the file accessor of <file> is valid, else FALSE. *)
BEGIN
  RETURN FilePtrIO.isValidAccessor(file^.handle)
END isValidFile;


(* Opening and Closing a File *)

CONST DefaultBufferSize = FilePtrIO.DefaultBufferSize;
(* The buffer size used when no buffer size is specified. *)

PROCEDURE Open
  ( NEW file       : File;
    CONST filename : ARRAY OF CHAR;
    mode           : FileMode;
    VAR status     : IOStatus );
(* Opens file <filename> in file mode <mode>.  On success, a  newly allocated
   and initialised file channel is passed back in <file>.  On failure, NIL is
   passed back in <file>.  A status code is passed back in <status>. *)
VAR newFile;
BEGIN
  (* allocate new channel *)
  NEW newFile;
  IF newFile = NIL THEN
    status := {TRUE, AllocationFailed};
    RETURN
  END;
  
  (* open file *)
  FilePtrIO.Open(newFile, filename, mode, status);
  IF status.failed THEN
    RELEASE newFile;
    RETURN;
  END;
    
  (* install channel type specific methods *)
  newFile^.op := opVector;
  
  (* pass new channel *)
  file := newFile;
  RETURN
END Open;

PROCEDURE OpenWithBufferSize
  ( NEW file       : File;
    CONST filename : ARRAY OF CHAR;
    mode           : FileMode;
    bufSize        : IOSIZE;
    VAR status     : IOStatus );
(* Opens the file <filename> in file mode <mode> using an internal file buffer
   of size <bufSize>  and passes a file accessor back in <file>.  If <bufSize>
   is less than MinBufferSize, value MinBufferSize is used instead. Passes NIL
   back in <file> if unsuccessful. Sets the file position depending on <mode>.
   The status is passed back in <status>. *)
BEGIN
  (* TO DO *)
END OpenWithBufferSize;

PROCEDURE ReOpen ( file : File; mode : FileMode );
(* Flushes <file> and changes its access mode to <mode>.
   The file position is set depending on <mode>. *)
BEGIN
  (* TO DO *)
END ReOpen;

(* Opening or reopening a file in a mode  whose append flag is not set  causes
   the file position  to be set  to the beginning of the file.  Opening or re-
   opening a file in a mode whose append flag is set  causes the file position
   to be set  to the end of the file.  An attempt to open a file that is a di-
   rectory fails with status code MayNotOpenDirectory. *)

PROCEDURE Close ( VAR file : File; VAR status : IOStatus );
(* Performs Flush on <file>,  closes  the associated file,  deallocates <file>
   and passes NIL back in <file>. A status code is passed back in <status>. *)
BEGIN
  FilePtrIO.Close(file^.handle, status)
END Close;


(* Status operations *)

PROCEDURE statusOf ( chan : IOChan.Channel ) : IOStatus;
(* Returns the status of the most recent operation on <chan>. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    RETURN FilePtrIO.statusOf(chan^.handle)
  ELSE
    RETURN {TRUE, InvalidChannelType}
  END
END statusOf;

PROCEDURE StatusMsg ( chan : IOChan.Channel; status : IOStatus );
(* Writes a status message for <status> to <chan>. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    (* TO DO *)
  ELSE
    (* invalid channel type *)
  END
END StatusMsg;

PROCEDURE SetStatus
  ( chan : ChanIO.Channel; status : IOStatus; VAR valid : BOOLEAN );
(* Sets  the status of <file> to <status> if it meets the following condition:
   Where <status.code> is set to Success <status.failed> must be FALSE, other-
   wise TRUE.  Passes TRUE in <valid> if successful, otherwise FALSE. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    FilePtrIO.SetStatus(chan^.handle, status, valid)
  ELSE
    (* invalid channel type *)
  END
END SetStatus;


(* Querying File Parameters *)

PROCEDURE modeOf ( chan : ChanIO.Channel ) : FileMode;
(* Returns the file access mode of <chan>. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    RETURN FilePtrIO.modeOf(chan)
  ELSE
    (* invalid channel type *)
  END
END modeOf;

PROCEDURE nameLen ( chan : ChanIO.Channel ) : CARDINAL;
(* Returns the length of the filename associated with <chan>. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    RETURN FilePtrIO.nameLen(chan^.handle)
  ELSE
    (* invalid channel type *)
  END
END nameLen;

PROCEDURE GetName ( chan : ChanIO.Channel; VAR filename : ARRAY OF CHAR );
(* Passes  the name of the file associated with <chan> back in <filename>.  If
   the name exceeds the capacity of <filename>, an empty string is passed back
   in <filename>.  Does not alter the status of <chan>. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    FilePtrIO.GetName(chan^.handle, filename)
  ELSE
    (* invalid channel type *)
  END
END GetName;


(* File Positioning *)

TYPE Size = ALIAS OF FilePtrIO.Size;

TYPE Pos = ALIAS OF FilePtrIO.Pos;

PROCEDURE currentPos ( file : File ) : Pos;
(* Returns the current read/write position of <file>. *)
BEGIN
  RETURN FilePtrIO.currentPos(file^.handle)
END currentPos;

PROCEDURE isValidPos ( file : File; pos : Pos ) : BOOLEAN;
(* Returns TRUE if <pos> is a valid position for <file>, else FALSE. *)
BEGIN
  RETURN FilePtrIO.isValidPos(file^.handle, pos)
END isValidPos;

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

PROCEDURE eof ( file : File ) : BOOLEAN; (* for convenience *)
(* Returns TRUE if the end of <file> has been reached, otherwise FALSE. *)
BEGIN
  RETURN FilePtrIO.eof(file^.handle)
END eof;

PROCEDURE dataReady ( chan : ChanIO.Channel; octets : IOSIZE ) : BOOLEAN;
(* Returns TRUE if <chan> has at least <octets> number of octets available for
   reading, otherwise FALSE. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    FilePtrIO.dataReady(chan^.handle, octets)
  ELSE
    (* invalid channel type *)
  END
END dataReady;

PROCEDURE ReadOctet ( chan : ChanIO.Channel; VAR data : OCTET );
(* Reads one octet of data  at the current position of <chan>,  passes it back
   in <data> and advances the current read/write position of <chan> by one. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    FilePtrIO.ReadOctet(chan^.handle, data)
  ELSE
    (* invalid channel type *)
  END
END ReadOctet;

PROCEDURE ReadBlock
  ( chan : ChanIO.Channel; VAR data : ARRAY OF OCTET );
(* Reads a block of data  at the current position of <chan>.  Passes the block
   of data back in <data>  and  advances  the  current  read/write position of
   <chan> by the number of octets read. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    FilePtrIO.ReadBlock(chan^.handle, data)
  ELSE
    (* invalid channel type *)
  END
END ReadBlock;

CONST
  InsertBufferSize = FilePtrIO.InsertBufferSize;

PROCEDURE Insert ( chan : ChanIO.Channel; data : OCTET );
(* Inserts octet <data> into input channel <chan>  to be read by the next read
   operation on <chan>.  Inserted octets are stored in a  FIFO buffer  of size
   InsertBufferSize.  The minimum size of the insert buffer is two octets. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    FilePtrIO.Insert(chan^.handle, data)
  ELSE
    (* invalid channel type *)
  END
END Insert;

(* Any attempt to perform the above I/O operations on any file whose Read flag
   is not set will fail with status NotOpenForReading. *)

PROCEDURE WriteOctet ( chan : ChanIO.Channel; data : OCTET );
(* Performs a write or append operation depending on the access mode of <chan>
   and advances the current read/write position by one after the data has been
   written. In write mode <data> is written at the current position of <chan>.
   In append mode the procedure  atomically  sets the current read/write posi-
   tion to the end of the file and appends <data> to the end of <chan>. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    FilePtrIO.WriteOctet(chan^.handle, data)
  ELSE
    (* invalid channel type *)
  END
END WriteOctet;

PROCEDURE WriteBlock
  ( chan : ChanIO.Channel; data : BARE ARRAY OF OCTET; VAR written : IOSIZE );
(* Performs a write or append operation depending on the access mode of <chan>
   and advances the current read/write position by the actual number of octets
   written which is passed back in <written>.  In write mode <data> is written
   at the current read/write position of <chan>.  In append mode the procedure
   atomically sets the current read/write position to the end of the file  and
   appends <data> to the end of <chan>. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    FilePtrIO.WriteBlock(chan^.handle, data, written)
  ELSE
    (* invalid channel type *)
  END
END WriteBlock;

PROCEDURE isBuffered ( chan : ChanIO.Channel ) : BOOLEAN;
(* Returns TRUE if <chan> is buffered, otherwise FALSE. *)
BEGIN
  RETURN TRUE
END isBuffered;

PROCEDURE Flush ( chan : ChanIO.Channel );
(* Writes unwritten data in any buffer of <chan> to its associated file. *)
BEGIN
  CASE chan OF
  | FileIO.File :
    FilePtrIO.Flush(chan^.handle)
  ELSE
    (* invalid channel type *)
  END
END Flush;

(* Any attempt  to write to  or flush a file whose Write flag is not set shall
   fail with status NotOpenForWriting. *)

BEGIN
  NEW opVector;
  opVector^.statusOf := statusOf;
  opVector^.StatusMsg := StatusMsg;
  opVector^.SetStatus := SetStatus;
  opVector^.modeOf := modeOf;
  opVector^.nameLen := nameLen;
  opVector^.GetName := GetName;
  opVector^.dataReady := dataReady;
  opVector^.ReadOctet := ReadOctet;
  opVector^.ReadBlock := ReadBlock;
  opVector^.Insert := Insert;
  opVector^.WriteOctet := WriteOctet;
  opVector^.WriteBlock := WriteBlock;
  opVector^.isBuffered := isBuffered;
  opVector^.Flush := Flush
END FileIO.