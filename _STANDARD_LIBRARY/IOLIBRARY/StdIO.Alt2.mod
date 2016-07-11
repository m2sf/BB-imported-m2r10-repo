(* (C) 2016 by B.Kowarsch & R.Sutcliffe. All rights reserved. *)

IMPLEMENTATION MODULE StdIO;

(* Channel Based Standard IO Streams *)

VAR opVector : ChanIO.Operations;

(* Inspection *)

PROCEDURE isStdIOChannel ( chan : ChanIO.Channel ) : BOOLEAN;
(* Returns TRUE if <chan> is a valid StdIO channel, else FALSE. *)
BEGIN
  CASE chan OF
  | Stream :
    RETURN TRUE
  ELSE
    RETURN FALSE
  END
END isStdIOChannel;

PROCEDURE isValidStream ( stream : Stream ) : BOOLEAN;
(* Returns TRUE if the stream accessor of <stream> is valid, else FALSE. *)
BEGIN
  CASE chan OF
  | Stream :
    RETURN FileDescIO.isValidFile(stream^.handle)
  ELSE
    (* invalid channel type *)
  END
END isValidStream;


(* StdIO Channels *)

VAR defaultIn, defaultOut, defaultErr : Stream;

PROCEDURE in : Stream;
(* Returns a StdIO stream channel associated with stdin *)
BEGIN
  RETURN defaultIn
END in;

PROCEDURE out : Stream;
(* Returns a StdIO stream channel associated with stdout *)
BEGIN
  RETURN defaultOut
END out;

PROCEDURE err : Stream;
(* Returns a StdIO stream channel associated with stderr *)
BEGIN
  RETURN defaultErr
END err;


(* Status operations *)

PROCEDURE statusOf ( chan : IOChan.Channel ) : IOStatus;
(* Returns the status of the most recent operation on <chan>. *)
BEGIN
  CASE chan OF
  | Stream :
    RETURN FileDescIO.statusOf(chan^.handle)
  ELSE
    (* invalid channel type *)
  END
END statusOf;

PROCEDURE StatusMsg ( chan : IOChan.Channel; status : IOStatus );
(* Writes a status message for <status> to <chan>. *)
BEGIN
  CASE chan OF
  | Stream :
    FileDescIO.StatusMsg(chan^.handle, status)
  ELSE
    (* invalid channel type *)
  END
END StatusMsg;

PROCEDURE SetStatus
  ( chan : ChanIO.Channel; status : IOStatus; VAR valid : BOOLEAN );
(* Sets  the status of <chan> to <status> if it meets the following condition:
   Where <status.code> is set to Success <status.failed> must be FALSE, other-
   wise TRUE.  Passes TRUE in <valid> if successful, otherwise FALSE. *)
BEGIN
  CASE chan OF
  | Stream :
    FileDescIO.SetStatus(chan^.handle, status)
  ELSE
    (* invalid channel type *)
  END
END SetStatus;


(* Querying Stream Parameters *)

PROCEDURE modeOf ( chan : ChanIO.Channel ) : FileMode;
(* Returns the access mode of <chan>. *)
BEGIN
  CASE chan OF
  | Stream :
    RETURN FileDescIO.modeOf(chan^.handle)
  ELSE
    (* invalid channel type *)
  END
END modeOf;

PROCEDURE nameLen ( chan : ChanIO.Channel ) : CARDINAL;
(* Returns the length of the name associated with <chan>. *)
BEGIN
  CASE chan OF
  | Stream :
    RETURN FileDescIO.nameLen(chan^.handle)
  ELSE
    (* invalid channel type *)
  END
END nameLen;

PROCEDURE GetName ( chan : ChanIO.Channel; VAR name : ARRAY OF CHAR );
(* Passes the name of the stream associated with <chan> back in <name>. If the
   name exceeds  the capacity of <name>,  an empty string is passed in <name>.
   Does not alter the status of <chan>. *)
BEGIN
  CASE chan OF
  | Stream :
    FileDescIO.GetName(chan^.handle, name)
  ELSE
    (* invalid channel type *)
  END
END GetName;


(* I/O operations *)

PROCEDURE dataReady ( chan : ChanIO.Channel; octets : IOSIZE ) : BOOLEAN;
(* Returns TRUE if <chan> has at least <octets> number of octets available for
   reading, otherwise FALSE. *)
BEGIN
  CASE chan OF
  | Stream :
    RETURN FileDescIO.dataReady(chan^.handle, octets)
  ELSE
    (* invalid channel type *)
  END
END dataReady;

PROCEDURE ReadOctet ( chan : ChanIO.Channel; VAR data : OCTET );
(* Reads one octet of data from <chan> and passes it back in <data>. *)
BEGIN
  CASE chan OF
  | Stream :
    FileDescIO.ReadOctet(chan^.handle, data)
  ELSE
    (* invalid channel type *)
  END
END ReadOctet;

PROCEDURE ReadBlock
  ( chan : ChanIO.Channel; VAR data : ARRAY OF OCTET );
(* Reads a block of data from <chan> and passes it back in <data>. *)
BEGIN
  CASE chan OF
  | Stream :
    FileDescIO.ReadBlock(chan^.handle, data)
  ELSE
    (* invalid channel type *)
  END
END ReadBlock;

PROCEDURE insertReady ( chan : ChanIO.Channel ) : BOOLEAN;
(* Returns FALSE if the insert buffer of <chan> is full, otherwise TRUE. *)
BEGIN
  CASE chan OF
  | Stream :
    RETURN FileDescIO.insertReady(chan^.handle)
  ELSE
    (* invalid channel type *)
  END
END insertReady;

PROCEDURE Insert ( chan : ChanIO.Channel; data : OCTET );
(* Inserts octet <data> into input channel <chan>  to be read by the next read
   operation on <chan>. *)
BEGIN
  CASE chan OF
  | Stream :
    FileDescIO.Insert(chan^.handle, data)
  ELSE
    (* invalid channel type *)
  END
END Insert;

(* Any attempt to perform the above I/O operations on a stream whose Read flag
   is not set will fail with status NotOpenForReading. *)

PROCEDURE WriteOctet ( chan : ChanIO.Channel; data : OCTET );
(* Writes <data> to <chan>. *)
BEGIN
  CASE chan OF
  | Stream :
    FileDescIO.WriteOctet(chan^.handle, data)
  ELSE
    (* invalid channel type *)
  END
END WriteOctet;

PROCEDURE WriteBlock
  ( chan : ChanIO.Channel; data : BARE ARRAY OF OCTET; VAR written : IOSIZE );
(* Writes <data> to <chan> and passes the actual number of octets written back
   in <written>. *)
BEGIN
  CASE chan OF
  | Stream :
    FileDescIO.WriteBlock(chan^.handle, data, written)
  ELSE
    (* invalid channel type *)
  END
END WriteBlock;

(* Any attempt to perform the above I/O operations on a stream whose Write flag
   is not set will fail with status NotOpenForWriting. *)

PROCEDURE isFlushable ( chan : ChanIO.Channel ) : BOOLEAN;
(* Always returns FALSE. *)
BEGIN
  CASE chan OF
  | Stream :
    RETURN FALSE
  ELSE
    (* invalid channel type *)
  END
END isFlushable;

PROCEDURE Flush ( chan : ChanIO.Channel );
(* Always fails with status OperationNotSupported. *)
BEGIN
  CASE chan OF
  | Stream :
    FileDescIO.SetStatus(chan, {TRUE, IOStatus.OperationNotSupported})
  ELSE
    (* invalid channel type *)
  END
END Flush;

BEGIN
  (* install methods *)
  NEW opVector;
  opVector^.statusOf := statusOf;
  opVector^.StatusMsg := StatusMsg;
  opVector^.SetStatus := SetStatus;
  opVector^.statusOf := statusOf;
  opVector^.modeOf := modeOf;
  opVector^.nameLen := NameLen;
  opVector^.GetName := GetName;
  opVector^.dataReady := dataReady;
  opVector^.ReadOctet := ReadOctet;
  opVector^.ReadBlock := ReadBlock;
  opVector^.insertReady := insertReady;
  opVector^.Insert := Insert;
  opVector^.WriteOctet := WriteOctet;
  opVector^.WriteBlock := WriteBlock;
  opVector^.isFlushable := isFlushable;
  opVector^.Flush := Flush;
  
  (* init stdin channel *)
  NEW defaultIn;
  defaultIn^.op := opVector;
  defaultIn^.handle := FileDescIO.defaultInFile();
  
  (* init stdout channel *)
  NEW defaultOut;
  defaultOut^.op := opVector;
  defaultOut^.handle := FileDescIO.defaultOutFile();
  
  (* init stderr channel *)
  NEW defaultErr;
  defaultErr^.op := opVector;
  defaultErr^.handle := FileDescIO.defaultErrFile();
END StdIO.