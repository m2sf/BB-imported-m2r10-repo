(* (C) 2016 by B.Kowarsch & R.Sutcliffe. All rights reserved. *)

IMPLEMENTATION MODULE ChanIO;

(* Generic Channel Operations *)

PROCEDURE statusOf ( chan : Channel ) : IOStatus;
(* Returns the status of the most recent operation on <chan>. *)
BEGIN
  RETURN chan^.op^.chanOf(chan)
END statusOf;

PROCEDURE StatusMsg ( chan : Channel; status : IOStatus );
(* Writes a status message for <status> to <chan>. *)
BEGIN
  chan^.op^.StatusMsg(chan, status)
END StatusMsg;

PROCEDURE SetStatus
  ( chan : Channel; status : IOStatus; VAR valid : BOOLEAN );
(* Sets  the status of <chan> to <status> if it meets the following condition:
   Where <status.code> is Success,  <status.failed> must be FALSE,  else TRUE.
   If the condition is met, TRUE is passed back in <valid>, else FALSE. *)
BEGIN
  chan^.op^.SetStatus(chan, status, valid)
END SetStatus;

PROCEDURE modeOf ( chan : Channel ) : FileMode;
(* Returns the mode of <chan>. *)
BEGIN
  RETURN chan^.op^.modeOf(chan)
END modeOf;

PROCEDURE nameLen ( chan : Channel ) : LONGCARD;
(* Returns the length of the name of <chan>. *)
BEGIN
  chan^.op^.nameLen(chan)
END nameLen;

PROCEDURE GetName ( chan : Channel; VAR name : ARRAY OF CHAR );
(* Passes the name of <chan> back in <name>  or empty string if the length of
   the name exceeds the capacity of <name>.  Does not alter status. *)
BEGIN
  chan^.op^.GetName(chan, name)
END GetName;

PROCEDURE dataReady ( chan : Channel; octets : IOSIZE ) : BOOLEAN;
(* Returns TRUE if <chan> has at least <octets> number of octets available for
   reading, otherwise FALSE. *)
BEGIN
  RETURN chan^.op^.dataReady(chan, octets)
END dataReady;

PROCEDURE ReadOctet ( chan : Channel; VAR data : OCTET );
(* Reads one octet from <chan> and passes it back in <data>. *)
BEGIN
  chan^.op^.ReadOctet(chan, data)
END ReadOctet;

PROCEDURE ReadBlock ( chan : Channel; VAR data : ARRAY OF OCTET );
(* Reads a block of data up to the capacity of <data>  from <chan>  and passes
   it back in <data>. *)
BEGIN
  chan^.op^.ReadBlock(chan, data)
END ReadBlock;

PROCEDURE Insert ( chan : Channel; data : OCTET );
(* Inserts <data> into <chan> to be read by the next read operation. *)
BEGIN
  chan^.op^.Insert(chan, data)
END Insert;

PROCEDURE WriteOctet ( chan : Channel; data : OCTET );
(* Writes the octet passed in <data> to <chan>. *)
BEGIN
  chan^.op^.WriteOctet(chan, data)
END WriteOctet;

PROCEDURE WriteBlock
  ( chan : Channel; data : ARRAY OF OCTET; VAR written : IOSIZE );
(* Writes a block of data passed in <data> to <chan> and passes the number of
   octets written back in <written>. *)
BEGIN
  chan^.op^.WriteBlock(chan, data, written)
END WriteBlock;

PROCEDURE isBuffered ( chan : Channel ) : BOOLEAN;
(* Returns TRUE if <chan> is buffered, otherwise FALSE. *)
BEGIN
  RETURN chan^.op^.isBuffered(chan)
END isBuffered;

PROCEDURE Flush ( chan : Channel );
(* If <chan> is buffered, writes any unwritten buffer data to <chan>. *)
BEGIN
  chan^.op^.Flush(chan)
END Flush;

END ChanIO.