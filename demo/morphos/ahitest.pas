{
    $Id: ahitest.pas,v 1.2 2005/02/14 17:13:10 peter Exp $

    Using AHI device interface to produce sound
    Free Pascal for MorphOS example

    Copyright (C) 2005 by Karoly Balogh

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ * 2005.01.30 * }
{ * Needs MorphOS RTL 2005.01.30 or later! * }

program AHITest;

uses exec,doslib,utility,ahi; // AHI SUXX! :)


const
  FREQUENCY  = 44100;
  STYPE      = AHIST_M16S;
  BUFFERSIZE = 8192;

var
  myTask: PTask;
  oldPri: LongInt;

const
  AHImp : PMsgPort = nil;
  AHIios: Array[0..1] of PAHIRequest = (nil,nil);
  AHIio : PAHIRequest = nil;
  AHIiocopy: Pointer = nil;
  AHIdevice: ShortInt = -1;

  signals: DWord = 0;
  length : DWord = 0;

  link: PAHIRequest = nil;
  tmp : Pointer = nil;

  terminate: Boolean = False;

var
  { * Not an elegant way of buffer allocation, but i don't care. * }
  Buffer1: array[1..BUFFERSIZE] of Integer;
  Buffer2: array[1..BUFFERSIZE] of Integer;
  PB1, PB2: PInteger;

  IOErrCode: LongInt;


procedure cleanup(exitmsg: String; exitcode: LongInt);
begin
  if AHIdevice=0 then CloseDevice(PIORequest(AHIio));
  DeleteIORequest(PIORequest(AHIio));
  FreeMem(AHIiocopy);
  DeleteMsgPort(AHImp);
  SetTaskPri(myTask,oldPri);

  if exitmsg<>'' then writeln(exitmsg);
  halt(exitcode);
end;


{ * Fill up the buffer with some sound data * }
procedure fillbuffer;
var
  counter, counter2: longint;
  sndvalue: integer;
  chunksize: longint;
  chunknum : longint;
begin
  sndvalue:=32767;
  chunknum :=BUFFERSIZE div 32;
  chunksize:=BUFFERSIZE div chunknum;
  for counter:=1 to chunknum do begin
    for counter2:=1 to chunksize do
      pb1[(((counter-1)*chunksize)+counter2)-1]:=sndvalue;
    sndvalue:=0-sndvalue;
  end;
  length:=(BUFFERSIZE*2);
end;


begin
  PB1:=@Buffer1;
  PB2:=@Buffer2;

  myTask:=FindTask(nil);
  oldPri:=SetTaskPri(myTask,10);

  AHImp:=CreateMsgPort();
  if AHImp<>nil then begin
    AHIio:=CreateIORequest(AHImp,sizeof(TAHIRequest));
    if AHIio<>nil then begin
      AHIio^.ahir_Version:=4;
      AHIdevice:=OpenDevice(AHINAME,0,PIORequest(AHIio),0);
    end;
  end;

  if AHIdevice<>0 then
    cleanup('AHI opening error!',20);

  { * Make a copy of the request (for double buffering) * }
  AHIiocopy:=getmem(sizeof(TAHIRequest));
  if AHIiocopy=nil then
    cleanup('Memory allocation failure.',20);

  CopyMem(AHIio, AHIiocopy, sizeof(TAHIRequest));
  AHIios[0]:=AHIio;
  AHIios[1]:=AHIiocopy;

  writeln('Press CTRL-C to exit...');
  SetIoErr(0);

  while (not terminate) do begin

    { * Let's fill up the buffer with some data * }
    fillbuffer;

    { * Setting up IO request * }
    AHIios[0]^.ahir_Std.io_Message.mn_Node.ln_Pri := 127;
    AHIios[0]^.ahir_Std.io_Command := CMD_WRITE;
    AHIios[0]^.ahir_Std.io_Data    := pb1;
    AHIios[0]^.ahir_Std.io_Length  := length;
    AHIios[0]^.ahir_Std.io_Offset  := 0;
    AHIios[0]^.ahir_Frequency      := FREQUENCY;
    AHIios[0]^.ahir_Type           := STYPE;
    AHIios[0]^.ahir_Volume         := $10000;          { * Full volume * }
    AHIios[0]^.ahir_Position       := $8000;           { * Centered    * }
    AHIios[0]^.ahir_Link           := link;

    SendIO(PIORequest(AHIios[0]));

    if link<>nil then begin
      { * Wait until the last buffer is finished * }
      { * (== the new buffer is started) * }
      signals:=Wait(SIGBREAKF_CTRL_C Or (1 Shl AHImp^.mp_SigBit));

      { * Check for Ctrl-C and abort if pressed * }
      if (signals and SIGBREAKF_CTRL_C)>0 then begin
        SetIoErr(ERROR_BREAK);
        terminate:=True;
      end;

      { * Remove the reply and abort on error * }
      if (WaitIO(PIORequest(link)))<>0 then begin
        SetIoErr(ERROR_WRITE_PROTECTED);
        terminate:=True;
      end;
    end;

    link := AHIios[0];

    { * Swap buffer and request pointers, and restart * }
    tmp := pb1;
    pb1 := pb2;
    pb2 := tmp;

    tmp := AHIios[0];
    AHIios[0] := AHIios[1];
    AHIios[1] := tmp;
  end;

  { * Abort any pending IO requests * }
  AbortIO(PIORequest(AHIios[0]));
  WaitIO(PIORequest(AHIios[0]));

  if (link<>nil) then begin
    { * Only if the second request was started * }
    AbortIO(PIORequest(AHIios[1]));
    WaitIO(PIORequest(AHIios[1]));
  end;

  IOErrCode:=IoErr();
  if (IOErrCode<>0) and (IOErrCode<>ERROR_BREAK) then
    cleanup('Device I/O error.',20)
  else
    cleanup('',0);
end.

{
  $Log: ahitest.pas,v $
  Revision 1.2  2005/02/14 17:13:10  peter
    * truncate log

  Revision 1.1  2005/01/30 20:03:43  karoly
    * initial revision

}
