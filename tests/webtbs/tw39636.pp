{ %target=linux,darwin,freebsd,netbsd,openbsd,dragonfly }
program tw39636;

{$mode objfpc}{$h+}
{$I-}

uses
  BaseUnix, Unix;

const
  cSize = 1 shl 20;
  cSignals = 40;

var
  gSignals: Integer = 0;
  gTextBuf: array[0..cSize-1] of AnsiChar;

// Counts the received signals
procedure Handler(aSig: cint); cdecl;

begin
  Inc(gSignals);
end;


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    Writeln('check ',aCode,' failed: ',aMsg);
    Halt(aCode);
    end;
end;


// Returns the expected byte at offset aPos
function PatternAt(aPos: Integer): AnsiChar;

begin
  Result:=AnsiChar(Ord('A')+aPos mod 26);
end;


// Signals aParent repeatedly, then reads aHandle to the end and exits with 0 when all data arrived intact
procedure RunReader(aHandle: cint; aParent: TPid);

var
  lBuf: array[0..4095] of AnsiChar;
  lWait: TTimeSpec;
  lTotal, lRead, i: Integer;

begin
  lWait.tv_sec:=0;
  lWait.tv_nsec:=5000000;
  for i:=1 to cSignals do
    begin
    FpKill(aParent,SIGUSR1);
    FpNanoSleep(@lWait,nil);
    end;
  lTotal:=0;
  repeat
    lRead:=FpRead(aHandle,lBuf,SizeOf(lBuf));
    if (lRead<0) and (fpgeterrno=ESysEINTR) then
      continue;
    for i:=0 to lRead-1 do
      if lBuf[i]<>PatternAt(lTotal+i) then
        FpExit(2);
    if lRead>0 then
      Inc(lTotal,lRead);
  until lRead<=0;
  if lTotal<>cSize then
    FpExit(3);
  FpExit(0);
end;


// Waits for aPid and returns its exit status
function WaitChild(aPid: TPid): cint;

var
  lStatus: cint;

begin
  while FpWaitPid(aPid,@lStatus,0)<>aPid do
    Check(90,fpgeterrno=ESysEINTR,'waitpid');
  Check(91,WIFEXITED(lStatus),'reader exited normally');
  Result:=WEXITSTATUS(lStatus);
end;


// Writes cSize bytes with BlockWrite to a pipe while signals arrive
procedure TestBlockWrite;

var
  lIn, lOut: File;
  lData: array of AnsiChar;
  lPid: TPid;
  lWritten, lRes, i: Integer;

begin
  Check(10,AssignPipe(lIn,lOut)=0,'AssignPipe');
  SetLength(lData,cSize);
  for i:=0 to cSize-1 do
    lData[i]:=PatternAt(i);
  lPid:=FpFork;
  if lPid=0 then
    begin
    FpClose(FileRec(lOut).Handle);
    RunReader(FileRec(lIn).Handle,FpGetPPid);
    end;
  Check(11,lPid>0,'fork');
  FpClose(FileRec(lIn).Handle);
  gSignals:=0;
  BlockWrite(lOut,lData[0],cSize,lWritten);
  lRes:=IOResult;
  Close(lOut);
  Check(12,lRes=0,'BlockWrite IOResult');
  Check(13,lWritten=cSize,'BlockWrite wrote all bytes');
  Check(14,WaitChild(lPid)=0,'reader received all bytes intact');
  Check(15,gSignals>0,'signals received during BlockWrite');
end;


// Writes cSize bytes through a standard text file buffer to a pipe while signals arrive
procedure TestTextWrite;

var
  lOut: Text;
  lIn, lPipeOut: cint;
  lData: AnsiString;
  lPid: TPid;
  lRes, i: Integer;

begin
  Check(20,AssignPipe(lIn,lPipeOut)=0,'AssignPipe');
  Assign(lOut,'');
  Rewrite(lOut);
  // Standard output text file writing to the pipe instead of handle 1
  TextRec(lOut).Handle:=lPipeOut;
  SetTextBuf(lOut,gTextBuf,SizeOf(gTextBuf));
  SetLength(lData,cSize);
  for i:=1 to cSize do
    lData[i]:=PatternAt(i-1);
  lPid:=FpFork;
  if lPid=0 then
    begin
    FpClose(lPipeOut);
    RunReader(lIn,FpGetPPid);
    end;
  Check(21,lPid>0,'fork');
  FpClose(lIn);
  gSignals:=0;
  Write(lOut,lData);
  Flush(lOut);
  lRes:=IOResult;
  Close(lOut);
  Check(22,lRes=0,'Write/Flush IOResult');
  Check(23,WaitChild(lPid)=0,'reader received all bytes intact');
  Check(24,gSignals>0,'signals received during Write');
end;


// Writes cSize bytes through an AssignPipe text file while signals arrive
procedure TestPipeTextWrite;

var
  lIn, lOut: Text;
  lData: AnsiString;
  lPid: TPid;
  lRes, i: Integer;

begin
  Check(30,AssignPipe(lIn,lOut)=0,'AssignPipe');
  SetTextBuf(lOut,gTextBuf,SizeOf(gTextBuf));
  SetLength(lData,cSize);
  for i:=1 to cSize do
    lData[i]:=PatternAt(i-1);
  lPid:=FpFork;
  if lPid=0 then
    begin
    FpClose(TextRec(lOut).Handle);
    RunReader(TextRec(lIn).Handle,FpGetPPid);
    end;
  Check(31,lPid>0,'fork');
  FpClose(TextRec(lIn).Handle);
  gSignals:=0;
  Write(lOut,lData);
  Flush(lOut);
  lRes:=IOResult;
  Close(lOut);
  Check(32,lRes=0,'pipe Write/Flush IOResult');
  Check(33,WaitChild(lPid)=0,'reader received all bytes intact');
  Check(34,gSignals>0,'signals received during pipe Write');
end;


var
  lAct: SigActionRec;

begin
  FillChar(lAct,SizeOf(lAct),0);
  lAct.sa_handler:=SigActionHandler(@Handler);
  Check(1,FpSigAction(SIGUSR1,@lAct,nil)=0,'install handler');
  TestBlockWrite;
  TestTextWrite;
  TestPipeTextWrite;
  Writeln('ok');
end.
