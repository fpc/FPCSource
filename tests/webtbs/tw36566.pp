{ %TARGET=linux }
program tw36566;

{$mode objfpc}{$H+}

uses
  cthreads, BaseUnix, SysUtils, Classes, Serial;

function posix_openpt(aFlags: cint): cint; cdecl; external 'c';
function grantpt(aFD: cint): cint; cdecl; external 'c';
function unlockpt(aFD: cint): cint; cdecl; external 'c';
function ptsname(aFD: cint): PAnsiChar; cdecl; external 'c';

const
  C_TIMEOUT = 500;
  C_MAX_ELAPSED = 3*C_TIMEOUT;

type
  { Writes data to a pty master after a delay, then optionally closes it }
  TDelayedWriter = class(TThread)
  private
    FMaster: cint;
    FData: string;
    FHangup: Boolean;
  protected
    procedure Execute; override;
  public
    // Creates the writer for aMaster, started immediately
    constructor Create(aMaster: cint; const aData: string; aHangup: Boolean);
  end;

var
  lErrors: Integer;

constructor TDelayedWriter.Create(aMaster: cint; const aData: string; aHangup: Boolean);

begin
  FMaster:=aMaster;
  FData:=aData;
  FHangup:=aHangup;
  inherited Create(False);
end;


procedure TDelayedWriter.Execute;

begin
  Sleep(100);
  fpWrite(FMaster, FData[1], Length(FData));
  if FHangup then
    begin
      Sleep(100);
      fpClose(FMaster);
    end;
end;


{ Reports a failed check }
procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
      writeln('FAIL: ', aMsg);
      Flush(Output);
      Inc(lErrors);
    end;
end;


{ Opens a pty pair, returns the master and the slave opened as a raw serial port }
procedure OpenPty(out aMaster: cint; out aSlave: TSerialHandle);

begin
  aMaster:=posix_openpt(O_RDWR or O_NOCTTY);
  if (aMaster<0) or (grantpt(aMaster)<>0) or (unlockpt(aMaster)<>0) then
    begin
      writeln('Cannot create pseudo terminal');
      Halt(2);
    end;
  aSlave:=SerOpen(ptsname(aMaster));
  if aSlave<=0 then
    begin
      writeln('Cannot open pseudo terminal slave');
      Halt(2);
    end;
  SerSetParams(aSlave, 9600, 8, NoneParity, 1, []);
end;


{ Calls SerReadTimeout with C_TIMEOUT and returns the elapsed milliseconds in aElapsed }
function TimedRead(aSlave: TSerialHandle; var aBuf: array of byte; aCount: LongInt; out aElapsed: QWord): LongInt;

var
  lStart: QWord;

begin
  lStart:=GetTickCount64;
  Result:=SerReadTimeout(aSlave, aBuf, aCount, C_TIMEOUT);
  aElapsed:=GetTickCount64-lStart;
end;


{ Hangup during the read returns the data received before it }
procedure TestHangupAfterData;

var
  lMaster: cint;
  lSlave: TSerialHandle;
  lBuf: array[0..9] of byte;
  lRes: LongInt;
  lElapsed: QWord;
  lWriter: TDelayedWriter;

begin
  OpenPty(lMaster, lSlave);
  lWriter:=TDelayedWriter.Create(lMaster, 'abc', True);
  FillChar(lBuf, SizeOf(lBuf), 0);
  lRes:=TimedRead(lSlave, lBuf, 10, lElapsed);
  lWriter.WaitFor;
  lWriter.Free;
  Check(lRes=3, 'Hangup after data returns the 3 received bytes, got '+IntToStr(lRes));
  Check((lBuf[0]=Ord('a')) and (lBuf[1]=Ord('b')) and (lBuf[2]=Ord('c')), 'Hangup after data returns the bytes abc');
  Check(lElapsed<C_MAX_ELAPSED, 'Hangup after data returns within the timeout, took '+IntToStr(lElapsed)+' ms');
  SerClose(lSlave);
end;


{ Hangup without data returns 0 or -1 }
procedure TestHangupWithoutData;

var
  lMaster: cint;
  lSlave: TSerialHandle;
  lBuf: array[0..9] of byte;
  lRes: LongInt;
  lElapsed: QWord;

begin
  OpenPty(lMaster, lSlave);
  fpClose(lMaster);
  lRes:=TimedRead(lSlave, lBuf, 10, lElapsed);
  Check(lRes<=0, 'Hangup without data returns 0 or -1, got '+IntToStr(lRes));
  Check(lElapsed<C_MAX_ELAPSED, 'Hangup without data returns within the timeout, took '+IntToStr(lElapsed)+' ms');
  SerClose(lSlave);
end;


{ Data arriving in two parts is collected into one result }
procedure TestSplitData;

var
  lMaster: cint;
  lSlave: TSerialHandle;
  lBuf: array[0..9] of byte;
  lRes: LongInt;
  lElapsed: QWord;
  lWriter: TDelayedWriter;

begin
  OpenPty(lMaster, lSlave);
  fpWrite(lMaster, 'ab', 2);
  lWriter:=TDelayedWriter.Create(lMaster, 'cd', False);
  FillChar(lBuf, SizeOf(lBuf), 0);
  lRes:=TimedRead(lSlave, lBuf, 4, lElapsed);
  lWriter.WaitFor;
  lWriter.Free;
  Check(lRes=4, 'Data in two parts returns 4 bytes, got '+IntToStr(lRes));
  Check((lBuf[0]=Ord('a')) and (lBuf[1]=Ord('b')) and (lBuf[2]=Ord('c')) and (lBuf[3]=Ord('d')), 'Data in two parts returns the bytes abcd');
  Check(lElapsed<C_MAX_ELAPSED, 'Data in two parts returns within the timeout, took '+IntToStr(lElapsed)+' ms');
  SerClose(lSlave);
  fpClose(lMaster);
end;


{ An open line without data returns 0 after the timeout }
procedure TestIdleLine;

var
  lMaster: cint;
  lSlave: TSerialHandle;
  lBuf: array[0..9] of byte;
  lRes: LongInt;
  lElapsed: QWord;

begin
  OpenPty(lMaster, lSlave);
  lRes:=TimedRead(lSlave, lBuf, 10, lElapsed);
  Check(lRes=0, 'Idle line returns 0, got '+IntToStr(lRes));
  Check(lElapsed>=C_TIMEOUT-50, 'Idle line waits for the timeout, took '+IntToStr(lElapsed)+' ms');
  Check(lElapsed<C_MAX_ELAPSED, 'Idle line returns within the timeout, took '+IntToStr(lElapsed)+' ms');
  SerClose(lSlave);
  fpClose(lMaster);
end;


begin
  lErrors:=0;
  { SIGALRM ends the program if SerReadTimeout never returns }
  fpAlarm(20);
  TestHangupAfterData;
  TestHangupWithoutData;
  TestSplitData;
  TestIdleLine;
  if lErrors>0 then
    Halt(1);
  writeln('ok');
end.
