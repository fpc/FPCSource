{ ExceptionErrorMessage and ShowException without heap allocations, issue #41467 }
program texcerrmsg;

{$mode objfpc}{$h+}

uses
  SysUtils, SysConst;

type
  ETestError = class(Exception);

var
  gOldManager, gCountManager: TMemoryManager;
  gCalls: LongInt;
  gShown: ShortString;


{ Counting wrappers around the original memory manager }
function CountGetMem(aSize: PtrUInt): Pointer;

begin
  Inc(gCalls);
  Result:=gOldManager.GetMem(aSize);
end;


function CountFreeMem(p: Pointer): PtrUInt;

begin
  Inc(gCalls);
  Result:=gOldManager.FreeMem(p);
end;


function CountFreeMemSize(p: Pointer; aSize: PtrUInt): PtrUInt;

begin
  Inc(gCalls);
  Result:=gOldManager.FreeMemSize(p,aSize);
end;


function CountAllocMem(aSize: PtrUInt): Pointer;

begin
  Inc(gCalls);
  Result:=gOldManager.AllocMem(aSize);
end;


function CountReAllocMem(var p: Pointer; aSize: PtrUInt): Pointer;

begin
  Inc(gCalls);
  Result:=gOldManager.ReAllocMem(p,aSize);
end;


{ Starts counting heap calls }
procedure StartCounting;

begin
  gCalls:=0;
  SetMemoryManager(gCountManager);
end;


{ Stops counting heap calls, returns the number of calls }
function StopCounting: LongInt;

begin
  SetMemoryManager(gOldManager);
  Result:=gCalls;
end;


{ Stores the message passed to OnShowException }
procedure StoreShown(aMsg: ShortString);

begin
  gShown:=aMsg;
end;


{ Checks ExceptionErrorMessage for aObj with buffer size aSize against aExpected }
procedure CheckMessage(aCode: Integer; aObj: TObject; aSize: Integer; const aExpected: string);

var
  lBuf: array[0..511] of AnsiChar;
  lRes, lCalls: Integer;
  lGot: string;

begin
  FillChar(lBuf,SizeOf(lBuf),'#');
  StartCounting;
  lRes:=ExceptionErrorMessage(aObj,Pointer($1234),@lBuf[0],aSize);
  lCalls:=StopCounting;
  if lCalls<>0 then
    halt(aCode+1);
  SetString(lGot,PAnsiChar(@lBuf[0]),lRes);
  if lGot<>aExpected then
    begin
    writeln('expected "',aExpected,'", got "',lGot,'"');
    halt(aCode+2);
    end;
  if lBuf[lRes]<>'#' then
    halt(aCode+3);
end;


var
  lExc, lDot: Exception;
  lObj: TObject;
  lFull, lPlain, lDotted: string;
  lCalls: Integer;

begin
  GetMemoryManager(gOldManager);
  gCountManager:=gOldManager;
  gCountManager.GetMem:=@CountGetMem;
  gCountManager.FreeMem:=@CountFreeMem;
  gCountManager.FreeMemSize:=@CountFreeMemSize;
  gCountManager.AllocMem:=@CountAllocMem;
  gCountManager.ReAllocMem:=@CountReAllocMem;

  lExc:=ETestError.Create('Something failed');
  lDot:=ETestError.Create('Ends with a dot.');
  lObj:=TObject.Create;
  lFull:=Format(SExceptionErrorMessage,[Pointer($1234),'ETestError'])+':'#10'Something failed.';
  lPlain:=Format(SExceptionErrorMessage,[Pointer($1234),'TObject'])+'.';
  lDotted:=Format(SExceptionErrorMessage,[Pointer($1234),'ETestError'])+':'#10'Ends with a dot.';

  CheckMessage(10,lExc,512,lFull);
  CheckMessage(20,lObj,512,lPlain);
  CheckMessage(30,lDot,512,lDotted);
  CheckMessage(40,lExc,10,Copy(lFull,1,10));
  CheckMessage(50,lExc,0,'');

  IsConsole:=False;
  OnShowException:=@StoreShown;
  gShown:='';
  StartCounting;
  ShowException(lExc,Pointer($1234));
  lCalls:=StopCounting;
  IsConsole:=True;
  if lCalls<>0 then
    halt(61);
  if gShown<>lFull then
    halt(62);

  lExc.Free;
  lDot.Free;
  lObj.Free;
  writeln('ok');
end.
