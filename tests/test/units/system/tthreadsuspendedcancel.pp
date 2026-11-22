{ CloseThread and KillThread on a thread started suspended and never resumed: its function does not run, issue #41779 }
program tthreadsuspendedcancel;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils;

var
  gRan: LongInt;


// Sets gRan and returns the parameter as result
function MarkRun(aParam: Pointer): PtrInt;

begin
  InterlockedExchange(gRan,1);
  Result:=PtrInt(aParam);
end;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


{$ifdef linux}
// Number of threads of this process
function ThreadCount: Integer;

var
  lInfo: TSearchRec;

begin
  Result:=0;
  if FindFirst('/proc/self/task/*',faDirectory,lInfo)=0 then
    begin
    repeat
      if (lInfo.Name<>'.') and (lInfo.Name<>'..') then
        Inc(Result);
    until FindNext(lInfo)<>0;
    FindClose(lInfo);
    end;
end;


// Waits up to 5 seconds until the process has aCount threads
function WaitForThreadCount(aCount: Integer): Boolean;

var
  I: Integer;

begin
  for I:=1 to 100 do
    begin
    if ThreadCount=aCount then
      Exit(True);
    Sleep(50);
    end;
  Result:=False;
end;
{$endif linux}


// Starts MarkRun suspended and returns its handle
function StartSuspended(aCode: Integer): TThreadHandle;

var
  lId: TThreadID;

begin
  gRan:=0;
  Result:=BeginThread(@MarkRun,Pointer(1),lId,True);
  Check(aCode,Result<>TThreadHandle(0),'BeginThread returns a handle');
  Sleep(100);
end;


var
  lHandle: TThreadHandle;
{$ifdef linux}
  lThreads: Integer;
{$endif linux}

begin
{$ifdef linux}
  lThreads:=ThreadCount;
{$endif linux}
  lHandle:=StartSuspended(10);
  KillThread(lHandle);
  Sleep(300);
  Check(11,InterlockedExchangeAdd(gRan,0)=0,'KillThread: the thread function of a never resumed thread does not run');
{$ifdef linux}
  Check(12,WaitForThreadCount(lThreads),'KillThread ends a never resumed thread');
{$endif linux}

  lHandle:=StartSuspended(20);
  CloseThread(lHandle);
  Sleep(300);
  Check(21,InterlockedExchangeAdd(gRan,0)=0,'CloseThread: the thread function of a never resumed thread does not run');
  writeln('ok');
end.
