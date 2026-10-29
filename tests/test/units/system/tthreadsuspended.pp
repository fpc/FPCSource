{ BeginThread with Suspended=True starts the thread only on ResumeThread, issue #41779 }
program tthreadsuspended;

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


var
  lHandle: TThreadHandle;
  lId: TThreadID;
  lRes: DWord;

begin
  gRan:=0;
  lHandle:=BeginThread(@MarkRun,Pointer(42),lId,True);
  if lHandle=TThreadHandle(0) then
    halt(1);
  Sleep(200);
  if InterlockedExchangeAdd(gRan,0)<>0 then
    begin
    writeln('suspended thread ran before ResumeThread');
    halt(2);
    end;
  lRes:=ResumeThread(lHandle);
  if lRes<>1 then
    begin
    writeln('ResumeThread returned ',lRes,', expected 1');
    halt(3);
    end;
  lRes:=WaitForThreadTerminate(lHandle,5000);
  if lRes<>42 then
    begin
    writeln('exit code ',lRes,', expected 42');
    halt(4);
    end;
  if InterlockedExchangeAdd(gRan,0)<>1 then
    begin
    writeln('thread function did not run');
    halt(5);
    end;
  if ResumeThread(lHandle)=1 then
    begin
    writeln('second ResumeThread started the thread again');
    halt(6);
    end;
  CloseThread(lHandle);

  gRan:=0;
  lHandle:=BeginThread(@MarkRun,Pointer(7),lId,False);
  if lHandle=TThreadHandle(0) then
    halt(7);
  lRes:=WaitForThreadTerminate(lHandle,5000);
  CloseThread(lHandle);
  if (lRes<>7) or (InterlockedExchangeAdd(gRan,0)<>1) then
    begin
    writeln('non-suspended thread: exit code ',lRes,', expected 7');
    halt(8);
    end;

  lHandle:=BeginThread(@MarkRun,Pointer(9),lId,True,64*1024);
  if lHandle=TThreadHandle(0) then
    halt(9);
  ResumeThread(lHandle);
  lRes:=WaitForThreadTerminate(lHandle,5000);
  CloseThread(lHandle);
  if lRes<>9 then
    begin
    writeln('explicit stack size: exit code ',lRes,', expected 9');
    halt(10);
    end;
  writeln('ok');
end.
