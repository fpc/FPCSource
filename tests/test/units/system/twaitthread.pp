{ WaitForThreadTerminate returns the exit code of the thread, or dword(-1) on timeout, issue #16492 }
program twaitthread;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils;

var
  gRelease: PRTLEvent;


// Returns 42
function ReturnCode(aParam: Pointer): PtrInt;

begin
  Result:=42;
end;


// Waits for gRelease, then returns 7
function WaitAndReturn(aParam: Pointer): PtrInt;

begin
  RTLEventWaitFor(gRelease);
  Result:=7;
end;


var
  lHandle: TThreadHandle;
  lRes: DWord;

begin
  lHandle:=BeginThread(@ReturnCode,nil);
  if lHandle=TThreadHandle(0) then
    halt(1);
  lRes:=WaitForThreadTerminate(lHandle,0);
  CloseThread(lHandle);
  if lRes<>42 then
    begin
    writeln('exit code ',lRes,', expected 42');
    halt(2);
    end;
  gRelease:=RTLEventCreate;
  lHandle:=BeginThread(@WaitAndReturn,nil);
  if lHandle=TThreadHandle(0) then
    halt(3);
{$if defined(windows) or (defined(linux) and not defined(android))}
  lRes:=WaitForThreadTerminate(lHandle,50);
  if lRes<>DWord(-1) then
    begin
    writeln('timeout result ',lRes,', expected ',DWord(-1));
    halt(4);
    end;
{$endif}
  RTLEventSetEvent(gRelease);
  lRes:=WaitForThreadTerminate(lHandle,5000);
  CloseThread(lHandle);
  RTLEventDestroy(gRelease);
  if lRes<>7 then
    begin
    writeln('exit code after release ',lRes,', expected 7');
    halt(5);
    end;
  writeln('ok');
end.
