{ InitMultiThreading switches the RTL to multithreaded mode and runs the lazy init procedures, issue #41793 }
program tinitmt;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils;

var
  gLazyCount: LongInt;


// Counts the calls made by LazyInitThreading
procedure CountLazyInit;

begin
  Inc(gLazyCount);
end;


// Returns its parameter
function ReturnParam(aParam: Pointer): PtrInt;

begin
  Result:=PtrInt(aParam);
end;


var
  lWasMultiThread: Boolean;
  lHandle: TThreadHandle;

begin
  gLazyCount:=0;
  lWasMultiThread:=IsMultiThread;
  RegisterLazyInitThreadingProc(@CountLazyInit);
  if InitMultiThreading<>lWasMultiThread then
    begin
    writeln('first InitMultiThreading did not return the previous IsMultiThread');
    halt(1);
    end;
  if not IsMultiThread then
    begin
    writeln('IsMultiThread is false after InitMultiThreading');
    halt(2);
    end;
  if gLazyCount<>1 then
    begin
    writeln('lazy init procedure ran ',gLazyCount,' times, expected 1');
    halt(3);
    end;
  if not InitMultiThreading then
    begin
    writeln('second InitMultiThreading returned false');
    halt(4);
    end;
  lHandle:=BeginThread(@ReturnParam,Pointer(3));
  if lHandle=TThreadHandle(0) then
    halt(5);
  if WaitForThreadTerminate(lHandle,5000)<>3 then
    begin
    writeln('thread started after InitMultiThreading returned a wrong exit code');
    halt(6);
    end;
  CloseThread(lHandle);
  if gLazyCount<>1 then
    begin
    writeln('lazy init procedure ran again: ',gLazyCount,' times');
    halt(7);
    end;
  writeln('ok');
end.
