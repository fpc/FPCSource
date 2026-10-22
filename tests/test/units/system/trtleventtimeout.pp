{ RTLEventWaitFor with a timeout returns True when signalled and False on timeout, issue #34284 }
program trtleventtimeout;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils;

var
  gEvent: PRTLEvent;
  gFailed: Boolean;


// Write aMsg and mark the test as failed when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    gFailed:=True;
    end;
end;


// Sets gEvent after 100 ms
function SetLater(aParam: Pointer): PtrInt;

begin
  Sleep(100);
  RTLEventSetEvent(gEvent);
  Result:=0;
end;


var
  lId: TThreadID;

begin
  gFailed:=False;
  gEvent:=RTLEventCreate;
  Check(1,not RTLEventWaitFor(gEvent,50),'wait on unset event times out');
  RTLEventSetEvent(gEvent);
  Check(2,RTLEventWaitFor(gEvent,50),'wait on set event is signalled');
  Check(3,not RTLEventWaitFor(gEvent,20),'event is reset by a successful wait');
  RTLEventSetEvent(gEvent);
  RTLEventResetEvent(gEvent);
  Check(4,not RTLEventWaitFor(gEvent,20),'wait on reset event times out');
  lId:=BeginThread(@SetLater,nil);
  if lId=TThreadID(0) then
    begin
    writeln('cannot start thread');
    halt(1);
    end;
  Check(5,RTLEventWaitFor(gEvent,5000),'wait is signalled by another thread');
  WaitForThreadTerminate(lId,5000);
  CloseThread(lId);
  RTLEventDestroy(gEvent);
  if gFailed then
    halt(1);
  writeln('ok');
end.
