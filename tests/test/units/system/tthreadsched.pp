{ %target=linux }
{ BeginThread succeeds and the thread inherits SCHED_IDLE from its creator, issue #41807 }
program tthreadsched;

{$mode objfpc}{$h+}

uses
  cthreads, ctypes;

const
  SCHED_IDLE = 5;

type
  TSchedParam = record
    sched_priority: cint;
  end;

function sched_setscheduler(aPid: cint; aPolicy: cint; aParam: Pointer): cint; cdecl; external 'c' name 'sched_setscheduler';
function sched_getscheduler(aPid: cint): cint; cdecl; external 'c' name 'sched_getscheduler';

var
  gPolicy: cint = -1;


// Stores the scheduling policy of the calling thread in gPolicy
function GetPolicy(aParam: Pointer): PtrInt;

begin
  gPolicy:=sched_getscheduler(0);
  Result:=0;
end;


var
  lParam: TSchedParam;
  lId: TThreadID;

begin
  lParam.sched_priority:=0;
  if sched_setscheduler(0,SCHED_IDLE,@lParam)<>0 then
    halt(1);
  lId:=BeginThread(@GetPolicy,nil);
  if lId=TThreadID(0) then
    halt(2);
  WaitForThreadTerminate(lId,0);
  CloseThread(lId);
  if gPolicy<>SCHED_IDLE then
    begin
    writeln('thread policy ',gPolicy);
    halt(3);
    end;
  writeln('ok');
end.
