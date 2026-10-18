{ %target=linux }
{ ThreadSetPriority and TThread.Priority use the nice value and SCHED_IDLE, issue #16785 }
program tthreadprio;

{$mode objfpc}{$h+}

uses
  cthreads, ctypes, BaseUnix, Classes, SysUtils;

const
  SCHED_OTHER = 0;
  SCHED_IDLE = 5;
  PRIO_PROCESS = 0;

type
  TSampleThread = class(TThread)
  private
    FNice: cint;
  protected
    procedure Execute; override;
  public
    // Nice value seen by Execute
    property Nice: cint read FNice;
  end;

function getpriority(aWhich, aWho: cint): cint; cdecl; external 'c' name 'getpriority';
function sched_getscheduler(aPid: cint): cint; cdecl; external 'c' name 'sched_getscheduler';

var
  gSample, gSampled: PRTLEvent;
  gStop: Boolean;
  gNice: cint;
  gPolicy: cint;


procedure TSampleThread.Execute;

begin
  FNice:=getpriority(PRIO_PROCESS,0);
end;


// Records nice value and policy of the calling thread on every gSample event
function SampleLoop(aParam: Pointer): PtrInt;

begin
  repeat
    RTLEventWaitFor(gSample);
    gNice:=getpriority(PRIO_PROCESS,0);
    gPolicy:=sched_getscheduler(0);
    RTLEventSetEvent(gSampled);
  until gStop;
  Result:=0;
end;


// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    writeln('check ',aCode,' failed: ',aMsg);
    halt(aCode);
    end;
end;


// Lets the sample thread record its nice value and policy
procedure Sample;

begin
  RTLEventSetEvent(gSample);
  RTLEventWaitFor(gSampled);
end;


var
  lId: TThreadID;
  lPrio: longint;
  lThread: TSampleThread;

begin
  gSample:=RTLEventCreate;
  gSampled:=RTLEventCreate;
  lId:=BeginThread(@SampleLoop,nil);
  Check(1,lId<>TThreadID(0),'BeginThread');
  Sample;
  if (gNice<>0) or (gPolicy<>SCHED_OTHER) then
    begin
    writeln(Format('skipped: initial nice %d, policy %d',[gNice,gPolicy]));
    halt(0);
    end;

  Check(10,ThreadSetPriority(lId,-5),'ThreadSetPriority(-5)');
  lPrio:=ThreadGetPriority(lId);
  Check(11,lPrio=-5,Format('ThreadGetPriority after -5 is %d',[lPrio]));
  Sample;
  Check(12,(gNice=7) and (gPolicy=SCHED_OTHER),Format('nice %d, policy %d after -5, expected 7, %d',[gNice,gPolicy,SCHED_OTHER]));

  if fpgeteuid<>0 then
    begin
    Check(20,not ThreadSetPriority(lId,5),'unprivileged ThreadSetPriority(5) fails');
    lPrio:=ThreadGetPriority(lId);
    Check(21,lPrio=-5,Format('ThreadGetPriority after failed raise is %d',[lPrio]));
    end;

  Check(30,ThreadSetPriority(lId,-15),'ThreadSetPriority(-15)');
  lPrio:=ThreadGetPriority(lId);
  Check(31,lPrio=-15,Format('ThreadGetPriority after -15 is %d',[lPrio]));
  Sample;
  Check(32,gPolicy=SCHED_IDLE,Format('policy %d after -15, expected %d',[gPolicy,SCHED_IDLE]));

  gStop:=True;
  RTLEventSetEvent(gSample);
  WaitForThreadTerminate(lId,0);
  CloseThread(lId);

  lThread:=TSampleThread.Create(True);
  lThread.Priority:=tpLower;
  Check(40,lThread.Priority=tpLower,'TThread.Priority after tpLower');
  lThread.Start;
  lThread.WaitFor;
  Check(41,lThread.Nice=7,Format('nice of a tpLower TThread is %d, expected 7',[lThread.Nice]));
  lThread.Free;

  Check(50,ThreadSetPriority(GetCurrentThreadId,-10),'ThreadSetPriority(-10) on the calling thread');
  lPrio:=ThreadGetPriority(GetCurrentThreadId);
  Check(51,lPrio=-10,Format('ThreadGetPriority of the calling thread is %d',[lPrio]));
  Check(52,getpriority(PRIO_PROCESS,0)=13,Format('nice of the calling thread is %d, expected 13',[getpriority(PRIO_PROCESS,0)]));

  RTLEventDestroy(gSample);
  RTLEventDestroy(gSampled);
  writeln('ok');
end.
