{ %target=linux }
{ Linux POSIX timers: timer_create, timer_settime, timer_gettime, timer_getoverrun, timer_delete, issue #40888 }
program ttimer1;

{$mode objfpc}

uses
  BaseUnix, Linux;

const
  cMSec = 1000000;

var
  gErrors: Integer = 0;
  gTicks: LongInt = 0;
  gOverrun: LongInt = -1;
  gSigValue: Pointer = nil;
  gSigTid: LongInt = -1;
  gSigOverrun: LongInt = -1;
  gOverrunTimer: timer_t;

{
  Some Linux distributions
  accept any value for clock type
  gcc95 riscv64-linux
  Linux cfarm95 6.6.36-cfarm #1 SMP PREEMPT Sun Oct  6 18:27:25 UTC 2024 riscv64 GNU/Linux
  strace gives this:
  timer_create(0xfffffc18 /* CLOCK_??? */, NULL, [4]) = 0
  The same also happens for gcc185
  Linux cfarm185 4.18.0-553.120.1.el8_10.aarch64 #1 SMP Mon Apr 20 22:23:16 UTC 2026 aarch64 aarch64 aarch64 GNU/Linux
}

{$if defined(CPURISCV64) or defined(CPUAARCH64)}
  {$define OS_ACCEPTS_ALL_CLOCK_TYPE}
{$endif}

procedure CheckTrue(const aName: string; aValue: Boolean);

begin
  if aValue then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR');
    Inc(gErrors);
    end;
end;

{$ifdef OS_ACCEPTS_ALL_CLOCK_TYPE}
procedure CheckErrorTrue(const aName: string; aValue: Boolean);

begin
  if aValue then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' known OS ERROR');
    // Known OS limitation
    // Inc(gErrors);
    end;
end;
{$else}
procedure CheckErrorTrue(const aName: string; aValue: Boolean);
begin
  CheckTrue(aName,aValue);
end;
{$endif}


// Returns aValue as decimal text
function IntToStr(aValue: LongInt): string;

begin
  Str(aValue,Result);
end;


// Counts SIGUSR1 and stores the sigev_value and the timer id of the siginfo
procedure TickHandler(aSig: cint; aInfo: PSigInfo; aContext: PSigContext); cdecl;

begin
  Inc(gTicks);
  gSigValue:=aInfo^._sifields._timer._sigval;
  gSigTid:=aInfo^._sifields._timer._tid;
end;


// Stores the overrun counts of timer_getoverrun and of the siginfo when SIGUSR2 first arrives
procedure OverrunHandler(aSig: cint; aInfo: PSigInfo; aContext: PSigContext); cdecl;

begin
  if gOverrun<0 then
    begin
    gOverrun:=timer_getoverrun(gOverrunTimer);
    gSigOverrun:=aInfo^._sifields._timer._overrun;
    end;
end;


// Installs aHandler with SA_SIGINFO for aSig
procedure Install(aSig: cint; aHandler: Pointer);

var
  lAct: SigActionRec;

begin
  FillChar(lAct,SizeOf(lAct),0);
  lAct.sa_handler:=SigActionHandler(aHandler);
  lAct.sa_flags:=SA_SIGINFO;
  CheckTrue('install handler for signal '+IntToStr(aSig),FpSigAction(aSig,@lAct,nil)=0);
end;


// Sleeps aMSec milliseconds, continuing after signals
procedure SleepMs(aMSec: Integer);

var
  lReq, lRem: TTimeSpec;

begin
  lReq.tv_sec:=aMSec div 1000;
  lReq.tv_nsec:=(aMSec mod 1000)*cMSec;
  while FpNanoSleep(@lReq,@lRem)<>0 do
    lReq:=lRem;
end;


// Returns aSpec in nanoseconds
function Nanos(const aSpec: TTimeSpec): Int64;

begin
  Result:=Int64(aSpec.tv_sec)*1000000000+aSpec.tv_nsec;
end;


// Fills aSpec with a first expiration after aValueMs and a period of aIntervalMs milliseconds
procedure SetSpec(out aSpec: itimerspec; aValueMs, aIntervalMs: Integer);

begin
  aSpec.it_value.tv_sec:=aValueMs div 1000;
  aSpec.it_value.tv_nsec:=(aValueMs mod 1000)*cMSec;
  aSpec.it_interval.tv_sec:=aIntervalMs div 1000;
  aSpec.it_interval.tv_nsec:=(aIntervalMs mod 1000)*cMSec;
end;


procedure TestLayout;

var
  lEvent: sigevent;

begin
  CheckTrue('SizeOf(sigevent) is 64',SizeOf(sigevent)=64);
  CheckTrue('sigev_signo follows sigev_value',PtrUInt(@lEvent.sigev_signo)-PtrUInt(@lEvent)=SizeOf(pointer));
  CheckTrue('sigev_notify follows sigev_signo',PtrUInt(@lEvent.sigev_notify)-PtrUInt(@lEvent)=SizeOf(pointer)+4);
  CheckTrue('sigev_notify_thread_id starts the union',PtrUInt(@lEvent.sigev_notify_thread_id)-PtrUInt(@lEvent)=SizeOf(pointer)+8);
  CheckTrue('SizeOf(itimerspec) is two timespecs',SizeOf(itimerspec)=2*SizeOf(timespec));
end;


procedure TestSigInfoLayout;

var
  lInfo: TSigInfo;
  lBase: PtrUInt;

begin
  lBase:=PtrUInt(@lInfo._sifields);
  CheckTrue('siginfo _timer._tid starts _sifields',PtrUInt(@lInfo._sifields._timer._tid)=lBase);
  CheckTrue('siginfo _timer._overrun follows _tid',PtrUInt(@lInfo._sifields._timer._overrun)-lBase=4);
  CheckTrue('siginfo _timer._sigval is at the offset of _rt._sigval',
    PtrUInt(@lInfo._sifields._timer._sigval)=PtrUInt(@lInfo._sifields._rt._sigval));
  CheckTrue('siginfo _timer._timer1 overlays _tid',PtrUInt(@lInfo._sifields._timer._timer1)=lBase);
end;


procedure TestPeriodicSignal;

var
  lEvent: sigevent;
  lTimer, lSpare: timer_t;
  lSpec, lCur: itimerspec;
  lTries, lTicks: Integer;

begin
  Install(SIGUSR1,@TickHandler);
  CheckTrue('timer_create of a timer that takes the first id',timer_create(CLOCK_MONOTONIC,nil,@lSpare)=0);
  FillChar(lEvent,SizeOf(lEvent),0);
  lEvent.sigev_notify:=SIGEV_SIGNAL;
  lEvent.sigev_signo:=SIGUSR1;
  lEvent.sigev_value.sival_ptr:=@gTicks;
  CheckTrue('timer_create with SIGEV_SIGNAL',timer_create(CLOCK_MONOTONIC,@lEvent,@lTimer)=0);
  SetSpec(lSpec,10,10);
  CheckTrue('timer_settime arms a periodic timer',timer_settime(lTimer,0,@lSpec,nil)=0);
  lTries:=0;
  while (gTicks<3) and (lTries<200) do
    begin
    SleepMs(10);
    Inc(lTries);
    end;
  CheckTrue('the periodic timer delivers SIGUSR1 at least 3 times',gTicks>=3);
  CheckTrue('siginfo _timer._sigval is the sigev_value of the timer',gSigValue=@gTicks);
  CheckTrue('siginfo _timer._tid is the timer id, got $'+HexStr(gSigTid,8)+', timer $'+HexStr(LongInt(lTimer),8),gSigTid=LongInt(lTimer));
  CheckTrue('timer_gettime on an armed timer',timer_gettime(lTimer,@lCur)=0);
  CheckTrue('timer_gettime returns the interval',Nanos(lCur.it_interval)=10*cMSec);
  CheckTrue('timer_gettime returns a time until the next expiration within the interval',
    (Nanos(lCur.it_value)>0) and (Nanos(lCur.it_value)<=10*cMSec));
  SetSpec(lSpec,0,0);
  CheckTrue('timer_settime with a zero it_value disarms the timer',timer_settime(lTimer,0,@lSpec,@lCur)=0);
  CheckTrue('timer_settime returns the previous interval in old_value',Nanos(lCur.it_interval)=10*cMSec);
  lTicks:=gTicks;
  SleepMs(50);
  CheckTrue('a disarmed timer delivers no more signals',gTicks=lTicks);
  CheckTrue('timer_gettime on a disarmed timer',timer_gettime(lTimer,@lCur)=0);
  CheckTrue('a disarmed timer has a zero it_value',Nanos(lCur.it_value)=0);
  CheckTrue('timer_delete',timer_delete(lTimer)=0);
  CheckTrue('timer_delete of the timer with the first id',timer_delete(lSpare)=0);
end;


procedure TestOneShotNone;

var
  lEvent: sigevent;
  lTimer: timer_t;
  lSpec, lCur: itimerspec;
  lNow: TTimeSpec;

begin
  FillChar(lEvent,SizeOf(lEvent),0);
  lEvent.sigev_notify:=SIGEV_NONE;
  CheckTrue('timer_create with SIGEV_NONE',timer_create(CLOCK_MONOTONIC,@lEvent,@lTimer)=0);
  SetSpec(lSpec,200,0);
  CheckTrue('timer_settime arms a one-shot timer',timer_settime(lTimer,0,@lSpec,nil)=0);
  CheckTrue('timer_gettime on a one-shot timer',timer_gettime(lTimer,@lCur)=0);
  CheckTrue('the one-shot timer has not expired yet',(Nanos(lCur.it_value)>0) and (Nanos(lCur.it_value)<=200*cMSec));
  CheckTrue('a one-shot timer has a zero interval',Nanos(lCur.it_interval)=0);
  CheckTrue('clock_gettime of the timer clock',clock_gettime(CLOCK_MONOTONIC,@lNow)=0);
  lSpec.it_interval.tv_sec:=0;
  lSpec.it_interval.tv_nsec:=0;
  lSpec.it_value:=lNow;
  Inc(lSpec.it_value.tv_nsec,30*cMSec);
  if lSpec.it_value.tv_nsec>=1000000000 then
    begin
    Inc(lSpec.it_value.tv_sec);
    Dec(lSpec.it_value.tv_nsec,1000000000);
    end;
  CheckTrue('timer_settime with TIMER_ABSTIME',timer_settime(lTimer,TIMER_ABSTIME,@lSpec,nil)=0);
  timer_gettime(lTimer,@lCur);
  CheckTrue('an absolute expiration 30 ms ahead is at most 30 ms away',(Nanos(lCur.it_value)>0) and (Nanos(lCur.it_value)<=30*cMSec));
  SleepMs(80);
  timer_gettime(lTimer,@lCur);
  CheckTrue('an expired one-shot timer has a zero it_value',Nanos(lCur.it_value)=0);
  CheckTrue('timer_delete of the SIGEV_NONE timer',timer_delete(lTimer)=0);
end;


procedure TestOverrun;

var
  lEvent: sigevent;
  lSpec: itimerspec;
  lSet, lOld: TSigSet;

begin
  Install(SIGUSR2,@OverrunHandler);
  FpSigEmptySet(lSet);
  FpSigAddSet(lSet,SIGUSR2);
  CheckTrue('block SIGUSR2',FpSigProcMask(SIG_BLOCK,@lSet,@lOld)=0);
  FillChar(lEvent,SizeOf(lEvent),0);
  lEvent.sigev_notify:=SIGEV_SIGNAL;
  lEvent.sigev_signo:=SIGUSR2;
  CheckTrue('timer_create for the overrun test',timer_create(CLOCK_MONOTONIC,@lEvent,@gOverrunTimer)=0);
  SetSpec(lSpec,5,5);
  CheckTrue('timer_settime with a 5 ms period',timer_settime(gOverrunTimer,0,@lSpec,nil)=0);
  SleepMs(100);
  CheckTrue('unblock SIGUSR2',FpSigProcMask(SIG_SETMASK,@lOld,nil)=0);
  SetSpec(lSpec,0,0);
  timer_settime(gOverrunTimer,0,@lSpec,nil);
  CheckTrue('timer_getoverrun counts the expirations while the signal was blocked, got '+IntToStr(gOverrun),gOverrun>=5);
  CheckTrue('siginfo _timer._overrun is the timer_getoverrun count, got '+IntToStr(gSigOverrun),gSigOverrun=gOverrun);
  CheckTrue('timer_delete of the overrun timer',timer_delete(gOverrunTimer)=0);
end;


procedure TestErrors;

var
  lTimer: timer_t;
  lCur: itimerspec;

begin
  CheckErrorTrue('timer_create with an invalid clock fails',timer_create(-1000,nil,@lTimer)=-1);
  CheckErrorTrue('timer_create with an invalid clock sets EINVAL',fpgeterrno=ESysEINVAL);
  CheckTrue('timer_create with the default notification',timer_create(CLOCK_REALTIME,nil,@lTimer)=0);
  CheckTrue('timer_delete of the default timer',timer_delete(lTimer)=0);
  CheckTrue('timer_gettime of a deleted timer fails',timer_gettime(lTimer,@lCur)=-1);
  CheckTrue('timer_gettime of a deleted timer sets EINVAL',fpgeterrno=ESysEINVAL);
  CheckTrue('timer_delete of a deleted timer fails',timer_delete(lTimer)=-1);
end;


begin
  TestLayout;
  TestSigInfoLayout;
  TestPeriodicSignal;
  TestOneShotNone;
  TestOverrun;
  TestErrors;
  if gErrors>0 then
    Halt(1);
  Writeln('ok');
end.
