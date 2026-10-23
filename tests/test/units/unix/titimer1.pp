{ %target=linux,android,darwin,freebsd,netbsd,openbsd,dragonfly }
{ BaseUnix FpSetITimer and FpGetITimer, issue #40888 }
program titimer1;

{$mode objfpc}

uses
  BaseUnix, Unix;

const
  cUSec = 1000;

var
  gErrors: Integer = 0;
  gAlarms: LongInt = 0;
  gVTAlarms: LongInt = 0;
  gProfs: LongInt = 0;


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


// Returns aValue as decimal text
function IntToStr(aValue: Int64): string;

begin
  Str(aValue,Result);
end;


// Counts SIGALRM, SIGVTALRM and SIGPROF
procedure Handler(aSig: cint); cdecl;

begin
  case aSig of
    SIGALRM: Inc(gAlarms);
    SIGVTALRM: Inc(gVTAlarms);
    SIGPROF: Inc(gProfs);
  end;
end;


// Installs Handler for aSig
procedure Install(aSig: cint);

var
  lAct: SigActionRec;

begin
  FillChar(lAct,SizeOf(lAct),0);
  lAct.sa_handler:=SigActionHandler(@Handler);
  CheckTrue('install handler for signal '+IntToStr(aSig),FpSigAction(aSig,@lAct,nil)=0);
end;


// Sleeps aMSec milliseconds, continuing after signals
procedure SleepMs(aMSec: Integer);

var
  lReq, lRem: TTimeSpec;

begin
  lReq.tv_sec:=aMSec div 1000;
  lReq.tv_nsec:=(aMSec mod 1000)*1000000;
  while FpNanoSleep(@lReq,@lRem)<>0 do
    lReq:=lRem;
end;


// Returns aVal in microseconds
function Micros(const aVal: timeval): Int64;

begin
  Result:=Int64(aVal.tv_sec)*1000000+aVal.tv_usec;
end;


// Fills aVal with a first expiration after aValueMs and a period of aIntervalMs milliseconds
procedure SetVal(out aVal: TItimerVal; aValueMs, aIntervalMs: Integer);

begin
  aVal.it_value.tv_sec:=aValueMs div 1000;
  aVal.it_value.tv_usec:=(aValueMs mod 1000)*cUSec;
  aVal.it_interval.tv_sec:=aIntervalMs div 1000;
  aVal.it_interval.tv_usec:=(aIntervalMs mod 1000)*cUSec;
end;


procedure TestOneShot;

var
  lVal, lOld: TItimerVal;

begin
  CheckTrue('FpGetITimer(ITIMER_REAL)',FpGetITimer(ITIMER_REAL,@lVal)=0);
  CheckTrue('ITIMER_REAL is disarmed at start',Micros(lVal.it_value)=0);
  SetVal(lVal,2000,0);
  CheckTrue('FpSetITimer arms ITIMER_REAL for 2 s',FpSetITimer(ITIMER_REAL,@lVal,@lOld)=0);
  CheckTrue('FpSetITimer returns the disarmed previous setting',Micros(lOld.it_value)=0);
  CheckTrue('FpGetITimer on the armed timer',FpGetITimer(ITIMER_REAL,@lVal)=0);
  CheckTrue('the remaining time is at most 2 s, got '+IntToStr(Micros(lVal.it_value)),
    (Micros(lVal.it_value)>1000000) and (Micros(lVal.it_value)<=2000000));
  CheckTrue('a one-shot timer has a zero interval',Micros(lVal.it_interval)=0);
  CheckTrue('fpalarm(0) returns the remaining seconds of ITIMER_REAL, rounded up',FpAlarm(0)=2);
  CheckTrue('fpalarm(0) disarms ITIMER_REAL',(FpGetITimer(ITIMER_REAL,@lVal)=0) and (Micros(lVal.it_value)=0));
  FpAlarm(10);
  FpGetITimer(ITIMER_REAL,@lVal);
  CheckTrue('fpalarm(10) arms ITIMER_REAL for 10 s, got '+IntToStr(Micros(lVal.it_value)),
    (Micros(lVal.it_value)>9000000) and (Micros(lVal.it_value)<=10000000));
  SetVal(lVal,0,0);
  CheckTrue('FpSetITimer with a zero it_value disarms',FpSetITimer(ITIMER_REAL,@lVal,@lOld)=0);
  CheckTrue('FpSetITimer returns the remaining time of the alarm in ovalue',Micros(lOld.it_value)>9000000);
  CheckTrue('FpSetITimer accepts a nil ovalue',FpSetITimer(ITIMER_REAL,@lVal,nil)=0);
end;


procedure TestPeriodicReal;

var
  lVal: TItimerVal;
  lTries, lAlarms: Integer;

begin
  Install(SIGALRM);
  SetVal(lVal,20,20);
  CheckTrue('FpSetITimer arms a periodic ITIMER_REAL',FpSetITimer(ITIMER_REAL,@lVal,nil)=0);
  lTries:=0;
  while (gAlarms<3) and (lTries<200) do
    begin
    SleepMs(10);
    Inc(lTries);
    end;
  CheckTrue('the periodic ITIMER_REAL delivers SIGALRM at least 3 times',gAlarms>=3);
  FpGetITimer(ITIMER_REAL,@lVal);
  CheckTrue('FpGetITimer returns the 20 ms interval',Micros(lVal.it_interval)=20000);
  SetVal(lVal,0,0);
  FpSetITimer(ITIMER_REAL,@lVal,nil);
  lAlarms:=gAlarms;
  SleepMs(60);
  CheckTrue('a disarmed ITIMER_REAL delivers no more signals',gAlarms=lAlarms);
end;


// Spins until aCount^ reaches aMin or 3 s have passed
procedure BurnUntil(aCount: PLongInt; aMin: LongInt);

var
  lNow, lStart: TTimeVal;
  i: LongInt;

begin
  FpGetTimeOfDay(@lStart,nil);
  repeat
    for i:=1 to 100000 do
      ;
    FpGetTimeOfDay(@lNow,nil);
  until (aCount^>=aMin) or (Micros(lNow)-Micros(lStart)>3000000);
end;


procedure TestCPUTimers;

var
  lVal: TItimerVal;

begin
  Install(SIGVTALRM);
  Install(SIGPROF);
  SetVal(lVal,10,10);
  CheckTrue('FpSetITimer arms ITIMER_VIRTUAL',FpSetITimer(ITIMER_VIRTUAL,@lVal,nil)=0);
  BurnUntil(@gVTAlarms,2);
  CheckTrue('ITIMER_VIRTUAL delivers SIGVTALRM while the process runs',gVTAlarms>=2);
  SetVal(lVal,0,0);
  FpSetITimer(ITIMER_VIRTUAL,@lVal,nil);
  SetVal(lVal,10,10);
  CheckTrue('FpSetITimer arms ITIMER_PROF',FpSetITimer(ITIMER_PROF,@lVal,nil)=0);
  BurnUntil(@gProfs,2);
  CheckTrue('ITIMER_PROF delivers SIGPROF while the process runs',gProfs>=2);
  CheckTrue('FpGetITimer(ITIMER_PROF) returns the interval',(FpGetITimer(ITIMER_PROF,@lVal)=0) and (Micros(lVal.it_interval)=10000));
  SetVal(lVal,0,0);
  FpSetITimer(ITIMER_PROF,@lVal,nil);
end;


procedure TestErrors;

var
  lVal: TItimerVal;

begin
  CheckTrue('FpGetITimer with an invalid timer fails',FpGetITimer(99,@lVal)=-1);
  CheckTrue('FpGetITimer with an invalid timer sets EINVAL',fpgeterrno=ESysEINVAL);
  SetVal(lVal,10,0);
  CheckTrue('FpSetITimer with an invalid timer fails',FpSetITimer(99,@lVal,nil)=-1);
end;


begin
  TestOneShot;
  TestPeriodicReal;
  TestCPUTimers;
  TestErrors;
  if gErrors>0 then
    Halt(1);
  Writeln('ok');
end.
