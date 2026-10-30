{ %TARGET=win32,win64 }
{ signal() handlers of the signals unit are called with SEH exception handling, issue #36550 }
program tsignals;

{$mode objfpc}{$h+}

uses
  Windows, SysUtils, signals;

const
  cThreads = 4;
  cFaultsPerThread = 200;

var
  gJmp: jmp_buf;
  gCaught: longint;
  gZero: longint = 0;
  gFloatZero: double = 0;
  gFloatOne: double = 1;
  gPage: PLongint;
  gResumes: longint;
  gThreadCaught: array[1..cThreads] of longint;

threadvar
  tJmp: jmp_buf;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


// Writes to address nil
procedure DoAccessViolation;

begin
  PLongint(nil)^:=1;
end;


// Divides an integer by zero
procedure DoIntDivZero;

var
  lValue: longint;

begin
  lValue:=1000 div gZero;
  gCaught:=lValue;
end;


// Divides a double by zero
procedure DoFloatDivZero;

var
  lValue: double;

begin
  lValue:=gFloatOne/gFloatZero;
  gFloatOne:=lValue;
end;


// Records the signal and jumps back to gJmp
function OnSignal(aSig: longint): longint; cdecl;

begin
  gCaught:=aSig;
  LongJmp(gJmp,1);
  Result:=0;
end;


// Makes gPage writable so the faulting write can be repeated
function OnResume(aSig: longint): longint; cdecl;

var
  lOld: DWord;

begin
  Inc(gResumes);
  VirtualProtect(gPage,4096,PAGE_READWRITE,lOld);
  Result:=1;
end;


// Jumps back to the buffer of the current thread
function OnThreadSignal(aSig: longint): longint; cdecl;

begin
  LongJmp(tJmp,1);
  Result:=0;
end;


// Causes cFaultsPerThread access violations, counted in gThreadCaught[aIndex]
function FaultThread(aIndex: pointer): ptrint;

var
  lIndex: PtrInt;
  lCount: longint;

begin
  lIndex:=PtrInt(aIndex);
  lCount:=0;
  while lCount<cFaultsPerThread do
    if SetJmp(tJmp)=0 then
      DoAccessViolation
    else
      Inc(lCount);
  gThreadCaught[lIndex]:=lCount;
  Result:=0;
end;


// True if an access violation in a try block raises EAccessViolation
function AccessViolationRaises: boolean;

begin
  Result:=false;
  try
    DoAccessViolation;
  except
    on E: EAccessViolation do
      Result:=true;
  end;
end;


var
  lOld: DWord;
  lExceptCaught: boolean;
  lBefore, lAfter: longint;
  lThreads: array[1..cThreads] of TThreadHandle;
  i: longint;

begin
  Check(1,AccessViolationRaises,'access violation raises EAccessViolation without signal handlers');

  signal(SIGFPE,@OnSignal);
  Check(2,AccessViolationRaises,'access violation raises EAccessViolation when only SIGFPE has a handler');

  gCaught:=0;
  if SetJmp(gJmp)=0 then
    DoIntDivZero;
  Check(3,gCaught=SIGFPE,'integer division by zero calls the SIGFPE handler');

  gCaught:=0;
  if SetJmp(gJmp)=0 then
    DoFloatDivZero;
  Check(4,gCaught=SIGFPE,'float division by zero calls the SIGFPE handler');
  Check(5,gFloatOne*3=3,'float operations work after the SIGFPE handler');

  signal(SIGSEGV,@OnSignal);
  gCaught:=0;
  lExceptCaught:=false;
  if SetJmp(gJmp)=0 then
    try
      DoAccessViolation;
    except
      lExceptCaught:=true;
    end;
  Check(6,gCaught=SIGSEGV,'access violation calls the SIGSEGV handler');
  Check(7,not lExceptCaught,'the SIGSEGV handler runs before the except block');

  gPage:=VirtualAlloc(nil,4096,MEM_COMMIT or MEM_RESERVE,PAGE_READWRITE);
  VirtualProtect(gPage,4096,PAGE_NOACCESS,lOld);
  signal(SIGSEGV,@OnResume);
  gResumes:=0;
  lBefore:=12345;
  gPage^:=lBefore+1;
  lAfter:=gPage^;
  Check(8,gResumes=1,'the SIGSEGV handler is called once before resuming');
  Check(9,lAfter=12346,'the faulting write is repeated after the handler');
  Check(10,lBefore=12345,'registers are unchanged after resuming');
  VirtualFree(gPage,0,MEM_RELEASE);

  signal(SIGSEGV,@SIG_DFL);
  Check(11,AccessViolationRaises,'access violation raises EAccessViolation again with SIG_DFL');

  signal(SIGSEGV,@OnThreadSignal);
  for i:=1 to cThreads do
    lThreads[i]:=BeginThread(@FaultThread,pointer(PtrInt(i)));
  for i:=1 to cThreads do
    begin
    WaitForThreadTerminate(lThreads[i],0);
    CloseThread(lThreads[i]);
    Check(20+i,gThreadCaught[i]=cFaultsPerThread,'thread '+IntToStr(i)+' handled all its access violations');
    end;
  signal(SIGSEGV,@SIG_DFL);
  signal(SIGFPE,@SIG_DFL);
  Check(30,AccessViolationRaises,'access violation raises EAccessViolation at the end');
  writeln('ok');
end.
