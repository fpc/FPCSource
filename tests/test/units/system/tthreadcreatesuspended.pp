{ BeginThread with CREATE_SUSPENDED in creationFlags starts the thread only on ResumeThread, issue #41779 }
program tthreadcreatesuspended;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils;

var
  gRan: LongInt;
  gThreadID: TThreadID;


// Sets gRan, stores the thread ID and returns the parameter as result
function MarkRun(aParam: Pointer): PtrInt;

begin
  gThreadID:=GetCurrentThreadId;
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


// Starts MarkRun with aFlags and aStackSize, resumes it when aResume is set, returns the exit code
function RunThread(aCode: Integer; aFlags: DWord; aStackSize: SizeUInt; aParam: PtrInt; aResume: Boolean): DWord;

var
  lHandle: TThreadHandle;
  lId: TThreadID;

begin
  gRan:=0;
  gThreadID:=TThreadID(0);
  lId:=TThreadID(0);
  lHandle:=BeginThread(nil,aStackSize,@MarkRun,Pointer(aParam),aFlags,lId);
  Check(aCode,lHandle<>TThreadHandle(0),'BeginThread returns a handle');
  if aResume then
    begin
    Sleep(200);
    Check(aCode+1,InterlockedExchangeAdd(gRan,0)=0,'suspended thread does not run before ResumeThread');
    Check(aCode+2,ResumeThread(lHandle)=1,'ResumeThread returns 1');
    end;
  Result:=WaitForThreadTerminate(lHandle,5000);
  CloseThread(lHandle);
  Check(aCode+3,InterlockedExchangeAdd(gRan,0)=1,'thread function runs');
  Check(aCode+4,gThreadID=lId,'BeginThread returns the ID of the started thread');
end;


begin
  Check(1,RunThread(10,CREATE_SUSPENDED,DefaultStackSize,42,True)=42,'exit code of a CREATE_SUSPENDED thread');
  Check(2,RunThread(20,CREATE_SUSPENDED or STACK_SIZE_PARAM_IS_A_RESERVATION,256*1024,9,True)=9,
    'exit code with CREATE_SUSPENDED and STACK_SIZE_PARAM_IS_A_RESERVATION');
  Check(3,RunThread(30,0,DefaultStackSize,7,False)=7,'exit code of a thread started at once');
  writeln('ok');
end.
