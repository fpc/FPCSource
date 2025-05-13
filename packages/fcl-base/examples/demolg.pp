program testlg;

{$mode objFPC}
{$modeswitch advancedrecords}

Uses
  {$ifdef unix}
  cthreads,
  {$endif}
  sysutils,
  classes,
  syncobjs;

type

  { TLockGuard }
  generic TLockGuard<T:TSynchroObject> = record
    obj: T;
    class operator Initialize(var hdl: TLockGuard);
    class operator Finalize(var hdl: TLockGuard);
    procedure Init(AObj: T); 
  end;

class operator TLockGuard.Initialize(var hdl: TLockGuard);
begin
  hdl.obj := nil;
end;

class operator TLockGuard.Finalize(var hdl: TLockGuard);
begin
  if (hdl.obj=nil) then
    exit;
  hdl.obj.Release();
end;

procedure TLockGuard.Init(AObj:T);
begin
  self.obj := AObj;
  self.obj.Acquire();
end;

Function Fibonacci(TN,N : Integer) : Int64;
Var
  Next,Last : Int64;
  I : Integer;

begin
  if N=0 then
    exit(0);
  Result:=1;
  Last:=0;
  for I:=1 to N-1 do
    begin
    Next:=Result+last;
    Last:=Result;
    Result:=Next;
    Writeln('Thread['+IntToStr(TN)+'] '+IntToStr(Result));
    end;
end;

var
  ThreadCount : Integer;
  ExecuteCount : Integer;

Type
  { TCalcThread }
  TCalcThread = Class(TThread)
  Public
    class var ExecuteLock : TCriticalSection;
  Private
    FNo : Integer;
  Public  
    constructor create(aNo : Integer);
    destructor destroy; override;
    Procedure Execute; override;
    
  end;

{ TCalcThread }

constructor TCalcThread.create(aNo : Integer);
begin
  Inherited Create(False);
  InterlockedIncrement(ThreadCount);
  FNo:=aNo;
  Writeln('Creating thread ',FNo);
  FreeOnTerminate:=True;
end;

destructor TCalcThread.destroy; 
begin
  InterlockedDecrement(ThreadCount);
  Inherited;
end;

procedure TCalcThread.Execute;
var
  lock : specialize TLockGuard<TCriticalSection>;
  Res : Integer;
begin
  lock.Init(ExecuteLock);
  InterlockedIncrement(ExecuteCount);
  if ExecuteCount<>1 then 
    Writeln('Error : multiple threads are executing (start)');
  Res:=Fibonacci(FNo,10);
  writeln('Thread['+IntTostr(FNo),'] Fibonacci(10) = '+IntToStr(Res));
  InterlockedDecrement(ExecuteCount);
  if ExecuteCount<>0 then 
    Writeln('Error : multiple threads are executing (stop)');
end;

var
  I : integer;
begin
  TCalcThread.ExecuteLock:=TCriticalSection.Create;
  for I:=1 to 10 do
    TCalcThread.Create(i); 
  repeat
    sleep(10);
    CheckSynchronize;
  until (ThreadCount=0);
    
end.


