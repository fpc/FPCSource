unit utcfpmonitor;

{$mode ObjFPC}{$H+}
{ $DEFINE DEBUG_MONITOR}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
{$ifdef windows}
  fpwinmonitor
{$else}
  fpmonitor
{$endif};

const
  WaitPeriod = 10;
  WaitTimeout = 2000;
  MaxObjCount = 2;
  MaxThrdCount = 5;

Type
  TThreadOperation = (toNone,toEnter,toTryEnter,toExit,toPulse,toWait,toPulseAll);
  TOperationResult = Record
    Op : TThreadOperation;
    Tick : Int64;
    Res : Boolean;
  end;

  TTestObject = Class(TObject)
    // Operation/Timestamp when a thread performed a task
    Res : Array[1..MaxThrdCount] of TOperationResult;
  end;

  { TTestThread }
  TTestThread = Class(TThread)
  Private
    FObj : TTestObject;
    FOperation : TThreadOperation;
    FTimeout : Integer;
    FID : Integer;
    Constructor Create(aObj : TTestObject; aOperation : TThreadOperation; aId,aTimeout : Integer; aOnFree : TNotifyEvent);
  Public
    Procedure Execute; override;
  end;
  { TTestMonitorSupport }

  TTestMonitorSupport = Class(TTestCase)
  private
    FThrdCount : Integer;
    FObj : Array[1..MaxObjCount] of  TTestObject;
    FThrd : Array[1..MaxThrdCount] of TThread;
    function DoCreateThread(aObj: TTestObject; aOperation: TThreadOperation; aId, aTimeout: Integer): TTestThread;
    class procedure AssertEquals(Msg: String; aExpected, aActual: TThreadOperation); overload;
    function GetObj(AIndex: Integer): TTestObject;
    procedure ThreadDone(Sender : TObject);
    procedure WaitForAllThreads(aTimeOut: Integer=0);
  public
    Procedure Setup; override;
    Procedure TearDown; override;
    Property Obj1 : TTestObject Index 1 Read GetObj;
    Property Obj2 : TTestObject Index 2 Read GetObj;
  Published
    Procedure TestHookup;
    Procedure TestLock;
    Procedure TestLockMulti;
    Procedure TestTryLock;
    Procedure TestPulse;
    Procedure TestPulseAll;
    procedure TestWait;
  end;

implementation

Uses TypInfo;

{ TTestThread }

constructor TTestThread.Create(aObj: TTestObject; aOperation: TThreadOperation; aId,aTimeout: Integer; aOnFree : TNotifyEvent);
begin
  FObj:=aObj;
  FOperation:=aOperation;
  FTimeout:=aTimeout;
  FID:=aID;
  FreeOnTerminate:=True;
  OnTerminate:=aOnfree;
  Inherited Create(false);
end;

procedure TTestThread.Execute;

var
  OpRes : TOperationResult;

begin
  {$IFDEF DEBUG_MONITOR}  Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadID,' Begin executing operation ',FOperation);{$ENDIF}
  try
    OpRes.Res:=True;
    Case FOperation of
      toEnter : TMonitor.Enter(Fobj);
      toTryEnter : OpRes.Res:=TMonitor.TryEnter(Fobj);
      toExit  : TMonitor.Exit(Fobj);
      toPulse  : begin
                 Sleep(WaitPeriod * 2);
                 TMonitor.Pulse(Fobj);
                 end;
      toPulseAll  :
                 begin
                 TMonitor.Enter(Fobj);
                 OpRes.Res:=TMonitor.Wait(FObj,FTimeout);
                 end;
       toWait :
         begin
         TMonitor.Enter(Fobj);
         OpRes.Res:=TMonitor.Wait(FObj,FTimeout);
         end;
    end;
    OpRes.Tick:=GetTickCount64;
    OpRes.Op:=FOperation;
    FObj.Res[FID]:=OpRes;
    // We need to clean up !
    Case FOperation of
      toEnter,
      toWait,
      toPulseAll,
      toTryEnter:
        begin
        if OpRes.Res then
          begin
          TMonitor.Exit(Fobj);
            {$IFDEF DEBUG_MONITOR}  Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadID,' Unlocking previously locked object ',FOperation);{$ENDIF}
          end;
        end;
    else
      //
    end;

  except
    On E : Exception do
      Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadID,' exception ',E.ClassName,' during operation ',FOperation,' : ',E.Message);
  end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadID,' End executing operation ',FOperation);{$ENDIF}
end;

{ TTestMonitorSupport }

function TTestMonitorSupport.GetObj(AIndex: Integer): TTestObject;
begin
  Result:=FObj[aIndex];
end;

procedure TTestMonitorSupport.ThreadDone(Sender: TObject);

var
  aCount,I : Integer;

begin
  aCount:=0;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadID,' Begin done executing');{$ENDIF}
  For I:=1 to MaxThrdCount do
    begin
    if FThrd[i]=Sender then
      begin
      {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadID,' Done executing: found thread at ',I){$ENDIF};
      FThrd[i]:=Nil;
      end
    else if assigned(FThrd[I]) then
      inc(aCount);
    end;
  {$IFDEF DEBUG_MONITOR}Writeln(StdErr,GetTickCount64,': Thread ',GetCurrentThreadID,' End done executing. Threads still active: ',aCount);{$ENDIF}
end;

procedure TTestMonitorSupport.WaitForAllThreads(aTimeOut : Integer = 0);

var
  I : Integer;
  Last,Start : Int64;
  TimeOut,OK : Boolean;

begin
  If aTimeOut=0 then
    aTimeout:=WaitTimeout;
  Start:=GetTickCount64;
  {$IFDEF DEBUG_MONITOR}  Writeln(StdErr,Start,': Thread ',GetCurrentThreadID,' Waiting for ', FThrdCount,' threads to stop');{$ENDIF}
  Timeout:=False;
  Repeat
    OK:=True;
    CheckSynchronize(5);
    For I:=1 to MaxThrdCount do
      OK:=OK and (FThrd[i]=Nil);
    if not Ok then
      begin
      sleep(10);
      Last:=GetTickCount64;
      TimeOut:=(Last-Start)>aTimeout;
      end;
  Until OK or TimeOut;
  {$IFDEF DEBUG_MONITOR}
  if not OK then
    Writeln(StdErr,Last,': Thread ',GetCurrentThreadId,' Not all threads stopped');
  {$ENDIF}
end;


procedure TTestMonitorSupport.Setup;

var
  I : Integer;

begin
  inherited Setup;
  FThrdCount:=0;
  For I:=1 to MaxObjCount do
    FObj[i]:=TTestObject.Create;
  For I:=1 to MaxThrdCount do
    FThrd[i]:=Nil;
end;

procedure TTestMonitorSupport.TearDown;

var
  I : Integer;

begin
  FThrdCount:=0;
  For I:=1 to MaxObjCount do
    FreeAndNil(FObj[i]);
  For I:=1 to MaxThrdCount do
    FThrd[i]:=Nil;
  inherited TearDown;
end;

procedure TTestMonitorSupport.TestHookup;

var
  I : integer;

begin
  For I:=1 to MaxObjCount do
    AssertNotNull('Obj '+IntToStr(i),FObj[I]);
  For I:=1 to MaxThrdCount do
    AssertNull('Thrd '+IntToStr(i),FThrd[I]);
end;

function TTestMonitorSupport.DoCreateThread(aObj: TTestObject; aOperation: TThreadOperation; aId, aTimeout: Integer): TTestThread;

begin
  Inc(FThrdCount);
  FThrd[FThrdCount]:=TTestThread.Create(aObj,aOperation,Aid,aTimeout,@ThreadDone);
  Result:=TTestThread(FThrd[FThrdCount]);
end;

class procedure TTestMonitorSupport.AssertEquals(Msg: String; aExpected, aActual: TThreadOperation);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TOperationResult),Ord(aExpected)),
                   GetEnumName(TypeInfo(TOperationResult),Ord(aActual)));
end;

procedure TTestMonitorSupport.TestLock;

var
  N : Int64;

begin
  TMonitor.Enter(Obj1);
  DoCreateThread(Obj1,toEnter,1,0);
  Sleep(WaitPeriod);
  N:=GetTickCount64;
  TMonitor.Exit(Obj1);
  WaitForAllThreads;
  AssertTrue('Thread lock timestamp ',N<=FObj[1].Res[1].Tick);
  AssertEquals('Thread did a lock ',toEnter,Obj1.Res[1].Op);
end;

procedure TTestMonitorSupport.TestLockMulti;

var
  N : Int64;
  I : integer;

begin
  TMonitor.Enter(Obj1);
  For I:=1 to MaxThrdCount do
    DoCreateThread(Obj1,toEnter,I,0);
  Sleep(WaitPeriod);
  N:=GetTickCount64;
  TMonitor.Exit(Obj1);
  WaitForAllThreads;
  AssertTrue('Thread lock timestamp ',N<=FObj[1].Res[1].Tick);
  AssertEquals('Thread did a lock ',toEnter,Obj1.Res[1].Op);
end;

procedure TTestMonitorSupport.TestTryLock;

begin
  TMonitor.Enter(Obj1);
  DoCreateThread(Obj1,toTryEnter,1,0);
  Sleep(WaitPeriod);
  TMonitor.Exit(Obj1);
  Writeln(GetTickCount64,': Thread ',GetCurrentThreadID,' Released lock');
  WaitForAllThreads;
  AssertEquals('Thread tried a lock ',toTryEnter,Obj1.Res[1].Op);
  AssertFalse('Thread lock failed ',Obj1.Res[1].Res);
end;

procedure TTestMonitorSupport.TestPulse;

var
  N : Int64;

begin
  // Acquire the lock
  TMonitor.Enter(Obj1);
  DoCreateThread(Obj1,toPulse,1,INFINITE);
  Sleep(WaitPeriod);
  N:=GetTickCount64;
  TMonitor.Wait(Obj1,500);
  TMonitor.Exit(Obj1);
  WaitForAllThreads;
  AssertTrue('Thread pulse timestamp ',N<=FObj[1].Res[1].Tick);
  AssertEquals('Thread did a pulse',toPulse,Obj1.Res[1].Op);
  AssertTrue('Thread Wait was successful',Obj1.Res[1].Res);
end;

procedure TTestMonitorSupport.TestPulseAll;

var
  N : Int64;
  i : integer;

begin
   // Acquire the lock
  For I:=1 to MaxThrdCount do
    DoCreateThread(Obj1,toPulseAll,I,INFINITE);
  Sleep(WaitPeriod*MaxThrdCount);
  N:=GetTickCount64;
  TMonitor.PulseAll(Obj1);
  WaitForAllThreads(WaitTimeOut*MaxThrdCount);
  For I:=0 to MaxThrdCount do
    begin
    AssertEquals('Thread '+IntToStr(i)+' did a Wait',toPulseAll,Obj1.Res[1].Op);
    AssertTrue('Thread '+IntToStr(i)+' Wait was successful',Obj1.Res[1].Res);
    AssertTrue('Thread '+IntToStr(i)+' pulse timestamp ',N<=FObj[1].Res[1].Tick);
    end;
end;

procedure TTestMonitorSupport.TestWait;

var
  N : Int64;

begin
  // Acquire the lock
  DoCreateThread(Obj1,toWait,1,INFINITE);
  Sleep(WaitPeriod*4);
  N:=GetTickCount64;
  TMonitor.Pulse(Obj1);
  WaitForAllThreads;
  AssertEquals('Thread did a Wait',toWait,Obj1.Res[1].Op);
  AssertTrue('Thread Wait was successful',Obj1.Res[1].Res);
  AssertTrue('Thread pulse timestamp ',N<=FObj[1].Res[1].Tick);
end;

initialization
  RegisterTest(TTestMonitorSupport);
end.

