{ Tests of TSynchroObject.WaitFor and TSpinWait.SpinUntil from SyncObjs }
unit utcSynchroObject;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}

interface

uses
  Classes, SysUtils, {$IFNDEF VER3_2}System.TimeSpan, {$ENDIF}SyncObjs, punit;

// Registers the TSynchroObject and TSpinWait suites
procedure RegisterTests;

implementation

type
  { TRecordingObject }

  TRecordingObject = class(TSynchroObject)
  private
    FTimeout: Cardinal;
  public
    // Records aTimeout and returns wrSignaled
    function WaitFor(aTimeout: Cardinal = INFINITE): TWaitResult; overload; override;
    // Timeout passed to the last WaitFor call
    property Timeout: Cardinal read FTimeout;
  end;

  { THoldThread }

  THoldThread = class(TThread)
  private
    FSection: TCriticalSection;
    FHeld: TEvent;
    FHoldTime: Integer;
  protected
    procedure Execute; override;
  public
    // Creates a started thread that enters aSection, sets aHeld and leaves aSection after aHoldTime milliseconds
    constructor Create(aSection: TCriticalSection; aHeld: TEvent; aHoldTime: Integer);
  end;


function TRecordingObject.WaitFor(aTimeout: Cardinal): TWaitResult;

begin
  FTimeout:=aTimeout;
  Result:=wrSignaled;
end;


constructor THoldThread.Create(aSection: TCriticalSection; aHeld: TEvent; aHoldTime: Integer);

begin
  FSection:=aSection;
  FHeld:=aHeld;
  FHoldTime:=aHoldTime;
  inherited Create(False);
end;


procedure THoldThread.Execute;

begin
  FSection.Enter;
  try
    FHeld.SetEvent;
    Sleep(FHoldTime);
  finally
    FSection.Leave;
  end;
end;


{ Returns the result of aObject.WaitFor(aTimeout) through a TSynchroObject reference }
function BaseWaitFor(aObject: TSynchroObject; aTimeout: Cardinal): TWaitResult;

begin
  Result:=aObject.WaitFor(aTimeout);
end;


function TestBaseWaitFor: TTestString;

var
  lObject: TSynchroObject;

begin
  Result:='';
  lObject:=TSynchroObject.Create;
  try
    AssertTrue('TSynchroObject.WaitFor returns wrError', lObject.WaitFor=wrError);
    AssertTrue('TSynchroObject.WaitFor(0) returns wrError', lObject.WaitFor(0)=wrError);
{$IFNDEF VER3_2}
    AssertTrue('TSynchroObject.WaitFor with a TTimeSpan returns wrError', lObject.WaitFor(TTimeSpan.FromMilliseconds(10))=wrError);
{$ENDIF}
  finally
    lObject.Free;
  end;
end;


function TestOverride: TTestString;

var
  lObject: TRecordingObject;

begin
  Result:='';
  lObject:=TRecordingObject.Create;
  try
    AssertTrue('Overridden WaitFor is called through a TSynchroObject reference', BaseWaitFor(lObject, 42)=wrSignaled);
    AssertEquals('Overridden WaitFor receives the timeout', 42, lObject.Timeout);
    lObject.WaitFor;
    AssertEquals('WaitFor without arguments waits INFINITE', INFINITE, lObject.Timeout);
{$IFNDEF VER3_2}
    AssertTrue('TTimeSpan WaitFor calls the overridden WaitFor', lObject.WaitFor(TTimeSpan.FromMilliseconds(250))=wrSignaled);
    AssertEquals('TTimeSpan WaitFor passes the timeout in milliseconds', 250, lObject.Timeout);
{$ENDIF}
  finally
    lObject.Free;
  end;
end;


function TestEvents: TTestString;

var
  lEvent: TEvent;
  lSimple: TSimpleEvent;
  lCountdown: TCountdownEvent;

begin
  Result:='';
  lEvent:=TEvent.Create(nil, True, False, '');
  try
    AssertTrue('TEvent that is not set times out through TSynchroObject', BaseWaitFor(lEvent, 0)=wrTimeout);
    lEvent.SetEvent;
    AssertTrue('TEvent that is set is signaled through TSynchroObject', BaseWaitFor(lEvent, 0)=wrSignaled);
{$IFNDEF VER3_2}
    AssertTrue('TEvent that is set is signaled with a TTimeSpan', lEvent.WaitFor(TTimeSpan.FromMilliseconds(10))=wrSignaled);
{$ENDIF}
  finally
    lEvent.Free;
  end;
  lSimple:=TSimpleEvent.Create;
  try
    AssertTrue('TSimpleEvent that is not set times out through TSynchroObject', BaseWaitFor(lSimple, 0)=wrTimeout);
    lSimple.SetEvent;
    AssertTrue('TSimpleEvent that is set is signaled through TSynchroObject', BaseWaitFor(lSimple, 0)=wrSignaled);
  finally
    lSimple.Free;
  end;
  lCountdown:=TCountdownEvent.Create(1);
  try
    AssertTrue('TCountdownEvent with a count of 1 times out through TSynchroObject', BaseWaitFor(lCountdown, 0)=wrTimeout);
    lCountdown.Signal;
    AssertTrue('TCountdownEvent at 0 is signaled through TSynchroObject', BaseWaitFor(lCountdown, 0)=wrSignaled);
{$IFNDEF VER3_2}
    AssertTrue('TCountdownEvent at 0 is signaled with a TTimeSpan', lCountdown.WaitFor(TTimeSpan.FromMilliseconds(10))=wrSignaled);
{$ENDIF}
  finally
    lCountdown.Free;
  end;
end;


function TestCriticalSection: TTestString;

var
  lSection: TCriticalSection;
  lHeld: TEvent;
  lThread: THoldThread;
  lStart: QWord;

begin
  Result:='';
  lHeld:=TEvent.Create(nil, True, False, '');
  lSection:=TCriticalSection.Create;
  try
    AssertTrue('WaitFor(0) enters a free critical section', BaseWaitFor(lSection, 0)=wrSignaled);
    lSection.Release;
    AssertTrue('WaitFor enters a free critical section', lSection.WaitFor=wrSignaled);
    lSection.Release;
    lThread:=THoldThread.Create(lSection, lHeld, 300);
    try
      AssertTrue('Holding thread entered the critical section', lHeld.WaitFor(5000)=wrSignaled);
      AssertTrue('WaitFor(0) times out on a held critical section', lSection.WaitFor(0)=wrTimeout);
      lStart:=GetTickCount64;
      AssertTrue('WaitFor(50) times out on a held critical section', lSection.WaitFor(50)=wrTimeout);
      AssertTrue('WaitFor(50) waits for the timeout', GetTickCount64-lStart>=40);
      AssertTrue('WaitFor(5000) enters the critical section once it is left', lSection.WaitFor(5000)=wrSignaled);
      lSection.Release;
      lThread.WaitFor;
    finally
      lThread.Free;
    end;
    lHeld.ResetEvent;
    lThread:=THoldThread.Create(lSection, lHeld, 100);
    try
      AssertTrue('Holding thread entered the critical section again', lHeld.WaitFor(5000)=wrSignaled);
      AssertTrue('WaitFor(INFINITE) enters the critical section once it is left', lSection.WaitFor(INFINITE)=wrSignaled);
      lSection.Release;
      lThread.WaitFor;
    finally
      lThread.Free;
    end;
  finally
    lSection.Free;
    lHeld.Free;
  end;
end;


var
  gCalls: Integer;


{ Returns True from the third call on }
function TrueOnThirdCall: Boolean;

begin
  Inc(gCalls);
  Result:=gCalls>=3;
end;


var
  gDeadline: QWord;


{ Returns True once gDeadline has passed }
function TrueAfterDeadline: Boolean;

begin
  Result:=GetTickCount64>=gDeadline;
end;


{ Returns False }
function AlwaysFalse: Boolean;

begin
  Result:=False;
end;


{ Returns True }
function AlwaysTrue: Boolean;

begin
  Result:=True;
end;


function TestSpinUntil: TTestString;

var
  lStart: QWord;

begin
  Result:='';
  AssertTrue('SpinUntil with a Cardinal timeout returns True for a true condition', TSpinWait.SpinUntil(@AlwaysTrue, 100));
  gCalls:=0;
  AssertTrue('SpinUntil with a Cardinal timeout returns True once the condition becomes true', TSpinWait.SpinUntil(@TrueOnThirdCall, 1000));
  AssertEquals('SpinUntil stops evaluating once the condition is true', 3, gCalls);
  AssertFalse('SpinUntil with a Cardinal timeout returns False for a false condition', TSpinWait.SpinUntil(@AlwaysFalse, 0));
  gDeadline:=GetTickCount64+200;
  AssertTrue('SpinUntil keeps spinning until a condition that becomes true within the timeout', TSpinWait.SpinUntil(@TrueAfterDeadline, 5000));
  lStart:=GetTickCount64;
  AssertFalse('SpinUntil(100) returns False for a false condition', TSpinWait.SpinUntil(@AlwaysFalse, 100));
  AssertTrue('SpinUntil(100) spins until the timeout', GetTickCount64-lStart>=90);
{$IFNDEF VER3_2}
  gDeadline:=GetTickCount64+200;
  AssertTrue('SpinUntil with a TTimeSpan keeps spinning until the condition becomes true', TSpinWait.SpinUntil(@TrueAfterDeadline, TTimeSpan.FromMilliseconds(5000)));
  AssertTrue('SpinUntil with a TTimeSpan returns True for a true condition', TSpinWait.SpinUntil(@AlwaysTrue, TTimeSpan.FromMilliseconds(100)));
  AssertFalse('SpinUntil with a TTimeSpan returns False for a false condition', TSpinWait.SpinUntil(@AlwaysFalse, TTimeSpan.FromMilliseconds(0)));
{$ENDIF}
end;


procedure RegisterTests;

var
  lSuite: PSuite;

begin
  lSuite:=AddSuite('TSynchroObject');
  AddTest('BaseWaitFor', @TestBaseWaitFor, lSuite);
  AddTest('Override', @TestOverride, lSuite);
  AddTest('Events', @TestEvents, lSuite);
  AddTest('CriticalSection', @TestCriticalSection, lSuite);
  lSuite:=AddSuite('TSpinWait');
  AddTest('SpinUntil', @TestSpinUntil, lSuite);
end;


end.
