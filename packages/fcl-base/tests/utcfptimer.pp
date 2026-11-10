{ Tests of TFPTimer from fpTimer }
unit utcFPTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpTimer, punit;

// Registers the TFPTimer suite
procedure RegisterTests;

implementation

const
  CMaxTicks = 1000;

type
  { TTickRecorder }

  TTickRecorder = class
  private
    FCount: Integer;
    FSlowDelay: Integer;
    FSlowEnd: QWord;
    FTicks: array[0..CMaxTicks-1] of QWord;
    function GetTick(aIndex: Integer): QWord;
  public
    // Records the time of a tick; the first tick sleeps SlowDelay milliseconds
    procedure DoTimer(Sender: TObject);
    // Number of ticks with a recorded time from aStart up to but not including aStop
    function CountBetween(aStart, aStop: QWord): Integer;
    // Number of ticks recorded
    property Count: Integer read FCount;
    // Time at which tick aIndex was received
    property Ticks[aIndex: Integer]: QWord read GetTick;
    // Milliseconds the first tick sleeps
    property SlowDelay: Integer read FSlowDelay write FSlowDelay;
    // Time at which the first tick returned
    property SlowEnd: QWord read FSlowEnd;
  end;


function TTickRecorder.GetTick(aIndex: Integer): QWord;

begin
  Result:=FTicks[aIndex];
end;


procedure TTickRecorder.DoTimer(Sender: TObject);

var
  lIndex: Integer;

begin
  lIndex:=InterlockedIncrement(FCount)-1;
  if lIndex<CMaxTicks then
    FTicks[lIndex]:=GetTickCount64;
  if (lIndex=0) and (FSlowDelay>0) then
    begin
    Sleep(FSlowDelay);
    FSlowEnd:=GetTickCount64;
    end;
end;


function TTickRecorder.CountBetween(aStart, aStop: QWord): Integer;

var
  i, lCount: Integer;

begin
  Result:=0;
  lCount:=FCount;
  if lCount>CMaxTicks then
    lCount:=CMaxTicks;
  for i:=0 to lCount-1 do
    if (FTicks[i]>=aStart) and (FTicks[i]<aStop) then
      Inc(Result);
end;


{ Creates a timer with the given interval that reports to aRecorder }
function CreateTimer(aRecorder: TTickRecorder; aInterval: Cardinal; aUseTimerThread: Boolean): TFPTimer;

begin
  Result:=TFPTimer.Create(nil);
  Result.Interval:=aInterval;
  Result.UseTimerThread:=aUseTimerThread;
  Result.OnTimer:=@aRecorder.DoTimer;
end;


{ Calls CheckSynchronize for aDuration milliseconds }
procedure PumpSynchronize(aDuration: Integer);

var
  lStop: QWord;

begin
  lStop:=GetTickCount64+aDuration;
  while GetTickCount64<lStop do
    CheckSynchronize(5);
end;


{ Disables aTimer, lets its thread finish and frees it }
procedure FreeTimer(aTimer: TFPTimer; aPump: Boolean);

begin
  aTimer.Enabled:=False;
  if aPump then
    PumpSynchronize(150)
  else
    Sleep(150);
  aTimer.Free;
end;


function TestTicks: TTestString;

var
  lRecorder: TTickRecorder;
  lTimer: TFPTimer;
  lCount: Integer;

begin
  Result:='';
  lRecorder:=TTickRecorder.Create;
  try
    lTimer:=CreateTimer(lRecorder,50,True);
    lTimer.Enabled:=True;
    Sleep(525);
    lCount:=lRecorder.Count;
    FreeTimer(lTimer,False);
    AssertTrue('Timer of 50 ms ticks at least 5 times in 525 ms, got '+IntToStr(lCount), lCount>=5);
    AssertTrue('Timer of 50 ms ticks at most 11 times in 525 ms, got '+IntToStr(lCount), lCount<=11);
  finally
    lRecorder.Free;
  end;
end;


function TestStop: TTestString;

var
  lRecorder: TTickRecorder;
  lTimer: TFPTimer;
  lCount: Integer;

begin
  Result:='';
  lRecorder:=TTickRecorder.Create;
  try
    lTimer:=CreateTimer(lRecorder,20,True);
    try
      lTimer.Enabled:=True;
      Sleep(150);
      lTimer.Enabled:=False;
      Sleep(50);
      lCount:=lRecorder.Count;
      AssertTrue('Timer ticked while enabled', lCount>0);
      Sleep(150);
      AssertEquals('No ticks after the timer is disabled', lCount, lRecorder.Count);
    finally
      FreeTimer(lTimer,False);
    end;
  finally
    lRecorder.Free;
  end;
end;


function TestIntervalChange: TTestString;

var
  lRecorder: TTickRecorder;
  lTimer: TFPTimer;
  lChange: QWord;
  lCount: Integer;

begin
  Result:='';
  lRecorder:=TTickRecorder.Create;
  try
    lTimer:=CreateTimer(lRecorder,10000,True);
    lTimer.Enabled:=True;
    Sleep(100);
    AssertEquals('No tick before the first interval of 10 s elapsed', 0, lRecorder.Count);
    lChange:=GetTickCount64;
    lTimer.Interval:=50;
    Sleep(1000);
    FreeTimer(lTimer,False);
    AssertTrue('Timer ticks after the interval is changed to 50 ms', lRecorder.Count>0);
    AssertTrue('First tick within 700 ms after the interval change', lRecorder.Ticks[0]-lChange<700);
    lCount:=lRecorder.CountBetween(lChange+700,lChange+1000);
    AssertTrue('Timer with the new interval of 50 ms ticks at least 3 times in 300 ms, got '+IntToStr(lCount), lCount>=3);
    AssertTrue('Timer with the new interval of 50 ms ticks at most 7 times in 300 ms, got '+IntToStr(lCount), lCount<=7);
  finally
    lRecorder.Free;
  end;
end;


function TestSlowHandlerNoBurst: TTestString;

var
  lRecorder: TTickRecorder;
  lTimer: TFPTimer;
  lBurst: Integer;

begin
  Result:='';
  lRecorder:=TTickRecorder.Create;
  try
    lRecorder.SlowDelay:=200;
    lTimer:=CreateTimer(lRecorder,50,True);
    lTimer.Enabled:=True;
    Sleep(500);
    FreeTimer(lTimer,False);
    AssertTrue('Slow first tick returned', lRecorder.SlowEnd>0);
    lBurst:=lRecorder.CountBetween(lRecorder.SlowEnd,lRecorder.SlowEnd+30);
    AssertTrue('Ticks missed during a handler of 4 intervals are not replayed, got '+IntToStr(lBurst)+' ticks within 30 ms', lBurst<=1);
    AssertTrue('Timer keeps ticking after a slow handler', lRecorder.CountBetween(lRecorder.SlowEnd,High(QWord))>=2);
  finally
    lRecorder.Free;
  end;
end;


function TestBlockedSynchronizeNoBurst: TTestString;

var
  lRecorder: TTickRecorder;
  lTimer: TFPTimer;
  lStart: QWord;
  lBurst: Integer;

begin
  Result:='';
  lRecorder:=TTickRecorder.Create;
  try
    lTimer:=CreateTimer(lRecorder,50,False);
    lTimer.Enabled:=True;
    Sleep(300);
    lStart:=GetTickCount64;
    PumpSynchronize(300);
    FreeTimer(lTimer,True);
    lBurst:=lRecorder.CountBetween(lStart,lStart+30);
    AssertTrue('Ticks missed while the main thread is blocked for 6 intervals are not replayed, got '+IntToStr(lBurst)+' ticks within 30 ms', lBurst<=2);
    AssertTrue('Timer keeps ticking once the main thread synchronizes', lRecorder.Count>=3);
  finally
    lRecorder.Free;
  end;
end;


procedure RegisterTests;

var
  lSuite: PSuite;

begin
  lSuite:=AddSuite('TFPTimer');
  AddTest('Ticks', @TestTicks, lSuite);
  AddTest('Stop', @TestStop, lSuite);
  AddTest('IntervalChange', @TestIntervalChange, lSuite);
  AddTest('SlowHandlerNoBurst', @TestSlowHandlerNoBurst, lSuite);
  AddTest('BlockedSynchronizeNoBurst', @TestBlockedSynchronizeNoBurst, lSuite);
end;


end.
