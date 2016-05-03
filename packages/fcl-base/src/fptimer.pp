{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  A generic timer component. Can be used in GUI and non-GUI apps.
  Based heavily on an idea by Graeme Geldenhuys, extended so
  the tick mechanism is pluggable.
  
  Note that the system implementation will only work for timers
  in the main thread, as it uses synchronize to do the job.
  You need to enable threads in your application for the system
  implementation to work.
  
  A nice improvement would be an implementation that works
  in all threads, such as the threadedtimer of IBX for linux.

  Replaced SLEEP with TEvent for those platforms supporting threading:
           Windows, Linux, BSD.
  On the other platforms, use sleep. This unfortunately has a high overhead
     resulting in drift.  A five minute timer could be up to 40 seconds late
     do to entering and returning (linux x64).  MOdified to check the absolute
     time every minute, has reduced that lag to about 0.100 second.  This is
     still greater than TEvent, where the delay is only a few milliseconds (0-3).     
}

unit fptimer;

{$mode objfpc}{$H+}

{ 
  Windows, or any platform that uses Cthreads has TEvent with a timed wait
  which can include android and embedded.
  You can force the use of the Sleep() based timer by defining USESLEEP
}

{$IFNDEF USESLEEP}
{$if Defined(MSWINDOWS) or (Defined(UNIX) and not Defined(BEOS))}
{$define Has_EventWait}
{$endif}
{$ENDIF}

interface

uses
  Classes;

type
  TFPTimerDriver = Class;

  { TFPCustomTimer }

  TFPCustomTimer = class(TComponent)
  private
    FDriver : TFPTimerDriver;
    FOnStartTimer : TNotifyEvent;
    FOnStopTimer : TNotifyEvent;
    FOnTimer : TNotifyEvent;
    FInterval : Cardinal;
    FActive : Boolean;
    FEnabled : Boolean;
    FUseTimerThread : Boolean;
    procedure SetEnabled(const AValue: Boolean );
    procedure SetInterval(const AValue: Cardinal);
  protected
    property Active: Boolean read FActive write FActive;
    Function CreateTimerDriver : TFPTimerDriver;
    procedure Timer; virtual;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure StartTimer; virtual;
    procedure StopTimer; virtual;
  protected
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Cardinal read FInterval write SetInterval;
    property UseTimerThread: Boolean read FUseTimerThread write FUseTimerThread;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property OnStartTimer: TNotifyEvent read FOnStartTimer write FOnStartTimer;
    property OnStopTimer: TNotifyEvent read FOnStopTimer write FOnStopTimer;
  end;

  TFPTimer = Class(TFPCustomTimer)
  Published
    Property Enabled;
    Property Interval;
    Property UseTimerThread;
    Property OnTimer;
    Property OnStartTimer;
    Property OnStopTimer;
  end;

  { TFPTimerDriver }

  TFPTimerDriver = Class(TObject)
  Protected
    FTimer : TFPCustomTimer;
    FTimerStarted : Boolean;
    procedure SetInterval(const AValue: Cardinal); virtual;
  Public
    Constructor Create(ATimer : TFPCustomTimer); virtual;
    Procedure StartTimer; virtual; abstract;
    Procedure StopTimer; virtual; abstract;
    Property Timer : TFPCustomTimer Read FTimer;
    property TimerStarted: Boolean read FTimerStarted;
  end;
  TFPTimerDriverClass = Class of TFPTimerDriver;

Var
  DefaultTimerDriverClass : TFPTimerDriverClass = Nil;

implementation

uses
  SysUtils;

{ ---------------------------------------------------------------------
    TFPTimer
  ---------------------------------------------------------------------}

constructor TFPCustomTimer.Create(AOwner: TComponent);
begin
  inherited;
  FDriver:=CreateTimerDriver;
end;

destructor TFPCustomTimer.Destroy;

begin
  StopTimer;
  FDriver.FTimer:=Nil;
  FreeAndNil(FDriver);
  Inherited;
end;


Function TFPCustomTimer.CreateTimerDriver : TFPTimerDriver;

begin
  Result:=DefaultTimerDriverClass.Create(Self);
end;

procedure TFPCustomTimer.SetEnabled(const AValue: Boolean);
begin
  if AValue <> FEnabled then
    begin
    FEnabled := AValue;
    if FEnabled then
      StartTimer
    else
      StopTimer;
    end;
end;

procedure TFPCustomTimer.SetInterval(const AValue: Cardinal);
begin
  if FInterval <> AValue then
    begin
    fInterval := AValue;
    if FActive and (fInterval > 0) then
      FDriver.SetInterval(AValue)  // Allow driver to update Interval
    else
      StopTimer;                   // Timer not required
    end;
end;

procedure TFPCustomTimer.StartTimer;
var
  IsActive: Boolean;
begin
  IsActive:=FEnabled and (fInterval > 0) and Assigned(FOnTimer);
  If IsActive and not fActive and Not (csDesigning in ComponentState) then
    begin
    FDriver.StartTimer;
    if FDriver.TimerStarted then
      begin
      FActive := True;
      if Assigned(OnStartTimer) then
        OnStartTimer(Self);
      end;
    end;
end;

procedure TFPCustomTimer.StopTimer;
begin
  if FActive then
    begin
    FDriver.StopTimer;
    if not FDriver.TimerStarted then
      begin
      FActive:=False;
      if Assigned(OnStopTimer) then
        OnStopTimer(Self);
      end;
    end;
end;

procedure TFPCustomTimer.Timer;

begin
  { We check on FEnabled: If by any chance a tick comes in after it was
    set to false, the user won't notice, since no event is triggered.}
  If FActive and Assigned(FOnTimer) then
    FOnTimer(Self);
end;

{ ---------------------------------------------------------------------
  TFPTimerDriver
  ---------------------------------------------------------------------}

Constructor TFPTimerDriver.Create(ATimer : TFPCustomTimer);

begin
  FTimer:=ATimer;
end;

procedure TFPTimerDriver.SetInterval(const AValue: Cardinal);
begin
  // Default implementation is to restart the timer on Interval change
  if TimerStarted then
    begin
    StopTimer;
    FTimerStarted := (AValue > 0);
    if FTimerStarted then
      StartTimer;
    end;
end;


{ ---------------------------------------------------------------------
    Default implementation. Threaded timer, one thread per timer.
  ---------------------------------------------------------------------}

const
  cMilliSecs: Extended = 60.0 * 60.0 * 24.0 * 1000.0;
  
Type

  { TFPTimerThread }

  TFPTimerThread = class(TThread)
  private
    FTimerDriver: TFPTimerDriver;
    FStartTime : TDateTime;
    {$ifdef Has_EventWait}
    FWaitEvent: PEventState;
    {$else}
    fSignaled: Boolean;
    {$endif}
    fInterval: Cardinal;
    Function Timer : TFPCustomTimer;
    Function GetWakeTime(var AInterval,Counter : Int64; Out WakeInterval : Integer; Out WakeTime : TDateTime) : Boolean;
  public
    procedure Execute; override;
    constructor CreateTimerThread(ATimerDriver: TFPTimerDriver);
    procedure Terminate;
    procedure SetInterval(const AValue: Cardinal);
  end;

  { TFPThreadedTimerDriver }

  TFPThreadedTimerDriver = Class(TFPTimerDriver)
  Private
    FThread : TFPTimerThread;
  protected
    Procedure SetInterval(const AValue: cardinal); override;
  Public
    Procedure StartTimer; override;
    Procedure StopTimer; override;
  end;

{ ---------------------------------------------------------------------
    TFPTimerThread
  ---------------------------------------------------------------------}

constructor TFPTimerThread.CreateTimerThread(ATimerDriver: TFPTimerDriver);
begin
  inherited Create(True);
  FTimerDriver:=ATimerDriver;
  {$ifdef Has_EventWait}
  FWaitEvent := BasicEventCreate(nil,false,false,'');
  {$else}
  fSignaled := False;
  {$endif}
  fInterval := ATimerDriver.Timer.Interval;
  FreeOnTerminate := True;
end;

procedure TFPTimerThread.Terminate;
begin
  inherited Terminate;
  {$ifdef Has_EventWait}
  BasicEventSetEvent(fWaitEvent);
  {$else}
  fSignaled := True;
  {$endif}
end;

procedure TFPTimerThread.SetInterval(const AValue: Cardinal);
begin
  if fInterval <> AValue then
    begin
    fInterval := AValue;
    {$ifdef Has_EventWait}
    BasicEventSetEvent(fWaitEvent);   // Wake thread
    {$else}
    fSignaled := True;
    {$endif}
    end;
end;

Function TFPTimerThread.Timer : TFPCustomTimer;

begin
  If Assigned(FTimerDriver) Then
    Result:=FTimerDriver.FTimer
  else
    Result:=Nil;
end;

Function TFPTimerThread.GetWakeTime(var AInterval,Counter : Int64; Out WakeInterval : Longint; Out WakeTime : TDateTime) : Boolean;


Var
  Diff: Extended;
   
begin
    { Use Counter*fInterval to avoid numerical errors resulting from adding
      small values (AInterval/cMilliSecs) to a large real number (TDateTime),
      even when using Extended precision }
  WakeTime := FStartTime + (Counter*AInterval / cMilliSecs);
  Diff := (WakeTime - Now);
  if Diff > 0 then
    begin
    WakeInterval := Trunc(Diff * cMilliSecs);
    if WakeInterval < 10 then
      WakeInterval := 10;    // Provide a minimum wait time
    end
  else
    begin
    WakeInterval:=MaxInt;
    // Time has already expired, execute Timer and restart wait loop
    try
      if not Timer.UseTimerThread then
        Synchronize(@Timer.Timer)  // Call user event
      else
        Timer.Timer;
    except
      // Trap errors to prevent this thread from terminating
    end;
    Inc(Counter);
    Result:=True;
    end;
end;

{$ifdef Has_EventWait}
procedure TFPTimerThread.Execute;
var
  WakeTime, StartTime: TDateTime;
  WakeInterval: Integer;
  Counter: int64; { use Int64 to avoid overflow with Counter*fInterval (~49 days)}
  AInterval: int64;
  Diff: Extended;
  
Const
  wrSignaled = 0;
  wrTimeout  = 1;
  wrAbandoned= 2;
  wrError    = 3;
  
begin
  WakeInterval := MaxInt;
  Counter := 1;
  AInterval := fInterval;
  FStartTime := Now;
  while not Terminated do
    begin
    if GetWakeTime(AInterval,Counter,WakeInterval,WakeTime) then 
      Continue;
    if not Terminated then
      case BasicEventWaitFor(WakeInterval,fWaitEvent) of
      wrTimeout:
        begin
        if Terminated then
          Break
        else
          begin
          try
            if not Timer.UseTimerThread then
              // If terminate is called while here, then the Synchronize will be
              // queued while the stoptimer is being processed.
              // StopTimer cannot wait until thread completion as this would deadlock
              Synchronize(@Timer.Timer)  // Call user event
            else
              Timer.Timer;
          except
            // Trap errors to prevent this thread from terminating
          end;
          Inc(Counter);                // Next interval
          end;
        end;
      wrSignaled:
        begin
        if Terminated then
          Break
        else 
          begin                      // Interval has changed
          Counter := 1;              // Restart timer without creating new thread
          AInterval := fInterval;
          FStartTime := Now; 
          end;
        end;
      else
        Break;
      end
    end;
  BasicEventDestroy(fWaitEvent);
end;

{$ELSE Has_EventWait}

procedure TFPTimerThread.Execute;

var
  WakeTime, StartTime: TDateTime;
  WakeInterval: Integer;
  Counter: int64; { use Int64 to avoid overflow with Counter*fInterval (~49 days)}
  AInterval: int64;
  Diff: Extended;
  S,Last: Cardinal;
  RecheckTimeCounter: integer;
  
const
  cSleepTime = 500;           // 0.5 second, better than every 5 milliseconds
  cRecheckTimeCount = 120;    // Recheck clock every minute, as the sleep loop can loose time
  
begin
  WakeInterval := MaxInt;
  Counter := 1;
  AInterval := fInterval;
  FStartTime := Now;
  while not Terminated do
    begin
    if GetWakeTime(AInterval,Counter,WakeInterval,WakeTime) then
      Continue;
    if not Terminated then
      begin
      RecheckTimeCounter := cRecheckTimeCount;
      s := cSleepTime;
      repeat
        if s > WakeInterval then
          s := WakeInterval;
        sleep(s);
        if fSignaled then            // Terminated or interval has changed
          begin
          if not Terminated then
            begin
            fSignaled := False;
            Counter := 1;            // Restart timer
            AInterval := fInterval;
            StartTime := Now;
            end;
          break;                     // Need to break out of sleep loop
          end;

        dec(WakeInterval,s);         // Update total wait time
        dec(RecheckTimeCounter);     // Do we need to recheck current time
        if (RecheckTimeCounter < 0) and (WakeInterval > 0) then
          begin
          Diff := (WakeTime - Now);
          WakeInterval := Trunc(Diff * cMilliSecs);
          RecheckTimeCounter := cRecheckTimeCount;
          s := cSleepTime;
          end;
      until (WakeInterval<=0) or Terminated;
      if WakeInterval <= 0 then
        try
          inc(Counter);
          if not Timer.UseTimerThread then
            // If terminate is called while here, then the Synchronize will be
            // queued while the stoptimer is being processed.
            // StopTimer cannot wait until thread completion as this would deadlock
            Synchronize(@Timer.Timer)  // Call user event
          else
            Timer.Timer;
        except
          // Trap errors to prevent this thread from terminating
        end;
      end
    end;
end;
{$ENDIF Has_EventWait}
{ ---------------------------------------------------------------------
    TFPThreadedTimerDriver
  ---------------------------------------------------------------------}

procedure TFPThreadedTimerDriver.SetInterval(const AValue: cardinal);
begin
  if FThread <> nil then
    begin
    if AValue > 0 then
      FThread.SetInterval(AValue)
    else
      StopTimer;
    end;
end;

Procedure TFPThreadedTimerDriver.StartTimer;

begin
  if FThread = nil then
    begin
    FThread:=TFPTimerThread.CreateTimerThread(Self);
    FThread.Start;
    FTimerStarted := True;
    end;
end;

Procedure TFPThreadedTimerDriver.StopTimer;
begin
  if FThread <> nil then
    begin
    try
      // Cannot wait on thread in case
      // 1.  this is called in a Synchonize method and the FThread is
      //     about to run a synchronize method. In these cases we would have a deadlock
      // 2.  In a DLL and this is called as part of DLLMain, which never
      //     returns endthread (hence WaitFor) until DLLMain is exited
      FThread.Terminate;   // Will call FThread.Wake;
    finally
      FThread := nil;
    end;
    FTimerStarted := False;
    end;
end;


Initialization
  DefaultTimerDriverClass:=TFPThreadedTimerDriver;
end.
