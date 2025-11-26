{
    This file is part of the Free Component Library

    Webassembly Timer API - Objects layer.
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.timer.objects;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.SyncObjs,
{$ELSE}
  Classes, SysUtils, SyncObjs,
{$ENDIF}
  wasm.timer.api, wasm.timer.shared;

{ TTimer }
Type
  EWasmTimer = Class(Exception);

  { TWasmTimer }

  TWasmTimer = Class(TObject)
  Private
    FOnTimer : TNotifyEvent;
    FSender : TObject;
    FID : TWasmTimerID;
    FInterval : Integer;
  Public
    Constructor Create(aInterval : Integer; aEvent : TNotifyEvent; aSender : TObject);
    destructor Destroy; override;
    Procedure Execute;
    property OnTimer : TNotifyEvent Read FOnTimer;
    Property ID : TWasmTimerID Read FID;
    class procedure HandleWasmTimer(aTimerID: TWasmTimerID; userdata: pointer; var aContinue: Boolean); static;
    class function getPerformanceNow : Double;
  end;

  TTimer = Class(TComponent)
  private
    FTimer : TWasmTimer;
    FEnabled: Boolean;
    FInterval: Integer;
    FOnTimer: TNotifyEvent;
    procedure SetEnabled(AValue: Boolean);
    procedure SetInterval(AValue: Integer);
    procedure SetOnTimer(AValue: TNotifyEvent);
  protected
    procedure DoOnTimer(Sender: TObject); virtual;
    procedure CheckEnabled; virtual;
    procedure Loaded; override;
  public
    Destructor Destroy; override;
  Published
    Property Enabled : Boolean Read FEnabled Write SetEnabled;
    Property Interval : Integer Read FInterval Write SetInterval;
    Property OnTimer : TNotifyEvent Read FOnTimer Write SetOnTimer;
  end;

implementation

uses wasm.logger.api;

resourcestring
  SErrCouldNotCreateTimer = 'Could not create timer';

var
  ActiveTimers : TFPList;
  Lock : TCriticalSection;

procedure AddTimer(aTimer : TWasmTimer);
begin
  Lock.Enter;
  try
    ActiveTimers.Add(aTimer);
  finally
    Lock.Leave
  end;
end;

function ValidTimer(aTimer : TWasmTimer): boolean;
begin
  Lock.Enter;
  try
    Result:=ActiveTimers.IndexOf(aTimer)<>-1;
  finally
    Lock.Leave;
  end;
end;

procedure RemoveTimer(aTimer : TWasmTimer);

begin
  Lock.Enter;
  try
    ActiveTimers.Remove(aTimer);
  finally
    Lock.Leave;
  end;
end;

constructor TWasmTimer.Create(aInterval: Integer; aEvent: TNotifyEvent; aSender: TObject);
begin
  FOnTimer:=aEvent;
  FSender:=aSender;
  FInterval:=aInterval;
  FID:=__wasm_timer_allocate(aInterval,Self);
  if (FID=0) then
    begin
    __wasmtimer_log(wllError,SErrCouldNotCreateTimer);
    Raise EWasmTimer.Create(SErrCouldNotCreateTimer);
    end;
  AddTimer(Self);
end;

destructor TWasmTimer.Destroy;
begin
  FOnTimer:=Nil;
  RemoveTimer(Self);
  __wasm_timer_deallocate(FID);
  inherited Destroy;
end;

procedure TWasmTimer.Execute;
begin
  if assigned(FOnTimer) then
    FOnTimer(FSender);
end;

class procedure TWasmTimer.HandleWasmTimer(aTimerID: TWasmTimerID; userdata: pointer; var aContinue: Boolean);

var
  Obj : TWasmTimer absolute userdata;

begin
  __wasmtimer_log(wllTrace, 'Timer(ID: %d) tick. Data [%p]',[aTimerID,UserData]);
  aContinue:=ValidTimer(Obj) and (Obj.FID=aTimerID);
  __wasmtimer_log(wllDebug, 'Timer(id: %d) tick. Data [%p] continue: %b',[aTimerID,UserData,aContinue]);
  if aContinue then
    Obj.Execute;
end;

class function TWasmTimer.getPerformanceNow: Double;
begin
  if __wasm_timer_performance_now(@Result)<>ETIMER_SUCCESS then
    begin
    __wasmtimer_log(wllError, 'No performance timer available');
    Raise EWasmTimer.Create('No performance timer available');
    end;
end;

{ TTimer }

procedure TTimer.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  if csDesigning in ComponentState then
    exit;
  CheckEnabled;
end;

procedure TTimer.SetInterval(AValue: Integer);
begin
  if FInterval=AValue then Exit;
  FInterval:=AValue;
end;

procedure TTimer.SetOnTimer(AValue: TNotifyEvent);
begin
  if FOnTimer=AValue then Exit;
  FOnTimer:=AValue;
end;


procedure TTimer.DoOnTimer(Sender : TObject);

begin
  If FEnabled and Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TTimer.CheckEnabled;

begin
  if FEnabled then
    begin
    if Assigned(FTimer) or (Interval=0) then
      FreeAndNil(FTimer)
    else
      FTimer:=TWasmTimer.Create(Interval,@DoOnTimer,Self);
    end
  else
    FreeAndNil(FTimer);
end;

procedure TTimer.Loaded;
begin
  inherited Loaded;
  CheckEnabled;
end;

destructor TTimer.Destroy;
begin
  OnTimer:=Nil;
  Enabled:=False;

  inherited Destroy;
end;

initialization
  ActiveTimers:=TFPList.Create;
  Lock:=TCriticalSection.Create;
  OnWasmTimerTick:=@TWasmTimer.HandleWasmTimer

finalization
  OnWasmTimerTick:=Nil;
  FreeAndNil(ActiveTimers);
  FreeAndNil(Lock);
end.

