{
    $Id$

    fpAsync: Asynchronous event management for Free Pascal
    Copyright (C) 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Common implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


type
  PNotifyData = ^TNotifyData;
  TNotifyData = record
    Next: PNotifyData;
    Notify: TNotifyEvent;
    Sender: TObject;
    case Boolean of
      False: (TimerHandle: TAsyncTimer);
      True: (FileHandle: LongInt);
  end;


procedure EventHandler(Data: Pointer); cdecl;
begin
  with PNotifyData(Data)^ do
    Notify(Sender);
end;


function AddNotifyData(Obj: TEventLoop): PNotifyData;
begin
  New(Result);
  Result^.Next := PNotifyData(Obj.FFirstNotifyData);
  Obj.FFirstNotifyData := Result;
end;

procedure FreeNotifyData(Obj: TEventLoop; Data: PNotifyData);
var
  CurData, PrevData, NextData: PNotifyData;
begin
  PrevData := nil;
  CurData := Obj.FFirstNotifyData;
  while Assigned(CurData) do
  begin
    NextData := CurData^.Next;
    if CurData = Data then
      if Assigned(PrevData) then
        PrevData^.Next := NextData
      else
        Obj.FFirstNotifyData := NextData;
    PrevData := CurData;
    CurData := NextData;
  end;

  Dispose(Data);
end;



constructor TEventLoop.Create;
begin
  asyncInit(Handle);
end;

destructor TEventLoop.Destroy;
var
  NotifyData, NextNotifyData: PNotifyData;
begin
  asyncFree(Handle);
  NotifyData := FFirstNotifyData;
  while Assigned(NotifyData) do
  begin
    NextNotifyData := NotifyData^.Next;
    Dispose(NotifyData);
    NotifyData := NextNotifyData;
  end;
end;

function TEventLoop.Handle: TAsyncHandle;
begin
  Result := TAsyncHandle(Self);
end;

procedure TEventLoop.Run;
begin
  asyncRun(Handle);
end;

procedure TEventLoop.Break;
begin
  asyncBreak(Handle);
end;

function TEventLoop.AddTimerCallback(AMSec: LongInt; APeriodic: Boolean;
  ACallback: TAsyncCallback; AUserData: Pointer): TAsyncTimer;
begin
  Result := asyncAddTimer(Handle, AMSec, APeriodic, ACallback, AUserData);
end;

procedure TEventLoop.RemoveTimerCallback(ATimer: TAsyncTimer);
begin
  asyncRemoveTimer(Handle, ATimer);
end;

function TEventLoop.AddTimerNotify(AMSec: LongInt; APeriodic: Boolean;
  ANotify: TNotifyEvent; ASender: TObject): Pointer;
var
  UserData: PNotifyData;
begin
  UserData := AddNotifyData(Self);
  UserData^.Notify := ANotify;
  UserData^.Sender := ASender;
  UserData^.TimerHandle :=
    asyncAddTimer(Handle, AMSec, APeriodic, @EventHandler, UserData);
  Result := UserData;
end;

procedure TEventLoop.RemoveTimerNotify(AHandle: Pointer);
var
  Data: PNotifyData;
begin
  Data := PNotifyData(AHandle);
  asyncRemoveTimer(Handle, Data^.TimerHandle);
  FreeNotifyData(Self, Data);
end;

procedure TEventLoop.SetIOCallback(AHandle: Integer; ACallback: TAsyncCallback;
  AUserData: Pointer);
begin
  asyncSetIOCallback(Handle, AHandle, ACallback, AUserData);
end;

procedure TEventLoop.ClearIOCallback(AHandle: Integer);
begin
  asyncClearIOCallback(Handle, AHandle);
end;

function TEventLoop.SetIONotify(AHandle: Integer; ANotify: TNotifyEvent;
  ASender: TObject): Pointer;
var
  UserData: PNotifyData;
begin
  UserData := AddNotifyData(Self);
  UserData^.Notify := ANotify;
  UserData^.Sender := ASender;
  UserData^.FileHandle := AHandle;
  asyncSetIOCallback(Handle, AHandle, @EventHandler, UserData);
  Result := UserData;
end;

procedure TEventLoop.ClearIONotify(AHandle: Pointer);
var
  Data: PNotifyData;
begin
  Data := PNotifyData(AHandle);
  asyncClearIOCallback(Handle, Data^.FileHandle);
  FreeNotifyData(Self, Data);
end;

function TEventLoop.TimerTicks: Int64;
begin
  Result := asyncGetTicks;
end;

function TEventLoop.GetIsRunning: Boolean;
begin
  Result := asyncIsRunning(Handle);
end;

procedure TEventLoop.SetIsRunning(AIsRunning: Boolean);
begin
  if IsRunning then
  begin
    if not AIsRunning then
      Run;
  end else
    if AIsRunning then
      Break;
end;


{
  $Log$
  Revision 1.1.2.1  2001-09-08 15:43:24  sg
  * First public version

}
