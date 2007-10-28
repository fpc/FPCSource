{

    libasync: Asynchronous event management
    Copyright (C) 2001-2002 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Common implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

type
  PTimerData = ^TTimerData;
  TTimerData = record
    Next: PTimerData;
    MSec: LongInt;
    NextTick: Int64;
    Callback: TAsyncCallback;
    UserData: Pointer;
    Periodic: Boolean;
  end;

  TCallbackTypes = set of (cbRead, cbWrite);


{ An implementation unit has to implement the following fordward procedures,
  and additionally asyncGetTicks }

procedure InternalInit(Handle: TAsyncHandle); forward;

procedure InternalFree(Handle: TAsyncHandle); forward;

procedure InternalRun(Handle: TAsyncHandle; TimeOut: Int64); forward;

procedure InternalInitIOCallback(Handle: TAsyncHandle; Data: PIOCallbackData;
  InitData: Boolean; CallbackTypes: TCallbackTypes); forward;

procedure InternalClearIOCallback(Handle: TAsyncHandle; IOHandle: LongInt;
  CallbackTypes: TCallbackTypes); forward;



function InitIOCallback(Handle: TAsyncHandle; IOHandle: LongInt;
  ARead: Boolean; ReadCallback: TAsyncCallback; ReadUserData: Pointer;
  AWrite: Boolean; WriteCallback: TAsyncCallback; WriteUserData: Pointer):
  TAsyncResult;
var
  Data: PIOCallbackData;
  NeedData: Boolean;
  CallbackTypes: TCallbackTypes;
begin
  if (IOHandle < 0) or (IOHandle > MaxHandle) then
  begin
    Result := asyncInvalidFileHandle;
    exit;
  end;

  NeedData := True;
  Data := Handle^.Data.FirstIOCallback;
  while Assigned(Data) do
  begin
    if Data^.IOHandle = IOHandle then
    begin
      if ARead then
      begin
        if Assigned(Data^.ReadCallback) then
        begin
          Result := asyncHandlerAlreadySet;
          exit;
        end;
        Data^.ReadCallback := ReadCallback;
        Data^.ReadUserData := ReadUserData;
      end;
      if AWrite then
      begin
        if Assigned(Data^.WriteCallback) then
        begin
          Result := asyncHandlerAlreadySet;
          exit;
        end;
        Data^.WriteCallback := WriteCallback;
        Data^.WriteUserData := WriteUserData;
      end;
      NeedData := False;
      break;
    end;
    Data := Data^.Next;
  end;

  if NeedData then
  begin
    New(Data);
    Data^.Next := Handle^.Data.FirstIOCallback;
    Handle^.Data.FirstIOCallback := Data;
    Data^.IOHandle := IOHandle;
    if ARead then
    begin
      Data^.ReadCallback := ReadCallback;
      Data^.ReadUserData := ReadUserData;
    end else
      Data^.ReadCallback := nil;
    if AWrite then
    begin
      Data^.WriteCallback := WriteCallback;
      Data^.WriteUserData := WriteUserData;
    end else
      Data^.WriteCallback := nil;
  end;

  CallbackTypes := [];
  if ARead then
    CallbackTypes := [cbRead];
  if AWrite then
    CallbackTypes := CallbackTypes + [cbWrite];
  InternalInitIOCallback(Handle, Data, NeedData, CallbackTypes);

  Handle^.Data.HasCallbacks := True;
  Result := asyncOK;
end;

procedure CheckForCallbacks(Handle: TAsyncHandle);
begin
  if (Handle^.Data.HasCallbacks) and
    (not Assigned(Handle^.Data.FirstIOCallback)) and
    (not Assigned(Handle^.Data.FirstTimer)) then
    Handle^.Data.HasCallbacks := False;
end;


procedure asyncInit(Handle: TAsyncHandle); cdecl;
begin
  InternalInit(Handle);
end;

procedure asyncFree(Handle: TAsyncHandle); cdecl;
var
  Timer, NextTimer: PTimerData;
  IOCallback, NextIOCallback: PIOCallbackData;
begin
  InternalFree(Handle);

  Timer := PTimerData(Handle^.Data.FirstTimer);
  while Assigned(Timer) do
  begin
    NextTimer := Timer^.Next;
    Dispose(Timer);
    Timer := NextTimer;
  end;

  IOCallback := PIOCallbackData(Handle^.Data.FirstIOCallback);
  while Assigned(IOCallback) do
  begin
    NextIOCallback := IOCallback^.Next;
    Dispose(IOCallback);
    IOCallback := NextIOCallback;
  end;

  Handle^.Data.NextIOCallback := nil;
end;

procedure asyncRun(Handle: TAsyncHandle); cdecl;
var
  Timer, NextTimer: PTimerData;
  TimeOut, CurTime, NextTick: Int64;
begin
  if Handle^.Data.IsRunning then
    exit;

  Handle^.Data.DoBreak := False;
  Handle^.Data.IsRunning := True;

  // Prepare timers
  if Assigned(Handle^.Data.FirstTimer) then
  begin
    CurTime := asyncGetTicks;
    Timer := Handle^.Data.FirstTimer;
    while Assigned(Timer) do
    begin
      Timer^.NextTick := CurTime + Timer^.MSec;
      Timer := Timer^.Next;
    end;
  end;

  while (not Handle^.Data.DoBreak) and Handle^.Data.HasCallbacks do
  begin
    Timer := Handle^.Data.FirstTimer;
    if Assigned(Handle^.Data.FirstTimer) then
    begin
      // Determine when the next timer tick will happen
      CurTime := asyncGetTicks;
      NextTick := High(Int64);
      Timer := Handle^.Data.FirstTimer;
      while Assigned(Timer) do
      begin
        if Timer^.NextTick < NextTick then
          NextTick := Timer^.NextTick;
        Timer := Timer^.Next;
      end;
      TimeOut := NextTick - CurTime;
      if TimeOut < 0 then
        TimeOut := 0;
    end else
      TimeOut := -1;

    InternalRun(Handle, TimeOut);

    {if Handle^.Data.HighestHandle >= 0 then
    begin
      CurReadFDSet := PFDSet(Handle^.Data.FDData)[0];
      CurWriteFDSet := PFDSet(Handle^.Data.FDData)[1];
      AsyncResult := Select(Handle^.Data.HighestHandle + 1,
        @CurReadFDSet, @CurWriteFDSet, nil, TimeOut);
    end else
      AsyncResult := Select(0, nil, nil, nil, TimeOut);

    if (AsyncResult > 0) and not Handle^.Data.DoBreak then
    begin
      // Check for I/O events
      Handle^.Data.CurIOCallback := Handle^.Data.FirstIOCallback;
      while Assigned(Handle^.Data.CurIOCallback) do
      begin
        CurIOCallback := PIOCallbackData(Handle^.Data.CurIOCallback);
        Handle^.Data.NextIOCallback := CurIOCallback^.Next;
        if FD_IsSet(CurIOCallback^.IOHandle, CurReadFDSet) and
          FD_IsSet(CurIOCallback^.IOHandle, PFDSet(Handle^.Data.FDData)[0]) and
          Assigned(CurIOCallback^.ReadCallback) then
        begin
          CurIOCallback^.ReadCallback(CurIOCallback^.ReadUserData);
          if Handle^.Data.DoBreak then
            break;
        end;

        CurIOCallback := PIOCallbackData(Handle^.Data.CurIOCallback);
        if Assigned(CurIOCallback) and
          FD_IsSet(CurIOCallback^.IOHandle, CurWriteFDSet) and
          FD_IsSet(CurIOCallback^.IOHandle, PFDSet(Handle^.Data.FDData)[1]) and
          Assigned(CurIOCallback^.WriteCallback) then
        begin
          CurIOCallback^.WriteCallback(CurIOCallback^.WriteUserData);
          if Handle^.Data.DoBreak then
            break;
        end;

        Handle^.Data.CurIOCallback := Handle^.Data.NextIOCallback;
      end;
    end;}

    if Assigned(Handle^.Data.FirstTimer) then
    begin
      // Check for triggered timers
      CurTime := asyncGetTicks;
      Timer := Handle^.Data.FirstTimer;
      while Assigned(Timer) do
      begin
        if Timer^.NextTick <= CurTime then
        begin
          Timer^.Callback(Timer^.UserData);
          NextTimer := Timer^.Next;
          if Timer^.Periodic then
            Inc(Timer^.NextTick, Timer^.MSec)
          else
            asyncRemoveTimer(Handle, Timer);
          if Handle^.Data.DoBreak then
            break;
          Timer := NextTimer;
        end else
          Timer := Timer^.Next;
      end;
    end;

  end;
  Handle^.Data.CurIOCallback := nil;
  Handle^.Data.NextIOCallback := nil;
  Handle^.Data.IsRunning := False;
end;

procedure asyncBreak(Handle: TAsyncHandle); cdecl;
begin
  Handle^.Data.DoBreak := True;
end;

function asyncIsRunning(Handle: TAsyncHandle): Boolean; cdecl;
begin
  Result := Handle^.Data.IsRunning;
end;

function asyncAddTimer(
  Handle: TAsyncHandle;
  MSec: LongInt;
  Periodic: Boolean;
  Callback: TAsyncCallback;
  UserData: Pointer
  ): TAsyncTimer; cdecl;
var
  Data: PTimerData;
begin
  if not Assigned(Callback) then
    exit;

  New(Data);
  Result := Data;
  Data^.Next := Handle^.Data.FirstTimer;
  Handle^.Data.FirstTimer := Data;
  Data^.MSec := MSec;
  Data^.Periodic := Periodic;
  Data^.Callback := Callback;
  Data^.UserData := UserData;
  if Handle^.Data.IsRunning then
    Data^.NextTick := asyncGetTicks + MSec;

  Handle^.Data.HasCallbacks := True;
end;

procedure asyncRemoveTimer(
  Handle: TAsyncHandle;
  Timer: TASyncTimer); cdecl;
var
  Data, CurData, PrevData, NextData: PTimerData;
begin
  Data := PTimerData(Timer);
  CurData := Handle^.Data.FirstTimer;
  PrevData := nil;
  while Assigned(CurData) do
  begin
    NextData := CurData^.Next;
    if CurData = Data then
    begin
      if Assigned(PrevData) then
        PrevData^.Next := NextData
      else
        Handle^.Data.FirstTimer := NextData;
      break;
    end;
    PrevData := CurData;
    CurData := NextData;
  end;
  Dispose(Data);
  CheckForCallbacks(Handle);
end;

function asyncSetIOCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt;
  Callback: TAsyncCallback;
  UserData: Pointer): TAsyncResult; cdecl;
begin
  Result := InitIOCallback(Handle, IOHandle, True, Callback, UserData,
    True, Callback, UserData);
end;

procedure asyncClearIOCallback(Handle: TAsyncHandle;
  IOHandle: LongInt); cdecl;
var
  CurData, PrevData, NextData: PIOCallbackData;
begin
  CurData := Handle^.Data.FirstIOCallback;
  PrevData := nil;
  while Assigned(CurData) do
  begin
    NextData := CurData^.Next;
    if CurData^.IOHandle = IOHandle then
    begin
      if Handle^.Data.CurIOCallback = CurData then
        Handle^.Data.CurIOCallback := nil;
      if Handle^.Data.NextIOCallback = CurData then
        Handle^.Data.NextIOCallback := NextData;

      InternalClearIOCallback(Handle, IOHandle, [cbRead, cbWrite]);

      if Assigned(PrevData) then
        PrevData^.Next := NextData
      else
        Handle^.Data.FirstIOCallback := NextData;
      Dispose(CurData);
      break;
    end;
    PrevData := CurData;
    CurData := NextData;
  end;
  CheckForCallbacks(Handle);
end;

function asyncSetDataAvailableCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt;
  Callback: TAsyncCallback;
  UserData: Pointer): TAsyncResult; cdecl;
begin
  Result := InitIOCallback(Handle, IOHandle, True, Callback, UserData, False,
    nil, nil);
end;

procedure asyncClearDataAvailableCallback(Handle: TAsyncHandle;
  IOHandle: LongInt); cdecl;
var
  CurData, PrevData, NextData: PIOCallbackData;
begin
  CurData := Handle^.Data.FirstIOCallback;
  PrevData := nil;
  while Assigned(CurData) do
  begin
    NextData := CurData^.Next;
    if CurData^.IOHandle = IOHandle then
    begin
      if Handle^.Data.CurIOCallback = CurData then
        Handle^.Data.CurIOCallback := nil;
      if Handle^.Data.NextIOCallback = CurData then
        Handle^.Data.NextIOCallback := NextData;

      InternalClearIOCallback(Handle, IOHandle, [cbRead]);

      if Assigned(CurData^.WriteCallback) then
        CurData^.ReadCallback := nil
      else
      begin
        if Assigned(PrevData) then
          PrevData^.Next := NextData
        else
          Handle^.Data.FirstIOCallback := NextData;
        Dispose(CurData);
      end;
      break;
    end;
    PrevData := CurData;
    CurData := NextData;
  end;
  CheckForCallbacks(Handle);
end;

function asyncSetCanWriteCallback(
  Handle: TAsyncHandle;
  IOHandle: LongInt;
  Callback: TAsyncCallback;
  UserData: Pointer): TAsyncResult; cdecl;
begin
  Result := InitIOCallback(Handle, IOHandle, False, nil, nil, True,
    Callback, UserData);
end;

procedure asyncClearCanWriteCallback(Handle: TAsyncHandle;
  IOHandle: LongInt); cdecl;
var
  CurData, PrevData, NextData: PIOCallbackData;
begin
  CurData := Handle^.Data.FirstIOCallback;
  PrevData := nil;
  while Assigned(CurData) do
  begin
    NextData := CurData^.Next;
    if CurData^.IOHandle = IOHandle then
    begin
      if Handle^.Data.CurIOCallback = CurData then
        Handle^.Data.CurIOCallback := nil;
      if Handle^.Data.NextIOCallback = CurData then
        Handle^.Data.NextIOCallback := NextData;

      InternalClearIOCallback(Handle, IOHandle, [cbWrite]);

      if Assigned(CurData^.ReadCallback) then
        CurData^.WriteCallback := nil
      else
      begin
        if Assigned(PrevData) then
          PrevData^.Next := NextData
        else
          Handle^.Data.FirstIOCallback := NextData;
        Dispose(CurData);
      end;
      break;
    end;
    PrevData := CurData;
    CurData := NextData;
  end;
  CheckForCallbacks(Handle);
end;


