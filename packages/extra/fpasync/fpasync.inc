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

procedure TEventLoop.SetDataAvailableCallback(AHandle: Integer; ACallback: TAsyncCallback;
  AUserData: Pointer);
begin
  asyncSetDataAvailableCallback(Handle, AHandle, ACallback, AUserData);
end;

procedure TEventLoop.ClearDataAvailableCallback(AHandle: Integer);
begin
  asyncClearDataAvailableCallback(Handle, AHandle);
end;

function TEventLoop.SetDataAvailableNotify(AHandle: Integer; ANotify: TNotifyEvent;
  ASender: TObject): Pointer;
var
  UserData: PNotifyData;
begin
  UserData := AddNotifyData(Self);
  UserData^.Notify := ANotify;
  UserData^.Sender := ASender;
  UserData^.FileHandle := AHandle;
  asyncSetDataAvailableCallback(Handle, AHandle, @EventHandler, UserData);
  Result := UserData;
end;

procedure TEventLoop.ClearDataAvailableNotify(AHandle: Pointer);
var
  Data: PNotifyData;
begin
  Data := PNotifyData(AHandle);
  asyncClearDataAvailableCallback(Handle, Data^.FileHandle);
  FreeNotifyData(Self, Data);
end;

procedure TEventLoop.SetCanWriteCallback(AHandle: Integer; ACallback: TAsyncCallback;
  AUserData: Pointer);
begin
  asyncSetCanWriteCallback(Handle, AHandle, ACallback, AUserData);
end;

procedure TEventLoop.ClearCanWriteCallback(AHandle: Integer);
begin
  asyncClearCanWriteCallback(Handle, AHandle);
end;

function TEventLoop.SetCanWriteNotify(AHandle: Integer; ANotify: TNotifyEvent;
  ASender: TObject): Pointer;
var
  UserData: PNotifyData;
begin
  UserData := AddNotifyData(Self);
  UserData^.Notify := ANotify;
  UserData^.Sender := ASender;
  UserData^.FileHandle := AHandle;
  asyncSetCanWriteCallback(Handle, AHandle, @EventHandler, UserData);
  Result := UserData;
end;

procedure TEventLoop.ClearCanWriteNotify(AHandle: Pointer);
var
  Data: PNotifyData;
begin
  Data := PNotifyData(AHandle);
  asyncClearCanWriteCallback(Handle, Data^.FileHandle);
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


// -------------------------------------------------------------------
//   TWriteBuffer
// -------------------------------------------------------------------

procedure TWriteBuffer.BufferEmpty;
begin
  if Assigned(FOnBufferEmpty) then
    FOnBufferEmpty(Self);
end;

constructor TWriteBuffer.Create;
begin
  inherited Create;

  FBuffer := nil;
  FBytesInBuffer := 0;
  EndOfLineMarker := #10;
end;

destructor TWriteBuffer.Destroy;
begin
  if Assigned(FBuffer) then
    FreeMem(FBuffer);
  inherited Destroy;
end;

function TWriteBuffer.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  if ((Offset = 0) and ((Origin = soFromCurrent) or (Origin = soFromEnd))) or
     ((Offset = FBytesInBuffer) and (Origin = soFromBeginning)) then
    Result := FBytesInBuffer
  else
    // !!!: No i18n for this string - solve this problem in the FCL?!?
    raise EStreamError.Create('Invalid stream operation');
end;

function TWriteBuffer.Write(const ABuffer; Count: LongInt): LongInt;
begin
  ReallocMem(FBuffer, FBytesInBuffer + Count);
  Move(ABuffer, FBuffer[FBytesInBuffer], Count);
  Inc(FBytesInBuffer, Count);
  WantWrite;
  Result := Count;
end;

procedure TWriteBuffer.WriteLine(const line: String);
var
  s: String;
begin
  s := line + EndOfLineMarker;
  WriteBuffer(s[1], Length(s));
end;

procedure TWriteBuffer.Run;
var
  CurStart, Written: Integer;
  NewBuf: PChar;
  Failed: Boolean;
begin
  CurStart := 0;
  Failed := True;
  repeat
    if FBytesInBuffer = 0 then
    begin
      BufferEmpty;
      exit;
    end;

    Written := DoRealWrite(FBuffer[CurStart], FBytesInBuffer - CurStart);
    if Written > 0 then
    begin
      Inc(CurStart, Written);
      Failed := False;
      GetMem(NewBuf, FBytesInBuffer - CurStart);
      Move(FBuffer[CurStart], NewBuf[0], FBytesInBuffer - CurStart);
      FreeMem(FBuffer);
      FBuffer := NewBuf;
      Dec(FBytesInBuffer, CurStart);
    end;
  until Written <= 0;

  if Failed then
    WritingFailed;
end;


// -------------------------------------------------------------------
//   TAsyncWriteStream
// -------------------------------------------------------------------

function TAsyncWriteStream.DoRealWrite(const ABuffer; Count: Integer): Integer;
begin
  Result := FDataStream.Write(ABuffer, count);
end;

procedure TAsyncWriteStream.WritingFailed;
begin
  if (FDataStream <> FBlockingStream) and Assigned(FNotifyHandle) then
  begin
    FManager.ClearCanWriteNotify(FNotifyHandle);
    FNotifyHandle := nil;
  end;
end;

procedure TAsyncWriteStream.WantWrite;
begin
  FNotifyHandle := FManager.SetCanWriteNotify(FBlockingStream.Handle, @CanWrite, nil);
end;

procedure TAsyncWriteStream.BufferEmpty;
begin
  if Assigned(FNotifyHandle) then
  begin
    FManager.ClearCanWriteNotify(FNotifyHandle);
    FNotifyHandle := nil;
  end;
  inherited BufferEmpty;
end;

procedure TAsyncWriteStream.CanWrite(UserData: TObject);
begin
  Run;
end;

constructor TAsyncWriteStream.Create(AManager: TEventLoop; AStream: THandleStream);
begin
  Self.Create(AManager, AStream, AStream);
end;

constructor TAsyncWriteStream.Create(AManager: TEventLoop;
  ADataStream: TStream; ABlockingStream: THandleStream);
begin
  ASSERT(Assigned(ADataStream) and Assigned(ABlockingStream));

  inherited Create;
  FManager := AManager;
  FDataStream := ADataStream;
  FBlockingStream := ABlockingStream;
end;

destructor TAsyncWriteStream.Destroy;
begin
  if Assigned(FNotifyHandle) then
    FManager.ClearCanWriteNotify(FNotifyHandle);
  inherited Destroy;
end;


{
  $Log$
  Revision 1.2  2002-09-07 15:42:57  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:02  peter
    * splitted to base and extra

}
