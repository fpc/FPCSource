{
    $Id$

    fpAsync: Asynchronous event management for Free Pascal
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


constructor EAsyncError.Create(AErrorCode: TAsyncResult);
begin
  inherited Create(Format('Async I/O error %d', [Ord(AErrorCode)]));
  FErrorCode := AErrorCode;
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
  CheckResult(asyncSetIOCallback(Handle, AHandle, ACallback, AUserData));
end;

procedure TEventLoop.ClearIOCallback(AHandle: Integer);
begin
  asyncClearIOCallback(Handle, AHandle);
end;

function TEventLoop.SetIONotify(AHandle: Integer; ANotify: TNotifyEvent;
  ASender: TObject): Pointer;
var
  UserData: PNotifyData;
  ResultCode: TAsyncResult;
begin
  UserData := AddNotifyData(Self);
  UserData^.Notify := ANotify;
  UserData^.Sender := ASender;
  UserData^.FileHandle := AHandle;
  ResultCode := asyncSetIOCallback(Handle, AHandle, @EventHandler, UserData);
  if ResultCode <> asyncOK then
  begin
    FreeNotifyData(Self, UserData);
    raise EAsyncError.Create(ResultCode);
  end else
    Result := UserData;
  {$IFDEF fpAsyncDebug}WriteLn('TEventLoop.SetIONotify: Filehandle=', AHandle, ', Result=', Integer(Result));{$ENDIF}
end;

procedure TEventLoop.ClearIONotify(AHandle: Pointer);
var
  Data: PNotifyData;
begin
  Data := PNotifyData(AHandle);
  {$IFDEF fpAsyncDebug}WriteLn('TEventLoop.ClearIONotify: Filehandle=', Data^.FileHandle, ', Data=', Integer(AHandle));{$ENDIF}
  asyncClearIOCallback(Handle, Data^.FileHandle);
  FreeNotifyData(Self, Data);
end;

procedure TEventLoop.SetDataAvailableCallback(AHandle: Integer; ACallback: TAsyncCallback;
  AUserData: Pointer);
begin
  CheckResult(asyncSetDataAvailableCallback(Handle, AHandle,
    ACallback, AUserData));
end;

procedure TEventLoop.ClearDataAvailableCallback(AHandle: Integer);
begin
  asyncClearDataAvailableCallback(Handle, AHandle);
end;

function TEventLoop.SetDataAvailableNotify(AHandle: Integer; ANotify: TNotifyEvent;
  ASender: TObject): Pointer;
var
  UserData: PNotifyData;
  ResultCode: TAsyncResult;
begin
  UserData := AddNotifyData(Self);
  UserData^.Notify := ANotify;
  UserData^.Sender := ASender;
  UserData^.FileHandle := AHandle;
  ResultCode := asyncSetDataAvailableCallback(Handle, AHandle,
    @EventHandler, UserData);
  if ResultCode <> asyncOK then
  begin
    FreeNotifyData(Self, UserData);
    raise EAsyncError.Create(ResultCode);
  end else
    Result := UserData;
  {$IFDEF fpAsyncDebug}WriteLn('TEventLoop.SetDataAvailableNotify: Filehandle=', AHandle, ', Result=', Integer(Result));{$ENDIF}
end;

procedure TEventLoop.ClearDataAvailableNotify(AHandle: Pointer);
var
  Data: PNotifyData;
begin
  Data := PNotifyData(AHandle);
  {$IFDEF fpAsyncDebug}WriteLn('TEventLoop.ClearDataAvailableNotify: Filehandle=', Data^.FileHandle, ', Data=', Integer(AHandle));{$ENDIF}
  asyncClearDataAvailableCallback(Handle, Data^.FileHandle);
  FreeNotifyData(Self, Data);
end;

procedure TEventLoop.SetCanWriteCallback(AHandle: Integer; ACallback: TAsyncCallback;
  AUserData: Pointer);
begin
  CheckResult(asyncSetCanWriteCallback(Handle, AHandle, ACallback, AUserData));
end;

procedure TEventLoop.ClearCanWriteCallback(AHandle: Integer);
begin
  asyncClearCanWriteCallback(Handle, AHandle);
end;

function TEventLoop.SetCanWriteNotify(AHandle: Integer; ANotify: TNotifyEvent;
  ASender: TObject): Pointer;
var
  UserData: PNotifyData;
  ResultCode: TAsyncResult;
begin
  UserData := AddNotifyData(Self);
  UserData^.Notify := ANotify;
  UserData^.Sender := ASender;
  UserData^.FileHandle := AHandle;
  ResultCode := asyncSetCanWriteCallback(Handle, AHandle,
    @EventHandler, UserData);
  if ResultCode <> asyncOK then
  begin
    FreeNotifyData(Self, UserData);
    raise EAsyncError.Create(ResultCode);
  end else
    Result := UserData;
  {$IFDEF fpAsyncDebug}WriteLn('TEventLoop.SetCanWriteNotify: Filehandle=', AHandle, ', Result=', Integer(Result));{$ENDIF}
end;

procedure TEventLoop.ClearCanWriteNotify(AHandle: Pointer);
var
  Data: PNotifyData;
begin
  Data := PNotifyData(AHandle);
  {$IFDEF fpAsyncDebug}WriteLn('TEventLoop.ClearCanWriteNotify: Filehandle=', Data^.FileHandle, ', Data=', Integer(AHandle));{$ENDIF}
  asyncClearCanWriteCallback(Handle, Data^.FileHandle);
  FreeNotifyData(Self, Data);
end;

class function TEventLoop.TimerTicks: Int64;
begin
  Result := asyncGetTicks;
end;

procedure TEventLoop.CheckResult(AResultCode: TAsyncResult);
begin
  if AResultCode <> asyncOK then
    raise EAsyncError.Create(AResultCode);
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
//   TGenericLineReader
// -------------------------------------------------------------------

destructor TGenericLineReader.Destroy;
begin
  if Assigned(RealBuffer) then
  begin
    FreeMem(RealBuffer);
    RealBuffer := nil;
  end;
  inherited Destroy;
end;

procedure TGenericLineReader.Run;
var
  NewData: array[0..1023] of Byte;
  p: PChar;
  BytesRead, OldBufSize, CurBytesInBuffer, LastEndOfLine, i, LineLength: Integer;
  line: String;
  FirstRun: Boolean;
begin
  FirstRun := True;
  while True do
  begin
    BytesRead := Read(NewData, SizeOf(NewData));
    //WriteLn('Linereader: ', BytesRead, ' bytes read');
    if BytesRead <= 0 then begin
      if FirstRun then
        NoData;
      break;
    end;
    FirstRun := False;
    OldBufSize := FBytesInBuffer;

    // Append the new received data to the read buffer
    Inc(FBytesInBuffer, BytesRead);
    ReallocMem(RealBuffer, FBytesInBuffer);
    Move(NewData, RealBuffer[OldBufSize], BytesRead);

    {Process all potential lines in the current buffer. Attention: FBuffer and
     FBytesInBuffer MUST be updated for each line, as they can be accessed from
     within the FOnLine handler!}
    LastEndOfLine := 0;
    if OldBufSize > 0 then
      i := OldBufSize - 1
    else
      i := 0;

    CurBytesInBuffer := FBytesInBuffer;

    while i <= CurBytesInBuffer - 2 do
    begin
      if (RealBuffer[i] = #13) or (RealBuffer[i] = #10) then
      begin
        LineLength := i - LastEndOfLine;
        SetLength(line, LineLength);
        if LineLength > 0 then
          Move(RealBuffer[LastEndOfLine], line[1], LineLength);

        if ((RealBuffer[i] = #13) and (RealBuffer[i + 1] = #10)) or
           ((RealBuffer[i] = #10) and (RealBuffer[i + 1] = #13)) then
          Inc(i);
        LastEndOfLine := i + 1;

        if Assigned(FOnLine) then begin
          FBuffer := RealBuffer + LastEndOfLine;
          FBytesInBuffer := CurBytesInBuffer - LastEndOfLine;
          FOnLine(line);
          // Check if <this> has been destroyed by FOnLine:
          if not Assigned(FBuffer) then exit;
        end;
      end;
      Inc(i);
    end;

    FBytesInBuffer := CurBytesInBuffer;

    if LastEndOfLine > 0 then
    begin
      // Remove all processed lines from the buffer
      Dec(FBytesInBuffer, LastEndOfLine);
      GetMem(p, FBytesInBuffer);
      Move(RealBuffer[LastEndOfLine], p^, FBytesInBuffer);
      if Assigned(RealBuffer) then
        FreeMem(RealBuffer);
      RealBuffer := p;
    end;
    FBuffer := RealBuffer;
  end;
end;


// -------------------------------------------------------------------
//   TAsyncStreamLineReader
// -------------------------------------------------------------------

constructor TAsyncStreamLineReader.Create(AEventLoop: TEventLoop;
  AStream: THandleStream);
begin
  Self.Create(AEventLoop, AStream, AStream);
end;

constructor TAsyncStreamLineReader.Create(AEventLoop: TEventLoop;
  ADataStream: TStream; ABlockingStream: THandleStream);
begin
  ASSERT(Assigned(ADataStream) and Assigned(ABlockingStream));

  inherited Create;
  FEventLoop := AEventLoop;
  FDataStream := ADataStream;
  FBlockingStream := ABlockingStream;
  NotifyHandle := EventLoop.SetDataAvailableNotify(
    FBlockingStream.Handle, @StreamDataAvailable, nil);
end;

destructor TAsyncStreamLineReader.Destroy;
begin
  if Assigned(NotifyHandle) then
    EventLoop.ClearDataAvailableNotify(NotifyHandle);
  inherited Destroy;
end;

procedure TAsyncStreamLineReader.StopAndFree;
begin
  DoStopAndFree := True;
end;

function TAsyncStreamLineReader.Read(var ABuffer; count: Integer): Integer;
begin
  Result := FDataStream.Read(ABuffer, count);
end;

procedure TAsyncStreamLineReader.NoData;
var
  s: String;
begin
  if (FDataStream = FBlockingStream) or (FDataStream.Position = FDataStream.Size) then begin

    if (FBytesInBuffer > 0) and Assigned(FOnLine) then begin
      if FBuffer[FBytesInBuffer - 1] in [#13, #10] then
        Dec(FBytesInBuffer);
      SetLength(s, FBytesInBuffer);
      Move(FBuffer^, s[1], FBytesInBuffer);
      FOnLine(s);
    end;

    EventLoop.ClearDataAvailableNotify(NotifyHandle);
    NotifyHandle := nil;
    if Assigned(FOnEOF) then
      FOnEOF(Self);
  end;
end;

procedure TAsyncStreamLineReader.StreamDataAvailable(UserData: TObject);
begin
  Run;
  if DoStopAndFree then
    Free;
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
  if (FDataStream <> FBlockingStream) and Assigned(NotifyHandle) then
  begin
    EventLoop.ClearCanWriteNotify(NotifyHandle);
    NotifyHandle := nil;
  end;
end;

procedure TAsyncWriteStream.WantWrite;
begin
  if not Assigned(NotifyHandle) then
    NotifyHandle := EventLoop.SetCanWriteNotify(FBlockingStream.Handle,
      @CanWrite, nil);
end;

procedure TAsyncWriteStream.BufferEmpty;
begin
  if Assigned(NotifyHandle) then
  begin
    EventLoop.ClearCanWriteNotify(NotifyHandle);
    NotifyHandle := nil;
  end;
  inherited BufferEmpty;
end;

procedure TAsyncWriteStream.CanWrite(UserData: TObject);
begin
  Run;
end;

constructor TAsyncWriteStream.Create(AEventLoop: TEventLoop;
  AStream: THandleStream);
begin
  Self.Create(AEventLoop, AStream, AStream);
end;

constructor TAsyncWriteStream.Create(AEventLoop: TEventLoop;
  ADataStream: TStream; ABlockingStream: THandleStream);
begin
  ASSERT(Assigned(ADataStream) and Assigned(ABlockingStream));

  inherited Create;
  FEventLoop := AEventLoop;
  FDataStream := ADataStream;
  FBlockingStream := ABlockingStream;
end;

destructor TAsyncWriteStream.Destroy;
begin
  if Assigned(NotifyHandle) then
    EventLoop.ClearCanWriteNotify(NotifyHandle);
  inherited Destroy;
end;


{
  $Log$
  Revision 1.4  2002-12-27 15:46:15  peter
    * add class to timerticks

  Revision 1.3  2002/09/15 15:45:38  sg
  * Added stream line reader classes

  Revision 1.2  2002/09/07 15:42:57  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:02  peter
    * splitted to base and extra

}
