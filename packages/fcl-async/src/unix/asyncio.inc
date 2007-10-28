{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TAsyncIOManager.CalcHighestHandle(max: Integer);
var
  i: Integer;
begin
  With IOData do
    begin
    HighestHandle := -1;
    for i := max downto 0 do
      if FD_IsSet(i, ReadMap) or FD_IsSet(i, WriteMap) then begin
        HighestHandle := i;
        break;
      end;
    end;
end;

function TAsyncIOManager.GetHandleAsync(AHandle: Integer): Boolean;
begin
  Result := (fcntl(AHandle, F_GetFl) and Open_NonBlock) <> 0;
end;

procedure TAsyncIOManager.SetHandleAsync(AHandle: Integer; AValue: Boolean);
var
  SavedBits: Integer;
begin
  SavedBits := fcntl(AHandle, F_GetFl) and not Open_NonBlock;
  if AValue then
    fcntl(AHandle, F_SetFl, SavedBits or Open_NonBlock)
  else
    fcntl(AHandle, F_SetFl, SavedBits);
end;

constructor TAsyncIOManager.Create;
begin
  inherited Create;
  With IOdata do
    begin
    FD_Zero(ReadMap);
    FD_Zero(WriteMap);
    end;
  HighestHandle := -1;
end;

procedure TAsyncIOManager.Run;
var
  ThisReadMap, ThisWriteMap: TFDSet;
  i, res: Integer;
begin
  DoBreak := False;
  With IOdata do
    begin
    while (not DoBreak) and ((HighestHandle >= 0) or (FTimeout > 0)) do begin
      ThisReadMap := ReadMap;
      ThisWriteMap := WriteMap;

      if FTimeout > 0 then
        res := Select(HighestHandle + 1, @ThisReadMap, @ThisWriteMap, nil, FTimeout)
      else
        res := Select(HighestHandle + 1, @ThisReadMap, @ThisWriteMap, nil, nil);
      if res < 0 then
        break;

      if res = 0 then
        ExecuteNotify(TimeoutNotify)
      else
        for i := 0 to HighestHandle do begin
          if FD_IsSet(i, ThisReadMap) and FD_IsSet(i, ReadMap)then
            ExecuteNotify(ReadNotifies[i]);
          if FD_IsSet(i, ThisWriteMap) and FD_IsSet(i, WriteMap) then
            ExecuteNotify(WriteNotifies[i]);
        end;
    end;
  end;
end;

procedure TAsyncIOManager.BreakRun;
begin
  DoBreak := True;
end;

procedure TAsyncIOManager.SetReadHandler(AHandle: Integer;
  AMethod: TAsyncIONotify; AUserData: TObject);
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle) and Assigned(AMethod));
  if (AHandle < 0) or (AHandle > MaxHandle) then
    exit;
  if AHandle > HighestHandle then
    HighestHandle := AHandle;
  FD_Set(AHandle, IOdata.ReadMap);
  ReadNotifies[AHandle].Method := AMethod;
  ReadNotifies[AHandle].UserData := AUserData;
end;

procedure TAsyncIOManager.ClearReadHandler(AHandle: Integer);
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle));
  if (AHandle >= 0) and (AHandle <= MaxHandle) then
  begin
    FD_Clr(AHandle, IOdata.ReadMap);
    if AHandle = HighestHandle then
      CalcHighestHandle(AHandle);
  end;
end;

function TAsyncIOManager.GetReadHandler(AHandle: Integer): TAsyncIONotify;
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle));
  if (AHandle < 0) or (AHandle > MaxHandle) then
    Result := nil
  else
    Result := ReadNotifies[AHandle].Method;
end;

procedure TAsyncIOManager.SetWriteHandler(AHandle: Integer;
  AMethod: TAsyncIONotify; AUserData: TObject);
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle) and Assigned(AMethod));
  if (AHandle < 0) or (AHandle > MaxHandle) then
    exit;

  if AHandle > HighestHandle then
    HighestHandle := AHandle;
  FD_Set(AHandle, IOData.WriteMap);
  WriteNotifies[AHandle].Method := AMethod;
  WriteNotifies[AHandle].UserData := AUserData;
end;

procedure TAsyncIOManager.ClearWriteHandler(AHandle: Integer);
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle));
  if (AHandle >= 0) and (AHandle <= MaxHandle) then begin
    FD_Clr(AHandle, IOdata.WriteMap);
    if AHandle = HighestHandle then
      CalcHighestHandle(AHandle);
  end;
end;

function TAsyncIOManager.GetWriteHandler(AHandle: Integer): TAsyncIONotify;
begin
  ASSERT((AHandle >= 0) and (AHandle <= MaxHandle));
  if (AHandle < 0) or (AHandle > MaxHandle) then
    Result := nil
  else
    Result := WriteNotifies[AHandle].Method;
end;

