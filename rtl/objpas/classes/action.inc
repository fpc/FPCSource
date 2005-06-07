{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************}
{*                           TBasicActionLink                               *}
{****************************************************************************}

constructor TBasicActionLink.Create(AClient: TObject);
begin
  inherited Create;
  AssignClient(AClient);
end;


procedure TBasicActionLink.AssignClient(AClient: TObject);
begin
end;


destructor TBasicActionLink.Destroy;
begin
  if FAction <> nil then
    FAction.UnRegisterChanges(Self);
  inherited Destroy;
end;


procedure TBasicActionLink.Change;
begin
  if Assigned(OnChange) then
    OnChange(FAction);
end;


function TBasicActionLink.Execute(AComponent: TComponent): Boolean;
begin
  FAction.ActionComponent := AComponent;
  try
    Result := FAction.Execute;
  finally
    if FAction <> nil then
      FAction.ActionComponent := nil;
  end;
end;


procedure TBasicActionLink.SetAction(Value: TBasicAction);
begin
  if Value <> FAction then
  begin
    if FAction <> nil then FAction.UnRegisterChanges(Self);
    FAction := Value;
    if Value <> nil then Value.RegisterChanges(Self);
  end;
end;


function TBasicActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := True;
end;


procedure TBasicActionLink.SetOnExecute(Value: TNotifyEvent);
begin
end;


function TBasicActionLink.Update: Boolean;
begin
  Result := FAction.Update;
end;

{****************************************************************************}
{*                             TBasicAction                                 *}
{****************************************************************************}

constructor TBasicAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
end;


destructor TBasicAction.Destroy;
begin
  inherited Destroy;
  while FClients.Count > 0 do
    UnRegisterChanges(TBasicActionLink(FClients.Last));
  FClients.Free;
end;


function TBasicAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := False;
end;


procedure TBasicAction.ExecuteTarget(Target: TObject);
begin
end;


procedure TBasicAction.UpdateTarget(Target: TObject);
begin
end;


function TBasicAction.Execute: Boolean;
begin
  if Assigned(FOnExecute) then
   begin
     FOnExecute(Self);
     Result := True;
   end
  else
   Result := False;
end;


function TBasicAction.Update: Boolean;
begin
  if Assigned(FOnUpdate) then
   begin
     FOnUpdate(Self);
     Result := True;
   end
  else
   Result := False;
end;


procedure TBasicAction.SetOnExecute(Value: TNotifyEvent);
var
  I: Integer;
begin
  if (TMethod(Value).Code <> TMethod(OnExecute).Code) or
     (TMethod(Value).Data <> TMethod(OnExecute).Data) then
  begin
    for I := 0 to FClients.Count - 1 do
      TBasicActionLink(FClients[I]).SetOnExecute(Value);
    FOnExecute := Value;
    Change;
  end;
end;


procedure TBasicAction.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


procedure TBasicAction.RegisterChanges(Value: TBasicActionLink);
begin
  Value.FAction := Self;
  FClients.Add(Value);
end;


procedure TBasicAction.UnRegisterChanges(Value: TBasicActionLink);
var
  I: Integer;
begin
  for I := 0 to FClients.Count - 1 do
    if TBasicActionLink(FClients[I]) = Value then
     begin
       Value.FAction := nil;
       FClients.Delete(I);
       break;
     end;
end;


