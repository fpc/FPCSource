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
{*                             TComponent                                   *}
{****************************************************************************}

function TComponent.GetComponent(AIndex: Integer): TComponent;
begin
  if Assigned(FComponents) then
    Result := TComponent(FComponents.Items[Aindex])
  else
    Result := nil;
end;


function TComponent.IsImplementorOf(const Intf: IInterface):boolean;
var
  ref: IInterfaceComponentReference;
begin
  result := Assigned(intf) and Supports(intf,IInterfaceComponentReference,ref) and (ref.getcomponent=self);
end;


procedure TComponent.ReferenceInterface(const intf: IInterface; op: TOperation);
var
  ref: IInterfaceComponentReference;
  comp: TComponent;
begin
  if assigned(intf) and supports(intf,IInterfaceComponentReference,ref) then
  begin
    comp := ref.getcomponent;
    comp.notification(self,op);
  end;
end;


function TComponent.GetComponentCount: Integer;
begin
  if assigned(FComponents) then
    Result := FComponents.Count
  else
    Result := 0;
end;


function TComponent.GetComponentIndex: Integer;
begin
  if Assigned(FOwner) and Assigned(FOwner.FComponents) then
    Result := FOwner.FComponents.IndexOf(Self)
  else
    Result := -1;
end;


procedure TComponent.Insert(AComponent: TComponent);
begin
  if not Assigned(FComponents) then
    FComponents := TList.Create;
  FComponents.Add(AComponent);
  AComponent.FOwner := Self;
end;


procedure TComponent.ReadLeft(Reader: TReader);
begin
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
end;


procedure TComponent.ReadTop(Reader: TReader);
begin
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
end;


procedure TComponent.Remove(AComponent: TComponent);
begin
  AComponent.FOwner := nil;
  if Assigned(FCOmponents) then
  begin
    FComponents.Remove(AComponent);
    iF FComponents.Count = 0 then
      FreeAndNil(FComponents);
  end;
end;


procedure TComponent.RemoveNotification(AComponent: TComponent);
begin
  if Assigned(FFreeNotifies) then
  begin
    FFreeNotifies.Remove(AComponent);
    if FFreeNotifies.Count=0 then
    begin
      FreeAndNil(FFreeNotifies);
      Exclude(FComponentState,csFreeNotification);
    end;
  end;
end;


procedure TComponent.SetComponentIndex(Value: Integer);
var
  OldIndex, Count: Integer;
begin
  if not Assigned(FOwner) then Exit;
  OldIndex := GetComponentIndex;
  if OldIndex < 0 then Exit;
  if Value < 0 then Value := 0;
  Count := FOwner.FComponents.Count;
  if Value >= Count then Value := Count-1;
  if Value <> OldIndex then
  begin
    FOwner.FComponents.Delete(OldIndex);
    FOwner.FComponents.Insert(Value, Self);
  end;
end;


procedure TComponent.SetReference(Enable: Boolean);
var
  Field: ^TComponent;
begin
  if Assigned(FOwner) then
  begin
    Field := FOwner.FieldAddress(Name);
    if Assigned(Field) then
      if Enable then
        Field^ := Self
      else
        Field^ := nil;
  end;
end;


procedure TComponent.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(FDesignInfo).Lo);
end;


procedure TComponent.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(FDesignInfo).Hi);
end;


{procedure TComponent.ChangeName(const NewName: TComponentName);
begin
  FName := NewName;
end;}


procedure TComponent.DefineProperties(Filer: TFiler);
var
  Ancestor: TComponent;
  Temp: Longint;
begin
  Temp := 0;
  Ancestor := TComponent(Filer.Ancestor);
  If Assigned(Ancestor) then Temp:=Ancestor.FDesignInfo;
  Filer.Defineproperty('left',@readleft,@writeleft,
                       (longrec(FDesignInfo).Lo<>Longrec(temp).Lo));
  Filer.Defineproperty('top',@readtop,@writetop,
                       (longrec(FDesignInfo).Hi<>Longrec(temp).Hi));
end;


procedure TComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // Does nothing.
end;


function TComponent.GetChildOwner: TComponent;
begin
  Result := nil;
end;


function TComponent.GetChildParent: TComponent;
begin
  Result := Self;
end;


function TComponent.GetNamePath: string;
begin
  Result := FName;
end;


function TComponent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TComponent.Loaded;
begin
  Exclude(FComponentState,csLoading);
end;


procedure TComponent.Loading;
begin
  Include(FComponentState,csLoading);
end;


procedure TComponent.Notification(AComponent: TComponent; Operation: TOperation);
var
  Runner: Integer;
begin
  if (Operation=opRemove) and Assigned(FFreeNotifies) then
  begin
    FFreeNotifies.Remove(AComponent);
    if FFreeNotifies.Count=0 then
    begin
      FreeAndNil(FFreeNotifies);
      Exclude(FComponentState,csFreeNotification);
    end;
  end;

  if Assigned(FComponents) then
    for Runner := 0 To FComponents.Count-1 do
      TComponent(FComponents.List^[Runner]).Notification(AComponent,Operation);
end;


procedure TComponent.PaletteCreated;
begin
end;


procedure TComponent.ReadState(Reader: TReader);
begin
  Reader.ReadData(Self);
end;


procedure TComponent.SetAncestor(Value: Boolean);
var
  Runner: Integer;
begin
  if Value then
    Include(FComponentState,csAncestor)
  else
    Exclude(FCOmponentState,csAncestor);

  if Assigned(FComponents) then
    For Runner := 0 To FComponents.Count-1 do
      TComponent(FComponents.List^[Runner]).SetAncestor(Value);
end;


procedure TComponent.SetDesigning(Value: Boolean; SetChildren : Boolean = True);
var
  Runner: Integer;
begin
  if Value then
    Include(FComponentState,csDesigning)
  else
    Exclude(FComponentState,csDesigning);

  if Assigned(FComponents) and SetChildren then
    For Runner := 0 To FComponents.Count - 1 do
      TComponent(FComponents.List^[Runner]).SetDesigning(Value);
end;


procedure TComponent.SetDesignInstance(Value: Boolean);
begin
  if Value then
    Include(FComponentState,csDesignInstance)
  else
    Exclude(FComponentState,csDesignInstance);
end;


procedure TComponent.SetInline(Value: Boolean);
begin
  if Value then
    Include(FComponentState,csInline)
  else
    Exclude(FComponentState,csInline);
end;


procedure TComponent.SetName(const NewName: TComponentName);
begin
  if FName=NewName then Exit;
  if (NewName<>'') and not IsValidIdent(NewName) then
    raise EComponentError.CreateFmt(SInvalidName,[NewName]);

  if Assigned(FOwner) Then
    FOwner.ValidateRename(Self,FName,NewName)
  else
    ValidateRename(Nil,FName,NewName);

  SetReference(False);
  //ChangeName(NewName);
  FName := NewName;
  Setreference(True);
end;


procedure TComponent.SetChildOrder(Child: TComponent; Order: Integer);
begin
  // does nothing
end;


procedure TComponent.SetParentComponent(Value: TComponent);
begin
  // Does nothing
end;


procedure TComponent.Updating;
begin
  Include (FComponentState,csUpdating);
end;


procedure TComponent.Updated;
begin
  Exclude(FComponentState,csUpdating);
end;


class procedure TComponent.UpdateRegistry(Register: Boolean; const ClassID, ProgID: string);
begin
  // For compatibility only.
end;


procedure TComponent.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
//!! This contradicts the Delphi manual.
  if Assigned(AComponent) and (CompareText(CurName,NewName)<>0) and
      (AComponent.Owner = Self) and Assigned(FindComponent(NewName)) then
    raise EComponentError.Createfmt(SDuplicateName,[newname]);

  if (csDesigning in FComponentState) and Assigned(FOwner) then
    FOwner.ValidateRename(AComponent,Curname,Newname);
end;


procedure TComponent.ValidateContainer(AComponent: TComponent);
begin
  AComponent.ValidateInsert(Self);
end;


procedure TComponent.ValidateInsert(AComponent: TComponent);
begin
  // Does nothing.
end;


procedure TComponent.WriteState(Writer: TWriter);
begin
  Writer.WriteComponentData(Self);
end;


constructor TComponent.Create(AOwner: TComponent);
begin
  FComponentStyle := [csInheritable];
  if Assigned(AOwner) then
    AOwner.InsertComponent(Self);
end;


destructor TComponent.Destroy;
var
  C: TComponent;
begin
  while Assigned(FFreeNotifies) and (FFreeNotifies.Count > 0) do
  begin
    C := TComponent(FFreeNotifies.List^[0]);
    FFreeNotifies.Delete(0);
    C.Notification(Self, opRemove);
  end;
  FreeAndNil(FFreeNotifies);
  Exclude(FComponentState,csFreeNotification);

  DestroyComponents;
  if Assigned(FOwner) then
    FOwner.RemoveComponent(Self);

  inherited Destroy;
end;


procedure TComponent.BeforeDestruction;
begin
  Destroying;
end;


procedure TComponent.DestroyComponents;
var
  acomponent: TComponent;
begin
  while assigned(FComponents) do
  begin
    aComponent := TComponent(FComponents.Last);
    Remove(aComponent);
    Acomponent.Free;
  end;
end;


procedure TComponent.Destroying;
var
  Runner: Integer;
begin
  if csDestroying in FComponentstate then Exit;
  Include(FComponentState,csDestroying);
  if Assigned(FComponents) then
    for Runner := 0 to FComponents.Count-1 do
      TComponent(FComponents.List^[Runner]).Destroying;
end;


function TComponent.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action.HandlesTarget(Self) then
  begin
    Action.ExecuteTarget(Self);
    Result := True;
  end else
    Result := False;
end;


function TComponent.FindComponent(const AName: string): TComponent;
var
  I: Integer;
begin
  Result := nil;
  if (AName='') or not Assigned(FComponents) then
    Exit;

  for I := 0 to FComponents.Count-1 do
    if (CompareText(TComponent(FComponents.List^[I]).Name,AName)=0) then
    begin
      Result := TComponent(FComponents.List^[I]);
      Exit;
    end;
end;


procedure TComponent.FreeNotification(AComponent: TComponent);
begin
  if Assigned(Owner) and (AComponent=Owner) then
    Exit;

  if not (csDestroying in ComponentState) then
  begin
    if not Assigned(FFreeNotifies) then
    begin
      FFreeNotifies := TList.Create;
      Include(FComponentState,csFreeNotification);
    end;

    if FFreeNotifies.IndexOf(AComponent)=-1 then
    begin
      FFreeNotifies.Add(AComponent);
      AComponent.FreeNotification(Self);
    end;
  end else
    AComponent.Notification(Self,opRemove)
end;


procedure TComponent.RemoveFreeNotification(AComponent: TComponent);
begin
  RemoveNotification(AComponent);
  AComponent.RemoveNotification(Self);
end;


procedure TComponent.FreeOnRelease;
begin
  // Delphi compatibility only at the moment.
end;


function TComponent.GetParentComponent: TComponent;
begin
  Result := nil;
end;


function TComponent.HasParent: Boolean;
begin
  Result := False;
end;


procedure TComponent.InsertComponent(AComponent: TComponent);
begin
  AComponent.ValidateContainer(Self);
  ValidateRename(AComponent,'',AComponent.FName);
  Insert(AComponent);
  AComponent.SetReference(True);
  if csDesigning in FComponentState then
    AComponent.SetDesigning(true);
  Notification(AComponent,opInsert);
end;


procedure TComponent.RemoveComponent(AComponent: TComponent);
begin
  Notification(AComponent,opRemove);
  AComponent.SetReference(False);
  Remove(AComponent);
  Acomponent.Setdesigning(False);
  ValidateRename(AComponent,AComponent.FName,'');
end;


function  TComponent.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): Integer;
begin
  SafeCallException := 0;
end;


procedure TComponent.SetSubComponent(ASubComponent: Boolean);
begin
  if ASubComponent then
    Include(FComponentStyle, csSubComponent)
  else
    Exclude(FComponentStyle, csSubComponent);
end;


function TComponent.UpdateAction(Action: TBasicAction): Boolean;
begin
  if Action.HandlesTarget(Self) then
  begin
    Action.UpdateTarget(Self);
    Result := True;
  end else
    Result := False;
end;


function TComponent.QueryInterface(const IID: TGUID; out Obj): HResult;stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;


function TComponent._AddRef: Integer;stdcall;
begin
  Result := -1;
end;


function TComponent._Release: Integer;stdcall;
begin
  Result := -1;
end;


function TComponent.iicrGetComponent: TComponent;
begin
  Result := self;
end;
