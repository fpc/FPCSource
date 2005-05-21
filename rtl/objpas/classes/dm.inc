{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    <What does this file>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Constructor TDataModule.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  if (ClassType <> TDataModule) and
     not (csDesigning in ComponentState) then
    begin
    if not InitInheritedComponent(Self, TDataModule) then
      raise EStreamError.CreateFmt(SErrNoSTreaming, [ClassName]);
    if OldCreateOrder then
      DoCreate;
    end;
end;

Constructor TDataModule.CreateNew(AOwner: TComponent);

begin
  CreateNew(AOwner,0);
end;

constructor TDataModule.CreateNew(AOwner: TComponent; CreateMode: Integer);
begin
  inherited Create(AOwner);
  if Assigned(AddDataModule) and (CreateMode>=0) then
    AddDataModule(Self);
end;

Procedure TDataModule.AfterConstruction;
begin
   If not OldCreateOrder then
     DoCreate;
end;

Procedure TDataModule.BeforeDestruction;
begin
  Destroying;
  RemoveFixupReferences(Self, '');
  if not OldCreateOrder then
    DoDestroy;
end;

destructor TDataModule.Destroy;
begin
  if OldCreateOrder then
    DoDestroy;
  if Assigned(RemoveDataModule) then
    RemoveDataModule(Self);
  inherited Destroy;
end;

Procedure TDataModule.DoCreate;
begin
  if Assigned(FOnCreate) then
    try
      FOnCreate(Self);
    except
      if not HandleCreateException then
        raise;
    end;
end;

Procedure TDataModule.DoDestroy;
begin
  if Assigned(FOnDestroy) then
    try
      FOnDestroy(Self);
    except
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(Self);
    end;
end;

procedure TDataModule.DefineProperties(Filer: TFiler);

var
  Ancestor : TDataModule;
  HaveData : Boolean;

begin
  inherited DefineProperties(Filer);
  Ancestor := TDataModule(Filer.Ancestor);
  HaveData:=(Ancestor=Nil) or
            (FDSize.X<>Ancestor.FDSize.X) or
            (FDSize.Y<>Ancestor.FDSize.Y) or
            (FDPos.Y<>Ancestor.FDPos.Y) or
            (FDPos.X<>Ancestor.FDPos.X);
  Filer.DefineProperty('Height', @ReadH, @WriteH, HaveData);
  Filer.DefineProperty('HorizontalOffset', @ReadL, @WriteL, HaveData);
  Filer.DefineProperty('VerticalOffset', @ReadT,@WriteT, HaveData);
  Filer.DefineProperty('Width', @ReadW, @WriteW, HaveData);
end;

procedure TDataModule.GetChildren(Proc: TGetChildProc; Root: TComponent);

var
  I : Integer;

begin
  inherited GetChildren(Proc, Root);
  if (Root=Self) then
    for I:=0 to ComponentCount-1 do
      If Not Components[I].HasParent then
         Proc(Components[i]);
end;


function TDataModule.HandleCreateException: Boolean;
begin
  Result:=Assigned(ApplicationHandleException);
  if Result then
    ApplicationHandleException(Self);
end;

Procedure TDataModule.ReadState(Reader: TReader);
begin
  FOldOrder := false;
  inherited ReadState(Reader);
end;

Procedure TDataModule.ReadT(Reader: TReader);
begin
  FDPos.Y := Reader.ReadInteger;
end;

Procedure TDataModule.WriteT(Writer: TWriter);
begin
  Writer.WriteInteger(FDPos.Y);
end;

Procedure TDataModule.ReadL(Reader: TReader);
begin
  FDPos.X := Reader.ReadInteger;
end;

Procedure TDataModule.WriteL(Writer: TWriter);
begin
  Writer.WriteInteger(FDPos.X);
end;

Procedure TDataModule.ReadW(Reader: TReader);
begin
  FDSIze.X := Reader.ReadInteger;
end;

Procedure TDataModule.WriteW(Writer: TWriter);
begin
  Writer.WriteInteger(FDSIze.X);
end;

Procedure TDataModule.ReadH(Reader: TReader);
begin
  FDSIze.Y := Reader.ReadInteger;
end;

Procedure TDataModule.WriteH(Writer: TWriter);
begin
  Writer.WriteInteger(FDSIze.Y);
end;
