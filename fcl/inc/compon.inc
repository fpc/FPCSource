{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************}
{*                             TComponent                                   *}
{****************************************************************************}


Function  TComponent.GetComponent(AIndex: Integer): TComponent;

begin
end;


Function  TComponent.GetComponentCount: Integer;

begin
end;


Function  TComponent.GetComponentIndex: Integer;

begin
end;


Procedure TComponent.Insert(AComponent: TComponent);

begin
end;


Procedure TComponent.ReadLeft(Reader: TReader);

begin
end;


Procedure TComponent.ReadTop(Reader: TReader);

begin
end;


Procedure TComponent.Remove(AComponent: TComponent);

begin
end;


Procedure TComponent.SetComponentIndex(Value: Integer);

begin
end;


Procedure TComponent.SetReference(Enable: Boolean);

begin
end;


Procedure TComponent.WriteLeft(Writer: TWriter);

begin
end;


Procedure TComponent.WriteTop(Writer: TWriter);

begin
end;


Procedure TComponent.ChangeName(const NewName: TComponentName);

begin
end;


Procedure TComponent.DefineProperties(Filer: TFiler); 

begin
end;


Procedure TComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);

begin
end;


Function  TComponent.GetChildOwner: TComponent;

begin
end;


Function  TComponent.GetChildParent: TComponent; 

begin
end;


Function  TComponent.GetNamePath: string; 

begin
end;


Function  TComponent.GetOwner: TPersistent; 

begin
end;


Procedure TComponent.Loaded; 

begin
end;


Procedure TComponent.Notification(AComponent: TComponent;
  Operation: TOperation);

begin
end;


Procedure TComponent.ReadState(Reader: TReader); 

begin
end;


Procedure TComponent.SetAncestor(Value: Boolean);

begin
end;


Procedure TComponent.SetDesigning(Value: Boolean);

begin
end;


Procedure TComponent.SetName(const NewName: TComponentName); 

begin
end;


Procedure TComponent.SetChildOrder(Child: TComponent; Order: Integer);

begin
end;


Procedure TComponent.SetParentComponent(Value: TComponent);

begin
end;


Procedure TComponent.Updating; 

begin
end;


Procedure TComponent.Updated; 

begin
end;


class Procedure TComponent.UpdateRegistry(Register: Boolean; const ClassID, ProgID: string); 

begin
end;


Procedure TComponent.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string); 

begin
end;


Procedure TComponent.ValidateContainer(AComponent: TComponent); 

begin
end;


Procedure TComponent.ValidateInsert(AComponent: TComponent); 

begin
end;


Procedure TComponent.WriteState(Writer: TWriter); 

begin
end;


Constructor TComponent.Create(AOwner: TComponent); 

begin
end;


Destructor TComponent.Destroy; 

begin
end;


Procedure TComponent.DestroyComponents;

begin
end;


Procedure TComponent.Destroying;

begin
end;


Function  TComponent.FindComponent(const AName: string): TComponent;

begin
end;


Procedure TComponent.FreeNotification(AComponent: TComponent);

begin
end;


Procedure TComponent.FreeOnRelease;

begin
end;


Function  TComponent.GetParentComponent: TComponent; 

begin
end;


Function  TComponent.HasParent: Boolean; 

begin
end;


Procedure TComponent.InsertComponent(AComponent: TComponent);

begin
end;


Procedure TComponent.RemoveComponent(AComponent: TComponent);

begin
end;


Function  TComponent.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): Integer; 

begin
end;

{
  $Log$
  Revision 1.1  1998-05-04 14:30:11  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
