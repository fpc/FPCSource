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
{*                             TREADER                                      *}
{****************************************************************************}

Procedure TReader.CheckValue(Value: TValueType);

begin
end;



Procedure TReader.DoFixupReferences;

begin
end;



Procedure TReader.FreeFixups;

begin
end;



Function TReader.GetPosition: Longint;

begin
end;



Procedure TReader.PropertyError;

begin
end;



Procedure TReader.ReadBuffer;

begin
end;



Procedure TReader.ReadData(Instance: TComponent);

begin
end;



Procedure TReader.ReadDataInner(Instance: TComponent);

begin
end;



Procedure TReader.ReadProperty(AInstance: TPersistent);

begin
end;



Procedure TReader.ReadPropValue(Instance: TPersistent; PropInfo: Pointer);

begin
end;



Function TReader.ReadSet(SetType: Pointer): Integer;

begin
end;



Procedure TReader.SetPosition(Value: Longint);

begin
end;



Procedure TReader.SkipSetBody;

begin
end;



Procedure TReader.SkipValue;

begin
end;



Procedure TReader.SkipProperty;

begin
end;



Procedure TReader.SkipComponent(SkipHeader: Boolean);

begin
end;



Function TReader.Error(const Message: string): Boolean; 

begin
end;



Function TReader.FindMethod(Root: TComponent; const MethodName: string): Pointer; 

begin
end;



Procedure TReader.SetName(Component: TComponent; var Name: string); 

begin
end;



Procedure TReader.ReferenceName(var Name: string); 

begin
end;



Function TReader.FindAncestorComponent(const Name: string;
  ComponentClass: TPersistentClass): TComponent; 

begin
end;



destructor TReader.Destroy; 

begin
end;



Procedure TReader.BeginReferences;

begin
end;



Procedure TReader.DefineProperty(const Name: string;
  rd : TReaderProc; wd : TWriterProc;
  HasData: Boolean); 

begin
end;



Procedure TReader.DefineBinaryProperty(const Name: string;
  rd, wd: TStreamProc;
  HasData: Boolean); 

begin
end;



Function TReader.EndOfList: Boolean;

begin
end;



Procedure TReader.EndReferences;

begin
end;



Procedure TReader.FixupReferences;

begin
end;



Procedure TReader.FlushBuffer; 

begin
end;



Function TReader.NextValue: TValueType;

begin
end;



Procedure TReader.Read(var Buf; Count: Longint);

begin
end;



Function TReader.ReadBoolean: Boolean;

begin
end;



Function TReader.ReadChar: Char;

begin
end;



Procedure TReader.ReadCollection(Collection: TCollection);

begin
end;



Function TReader.ReadComponent(Component: TComponent): TComponent;

begin
end;



Procedure TReader.ReadComponents(AOwner, AParent: TComponent;
  Proc: TReadComponentsProc);

begin
end;



Function TReader.ReadFloat: Extended;

begin
end;



Function TReader.ReadIdent: string;

begin
end;



Function TReader.ReadInteger: Longint;

begin
end;



Procedure TReader.ReadListBegin;

begin
end;



Procedure TReader.ReadListEnd;

begin
end;



Procedure TReader.ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer);

begin
end;



Function TReader.ReadRootComponent(Root: TComponent): TComponent;

begin
end;



Procedure TReader.ReadSignature;

begin
end;



Function TReader.ReadStr: string;

begin
end;



Function TReader.ReadString: string;

begin
end;



Function TReader.ReadValue: TValueType;

begin
end;



Procedure TReader.CopyValue(Writer: TWriter); {!!!}

begin
end;
{
  $Log$
  Revision 1.1  1998-05-04 14:30:12  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
