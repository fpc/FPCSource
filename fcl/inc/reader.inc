{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

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
  GetPosition:=0;
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
  ReadSet:=0;
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
  Error:=false;
end;



Function TReader.FindMethod(ARoot: TComponent; const AMethodName: string): Pointer;

begin
  FindMethod:=nil;
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
  FindAncestorComponent:=nil;
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
  EndOfList:=false;
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
  NextValue:=vaNull;
end;



Procedure TReader.Read(var Buf; Count: Longint);

begin
end;



Function TReader.ReadBoolean: Boolean;

begin
  ReadBoolean:=false;
end;



Function TReader.ReadChar: Char;

begin
  ReadChar:=#0;
end;



Procedure TReader.ReadCollection(Collection: TCollection);

begin
end;



Function TReader.ReadComponent(Component: TComponent): TComponent;

begin
  ReadComponent:=nil;
end;



Procedure TReader.ReadComponents(AOwner, AParent: TComponent;
  Proc: TReadComponentsProc);

begin
end;



Function TReader.ReadFloat: Extended;

begin
  ReadFloat:=0.0;
end;



Function TReader.ReadIdent: string;

begin
  ReadIdent:='';
end;



Function TReader.ReadInteger: Longint;

begin
  ReadInteger:=0;
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



Function TReader.ReadRootComponent(ARoot: TComponent): TComponent;

begin
  ReadRootComponent:=nil;
end;



Procedure TReader.ReadSignature;

begin
end;



Function TReader.ReadStr: string;

begin
  ReadStr:='';
end;



Function TReader.ReadString: string;

begin
  ReadString:='';
end;



Function TReader.ReadValue: TValueType;

begin
  ReadValue:=vaNull;
end;



Procedure TReader.CopyValue(Writer: TWriter); {!!!}

begin
end;
{
  $Log$
  Revision 1.6  2000-01-07 01:24:33  peter
    * updated copyright to 2000

  Revision 1.5  2000/01/06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.2  2000/01/04 18:07:16  michael
  + Streaming implemented

  Revision 1.3  1999/09/13 08:35:16  fcl
  * Changed some argument names (Root->ARoot etc.) because the new compiler
    now performs more ambiguity checks  (sg)

  Revision 1.2  1999/04/08 10:18:54  peter
    * makefile updates

}
