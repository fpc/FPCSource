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
{*                             TWriter                                      *}
{****************************************************************************}

Procedure TWriter.AddAncestor(Component: TComponent);

begin
end;


function  TWriter.GetPosition: Longint;

begin
  GetPosition:=0;
end;


Procedure TWriter.SetPosition(Value: Longint);

begin
end;


Procedure TWriter.WriteBuffer;

begin
end;


Procedure TWriter.WriteData(Instance: TComponent);

begin
end;


Procedure TWriter.WriteProperty(Instance: TPersistent; PropInfo: Pointer);

begin
end;


Procedure TWriter.WriteProperties(Instance: TPersistent);

begin
end;


Procedure TWriter.WritePropName(const PropName: string);

begin
end;


Procedure TWriter.WriteBinary(wd : TStreamProc);

begin
end;


Procedure TWriter.WritePrefix(Flags: TFilerFlags; AChildPos: Integer);

begin
end;


Procedure TWriter.WriteValue(Value: TValueType);

begin
end;


Destructor TWriter.Destroy;

begin
end;


Procedure TWriter.DefineProperty(const Name: string;
  rd : TReaderProc; wd : TWriterProc;
  HasData: Boolean);

begin
end;


Procedure TWriter.DefineBinaryProperty(const Name: string;
  rd, wd: TStreamProc;
  HasData: Boolean);

begin
end;


Procedure TWriter.FlushBuffer;

begin
end;


Procedure TWriter.Write(const Buf; Count: Longint);

begin
end;


Procedure TWriter.WriteBoolean(Value: Boolean);

begin
end;


Procedure TWriter.WriteCollection(Value: TCollection);

begin
end;


Procedure TWriter.WriteComponent(Component: TComponent);

begin
end;


Procedure TWriter.WriteChar(Value: Char);

begin
end;


Procedure TWriter.WriteDescendent(ARoot: TComponent; AAncestor: TComponent);

begin
end;


Procedure TWriter.WriteFloat(Value: Extended);

begin
end;


Procedure TWriter.WriteIdent(const Ident: string);

begin
end;


Procedure TWriter.WriteInteger(Value: Longint);

begin
end;


Procedure TWriter.WriteListBegin;

begin
end;


Procedure TWriter.WriteListEnd;

begin
end;


Procedure TWriter.WriteRootComponent(ARoot: TComponent);

begin
end;


Procedure TWriter.WriteSignature;

begin
end;


Procedure TWriter.WriteStr(const Value: string);

begin
end;


Procedure TWriter.WriteString(const Value: string);

begin
end;
{
  $Log$
  Revision 1.3  1999-09-13 08:35:16  fcl
  * Changed some argument names (Root->ARoot etc.) because the new compiler
    now performs more ambiguity checks  (sg)

  Revision 1.2  1999/04/08 10:18:58  peter
    * makefile updates

}
