{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

program testpop;

uses db,memds,classes,sysutils;

Procedure DoTest;

var
  I,ACount : integer;
  D   : TDateTime;

begin
  with TMemDataset.Create(Nil) do
    Try
      With FieldDefs do
        begin
        Clear;
        Add('Boolean', ftBoolean, 0, False);
        Add('Integer', ftInteger, 0, False);
        Add('SmallInt', ftSmallInt, 0, False);
        Add('Float', ftFloat, 0, False);
        Add('String', ftString, 30, False);
        Add('Time', ftTime, 0, False);
        Add('Date', ftDate, 0, False);
        end;
      CreateTable;
      Open;
      D:=now;
      ACount:=1000;
      for I:=1 to ACount do
        begin
        Append;
        FieldByName('Boolean').AsBoolean:=False;
        FieldByName('Integer').AsInteger:=I;
        FieldByName('SmallInt').AsInteger:=I;
        FieldByName('Float').AsFloat:=I/10;
        FieldByName('String').AsString:='Test-Data '+IntToStr(I);
        FieldByName('Time').AsDateTime:=D;
        FieldByName('Date').AsDateTime:=D;
        Post;
        end;
      First;
      ACount:=0;
      While Not EOF do
        begin
        Inc(ACount);
        Writeln('Record ',ACount,' : ');
        Writeln('------------------------');
        For I:=0 to Fields.Count-1 do
          Writeln(Fields[I].FieldName,' : ',Fields[I].AsString);
        Writeln;
        Next;
        end;
      Writeln('Total data size : ',DataSize);
      If (ParamCount>0) then
        FileName:=ParamStr(1);
      Close;
  finally
    Free;
  end;
end;

begin
  DoTest;
end.
