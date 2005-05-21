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

program testopen;

uses db,memds,classes,sysutils;

Procedure DoTest;

var
  I,ACount : integer;
  D   : TDateTime;

begin
  with TMemDataset.Create(Nil) do
    Try
      FileName:=ParamStr(1);
      Open;
      First;
      ACount:=0;
      While Not EOF do
        begin
        Inc(ACount);
        Writeln('Record ',ACount,' : ');
        Writeln('------------------------');
        For I:=0 to FieldCount-1 do
          Writeln(Fields[I].FieldName,' : ',Fields[I].AsString);
        Writeln;
        Next;
        end;
      Writeln('Total data size : ',DataSize);
      Close;
  finally
    Free;
  end;
end;

begin
  If ParamCount<>1 then
    begin
    Writeln('Usage : testopen <filename>');
    Halt(1);
    end;
  DoTest;
end.
