program browseds;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canney and other members of the
    Free Pascal development team

    Dumps SQL table.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$Mode ObjFpc}
{$H+}
{$define DEBUGHEAP}

uses 
{$ifdef DEBUGHEAP}
  Heaptrc,
{$endif}
{$ifdef Linux}
  cmem,
{$endif}
  crt,
  sqlite3ds,
  sysutils, db, inifiles;

const
  SQLITEDS_TESTS_INI_FILE = 'sqlitedstests.ini';
  DEFAULT_TABLENAME = 'tabletest';
  DEFAULT_FILENAME = 'test.db';

var 
  dsTest: TSqlite3Dataset;
  ini: TIniFile;
  i:Integer;

procedure DumpFieldData (F : TField);
begin
  with F do
  begin
    Write (FieldName:10,FieldTypeNames[DataType]:12);
    if DataType <> ftMemo then
      Write(AsString:30)
    else
      Write('(memo)':30);
    case Datatype of
      ftSmallint, ftInteger, ftWord, ftAutoInc : Writeln (AsLongint:28);
      ftBoolean : Writeln (AsBoolean:28);
      ftFloat : Writeln (AsFloat:28);
      ftDate, ftTime, ftDateTime : Writeln (AsDateTime:28);
      ftLargeInt: WriteLn(AsLargeInt:28);
      ftMemo: WriteLn('(memo)':28);
      ftString: WriteLn(AsString:28);
      ftCurrency: WriteLn(AsCurrency:28);
    else
      WriteLn;
    end;
  end;
end;

begin
  {$ifdef DEBUGHEAP}
  SetHeapTraceOutput(ExtractFileName(ParamStr(0))+'.heap.log');
  {$endif}
  dsTest:=TSqlite3Dataset.Create(nil);
  with dsTest do
  begin
    //Load Database properties from a inifile
    ini:=TIniFile.Create(SQLITEDS_TESTS_INI_FILE);
    FileName:=ini.ReadString('testinfo','filename',DEFAULT_FILENAME);
    TableName:=ini.ReadString('testinfo','tablename',DEFAULT_TABLENAME);
    ini.Destroy;
    //Calling Open with an empty SQL, is the same of setting SQL to 'SELECT * from [TableName]';
    Open;
    //Browse all records
    while not Eof do
    begin
      ClrScr;
      Writeln('Record ',RecNo,'/',RecordCount);
      Writeln('--------------------------------------------------------------------------------');
      Writeln ('Field Name':10,'Data Type':12,'As String':30, 'As Native Type':28);
      Writeln('--------------------------------------------------------------------------------');
      for i:=0 to FieldCount - 1 do
        DumpFieldData(Fields[I]);
      Next;
      WriteLn;
      if not Eof then
        WriteLn(':::: Press a key to see the next record ::::')
      else
        WriteLn(':::: Press a key to finish the program ::::');
      Readkey;
    end;
    Destroy;
  end;
end.
