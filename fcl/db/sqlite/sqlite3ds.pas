unit sqlite3ds;

{
    This is TSqlite3Dataset, a TDataset descendant class for use with fpc compiler
    Copyright (C) 2004  Luiz Américo Pereira Câmara
    Email: pascalive@bol.com.br

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 2.1 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

{$mode objfpc}
{$H+}
{ $Define DEBUG}

interface

uses
  Classes, SysUtils, customsqliteds;

type
  { TSqlite3Dataset }

  TSqlite3Dataset = class (TCustomSqliteDataset)
  private
    function SqliteExec(AHandle: Pointer; ASql:PChar):Integer;override;
    function GetSqliteHandle: Pointer; override;
    function GetSqliteVersion: String; override;
    procedure SqliteClose(AHandle: Pointer);override;
    procedure BuildLinkedList; override;
  protected
    procedure InternalInitFieldDefs; override;
  public
    function SqliteReturnString: String; override;
    function TableExists: Boolean;override;
    function QuickQuery(const ASql:String;const AStrList: TStrings;FillObjects:Boolean):String;override;
  end;

implementation

uses
  sqlite3,db;

function GetAutoIncValue(NextValue: Pointer; Columns: Integer; ColumnValues: PPChar; ColumnNames: PPChar): integer; cdecl;
var
  CodeError, TempInt: Integer;
begin
  TempInt:=-1;
  if ColumnValues[0] <> nil then
  begin
    Val(StrPas(ColumnValues[0]),TempInt,CodeError);
    if CodeError <> 0 then
      DatabaseError('SqliteDs - Error trying to get last autoinc value');
  end;
  Integer(NextValue^):=Succ(TempInt);
  Result:=1;
end;

{ TSqlite3Dataset }

function TSqlite3Dataset.SqliteExec(AHandle: Pointer; ASql: PChar): Integer;
begin
  Result:=sqlite3_exec(AHandle, ASql, nil, nil, nil);
end;

procedure TSqlite3Dataset.SqliteClose(AHandle: Pointer);
begin
  sqlite3_close(AHandle);
  //todo:handle return data
end;


function TSqlite3Dataset.GetSqliteHandle: Pointer;
begin
  FSqliteReturnId:=sqlite3_open(PChar(FFileName),@Result);
end;

procedure TSqlite3Dataset.InternalInitFieldDefs;
var
  vm:Pointer;
  ColumnStr:String;
  Counter,FieldSize:Integer;
  AType:TFieldType;
begin
  FieldDefs.Clear;
  sqlite3_prepare(FSqliteHandle,PChar(FSql),-1,@vm,nil);
	sqlite3_step(vm);
	for Counter:= 0 to sqlite3_column_count(vm) - 1 do
	begin
   ColumnStr:= UpperCase(StrPas(sqlite3_column_decltype(vm,Counter)));
   if (ColumnStr = 'INTEGER') then
   begin
     AType:= ftInteger;
     FieldSize:=SizeOf(LongInt);
   end else if (ColumnStr = 'VARCHAR') then
   begin
     AType:= ftString;
     FieldSize:=10;//??
   end else if (ColumnStr = 'BOOLEAN') then
   begin
     AType:= ftBoolean;
     FieldSize:=SizeOf(Boolean);
   end else if (ColumnStr = 'FLOAT') then
   begin
     AType:= ftFloat;
     FieldSize:=SizeOf(Double);
   end else if (ColumnStr = 'WORD') then
   begin
     AType:= ftWord;
     FieldSize:=SizeOf(Word);
   end else if (ColumnStr = 'DATETIME') then
   begin
     AType:= ftDateTime;
     FieldSize:=SizeOf(TDateTime);
   end else if (ColumnStr = 'DATE') then
   begin
     AType:= ftDate;
     FieldSize:=SizeOf(TDateTime);
   end else if (ColumnStr = 'LARGEINT') then
   begin
     AType:= ftLargeInt;
     FieldSize:=SizeOf(Int64);
   end else if (ColumnStr = 'CURRENCY') then
   begin
     AType:= ftCurrency;
     FieldSize:=SizeOf(Double);
   end else if (ColumnStr = 'TIME') then
   begin
     AType:= ftTime;
     FieldSize:=SizeOf(TDateTime);
   end else if (ColumnStr = 'MEMO') then
   begin
     AType:= ftMemo;
     FieldSize:=10;//??
   end else if (ColumnStr = 'AUTOINC') then
   begin
     AType:= ftAutoInc;
     FieldSize:=SizeOf(Integer);
     if FAutoIncFieldNo = -1 then
       FAutoIncFieldNo:= Counter;
   end else
   begin
     DatabaseError('Field type "'+ColumnStr+'" not recognized',Self);
   end;
   FieldDefs.Add(StrPas(sqlite3_column_name(vm,Counter)), AType, FieldSize, False);
   {$ifdef DEBUG}
   writeln('Field Name: ',sqlite3_column_name(vm,Counter));
   writeln('Field Type: ',sqlite3_column_decltype(vm,Counter));
   {$endif}
  end;
	sqlite3_finalize(vm);
  FRowBufferSize:=(SizeOf(PPChar)*FieldDefs.Count);
  {$ifdef DEBUG}
  writeln('FieldDefs.Count: ',FieldDefs.Count);
  {$endif}
end;

procedure TSqlite3Dataset.BuildLinkedList;
var
  TempItem:PDataRecord;
  vm:Pointer;
  Counter:Integer;
begin
  //Get AutoInc Field initial value
  if FAutoIncFieldNo <> -1 then
    sqlite3_exec(FSqliteHandle,PChar('Select Max('+Fields[FAutoIncFieldNo].FieldName+') from ' + FTableName),
      @GetAutoIncValue,@FNextAutoInc,nil);

  FSqliteReturnId:=sqlite3_prepare(FSqliteHandle,Pchar(FSql),-1,@vm,nil);
  if FSqliteReturnId <> SQLITE_OK then
  case FSqliteReturnId of
  SQLITE_ERROR:
    DatabaseError('Invalid SQL',Self);
  else
    DatabaseError('Error returned by sqlite while retrieving data: '+SqliteReturnString,Self);
  end;

  FDataAllocated:=True;

  TempItem:=FBeginItem;
  FRecordCount:=0;
  FRowCount:=sqlite3_column_count(vm);
  FSqliteReturnId:=sqlite3_step(vm);
  while FSqliteReturnId = SQLITE_ROW do
  begin
    Inc(FRecordCount);
    New(TempItem^.Next);
    TempItem^.Next^.Previous:=TempItem;
    TempItem:=TempItem^.Next;
    GetMem(TempItem^.Row,FRowBufferSize);
    For Counter := 0 to FRowCount - 1 do
      TempItem^.Row[Counter]:=StrNew(sqlite3_column_text(vm,Counter));
    FSqliteReturnId:=sqlite3_step(vm);
  end;
  sqlite3_finalize(vm);

  // Attach EndItem
  TempItem^.Next:=FEndItem;
  FEndItem^.Previous:=TempItem;

  // Alloc item used in append/insert
  GetMem(FCacheItem^.Row,FRowBufferSize);
  for Counter := 0 to FRowCount - 1 do
    FCacheItem^.Row[Counter]:=nil;
  // Fill FBeginItem.Row with nil -> necessary for avoid exceptions in empty datasets
  GetMem(FBeginItem^.Row,FRowBufferSize);
  for Counter := 0 to FRowCount - 1 do
    FBeginItem^.Row[Counter]:=nil;
end;

function TSqlite3Dataset.TableExists: Boolean;
var
  AHandle,vm:Pointer;
begin
  Result:=False;
  if not (FTableName = '') and FileExists(FFileName) then
  begin
    if FSqliteHandle = nil then
    begin
      {$ifdef DEBUG}
      writeln('TableExists - FSqliteHandle=nil : Opening a file');
      {$endif}
      AHandle:=GetSqliteHandle;
    end
    else
    begin
      {$ifdef DEBUG}
      writeln('TableExists - FSqliteHandle<>nil : Using FSqliteHandle');
      {$endif}
      AHandle:=FSqliteHandle;
    end;
    FSqliteReturnId:=sqlite3_prepare(AHandle,
    Pchar('SELECT name FROM SQLITE_MASTER WHERE type = ''table'' AND name LIKE '''+ FTableName+ ''';'),
      -1,@vm,nil);
    {$ifdef DEBUG}
    WriteLn('TableExists.sqlite3_prepare - SqliteReturnString:',SqliteReturnString);
    {$endif}
    FSqliteReturnId:=sqlite3_step(vm);
    {$ifdef DEBUG}
    WriteLn('TableExists.sqlite3_step - SqliteReturnString:',SqliteReturnString);
    {$endif}
    Result:=FSqliteReturnId = SQLITE_ROW;
    sqlite3_finalize(vm);
    if (FSqliteHandle = nil) then
      sqlite3_close(AHandle);
  end;
  {$ifdef DEBUG}
  WriteLn('TableExists ('+FTableName+') Result:',Result);
  {$endif}
end;

function TSqlite3Dataset.SqliteReturnString: String;
begin
 case FSqliteReturnId of
      SQLITE_OK           : Result := 'SQLITE_OK          ';
      SQLITE_ERROR        : Result := 'SQLITE_ERROR       ';
      SQLITE_INTERNAL     : Result := 'SQLITE_INTERNAL    ';
      SQLITE_PERM         : Result := 'SQLITE_PERM        ';
      SQLITE_ABORT        : Result := 'SQLITE_ABORT       ';
      SQLITE_BUSY         : Result := 'SQLITE_BUSY        ';
      SQLITE_LOCKED       : Result := 'SQLITE_LOCKED      ';
      SQLITE_NOMEM        : Result := 'SQLITE_NOMEM       ';
      SQLITE_READONLY     : Result := 'SQLITE_READONLY    ';
      SQLITE_INTERRUPT    : Result := 'SQLITE_INTERRUPT   ';
      SQLITE_IOERR        : Result := 'SQLITE_IOERR       ';
      SQLITE_CORRUPT      : Result := 'SQLITE_CORRUPT     ';
      SQLITE_NOTFOUND     : Result := 'SQLITE_NOTFOUND    ';
      SQLITE_FULL         : Result := 'SQLITE_FULL        ';
      SQLITE_CANTOPEN     : Result := 'SQLITE_CANTOPEN    ';
      SQLITE_PROTOCOL     : Result := 'SQLITE_PROTOCOL    ';
      SQLITE_EMPTY        : Result := 'SQLITE_EMPTY       ';
      SQLITE_SCHEMA       : Result := 'SQLITE_SCHEMA      ';
      SQLITE_TOOBIG       : Result := 'SQLITE_TOOBIG      ';
      SQLITE_CONSTRAINT   : Result := 'SQLITE_CONSTRAINT  ';
      SQLITE_MISMATCH     : Result := 'SQLITE_MISMATCH    ';
      SQLITE_MISUSE       : Result := 'SQLITE_MISUSE      ';
      SQLITE_NOLFS        : Result := 'SQLITE_NOLFS       ';
      SQLITE_AUTH         : Result := 'SQLITE_AUTH        ';
      SQLITE_FORMAT       : Result := 'SQLITE_FORMAT      ';
      SQLITE_RANGE        : Result := 'SQLITE_RANGE       ';
      SQLITE_ROW          : Result := 'SQLITE_ROW         ';
      SQLITE_NOTADB       : Result := 'SQLITE_NOTADB      ';
      SQLITE_DONE         : Result := 'SQLITE_DONE        ';
  else
    Result:='Unknow Return Value';
 end;
end;

function TSqlite3Dataset.GetSqliteVersion: String;
begin
  Result:=StrPas(sqlite3_version);
end;

function TSqlite3Dataset.QuickQuery(const ASql:String;const AStrList: TStrings;FillObjects:Boolean):String;
var
  vm,AHandle:Pointer;
    
  procedure FillStrings;
  begin
    while FSqliteReturnId = SQLITE_ROW do
    begin
      AStrList.Add(StrPas(sqlite3_column_text(vm,0)));
      FSqliteReturnId:=sqlite3_step(vm);  
    end;
  end;
  procedure FillStringsAndObjects;
  begin
    while FSqliteReturnId = SQLITE_ROW do
    begin
      AStrList.AddObject(StrPas(sqlite3_column_text(vm,0)),TObject(PtrInt(sqlite3_column_int(vm,1))));
      FSqliteReturnId:=sqlite3_step(vm);  
    end;
  end;    
begin
  if FSqliteHandle <> nil then
    AHandle:=FSqliteHandle
  else
    if FileExists(FFileName) then
      AHandle:=GetSqliteHandle
    else
      DatabaseError('File "'+FFileName+'" not Exists',Self);    
  Result:='';
  // It's up to the caller clear or not the list
  //if AStrList <> nil then
  //  AStrList.Clear;
  FSqliteReturnId:=sqlite3_prepare(AHandle,Pchar(ASql),-1,@vm,nil);
  if FSqliteReturnId <> SQLITE_OK then
    DatabaseError('Error returned by sqlite in QuickQuery: '+SqliteReturnString,Self);
    
  FSqliteReturnId:=sqlite3_step(vm);
  if (FSqliteReturnId = SQLITE_ROW) and (sqlite3_column_count(vm) > 0) then
  begin
    Result:=StrPas(sqlite3_column_text(vm,0));
    if AStrList <> nil then
    begin   
      if FillObjects and (sqlite3_column_count(vm) > 1) then
        FillStringsAndObjects
      else
        FillStrings;
    end;          
  end;  
  sqlite3_finalize(vm); 
  if FSqliteHandle = nil then
    sqlite3_close(AHandle);
end;

end.

