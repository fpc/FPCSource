unit sqliteds;

{
    This is TSqliteDataset, a TDataset descendant class for use with fpc compiler
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
  { TSqliteDataset }

  TSqliteDataset = class (TCustomSqliteDataset)
  private
    function SqliteExec(AHandle: Pointer; ASql:PChar):Integer;override;
    function InternalGetHandle: Pointer; override;
    function GetSqliteEncoding: String;
    function GetSqliteVersion: String; override;
    procedure InternalCloseHandle;override;
    procedure BuildLinkedList; override;
  protected
    procedure InternalInitFieldDefs; override;
    function GetRowsAffected:Integer; override;
  public
    procedure ExecuteDirect(const ASql: String);override;
    function SqliteReturnString: String; override;
    function TableExists(const ATableName:String): Boolean;override;
    function QuickQuery(const ASql:String;const AStrList: TStrings;FillObjects:Boolean):String;override;
    property SqliteEncoding: String read GetSqliteEncoding;
  end;

implementation

uses
  sqlite,db;

var
  DummyAutoIncFieldNo:Integer;

//function sqlite_last_statement_changes(dbhandle:Pointer):longint;cdecl;external 'sqlite' name 'sqlite_last_statement_changes';

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

function GetFieldDefs(TheDataset: Pointer; Columns: Integer; ColumnValues: PPChar; ColumnNames: PPChar): integer; cdecl;
var
  FieldSize:Word;
  i:Integer;
  AType:TFieldType;
  ColumnStr:String;
begin
 // Sqlite is typeless (allows any type in any field)
 // regardless of what is in Create Table, but returns
 // exactly what is in Create Table statement
 // here is a trick to get the datatype.
 // If the field contains another type, may have problems
 for i:= 0 to Columns - 1 do
 begin
   ColumnStr:= UpperCase(StrPas(ColumnNames[i + Columns]));
   if (ColumnStr = 'INTEGER') or (ColumnStr = 'INT') then
   begin
     if TCustomSqliteDataset(TheDataset).AutoIncrementKey and
          (UpperCase(StrPas(ColumnNames[i])) = UpperCase(TCustomSqliteDataset(TheDataset).PrimaryKey)) then
     begin
       AType:= ftAutoInc;
       DummyAutoIncFieldNo:=i;
     end
     else
       AType:= ftInteger;
     FieldSize:=SizeOf(LongInt);
   end else if Pos('VARCHAR',ColumnStr) = 1 then
   begin
     AType:= ftString;
     FieldSize:=0;
   end else if Pos('BOOL',ColumnStr) = 1 then
   begin
     AType:= ftBoolean;
     FieldSize:=SizeOf(WordBool);
   end else if Pos('AUTOINC',ColumnStr) = 1 then
   begin
     AType:= ftAutoInc;
     FieldSize:=SizeOf(LongInt);
     if DummyAutoIncFieldNo = -1 then
       DummyAutoIncFieldNo:= i;
   end else if (Pos('FLOAT',ColumnStr)=1) or (Pos('NUMERIC',ColumnStr)=1) then
   begin
     AType:= ftFloat;
     FieldSize:=SizeOf(Double);
   end else if (ColumnStr = 'DATETIME') then
   begin
     AType:= ftDateTime;
     FieldSize:=SizeOf(TDateTime);
   end else if (ColumnStr = 'DATE') then
   begin
     AType:= ftDate;
     FieldSize:=SizeOf(TDateTime);
   end else if (ColumnStr = 'TIME') then
   begin
     AType:= ftTime;
     FieldSize:=SizeOf(TDateTime);
   end else if (ColumnStr = 'LARGEINT') then
   begin
     AType:= ftLargeInt;
     FieldSize:=SizeOf(LargeInt);
   end else if (ColumnStr = 'TEXT') then
   begin
     AType:= ftMemo;
     FieldSize:=0;
   end else if (ColumnStr = 'CURRENCY') then
   begin
     AType:= ftCurrency;
     FieldSize:=SizeOf(Double);
   end else if (ColumnStr = 'WORD') then
   begin
     AType:= ftWord;
     FieldSize:=SizeOf(Word);
   end else
   begin
     AType:=ftString;
     FieldSize:=0;
   end;
   TDataset(TheDataset).FieldDefs.Add(StrPas(ColumnNames[i]), AType, FieldSize, False);
 end;
 Result:=-1;
end;


{ TSqliteDataset }

function TSqliteDataset.SqliteExec(AHandle: Pointer; ASql: PChar): Integer;
begin
  Result:=sqlite_exec(AHandle, ASql, nil, nil, nil);
end;

procedure TSqliteDataset.InternalCloseHandle;
begin
  sqlite_close(FSqliteHandle);
  FSqliteHandle:=nil;
end;

function TSqliteDataset.InternalGetHandle: Pointer;
begin
  Result:=sqlite_open(PChar(FFileName),0,nil);
end;

procedure TSqliteDataset.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  sqlite_exec(FSqliteHandle,PChar('PRAGMA empty_result_callbacks = ON;PRAGMA show_datatypes = ON;'),nil,nil,nil);
  DummyAutoIncFieldNo:=-1;
  FSqliteReturnId:=sqlite_exec(FSqliteHandle,PChar(FSql),@GetFieldDefs,Self,nil);
  FAutoIncFieldNo:=DummyAutoIncFieldNo;
  {
  if FSqliteReturnId <> SQLITE_ABORT then
     DatabaseError(SqliteReturnString,Self);
  }
  FRowBufferSize:=(SizeOf(PPChar)*FieldDefs.Count);
end;

function TSqliteDataset.GetRowsAffected: Integer;
begin
  Result:=sqlite_changes(FSqliteHandle);
  //Result:=sqlite_last_statement_changes(FSqliteHandle);
end;

procedure TSqliteDataset.ExecuteDirect(const ASql: String);
var
  vm:Pointer;
  ColumnNames,ColumnValues:PPChar;
  ColCount:Integer;
begin
  FSqliteReturnId:=sqlite_compile(FSqliteHandle,Pchar(ASql),nil,@vm,nil);
  if FSqliteReturnId <> SQLITE_OK then
    DatabaseError(SqliteReturnString,Self);

  FSqliteReturnId:=sqlite_step(vm,@ColCount,@ColumnValues,@ColumnNames);

  sqlite_finalize(vm, nil);
end;

procedure TSqliteDataset.BuildLinkedList;
var
  TempItem:PDataRecord;
  vm:Pointer;
  ColumnNames,ColumnValues:PPChar;
  Counter:Integer;
begin
  //Get AutoInc Field initial value
  if FAutoIncFieldNo <> -1 then
    sqlite_exec(FSqliteHandle,PChar('Select Max('+Fields[FAutoIncFieldNo].FieldName+') from ' + FTableName),
      @GetAutoIncValue,@FNextAutoInc,nil);

  FSqliteReturnId:=sqlite_compile(FSqliteHandle,Pchar(FSql),nil,@vm,nil);
  if FSqliteReturnId <> SQLITE_OK then
    DatabaseError(SqliteReturnString,Self);

  FDataAllocated:=True;

  TempItem:=FBeginItem;
  FRecordCount:=0;
  FSqliteReturnId:=sqlite_step(vm,@FRowCount,@ColumnValues,@ColumnNames);
  while FSqliteReturnId = SQLITE_ROW do
  begin
    Inc(FRecordCount);
    New(TempItem^.Next);
    TempItem^.Next^.Previous:=TempItem;
    TempItem:=TempItem^.Next;
    GetMem(TempItem^.Row,FRowBufferSize);
    for Counter := 0 to FRowCount - 1 do
      TempItem^.Row[Counter]:=StrNew(ColumnValues[Counter]);
    FSqliteReturnId:=sqlite_step(vm,@FRowCount,@ColumnValues,@ColumnNames);
  end;
  sqlite_finalize(vm, nil);

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

function TSqliteDataset.TableExists(const ATableName:String): Boolean;
var
  vm:Pointer;
  ColumnNames,ColumnValues:PPChar;
  AInt:Integer;
begin
  {$ifdef DEBUG}
  WriteLn('##TSqliteDataset.TableExists##');
  {$endif}
  Result:=False;
  if not (ATableName = '') and FileExists(FFileName) then
  begin
    if FSqliteHandle = nil then
      GetSqliteHandle;
    FSqliteReturnId:=sqlite_compile(FSqliteHandle,
      Pchar('SELECT name FROM SQLITE_MASTER WHERE type = ''table'' AND name LIKE '''+ ATableName+ ''';'),
      nil,@vm,nil);
    {$ifdef DEBUG}
    WriteLn('  sqlite_compile - SqliteReturnString:',SqliteReturnString);
    {$endif}
    FSqliteReturnId:=sqlite_step(vm,@AInt,@ColumnValues,@ColumnNames);
    {$ifdef DEBUG}
    WriteLn('  sqlite_step - SqliteReturnString:',SqliteReturnString);
    {$endif}
    Result:=FSqliteReturnId = SQLITE_ROW;
    sqlite_finalize(vm, nil);
  end;
  {$ifdef DEBUG}
  WriteLn('  Table '+ATableName+' exists:',Result);
  {$endif}
end;

function TSqliteDataset.SqliteReturnString: String;
begin
 case FSqliteReturnId of
      SQLITE_OK           : Result := 'SQLITE_OK';
      SQLITE_ERROR        : Result := 'SQLITE_ERROR';
      SQLITE_INTERNAL     : Result := 'SQLITE_INTERNAL';
      SQLITE_PERM         : Result := 'SQLITE_PERM';
      SQLITE_ABORT        : Result := 'SQLITE_ABORT';
      SQLITE_BUSY         : Result := 'SQLITE_BUSY';
      SQLITE_LOCKED       : Result := 'SQLITE_LOCKED';
      SQLITE_NOMEM        : Result := 'SQLITE_NOMEM';
      SQLITE_READONLY     : Result := 'SQLITE_READONLY';
      SQLITE_INTERRUPT    : Result := 'SQLITE_INTERRUPT';
      SQLITE_IOERR        : Result := 'SQLITE_IOERR';
      SQLITE_CORRUPT      : Result := 'SQLITE_CORRUPT';
      SQLITE_NOTFOUND     : Result := 'SQLITE_NOTFOUND';
      SQLITE_FULL         : Result := 'SQLITE_FULL';
      SQLITE_CANTOPEN     : Result := 'SQLITE_CANTOPEN';
      SQLITE_PROTOCOL     : Result := 'SQLITE_PROTOCOL';
      SQLITE_EMPTY        : Result := 'SQLITE_EMPTY';
      SQLITE_SCHEMA       : Result := 'SQLITE_SCHEMA';
      SQLITE_TOOBIG       : Result := 'SQLITE_TOOBIG';
      SQLITE_CONSTRAINT   : Result := 'SQLITE_CONSTRAINT';
      SQLITE_MISMATCH     : Result := 'SQLITE_MISMATCH';
      SQLITE_MISUSE       : Result := 'SQLITE_MISUSE';
      SQLITE_NOLFS        : Result := 'SQLITE_NOLFS';
      SQLITE_AUTH         : Result := 'SQLITE_AUTH';
      SQLITE_FORMAT       : Result := 'SQLITE_FORMAT';
      SQLITE_RANGE        : Result := 'SQLITE_RANGE';
      SQLITE_ROW          : begin Result := 'SQLITE_ROW - not an error'; Exit; end;
      SQLITE_DONE         : begin Result := 'SQLITE_DONE - not an error'; Exit; end;
  else
    Result:='Unknow Return Value';
 end;
 Result:=Result+' - '+sqlite_error_string(FSqliteReturnId);
end;

function TSqliteDataset.GetSqliteEncoding: String;
begin
  Result:=StrPas(sqlite_encoding);
end;
  
function TSqliteDataset.GetSqliteVersion: String;
begin
  Result:=StrPas(sqlite_version);
end;

function TSqliteDataset.QuickQuery(const ASql:String;const AStrList: TStrings;FillObjects:Boolean):String;
var
  vm:Pointer;
  ColumnNames,ColumnValues:PPChar;
  ColCount:Integer;
  
  procedure FillStrings;
  begin
    while FSqliteReturnId = SQLITE_ROW do
    begin
      AStrList.Add(StrPas(ColumnValues[0]));
      FSqliteReturnId:=sqlite_step(vm,@ColCount,@ColumnValues,@ColumnNames);  
    end;
  end;
  procedure FillStringsAndObjects;
  begin
    while FSqliteReturnId = SQLITE_ROW do
    begin
      // I know, this code is really dirty!!
      AStrList.AddObject(StrPas(ColumnValues[0]),TObject(PtrInt(StrToInt(StrPas(ColumnValues[1])))));
      FSqliteReturnId:=sqlite_step(vm,@ColCount,@ColumnValues,@ColumnNames);  
    end;
  end;    
begin
  if FSqliteHandle = nil then
    GetSqliteHandle;
  Result:='';
  FSqliteReturnId:=sqlite_compile(FSqliteHandle,Pchar(ASql),nil,@vm,nil);
  if FSqliteReturnId <> SQLITE_OK then
    DatabaseError(SqliteReturnString,Self);
    
  FSqliteReturnId:=sqlite_step(vm,@ColCount,@ColumnValues,@ColumnNames);
  if (FSqliteReturnId = SQLITE_ROW) and (ColCount > 0) then
  begin
    Result:=StrPas(ColumnValues[0]);
    if AStrList <> nil then
    begin   
      if FillObjects and (ColCount > 1) then
        FillStringsAndObjects
      else
        FillStrings;
    end;          
  end;  
  sqlite_finalize(vm, nil); 
end;
    
end.

