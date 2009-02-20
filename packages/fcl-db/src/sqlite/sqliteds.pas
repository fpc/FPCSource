unit SqliteDS;

{
  This is TSqliteDataset, a TDataset descendant class for use with fpc compiler
  Copyright (C) 2004  Luiz Américo Pereira Câmara
  Email: pascalive@bol.com.br
    
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}
{$H+}
{.$Define DEBUG_SQLITEDS}

interface

uses
  Classes, SysUtils, CustomSqliteDS;

type
  { TSqliteDataset }

  TSqliteDataset = class(TCustomSqliteDataset)
  private
    function SqliteExec(ASQL: PChar; ACallback: TSqliteCdeclCallback; Data: Pointer): Integer; override;
    function InternalGetHandle: Pointer; override;
    function GetSqliteEncoding: String;
    function GetSqliteVersion: String; override;
    procedure InternalCloseHandle; override;
    procedure BuildLinkedList; override;
  protected
    procedure RetrieveFieldDefs; override;
    function GetRowsAffected:Integer; override;
  public
    procedure ExecuteDirect(const ASQL: String); override;
    function ReturnString: String; override;
    function QuickQuery(const ASQL: String; const AStrList: TStrings; FillObjects: Boolean): String; override;
    property SqliteEncoding: String read GetSqliteEncoding;
  end;

implementation

uses
  sqlite, db;

//function sqlite_last_statement_changes(dbhandle:Pointer):longint;cdecl;external 'sqlite' name 'sqlite_last_statement_changes';

function GetAutoIncValue(NextValue: Pointer; Columns: Integer; ColumnValues: PPChar; ColumnNames: PPChar): Integer; cdecl;
var
  CodeError, TempInt: Integer;
begin
  TempInt := 0;
  if ColumnValues[0] <> nil then
  begin
    Val(String(ColumnValues[0]), TempInt, CodeError);
    if CodeError <> 0 then
      DatabaseError('TSqliteDataset: Error trying to get last autoinc value');
  end;
  Integer(NextValue^) := Succ(TempInt);
  Result := 1;
end;

{ TSqliteDataset }

function TSqliteDataset.SqliteExec(ASQL: PChar; ACallback: TSqliteCdeclCallback; Data: Pointer): Integer;
begin
  Result := sqlite_exec(FSqliteHandle, ASQL, ACallback, Data, nil);
end;

procedure TSqliteDataset.InternalCloseHandle;
begin
  sqlite_close(FSqliteHandle);
  FSqliteHandle := nil;
end;

function TSqliteDataset.InternalGetHandle: Pointer;
var
  ErrorStr: PChar;
begin
  Result := sqlite_open(PChar(FFileName), 0, @ErrorStr);
  if Result = nil then
  begin
    DatabaseError('Error opening "' + FFileName + '": ' + String(ErrorStr));
    sqlite_freemem(ErrorStr);
  end;
end;

procedure TSqliteDataset.RetrieveFieldDefs;
var
  ColumnCount, i:Integer;
  AType: TFieldType;
  vm: Pointer;
  ColumnNames, ColumnValues:PPChar;
  ColumnStr: String;
begin
  FieldDefs.Clear;
  FAutoIncFieldNo := -1;
  FReturnCode := sqlite_compile(FSqliteHandle, PChar(FSQL), nil, @vm, nil);
  if FReturnCode <> SQLITE_OK then
    DatabaseError(ReturnString, Self);
  sqlite_step(vm, @ColumnCount, @ColumnValues, @ColumnNames);
  //Prepare the array of pchar2sql functions
  SetLength(FGetSqlStr, ColumnCount);
  // Sqlite is typeless (allows any type in any field)
  // regardless of what is in Create Table, but returns
  // exactly what is in Create Table statement
  // here is a trick to get the datatype.
  // If the field contains another type, may have problems
  for i := 0 to ColumnCount - 1 do
  begin
    ColumnStr := UpperCase(String(ColumnNames[i + ColumnCount]));
    if (ColumnStr = 'INTEGER') or (ColumnStr = 'INT') then
    begin
      if AutoIncrementKey and
           (UpperCase(String(ColumnNames[i])) = UpperCase(PrimaryKey)) then
      begin
        AType := ftAutoInc;
        FAutoIncFieldNo := i;
      end
      else
        AType := ftInteger;
    end else if Pos('VARCHAR', ColumnStr) = 1 then
    begin
      AType := ftString;
    end else if Pos('BOOL', ColumnStr) = 1 then
    begin
      AType := ftBoolean;
    end else if Pos('AUTOINC', ColumnStr) = 1 then
    begin
      AType := ftAutoInc;
      if FAutoIncFieldNo = -1 then
        FAutoIncFieldNo := i;
    end else if (Pos('FLOAT', ColumnStr)=1) or (Pos('NUMERIC', ColumnStr) = 1) then
    begin
      AType := ftFloat;
    end else if (ColumnStr = 'DATETIME') then
    begin
      AType := ftDateTime;
    end else if (ColumnStr = 'DATE') then
    begin
      AType := ftDate;
    end else if (ColumnStr = 'TIME') then
    begin
      AType := ftTime;
    end else if (ColumnStr = 'LARGEINT') then
    begin
      AType := ftLargeInt;
    end else if (ColumnStr = 'TEXT') then
    begin
      AType := ftMemo;
    end else if (ColumnStr = 'CURRENCY') then
    begin
      AType := ftCurrency;
    end else if (ColumnStr = 'WORD') then
    begin
      AType := ftWord;
    end else
    begin
      AType := ftString;
    end;    
    if AType = ftString then
      FieldDefs.Add(String(ColumnNames[i]), AType, dsMaxStringSize)
    else
      FieldDefs.Add(String(ColumnNames[i]), AType);  
    //Set the pchar2sql function
    if AType in [ftString, ftMemo] then
      FGetSqlStr[i] := @Char2SQLStr
    else
      FGetSqlStr[i] := @Num2SQLStr;
  end;
  sqlite_finalize(vm, nil);
  {
  if FReturnCode <> SQLITE_ABORT then
     DatabaseError(ReturnString,Self);
  }
end;

function TSqliteDataset.GetRowsAffected: Integer;
begin
  Result := sqlite_changes(FSqliteHandle);
  //Result := sqlite_last_statement_changes(FSqliteHandle);
end;

procedure TSqliteDataset.ExecuteDirect(const ASQL: String);
var
  vm: Pointer;
  ColumnNames, ColumnValues: PPChar;
  ColCount: Integer;
begin
  FReturnCode := sqlite_compile(FSqliteHandle, Pchar(ASQL), nil, @vm, nil);
  if FReturnCode <> SQLITE_OK then
    DatabaseError(ReturnString,Self);

  FReturnCode := sqlite_step(vm, @ColCount, @ColumnValues, @ColumnNames);

  sqlite_finalize(vm, nil);
end;

procedure TSqliteDataset.BuildLinkedList;
var
  TempItem: PDataRecord;
  vm: Pointer;
  ColumnNames, ColumnValues: PPChar;
  Counter, ColumnCount: Integer;
begin
  //Get AutoInc Field initial value
  if FAutoIncFieldNo <> -1 then
    sqlite_exec(FSqliteHandle, PChar('Select Max(' + Fields[FAutoIncFieldNo].FieldName + ') from ' + FTableName),
      @GetAutoIncValue, @FNextAutoInc, nil);

  FReturnCode := sqlite_compile(FSqliteHandle, PChar(FSQL), nil, @vm, nil);
  if FReturnCode <> SQLITE_OK then
    DatabaseError(ReturnString, Self);

  FDataAllocated := True;

  TempItem := FBeginItem;
  FRecordCount := 0;
  FReturnCode := sqlite_step(vm, @ColumnCount, @ColumnValues, @ColumnNames);
  FRowCount := ColumnCount;
  //add extra rows for calculated fields
  if FCalcFieldList <> nil then
    Inc(FRowCount, FCalcFieldList.Count);
  FRowBufferSize := (SizeOf(PPChar) * FRowCount);

  while FReturnCode = SQLITE_ROW do
  begin
    Inc(FRecordCount);
    New(TempItem^.Next);
    TempItem^.Next^.Previous := TempItem;
    TempItem := TempItem^.Next;
    GetMem(TempItem^.Row, FRowBufferSize);
    for Counter := 0 to ColumnCount - 1 do
      TempItem^.Row[Counter] := StrNew(ColumnValues[Counter]);
    //initialize calculated fields with nil
    for Counter := ColumnCount to FRowCount - 1 do
      TempItem^.Row[Counter] := nil;
    FReturnCode := sqlite_step(vm, @FRowCount, @ColumnValues, @ColumnNames);
  end;
  sqlite_finalize(vm, nil);

  // Attach EndItem
  TempItem^.Next := FEndItem;
  FEndItem^.Previous := TempItem;

  // Alloc item used in append/insert
  GetMem(FCacheItem^.Row, FRowBufferSize);
  for Counter := 0 to FRowCount - 1 do
    FCacheItem^.Row[Counter] := nil;
  // Fill FBeginItem.Row with nil -> necessary for avoid exceptions in empty datasets
  GetMem(FBeginItem^.Row, FRowBufferSize);
  for Counter := 0 to FRowCount - 1 do
    FBeginItem^.Row[Counter] := nil;
end;

function TSqliteDataset.ReturnString: String;
begin
  case FReturnCode of
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
    SQLITE_ROW          :
      begin
        Result := 'SQLITE_ROW - not an error';
        Exit;
      end;
    SQLITE_DONE         :
      begin
        Result := 'SQLITE_DONE - not an error';
        Exit;
      end;
  else
    Result := 'Unknow Return Value';
  end;
  Result := Result + ' - ' + sqlite_error_string(FReturnCode);
end;

function TSqliteDataset.GetSqliteEncoding: String;
begin
  Result := String(sqlite_encoding);
end;
  
function TSqliteDataset.GetSqliteVersion: String;
begin
  Result := String(sqlite_version);
end;

function TSqliteDataset.QuickQuery(const ASQL: String; const AStrList: TStrings; FillObjects: Boolean): String;
var
  vm: Pointer;
  ColumnNames, ColumnValues: PPChar;
  ColCount: Integer;
  
  procedure FillStrings;
  begin
    while FReturnCode = SQLITE_ROW do
    begin
      AStrList.Add(String(ColumnValues[0]));
      FReturnCode := sqlite_step(vm, @ColCount, @ColumnValues, @ColumnNames);
    end;
  end;
  procedure FillStringsAndObjects;
  begin
    while FReturnCode = SQLITE_ROW do
    begin
      // I know, this code is really dirty!!
      AStrList.AddObject(String(ColumnValues[0]),
        TObject(PtrInt(StrToInt(String(ColumnValues[1])))));
      FReturnCode := sqlite_step(vm, @ColCount, @ColumnValues, @ColumnNames);
    end;
  end;    
begin
  if FSqliteHandle = nil then
    GetSqliteHandle;
  Result := '';
  FReturnCode := sqlite_compile(FSqliteHandle, PChar(ASQL), nil, @vm, nil);
  if FReturnCode <> SQLITE_OK then
    DatabaseError(ReturnString,Self);
    
  FReturnCode := sqlite_step(vm, @ColCount, @ColumnValues, @ColumnNames);
  if (FReturnCode = SQLITE_ROW) and (ColCount > 0) then
  begin
    Result := String(ColumnValues[0]);
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

