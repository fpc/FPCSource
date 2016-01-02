{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    MS-SQL Server Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpddmssql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb, db;
  
Type

  { TFPDDFBSQLEngine }

  TFPDDMSSQLEngine = Class(TFPDDSQLEngine)
  Public
    Function  CreateSequenceSQL(Sequence : TDDSequenceDef) : String; override;
  end;

  TSQLDBMSSQLDDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Function ImportIndexes(Table : TDDTableDef) : Integer; override;
    Function ImportSequences(Sequences : TDDSequenceDefs; List : TStrings; UpdateExisting : boolean) : Integer; override;
    Function CreateSQLEngine : TFPDDSQLEngine; override;
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
    Class function Description : string; override;
    Class function DBType : String; override;
  end;


Procedure RegisterMSSQLDDEngine;
Procedure UnRegisterMSSQLDDEngine;

implementation

uses mssqlconn;

Procedure RegisterMSSQLDDEngine;

begin
  RegisterDictionaryEngine(TSQLDBMSSQLDDEngine);
end;

Procedure UnRegisterMSSQLDDEngine;

begin
  UnRegisterDictionaryEngine(TSQLDBMSSQLDDEngine);
end;

{ TSQLDBMSSQLDDEngine }

function TSQLDBMSSQLDDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TMSSQLConnection.Create(Self);
end;

class function TSQLDBMSSQLDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=[ecImport,ecCreateTable,ecViewTable, ecTableIndexes,
           ecRunQuery, ecRowsAffected, ecSequences];
end;

class function TSQLDBMSSQLDDEngine.Description: string;
begin
  Result:='Microsoft SQL Server connection using SQLDB';
end;

class function TSQLDBMSSQLDDEngine.DBType: String;
begin
  Result:='MS-SQL Server';
end;


function TSQLDBMSSQLDDEngine.ImportIndexes(Table: TDDTableDef): Integer;

const
  SQLindexes = 'SELECT '+
     '  TableName = t.name, '+
     '  IndexName = ind.name, '+
     '  IndexId = ind.index_id, '+
     '  ColumnId = ic.index_column_id, '+
     '  ColumnName = col.name, '+
     '  IsUniqueIndex = ind.is_unique, '+
     '  IsConstraint = ind.is_unique_constraint '+
     '  ind.*, '+
     '  ic.*, '+
     '  col.* '+
     '  FROM '+
     '    sys.indexes ind ' +
     '    INNER JOIN sys.index_columns ic ON  ind.object_id = ic.object_id and ind.index_id = ic.index_id '+
     '    INNER JOIN sys.columns col ON ic.object_id = col.object_id and ic.column_id = col.column_id '+
     '    INNER JOIN sys.tables t ON ind.object_id = t.object_id '+
     '  WHERE '+
     '    AND (t.name=:TableName) '+
     '  ORDER BY '+
     '    t.name, ind.name, ind.index_id, ic.index_column_id ';
      
        
Var
  Q : TSQLQuery;
  FIndexName, FFieldName, FUnique, FConstraint : TField;

  procedure BindIndexFields;
  begin
    FIndexName := Q.FieldByName ('IndexName');
    FFieldName := Q.FieldbyName('ColumnName');
    FUnique :=Q.FieldByName('IsUniqueIndex');
    FConstraint := Q.FieldByName('IsConstraint');
  end;

  function CreateIndex (AName, indexname: string) : TDDIndexDef;
  var n, s : string;
  begin
    n := trim(AName);
    if n = '' then
      n := trim(indexname);
    if trim (indexName) = '' then
      indexname := AName;
    result := Table.Indexes.AddIndex(n);
    if FUnique.AsInteger<>0 then
      result.Options:=[ixUnique];
  end;
  

Var
  FN,IndName : String;
  IDD : TDDIndexDef;

begin
  FN:='';
  IndName:='';
  IDD:=Nil;
  Q:=CreateSQLQuery(Nil);
  Q.SQL.text := SQLindexes;
  Q.Params[0].AsString:=Table.TableName;
  Q.Open;
  try
    BindIndexFields;
    while not Q.eof do
      begin
      if IndName<>FIndexName.AsString then
        begin
        if (IDD<>Nil) then
          IDD.Fields:=FN;
        IndName:=FIndexName.AsString;
        IDD:=CreateIndex('',IndName);
        FN:='';
        end;
      if FN<>'' then
        FN:=FN+';';
      FN:=FN+Trim(FFieldName.asstring);
      Q.Next;
      end;
    if (IDD<>Nil) then
      IDD.Fields:=FN;
  finally
    Q.Free;
  end;
end;

function TSQLDBMSSQLDDEngine.ImportSequences(Sequences: TDDSequenceDefs;
  List: TStrings; UpdateExisting: boolean): Integer;

const
  SQL = 'SELECT '+
        '  seq.name AS TheSequenceName, seq.start_value AS TheStartValue, seq.increment as TheIncrement '+
        'FROM '+
        '  sys.sequences AS seq ';

Var
  Q : TSQLQuery;
  Seq : TDDSequenceDef;
  n : string;

begin
  result := 0;
  Q:=CreateSQLQuery(Nil);
  try
    Q.Sql.Text := SQL;
    Q.Open;
    try
      while not Q.eof do
        begin
        n := trim(Q.Fields[0].asstring);
        seq := Sequences.FindSequence(n);
        if not assigned (Seq) then
          Seq := Sequences.AddSequence(n)
        else if not UpdateExisting then
          Seq := nil;
        if assigned (Seq) then
          begin
          Seq.StartValue := Round(Q.FIelds[1].AsFloat);
          Seq.Increment := Round(Q.FIelds[2].AsFloat);
          inc (result);
          end;
        Q.Next;
        end;
    finally
      Q.CLose;
    end;
  finally
    Q.Free;
  end;
end;


function TSQLDBMSSQLDDEngine.CreateSQLEngine: TFPDDSQLEngine;
begin
  Result:=TFPDDMSSQLEngine.Create;
end;

{ TFPDDMSSQLEngine }

function TFPDDMSSQLEngine.CreateSequenceSQL(Sequence: TDDSequenceDef): String;
begin
  Result:='CREATE SEQUENCE '+Sequence.SequenceName;
  if Sequence.StartValue<>0 then
    Result:=Result+ ' STAR WITH ' +IntToStr(Sequence.StartValue);
  if Sequence.Increment<>0 then
    Result:=Result+ ' INCREMENT BY ' +IntToStr(Sequence.Increment);
end;

end.

