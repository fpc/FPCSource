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

  { TFPDDMSSQLEngine }

  TFPDDMSSQLEngine = Class(TFPDDSQLEngine)
  Public
    Function  CreateDomainSQL(Domain : TDDDomainDef) : String; override;
  end;

  { TSQLDBMSSQLDDEngine }

  TSQLDBMSSQLDDEngine = Class(TSQLDBDDEngine)
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Function ImportIndexes(Table : TDDTableDef) : Integer; override;
    Function ImportSequences(Sequences : TDDSequenceDefs; List : TStrings; UpdateExisting : boolean) : Integer; override;
    Function ImportDomains(Domains : TDDDomainDefs; List : TStrings; UpdateExisting : boolean) : Integer; override;
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
  Result:=[ecImport, ecCreateTable, ecViewTable, ecTableIndexes,
           ecRunQuery, ecRowsAffected, ecSequences, ecDomains];
end;

class function TSQLDBMSSQLDDEngine.Description: string;
begin
  Result:='Microsoft SQL Server connection using SQLDB';
end;

class function TSQLDBMSSQLDDEngine.DBType: String;
begin
  Result:='Microsoft SQL Server';
end;


function TSQLDBMSSQLDDEngine.ImportIndexes(Table: TDDTableDef): Integer;

const
  SQL_Indexes = 'SELECT '+
     '  t.name AS TableName, '+
     '  ind.name AS IndexName, '+
     '  ind.index_id AS IndexId, '+
     '  ic.index_column_id AS ColumnId, '+
     '  col.name AS ColumnName, '+
     '  ind.is_unique AS IsUniqueIndex, '+
     '  ind.is_unique_constraint AS IsConstraint '+
     'FROM '+
     '  sys.indexes ind ' +
     '  INNER JOIN sys.index_columns ic ON  ind.object_id = ic.object_id and ind.index_id = ic.index_id '+
     '  INNER JOIN sys.columns col ON ic.object_id = col.object_id and ic.column_id = col.column_id '+
     '  INNER JOIN sys.tables t ON ind.object_id = t.object_id '+
     'WHERE '+
     '  t.name=:TableName '+
     'ORDER BY '+
     '  t.name, ind.name, ind.index_id, ic.index_column_id ';
      
        
Var
  Q : TSQLQuery;
  FIndexName, FFieldName, FUnique, FConstraint : TField;

  procedure BindIndexFields;
  begin
    FIndexName := Q.FieldByName ('IndexName');
    FFieldName := Q.FieldbyName('ColumnName');
    FUnique     := Q.FieldByName('IsUniqueIndex');
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
  Q.SQL.text := SQL_Indexes;
  Q.Params[0].AsString:=Table.TableName;
  Q.Open;
  try
    BindIndexFields;
    while not Q.Eof do
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
      FN:=FN+Trim(FFieldName.AsString);
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
  SQL_Sequences = 'SELECT SEQUENCE_NAME, START_VALUE, INCREMENT FROM INFORMATION_SCHEMA.SEQUENCES';

Var
  Q : TSQLQuery;
  Seq : TDDSequenceDef;
  n : string;

begin
  result := 0;
  Q:=CreateSQLQuery(Nil);
  try
    Q.Sql.Text := SQL_Sequences;
    Q.Open;
    try
      while not Q.eof do
        begin
        n := trim(Q.Fields[0].AsString);
        seq := Sequences.FindSequence(n);
        if not assigned (Seq) then
          Seq := Sequences.AddSequence(n)
        else if not UpdateExisting then
          Seq := nil;
        if assigned (Seq) then
          begin
          Seq.StartValue := Round(Q.Fields[1].AsFloat);
          Seq.Increment := Round(Q.Fields[2].AsFloat);
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

function TSQLDBMSSQLDDEngine.ImportDomains(Domains: TDDDomainDefs;
  List: TStrings; UpdateExisting: boolean): Integer;

const
  SQL_Domains = 'SELECT * FROM INFORMATION_SCHEMA.DOMAINS';

Var
  Q : TSQLQuery;
  FName, FDomainName, FDomainDefault,
  FCharLength, FPrecision, FScale, FDataType : TField;

  procedure BindFields;
  begin
    FName := Q.fieldbyname('DOMAIN_NAME');
    FDomainDefault := q.fieldbyname('DOMAIN_DEFAULT');
    FCharLength := q.fieldbyname('CHARACTER_MAXIMUM_LENGTH');
    FPrecision := q.fieldbyname('NUMERIC_PRECISION');
    FScale := q.fieldbyname('NUMERIC_SCALE');
    FDataType := q.fieldbyname('DATA_TYPE');
  end;

  function ImportDomain : boolean;
  var Dom : TDDDomainDef;
      n : string;
  begin
    n := trim(FName.AsString);
    Dom := Domains.FindDomain(n);
    if not assigned (Dom) then
      Dom := Domains.AddDomain(n)
    else if not UpdateExisting then
      Dom := nil;
    if assigned (Dom) then
      begin
      result := true;
      Dom.FieldType := SQLDataTypeToFieldType(FDataType.AsString);
      Dom.Precision := FPrecision.AsInteger;
      if Dom.FieldType in [ftFloat, ftBcd, ftFmtBCD] then
        Dom.Size := FScale.AsInteger
      else if Dom.FieldType in [ftString, ftFixedChar] then
        Dom.Size := FCharLength.AsInteger
      else
        Dom.Size := 0;
      end
    else
      result := false;
  end;

begin
  result := 0;
  Q:=CreateSQLQuery(Nil);
  try
    Q.Sql.Text := SQL_Domains;
    Q.Open;
    BindFields;
    try
      while not Q.eof do
        begin
        if ImportDomain then
          inc (result);
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

function TFPDDMSSQLEngine.CreateDomainSQL(Domain: TDDDomainDef): String;
begin
  Result:='CREATE TYPE '+Domain.DomainName+' FROM '+FieldTypeString(Domain.FieldType,Domain.Size,Domain.Precision);
  if Domain.Required then
    Result:=Result+' NOT NULL';
end;

end.

