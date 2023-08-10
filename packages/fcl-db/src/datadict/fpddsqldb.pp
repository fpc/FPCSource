{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    SQLDB Data Dictionary Engine common Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPDDSQLDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, sqltypes, sqldb, fpdatadict;

Type

  { TSQLDBDDEngine }

  TSQLDBDDEngine = Class(TFPDDEngine)
  Private
    FConn: TSQLConnection;
  Protected
    Function SQLDataTypeToFieldType(const SQLDataType: string) : TFieldType; virtual;
    Function CreateConnection(AConnectString  : String) : TSQLConnection; virtual; abstract;
    Function CreateSQLQuery(ADatasetOwner: TComponent) : TSQLQuery;
    Property Connection : TSQLConnection Read FConn;
  Public
    Procedure Disconnect ; override;
    Function HostSupported: Boolean; virtual;
    Function Connect(const AConnectString : String) : Boolean; override;
    Function GetTableList(List : TStrings) : Integer; override;
    Function GetObjectList(ASchemaType: TSchemaType; AList : TSqlObjectIdentifierList): Integer; override;
    Function ImportFields(Table : TDDTableDef) : Integer; override;
    Function ImportIndexes(Table : TDDTableDef) : Integer; override;
    Function ViewTable(Const TableName: String; DatasetOwner : TComponent) : TDataset; override;
    Function RunQuery(SQL : String) : Integer; override; overload;
    Function RunQuery(SQL : String; Params : TParams) : Integer; override; overload;
    Procedure ApplyParams(DS : TDataset; Params : TParams); virtual;
    Function CreateQuery(SQL : String; DatasetOwner : TComponent) : TDataset; override;
    Procedure SetQueryStatement(SQL : String; AQuery : TDataset); override;
    Function GetTableIndexDefs(ATableName : String; Defs : TDDIndexDefs) : integer ; override;
    Class function EngineCapabilities : TFPDDEngineCapabilities; override;
  end;



Const
  // used in connectionstring
  KeyHostName     = 'Host';
  KeyDatabaseName = 'Database';
  KeyUserName     = 'User';
  KeyPassword     = 'Password';
  KeyEncode       = 'Trivial';
  KeyCharset      = 'Charset';

implementation


uses strutils;

Resourcestring
  SErrQueryNotSQLQuery = 'Query object "%s" is not a SQL Query';
  
{ TSQLDBDDEngine }

function TSQLDBDDEngine.HostSupported: Boolean;
begin
  Result:=True;
end;

function TSQLDBDDEngine.SQLDataTypeToFieldType(const SQLDataType: string
  ): TFieldType;
begin
  // ANSI standard types
  case SQLDataType of
    'char'    : Result := ftFixedChar;
    'varchar' : Result := ftString;
    'smallint': Result := ftSmallint;
    'int',
    'integer' : Result := ftInteger;
    'bigint'  : Result := ftLargeInt;
    'float'   : Result := ftFloat;
    'date'    : Result := ftDate;
    'time'    : Result := ftTime;
    'datetime': Result := ftDateTime;
    else        Result := ftUnknown;
  end;
end;

function TSQLDBDDEngine.CreateSQLQuery(ADatasetOwner: TComponent): TSQLQuery;
begin
  Result:=TSQLQuery.Create(ADatasetOwner);
  Result.DataBase:=FConn;
  Result.Transaction:=FConn.TRansaction;
end;

procedure TSQLDBDDEngine.Disconnect;
begin
  FreeAndNil(FConn);
  FConnectString:='';
  FConnected:=False;
end;

function TSQLDBDDEngine.Connect(const AConnectString: String): Boolean;

Var
  L : TStringList;
  
begin
  FConn:=CreateConnection(AConnectString);
  FConn.Transaction:=TSQLTransaction.Create(FConn);
  L:=TStringList.Create;
  Try
    L.CommaText:=AConnectString;
    If HostSupported then
      FConn.HostName:=L.Values[KeyHostName];
    FConn.DatabaseName:=L.Values[KeyDatabaseName];
    FConn.UserName:=L.Values[KeyUserName];
    FConn.Password:=XorDecode(KeyEncode,L.Values[KeyPassword]);
    FConn.LoginPrompt:=False;
    FConn.Connected:=True;
    FConn.CharSet:=L.Values[KeyCharset];
    FConnected:=True;
    FConnectString:=AConnectString;
    Result:=True;
  Finally
    L.Free;
  end;
end;

function TSQLDBDDEngine.GetTableList(List: TStrings): Integer;
begin
  FConn.GetTableNames(List,False);
  result := list.count;
end;

Function TSQLDBDDEngine.GetObjectList(ASchemaType: TSchemaType; AList : TSqlObjectIdentifierList): Integer;
begin
  Result := FConn.GetObjectNames(ASchemaType, AList); 
end;


function TSQLDBDDEngine.ImportFields(Table: TDDTableDef): Integer;

Const
  SQL = 'SELECT * FROM %s WHERE (1=0)';

Var
  Q : TSQLQuery;

begin
  Q:=CreateSQLQuery(Nil);
  try
    Q.Sql.Text:=Format(SQL,[Table.TableName]);
    Q.Open;
    try
      Result:=Table.ImportFromDataset(Q);
    finally
      Q.CLose;
    end;
  finally
    Q.Free;
  end;
end;


function TSQLDBDDEngine.ImportIndexes(Table: TDDTableDef): Integer;
begin
end;


function TSQLDBDDEngine.ViewTable(const TableName: String;
  DatasetOwner: TComponent): TDataset;
  
Var
  Q : TSQLQuery;
  
begin
  Q:=CreateSQLQuery(DatasetOwner);
  Q.SQL.Text:='SELECT * FROM '+TableName;
  Result:=Q;
end;

function TSQLDBDDEngine.RunQuery(SQL: String): Integer;

begin
  Result:=RunQuery(SQL,Nil)
end;

function TSQLDBDDEngine.RunQuery(SQL: String; Params: TParams): Integer;

Var
  Q : TSQLQuery;

begin
  Q:=CreateSQLQuery(Nil);
  Try
    Q.SQL.Text:=SQL;
    ApplyParams(Q,Params);
    Q.ExecSQL;
    Result:=0;
  Finally
    Q.Free;
  end;

end;

procedure TSQLDBDDEngine.ApplyParams(DS: TDataset; Params: TParams);
begin
  if DS is TSQLQuery then
    TSQLQuery(DS).Params.Assign(Params);
end;

function TSQLDBDDEngine.CreateQuery(SQL: String; DatasetOwner: TComponent
  ): TDataset;

Var
  Q : TSQLQuery;

begin
  Q:=CreateSQLQuery(Nil);
  Result:=Q;
  Q.SQL.Text:=SQL;
  Q.Open;
end;

procedure TSQLDBDDEngine.SetQueryStatement(SQL: String; AQuery: TDataset);
begin
  If Not (AQuery is TSQLQuery) then
    Raise EDataDict.CreateFmt(SErrQueryNotSQLQuery,[AQuery.ClassName]);
  (AQuery as TSQLQuery).SQL.Text:=SQL;
end;

function TSQLDBDDEngine.GetTableIndexDefs(ATableName: String; Defs: TDDIndexDefs
  ): integer;
  
Var
  Q : TSQLQuery;
  
begin
  Q:=TSQLQuery.Create(Self);
  Try
    Q.Database:=FConn;
    Q.Transaction:=FConn.Transaction;
    Q.SQL.text:=Format('SELECT * FROM %s WHERE (1=2)',[ATableName]);
    Q.ReadOnly:=False;
    Q.Prepare;
    Q.ServerIndexDefs.Update;
    IndexDefsToDDIndexDefs(Q.ServerIndexDefs,Defs);
    Result:=Defs.Count;
  finally
    Q.Free;
  end;
end;

class function TSQLDBDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=[ecImport, ecViewTable, ecRunQuery, ecTableIndexes, ecRowsAffected];
end;

end.

