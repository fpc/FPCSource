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
unit fpddsqldb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, sqldb, fpdatadict;

Type

  { TSQLDBDDEngine }

  TSQLDBDDEngine = Class(TFPDDEngine)
  Private
    FConn: TSQLConnection;
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; virtual; abstract;
    Function CreateSQLQuery(ADatasetOwner: TComponent) : TSQLQuery;
    Property Connection : TSQLConnection Read FConn;
  Public
    Procedure Disconnect ; override;
    Function HostSupported: Boolean; virtual;
    Function Connect(const AConnectString : String) : Boolean; override;
    Function GetTableList(List : TStrings) : Integer; override;
    Function ImportFields(Table : TDDTableDef) : Integer; override;
    Function ViewTable(Const TableName: String; DatasetOwner : TComponent) : TDataset; override;
    Function RunQuery(SQL : String) : Integer; override;
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

implementation


uses strutils;

Resourcestring
  SErrQueryNotSQLQuery = 'Query object "%s" is not a SQL Query';
  
{ TSQLDBDDEngine }

function TSQLDBDDEngine.HostSupported: Boolean;
begin
  Result:=True;
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

function TSQLDBDDEngine.ImportFields(Table: TDDTableDef): Integer;

Const
  SQL = 'SELECT ' +
        ' F.RDB$FIELD_POSITION as FieldPosition,' +
        ' F.RDB$FIELD_NAME as Name,' +
        ' F.RDB$NULL_FLAG as FieldNull,' +
        ' F.RDB$Description as Description,' +
        ' F.RDB$DEFAULT_SOURCE as FieldDefault,' +
        ' D.RDB$DEFAULT_SOURCE as DomainDefault,' +
        ' D.RDB$FIELD_LENGTH as CharLength,' +
        ' D.RDB$FIELD_PRECISION as FieldPrecision,' +
        ' D.RDB$FIELD_SCALE as Scale,' +
        ' D.RDB$FIELD_TYPE as FieldType,' +
        ' D.RDB$FIELD_SUB_TYPE as Subtype,' +
        ' D.RDB$NULL_FLAG as DomainNull ' +
        ' FROM '+
        ' RDB$RELATION_FIELDS F left join RDB$FIELDS D on F.RDB$FIELD_Source = D.RDB$FIELD_NAME'+
        ' WHERE (RDB$RELATION_NAME = ''%s'')' +
        ' ORDER BY RDB$FIELD_POSITION';

Var
  Q : TSQLQuery;
  FName, FPosition, FFieldnull, FDescription, FFieldDefault, FDomainDefault,
  FCharLength, FPrecision, FScale, FFieldType, FSubType, FDomainnull : TField;

  procedure BindFields;
  begin
    FName := q.fieldbyname('Name');
    FPosition := q.fieldbyname('FieldPosition');
    FFieldnull := q.fieldbyname('FieldNull');
    FDescription := q.fieldbyname('Description');
    FFieldDefault := q.fieldbyname('FieldDefault');
    FDomainDefault := q.fieldbyname('DomainDefault');
    FCharLength := q.fieldbyname('CharLength');
    FPrecision := q.fieldbyname('FieldPrecision');
    FScale := q.fieldbyname('Scale');
    FFieldType := q.fieldbyname('FieldType');
    FSubType := q.fieldbyname('SubType');
    FDomainnull := q.fieldbyname('Domainnull');
  end;

  function ConvertFBFieldType (FDfieldtype, FBsubtype : integer) : TFieldType;
  var t : integer;
      b : byte;
  begin
    t := FFieldType.asinteger;
    if t > 255 then
      begin
      if t = 261 then
        result := ftBlob       {BLOB}
      else
        result := ftUnknown;
      end
    else
      begin
      b := byte(t and $FF);
      if (b in [7,8,16]) and (FBsubtype <> 0) then
        // BCD types: 1= Numeric, 2 := Decimal
        result := ftBCD
      else
        case b of
          14 : result := ftFixedChar; {CHAR}
          37 : result := ftString;    {VARCHAR}
          40 : result := ftString;    {CSTRING ?}
          11 : result := ftFloat;     {D-FLOAT ?}
          27 : result := ftFloat;     {DOUBLE}
          10 : result := ftFloat;     {FLOAT}
          16 : result := ftLargeint;  {INT64}
          8  : result := ftInteger;   {INTEGER}
          9  : result := ftlargeint;  {QUAD ?}
          7  : result := ftSmallint;  {SMALLINT}
          12 : result := ftDate;      {DATE dialect 3}
          13 : result := ftTime;      {TIME}
          35 : result := ftDateTime;  {TIMESTAMP dialect 3, DATE in dialect 1,2}
          else result := ftUnknown;
        end;
      end;
  end;
  
  {Opmerking: bestaande fielddefs die niet meer in de tabel zitten worden niet verwijderd !? }
  
  function ImportFieldDef : boolean;
  var FD : TDDFieldDef;
      n, s : string;
  begin
    n := trim(FName.asstring);
    FD := Table.Fields.FindField(n);
    if not assigned (FD) then
      FD := Table.AddField(n);
    FD.FieldName := n;
    FD.FieldType := ConvertFBFieldType (FFieldType.asinteger, FSubType.asinteger);
    FD.Precision := FPrecision.asinteger;
    if FScale.asinteger < 0 then
      FD.Size := -FScale.asinteger
    else if FD.Fieldtype in [ftString, ftFixedChar] then
      FD.Size := FCharLength.asinteger
    else
      FD.Size := 0;
      { // Fixed length types don't have a size in the dictionary
      case byte(FFieldType.asinteger and $FF) of
        7 : FD.Size := 2;
        10,8 : FD.Size := 4;
        35,11,27,9,16,12 : FD.Size := 8;
      end; }
    if not fDescription.IsNull then
      FD.Hint := FDescription.asstring;
    s := trim(FFieldDefault.asstring);
    n := trim(FDomainDefault.asstring);
    if s <> '' then
      FD.DefaultExpression:=s
    else if n <> '' then;
      FD.DefaultExpression:=n;
    if FFieldnull.asinteger = 1 then
      FD.Required:=true
    else if FDomainnull.asinteger = 1 then
      FD.Required:=true
    else
      FD.Required:=false;
    FD.index := FPosition.AsInteger;
    result := true;
  end;
  
  function ImportFromSQLDef : integer;
  begin
    result := 0;
    Q.First;
    BindFields;
    while not Q.eof do
      begin
      if ImportFieldDef then
        inc (result);
      Q.Next;
      end;
  end;
  
begin
  Q:=CreateSQLQuery(Nil);
  try
    Q.Sql.Text:=Format(SQL,[Table.TableName]);
    Q.Open;
    try
      result := ImportFromSQLDef;
    finally
      Q.CLose;
    end;
  finally
    Q.Free;
  end;
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

Var
  Q : TSQLQuery;

begin
  Q:=CreateSQLQuery(Nil);
  Try
    Q.SQL.Text:=SQL;
    Q.ExecSQL;
    Result:=0;
  Finally
    Q.Free;
  end;
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
    Q.SQL.text:=Format('SELECT * FROM %s WHERE (1=2)',[ATAbleName]);
    Q.ReadOnly:=False;
    Q.Prepare;
    Q.IndexDefs.Update;
    IndexDefsToDDIndexDefs(Q.IndexDefs,Defs);
    Result:=Defs.Count;
  finally
    Q.Free;
  end;
end;

class function TSQLDBDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=[ecimport,ecViewTable, ecRunQuery, ecTableIndexes];
end;

end.

