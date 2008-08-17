{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    Firebird/Interbase Data Dictionary Engine Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpddfb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, fpdatadict, fpddsqldb;
  
Type

  { TSQLDBFBDDEngine }

  { TFPDDFBSQLEngine }

  TFPDDFBSQLEngine = Class(TFPDDSQLEngine)
  Public
    Function  CreateSequenceSQL(Sequence : TDDSequenceDef) : String; override;
  end;

  TSQLDBFBDDEngine = Class(TSQLDBDDEngine)
  private
  Protected
    Function CreateConnection(AConnectString  : String) : TSQLConnection; override;
  Public
    Class function EngineCapabilities : TFPDDEngineCapabilities; virtual;
    function ImportFields(Table: TDDTableDef): Integer; override;
    Function CreateSQLEngine : TFPDDSQLEngine; override;
    Class function Description : string; override;
    Class function DBType : String; override;
  end;

  // Backwards compatibility
  TSQLDBIBDDEngine = TSQLDBFBDDEngine;
  

Procedure RegisterFBDDEngine;
Procedure UnRegisterFBDDEngine;

implementation

uses ibconnection, db;

Procedure RegisterFBDDEngine;

begin
  RegisterDictionaryEngine(TSQLDBFBDDEngine);
end;

Procedure UnRegisterFBDDEngine;

begin
  UnRegisterDictionaryEngine(TSQLDBFBDDEngine);
end;

{ TSQLDBFBDDEngine }

function TSQLDBFBDDEngine.CreateConnection(AConnectString: String
  ): TSQLConnection;
begin
  Result:=TIBConnection.Create(Self);
end;

class function TSQLDBFBDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=[ecImport,ecCreateTable,ecViewTable, ecTableIndexes,
           ecRunQuery, ecRowsAffected, ecSequences, ecDomains];
end;

class function TSQLDBFBDDEngine.Description: string;
begin
  Result:='Firebird/Interbase connection using SQLDB';
end;

class function TSQLDBFBDDEngine.DBType: String;
begin
  Result:='Firebird/Interbase';
end;

function TSQLDBFBDDEngine.ImportFields(Table: TDDTableDef): Integer;
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

function TSQLDBFBDDEngine.CreateSQLEngine: TFPDDSQLEngine;
begin
  Result:=TFPDDFBSQLEngine.Create;
end;

{ TFPDDFBSQLEngine }

function TFPDDFBSQLEngine.CreateSequenceSQL(Sequence: TDDSequenceDef): String;
begin
  Result:='CREATE GENERATOR '+Sequence.SequenceName;
end;

end.

