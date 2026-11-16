{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Database schema

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Opf.Schema;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}

Type

  TOPColumnInfo = record
    ColumnName: UTF8String;
    DataType: UTF8String;
    IsNullable: Boolean;
    DefaultValue: UTF8String;
    MaxLength: Integer;
    // Exact numerics; 0 when unknown
    NumericPrecision: Integer;
    NumericScale: Integer;
  end;
  TOPColumnInfoArray = array of TOPColumnInfo;

  TOPFKInfo = record
    ConstraintName: UTF8String;
    ColumnName: UTF8String;
    ReferencedTable: UTF8String;
    ReferencedColumn: UTF8String;
    OnDelete: UTF8String;
  end;
  TOPFKInfoArray = array of TOPFKInfo;

  TOPIndexInfo = record
    IndexName: UTF8String;
    Columns: array of UTF8String;
    IsUnique: Boolean;
  end;
  TOPIndexInfoArray = array of TOPIndexInfo;

  TOPTableInfo = record
    TableName: UTF8String;
    Columns: TOPColumnInfoArray;
    PrimaryKey: array of UTF8String;
    Indexes: TOPIndexInfoArray;
    ForeignKeys: TOPFKInfoArray;
  end;
  TOPTableInfoArray = array of TOPTableInfo;

  { TOPSchemaInfo }

  TOPSchemaInfo = class(TObject)
  public
    Tables: TOPTableInfoArray;
    // Position of the named table in Tables, -1 if not found
    function FindTable(const aName: UTF8String): Integer;
    // True when a table with this name exists
    function TableExists(const aName: UTF8String): Boolean;
  end;

  { Schema Diff Types }

  TSchemaDiffKind = (sdkAddTable, sdkDropTable, sdkAddColumn, sdkDropColumn,
    sdkAlterColumnType, sdkAlterColumnNullable, sdkAddIndex, sdkDropIndex,
    sdkAddForeignKey, sdkDropForeignKey, sdkAddJunctionTable);

  TOPSchemaDiffItem = record
    Kind: TSchemaDiffKind;
    TableName: UTF8String;
    ColumnName: UTF8String;
    Detail: UTF8String;
    // sdkAlterColumnType: the new type cannot hold every existing value
    // (shorter, less precise, or another type family)
    Narrowing: Boolean;
  end;
  TOPSchemaDiffArray = array of TOPSchemaDiffItem;

  { TOPSchemaDiff }

  { Compares the registered ORM model (desired state) with an introspected
    database schema (actual state) and produces a list of differences.
    aRegistry: TOPRegistry, aRelationRegistry: TOPRelationRegistry,
    aM2MRegistry: TOPManyToManyRegistry, aDDLBuilder: TOPDDLBuilder
    are passed as TObject to avoid circular unit dependencies. }

  TOPSchemaDiff = class(TObject)
  public
    // Lists the differences between the registered model and the actual database
    class function Compare(aRegistry: TObject; aRelationRegistry: TObject;
      aM2MRegistry: TObject; aActual: TOPSchemaInfo;
      aDDLBuilder: TObject): TOPSchemaDiffArray; overload;
    // aIgnoreTables: tables that are never reported (e.g. a custom migration tracking table)
    class function Compare(aRegistry: TObject; aRelationRegistry: TObject;
      aM2MRegistry: TObject; aActual: TOPSchemaInfo;
      aDDLBuilder: TObject; const aIgnoreTables: array of UTF8String): TOPSchemaDiffArray; overload;
  end;

{ Split an SQL type into its name and up to two numeric arguments:
  'NUMERIC(12, 4)' gives 'NUMERIC', 12, 4. Missing arguments are 0. }
procedure ParseSQLType(const aType: UTF8String; out aBase: UTF8String; out aP1, aP2: Integer);
{ Canonical name of a type family: INTEGER, INT4 and SERIAL all give INT }
function CanonicalTypeName(const aBase: UTF8String): UTF8String;

{ Strip constraint suffixes (NOT NULL, UNIQUE, DEFAULT) from a
  GetPropertyDefSQL result, leaving only the base SQL type. }
function ExtractBaseType(const aFullDef: UTF8String): UTF8String;

{ Stable sort of differences into the order in which they can be applied }
procedure SortDiffs(var aDiffs: TOPSchemaDiffArray);

{ True if changing aColumn to type aExpected can lose data }
function IsNarrowingTypeChange(const aExpected: UTF8String; const aColumn: TOPColumnInfo): Boolean;

implementation

uses Opf.Registry, Opf.Fields, Opf.Relations, Opf.M2M, Opf.DDL, Opf.Sql, Opf.Driver;

{ A model name as the database stores it: the DDL builder's reserved word
  strategy applied (rwsAppendChar: Order becomes Order_), without quotes. }

function Phys(aDDL: TOPDDLBuilder; const aName: UTF8String; aType: TIdentifierType = itField): UTF8String;
begin
  if (aDDL=nil) or (aName='') then
    Result:=aName
  else
    Result:=aDDL.PhysicalIdentifier(aName, aType);
end;

{ TOPSchemaInfo }

function TOPSchemaInfo.FindTable(const aName: UTF8String): Integer;

var
  I: Integer;
  NameUp: UTF8String;

begin
  NameUp:=UpperCase(aName);
  for I:=0 to High(Tables) do
    if UpperCase(Tables[I].TableName)=NameUp then
      Exit(I);
  Result:=-1;
end;

function TOPSchemaInfo.TableExists(const aName: UTF8String): Boolean;

begin
  Result:=FindTable(aName)>=0;
end;

{ TOPSchemaDiff }

procedure AddDiff(var aDiffs: TOPSchemaDiffArray; aKind: TSchemaDiffKind;
  const aTable, aColumn, aDetail: UTF8String);
var
  L: Integer;
begin
  L:=Length(aDiffs);
  SetLength(aDiffs, L+1);
  aDiffs[L].Kind:=aKind;
  aDiffs[L].TableName:=aTable;
  aDiffs[L].ColumnName:=aColumn;
  aDiffs[L].Detail:=aDetail;
  aDiffs[L].Narrowing:=False;
end;

function ExtractBaseType(const aFullDef: UTF8String): UTF8String;
var
  P: Integer;
begin
  Result:=aFullDef;
  // GetPropertyDefSQL appends ' NOT NULL', ' UNIQUE', ' DEFAULT ...'
  // Find first occurrence of these suffixes and truncate
  P:=Pos(' NOT ', UpperCase(Result));
  if P>0 then
    Result:=Copy(Result, 1, P-1);
  P:=Pos(' UNIQUE', UpperCase(Result));
  if P>0 then
    Result:=Copy(Result, 1, P-1);
  P:=Pos(' DEFAULT ', UpperCase(Result));
  if P>0 then
    Result:=Copy(Result, 1, P-1);
  Result:=Trim(Result);
end;

procedure ParseSQLType(const aType: UTF8String; out aBase: UTF8String; out aP1, aP2: Integer);
var
  S, Args: UTF8String;
  P, Q, C: Integer;
begin
  S:=UpperCase(Trim(aType));
  aP1:=0;
  aP2:=0;
  P:=Pos('(', S);
  if P=0 then
  begin
    aBase:=S;
    Exit;
  end;
  Q:=Pos(')', S);
  if Q<P then
    Q:=Length(S)+1;
  Args:=Copy(S, P+1, Q-P-1);
  // Text after the parenthesis is part of the type: TIMESTAMP(3) WITH TIME ZONE
  aBase:=Trim(Trim(Copy(S, 1, P-1))+' '+Trim(Copy(S, Q+1, Length(S))));
  C:=Pos(',', Args);
  if C>0 then
  begin
    aP1:=StrToIntDef(Trim(Copy(Args, 1, C-1)), 0);
    aP2:=StrToIntDef(Trim(Copy(Args, C+1, Length(Args))), 0);
  end
  else
    aP1:=StrToIntDef(Trim(Args), 0);
end;

{ One name per family of equivalent types, so that the type generated for a
  field and the type a database reports for it compare equal. }

function CanonicalTypeName(const aBase: UTF8String): UTF8String;

  function IsOneOf(const aNames: array of UTF8String): Boolean;
  var
    N: UTF8String;
  begin
    for N in aNames do
      if aBase=N then
        Exit(True);
    Result:=False;
  end;

begin
  if IsOneOf(['INT', 'INTEGER', 'INT4', 'SERIAL', 'SERIAL4', 'MEDIUMINT']) then
    Result:='INT'
  else if IsOneOf(['BIGINT', 'INT8', 'BIGSERIAL', 'SERIAL8']) then
    Result:='BIGINT'
  else if IsOneOf(['SMALLINT', 'INT2', 'TINYINT']) then
    Result:='SMALLINT'
  else if IsOneOf(['BOOL', 'BOOLEAN', 'BIT']) then
    Result:='BOOL'
  else if IsOneOf(['VARCHAR', 'CHARACTER VARYING', 'VARCHAR2', 'NVARCHAR', 'NVARCHAR2', 'VARYING']) then
    Result:='VARCHAR'
  else if IsOneOf(['CHAR', 'CHARACTER', 'BPCHAR', 'NCHAR']) then
    Result:='CHAR'
  else if IsOneOf(['DECIMAL', 'NUMERIC', 'NUMBER', 'DEC']) then
    Result:='DECIMAL'
  // Approximate numerics form one family: precision is not compared
  else if IsOneOf(['FLOAT', 'REAL', 'DOUBLE', 'DOUBLE PRECISION', 'FLOAT4', 'FLOAT8',
                   'BINARY_DOUBLE', 'BINARY_FLOAT']) then
    Result:='FLOAT'
  else if IsOneOf(['TIMESTAMP', 'TIMESTAMP WITHOUT TIME ZONE', 'DATETIME', 'DATETIME2', 'SMALLDATETIME']) then
    Result:='TIMESTAMP'
  else if IsOneOf(['TIME', 'TIME WITHOUT TIME ZONE']) then
    Result:='TIME'
  else if IsOneOf(['TEXT', 'CLOB', 'MEMO', 'BLOB SUB_TYPE 1', 'BLOB SUB_TYPE TEXT',
                   'LONGTEXT', 'MEDIUMTEXT', 'TINYTEXT', 'NTEXT']) then
    Result:='TEXT'
  else if IsOneOf(['BLOB', 'BYTEA', 'BLOB SUB_TYPE 0', 'BLOB SUB_TYPE BINARY',
                   'LONGBLOB', 'MEDIUMBLOB', 'VARBINARY', 'IMAGE']) then
    Result:='BLOB'
  else
    Result:=aBase;
end;

{ Compare the expected type with an introspected column: the type family,
  and for character and exact numeric types the length or precision/scale
  when both sides report them. }

function ColumnTypeMatches(const aExpected: UTF8String; const aColumn: TOPColumnInfo): Boolean;
var
  EB, AB, EC: UTF8String;
  E1, E2, A1, A2: Integer;
begin
  ParseSQLType(aExpected, EB, E1, E2);
  ParseSQLType(aColumn.DataType, AB, A1, A2);
  EC:=CanonicalTypeName(EB);
  Result:=(EC=CanonicalTypeName(AB));
  if not Result then
    Exit;
  if (EC='VARCHAR') or (EC='CHAR') then
  begin
    if A1=0 then
      A1:=aColumn.MaxLength;
    Result:=(E1=0) or (A1<=0) or (E1=A1);
  end
  else if EC='DECIMAL' then
  begin
    if A1=0 then
    begin
      A1:=aColumn.NumericPrecision;
      A2:=aColumn.NumericScale;
    end;
    Result:=(E1=0) or (A1<=0) or ((E1=A1) and (E2=A2));
  end;
end;

function IsNarrowingTypeChange(const aExpected: UTF8String; const aColumn: TOPColumnInfo): Boolean;

  // Rank within a family that only widens: SMALLINT < INT < BIGINT,
  // CHAR < VARCHAR < TEXT. 0: not in such a family.
  function Rank(const aCanonical: UTF8String; out aFamily: Integer): Integer;
  begin
    aFamily:=0;
    Result:=0;
    if aCanonical='SMALLINT' then begin aFamily:=1; Result:=1; end
    else if aCanonical='INT' then begin aFamily:=1; Result:=2; end
    else if aCanonical='BIGINT' then begin aFamily:=1; Result:=3; end
    else if aCanonical='CHAR' then begin aFamily:=2; Result:=1; end
    else if aCanonical='VARCHAR' then begin aFamily:=2; Result:=2; end
    else if aCanonical='TEXT' then begin aFamily:=2; Result:=3; end;
  end;

var
  EB, AB, EC, AC: UTF8String;
  E1, E2, A1, A2, EF, AF, ER, AR: Integer;
begin
  ParseSQLType(aExpected, EB, E1, E2);
  ParseSQLType(aColumn.DataType, AB, A1, A2);
  EC:=CanonicalTypeName(EB);
  AC:=CanonicalTypeName(AB);
  if (EC='VARCHAR') or (EC='CHAR') or (AC='VARCHAR') or (AC='CHAR') then
    if A1=0 then
      A1:=aColumn.MaxLength;
  if (EC='DECIMAL') and (AC='DECIMAL') and (A1=0) then
  begin
    A1:=aColumn.NumericPrecision;
    A2:=aColumn.NumericScale;
  end;
  ER:=Rank(EC, EF);
  AR:=Rank(AC, AF);
  if (EF<>0) and (EF=AF) then
  begin
    if ER<AR then
      Exit(True);
    // Text: a length limit lower than the current one
    if (EF=2) and (EC<>'TEXT') and (E1>0) then
      Exit((AC='TEXT') or ((A1>0) and (E1<A1)));
    Exit(False);
  end;
  if (EC='DECIMAL') and (AC='DECIMAL') then
    // Fewer digits after or before the decimal point
    Exit((E1>0) and (A1>0) and ((E2<A2) or ((E1-E2)<(A1-A2))));
  // Same family otherwise (FLOAT, TIMESTAMP, ...): nothing is lost
  if EC=AC then
    Exit(False);
  // Another type family: values may not convert
  Result:=True;
end;

function FindColumnInTable(const aColumns: TOPColumnInfoArray; const aName: UTF8String): Integer;
var
  I: Integer;
  NameUp: UTF8String;
begin
  NameUp:=UpperCase(aName);
  for I:=0 to High(aColumns) do
    if UpperCase(aColumns[I].ColumnName)=NameUp then
      Exit(I);
  Result:=-1;
end;

function FindIndexByName(const aIndexes: TOPIndexInfoArray; const aName: UTF8String): Integer;
var
  I: Integer;
  NameUp: UTF8String;
begin
  NameUp:=UpperCase(aName);
  for I:=0 to High(aIndexes) do
    if UpperCase(aIndexes[I].IndexName)=NameUp then
      Exit(I);
  Result:=-1;
end;

function FindFKByNameOrColumns(const aFKs: TOPFKInfoArray; const aConstraintName, aColumnName, aRefTable: UTF8String): Integer;
var
  I: Integer;
begin
  // First try matching by constraint name. A constraint of that name on
  // another column is the old foreign key of a changed relation: no match.
  if aConstraintName<>'' then
    for I:=0 to High(aFKs) do
      if SameText(aFKs[I].ConstraintName, aConstraintName)
         and SameText(aFKs[I].ColumnName, aColumnName) then
        Exit(I);
  // Fallback: match by (ColumnName, ReferencedTable) tuple
  for I:=0 to High(aFKs) do
    if SameText(aFKs[I].ColumnName, aColumnName) and SameText(aFKs[I].ReferencedTable, aRefTable) then
      Exit(I);
  Result:=-1;
end;

function IsJunctionTable(const aTableName: UTF8String; aM2MReg: TOPManyToManyRegistry; aDDL: TOPDDLBuilder): Boolean;
var
  I: Integer;
begin
  for I:=0 to aM2MReg.DefCount-1 do
    if SameText(Phys(aDDL, aM2MReg.Defs[I].JunctionTable, itTable), aTableName) then
      Exit(True);
  Result:=False;
end;

function IsInternalTable(const aTableName: UTF8String): Boolean;
begin
  Result:=(Length(aTableName)>=5) and SameText(Copy(aTableName, 1, 5), '_opf_');
end;

function IsMappedTable(aReg: TOPRegistry; const aTableName: UTF8String; aDDL: TOPDDLBuilder): Boolean;
var
  I: Integer;
begin
  for I:=0 to aReg.PersisterCount-1 do
    if SameText(Phys(aDDL, aReg.Persisters[I].TableName, itTable), aTableName) then
      Exit(True);
  Result:=False;
end;

function IsIgnoredTable(const aTableName: UTF8String; const aIgnoreTables: array of UTF8String;
  aDDL: TOPDDLBuilder): Boolean;
var
  S: UTF8String;
begin
  for S in aIgnoreTables do
    if SameText(S, aTableName) or SameText(Phys(aDDL, S, itTable), aTableName) then
      Exit(True);
  Result:=False;
end;

{ Order the tables to drop so that a table comes before the tables it
  references (with foreign keys) among them. Tables in a reference cycle
  keep their order at the end. }

procedure OrderDropTables(var aNames: TUTF8StringArray; aActual: TOPSchemaInfo);
var
  Done: array of Boolean;
  Ordered: TUTF8StringArray;
  I, J, K, T, Cnt: Integer;
  Referenced, Progress: Boolean;
begin
  Cnt:=Length(aNames);
  SetLength(Done, Cnt);
  for I:=0 to Cnt-1 do
    Done[I]:=False;
  Ordered:=nil;
  repeat
    Progress:=False;
    for I:=0 to Cnt-1 do
    begin
      if Done[I] then
        Continue;
      // Still referenced by another table that is not dropped yet?
      Referenced:=False;
      for J:=0 to Cnt-1 do
      begin
        if Done[J] or (J=I) then
          Continue;
        T:=aActual.FindTable(aNames[J]);
        if T<0 then
          Continue;
        for K:=0 to High(aActual.Tables[T].ForeignKeys) do
          if SameText(aActual.Tables[T].ForeignKeys[K].ReferencedTable, aNames[I]) then
            Referenced:=True;
      end;
      if not Referenced then
      begin
        Ordered:=Concat(Ordered, [aNames[I]]);
        Done[I]:=True;
        Progress:=True;
      end;
    end;
  until not Progress;
  for I:=0 to Cnt-1 do
    if not Done[I] then
      Ordered:=Concat(Ordered, [aNames[I]]);
  aNames:=Ordered;
end;

procedure CompareTables(var aDiffs: TOPSchemaDiffArray; aReg: TOPRegistry;
  aM2MReg: TOPManyToManyRegistry; aActual: TOPSchemaInfo; const aIgnoreTables: array of UTF8String;
  aDDL: TOPDDLBuilder);
var
  I: Integer;
  TblName: UTF8String;
  Drops: TUTF8StringArray;
begin
  // Desired tables missing in the database
  for I:=0 to aReg.PersisterCount-1 do
    if not aActual.TableExists(Phys(aDDL, aReg.Persisters[I].TableName, itTable)) then
      AddDiff(aDiffs, sdkAddTable, aReg.Persisters[I].TableName, '', '');
  // Actual tables not in desired (drop candidates)
  Drops:=nil;
  for I:=0 to High(aActual.Tables) do
  begin
    TblName:=aActual.Tables[I].TableName;
    if IsInternalTable(TblName) or IsJunctionTable(TblName, aM2MReg, aDDL)
       or IsIgnoredTable(TblName, aIgnoreTables, aDDL) then
      Continue;
    if not IsMappedTable(aReg, TblName, aDDL) then
      Drops:=Concat(Drops, [TblName]);
  end;
  // Referencing tables first, or dropping a referenced table fails
  OrderDropTables(Drops, aActual);
  for I:=0 to High(Drops) do
    AddDiff(aDiffs, sdkDropTable, Drops[I], '', '');
  // Junction tables
  for I:=0 to aM2MReg.DefCount-1 do
    if not aActual.TableExists(Phys(aDDL, aM2MReg.Defs[I].JunctionTable, itTable)) then
      AddDiff(aDiffs, sdkAddJunctionTable, aM2MReg.Defs[I].JunctionTable, '', '');
end;

function IsPrimaryKeyColumn(const aTblInfo: TOPTableInfo; const aColumn: UTF8String): Boolean;
var
  S: UTF8String;
begin
  for S in aTblInfo.PrimaryKey do
    if SameText(S, aColumn) then
      Exit(True);
  Result:=False;
end;

procedure CompareColumn(var aDiffs: TOPSchemaDiffArray; aPD: TPersisterDef;
  aFM: TOPFieldMap; const aColumn: TOPColumnInfo; const aTblInfo: TOPTableInfo; aDDL: TOPDDLBuilder);
var
  ExpectedType, ActualType: UTF8String;
  DesiredNotNull, ActualNotNull: Boolean;
begin
  ExpectedType:=ExtractBaseType(aDDL.GetPropertyDefSQL(aPD.TableName, aFM));
  ActualType:=Trim(aColumn.DataType);
  // An SQLite auto-increment key is INTEGER PRIMARY KEY, the 64-bit rowid,
  // whatever the field's size
  if (aDDL.SQLDialect=dSQLite) and (([foAutoInc, foInKey]*aFM.Options)=[foAutoInc, foInKey]) then
    // nothing to compare
  else if not ColumnTypeMatches(ExpectedType, aColumn) then
  begin
    AddDiff(aDiffs, sdkAlterColumnType, aPD.TableName, aFM.FieldName,
      'expected='+ExpectedType+' actual='+ActualType);
    aDiffs[High(aDiffs)].Narrowing:=IsNarrowingTypeChange(ExpectedType, aColumn);
  end;
  // Key columns are NOT NULL, whether or not foRequired is set; some databases
  // (SQLite) report primary key columns as nullable.
  DesiredNotNull:=([foRequired, foInKey, foAutoInc]*aFM.Options)<>[];
  ActualNotNull:=(not aColumn.IsNullable) or IsPrimaryKeyColumn(aTblInfo, aColumn.ColumnName);
  if DesiredNotNull<>ActualNotNull then
  begin
    if DesiredNotNull then
      AddDiff(aDiffs, sdkAlterColumnNullable, aPD.TableName, aFM.FieldName, 'NOT NULL')
    else
      AddDiff(aDiffs, sdkAlterColumnNullable, aPD.TableName, aFM.FieldName, 'NULL');
  end;
end;

procedure CompareColumns(var aDiffs: TOPSchemaDiffArray; aPD: TPersisterDef;
  const aTblInfo: TOPTableInfo; aDDL: TOPDDLBuilder);
var
  J, K, ColIdx: Integer;
  FM: TOPFieldMap;
  Found: Boolean;
begin
  // Desired columns against actual
  for J:=0 to aPD.FieldMap.Count-1 do
  begin
    FM:=aPD.FieldMap[J];
    ColIdx:=FindColumnInTable(aTblInfo.Columns, Phys(aDDL, FM.FieldName));
    if ColIdx<0 then
      AddDiff(aDiffs, sdkAddColumn, aPD.TableName, FM.FieldName, '')
    else
      CompareColumn(aDiffs, aPD, FM, aTblInfo.Columns[ColIdx], aTblInfo, aDDL);
  end;
  // Actual columns not in desired (drop candidates)
  for J:=0 to High(aTblInfo.Columns) do
  begin
    Found:=False;
    for K:=0 to aPD.FieldMap.Count-1 do
      if SameText(Phys(aDDL, aPD.FieldMap[K].FieldName), aTblInfo.Columns[J].ColumnName) then
      begin
        Found:=True;
        Break;
      end;
    if not Found then
      AddDiff(aDiffs, sdkDropColumn, aPD.TableName, aTblInfo.Columns[J].ColumnName, '');
  end;
end;

function IsDesiredIndex(aPD: TPersisterDef; const aIndexName: UTF8String; aDDL: TOPDDLBuilder): Boolean;
var
  K: Integer;
begin
  for K:=0 to High(aPD.Indexes) do
    if SameText(Phys(aDDL, aPD.Indexes[K].IndexName), aIndexName) then
      Exit(True);
  Result:=False;
end;

{ True for a single column unique index on a foUnique field: it backs the
  UNIQUE constraint (MySQL names it after the column, SQLite migrations add
  it as uq_<table>_<column>) }

function IsUniqueFieldIndex(aPD: TPersisterDef; const aIndex: TOPIndexInfo; aDDL: TOPDDLBuilder): Boolean;
var
  K: Integer;
begin
  Result:=False;
  if not aIndex.IsUnique or (Length(aIndex.Columns)<>1) then
    Exit;
  for K:=0 to aPD.FieldMap.Count-1 do
    if (foUnique in aPD.FieldMap[K].Options)
       and SameText(Phys(aDDL, aPD.FieldMap[K].FieldName), aIndex.Columns[0]) then
      Exit(True);
end;

{ True for the index that backs the primary key: same columns as the primary
  key, or the name the primary key constraint gets (ours, PostgreSQL, Firebird) }

function IsPrimaryKeyIndex(aPD: TPersisterDef; const aTblInfo: TOPTableInfo;
  const aIndex: TOPIndexInfo; aDDL: TOPDDLBuilder): Boolean;
var
  K, L: Integer;
  Found: Boolean;
  N: UTF8String;
begin
  N:=aIndex.IndexName;
  if (aPD.PKConstraintName<>'') and SameText(N, Phys(aDDL, aPD.PKConstraintName)) then
    Exit(True);
  if Assigned(aDDL) and SameText(N, Phys(aDDL, aPD.TableName+aDDL.PKSuffix)) then
    Exit(True);
  if SameText(N, Phys(aDDL, aPD.TableName, itTable)+'_pkey') or SameText(Copy(N, 1, 11), 'RDB$PRIMARY') then
    Exit(True);
  Result:=(Length(aIndex.Columns)>0) and (Length(aIndex.Columns)=Length(aTblInfo.PrimaryKey));
  if not Result then
    Exit;
  for K:=0 to High(aIndex.Columns) do
  begin
    Found:=False;
    for L:=0 to High(aTblInfo.PrimaryKey) do
      if SameText(aIndex.Columns[K], aTblInfo.PrimaryKey[L]) then
        Found:=True;
    if not Found then
      Exit(False);
  end;
end;

procedure CompareIndexes(var aDiffs: TOPSchemaDiffArray; aPD: TPersisterDef;
  const aTblInfo: TOPTableInfo; aDDL: TOPDDLBuilder);
var
  J: Integer;
  IdxName: UTF8String;
begin
  // Desired indexes against actual
  for J:=0 to High(aPD.Indexes) do
    if FindIndexByName(aTblInfo.Indexes, Phys(aDDL, aPD.Indexes[J].IndexName))<0 then
      AddDiff(aDiffs, sdkAddIndex, aPD.TableName, '', aPD.Indexes[J].IndexName);
  // Actual indexes not in desired (drop candidates)
  for J:=0 to High(aTblInfo.Indexes) do
  begin
    IdxName:=aTblInfo.Indexes[J].IndexName;
    // Skip PK indexes and auto-generated indexes
    if IsPrimaryKeyIndex(aPD, aTblInfo, aTblInfo.Indexes[J], aDDL) then
      Continue;
    if (Pos('SQLITE_AUTOINDEX_', UpperCase(IdxName))>0) then
      Continue;
    if IsUniqueFieldIndex(aPD, aTblInfo.Indexes[J], aDDL) then
      Continue;
    if not IsDesiredIndex(aPD, IdxName, aDDL) then
      AddDiff(aDiffs, sdkDropIndex, aPD.TableName, '', IdxName);
  end;
end;

procedure CompareTableContents(var aDiffs: TOPSchemaDiffArray; aReg: TOPRegistry;
  aActual: TOPSchemaInfo; aDDL: TOPDDLBuilder);
var
  I, TblIdx, J: Integer;
  PD: TPersisterDef;
begin
  for I:=0 to aReg.PersisterCount-1 do
  begin
    PD:=aReg.Persisters[I];
    TblIdx:=aActual.FindTable(Phys(aDDL, PD.TableName, itTable));
    if TblIdx<0 then
    begin
      // Reported as sdkAddTable; the indexes of the new table are still needed
      for J:=0 to High(PD.Indexes) do
        AddDiff(aDiffs, sdkAddIndex, PD.TableName, '', PD.Indexes[J].IndexName);
      Continue;
    end;
    CompareColumns(aDiffs, PD, aActual.Tables[TblIdx], aDDL);
    CompareIndexes(aDiffs, PD, aActual.Tables[TblIdx], aDDL);
  end;
end;

{ Resolve child and parent table names of a relation.
  Returns False if either class is not registered. }

function GetRelationTables(aReg: TOPRegistry; aRelDef: TOPRelationDef;
  out aChildTable, aParentTable: UTF8String): Boolean;
var
  ChildPD, ParentPD: TPersisterDef;
begin
  ChildPD:=aReg.FindPersister(aRelDef.ChildClass);
  ParentPD:=aReg.FindPersister(aRelDef.ParentClass);
  Result:=(ChildPD<>nil) and (ParentPD<>nil);
  if not Result then
    Exit;
  aChildTable:=ChildPD.TableName;
  aParentTable:=ParentPD.TableName;
end;

{ FindFKByNameOrColumns for the foreign key of a relation, with the names
  as the database stores them }

function FindRelationFK(const aFKs: TOPFKInfoArray; aRelDef: TOPRelationDef;
  const aParentTable: UTF8String; aDDL: TOPDDLBuilder): Integer;
begin
  Result:=FindFKByNameOrColumns(aFKs, Phys(aDDL, aRelDef.ConstraintName),
    Phys(aDDL, aRelDef.ForeignKeyFieldName), Phys(aDDL, aParentTable, itTable));
end;

procedure CompareAddedForeignKeys(var aDiffs: TOPSchemaDiffArray; aReg: TOPRegistry;
  aRelReg: TOPRelationRegistry; aActual: TOPSchemaInfo; aDDL: TOPDDLBuilder);
var
  I, TblIdx: Integer;
  RelDef: TOPRelationDef;
  ChildTblName, ParentTblName: UTF8String;
begin
  for I:=0 to aRelReg.RelationCount-1 do
  begin
    RelDef:=aRelReg.Relations[I];
    if not GetRelationTables(aReg, RelDef, ChildTblName, ParentTblName) then
      Continue;
    TblIdx:=aActual.FindTable(Phys(aDDL, ChildTblName, itTable));
    // A new table gets its foreign keys after all tables are created
    if (TblIdx<0) or (FindRelationFK(aActual.Tables[TblIdx].ForeignKeys,
      RelDef, ParentTblName, aDDL)<0) then
      AddDiff(aDiffs, sdkAddForeignKey, ChildTblName, RelDef.ForeignKeyFieldName,
        'parent='+ParentTblName);
  end;
end;

procedure CompareDroppedForeignKeys(var aDiffs: TOPSchemaDiffArray; aPD: TPersisterDef;
  aReg: TOPRegistry; aRelReg: TOPRelationRegistry; const aTblInfo: TOPTableInfo; aDDL: TOPDDLBuilder);
var
  J, K: Integer;
  RelDef: TOPRelationDef;
  ChildTblName, ParentTblName: UTF8String;
  FKMatched: array of Boolean;
begin
  SetLength(FKMatched, Length(aTblInfo.ForeignKeys));
  for J:=0 to High(FKMatched) do
    FKMatched[J]:=False;
  // Mark FKs that match a relation def
  for J:=0 to aRelReg.RelationCount-1 do
  begin
    RelDef:=aRelReg.Relations[J];
    if not GetRelationTables(aReg, RelDef, ChildTblName, ParentTblName) then
      Continue;
    if not SameText(ChildTblName, aPD.TableName) then
      Continue;
    K:=FindRelationFK(aTblInfo.ForeignKeys, RelDef, ParentTblName, aDDL);
    if K>=0 then
      FKMatched[K]:=True;
  end;
  // Report unmatched FKs
  for J:=0 to High(aTblInfo.ForeignKeys) do
    if not FKMatched[J] then
      AddDiff(aDiffs, sdkDropForeignKey, aPD.TableName,
        aTblInfo.ForeignKeys[J].ColumnName, aTblInfo.ForeignKeys[J].ConstraintName);
end;

procedure CompareForeignKeys(var aDiffs: TOPSchemaDiffArray; aReg: TOPRegistry;
  aRelReg: TOPRelationRegistry; aM2MReg: TOPManyToManyRegistry; aActual: TOPSchemaInfo;
  aDDL: TOPDDLBuilder);
var
  I, TblIdx: Integer;
  PD: TPersisterDef;
begin
  // Desired FKs (from relations) against actual
  CompareAddedForeignKeys(aDiffs, aReg, aRelReg, aActual, aDDL);
  // Actual FKs not in desired (drop candidates)
  for I:=0 to aReg.PersisterCount-1 do
  begin
    PD:=aReg.Persisters[I];
    TblIdx:=aActual.FindTable(Phys(aDDL, PD.TableName, itTable));
    if TblIdx<0 then
      Continue;
    // Junction table FKs are managed by M2M, not relation defs
    if IsJunctionTable(aActual.Tables[TblIdx].TableName, aM2MReg, aDDL) then
      Continue;
    CompareDroppedForeignKeys(aDiffs, PD, aReg, aRelReg, aActual.Tables[TblIdx], aDDL);
  end;
end;

class function TOPSchemaDiff.Compare(aRegistry: TObject; aRelationRegistry: TObject;
  aM2MRegistry: TObject; aActual: TOPSchemaInfo;
  aDDLBuilder: TObject): TOPSchemaDiffArray;
begin
  Result:=Compare(aRegistry, aRelationRegistry, aM2MRegistry, aActual, aDDLBuilder, []);
end;

class function TOPSchemaDiff.Compare(aRegistry: TObject; aRelationRegistry: TObject;
  aM2MRegistry: TObject; aActual: TOPSchemaInfo;
  aDDLBuilder: TObject; const aIgnoreTables: array of UTF8String): TOPSchemaDiffArray;
var
  Reg: TOPRegistry;
  RelReg: TOPRelationRegistry;
  M2MReg: TOPManyToManyRegistry;
begin
  SetLength(Result, 0);
  Reg:=TOPRegistry(aRegistry);
  RelReg:=TOPRelationRegistry(aRelationRegistry);
  M2MReg:=TOPManyToManyRegistry(aM2MRegistry);
  CompareTables(Result, Reg, M2MReg, aActual, aIgnoreTables, TOPDDLBuilder(aDDLBuilder));
  CompareTableContents(Result, Reg, aActual, TOPDDLBuilder(aDDLBuilder));
  CompareForeignKeys(Result, Reg, RelReg, M2MReg, aActual, TOPDDLBuilder(aDDLBuilder));
  SortDiffs(Result);
end;

{ Order in which differences can be applied: new tables before their indexes
  and foreign keys (a foreign key can reference another new table), foreign
  keys and indexes dropped before the columns and tables they use. Foreign
  keys are dropped before any is added: a relation that moved to another
  column keeps its constraint name, so the old one must go first. }

const
  DiffOrder: array[TSchemaDiffKind] of Integer = (
    0,   // sdkAddTable
    10,  // sdkDropTable
    3,   // sdkAddColumn
    9,   // sdkDropColumn
    4,   // sdkAlterColumnType
    5,   // sdkAlterColumnNullable
    6,   // sdkAddIndex
    8,   // sdkDropIndex
    7,   // sdkAddForeignKey
    2,   // sdkDropForeignKey
    1    // sdkAddJunctionTable
  );

procedure SortDiffs(var aDiffs: TOPSchemaDiffArray);
var
  I, J: Integer;
  Item: TOPSchemaDiffItem;
begin
  // Insertion sort: stable, so the comparison order is kept within a kind
  for I:=1 to High(aDiffs) do
  begin
    Item:=aDiffs[I];
    J:=I-1;
    while (J>=0) and (DiffOrder[aDiffs[J].Kind]>DiffOrder[Item.Kind]) do
    begin
      aDiffs[J+1]:=aDiffs[J];
      Dec(J);
    end;
    aDiffs[J+1]:=Item;
  end;
end;

end.
