{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Data Dictionary diff mechanism, compare 2 data dictionaries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpdddiff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpdatadict;

type

  TDiffKind = (DiffTables, DiffFields, DiffIndexes);
  TDiffKindSet = set of TDiffKind;

  TDifferenceType = (dtMissing, dtDifferent, dtSurplus);
  
const
  diffAll = [DiffTables, DiffFields, DiffIndexes];
  
type
  
  { TCustomDDDiffer }

  TCustomDDDiffer = class
  private
    FSourceDD: TFPdatadictionary;
    FTargetDD: TFPdatadictionary;
  protected
    procedure TableDifference (DiffType: TDifferenceType; SourceTable, TargetTable: TDDTableDef); virtual;
    procedure IndexDifference (DiffType: TDifferenceType; SourceIndex, TargetIndex: TDDIndexDef); virtual;
    procedure FieldDifference (DiffType: TDifferenceType; SourceField, TargetField: TDDFieldDef); virtual;
    procedure CompareTables (Kind: TDiffKindSet);
    procedure CompareTable (TableName: string; Kind: TDiffKindSet);
    procedure CompareFields (Source, Target: TDDFieldDefs; Kind: TDiffKindSet);
    procedure CompareField (Source, Target: TDDFieldDefs; Fieldname: string; Kind: TDiffKindSet);
    procedure CompareIndexes (Source, Target: TDDIndexDefs; Kind: TDiffKindSet);
    procedure CompareIndex (Source, Target: TDDIndexDefs; Indexname: string; Kind: TDiffKindSet);
  public
    procedure Compare (Kind: TDiffKindSet);
    property SourceDD : TFPdatadictionary read FSourceDD write FSourceDD;
    property TargetDD : TFPdatadictionary read FTargetDD write FTargetDD;
  end;
  
  EDataDictDiff = Class(EDataDict);
  
implementation

uses db;

resourcestring
  SErrMissingDatadict = 'Source and/or target datadictionary not assigned.';

{ TCustomDDDiffer }

procedure TCustomDDDiffer.TableDifference(DiffType: TDifferenceType;
  SourceTable, TargetTable: TDDTableDef);
begin
end;

procedure TCustomDDDiffer.IndexDifference(DiffType: TDifferenceType;
  SourceIndex, TargetIndex: TDDIndexDef);
begin
end;

procedure TCustomDDDiffer.FieldDifference(DiffType: TDifferenceType;
  SourceField, TargetField: TDDFieldDef);
begin
end;

procedure TCustomDDDiffer.CompareTables(Kind: TDiffKindSet);
var
  Tablenames : TStringlist;
  r : integer;
begin
  Tablenames := TStringlist.Create;
  try
    TableNames.Duplicates:=dupIgnore;
    TableNames.sorted := true;
    for r := 0 to SourceDD.Tables.Count-1 do
      TableNames.Add (SourceDD.Tables[r].TableName);
    for r := 0 to TargetDD.Tables.Count-1 do
      TableNames.Add (TargetDD.Tables[r].TableName);
    for r := 0 to TableNames.count-1 do
      CompareTable (TableNames[r], Kind);
  finally
    Tablenames.Free;
  end;
end;

procedure TCustomDDDiffer.CompareTable(TableName: string; Kind: TDiffKindSet);
var
  SourceTable, TargetTable : TDDTableDef;
begin
  SourceTable := FSourceDD.Tables.FindTable(TableName);
  TargetTable := FTargetDD.Tables.FindTable(TableName);
  if Not assigned (TargetTable) then
    begin
    if DiffTables in Kind then
      TableDifference (dtMissing, SourceTable, nil);
    end
  else if not assigned (SourceTable) then
    begin
    if DiffTables in Kind then
      TableDifference (dtSurplus, nil, TargetTable);
    end
  else
    begin  // table exists in source and target, compare fields and Indexes
    if DiffFields in Kind then
      CompareFields (SourceTable.Fields, TargetTable.Fields, Kind);
    if DiffIndexes in Kind then
      CompareIndexes(SourceTable.Indexes, TargetTable.Indexes, Kind);
    end;
end;

procedure TCustomDDDiffer.CompareFields(Source, Target: TDDFieldDefs;
  Kind: TDiffKindSet);
var
  FieldList : TStringlist;
  r : integer;
begin
  FieldList := TStringlist.Create;
  try
    FieldList.Duplicates := dupIgnore;
    FieldList.Sorted := true;
    for r := 0 to Source.Count-1 do
      FieldList.Add (Source[r].FieldName);
    for r := 0 to Target.Count-1 do
      FieldList.Add (Target[r].FieldName);
    for r := 0 to FieldList.count-1 do
      CompareField(Source, Target, FieldList[r], Kind);
  finally
    FieldList.Free;
  end;
end;

procedure TCustomDDDiffer.CompareField(Source, Target: TDDFieldDefs;
  Fieldname: string; Kind: TDiffKindSet);

  Function FieldTypesEqual(F1,F2 : TDDFieldDef) : boolean;

  begin
    Result:=(F1.FieldType=F2.FieldType);
  end;

var
  SourceField, TargetField : TDDFieldDef;
begin
  SourceField := Source.FindField(FieldName);
  TargetField := Target.FindField(FieldName);
  if not assigned (TargetField) then
    FieldDifference(dtMissing, SourceField, nil)
  else if not assigned (SourceField) then
    FieldDifference(dtSurplus, nil, TargetField)
  else if (Not FieldTypesEqual(SourceField,TargetField))
          or (SourceField.required <> TargetField.required)
          or (SourceField.DefaultExpression <> TargetField.DefaultExpression)
          or ((SourceField.Size <> TargetField.Size) and not (SourceField.Fieldtype in [ftBlob]))
          or (SourceField.Precision <> TargetField.Precision) then
    FieldDifference(dtDifferent, SourceField, TargetField)
end;

procedure TCustomDDDiffer.CompareIndexes(Source, Target: TDDIndexDefs;
  Kind: TDiffKindSet);
var
  IndexList : TStringlist;
  r : integer;
begin
  IndexList := TStringlist.Create;
  try
    IndexList.Duplicates := dupIgnore;
    IndexList.Sorted := true;
    for r := 0 to Source.Count-1 do
      IndexList.Add (Source[r].IndexName);
    for r := 0 to Target.Count-1 do
      IndexList.Add (Target[r].IndexName);
    for r := 0 to IndexList.count-1 do
      CompareIndex(Source, Target, IndexList[r], Kind);
  finally
    IndexList.Free;
  end;
end;

procedure TCustomDDDiffer.CompareIndex(Source, Target: TDDIndexDefs;
  Indexname: string; Kind: TDiffKindSet);
var
  SourceIndex, TargetIndex : TDDIndexDef;
begin
  SourceIndex := Source.FindIndex(IndexName);
  TargetIndex := Target.FindIndex(IndexName);
  if not assigned (TargetIndex) then
    IndexDifference(dtMissing, SourceIndex, nil)
  else if not assigned (SourceIndex) then
    IndexDifference(dtSurplus, nil, TargetIndex)
  else if (CompareText(SourceIndex.Expression,TargetIndex.Expression) <> 0) or
          (CompareText(SourceIndex.Fields,TargetIndex.Fields) <> 0) or
          (SourceIndex.Options <> TargetIndex.Options) or
          (CompareText(SourceIndex.DescFields,TargetIndex.DescFields) <> 0) or
          (CompareText(SourceIndex.CaseInsFields,TargetIndex.CaseInsFields) <> 0) then
    IndexDifference(dtDifferent, SourceIndex, TargetIndex)
end;

procedure TCustomDDDiffer.Compare (Kind: TDiffKindSet);
begin
  if not assigned (FSourceDD) or not assigned (FTargetDD) then
    raise EDataDictDiff.Create(SErrMissingDatadict);
  CompareTables (Kind);
end;

end.

