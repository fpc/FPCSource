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
unit FPDDDiff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpdatadict;

type

  TDiffKind = (DiffTables, DiffFields, DiffIndexes, DiffSequences, DiffDomains);
  TDiffKindSet = set of TDiffKind;

  TDifferenceType = (dtMissing, dtDifferent, dtSurplus);
  
const
  diffAll = [DiffTables, DiffFields, DiffIndexes, DiffSequences, DiffDomains];
  
type
  
  { TCustomDDDiffer }

  TCustomDDDiffer = class
  private
    FSourceDD: TFPdatadictionary;
    FTargetDD: TFPdatadictionary;
  protected
    procedure DomainDifference (DiffType: TDifferenceType; SourceDomain, TargetDomain: TDDDomainDef); virtual;
    procedure SequenceDifference (DiffType: TDifferenceType; SourceSequence, TargetSequence: TDDSequenceDef); virtual;
    procedure TableDifference (DiffType: TDifferenceType; SourceTable, TargetTable: TDDTableDef); virtual;
    procedure IndexDifference (DiffType: TDifferenceType; SourceIndex, TargetIndex: TDDIndexDef); virtual;
    procedure FieldDifference (DiffType: TDifferenceType; SourceField, TargetField: TDDFieldDef); virtual;
    procedure CompareTables (Kind: TDiffKindSet);
    procedure CompareTable (const TableName: string; Kind: TDiffKindSet);
    procedure CompareFields (Source, Target: TDDFieldDefs; Kind: TDiffKindSet);
    procedure CompareField (Source, Target: TDDFieldDefs; const Fieldname: string; Kind: TDiffKindSet);
    procedure CompareIndexes (Source, Target: TDDIndexDefs; Kind: TDiffKindSet);
    procedure CompareIndex (Source, Target: TDDIndexDefs; const Indexname: string; Kind: TDiffKindSet);
    procedure CompareDomains (Kind: TDiffKindSet);
    procedure CompareDomain (Source, Target: TDDDomainDefs; const DomainName: string; Kind: TDiffKindSet);
    procedure CompareSequences (Kind: TDiffKindSet);
    procedure CompareSequence (Source, Target: TDDSequenceDefs; const SequenceName: string; Kind: TDiffKindSet);
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

procedure TCustomDDDiffer.DomainDifference(DiffType: TDifferenceType;
  SourceDomain, TargetDomain: TDDDomainDef);
begin

end;

procedure TCustomDDDiffer.SequenceDifference(DiffType: TDifferenceType;
  SourceSequence, TargetSequence: TDDSequenceDef);
begin

end;

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
  List : TStringlist;
  r : integer;

begin
  List := TStringlist.Create;
  try
    List.Duplicates:=dupIgnore;
    List.sorted := true;
    for r := 0 to SourceDD.Tables.Count-1 do
      List.Add (SourceDD.Tables[r].TableName);
    for r := 0 to TargetDD.Tables.Count-1 do
      List.Add (TargetDD.Tables[r].TableName);
    for r := 0 to List.count-1 do
      CompareTable (List[r], Kind);
  finally
    List.Free;
  end;
end;

procedure TCustomDDDiffer.CompareTable(const TableName: string; Kind: TDiffKindSet);

var
  Src, Targ : TDDTableDef;
begin
  Src := FSourceDD.Tables.FindTable(TableName);
  Targ := FTargetDD.Tables.FindTable(TableName);
  if Not assigned (Targ) then
    begin
    if DiffTables in Kind then
      TableDifference (dtMissing, Src, nil);
    end
  else if not assigned (Src) then
    begin
    if DiffTables in Kind then
      TableDifference (dtSurplus, nil, Targ);
    end
  else
    begin  // table exists in source and target, compare fields and Indexes
    if DiffFields in Kind then
      CompareFields (Src.Fields, Targ.Fields, Kind);
    if DiffIndexes in Kind then
      CompareIndexes(Src.Indexes, Targ.Indexes, Kind);
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
  const Fieldname: string; Kind: TDiffKindSet);

  Function FieldTypesEqual(F1,F2 : TDDFieldDef) : boolean;

  begin
    Result:=(F1.FieldType=F2.FieldType);
  end;

var
  Src, Targ : TDDFieldDef;
begin
  Src := Source.FindField(FieldName);
  Targ := Target.FindField(FieldName);
  if not assigned (Targ) then
    FieldDifference(dtMissing, Src, nil)
  else if not assigned (Src) then
    FieldDifference(dtSurplus, nil, Targ)
  else if (Not FieldTypesEqual(Src,Targ))
          or (Src.required <> Targ.required)
          or (comparetext(Src.DomainName, Targ.DomainName) <> 0)
          or (comparetext(Src.DefaultExpression, Targ.DefaultExpression) <> 0)
          or ((Src.Size <> Targ.Size) and not (Src.Fieldtype in [ftBlob]))
          or (Src.Precision <> Targ.Precision) then
    FieldDifference(dtDifferent, Src, Targ)
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
  const Indexname: string; Kind: TDiffKindSet);
var
  Src, Targ : TDDIndexDef;
begin
  Src := Source.FindIndex(IndexName);
  Targ := Target.FindIndex(IndexName);
  if not assigned (Targ) then
    IndexDifference(dtMissing, Src, nil)
  else if not assigned (Src) then
    IndexDifference(dtSurplus, nil, Targ)
  else if (CompareText(Src.Expression,Targ.Expression) <> 0) or
          (CompareText(Src.Fields,Targ.Fields) <> 0) or
          (Src.Options <> Targ.Options) or
          (CompareText(Src.DescFields,Targ.DescFields) <> 0) or
          (CompareText(Src.CaseInsFields,Targ.CaseInsFields) <> 0) then
    IndexDifference(dtDifferent, Src, Targ)
end;

procedure TCustomDDDiffer.CompareDomains(Kind: TDiffKindSet);

Var
  List : TStringList;
  R : Integer;

begin
  List := TStringlist.Create;
  try
    List.Duplicates:=dupIgnore;
    List.sorted := true;
    for r := 0 to SourceDD.Domains.Count-1 do
      List.Add (SourceDD.Domains[r].DomainName);
    for r := 0 to TargetDD.Domains.Count-1 do
      List.Add (TargetDD.Domains[r].DomainName);
    for r := 0 to List.count-1 do
      CompareDomain (SourceDD.Domains,TargetDD.Domains,List[r], Kind);
  finally
    List.Free;
  end;
end;

procedure TCustomDDDiffer.CompareDomain(Source, Target: TDDDomainDefs;
  const DomainName: string; Kind: TDiffKindSet);

var
  Src,Targ : TDDDomainDef;

begin
  Src := Source.FindDomain(DomainName);
  Targ := Target.FindDomain(DomainName);
  if not assigned (Targ) then
    DomainDifference(dtMissing, Src, nil)
  else if not assigned (Src) then
    DomainDifference(dtSurplus, nil, Targ)
  else if (Src.FieldType<>Targ.FieldType) or
          (Src.Required<>Targ.Required) or
          (Src.Precision<>Targ.Precision) or
          (Src.Size<>Targ.Size) then
    DomainDifference(dtDifferent, Src, Targ)
end;

procedure TCustomDDDiffer.CompareSequences(Kind: TDiffKindSet);

Var
  List : TStringList;
  R : Integer;

begin
  List := TStringlist.Create;
  try
    List.Duplicates:=dupIgnore;
    List.sorted := true;
    for r := 0 to SourceDD.Sequences.Count-1 do
      List.Add (SourceDD.Sequences[r].SequenceName);
    for r := 0 to TargetDD.Sequences.Count-1 do
      List.Add (TargetDD.Sequences[r].SequenceName);
    for r := 0 to List.count-1 do
      CompareSequence (SourceDD.Sequences,TargetDD.Sequences,List[r], Kind);
  finally
    List.Free;
  end;
end;

procedure TCustomDDDiffer.CompareSequence(Source, Target: TDDSequenceDefs;
  const SequenceName: string; Kind: TDiffKindSet);

var
  Src,Targ : TDDSequenceDef;

begin
  Src := Source.FindSequence(SequenceName);
  Targ := Target.FindSequence(SequenceName);
  if not assigned (Targ) then
    SequenceDifference(dtMissing, Src, nil)
  else if not assigned (Src) then
    SequenceDifference(dtSurplus, nil, Targ)
  else if (Src.StartValue<>Targ.StartValue) or
          (Src.Increment<>Targ.Increment) then
    SequenceDifference(dtDifferent, Src, Targ)
end;

procedure TCustomDDDiffer.Compare (Kind: TDiffKindSet);
begin
  if not assigned (FSourceDD) or not assigned (FTargetDD) then
    raise EDataDictDiff.Create(SErrMissingDatadict);
  CompareTables (Kind);
end;

end.

