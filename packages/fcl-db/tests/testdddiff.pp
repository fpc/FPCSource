{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    FPCUnit fpdddiff test.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit testdddiff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, fpcunit, fpdddiff, fpdatadict;

type

  { TMyDiff }

  TMyDiff = class (TCustomDDDiffer)
  private
    FMsg: TStringlist;
    function GetIndexName (ID : TDDIndexDef) : string;
    function GetFieldName (FD : TDDFieldDef) : string;
  protected
    procedure TableDifference (DiffType: TDifferenceType; SourceTable, TargetTable: TDDTableDef); override;
    procedure IndexDifference (DiffType: TDifferenceType; SourceIndex, TargetIndex: TDDIndexDef); override;
    procedure FieldDifference (DiffType: TDifferenceType; SourceField, TargetField: TDDFieldDef); override;
  public
    Constructor create;
    destructor destroy; override;
  public
    property Messages : TStringlist read FMsg;
  end;
  
  { TTestDDDiff }

  TTestDDDiff = class (TTestcase)
  private
    Differ : TMyDiff;
    SourceDD, TargetDD : TFPDataDictionary;
    procedure SetupSourceDD;
    procedure SetupTargetDD;
    function CreateTable (DD: TFPDataDictionary; tablename:string) : TDDTableDef;
    procedure AssertMessageCount (ACount: integer);
    procedure AssertMessage (AMessage: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEquals;
    procedure TestSourceTable;
    procedure TestTargetTable;
    procedure TestSourceField;
    procedure TestTargetField;
    procedure TestSourceIndex;
    procedure TestTargetIndex;
    procedure TestFieldType;
    procedure TestFieldSize;
    procedure TestFieldPrecision;
    procedure TestFieldDefExpression;
    procedure TestFieldRequired;
    procedure TestIndexOptions;
    procedure TestIndexExpression;
    procedure TestIndexFields;
    procedure TestIndexDescFields;
    procedure TestIndexCaseInsFields;
  end;
  
  
implementation

uses db;

{ TMyDiff }

function TMyDiff.GetIndexName(ID: TDDIndexDef): string;
begin
  result := TDDIndexdefs(ID.Collection).TableName + '.' + ID.IndexName;
end;

function TMyDiff.GetFieldName(FD: TDDFieldDef): string;
begin
  result := TDDFielddefs(FD.Collection).TableName + '.' + FD.FieldName;
end;

procedure TMyDiff.TableDifference(DiffType: TDifferenceType; SourceTable,
  TargetTable: TDDTableDef);
begin
  case DiffType of
    dtMissing: FMsg.Add (format('ST %s', [SourceTable.TableName]));
    dtSurplus: FMsg.Add (format('TT %s', [TargetTable.TableName]));
    dtDifferent: FMsg.Add (format('DT', [TargetTable.TableName]));
  end;
end;

procedure TMyDiff.IndexDifference(DiffType: TDifferenceType; SourceIndex,
  TargetIndex: TDDIndexDef);
begin
  case DiffType of
    dtMissing: FMsg.Add (format('SI %s', [getindexname(SourceIndex)]));
    dtSurplus: FMsg.Add (format('TI %s', [getindexname(TargetIndex)]));
    dtDifferent: FMsg.Add (format('DI %s', [getindexname(TargetIndex)]));
  end;
end;

procedure TMyDiff.FieldDifference(DiffType: TDifferenceType; SourceField,
  TargetField: TDDFieldDef);
begin
  case DiffType of
    dtMissing: FMsg.Add (format('SF %s', [getfieldname(SourceField)]));
    dtSurplus: FMsg.Add (format('TF %s', [getfieldname(TargetField)]));
    dtDifferent: FMsg.Add (format('DF %s', [getfieldname(TargetField)]));
  end;
end;

constructor TMyDiff.create;
begin
  inherited;
  FMsg := TStringlist.Create;
end;

destructor TMyDiff.destroy;
begin
  FMsg.Free;
  inherited destroy;
end;

{ TTestDDDiff }

procedure TTestDDDiff.SetupSourceDD;
begin
  SourceDD := TFPDataDictionary.Create;
  CreateTable (SourceDD, 'EERSTE');
  CreateTable (SourceDD, 'TWEEDE');
end;

procedure TTestDDDiff.SetupTargetDD;
begin
  TargetDD := TFPDataDictionary.Create;
  CreateTable (TargetDD, 'EERSTE');
  CreateTable (TargetDD, 'TWEEDE');
end;

function TTestDDDiff.CreateTable(DD: TFPDataDictionary; tablename: string): TDDTableDef;
begin
  result := dd.Tables.AddTable(tablename);
  with result.Fields.AddField('ID') do
    begin
    FieldType := ftLargeint;
    Required:=True;
    end;
  with result.Fields.AddField('eerste') do
    begin
    FieldType := ftString;
    Required:=True;
    Size := 25;
    end;
  with result.Fields.AddField('Tweede') do
    begin
    FieldType := ftFloat;
    Required:=False;
    Size := 12;
    Precision := 4;
    end;
  with result.Fields.AddField('Extralang') do
    begin
    FieldType := ftString;
    Required:=false;
    Size := 1024;
    end;
  with result.Indexes.AddDDIndexDef('Primary') do
    begin
    Fields:='ID';
    options := [ixPrimary];
    end;
  with result.Indexes.AddDDIndexDef('UniqueEerste') do
    begin
    Fields:='eerste,tweede';
    DescFields:='eerste';
    options := [ixUnique];
    end;
end;

procedure TTestDDDiff.AssertMessageCount(ACount: integer);
begin
  AssertEquals('Number of differences', ACount, Differ.Messages.count);
end;

procedure TTestDDDiff.AssertMessage(AMessage: string);
begin
  if Differ.Messages.count > 1 then
    Fail ('More differences then expected: expected '+AMessage+', got '+differ.Messages.Commatext)
  else if Differ.messages.count = 0 then
    Fail ('No differences found, expected 1: '+AMessage);
  AssertEquals ('Difference detected,', AMessage, Differ.Messages[0])
end;

procedure TTestDDDiff.SetUp;
begin
  inherited SetUp;
  SetupSourceDD;
  SetupTargetDD;
  Differ := TMyDiff.Create;
  Differ.SourceDD := SourceDD;
  Differ.TargetDD := TargetDD;
end;

procedure TTestDDDiff.TearDown;
begin
  Differ.Free;
  FreeAndNil(SourceDD);
  FreeAndNil(TargetDD);
  inherited TearDown;
end;

procedure TTestDDDiff.TestEquals;
begin
  Differ.Compare(diffAll);
  AssertMessageCount (0);
end;

procedure TTestDDDiff.TestSourceTable;
begin
  SourceDD.Tables.AddTable ('eentabel');
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('ST eentabel');
end;

procedure TTestDDDiff.TestTargetTable;
begin
  TargetDD.Tables.AddTable ('eentabel');
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('TT eentabel');
end;

procedure TTestDDDiff.TestSourceField;
begin
  with SourceDD.Tables.TableByName('TWEEDE').AddField ('extra') do
    begin
    FieldType := ftCurrency;
    size := 12;
    precision := 2;
    required := true;
    end;
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('SF TWEEDE.extra');
end;

procedure TTestDDDiff.TestTargetField;
begin
  with TargetDD.Tables.TableByName('TWEEDE').AddField ('extra') do
    begin
    FieldType := ftCurrency;
    size := 12;
    precision := 2;
    required := true;
    end;
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('TF TWEEDE.extra');
end;

procedure TTestDDDiff.TestSourceIndex;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Indexes.AddIndex ('extra') do
    begin
    Fields := 'Tweede';
    Options := [ixUnique];
    end;
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('SI TWEEDE.extra');
end;

procedure TTestDDDiff.TestTargetIndex;
begin
  with TargetDD.Tables.TableByName('TWEEDE').Indexes.AddIndex ('extra') do
    begin
    Fields := 'Tweede';
    Options := [ixUnique];
    end;
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('TI TWEEDE.extra');
end;

procedure TTestDDDiff.TestFieldType;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Fields.FieldByName ('tweede') do
    FieldType := ftCurrency;
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DF TWEEDE.Tweede');
end;

procedure TTestDDDiff.TestFieldSize;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Fields.FieldByName ('tweede') do
    Size := 16;
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DF TWEEDE.Tweede');
end;

procedure TTestDDDiff.TestFieldPrecision;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Fields.FieldByName ('tweede') do
    Precision := 0;
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DF TWEEDE.Tweede');
end;

procedure TTestDDDiff.TestFieldDefExpression;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Fields.FieldByName ('tweede') do
    DefaultExpression := '258.2345';
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DF TWEEDE.Tweede');
end;

procedure TTestDDDiff.TestFieldRequired;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Fields.FieldByName ('tweede') do
    Required := true;
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DF TWEEDE.Tweede');
end;

procedure TTestDDDiff.TestIndexOptions;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Indexes.IndexByName('UniqueEerste') do
    Options := [ixUnique, ixDescending];
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DI TWEEDE.UniqueEerste');
end;

procedure TTestDDDiff.TestIndexExpression;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Indexes.IndexByName('UniqueEerste') do
    Expression := 'Eerste+Tweede';
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DI TWEEDE.UniqueEerste');
end;

procedure TTestDDDiff.TestIndexFields;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Indexes.IndexByName('UniqueEerste') do
    Fields := 'Eerste';
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DI TWEEDE.UniqueEerste');
end;

procedure TTestDDDiff.TestIndexDescFields;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Indexes.IndexByName('UniqueEerste') do
    DescFields := 'Tweede';
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DI TWEEDE.UniqueEerste');
end;

procedure TTestDDDiff.TestIndexCaseInsFields;
begin
  with SourceDD.Tables.TableByName('TWEEDE').Indexes.IndexByName('UniqueEerste') do
    CaseInsFields := 'Eesrte';
  Differ.Compare(diffAll);
  AssertMessageCount (1);
  AssertMessage ('DI TWEEDE.UniqueEerste');
end;

initialization

  RegisterTest (TTestDDDiff);
  
end.

