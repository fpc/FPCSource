unit FPDDPopCode;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    type safe field populate code generator

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, fpdatadict, db;

Type
  TDDCodeGenOption = (dcoFields,dcoIndexes,dcoProcedurePerTable,dcoUseWith,
                      dcoClassDecl,dcoGenerators,dcoDomains,dcoMergeDomains);
  TDDCodeGenOptions = Set of TDDCodeGenoption;
  
  { TFPDDPopulateCodeGenerator }

  TFPDDPopulateCodeGenerator = Class(TComponent)
  private
    FClassName: String;
    FDD: TFPDataDictionary;
    FDDV: String;
    FIndent: Integer;
    FCurrentIndent: Integer;
    FOptions: TDDCodeGenOptions;
    FTables: TStrings;
    FProcedures : TStrings;
    procedure SetOptions(const AValue: TDDCodeGenOptions);
    procedure SetTables(const AValue: TStrings);
  Protected
    // General code generating routines
    procedure AddProperty(const ObjName, PropName, PropValue: String; Lines: TStrings);
    procedure AddProperty(const ObjName, PropName: String; PropValue: Boolean; Lines: TStrings);
    procedure AddStringProperty(const ObjName, PropName, PropValue: String;  Lines: TStrings);
    procedure AddProcedure(AProcedureName: String; Lines: TStrings); virtual;
    procedure EndProcedure(Lines: TStrings);
    Procedure Indent;
    Procedure Undent;
    procedure AddLine(ALine: String; Lines: TStrings); virtual;
    Function EscapeString(Const S : String) : string;
    procedure CreateClassDecl(Lines: TStrings); virtual;
    // Data dictionare specific
    procedure CheckDatadict;
    procedure CreateFooter(Lines: TStrings);
    procedure CreateHeader(Lines: TStrings);
    // Table code
    Function DoTable (Const ATable : TDDtableDef) : Boolean; virtual;
    procedure CreateTableCode(T: TDDTableDef; Lines: TStrings);
    procedure AddTableVars(Lines: TStrings);
    procedure AddDomainVars(Lines: TStrings);
    procedure AddSequenceVars(Lines: TStrings);
    procedure DoTableHeader(ATable: TDDTableDef; Lines: TStrings);
    procedure DoTableFooter(ATable: TDDTableDef; Lines: TStrings);
    // Field code
    Function DoField (Const ATable : TDDtableDef; Const AField : TDDFieldDef) : Boolean; virtual;
    procedure CreateFieldCode(ATable: TDDTableDef; AField: TDDFieldDef;  Lines: TStrings);
    // Index code
    Function DoIndex (Const ATable : TDDtableDef; Const AIndex : TDDIndexDef) : Boolean; virtual;
    procedure CreateIndexCode(ATable: TDDTableDef; AIndex: TDDIndexDef;  Lines: TStrings);
    // Sequence code
    Procedure WriteSequences(Const ASequences : TDDSequenceDefs; Lines :TStrings);
    Function DoSequence (Const ASequence : TDDSequenceDef) : Boolean; virtual;
    procedure CreateSequenceCode(ASequence: TDDSequenceDef;  Lines: TStrings);
    // Domain code
    Procedure WriteDomains(Const ADomains : TDDDomainDefs; Lines :TStrings);
    Function DoDomain (Const ADomain : TDDDomainDef) : Boolean; virtual;
    procedure CreateDomainCode(ADomain: TDDDomainDef;  Lines: TStrings);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure CreateCode(Lines : TStrings);
    Property DataDictionary : TFPDataDictionary Read FDD Write FDD;
  Published
    Property Options : TDDCodeGenOptions Read FOptions Write SetOptions;
    Property Tables : TStrings Read FTables Write SetTables;
    Property IndentSize : Integer Read FIndent Write FIndent;
    Property DDVarName : String Read FDDV Write FDDV;
    Property ClassName : String Read FClassName Write FClassName;
  end;
  
implementation

Resourcestring
  SErrNoDataDictionary = 'Cannot perform this operation without datadictionary';
  SErrNoDataDictionaryName = 'Cannot perform this operation without datadictionary name';
  

{ TFPDDPopulateCodeGenerator }

procedure TFPDDPopulateCodeGenerator.SetOptions(const AValue: TDDCodeGenOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TFPDDPopulateCodeGenerator.SetTables(const AValue: TStrings);
begin
  if FTables=AValue then exit;
  FTables.Assign(AValue);
end;

function TFPDDPopulateCodeGenerator.DoTable(Const ATable: TDDtableDef): Boolean;
begin
  Result:=Assigned(ATable) and ((FTables.Count=0) or (FTables.IndexOf(ATable.TableName)<>-1));
end;

function TFPDDPopulateCodeGenerator.DoField(const ATable: TDDtableDef;
  const AField: TDDFieldDef): Boolean;
begin
  Result:=Assigned(ATable) and Assigned(AField);
end;

constructor TFPDDPopulateCodeGenerator.Create(AOwner: TComponent);

Var
  T : TStringList;

begin
  inherited Create(AOwner);
  T:=TStringList.Create;
  T.Sorted:=true;
  T.Duplicates:=dupIgnore;
  FTables:=T;
  IndentSize:=2;
end;

destructor TFPDDPopulateCodeGenerator.Destroy;
begin
  FreeAndNil(FTables);
  inherited Destroy;
end;

procedure TFPDDPopulateCodeGenerator.CheckDatadict;

begin
  If (FDD=Nil) then
    Raise EDataDict.Create(SErrNoDataDictionary);
  If (FDDV='') then
    Raise EDataDict.Create(SErrNoDataDictionaryName);
end;

function TFPDDPopulateCodeGenerator.EscapeString(const S: String): string;
begin
  Result:=StringReplace(S,'''','''''',[rfReplaceAll]);
end;

procedure TFPDDPopulateCodeGenerator.AddProcedure(AProcedureName : String; Lines: TStrings);

Var
  S : String;

begin
  S:=AProcedureName;
  FProcedures.Add(S);
  If (FClassName<>'') then
    S:=FClassName+'.'+S;
  AddLine('Procedure '+S+';',Lines);
end;

procedure TFPDDPopulateCodeGenerator.EndProcedure(Lines: TStrings);

begin
  Undent;
  AddLine('end;',lines);
  AddLine('',Lines)
end;


procedure TFPDDPopulateCodeGenerator.AddLine(ALine: String; Lines: TStrings);
begin
  If (ALine<>'') and (FCurrentIndent<>0) then
    Aline:=StringOfChar(' ',FCurrentIndent)+ALine;
  Lines.Add(ALine);
end;

procedure TFPDDPopulateCodeGenerator.Indent;
begin
  Inc(FCurrentIndent,FIndent);
end;

procedure TFPDDPopulateCodeGenerator.Undent;
begin
  Dec(FCurrentIndent,FIndent);
  If (FCurrentIndent<0) then
    FCurrentIndent:=0;
end;

procedure TFPDDPopulateCodeGenerator.AddTableVars(Lines: TStrings);

begin
  AddLine('',Lines);
  AddLine('Var',Lines);
  Indent;
  AddLine('T : TDDTableDef;',lines);
  If dcoFields in Options then
    AddLine('F : TDDFieldDef;',lines);
  If dcoIndexes in Options then
    AddLine('ID : TDDIndexDef;',lines);
  Undent;
end;

procedure TFPDDPopulateCodeGenerator.AddDomainVars(Lines: TStrings);
begin
  AddLine('Var',Lines);
  Indent;
  AddLine('D : TDDDomainDef;',lines);
  Undent;
end;

procedure TFPDDPopulateCodeGenerator.AddSequenceVars(Lines: TStrings);
begin
  AddLine('Var',Lines);
  Indent;
  AddLine('D : TDDSequenceDef;',lines);
  Undent;
end;


procedure TFPDDPopulateCodeGenerator.DoTableHeader(ATable : TDDTableDef; Lines: TStrings);

begin
  If dcoProcedurePerTable in Options then
    begin
    AddProcedure('PopulateTable'+ATable.TableName,Lines);
    AddTableVars(Lines);
    AddLine('',Lines);
    AddLine('begin',Lines);
    Indent;
    end;
  AddLine(Format('T:=%s.Tables.AddTable(''%s'');',[FDDV,ATable.TableName]),Lines);
end;

procedure TFPDDPopulateCodeGenerator.DoTableFooter(ATable : TDDTableDef; Lines: TStrings);

begin
  If dcoProcedurePerTable in Options then
    EndProcedure(Lines);
end;

procedure TFPDDPopulateCodeGenerator.AddProperty(Const ObjName,PropName : String; PropValue : Boolean; Lines: TStrings);

begin
  If PropValue then
    AddProperty(ObjName,PropName,'True',Lines)
  else
    AddProperty(ObjName,PropName,'False',Lines);
end;

procedure TFPDDPopulateCodeGenerator.AddProperty(Const ObjName,PropName,PropValue : String; Lines: TStrings);

begin
  If Not (dcoUseWith in Options) then
    AddLine(Format('%s.%s:=%s;',[Objname,Propname,PropValue]),lines)
  else
    AddLine(Format('%s:=%s;',[Propname,PropValue]),lines);
end;

procedure TFPDDPopulateCodeGenerator.AddStringProperty(Const ObjName,PropName,PropValue : String; Lines: TStrings);

begin
  If (PropValue<>'') then
    If Not (dcoUseWith in Options) then
      AddLine(Format('%s.%s:=''%s'';',[Objname,Propname,EscapeString(PropValue)]),lines)
    else
      AddLine(Format('%s:=''%s'';',[Propname,EscapeString(PropValue)]),lines);
end;

procedure TFPDDPopulateCodeGenerator.CreateFieldCode(ATable : TDDTableDef; AField : TDDFieldDef; Lines: TStrings);

Var
  I : Integer;
  S : String;

begin
  AddLine(Format('F:=T.Fields.AddField(''%s'');',[AField.FieldName]),Lines);
  If (dcoUseWith in Options) then
     begin
     AddLine('With F do',Lines);
     Indent;
     AddLine('begin',Lines);
     end;
  if (AField.FieldType<>ftUnknown) then
    AddProperty('F','FieldType',GetEnumName(TypeInfo(TFieldType),Ord(AField.FieldType)),Lines);
  If (AField.AlignMent<>taLeftJustify) then
    AddProperty('F','AlignMent',GetEnumName(TypeInfo(TAlignMent),Ord(AField.AlignMent)),Lines);
  AddStringProperty('F','CustomConstraint',AField.CustomConstraint,Lines);
  AddStringProperty('F','ConstraintErrorMessage',AField.ConstraintErrorMessage,Lines);
  AddStringProperty('F','DBDefault',AField.DBDefault,Lines);
  AddStringProperty('F','DefaultExpression',AField.DefaultExpression,Lines);
  AddStringProperty('F','DisplayLabel',AField.DisplayLabel,Lines);
  AddStringProperty('F','DomainName',AField.DomainName,Lines);
  If (AField.DisplayWidth<>0) then
    AddProperty('F','DisplayWidth1',IntToStr(AField.DisplayWidth),Lines);
  AddStringProperty('F','Constraint',AField.Constraint,Lines);
  AddProperty('F','ReadOnly',AField.ReadOnly,Lines);
  If (dcoMergeDomains in Options) then
    AddProperty('F','Required',AField.FieldIsRequired,Lines)
  else
    AddProperty('F','Required',AField.Required,Lines);
  AddProperty('F','Visible',AField.Visible,Lines);
  If (AField.Size<>0) then
    AddProperty('F','Size',IntToStr(AField.Size),Lines);
  If (AField.Precision<>0) then
    AddProperty('F','Precision',IntToStr(AField.Precision),Lines);
  AddStringProperty('F','Hint',AField.Hint,Lines);
  I:=Integer(AField.ProviderFlags);
  S:=SetToString(PTypeInfo(TypeInfo(TProviderFlags)),I,True);
  AddProperty('F','ProviderFlags',S,Lines);
  If (dcoUseWith in Options) then
     begin
     AddLine('end;',Lines);
     Undent;
     end;
end;

function TFPDDPopulateCodeGenerator.DoIndex(const ATable: TDDtableDef;
  const AIndex: TDDIndexDef): Boolean;
begin
  Result:=Assigned(ATable) and Assigned(AIndex);
end;

procedure TFPDDPopulateCodeGenerator.CreateIndexCode(ATable: TDDTableDef;
  AIndex: TDDIndexDef; Lines: TStrings);

Var
  S : string;
  I : Integer;

begin
  AddLine(Format('ID:=T.Indexes.AddIndex(''%s'');',[AIndex.IndexName]),Lines);
  If (dcoUseWith in Options) then
     begin
     AddLine('With ID do',Lines);
     Indent;
     AddLine('begin',Lines);
     end;
  AddStringProperty('ID','Expression',AIndex.Expression,Lines);
  AddStringProperty('ID','Fields',AIndex.Fields,Lines);
  AddStringProperty('ID','CaseInsFields',AIndex.CaseInsFields,Lines);
  AddStringProperty('ID','DescFields',AIndex.DescFields,Lines);
  AddStringProperty('ID','Source',AIndex.Source,Lines);
  I:=Integer(AIndex.Options);
  S:=SetToString(PTypeInfo(TypeInfo(TIndexOptions)),I,True);
  AddProperty('ID','Options',S,Lines);
  If (dcoUseWith in Options) then
     begin
     AddLine('end;',Lines);
     Undent;
     end;

end;

procedure TFPDDPopulateCodeGenerator.WriteSequences(
  const ASequences: TDDSequenceDefs; Lines: TStrings);

Var
  I : Integer;
  S : TDDSequenceDef;

begin
  If (dcoProcedurePerTable in Options) then
    begin
    AddProcedure('PopulateSequences',Lines);
    AddSequenceVars(Lines);
    AddLine('',Lines);
    AddLine('begin',Lines);
    Indent;
    end;
  For I:=0 to ASequences.Count-1 do
    begin
    S:=ASequences[i];
    If DoSequence(S) then
      CreateSequenceCode(S,Lines);
    end;

  If (dcoProcedurePerTable in Options) then

    EndProcedure(Lines);
end;

function TFPDDPopulateCodeGenerator.DoSequence(const ASequence: TDDSequenceDef): Boolean;
begin
  Result:=Assigned(ASequence);
end;

procedure TFPDDPopulateCodeGenerator.CreateSequenceCode(ASequence: TDDSequenceDef; Lines: TStrings);
begin
  AddLine(Format('S:=%s.Sequences.AddSequence(''%s'');',[FDDV,ASequence.SequenceName]),Lines);
  If (dcoUseWith in Options) then
     begin
     AddLine('With S do',Lines);
     Indent;
     AddLine('begin',Lines);
     end;
  If (ASequence.StartValue<>0) then
    AddProperty('S','StartValue',IntToStr(ASequence.StartValue),Lines);
  If (ASequence.Increment<>0) then
    AddProperty('S','Increment',IntToStr(ASequence.Increment),Lines);
  If (dcoUseWith in Options) then
     begin
     AddLine('end;',Lines);
     Undent;
     end;
end;

procedure TFPDDPopulateCodeGenerator.WriteDomains(const ADomains: TDDDomainDefs; Lines :TStrings);

Var
  I : Integer;
  D : TDDDomainDef;

begin
  If (dcoProcedurePerTable in Options) then
    begin
    AddProcedure('PopulateDomains',Lines);
    AddDomainVars(Lines);
    AddLine('',Lines);
    AddLine('begin',Lines);
    Indent;
    end;
  For I:=0 to FDD.Domains.Count-1 do
    begin
    D:=FDD.Domains[i];
    If DoDomain(D) then
      CreateDomainCode(D,Lines);
    end;
  If (dcoProcedurePerTable in Options) then
    EndProcedure(Lines);
end;

function TFPDDPopulateCodeGenerator.DoDomain(const ADomain: TDDDomainDef
  ): Boolean;
begin
  Result:=Assigned(ADomain);
end;

procedure TFPDDPopulateCodeGenerator.CreateDomainCode(ADomain: TDDDomainDef;
  Lines: TStrings);
begin
  AddLine(Format('D:=%s.Domains.AddDomain(''%s'');',[FDDV,ADomain.DomainName]),Lines);
  If (dcoUseWith in Options) then
     begin
     AddLine('With D do',Lines);
     Indent;
     AddLine('begin',Lines);
     end;
  if (ADomain.FieldType<>ftUnknown) then
    AddProperty('D','FieldType',GetEnumName(TypeInfo(TFieldType),Ord(ADomain.FieldType)),Lines);
  AddProperty('D','Required',ADomain.Required,Lines);
  If (ADomain.Size<>0) then
    AddProperty('D','Size',IntToStr(ADomain.Size),Lines);
  If (ADomain.Precision<>0) then
    AddProperty('D','Precision',IntToStr(ADomain.Precision),Lines);
  If (dcoUseWith in Options) then
     begin
     AddLine('end;',Lines);
     Indent;
     end;
end;

procedure TFPDDPopulateCodeGenerator.CreateHeader(Lines: TStrings);

begin
  If Not (dcoProcedurePerTable in Options) then
    begin
    AddProcedure('PopulateDataDictionary',Lines);
    AddTableVars(Lines);
    AddLine('',Lines);
    AddLine('begin',Lines);
    Indent;
    end
end;

procedure TFPDDPopulateCodeGenerator.CreateFooter(Lines: TStrings);

Var
  i : integer;
  L : TStrings;

begin
  If (dcoProcedurePerTable in Options) then
    begin
    L:=TStringList.Create;
    try
      L.Assign(FProcedures);
      AddProcedure('PopulateDataDictionary',Lines);
      AddLine('',Lines);
      AddLine('begin',Lines);
      Indent;
      For I:=0 to L.Count-1 do
        begin
        AddLine(L[i]+';',Lines);
        end;
    finally
      L.Free;
    end;
    end;
  Undent;
  EndProcedure(Lines);
end;

procedure TFPDDPopulateCodeGenerator.CreateTableCode(T : TDDTableDef; Lines: TStrings);

Var
  I : Integer;
  F : TDDFieldDef;
  Id : TDDindexDef;

begin
  DoTableHeader(T,Lines);
  try
    If dcoFields in Options then
      For I:=0 to T.Fields.Count-1 Do
        begin
        F:=T.Fields[I];
        If DoField(T,F) then
          CreateFieldcode(T,F,Lines);
        end;
    If dcoIndexes in Options then
      For I:=0 to T.Indexes.Count-1 Do
        begin
        ID:=T.Indexes[I];
        If DoIndex(T,ID) then
          CreateIndexCode(T,ID,Lines);
        end;
  Finally
    DoTableFooter(T,Lines);
  end;
end;

procedure TFPDDPopulateCodeGenerator.CreateClassDecl(Lines: TStrings);

Var
  I : integer;

begin
  AddLine('(*',Lines);
  Indent;
  AddLine(Format('%s = Class(TObject)',[ClassName]),Lines);
  AddLine('Private',lines);
  Indent;
  AddLine(Format('F%s : TFPDataDictionary;',[FDDV]),Lines);
  Undent;
  AddLine('Public',Lines);
  Indent;
  For I:=0 to FProcedures.Count-1 do
    AddLine(Format('Procedure %s;',[FProcedures[i]]),Lines);
  AddLine(Format('Property %s : TFPDataDictionary Read F%:0s Write F%:0s;',[FDDV]),Lines);
  Undent;
  AddLine('end;',lines);
  Undent;
  AddLine('*)',Lines);
end;

procedure TFPDDPopulateCodeGenerator.CreateCode(Lines: TStrings);

Var
  I : Integer;
  T : TDDTableDef;
  F : TDDFieldDef;

begin
  FCurrentIndent:=0;
  CheckDataDict;
  FProcedures:=TStringList.Create;
  try
    CreateHeader(Lines);
    Try
      If (FDD.Domains.Count>0) then
        WriteDomains(FDD.Domains,Lines);
      If (FDD.Sequences.Count>0) then
        WriteSequences(FDD.Sequences,Lines);
      For I:=0 to FDD.Tables.Count-1 do
        begin
        T:=FDD.Tables[i];
        If DoTable(T) then
          CreateTableCode(T,Lines);
        end;
    Finally
      CreateFooter(Lines);
    end;
    If (dcoClassDecl in Options) and (FClassName<>'') then
      CreateClassDecl(Lines);
  finally
    FreeAndNil(FProcedures);
  end;
end;

end.

