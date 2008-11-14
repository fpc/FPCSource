unit fpddpopcode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, fpdatadict, db;

Type
  TDDCodeGenOption = (dcoFields,dcoIndexes,dcoProcedurePerTable,dcoUseWith,dcoClassDecl);
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
    procedure DoTableHeader(ATable: TDDTableDef; Lines: TStrings);
    procedure DoTableFooter(ATable: TDDTableDef; Lines: TStrings);
    // Field code
    Function DoField (Const ATable : TDDtableDef; Const AField : TDDFieldDef) : Boolean; virtual;
    procedure CreateFieldCode(ATable: TDDTableDef; AField: TDDFieldDef;  Lines: TStrings);
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
  If (AField.DisplayWidth<>0) then
    AddProperty('F','DisplayWidth',IntToStr(AField.DisplayWidth),Lines);
  AddStringProperty('F','Constraint',AField.Constraint,Lines);
  AddProperty('F','ReadOnly',AField.ReadOnly,Lines);
  AddProperty('F','Required',AField.Required,Lines);
  AddProperty('F','Visible',AField.Visible,Lines);
  If (AField.Size<>0) then
    AddProperty('F','Size',IntToStr(AField.Size),Lines);
  If (AField.Precision<>0) then
    AddProperty('F','Precision',IntToStr(AField.Precision),Lines);
  AddStringProperty('F','Hint',AField.Hint,Lines);
  If (dcoUseWith in Options) then
     begin
     AddLine('end;',Lines);
     Undent;
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

