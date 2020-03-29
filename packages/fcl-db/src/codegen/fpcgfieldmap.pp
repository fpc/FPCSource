unit fpcgfieldmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpddcodegen;

Type

  { TGenFieldMapOptions }
  TFieldMapOption = (fmoPublicFields,fmoRequireFields);
  TFieldMapOptions = Set of TFieldMapOption;

  TGenFieldMapOptions = Class(TClassCodeGeneratorOptions)
  Private
    FOptions: TFieldMapOptions;
  Protected
    function GetMapAncestorName: String; virtual;
    function GetMapName: String; virtual;
    procedure SetMapAncestorName(const AValue: String); virtual;
    procedure SetMapClassName(const AValue: String); virtual;
  Public
    Constructor Create; override;
    Procedure Assign(ASource: TPersistent); override;
    Property MapAncestorName : String Read GetMapAncestorName Write SetMapAncestorName;
    Property MapClassName : String Read GetMapName Write SetMapClassName;
  Published
    Property FieldMapOptions : TFieldMapOptions Read FOptions Write FOptions;
  end;

  { TDDDBFieldMapCodeGenerator }

  TDDBaseFieldMapCodeGenerator = Class(TDDClassCodeGenerator)
  private
    function GetOpt: TGenFieldMapOptions;
  Protected
    // Overrides;
    Function GetInterfaceUsesClause : string; override;
    Function CreateOptions : TCodeGeneratorOptions; override;
    // New methods
    procedure DoCreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String);
    procedure WriteMapInitFields(Strings: TStrings; const ObjectClassName,   MapClassName: String); virtual;
    procedure CreateFieldMapImplementation(Strings: TStrings; const ObjectClassName, MapClassName: String);
    Property FieldMapOpts : TGenFieldMapOptions Read Getopt;
  Public
    procedure CreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String);
  end;

  TDDDBFieldMapCodeGenerator = Class(TDDBaseFieldMapCodeGenerator)
  Protected
    Procedure DoGenerateInterface(Strings: TStrings); override;
    Procedure DoGenerateImplementation(Strings: TStrings); override;
  Public
    Property FieldMapOpts;
  end;

implementation

{ TDDDBFieldMapCodeGenerator }

function TDDBaseFieldMapCodeGenerator.GetOpt: TGenFieldMapOptions;
begin
  Result:=CodeOptions as TGenFieldMapOptions;
end;

function TDDBaseFieldMapCodeGenerator.GetInterfaceUsesClause: string;
begin
  Result:=inherited GetInterfaceUsesClause;
  If (Result<>'') then
    Result:=Result+', db, fieldmap';
end;

procedure TDDDBFieldMapCodeGenerator.DoGenerateInterface(Strings: TStrings);
begin
  inherited DoGenerateInterface(Strings);
  AddLn(Strings,'Type');
  CreatefieldMapDeclaration(Strings,'',GetOpt.MapClassName,GetOpt.MapAncestorName);
end;

procedure TDDDBFieldMapCodeGenerator.DoGenerateImplementation(Strings: TStrings
  );
begin
  inherited DoGenerateImplementation(Strings);
  With FieldMapOpts do
    CreateFieldMapImplementation(Strings,ObjectClassName,MapClassName);
end;

function TDDBaseFieldMapCodeGenerator.CreateOptions: TCodeGeneratorOptions;
begin
  Result:=TGenFieldMapOptions.Create;
end;

procedure TDDBaseFieldMapCodeGenerator.DoCreateFieldMapDeclaration(
  Strings: TStrings; const ObjectClassName, MapClassName,
  MapAncestorName: String);

Var
  I : Integer;
  F : TFieldPropDef;

begin
  AddLn(Strings,'Private');
  IncIndent;
  Try
    For I:=0 to Fields.Count-1 do
      begin
      F:=Fields[I];
      If F.Enabled then
        AddLn(Strings,'F%s : TField;',[F.PropertyName]);
      end;
  Finally
    DecIndent;
  end;
  AddLn(Strings,'Public');
  IncIndent;
  Try
    AddLn(Strings,'Procedure InitFields; Override;');
    if fmoPublicFields in  FieldMapOpts.FieldMapOptions then
      For I:=0 to Fields.Count-1 do
        begin
        F:=Fields[I];
        If F.Enabled then
          AddLn(Strings,'Property %s : TField read F%s;',[F.PropertyName,F.FieldName]);
        end;
  Finally
    DecIndent;
  end;
end;

procedure TDDBaseFieldMapCodeGenerator.CreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName, MapClassName, MapAncestorName: String);
begin
  Addln(Strings);
  IncIndent;
  try
    Addln(Strings,'{ %s }',[MapClassName]);
    Addln(Strings);
    Addln(Strings,'%s = Class(%s)',[MapClassName,MapAncestorName]);
    DoCreateFieldMapDeclaration(Strings,ObjectClassName,MapClassName,MapAncestorName);
    AddLn(Strings,'end;');
  Finally
    DecIndent;
  end;
end;

procedure TDDBaseFieldMapCodeGenerator.CreateFieldMapImplementation(
  Strings: TStrings; const ObjectClassName, MapClassName: String);

Var
  S : String;

begin
  AddLn(Strings,' { %s }',[MapClassName]);
  AddLn(Strings);
  S:=Format('Procedure %s.InitFields;',[MapClassName]);
  BeginMethod(Strings,S);
  Try
    WriteMapInitFields(Strings,ObjectClassName,MapClassName);
  Finally
    EndMethod(Strings,S);
  end;
end;

procedure TDDBaseFieldMapCodeGenerator.WriteMapInitFields(Strings: TStrings;
  const ObjectClassName, MapClassName: String);

Const
  Finders : Array[Boolean] of string = ('FindField','FieldByName');

Var
  I: Integer;
  F : TFieldPropDef;
  Fmt : String;
begin
  AddLn(Strings,'begin');
  IncIndent;
  try
    Fmt:='F%s:='+Finders[fmoRequireFields in FieldMapOpts.FieldMapOptions]+'(%s);';
    For I:=0 to Fields.Count-1 Do
      begin
      F:=Fields[i];
      If F.Enabled then
        AddLn(Strings,Fmt,[F.PropertyName,CreateString(F.FieldName)]);
      end;
  Finally
    DecIndent;
  end;
end;


{ TGenFieldMapOptions }

function TGenFieldMapOptions.GetMapAncestorName: String;
begin
  Result:=AncestorClass;
end;

function TGenFieldMapOptions.GetMapName: String;
begin
  Result:=ObjectClassName;
end;

procedure TGenFieldMapOptions.SetMapAncestorName(const AValue: String);
begin
  AncestorClass:=AValue;
end;

procedure TGenFieldMapOptions.SetMapClassName(const AValue: String);
begin
  ObjectClassName:=AValue;
end;

constructor TGenFieldMapOptions.Create;
begin
  inherited Create;
  MapClassName:='TMyObjectMap';
  MapAncestorName:='TFieldMap';
end;

procedure TGenFieldMapOptions.Assign(ASource: TPersistent);

Var
  O : TGenFieldMapOptions;

begin
  if ASource is TGenFieldMapOptions then
    begin
    O:=ASource as TGenFieldMapOptions;
    MapClassName:=O.MapClassName;
    MapAncestorName:=O.MapAncestorName;
    Options:=O.Options;
    end;
  inherited Assign(ASource);
end;

Initialization
  RegisterCodeGenerator('FieldMap','TFieldMap descendent.',TDDDBFieldMapCodeGenerator);

Finalization
  UnRegisterCodeGenerator(TDDDBFieldMapCodeGenerator);
end.

