unit fpcgfieldmap;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    Fieldmap implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpddcodegen;

Type

  { TGenFieldMapOptions }
  TFieldMapOption = (fmoPublicFields,fmoRequireFields,fmoLoadObject);
  TFieldMapOptions = Set of TFieldMapOption;

  TGenFieldMapOptions = Class(TClassCodeGeneratorOptions)
  Private
    FOptions: TFieldMapOptions;
    FMapClassName : String;
    FMapAncestorClassName : String;
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
    Property AncestorClass;
  Published
    Property FieldMapOptions : TFieldMapOptions Read FOptions Write FOptions;
  end;

  { TDDDBFieldMapCodeGenerator }

  { TDDBaseFieldMapCodeGenerator }

  TDDBaseFieldMapCodeGenerator = Class(TDDClassCodeGenerator)
  private
    function GetOpt: TGenFieldMapOptions;
  Protected
    // Overrides;
    Function GetInterfaceUsesClause : string; override;
    Function CreateOptions : TCodeGeneratorOptions; override;
    // New methods
    procedure WriteFillMethod(Strings: TStrings; const ObjectClassName, MapClassName: String); virtual;
    procedure DoCreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String); virtual;
    procedure WriteMapInitFields(Strings: TStrings; const ObjectClassName,   MapClassName: String); virtual;
    procedure CreateFieldMapImplementation(Strings: TStrings; const ObjectClassName, MapClassName: String);
    Property FieldMapOpts : TGenFieldMapOptions Read Getopt;
  Public
    Class function NeedsFieldDefs: Boolean; override;
    procedure CreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String);
  end;

  { TGenFieldMapCodeGenOptions }

  TGenFieldMapCodeGenOptions = class(TGenFieldMapOptions)
  Public
    constructor create; override;
  Published
    Property AncestorClass;
    Property MapClassName;
    Property MapAncestorName;
  end;

  TDDDBFieldMapCodeGenerator = Class(TDDBaseFieldMapCodeGenerator)
  Protected
    Function CreateOptions : TCodeGeneratorOptions; override;
    Procedure DoGenerateInterface(Strings: TStrings); override;
    Procedure DoGenerateImplementation(Strings: TStrings); override;
  Public
    Property FieldMapOpts;
  end;

implementation

uses typinfo;

{ TGenFieldMapCodeGenOptions }

constructor TGenFieldMapCodeGenOptions.create;
begin
  inherited create;
  FieldMapOptions:=[fmoLoadObject]
end;

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
  CreatefieldMapDeclaration(Strings,GetOpt.ObjectClassName,GetOpt.MapClassName,GetOpt.MapAncestorName);
end;

procedure TDDDBFieldMapCodeGenerator.DoGenerateImplementation(Strings: TStrings
  );
begin
  inherited DoGenerateImplementation(Strings);
  With FieldMapOpts do
    CreateFieldMapImplementation(Strings,ObjectClassName,MapClassName);
end;

Function TDDDBFieldMapCodeGenerator.CreateOptions : TCodeGeneratorOptions; 

begin
  Result:=TGenFieldMapCodeGenOptions.Create

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
    if fmoLoadObject in  FieldMapOpts.FieldMapOptions then
      begin
      AddLn(Strings,'Procedure Fill(aObject: %s); virtual;',[ObjectClassName]);
      AddLn(Strings,'Procedure LoadObject(aObject: TObject); override;');
      end;
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
  if fmoLoadObject in FieldMapOpts.FieldMapOptions then
    begin
    WriteFillMethod(Strings, ObjectClassName, MapClassName);
    S:=Format('Procedure %s.LoadObject(aObject: TObject);',[MapClassName]);
    BeginMethod(Strings,S);
    Try
      Addln(Strings,'begin');
      IncIndent;
      AddLn(Strings,'Fill(aObject as %s);',[ObjectClassName]);
      DecIndent;
    finally
      EndMethod(Strings,S);
    end;
    end;
end;

class function TDDBaseFieldMapCodeGenerator.NeedsFieldDefs: Boolean;
begin
  Result:=True;
end;

procedure TDDBaseFieldMapCodeGenerator.WriteFillMethod(Strings: TStrings; const ObjectClassName, MapClassName: String);

Const
  SAddLoadCode = '// Add code to load property %s (of type %s) from field %s';

  SupportedPropTypes =  [ptBoolean, // Boolean
                         ptShortString, ptAnsiString, ptUtf8String, // Ansistring
                         ptWord,ptByte,ptLongint,ptCardinal,ptSmallInt,ptShortInt, // Integer
                         ptCurrency, // Currency
                         ptDateTime // DateTime
                         ];

Var
  S,Fmt : String;
  F : TFieldPropDef;
  I : Integer;

begin
  S:=Format('Procedure %s.Fill(aObject: %s);',[MapClassName,ObjectClassName]);
  BeginMethod(Strings,S);
  Try
    Addln(Strings,'begin');
    IncIndent;
    Fmt:='%s:=GetFromField(Self.F%s,%s);';
    Addln(Strings,'With aObject do');
    IncIndent;
    Addln(Strings,'begin');
    For I:=0 to Fields.Count-1 Do
      begin
      F:=Fields[i];
      If F.PropertyType in SupportedPropTypes then
        AddLn(Strings,Fmt,[F.PropertyName,F.PropertyName,F.PropertyName])
      else if F.PropertyType in [ptWideString, ptUnicodeString] then
        begin
        AddLn(Strings,'If Assigned(Self.F%s) then',[F.PropertyName]);
        incIndent;
        AddLn(Strings,'%s:=F%s.AsUnicodeString;',[F.PropertyName,F.PropertyName]);
        DecIndent;
        end
      else if F.PropertyType in [ptSingle,ptDouble,ptExtended,ptComp] then
        begin
        AddLn(Strings,'If Assigned(Self.F%s) then',[F.PropertyName]);
        incIndent;
        AddLn(Strings,'%s:=Self.F%s.AsFloat;',[F.PropertyName,F.PropertyName]);
        DecIndent;
        end
      else if F.PropertyType in [ptInt64,ptQWord] then
        begin
        AddLn(Strings,'If Assigned(Self.F%s) then',[F.PropertyName]);
        incIndent;
        AddLn(Strings,'%s:=Self.F%s.AsLargeInt;',[F.PropertyName,F.PropertyName]);
        DecIndent;
        end
      else
        AddLn(Strings,SAddLoadCode,[F.PropertyName,GetEnumName(TypeInfo(TPropType),Ord(F.PropertyType)), F.FieldName]);
      end;
    Addln(Strings,'end;');
    DecIndent;
  Finally
    DecIndent;
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
  Result:=FMapAncestorClassName;
  if Result='' then 
    Result:='TFieldMap';
end;

function TGenFieldMapOptions.GetMapName: String;
begin
  Result:=FMapClassName;
  if Result='' then
    Result:=ObjectClassName+'Map';
end;

procedure TGenFieldMapOptions.SetMapAncestorName(const AValue: String);
begin
  FMapAncestorClassName:=AValue;
end;

procedure TGenFieldMapOptions.SetMapClassName(const AValue: String);
begin
  FMapClassName:=AValue;
end;

constructor TGenFieldMapOptions.Create;
begin
  inherited Create;
  AncestorClass:='TObject';
  ObjectClassName:='TMyObject';
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
    FieldMapOptions:=O.FieldMapOptions;
    end;
  inherited Assign(ASource);
end;

Initialization
  RegisterCodeGenerator('FieldMap','Object and TFieldMap descendent.',TDDDBFieldMapCodeGenerator);

Finalization
  UnRegisterCodeGenerator(TDDDBFieldMapCodeGenerator);
end.

