{$IFNDEF FPC_DOTTEDUNITS}
unit fpcgfieldmap;
{$ENDIF FPC_DOTTEDUNITS}
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

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, Data.CodeGen.Base;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fpddcodegen;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TGenFieldMapOptions }
  TFieldMapOption = (fmoPublicFields,fmoRequireFields,fmoLoadObject,fmoCreateParamMap,fmoSaveObject,fmoOverrideTransformString,fmoDefineArray,fmoDefineList);
  TListParent = (lpFPList,lpList,lpObjectList,lpFPObjectList,lpGenericList,lpFGLGenericList);
  
  TFieldMapOptions = Set of TFieldMapOption;

  TGenFieldMapOptions = Class(TClassCodeGeneratorOptions)
  Private
    FListParent: TListParent;
    FOptions: TFieldMapOptions;
    FMapClassName : String;
    FMapAncestorClassName : String;
    FParamMapClassName : String;
    FParamMapAncestorClassName : String;
  Protected
    function GetMapAncestorName: String; virtual;
    function GetMapName: String; virtual;
    procedure SetMapAncestorName(const aValue: String); virtual;
    procedure SetMapClassName(const aValue: String); virtual;
    function GetParamMapAncestorName: String;virtual;
    function GetParamMapName: String;virtual;
    procedure SetParamMapAncestorName(const aValue: String); virtual;
    procedure SetParamMapClassName(const aValue: String); virtual;
  Public
    Constructor Create; override;
    Procedure Assign(ASource: TPersistent); override;
    Property MapAncestorName : String Read GetMapAncestorName Write SetMapAncestorName;
    Property MapClassName : String Read GetMapName Write SetMapClassName;
    Property ParamMapAncestorName : String Read GetParamMapAncestorName Write SetParamMapAncestorName;
    Property ParamMapClassName : String Read GetParamMapName Write SetParamMapClassName;
    Property ListParent : TListParent Read FListParent Write FlistParent;
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
    function GetListParent: string; virtual;
    function GetListParentUnit: String; virtual;
    procedure CreateObjectListDeclaration(Strings: TStrings; const aObjectClassName: string);virtual;
    procedure CreateObjectListImplementation(Strings: TStrings; const aObjectClassName: string);virtual;
    procedure AddTransFormOverrideDeclarations(Strings: TStrings); virtual;
    procedure AddTransFormOverrideImplementations(Strings: TStrings; MapClassName: string); virtual;
    procedure WriteFillMethod(Strings: TStrings; const ObjectClassName, MapClassName: String); virtual;
    procedure WriteSaveMethod(Strings: TStrings; const ObjectClassName, MapClassName: String); virtual;
    procedure DoCreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String); virtual;
    procedure DoCreateParamMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String); virtual;
    procedure WriteMapInitFields(Strings: TStrings; const ObjectClassName,   MapClassName: String); virtual;
    procedure WriteParamMapInitParams(Strings: TStrings; const ObjectClassName,   MapClassName: String); virtual;
    procedure CreateFieldMapImplementation(Strings: TStrings; const ObjectClassName, MapClassName: String);
    procedure CreateParamMapImplementation(Strings: TStrings; const ObjectClassName, MapClassName: String);
    procedure CreateObjectArrayDeclaration(Strings: TStrings; const aObjectClassName: string);
    Property FieldMapOpts : TGenFieldMapOptions Read Getopt;
  Public
    Class function NeedsFieldDefs: Boolean; override;
    procedure CreateFieldMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String);
    procedure CreateParamMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String); virtual;
  end;

  { TGenFieldMapCodeGenOptions }

  TGenFieldMapCodeGenOptions = class(TGenFieldMapOptions)
  Public
    constructor create; override;
  Published
    Property AncestorClass;
    Property MapClassName;
    Property MapAncestorName;
    Property ParamMapClassName;
    Property ParamMapAncestorName;
    Property ListParent;
  end;

  TDDDBFieldMapCodeGenerator = Class(TDDBaseFieldMapCodeGenerator)
  private
  Protected
    Function CreateOptions : TCodeGeneratorOptions; override;
    Procedure DoGenerateInterface(Strings: TStrings); override;
    Procedure DoGenerateImplementation(Strings: TStrings); override;
  Public
    Property FieldMapOpts;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.TypInfo;
{$ELSE FPC_DOTTEDUNITS}
uses typinfo;
{$ENDIF FPC_DOTTEDUNITS}

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

procedure TDDBaseFieldMapCodeGenerator.CreateObjectArrayDeclaration(Strings : TStrings; const aObjectClassName : string);

begin
  IncIndent;
  AddLn(Strings,'%sArray = Array of %s;',[aObjectClassName,aObjectClassName]);
  Addln(Strings,'');
  DecIndent;
end;

function TDDBaseFieldMapCodeGenerator.GetListParent : string;

begin
  case FieldMapOpts.ListParent of
    lpList,
    lpGenericList : Result:='TList';
    lpFGLGenericList :Result:='TFPGList';
    lpFPList : Result:='TFPList';
    lpFPObjectList : Result:='TFPObjectList';
    lpObjectList : Result:='TObjectList';
  end;
end;

function GetListClass(const aObjectClassName : string) : string;

begin
  Result:=aObjectClassName+'List';
end;

procedure TDDBaseFieldMapCodeGenerator.CreateObjectListDeclaration(Strings : TStrings; const aObjectClassName : string);

var
  lListClassName,lListParent : String;

begin
  IncIndent;
  lListParent:=GetListParent;
  lListClassName:=GetListClass(aObjectClassName);
  Addln(Strings,'{ %s }',[lListClassName]);
  AddLn(Strings);
  if FieldMapOpts.ListParent=lpGenericList then
    AddLn(Strings,'%s = specialize %s<%s>;',[lListClassName,lListParent,aObjectClassName])
  else
    begin
    AddLn(Strings,'%s = Class(%s)',[lListClassName,lListParent]);
    AddLn(Strings,'Private');
    IncIndent;
    AddLn(Strings,'Function _GetObj(const aIndex : Integer) : %s;',[aObjectClassName]);
    AddLn(Strings,'Procedure _SetObj(const aIndex : Integer; const aValue : %s);',[aObjectClassName]);
    DecIndent;
    AddLn(Strings,'Public');
    IncIndent;
    AddLn(Strings,'Property Objects[aIndex : Integer] : %s Read _GetObj Write _SetObj; default;',[aObjectClassName]);
    DecIndent;
    AddLn(Strings,'end;');
    end;
  DecIndent;
end;

procedure TDDBaseFieldMapCodeGenerator.CreateObjectListImplementation(Strings : TStrings; const aObjectClassName : string);

var
  S,lListClass : String;

begin
  lListClass:=aObjectClassName+'List';
  if FieldMapOpts.ListParent=lpGenericList then
    Exit; // nothing to do.
  S:=Format('Function %s._GetObj(const aIndex : Integer) : %s;',[lListClass,aObjectClassName]);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  IncIndent;
  AddLn(Strings,'Result:=%s(Items[aIndex]);',[aObjectClassName]);
  DecIndent;
  EndMethod(Strings,S);
  S:=Format('Procedure %s._SetObj(const aIndex : Integer; const aValue : %s);',[lListClass,aObjectClassName]);
  BeginMethod(Strings,S);
  AddLn(Strings,'begin');
  IncIndent;
  AddLn(Strings,'Items[aIndex]:=aValue;');
  DecIndent;
  EndMethod(Strings,S);
end;

function TDDBaseFieldMapCodeGenerator.GetListParentUnit : String;

begin
  Case FieldMapOpts.ListParent of
    lpFPObjectList,
    lpObjectList: Result:='contnrs';
    lpGenericList : Result:='Generics.Collections';
    lpFGLGenericList : Result:='fgl';
  else
    Result:='';
  end;
end;

function TDDBaseFieldMapCodeGenerator.GetInterfaceUsesClause: string;

Var
  ListUnit : String;

begin
  Result:=inherited GetInterfaceUsesClause;
  If (Result<>'') then
    Result:=Result+', db, fieldmap';
  if fmoDefineList in FieldMapOpts.FieldMapOptions then
    begin
    ListUnit:=GetListParentUnit;
    if ListUnit<>'' then
      Result:=Result+', '+ListUnit;
    end;
end;


procedure TDDDBFieldMapCodeGenerator.DoGenerateInterface(Strings: TStrings);
begin
  inherited DoGenerateInterface(Strings);
  AddLn(Strings,'Type');
  if fmoDefineArray in FieldMapOpts.FieldMapOptions then
    CreateObjectArrayDeclaration(Strings,GetOpt.ObjectClassName);
  if fmoDefineList in FieldMapOpts.FieldMapOptions then
    CreateObjectListDeclaration(Strings,GetOpt.ObjectClassName);
  CreatefieldMapDeclaration(Strings,GetOpt.ObjectClassName,GetOpt.MapClassName,GetOpt.MapAncestorName);
  if fmoCreateParamMap in GetOpt.FieldMapOptions then
    CreateParamMapDeclaration(Strings,GetOpt.ObjectClassName,GetOpt.ParamMapClassName,GetOpt.ParamMapAncestorName);
end;

procedure TDDDBFieldMapCodeGenerator.DoGenerateImplementation(Strings: TStrings
  );
begin
  inherited DoGenerateImplementation(Strings);
  if fmoDefineList in FieldMapOpts.FieldMapOptions then
    CreateObjectListImplementation(Strings,GetOpt.ObjectClassName);
  With FieldMapOpts do
    CreateFieldMapImplementation(Strings,ObjectClassName,MapClassName);
  if fmoCreateParamMap in GetOpt.FieldMapOptions then
    CreateParamMapImplementation(Strings,GetOpt.ObjectClassName,GetOpt.ParamMapClassName);
end;

Function TDDDBFieldMapCodeGenerator.CreateOptions : TCodeGeneratorOptions; 

begin
  Result:=TGenFieldMapCodeGenOptions.Create

end;

function TDDBaseFieldMapCodeGenerator.CreateOptions: TCodeGeneratorOptions;
begin
  Result:=TGenFieldMapOptions.Create;
end;

procedure TDDBaseFieldMapCodeGenerator.AddTransFormOverrideDeclarations(Strings : TStrings);

  Procedure Decl(aType : string);
  begin
    AddLn(Strings,'function TransFormString(const aString: %s) : %s; override;',[aType,aType]);
  end;

begin
  AddLn(Strings,'Protected');
  IncIndent;
  Decl('RawByteString');
  Decl('UnicodeString');
  Decl('WideString');
  DecIndent;
end;

procedure TDDBaseFieldMapCodeGenerator.AddTransFormOverrideImplementations(Strings : TStrings; MapClassName : string);

  Procedure Decl(aType : string);

  var
    S : String;

  begin
    S:=Format('function %s.TransFormString(const aString: %s) : %s; ',[MapClassName,aType,atype]);
    BeginMethod(Strings,S);
    AddLn(Strings,'begin');
    IncIndent;
    AddLn(Strings,'Result:=aString;');
    DecIndent;
    EndMethod(Strings,S);
  end;

begin
  Decl('RawByteString');
  Decl('UnicodeString');
  Decl('WideString');
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
  if fmoOverrideTransformString in FieldMapOpts.FieldMapOptions then
    AddTransFormOverrideDeclarations(Strings);
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

procedure TDDBaseFieldMapCodeGenerator.DoCreateParamMapDeclaration(
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
        AddLn(Strings,'F%s : TParam;',[F.PropertyName]);
      end;
  Finally
    DecIndent;
  end;
  if fmoOverrideTransformString in FieldMapOpts.FieldMapOptions then
    AddTransFormOverrideDeclarations(Strings);
  AddLn(Strings,'Public');
  IncIndent;
  Try
    AddLn(Strings,'Procedure InitParams; Override;');
    if fmoLoadObject in  FieldMapOpts.FieldMapOptions then
      begin
      AddLn(Strings,'Procedure Save(aObject: %s); virtual;',[ObjectClassName]);
      AddLn(Strings,'Procedure SaveObject(aObject: TObject); override;');
      end;
    if fmoPublicFields in  FieldMapOpts.FieldMapOptions then
      For I:=0 to Fields.Count-1 do
        begin
        F:=Fields[I];
        If F.Enabled then
          AddLn(Strings,'Property %s : TParam read F%s;',[F.PropertyName,F.FieldName]);
        end;
  Finally
    DecIndent;
  end;
end;


procedure TDDBaseFieldMapCodeGenerator.CreateParamMapDeclaration(Strings: TStrings; const ObjectClassName,MapClassName, MapAncestorName: String);

begin
  Addln(Strings);
  IncIndent;
  try
    Addln(Strings,'{ %s }',[MapClassName]);
    Addln(Strings);
    Addln(Strings,'%s = Class(%s)',[MapClassName,MapAncestorName]);
    DoCreateParamMapDeclaration(Strings,ObjectClassName,MapClassName,MapAncestorName);
    AddLn(Strings,'end;');
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
  if fmoOverrideTransformString in FieldMapOpts.FieldMapOptions then
    AddTransFormOverrideImplementations(Strings,MapClassName);
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

procedure TDDBaseFieldMapCodeGenerator.CreateParamMapImplementation(
  Strings: TStrings; const ObjectClassName, MapClassName: String);

Var
  S : String;

begin
  AddLn(Strings,' { %s }',[MapClassName]);
  AddLn(Strings);
  S:=Format('Procedure %s.InitParams;',[MapClassName]);
  BeginMethod(Strings,S);
  Try
    WriteParamMapInitParams(Strings,ObjectClassName,MapClassName);
  Finally
    EndMethod(Strings,S);
  end;
  if fmoOverrideTransformString in FieldMapOpts.FieldMapOptions then
    AddTransFormOverrideImplementations(Strings,MapClassName);
  if fmoLoadObject in FieldMapOpts.FieldMapOptions then
    begin
    WriteSaveMethod(Strings, ObjectClassName, MapClassName);
    S:=Format('Procedure %s.SaveObject(aObject: TObject);',[MapClassName]);
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

procedure TDDBaseFieldMapCodeGenerator.WriteSaveMethod(Strings: TStrings; const ObjectClassName, MapClassName: String);

Const
  SAddLoadCode = '// Add code to save property %s (of type %s) to field %s';

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
  S:=Format('Procedure %s.Save(aObject: %s);',[MapClassName,ObjectClassName]);
  BeginMethod(Strings,S);
  Try
    Addln(Strings,'begin');
    IncIndent;
    Fmt:='SetParam(Self.F%s,%s);';
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

procedure TDDBaseFieldMapCodeGenerator.WriteParamMapInitParams(Strings: TStrings;
  const ObjectClassName, MapClassName: String);

Const
  Finders : Array[Boolean] of string = ('FindParam','ParamByName');

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

function TGenFieldMapOptions.GetParamMapAncestorName: String;
begin
  Result:=FParamMapAncestorClassName;
  if Result='' then
    Result:='TParamMap';
end;

function TGenFieldMapOptions.GetParamMapName: String;
begin
  Result:=FParamMapClassName;
  if Result='' then
    Result:=ObjectClassName+'ParamMap';
end;

procedure TGenFieldMapOptions.SetParamMapAncestorName(const aValue: String);
begin
  FParamMapAncestorClassName:=aValue;
end;

procedure TGenFieldMapOptions.SetParamMapClassName(const aValue: String);
begin
  FParamMapClassName:=aValue;
end;

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

procedure TGenFieldMapOptions.SetMapAncestorName(const aValue: String);
begin
  FMapAncestorClassName:=aValue;
end;

procedure TGenFieldMapOptions.SetMapClassName(const aValue: String);
begin
  FMapClassName:=aValue;
end;

constructor TGenFieldMapOptions.Create;
begin
  inherited Create;
  AncestorClass:='TObject';
  ObjectClassName:='TMyObject';

  // The rest is auto generated if empty
end;


procedure TGenFieldMapOptions.Assign(aSource: TPersistent);

Var
  O : TGenFieldMapOptions;

begin
  if ASource is TGenFieldMapOptions then
    begin
    O:=ASource as TGenFieldMapOptions;
    FMapClassName:=O.FMapClassName;
    FMapAncestorClassName:=O.FMapAncestorClassName;
    FParamMapClassName:=O.FParamMapClassName;
    FParamMapAncestorClassName:=O.FParamMapAncestorClassName;
    FieldMapOptions:=O.FieldMapOptions;
    FListParent:=O.ListParent;
    end;
  inherited Assign(ASource);
end;

Initialization
  RegisterCodeGenerator('FieldMap','Object and TFieldMap descendent.',TDDDBFieldMapCodeGenerator);

Finalization
  UnRegisterCodeGenerator(TDDDBFieldMapCodeGenerator);
end.

