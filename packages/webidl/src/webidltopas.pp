
{
    This file is part of the Free Component Library

    WEBIDL to pascal code converter
    Copyright (c) 2021 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit webidltopas;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Contnrs, WebIdl.Parser, WebIdl.Scanner, WebIdl.Defs, Pascal.CodeGenerator;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, contnrs, WebIDLParser, WebIDLScanner, WebIDLDefs, pascodegen;
{$ENDIF FPC_DOTTEDUNITS}

Const
  SDefaultGetterName = 'GetDefault';
  SDefaultSetterName = 'SetDefault';


Type
  TPascalNativeType = (
    ntUnknown, // unknown
    ntNone,    // None -> void
    ntError,   // Special : error condition
    ntBoolean,
    ntShortInt,
    ntByte,
    ntSmallInt,
    ntWord,
    ntLongint,
    ntCardinal,
    ntInt64,
    ntQWord,
    ntSingle,
    ntDouble,
    ntUnicodeString,
    ntUTF8String,
    ntVariant,
    ntObject,
    ntInterface,
    ntArray,
    ntMethod);
  TPascalNativeTypes = Set of TPascalNativeType;

  { TPasData }

  TPasData = Class(TObject)
  private
    FPasName: String;
  Public
    IDL: TIDLBaseObject;
    Line, Column: integer;
    SrcFile: string;
    Resolved: TIDLTypeDefinition;
    NativeType : TPascalNativeType;
    NameChecked : Boolean;
    ChromeChecked : Boolean;
    FullMemberList : TIDLDefinitionList;
    ParentsMemberList : TIDLDefinitionList;
    Used : Boolean;
    Constructor Create(APasName: String; D: TIDLBaseObject);
    Destructor Destroy; override;
    Property PasName: String read FPasName write FPasName;
  end;
  TPasDataClass = class of TPasData;

  TBaseConversionOption = (
    coAddOptionsToHeader,
    coExpandUnionTypeArgs,
    coDictionaryAsClass,
    coChromeWindow,
    coOnlyUsed,
    coPrivateMethods
    );
  TBaseConversionOptions = Set of TBaseConversionOption;

const
  BaseConversionOptionName: array[TBaseConversionOption] of string = (
    'AddOptionsToHeader',
    'ExpandUnionTypeArgs',
    'DictionaryAsClass',
    'ChromeWindow',
    'OnlyUsed',
    'PrivateMethods'
    );
  NativeTypeNames : Array [TPascalNativeType] of String = (
    '',
    '',
    '',   // Special : error condition
    'Boolean',
    'ShortInt',
    'Byte',
    'SmallInt',
    'Word',
    'LongInt',
    'Cardinal',
    'Int64',
    'QWord',
    'Single',
    'Double',
    'UnicodeString',
    'UTF8String',
    'Variant',
    'Object',
    'Interface',
    'Array',
    'Method');

type

  { TBaseWebIDLToPas }

  TBaseWebIDLToPas = Class(TPascalCodeGenerator)
  private
    FArrayPrefix: String;
    FArraySuffix: String;
    FAutoTypes: TStrings;
    FBaseOptions: TBaseConversionOptions;
    FClassPrefix: String;
    FClassSuffix: String;
    FContext: TWebIDLContext;
    FDictionaryClassParent: String;
    FFieldPrefix: String;
    FGeneratingImplementation: Boolean;
    FGlobalVars: TStrings;
    FInputStream: TStream;
    FOutputStream: TStream;
    FTypePrefix: String;
    FGetterPrefix: String;
    FIncludeImplementationCode: TStrings;
    FIncludeInterfaceCode: TStrings;
    FInputFileName: String;
    FUsedDefs,
    FGlobalDefs: TFPObjectHashTable;
    FOutputFileName: String;
    FPasDataClass: TPasDataClass;
    FPasNameList: TFPObjectList; // list TPasData
    FSetterPrefix: String;
    FTypeAliases: TStrings; // user defined type maping name to name
    FVerbose: Boolean;
    FWebIDLVersion: TWebIDLVersion;
    function CreateCallBackFromInterface(aDef: TIDLInterfaceDefinition): TIDLCallBackDefinition;
    function GetUsed(D: TIDLDefinition): Boolean;
    function InUsedList(D: TIDLDefinition): Boolean;
    procedure ResolveCallbackInterfaces;
    procedure SetGlobalVars(const AValue: TStrings);
    procedure SetIncludeImplementationCode(AValue: TStrings);
    procedure SetIncludeInterfaceCode(AValue: TStrings);
    procedure SetOutputFileName(const AValue: String);
    procedure SetTypeAliases(AValue: TStrings);
  Protected
    function CheckExistingSequence(ST: TIDLSequenceTypeDefDefinition; out TN: TIDLString): Boolean;
    function CheckExistingUnion(UT: TIDLUnionTypeDefDefinition; out TN: TIDLString): Boolean;
    function GetAliasPascalType(aNativeTypeName: String; out PascalTypeName: string): TPascalNativeType;
    procedure TrimList(List: TStrings); virtual;
    procedure AddOptionsToHeader;
    Procedure Parse; virtual;
    Procedure WritePascal; virtual;
    function CreateParser(aContext: TWebIDLContext; S: TWebIDLScanner): TWebIDLParser; virtual;
    function CreateScanner(S: TStream): TWebIDLScanner; virtual;
    Function CreateContext: TWebIDLContext; virtual;
    // Auxiliary routines
    function CheckChromeOnly(D: TIDLDefinition): Boolean;
    function MarkUsed(D: TIDLDefinition; ParentIsUsed: Boolean): Boolean;
    procedure MarkUsedDefinitions(aList: TIDLDefinitionList; ParentIsUsed: Boolean);
    procedure PropagateChromeOnly(aList: TIDLDefinitionList);
    procedure AddFullMemberList(aParent: TIDLStructuredDefinition; AddToList: TIDLDefinitionList);
    function GetFullMemberList(aParent: TIDLStructuredDefinition): TIDLDefinitionList;
    function GetParentsMemberList(aParent: TIDLStructuredDefinition): TIDLDefinitionList;
    procedure GetOptions(L: TStrings; Full: boolean); virtual;
    procedure ProcessDefinitions; virtual;
    function CreatePasData(aName: String; aNativetype : TPascalNativeType; D: TIDLBaseObject; Escape: boolean): TPasData; virtual;
    function ClonePasData(Data: TPasData; OwnerDef: TIDLBaseObject): TPasData; virtual;
    procedure AddGlobalJSIdentifier(D: TIDLDefinition); virtual;
    procedure ResolveParentInterfaces(aList: TIDLDefinitionList); virtual;
    procedure ResolveParentInterface(Intf: TIDLInterfaceDefinition); virtual;
    procedure ResolveParentInterface(Intf: TIDLDictionaryDefinition); virtual;
    procedure ResolveTypeDefs(aList: TIDLDefinitionList); virtual;
    procedure ResolveTypeDef(D: TIDLDefinition); virtual;
    procedure RemoveInterfaceForwards(aList: TIDLDefinitionList); virtual;
    Function ConvertDef(D : TIDLDefinition) : Boolean;
    function FindGlobalDef(const aName: UTF8String): TIDLDefinition; virtual;
    function GetDefPos(Def: TIDLBaseObject; WithoutFile: boolean = false): string; virtual;
    function GetPasDataPos(D: TPasData; WithoutFile: boolean = false): string; virtual;
    // Pascal Name allocation/retrieval
    function AddSequenceDef(ST: TIDLSequenceTypeDefDefinition): Boolean; virtual;
    function AddUnionDef(UT: TIDLUnionTypeDefDefinition): Boolean; virtual;
    procedure EnsureUniqueNames(aParent : TIDLStructuredDefinition; ML: TIDLDefinitionList; const aParentName: String); virtual;
    procedure EnsureUniqueArgNames(Intf: TIDLStructuredDefinition); virtual;
    procedure AllocatePasNames(aList: TIDLDefinitionList; ParentName: String=''); virtual;
    function AllocatePasName(D: TIDLDefinition; ParentName: String; Recurse : Boolean): TPasData; virtual;
    function GetAliasPascalType(D: TIDLDefinition; out PascalTypeName : string): TPascalNativeType; virtual;
    function AllocateArgumentPasName(D: TIDLArgumentDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateAttributePasName(aParent : TIDLStructuredDefinition; D: TIDLAttributeDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateCallbackPasName(D: TIDLCallBackDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateDefaultPasName(D: TIDLDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateDictionaryMemberPasName(D: TIDLDictionaryMemberDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateDictionaryPasName(D: TIDLDictionaryDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateFunctionPasName(D: TIDLFunctionDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateInterfacePasName(D: TIDLInterfaceDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateNamespacePasName(D: TIDLNameSpaceDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateSequencePasName(D: TIDLSequenceTypeDefDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocatePromisePasName(D: TIDLPromiseTypeDefDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateUnionPasName(D: TIDLUnionTypeDefDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateMapLikePasName(D: TIDLMapLikeDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateEnumeratedPasName(D: TIDLEnumDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;
    function AllocateConstPasName(D: TIDLConstDefinition; ParentName: String; Recurse: Boolean): TPasData; virtual;

    function GetPasName(ADef: TIDLDefinition): String; virtual;
    function GetPasNativeType(ADef: TIDLDefinition): TPascalNativeType; virtual;
    function GetPasNativeTypeAndName(ADef: TIDLDefinition; out aPascalName : String): TPascalNativeType; virtual;
    function GetPasClassName(const aName: string): string; overload; virtual;
    function IDLToPascalNativeType(const aTypeName: String): TPascalNativetype; virtual;
    function GetPascalTypeAndName(Const aTypeName: String; Out aPascalName : String): TPascalNativeType; overload; virtual;
    function GetPascalTypeName(Const aTypeName: String; ForTypeDef: Boolean=False): String; overload; virtual;
    function GetPascalTypeName(aTypeDef: TIDLTypeDefDefinition; ForTypeDef: Boolean=False): String;
    function GetJSTypeName(aTypeDef: TIDLTypeDefDefinition): String; overload; virtual;
    function GetResolvedType(aDef: TIDLTypeDefDefinition; out PascalNativeType: TPascalNativeType; out aTypeName, aResolvedTypename: string): TIDLTypeDefinition; overload; virtual;
    function ConstructSequenceTypeName(Seq: TIDLSequenceTypeDefDefinition; ForTypeDef: Boolean=False): string; virtual;
    function GetInterfaceDefHead(Intf: TIDLInterfaceDefinition): String; virtual;
    function GetNamespaceDefHead(Intf: TIDLNamespaceDefinition): String; virtual;
    function GetDictionaryDefHead(const CurClassName: string; Dict: TIDLDictionaryDefinition): String; virtual;
    function CheckUnionTypeDefinition(D: TIDLDefinition): TIDLUnionTypeDefDefinition; virtual;
    Function CloneArgument(Arg: TIDLArgumentDefinition): TIDLArgumentDefinition; virtual;
    procedure AddArgumentToOverloads(aList: TFPObjectlist; aName, aPasName, aTypeName: String; PosEl: TIDLBaseObject); overload; virtual;
    procedure AddArgumentToOverloads(aList: TFPObjectlist; aDef: TIDLArgumentDefinition); overload; virtual;
    procedure AddUnionOverloads(aList: TFPObjectlist; aName, aPasName: String;  UT: TIDLUnionTypeDefDefinition); virtual;
    procedure AddOverloads(aList: TFPObjectlist; aDef: TIDLFunctionDefinition; aIdx: Integer); virtual;
    function CloneNonPartialArgumentList(aList: TFPObjectlist; ADest: TFPObjectlist= Nil; AsPartial: Boolean=True): integer; virtual;
    function GetOverloads(aDef: TIDLFunctionDefinition): TFPObjectlist; virtual;
    function GetArguments(aList: TIDLDefinitionList; ForceBrackets: Boolean): String; virtual;
    function HaveConsts(aList: TIDLDefinitionList): Boolean; virtual;
    // Code generation routines. Return the number of actually written defs.
    function WriteImplicitAutoType(aType: TIDLDefinition): Integer;
    function WriteFunctionImplicitTypes(aList: TIDLDefinitionList): Integer; virtual;
    function WriteAttributeImplicitTypes(aList: TIDLDefinitionList): Integer; virtual;
    function WriteDictionaryImplicitTypes(aList: TIDLDefinitionList): Integer; virtual;
    function WriteOtherImplicitTypes(Intf: TIDLStructuredDefinition; aMemberList: TIDLDefinitionList): Integer; virtual;
    function WriteDictionaryMemberImplicitTypes(aDict: TIDLDictionaryDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WriteDictionaryDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteForwardClassDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteInterfaceDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteNamespaceDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteMethodDefs(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WriteUtilityMethods(Intf: TIDLStructuredDefinition): Integer; virtual;
    function WriteTypeDefsAndCallbacks(aList: TIDLDefinitionList): Integer; virtual;
    function WriteEnumDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteConsts(aParent: TIDLDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WriteProperties(aParent: TIDLDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WritePlainFields(aParent: TIDLDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WriteDictionaryFields(aDict: TIDLDictionaryDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WritePrivateReadOnlyFields(aParent: TIDLDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WriteGetters(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WriteSetters(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; virtual;
    // Maplike-specific methods
    function WriteMapLikePrivateReadOnlyFields(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer; virtual;
    function WriteMapLikeMethodDefinitions(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition): integer; virtual;
    function WriteMapLikeProperties(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer; virtual;
    function WriteMapLikeGetters(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition): Integer; virtual;
    // Implementations. For webidl2pas, these are empty
    procedure WriteDefinitionImplementation(D: TIDLDefinition); virtual;
    procedure WriteTypeDefsAndCallbackImplementations(aList: TIDLDefinitionList); virtual;
    // Definitions. Return true if a definition was written.
    function WriteForwardClassDef(D: TIDLStructuredDefinition): Boolean; virtual;
    function WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition; aName: String = ''): Boolean; virtual;
    function WriteFunctionDefinition(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition): Boolean; virtual;
    function WriteTypeDef(aDef: TIDLTypeDefDefinition): Boolean; virtual;
    function WriteRecordDef(aDef: TIDLRecordDefinition): Boolean; virtual;
    function WriteEnumDef(aDef: TIDLEnumDefinition): Boolean; virtual;
    function WriteDictionaryField(aDict: TIDLDictionaryDefinition; aField: TIDLDictionaryMemberDefinition): Boolean; virtual;
    function WriteField(aAttr: TIDLAttributeDefinition): Boolean; virtual;
    function WriteConst(aConst: TIDLConstDefinition): Boolean ; virtual;
    function WriteInterfaceDef(Intf: TIDLInterfaceDefinition): Boolean; virtual;
    function WriteNamespaceDef(aNamespace: TIDLNamespaceDefinition): Boolean; virtual;
    function WriteDictionaryDef(aDict: TIDLDictionaryDefinition): Boolean; virtual;
    // Additional
    procedure WriteAliasTypeDef(aDef: TIDLTypeDefDefinition); virtual;
    procedure WritePromiseDef(aDef: TIDLPromiseTypeDefDefinition); virtual;
    procedure WriteSequenceDef(aDef: TIDLSequenceTypeDefDefinition); virtual;
    procedure WriteUnionDef(aDef: TIDLUnionTypeDefDefinition); virtual;
    // Extra interface/Implementation code.
    procedure WriteGlobalVar(aDef: String); virtual;
    procedure WriteNamespaceVars; virtual;
    procedure WriteGlobalVars;
    procedure WriteImplementation; virtual;
    procedure WriteIncludeInterfaceCode; virtual;
    Property Context: TWebIDLContext Read FContext;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure WriteOptions; virtual;
    procedure SetUsedList(aList : TStrings);
    function IsKeyWord(const S: String): Boolean; override;
    Property GeneratingImplementation : Boolean Read FGeneratingImplementation;
  Public
    Property InputFileName: String Read FInputFileName Write FInputFileName;
    Property InputStream: TStream Read FInputStream Write FInputStream;
    Property OutputFileName: String Read FOutputFileName Write SetOutputFileName;
    Property OutputStream: TStream Read FOutputStream Write FOutputStream;
    Property Verbose: Boolean Read FVerbose Write FVerbose;
    Property FieldPrefix: String Read FFieldPrefix Write FFieldPrefix;
    Property ClassPrefix: String Read FClassPrefix Write FClassPrefix;
    Property ClassSuffix: String Read FClassSuffix Write FClassSuffix;
    Property ArrayPrefix: String Read FArrayPrefix Write FArrayPrefix;
    Property ArraySuffix: String Read FArraySuffix Write FArraySuffix;
    Property GetterPrefix: String read FGetterPrefix write FGetterPrefix;
    Property SetterPrefix: String read FSetterPrefix write FSetterPrefix;
    Property TypePrefix: String read FTypePrefix write FTypePrefix;
    Property WebIDLVersion: TWebIDLVersion Read FWebIDLVersion Write FWebIDLVersion;
    Property TypeAliases: TStrings Read FTypeAliases Write SetTypeAliases;
    Property GlobalVars: TStrings Read FGlobalVars Write SetGlobalVars;
    Property IncludeInterfaceCode: TStrings Read FIncludeInterfaceCode Write SetIncludeInterfaceCode;
    Property IncludeImplementationCode: TStrings Read FIncludeImplementationCode Write SetIncludeImplementationCode;
    Property DictionaryClassParent: String Read FDictionaryClassParent Write FDictionaryClassParent;
    Property BaseOptions: TBaseConversionOptions read FBaseOptions write FBaseOptions;
    Property PasDataClass: TPasDataClass read FPasDataClass write FPasDataClass;
  end;

function BaseConversionOptionsToStr(Opts: TBaseConversionOptions): string;

Resourcestring
  SErrBeforeException = ' before an exception occurred';

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.TypInfo;
{$ELSE FPC_DOTTEDUNITS}
uses typinfo;
{$ENDIF FPC_DOTTEDUNITS}

function BaseConversionOptionsToStr(Opts: TBaseConversionOptions): string;
var
  o: TBaseConversionOption;
begin
  Result:='';
  for o in Opts do
    begin
    if Result<>'' then Result:=Result+',';
    Result:=Result+BaseConversionOptionName[o];
    end;
  Result:='['+Result+']';
end;

{ TPasData }

constructor TPasData.Create(APasName: String; D: TIDLBaseObject);
begin
  FPasName:=APasName;
  IDL:=D;
  SrcFile:=D.SrcFile;
  Line:=D.Line;
  Column:=D.Column;
end;

destructor TPasData.Destroy;
begin
  FreeAndNil(FullmemberList);
  FreeAndNil(ParentsMemberList);

  inherited Destroy;
end;

{ TBaseWebIDLToPas }

function TBaseWebIDLToPas.CreateContext: TWebIDLContext;
begin
  Result:=TWebIDLContext.Create(True);
end;

function TBaseWebIDLToPas.CreateScanner(S: TStream):  TWebIDLScanner;

begin
  Result:=TWebIDLScanner.Create(S);
end;

function TBaseWebIDLToPas.CreateParser(aContext: TWebIDLContext;S: TWebIDLScanner):  TWebIDLParser;

begin
  Result:=TWebIDLParser.Create(aContext,S);
  Result.Version:=FWebIDLVersion;
end;

procedure TBaseWebIDLToPas.Parse;

Var
  ms: TMemoryStream;
  S: TWebIDLScanner;
  P: TWebIDLParser;

begin
  P:=Nil;
  S:=Nil;
  ms:=TMemoryStream.Create;
  try
    if InputStream<>nil then
      ms.CopyFrom(InputStream,InputStream.Size-InputStream.Position)
    else
      ms.LoadFromFile(InputFileName);
    ms.Position:=0;
    S:=CreateScanner(ms);
    S.CurFile:=InputFileName;
    P:=CreateParser(Context,S);
    P.Parse;
  finally
    P.Free;
    S.Free;
    ms.Free;
  end;
end;


function TBaseWebIDLToPas.GetPasName(ADef: TIDLDefinition): String;

begin
  GetPasNativeTypeAndName(aDef,Result);
end;

function TBaseWebIDLToPas.GetPasNativeType(ADef: TIDLDefinition): TPascalNativeType;

var
  Dummy : String;

begin
  Result:=GetPasNativeTypeAndName(aDef,Dummy);
end;

function TBaseWebIDLToPas.GetPasNativeTypeAndName(ADef: TIDLDefinition; out aPascalName: String): TPascalNativeType;
begin
  aPascalName:='';
  Result:=ntUnknown;
  If Not Assigned(ADef) then
    raise EConvertError.CreateFmt('Attempt to get pascal name for empty definition',[Adef.GetNamePath]);
  if (ADef.Data is TPasData) then
    begin
    aPascalName:=TPasData(ADef.Data).PasName;
    Result:=TPasData(ADef.Data).NativeType;
    end
  else
    raise EConvertError.CreateFmt('No pascal data allocated for %s',[Adef.GetNamePath]);
end;

function TBaseWebIDLToPas.GetPasClassName(const aName: string): string;
begin
  if aName='' then
    raise EConvertError.Create('[20220725184209] empty name');
  Result:=ClassPrefix+aName+ClassSuffix;
end;

function TBaseWebIDLToPas.HaveConsts(aList: TIDLDefinitionList): Boolean;

Var
  D: TIDLDefinition;

begin
  Result:=False;
  For D in aList do
    if D is TIDLConstDefinition then
      if ConvertDef(D) then
        Exit(True);
end;

function TBaseWebIDLToPas.WriteFunctionImplicitTypes(aList: TIDLDefinitionList): Integer;

  procedure DoFunction(FD : TIDLFunctionDefinition);

  var
    D2,D3: TIDLDefinition;
    DA: TIDLArgumentDefinition absolute D2;
    UT: TIDLUnionTypeDefDefinition;

  begin
    if assigned(FD.ReturnType) then
      Result:=Result+WriteImplicitAutoType(FD.ReturnType);
    For D2 in FD.Arguments do
      begin
      WriteImplicitAutoType(DA.ArgumentType);
      UT:=CheckUnionTypeDefinition(DA.ArgumentType);
      if Assigned(UT) then
        For D3 in UT.Union do
          if (D3 is TIDLSequenceTypeDefDefinition) then
            if AddSequenceDef(D3 as TIDLSequenceTypeDefDefinition) then
              Inc(Result);
      end;
  end;

Var
  D : TIDLDefinition;

begin
  Result:=0;
  for D in aList do
    if ConvertDef(D) then
      if D is TIDLFunctionDefinition then
        DoFunction(TIDLFunctionDefinition(D))
      else if D is TIDLCallBackDefinition then
        DoFunction(TIDLCallBackDefinition(D).FunctionDef);
  if Result>0 then
    AddLn('');
end;

function TBaseWebIDLToPas.WriteImplicitAutoType(aType : TIDLDefinition) : Integer;


begin
  Result:=0;
  if (aType is TIDLSequenceTypeDefDefinition) then
    begin
    if AddSequenceDef(aType as TIDLSequenceTypeDefDefinition) then
      Inc(Result)
    end
  else if (aType is TIDLUnionTypeDefDefinition) then
    begin
    if AddUnionDef(aType as TIDLUnionTypeDefDefinition) then
      Inc(Result);
    end
end;



function TBaseWebIDLToPas.WriteAttributeImplicitTypes(aList: TIDLDefinitionList): Integer;
Var
  D: TIDLDefinition;
  FA: TIDLAttributeDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if D is TIDLAttributeDefinition then
      if ConvertDef(D) then
        Result:=Result+WriteImplicitAutoType(FA.AttributeType);
end;

function TBaseWebIDLToPas.WriteDictionaryImplicitTypes(aList: TIDLDefinitionList): Integer;
Var
  D: TIDLDefinition;
  MD : TIDLDictionaryMemberDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if D is TIDLDictionaryDefinition then
      if ConvertDef(D) then
        Result:=Result+WriteImplicitAutoType(MD.MemberType);
end;

function TBaseWebIDLToPas.WriteOtherImplicitTypes(
  Intf: TIDLStructuredDefinition; aMemberList: TIDLDefinitionList): Integer;
begin
  Result:=0;
  if Intf=nil then ;
  if aMemberList=nil then ;
end;

function TBaseWebIDLToPas.WriteDictionaryMemberImplicitTypes(
  aDict: TIDLDictionaryDefinition; aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  FD: TIDLDictionaryMemberDefinition absolute D;


begin
  Result:=0;
  if aDict=nil then ;
  for D in aList do
    if D is TIDLDictionaryMemberDefinition then
      if ConvertDef(D) then
        Result:=Result+WriteImplicitAutoType(FD.MemberType);
end;

function TBaseWebIDLToPas.WritePrivateReadOnlyFields(aParent: TIDLDefinition;
  aList: TIDLDefinitionList): Integer;
var
  D : TIDLDefinition;
  MD : TIDLMapLikeDefinition absolute D;

begin
  Result:=0;
  if aParent=nil then ;
  if aList=nil then ;
  for D in aList do
    if D is TIDLMapLikeDefinition then
      if ConvertDef(D) then
        Result:=Result+WriteMapLikePrivateReadOnlyFields(aParent,MD);
end;

function TBaseWebIDLToPas.WriteGetters(
  aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer;
var
  D : TIDLDefinition;
  MD : TIDLMapLikeDefinition absolute D;

begin
  Result:=0;
  if aParent=nil then ;
  if aList=nil then ;
  for D in aList do
    if D is TIDLMapLikeDefinition then
      if ConvertDef(D) then
        Result:=Result+WriteMapLikeGetters(aParent,MD);
end;

function TBaseWebIDLToPas.WriteSetters(
  aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer;
begin
  Result:=0;
  if aParent=nil then ;
  if aList=nil then ;
end;

function TBaseWebIDLToPas.WriteMapLikePrivateReadOnlyFields(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer;
begin
  if (aParent=Nil) and (aMap=Nil) then ; // Silence compiler warning
  Result:=1;
  AddLn('fsize : NativeInt; external name ''size'';');
end;

function TBaseWebIDLToPas.WriteProperties(aParent: TIDLDefinition;
  aList: TIDLDefinitionList): Integer;

var
  D : TIDLDefinition;
  MD : TIDLMapLikeDefinition absolute D;

begin
  Result:=0;
  if aParent=nil then ;
  if aList=nil then ;
  for D in aList do
    if D is TIDLMapLikeDefinition then
      if ConvertDef(D) then
        Result:=Result+WriteMapLikeProperties(aParent,MD);
end;

function TBaseWebIDLToPas.WriteMapLikeProperties(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer;

begin
  if (aParent=Nil) and (aMap=nil) then ; // Silence compiler warning
  AddLn('property size : NativeInt read fsize;');
  Result:=1;
end;

function TBaseWebIDLToPas.WriteMapLikeGetters(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition): Integer;
begin
  if (aParent<>Nil) and (aMap<>Nil) then;
  Result:=0;
  // AddLn('function _Getsize: NativeInt;');
  // Result:=1;
end;

function TBaseWebIDLToPas.WriteConst(aConst: TIDLConstDefinition): Boolean;
var
  S: UTF8String;
begin
  Result:=true;
  S:=aConst.Value;
  if aConst.ConstType=ctInteger then
    S:=StringReplace(S,'0x','$',[]);
  Addln('%s = %s;',[GetPasName(aConst),S])
end;

function TBaseWebIDLToPas.WriteConsts(aParent: TIDLDefinition;
  aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;

begin
  if aParent=nil then ;
  EnsureSection(csConst);
  Indent;
  Result:=0;
  For D in aList do
    if D is TIDLConstDefinition then
      if ConvertDef(D) then
        if WriteConst(D as TIDLConstDefinition) then
          Inc(Result);
  Undent;
end;

function TBaseWebIDLToPas.WritePlainFields(aParent: TIDLDefinition;
  aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  A: TIDLAttributeDefinition absolute D;

begin
  if aParent=nil then ;
  EnsureSection(csDeclaration);
  Result:=0;
  For D in aList do
    if D is TIDLAttributeDefinition then
      if ConvertDef(D) then
        if Not (aoReadOnly in A.Options) then
          if WriteField(A) then
            Inc(Result);
end;

function TBaseWebIDLToPas.WriteDictionaryField(aDict: TIDLDictionaryDefinition;
  aField: TIDLDictionaryMemberDefinition): Boolean;

Var
  Def,N,TN: String;

begin
  Result:=True;
  if aDict=nil then ;
  N:=GetPasName(aField);
  TN:=GetPasName(aField.MemberType);
  if TN='record' then
    TN:='TJSObject';
  if SameText(N,TN) then
    N:='_'+N;
  Def:=Format('%s: %s;',[N,TN]);
  if (N<>aField.Name) then
    Def:=Def+Format('external name ''%s'';',[aField.Name]);
  AddLn(Def);
end;

function TBaseWebIDLToPas.WriteDictionaryFields(
  aDict: TIDLDictionaryDefinition; aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  M: TIDLDictionaryMemberDefinition absolute D;

begin
  Indent;
  Result:=0;
  For D in aList do
    if D is TIDLDictionaryMemberDefinition then
      if ConvertDef(D) then
        if WriteDictionaryField(aDict,M) then
          Inc(Result);
  Undent;
end;

function TBaseWebIDLToPas.WriteMethodDefs(aParent: TIDLStructuredDefinition;
  aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  FD: TIDLFunctionDefinition absolute D;
  MD: TIDLMapLikeDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if ConvertDef(D) then
      if D is TIDLFunctionDefinition then
        begin
        if Not (foCallBack in FD.Options) then
           if WriteFunctionDefinition(aParent,FD) then
             Inc(Result);
        end
      else if D is TIDLMaplikeDefinition then
        Result:=Result+WriteMapLikeMethodDefinitions(aParent,MD);
end;

function TBaseWebIDLToPas.GetParentsMemberList(aParent: TIDLStructuredDefinition) : TIDLDefinitionList;

var
  D : TPasData;

begin
  D:=TPasData(aParent.Data);
  if Not Assigned(D) then
    Raise EWebIDLError.CreateFmt('%s does not have data assigned to it',[aParent]);
  if Not Assigned(D.ParentsMemberList) then
    begin
    D.ParentsMemberList:=TIDLDefinitionList.Create(aParent,False);
    While aParent<>Nil do
      begin
      AddFullMemberList(aParent,D.ParentsmemberList);
      if aParent is TIDLInterfaceDefinition then
        aParent:=TIDLInterfaceDefinition(aParent).ParentInterface
      else if aParent is TIDLDictionaryDefinition then
        aParent:=TIDLDictionaryDefinition(aParent).ParentDictionary
      else
        aParent:=Nil;
      end;
    end;
  Result:=D.ParentsmemberList;
end;

procedure TBaseWebIDLToPas.AddFullMemberList(aParent: TIDLStructuredDefinition; AddToList : TIDLDefinitionList);

Var
  List : TIDLDefinitionList;
  D : TIDLDefinition;

begin
  List:=GetFullMemberList(AParent);
  For D in List do
    addToList.Add(D);
end;

function TBaseWebIDLToPas.GetFullMemberList(aParent: TIDLStructuredDefinition) : TIDLDefinitionList;

var
  D : TPasData;

begin
  D:=TPasData(aParent.Data);
  if Not Assigned(D) then
    Raise EWebIDLError.CreateFmt('%s does not have data assigned to it',[aParent]);
  if Not Assigned(D.FullmemberList) then
    begin
    D.FullmemberList:=TIDLDefinitionList.Create(aParent,False);
    aParent.GetFullMemberList(D.FullmemberList);
    end;
  Result:=D.FullmemberList;
end;

function TBaseWebIDLToPas.WriteMapLikeMethodDefinitions(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition): integer;

var
  D1,KeyType,ValueType : String;
  lReadOnly : Boolean;
  L : TIDLDefinitionList;
  KNT,VNT : TPascalNativeType;

begin
  Result:=0;
  GetResolvedType(aMap.KeyType,KNT,D1,KeyType);
  GetResolvedType(aMap.ValueType,VNT,D1,ValueType);
//  KeyType:=GetResolName();
//  ValueType:=GetName(aMap.ValueType);
  lReadOnly:=aMap.IsReadonly;
  L:=GetFullMemberList(aParent);
  if Not L.HasName('get') then
    AddLn('function get(key: %s) : %s;',[KeyType,ValueType]);
  if Not L.HasName('has') then
  AddLn('function has(key: %s) : Boolean;',[KeyType]);
  if Not L.HasName('entries') then
    AddLn('function entries : IJSIterator;');
  if Not L.HasName('keys') then
    AddLn('function keys : IJSIterator;');
  if Not L.HasName('values') then
    AddLn('function values : IJSIterator;');
  Inc(Result,5);
  if not lReadOnly then
    begin
    if Not L.HasName('set') then
      AddLn('procedure set_(key: %s; value : %s);',[KeyType,ValueType]);
    if Not L.HasName('clear') then
      AddLn('procedure clear;');
    if Not L.HasName('delete') then
      AddLn('procedure delete(key: %s);',[KeyType]);
    Inc(Result,3);
    end;
end;

function TBaseWebIDLToPas.WriteUtilityMethods(Intf: TIDLStructuredDefinition
  ): Integer;
begin
  Result:=0;
  if Intf=nil then ;
end;

function TBaseWebIDLToPas.CheckExistingSequence(ST: TIDLSequenceTypeDefDefinition; out TN: TIDLString): Boolean;

var
  ArgTypeName,ArgResolvedTypeName : String;
  NT : TPascalNativeType;

begin
  GetResolvedType(ST,NT,ArgTypeName,ArgResolvedTypeName);
  TN:=ArgTypeName;
  Result:=FAutoTypes.IndexOf(TN)<>-1;
end;

function TBaseWebIDLToPas.CheckExistingUnion(UT: TIDLUnionTypeDefDefinition; out TN: TIDLString): Boolean;

var
  ArgTypeName,ArgResolvedTypeName : String;
  NT : TPascalNativeType;

begin
  GetResolvedType(UT,NT,ArgTypeName,ArgResolvedTypeName);
  TN:=ArgTypeName;
  Result:=FAutoTypes.IndexOf(TN)<>-1;
end;


function TBaseWebIDLToPas.AddSequenceDef(ST: TIDLSequenceTypeDefDefinition
  ): Boolean;
var
  TN : TIDLString;
begin
  Result:=Not CheckExistingSequence(ST,TN);
  if Result then
    begin
    FAutoTypes.Add(TN);
    if Verbose then
      DoLog('Automatically adding %s sequence definition for %s.',[TN,GetDefPos(ST)]);
    WriteSequenceDef(ST);
    end;
end;

function TBaseWebIDLToPas.AddUnionDef(UT: TIDLUnionTypeDefDefinition): Boolean;
var
  TN : TIDLString;
begin
  Result:=Not CheckExistingUnion(UT,TN);
  if Result then
    begin
    FAutoTypes.Add(TN);
    if Verbose then
      DoLog('Automatically adding %s sequence definition for %s.',[TN,GetDefPos(UT)]);
    WriteUnionDef(UT);
    end;
end;

procedure TBaseWebIDLToPas.EnsureUniqueNames(aParent : TIDLStructuredDefinition;ML: TIDLDefinitionList;const aParentName : String);

Var
  L: TFPObjectHashTable;


  Function CanRename(Def: TIDLDefinition) : Boolean;

  var
    isStringifier : Boolean;
    IsIterable : Boolean;

  begin
    IsStringifier:=(Def.Name='') and (Def is TIDLAttributeDefinition) and (aoStringifier in TIDLAttributeDefinition(Def).Options);
    isIterable:=(Def is TIDLIterableDefinition);
    Result:=not (IsStringifier or isIterable);
  end;

  Procedure CheckRename(Def: TIDLDefinition);

  var
    I: integer;
    OrigType : TPascalNativeType;
    OrigName,BaseName,NewName: String;
    IsOverload: Boolean;
    CurDef , ConflictDef: TIDLDefinition;

  begin
    OrigType:=GetPasNativeTypeAndName(Def,OrigName);
    BaseName:=LowerCase(OrigName);
    NewName:=BaseName;
    I:=0;
    IsOverload:=False;
    ConflictDef:=nil;
    Repeat
      CurDef:=TIDLDefinition(L.Items[NewName]);
      if (CurDef<>Nil) then
        // Overloads
        begin
        IsOverload:=((CurDef is TIDLFunctionDefinition) and (Def is TIDLFunctionDefinition));
        if IsOverload then
          CurDef:=Nil
        else
          begin
          ConflictDef:=CurDef;
          inc(I);
          if I>1 then
            raise EConvertError.CreateFmt('[20220725172221] Duplicate identifier %s at (%s) and (%s)',[BaseName,GetDefPos(Def),GetDefPos(CurDef)]);
          NewName:=KeywordPrefix+BaseName+KeywordSuffix;
          OrigName:=KeywordPrefix+OrigName+KeywordSuffix;
          end;
        end;
    Until (CurDef=Nil);
    if (BaseName<>NewName) then
      begin
      BaseName:=GetPasName(Def);
      if Verbose then
        DoLog('Renaming duplicate identifier (%s) %s at %s to %s, other at %s',[Def.ClassName,BaseName,GetDefPos(Def),OrigName,GetDefPos(ConflictDef)]);
      // Original TPasName is in list, will be freed automatically
      Def.Data:=CreatePasData(OrigName,OrigType,Def,False);
      end;
    if not IsOverload then
      L.Add(NewName,Def);
  end;

var
  D: TIDLDefinition;
begin
  if (aParent=Nil) and (aParentname='') then ; // Silence compiler warning
  L:=TFPObjectHashTable.Create(False);
  try
    For D in ML Do
      if ConvertDef(D) then
        if CanRename(D) and not (D is TIDLConstDefinition) then
          CheckRename(D);
    For D in ML Do
      if ConvertDef(D) then
        if CanRename(D) and (D is TIDLConstDefinition) then
          CheckRename(D);
  finally
    L.Free;
  end;
end;

procedure TBaseWebIDLToPas.EnsureUniqueArgNames(Intf: TIDLStructuredDefinition);
var
  Names: TFPObjectHashTable;

  procedure CheckRenameArgs(Func: TIDLFunctionDefinition);
  var
    i: Integer;
    Arg: TIDLArgumentDefinition;
    ArgName: String;
    ConflictDef: TIDLDefinition;
    D : TPasData;

  begin
    for i:=0 to Func.Arguments.Count-1 do
      begin
      Arg:=Func.Argument[i];
      D:=TPasData(Arg.Data);
      if D=Nil then
        Raise EWebIDLError.CreateFmt('Function %s argument %s does not have pascal data assigned',[Func.Name,Arg.Name]);
      if not D.NameChecked then
        begin
        ArgName:=GetPasName(Arg);
        ArgName:='a'+Uppercase(ArgName[1])+copy(ArgName,2,length(ArgName));
        repeat
          ConflictDef:=TIDLDefinition(Names.Items[LowerCase(ArgName)]);
          if (ConflictDef=Nil) then break;
          // name conflict -> rename
          ArgName:='_'+ArgName;
        until false;
        D.PasName:=ArgName;
        D.NameChecked:=True;
        end;
      end;
  end;

var
  Members, MembersWithParents: TIDLDefinitionList;
  D: TIDLDefinition;
  CurName: String;
begin
  Members:=GetFullMemberList(Intf);
  MembersWithParents:=GetParentsMemberList(Intf);
  Names:=TFPObjectHashTable.Create(False);
  try
    For D in MembersWithParents Do
      if ConvertDef(D) then
        begin
        CurName:=LowerCase(GetPasName(D));
        if Names.Items[CurName]=nil then
          Names.Add(CurName,D);
        end;
    For D in Members Do
      if D is TIDLFunctionDefinition then
        if ConvertDef(D) then
          CheckRenameArgs(TIDLFunctionDefinition(D));
  finally
    Names.Free;
  end;
end;

function TBaseWebIDLToPas.WriteInterfaceDef(Intf: TIDLInterfaceDefinition): Boolean;

Var
  aClassName: String;
  Decl: String;
  ML: TIDLDefinitionList;

begin
  Result:=True;
  ML:=GetFullMemberList(Intf);
  EnsureUniqueNames(Intf,ML,Intf.Name);
  EnsureUniqueArgNames(Intf);
  aClassName:=GetPasName(Intf);
  // class comment
  ClassComment(aClassName);
  // sub types
  WriteFunctionImplicitTypes(ML);
  WriteAttributeImplicitTypes(ML);
  WriteOtherImplicitTypes(Intf,ML);
  // class and ancestor
  Decl:=aClassName+' = '+GetInterfaceDefHead(Intf);
  AddLn(Decl);
  PushSection(csUnknown);
  // private section
  AddLn('Private');
  Indent;
  WritePrivateReadOnlyFields(Intf,ML);
  if Not (coPrivateMethods in BaseOptions) then
    begin
    Undent;
    AddLn('Protected');
    Indent;
    end;
  WriteGetters(Intf,ML);
  WriteSetters(Intf,ML);
  Undent;
  // write public section
  AddLn('Public');
  if HaveConsts(ML) then
    begin
    Indent;
    WriteConsts(Intf,ML);
    Undent;
    AddLn('Public');
    end;
  Indent;
  WritePlainFields(Intf,ML);
  WriteMethodDefs(Intf,ML);
  WriteUtilityMethods(Intf);
  WriteProperties(Intf,ML);
  PopSection;
  Undent;
  AddLn('end;');
end;

function TBaseWebIDLToPas.WriteNamespaceDef(aNamespace: TIDLNamespaceDefinition): Boolean;

Var
  aClassName: String;
  Decl: String;
  ML: TIDLDefinitionList;

begin
  Result:=True;
  ML:=GetFullMemberList(aNamespace);
  EnsureUniqueNames(aNameSpace,ML,aNameSpace.name);
  EnsureUniqueArgNames(aNamespace);
  aClassName:=GetPasName(aNamespace);
  // class comment
  ClassComment(aClassName);
  // sub types
  WriteFunctionImplicitTypes(ML);
  WriteAttributeImplicitTypes(ML);
  WriteOtherImplicitTypes(aNameSpace,ML);
  // class and ancestor
  Decl:=aClassName+' = '+GetNamespaceDefHead(aNamespace);
  AddLn(Decl);
  // private section
  AddLn('Private');
  Indent;
  WritePrivateReadOnlyFields(aNamespace,ML);
  if not (coPrivateMethods in BaseOptions) then
    begin
    Undent;
    AddLn('Protected');
    Indent;
    end;
  WriteGetters(aNamespace,ML);
  WriteSetters(aNamespace,ML);
  Undent;
  // write public section
  AddLn('Public');
  if HaveConsts(ML) then
    begin
    Indent;
    PushSection(csUnknown);
    WriteConsts(aNamespace,ML);
    PopSection;
    Undent;
    AddLn('Public');
    end;
  Indent;
  WriteMethodDefs(aNamespace,ML);
  WriteUtilityMethods(aNamespace);
  WriteProperties(aNamespace,ML);
  Undent;
  AddLn('end;');
end;

function TBaseWebIDLToPas.WriteDictionaryDef(aDict: TIDLDictionaryDefinition): Boolean;

Var
  CurClassName, Decl: String;
  DefList: TIDLDefinitionList;
begin
  Result:=True;
  DefList:=GetParentsMemberList(aDict);
  CurClassName:=GetPasName(aDict);
  ClassComment(CurClassName);
  WriteDictionaryMemberImplicitTypes(aDict, DefList);
  // class and ancestor
  Decl:=GetDictionaryDefHead(CurClassName,aDict);
  AddLn(Decl);
  WriteDictionaryFields(aDict,DefList);
  AddLn('end;');
end;

constructor TBaseWebIDLToPas.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  WebIDLVersion:=v2;
  FieldPrefix:='F';
  ClassPrefix:='T';
  ClassSuffix:='';
  ArrayPrefix:='T';
  ArraySuffix:='DynArray';
  GetterPrefix:='Get';
  SetterPrefix:='Set';
  TypePrefix:='T';
  FTypeAliases:=TStringList.Create;
  FGlobalVars:=TStringList.Create;
  FPasNameList:=TFPObjectList.Create(True);
  FPasDataClass:=TPasData;
  FAutoTypes:=TStringList.Create;
  FIncludeInterfaceCode:=TStringList.Create;
  FIncludeImplementationCode:=TStringList.Create;
  FGlobalDefs:=TFPObjectHashTable.Create(False);
end;


destructor TBaseWebIDLToPas.Destroy;
begin
  FreeAndNil(FUsedDefs);
  FreeAndNil(FGlobalDefs);
  FreeAndNil(FIncludeInterfaceCode);
  FreeAndNil(FIncludeImplementationCode);
  FreeAndNil(FAutoTypes);
  FreeAndNil(FGlobalVars);
  FreeAndNil(FTypeAliases);
  FreeAndNil(FPasNameList);
  inherited Destroy;
end;

procedure TBaseWebIDLToPas.WriteTypeDefsAndCallbackImplementations(aList : TIDLDefinitionList);

begin
  if aList<>Nil then;
  // Do nothing
end;

procedure TBaseWebIDLToPas.WriteImplementation;

Var
  S: String;
  D : TIDLDefinition;
  Cnt : Integer;
  OK : Boolean;
  Msg : String;

begin
  FGeneratingImplementation:=True;
  Msg:='';
  if Verbose then
    DoLog('Writing implementation section');
  Addln('');
  For S in FIncludeImplementationCode do
    Addln(S);
  Addln('');
  WriteTypeDefsAndCallbackImplementations(Context.Definitions);
  OK:=False;
  Cnt:=0;
  try
    For D in Context.Definitions do
      begin
      inc(Cnt);
      if ConvertDef(D) then
        if not ((D is TIDLStructuredDefinition) and (TIDLStructuredDefinition(D).IsPartial)) then
          WriteDefinitionImplementation(D);
      end;
    OK:=True;
  finally
    if not OK then
      Msg:=SErrBeforeException;
    if Verbose then
      DoLog('Wrote %d of %d definitions%s',[Cnt,Context.Definitions.Count,Msg]);
  end;
  FGeneratingImplementation:=False;
end;

procedure TBaseWebIDLToPas.WriteDefinitionImplementation(D: TIDLDefinition);

begin
  if Assigned(D) then;
end;

function TBaseWebIDLToPas.GetJSTypeName(aTypeDef: TIDLTypeDefDefinition): String;

begin
  if assigned(aTypeDef) then
    Result:=aTypeDef.GetJSTypeName
  else
    Result:='';
end;

function TBaseWebIDLToPas.GetPascalTypeName(aTypeDef: TIDLTypeDefDefinition; ForTypeDef: Boolean = False): String;

begin
  Result:=GetPascalTypeName(GetJSTypeName(aTypeDef),ForTypeDef)
end;

function TBaseWebIDLToPas.GetResolvedType(aDef: TIDLTypeDefDefinition; out PascalNativeType : TPascalNativeType; out aTypeName, aResolvedTypename: string): TIDLTypeDefinition;
begin
  Result:=nil;
  aTypeName:='';
  aResolvedTypename:='';
  if aDef=nil then
    exit;
  PascalNativeType:=GetPasNativeTypeAndName(aDef,aTypeName);
  //writeln('TBaseWebIDLToPas.GetResolvedType START aDef=',aDef.Name,':',aDef.ClassName,' ',aDef.TypeName,' ',GetDefPos(aDef),' Resolved=',(aDef.Data is TPasData) and (TPasData(aDef.Data).Resolved<>nil));
  Result:=aDef;
  while (aDef.Data is TPasData) and (TPasData(aDef.Data).Resolved<>nil) do
    begin
    Result:=TPasData(aDef.Data).Resolved;
    //writeln('TBaseWebIDLToPas.GetResolvedType RESOLVED Result=',Result.Name,' ',GetDefPos(Result));
    if not (Result is TIDLTypeDefDefinition) then
      break;
    if Result=aDef then
      break;
    aDef:=TIDLTypeDefDefinition(Result);
    end;
  if Result is TIDLTypeDefDefinition then
    aResolvedTypename:=GetPascalTypeName(TIDLTypeDefDefinition(Result))
  else
    aResolvedTypename:=GetPasName(Result);
end;

function TBaseWebIDLToPas.ConstructSequenceTypeName(
  Seq: TIDLSequenceTypeDefDefinition; ForTypeDef: Boolean): string;
begin
  Result:=GetPasName(Seq.ElementType);
  if Result='' then
    Result:=GetPascalTypeName(Seq.ElementType,ForTypeDef);
  if (Result='') then
    begin
    if ForTypeDef then
      raise EConvertError.Create('[20220725172227] sequence without name at '+GetDefPos(Seq));
    Result:=GetPasName(Seq);
    end;
  if LeftStr(Result,length(ArrayPrefix))<>ArrayPrefix then
    Result:=ArrayPrefix+Result;
  Result:=Result+ArraySuffix;
end;

function TBaseWebIDLToPas.GetInterfaceDefHead(Intf: TIDLInterfaceDefinition
  ): String;
begin
  Result:='class';
  if Intf=nil then ;
end;

function TBaseWebIDLToPas.GetNamespaceDefHead(Intf: TIDLNamespaceDefinition): String;
begin
  Result:='class';
  if Intf=nil then ;
end;

function TBaseWebIDLToPas.GetDictionaryDefHead(const CurClassName: string; Dict: TIDLDictionaryDefinition): String;
var
  CurParent: String;
begin
  if Dict=nil then ;
  if (coDictionaryAsClass in BaseOptions) then
    begin
    CurParent:=DictionaryClassParent;
    if CurParent='' then
      CurParent:='TJSObject';
    Result:='class('+CurParent+')'
    end
  else
    Result:='record';
  Result:=CurClassName+' = '+Result;
end;

function TBaseWebIDLToPas.IDLToPascalNativeType(const aTypeName: String) : TPascalNativetype;

begin
  Case aTypeName of
    'boolean': Result:=ntBoolean;

    'byte': Result:=ntShortInt;
    'octet': Result:=ntByte;
    'short': Result:=ntSmallInt;
    'unsigned short': Result:=ntWord;
    'long': Result:=ntLongint;
    'unsigned long': Result:=ntCardinal;
    'long long': Result:=ntInt64;
    'unsigned long long': Result:=ntQWord;

    'float',
    'unrestricted float': Result:=ntSingle;
    'double',
    'unrestricted double' : Result:=ntDouble;
    'union',
    'any': Result:=ntVariant;

    'DOMString',
    'USVString',
    'ByteString': Result:=ntUnicodeString;
    'UTF8String' : Result:=ntUtf8String;

    'record',
    'object': result:=ntObject; // Result:=GetPasClassName('Object');

    'Error',
    'DOMException': result:=ntError; // Result:=GetPasClassName('Error');

    'Int8Array',
    'Int16Array',
    'Int32Array',
    'Uint8Array',
    'Uint16Array',
    'Uint32Array',
    'Uint8ClampedArray',
    'Float32Array',
    'Float64Array' : Result:=ntArray;

    'ArrayBuffer',
    'ArrayBufferView',
    'DataView',
    'Document',
    'DocumentFragment',
    'Node': Result:=ntObject; // Result:=GetPasClassName(aTypeName);
    'undefined',
    'void': Result:=ntNone; // Result:=aTypeName;
  else
    Result:=ntUnknown;
  end;

end;

function TBaseWebIDLToPas.GetPascalTypeAndName(const aTypeName: String; out aPascalName: String): TPascalNativeType;

Var
  A: UTF8String;
  D: TIDLDefinition;
  P: Integer;

begin
  Result:=IDLToPascalNativeType(aTypeName);
  Case Result of
  ntObject:
    begin
    Case aTypeName of
      'ArrayBuffer',
      'ArrayBufferView',
      'DataView',
      'Document',
      'DocumentFragment',
      'Node': aPascalName:=GetPasClassName(aTypeName);
    else
      aPascalName:=GetPasClassName('Object')
    end;
    end;
  ntArray:
    begin
    Case aTypeName of
    'DataView',
    'Int8Array',
    'Int16Array',
    'Int32Array',
    'Uint8Array',
    'Uint16Array',
    'Uint32Array',
    'Uint8ClampedArray',
    'Float32Array',
    'Float64Array' : aPascalName:=GetPasClassName(aTypeName);
    end;
    end;
  ntError:
    aPascalName:=GetPasClassName('Error');
  ntUnknown:
    begin
    a:=aTypeName;
    D:=FindGlobalDef(aTypeName);
    if D=Nil then
      D:=FContext.FindDefinition(aTypeName);
    if (D<>Nil) and (D.Data<>Nil) then
      Result:=GetPasNativeTypeAndName(D,aPascalName)
    else
      begin
      A:=FTypeAliases.Values[aTypeName];
      If (A<>'') then
        begin
        aPascalName:=A;
        P:=Pos(',',A);
        if P>0 then
          SetLength(aPascalName,P-1);
        Result:=GetAliasPascalType(aTypeName,aPascalName);
        end;
      end;
    end;
  else
    aPascalName:=NativeTypeNames[Result];
  end;
end;

function TBaseWebIDLToPas.GetPascalTypeName(const aTypeName: String; ForTypeDef: Boolean): String;

begin
  if ForTypeDef then; // Silence compiler warning
  GetPascalTypeAndName(aTypeName,Result);
end;

function TBaseWebIDLToPas.WriteField(aAttr: TIDLAttributeDefinition): Boolean;
begin
  Result:=false;
  if aAttr=nil then ;
end;

function TBaseWebIDLToPas.WriteForwardClassDef(D: TIDLStructuredDefinition): Boolean;

begin
  Result:=not D.IsPartial;
  if Result then
    AddLn('%s = class;',[GetPasName(D)]);
end;

function TBaseWebIDLToPas.WriteForwardClassDefs(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;

begin
  Result:=0;
  Comment('Forward class definitions');
  For D in aList do
    if (D is TIDLInterfaceDefinition) or (D is TIDLNamespaceDefinition) then
      if ConvertDef(D) then
        begin
        if WriteForwardClassDef(D as TIDLStructuredDefinition) then
          Inc(Result);
        end;
  if coDictionaryAsClass in BaseOptions then
    For D in aList do
      if D is TIDLDictionaryDefinition then
        if ConvertDef(D) then
          if WriteForwardClassDef(D as TIDLDictionaryDefinition) then
            Inc(Result);
end;

procedure TBaseWebIDLToPas.WriteSequenceDef(aDef: TIDLSequenceTypeDefDefinition);


begin
  Addln('%s = array of %s;',[GetPasName(aDef),GetPascalTypeName(aDef.ElementType)])
end;


procedure TBaseWebIDLToPas.WriteUnionDef(aDef: TIDLUnionTypeDefDefinition);

Var
  aLine,S: UTF8String;
  D: TIDLDefinition;
begin
  S:='';
  For D in adef.Union do
    begin
    if (S<>'') then
      S:=S+', ';
    S:=S+(D as TIDLTypeDefDefinition).TypeName;
    end;
  Comment('Union of '+S);
  aLine:=GetPasName(aDef)+' = '+GetPascalTypeName('any')+';';
  AddLn(aLine);
end;


procedure TBaseWebIDLToPas.WriteGlobalVar(aDef : String);

var
  P : Integer;
  VarName, VarType: String;

begin
  P:=Pos('=',aDef);
  VarName:=Trim(Copy(aDef,1,P-1));
  VarType:=Trim(Copy(aDef,P+1));
  AddLn(VarName+': '+VarType+';');
end;

procedure TBaseWebIDLToPas.WriteGlobalVars;
var
  i: Integer;
begin
  if (GlobalVars.Count=0) and Not Context.HaveNamespaces  then
    exit;
  AddLn('var');
  Indent;
  for i:=0 to GlobalVars.Count-1 do
    begin
    WriteGlobalvar(GlobalVars[i]);
    end;
  WriteNamespaceVars;
  Undent;
end;

procedure TBaseWebIDLToPas.WriteNamespaceVars;

var
  i: Integer;
  VarName, VarType: String;

begin
  for I:=0 to Context.Definitions.Count-1 do
    if Context.Definitions[i] is TIDLNamespaceDefinition then
      begin
      VarName:=Context.Definitions[i].Name;
      VarType:=GetPasName(Context.Definitions[i]);
      AddLn(VarName+': '+VarType+';');
      end;
end;

procedure TBaseWebIDLToPas.WritePromiseDef(aDef: TIDLPromiseTypeDefDefinition);

begin
  if aDef<>Nil then;
  // AddLn(GetName(aDef)+' = '+ClassPrefix+'Promise'+ClassSuffix+';');
end;

procedure TBaseWebIDLToPas.WriteAliasTypeDef(aDef: TIDLTypeDefDefinition);

Var
  TN: String;

begin
  TN:=GetPascalTypeName(aDef,True);
  AddLn('%s = %s;',[GetPasName(aDef),TN]);
end;

function TBaseWebIDLToPas.WriteTypeDef(aDef: TIDLTypeDefDefinition): Boolean;

var
  TN : TIDLString;

begin
  Result:=(TypeAliases.IndexOfName(aDef.Name)=-1);
  if not Result then
    exit;
  if ADef is TIDLSequenceTypeDefDefinition then
    begin
    if not CheckExistingSequence(aDef as TIDLSequenceTypeDefDefinition,TN) then
      begin
      FAutoTypes.Add(TN);
      WriteSequenceDef(aDef as TIDLSequenceTypeDefDefinition);
      end;
    end
  else if ADef is TIDLUnionTypeDefDefinition then
    WriteUnionDef(aDef as TIDLUnionTypeDefDefinition)
  else if ADef is TIDLPromiseTypeDefDefinition then
    WritePromiseDef(aDef as TIDLPromiseTypeDefDefinition)
  else if ADef is TIDLRecordDefinition then
    WriteRecordDef(aDef as TIDLRecordDefinition)
  else
    WriteAliasTypeDef(aDef);
end;

function TBaseWebIDLToPas.WriteRecordDef(aDef: TIDLRecordDefinition): Boolean;

Var
  KT,VT: String;

begin
  Result:=True;
  KT:=GetPascalTypeName(aDef.KeyType);
  VT:=GetPascalTypeName(aDef.ValueType);
  AddLn('%s = Class(TJSObject)',[GetPasName(aDef)]);
  AddLn('private');
  Indent;
  AddLn('function GetValue(aKey: %s): %s; external name ''[]'';',[KT,VT]);
  AddLn('procedure SetValue(aKey: %s; const AValue: %s); external name ''[]'';',[KT,VT]);
  Undent;
  AddLn('public');
  Indent;
  AddLn('property Values[Name: %s]: %s read GetProperties write SetProperties; default;',[KT,VT]);
  Undent;
  AddLn('end;');
end;

function TBaseWebIDLToPas.WriteTypeDefsAndCallbacks(aList: TIDLDefinitionList): Integer;

const
  SimpleTypes = [ntError, ntBoolean, ntShortInt, ntByte, ntSmallInt, ntWord, ntLongint, ntCardinal,
                 ntInt64, ntQWord, ntSingle, ntDouble, ntUnicodeString,  ntUTF8String, ntVariant];

Var
  D: TIDLDefinition;
  TD: TIDLTypeDefDefinition absolute D;
  CD: TIDLCallbackDefinition absolute D;
begin
  Result:=0;
  EnsureSection(csType);
  // Better would be to sort the definitions on dependency.
  // Simple typedefs
  for D in aList do
    if D is TIDLTypeDefDefinition then
      begin
      if ConvertDef(D) then
        if GetPasNativeType(TD) in SimpleTypes then
          if WriteTypeDef(TD)  then
            Inc(Result);
      end;
  // Complex typedefs and callbacks (which can reference typedefs);
  for D in aList do
    if D is TIDLTypeDefDefinition then
        begin
        if ConvertDef(D) then
          if Not (GetPasNativeType(TD) in SimpleTypes) then
            if WriteTypeDef(TD)  then
              Inc(Result);
        end
    else if D is TIDLCallbackDefinition then
      begin
        if ConvertDef(D) then
          if WriteFunctionTypeDefinition(CD.FunctionDef,GetPasName(CD)) then
           Inc(Result);
      end;
end;

function TBaseWebIDLToPas.WriteEnumDef(aDef: TIDLEnumDefinition): Boolean;

begin
  Result:=True;
  AddLn('%s = String;',[GetPasName(aDef)]);
end;

function TBaseWebIDLToPas.WriteEnumDefs(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  ED: TIDLEnumDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLEnumDefinition then
      if ConvertDef(D) then
        if WriteEnumDef(ED) then
          Inc(Result);
end;

function TBaseWebIDLToPas.GetArguments(aList: TIDLDefinitionList; ForceBrackets: Boolean): String;

Var
  I, ArgType: TIDLDefinition;
  Arg: TIDLArgumentDefinition absolute I;
  NT : TPascalNativeType;
  ArgName, ArgTypeName, ArgResolvedTypeName: string;

begin
  Result:='';
  For I in aList do
    begin
    ArgName:=GetPasName(Arg);
    if IsKeyWord(ArgName) then
      ArgName:=ArgName+'_';
    ArgType:=GetResolvedType(Arg.ArgumentType,NT,ArgTypeName,ArgResolvedTypeName);
    ArgName:=ArgName+': '+ArgTypeName;
    //writeln('TBaseWebIDLToPas.GetArguments Arg="',ArgName,'" A.ArgumentType.TypeName=',Arg.ArgumentType.TypeName,' ',Def<>nil);
    if (ArgType is TIDLFunctionDefinition)
        or (ArgType is TIDLCallBackDefinition)
        or (ArgType is TIDLDictionaryDefinition)
        or (ArgType is TIDLSequenceTypeDefDefinition)
        or (ArgResolvedTypeName='Variant')
        or (ArgResolvedTypeName='UnicodeString')
        or (ArgResolvedTypeName='UTF8String') then
      ArgName:='const '+ArgName;
    if Result<>'' then
      Result:=Result+'; ';
    Result:=Result+ArgName;
    end;
  if (Result<>'') or ForceBrackets then
    Result:='('+Result+')';
end;

Type
  // A partial argument list is a list which has been generated for a optional argument.
  // Additional arguments can never be added to a partial list...
  TIDLPartialDefinitionList = Class(TIDLDefinitionList);

function TBaseWebIDLToPas.CloneNonPartialArgumentList(aList: TFPObjectlist;
  ADest: TFPObjectlist; AsPartial: Boolean): integer;

Var
  I,J: Integer;
  CD: TIDLDefinition;
  DL,CL: TIDLDefinitionList;

begin
  Result:=0;
  if ADest=Nil then
    ADest:=aList;
  I:=aList.Count-1;
  While (I>=0) do
    begin
    DL:=TIDLDefinitionList(alist[i]);
    if Not (DL is TIDLPartialDefinitionList) then
      begin
      Inc(Result);
      if AsPartial then
        CL:=TIDLPartialDefinitionList.Create(Nil,True)
      else
        CL:=TIDLDefinitionList.Create(Nil,True);
      aDest.Add(CL);
      For J:=0 to DL.Count-1 do
        begin
        CD:=CloneArgument(DL.Definitions[J] as TIDLArgumentDefinition);
        CL.Add(CD);
        end;
      end;
    Dec(I);
    end;
end;

procedure TBaseWebIDLToPas.AddArgumentToOverloads(aList: TFPObjectlist; aName,
  aPasName, aTypeName: String; PosEl: TIDLBaseObject);

Var
  I: Integer;
  CD: TIDLArgumentDefinition;
  DL: TIDLDefinitionList;
  ODef : TIDLDefinition absolute posEl;
  aType : TPascalNativeType;

begin
  For I:=0 to aList.Count-1 do
    begin
    DL:=TIDLDefinitionList(alist[i]);
    if Not (DL is TIDLPartialDefinitionList) then
      begin
      CD:=TIDLArgumentDefinition.Create(Nil,aName,PosEl.SrcFile,PosEl.Line,PosEl.Column);
      if PosEl is TIDLTypeDefDefinition then
        CD.ArgumentType:=TIDLTypeDefDefinition(PosEl).Clone(CD)
      else
        CD.ArgumentType:=TIDLTypeDefDefinition.Create(CD,'',PosEl.SrcFile,PosEl.Line,PosEl.Column);
      CD.ArgumentType.TypeName:=aTypeName;
      if (PosEl is TIDLDefinition) and (ODef.Data is TPasData) then
        begin
        CD.ArgumentType.Data:=ClonePasData(ODef.Data as TPasData,CD.ArgumentType);
        aType:=TPasData(CD.ArgumentType.Data).NativeType;
        end
      else
        begin
        if verbose then
          DoLog('Unknown native type for overload %s (%s -> %s)',[aName,aTypeName,aPasName]);
        end;
      DL.Add(CD);

      CD.Data:=CreatePasData(aPasName,aType,CD,false);
      ResolveTypeDef(CD.ArgumentType);
      end;
    end;
end;

procedure TBaseWebIDLToPas.AddArgumentToOverloads(aList: TFPObjectlist; aDef: TIDLArgumentDefinition);

Var
  I: Integer;
  CD: TIDLDefinition;
  DL: TIDLDefinitionList;

begin
  For I:=0 to aList.Count-1 do
    begin
    DL:=TIDLDefinitionList(aList[i]);
    if Not (DL is TIDLPartialDefinitionList) then
      begin
      CD:=CloneArgument(aDef);
      DL.Add(CD);
      end;
    end;
end;

procedure TBaseWebIDLToPas.AddUnionOverloads(aList: TFPObjectlist; aName,
  aPasName: String; UT: TIDLUnionTypeDefDefinition);

Var
  L,L2: TFPObjectList;
  I,J: Integer;
  D: TIDLDefinitionList;
  Dups: TStringList;
  CurTypeDef: TIDLTypeDefDefinition;

begin
  //writeln('TBaseWebIDLToPas.AddUnionOverloads Name=',aName,' PasName=',aPasName);
  L2:=Nil;
  Dups:=TStringList.Create;
  Dups.Sorted:=True;
  Dups.Duplicates:=dupIgnore;
  L:=TFPObjectList.Create(False);
  try
    L2:=TFPObjectList.Create(False);
    // Collect non partial argument lists
    for I:=0 to aList.Count-1 do
      begin
      D:=TIDLDefinitionList(aList[i]);
      if Not (D is TIDLPartialDefinitionList) then
        L.Add(D);
      end;
    // Collect unique pascal types. Note that this can reduce the list to 1 element...
    For I:=0 to UT.Union.Count-1 do
      begin
      CurTypeDef:=UT.Union[I] as TIDLTypeDefDefinition;
      //writeln('TBaseWebIDLToPas.AddUnionOverloads Union[',I,']='+GetTypeName(CurTypeDef));
      Dups.AddObject(CurTypeDef.TypeName,CurTypeDef);
      end;
    // First, clone list and add argument to cloned lists
    For I:=1 to Dups.Count-1 do
      begin
      // Clone list
      CloneNonPartialArgumentList(L,L2,False);
      // Add argument to cloned list
      CurTypeDef:=TIDLTypeDefDefinition(Dups.Objects[I]);
      //writeln('TBaseWebIDLToPas.AddUnionOverloads Dups[',i,']=',Dups[i]);
      AddArgumentToOverloads(L2,aName,aPasName,Dups[i],CurTypeDef);
      // Add overloads to original list
      For J:=0 to L2.Count-1 do
        aList.Add(L2[J]);
      L2.Clear;
      end;
    // Add first Union to original list
    CurTypeDef:=TIDLTypeDefDefinition(Dups.Objects[0]);
    //writeln('TBaseWebIDLToPas.AddUnionOverloads Dups[',0,']=',Dups[0]);
    AddArgumentToOverloads(L,aName,aPasName,Dups[0],CurTypeDef);
  finally
    Dups.Free;
    L2.Free;
    L.Free;
  end;
end;

function TBaseWebIDLToPas.CheckUnionTypeDefinition(D: TIDLDefinition
  ): TIDLUnionTypeDefDefinition;

begin
  Result:=Nil;
  If (D is TIDLUnionTypeDefDefinition) then
    Result:=D as TIDLUnionTypeDefDefinition
  else
    begin
    D:=Context.FindDefinition((D as TIDLTypeDefDefinition).TypeName);
    if (D is TIDLUnionTypeDefDefinition) then
      Result:=D as TIDLUnionTypeDefDefinition
    end
end;

function TBaseWebIDLToPas.CloneArgument(Arg: TIDLArgumentDefinition
  ): TIDLArgumentDefinition;
begin
  Result:=Arg.Clone(nil);
  if Arg.Data<>nil then
    Result.Data:=ClonePasData(TPasData(Arg.Data),Result)
  else if verbose then
    DoLog('Warning : cloning argument "%s" without associated data',[Arg.GetNamePath]);
  Result.ArgumentType:=Arg.ArgumentType.Clone(Result);
  if Arg.ArgumentType.Data<>nil then
    Result.ArgumentType.Data:=ClonePasData(TPasData(Arg.ArgumentType.Data),Result)
  else if verbose then
    DoLog('Warning : cloning argument "%s" type "%s" without associated data',[Arg.GetNamePath,Arg.ArgumentType.GetNamePath]);
//  if Assigned(Result.ArgumentType)
end;

procedure TBaseWebIDLToPas.AddOverloads(aList: TFPObjectlist;
  aDef: TIDLFunctionDefinition; aIdx: Integer);

Var
  Arg: TIDLArgumentDefinition;
  ArgType: TIDLDefinition;
  UT: TIDLUnionTypeDefDefinition;

begin
 if aIdx>=aDef.Arguments.Count then
    exit;
  Arg:=aDef.Argument[aIdx];
  //writeln('TBaseWebIDLToPas.AddOverloads ',aDef.Name,'[',aIdx,']=',Arg.Name,':',Arg.ArgumentType.ClassName,' at ',GetDefPos(Arg),' Arg.IsOptional=',Arg.IsOptional);
  if Arg.IsOptional then
    CloneNonPartialArgumentList(aList);
  // Add current to list.
  ArgType:=Arg.ArgumentType;
  UT:=Nil;
  if coExpandUnionTypeArgs in BaseOptions then
    UT:=CheckUnionTypeDefinition(ArgType);
  if UT=Nil then
    AddArgumentToOverloads(aList,Arg)
  else
    AddUnionOverLoads(aList,Arg.Name,GetPasName(Arg),UT);
  AddOverloads(aList,aDef,aIdx+1);
end;

function TBaseWebIDLToPas.GetOverloads(aDef: TIDLFunctionDefinition): TFPObjectlist;

begin
  Result:=TFPObjectList.Create;
  try
    Result.Add(TIDLDefinitionList.Create(Nil,True));
    AddOverloads(Result,aDef,0);
  except
    Result.Free;
    Raise;
  end;
end;

function TBaseWebIDLToPas.WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition; aName: String = ''): Boolean;

Var
  FN,RT,Args: String;

begin
  Result:=True;
  FN:=aName;
  if FN='' then
    FN:=GetPasName(aDef);
  RT:=GetJSTypeName(aDef.ReturnType);
  if (RT='void') then
    RT:='';
  Args:=GetArguments(aDef.Arguments,False);
  if (RT='') then
    AddLn('%s = procedure %s;',[FN,Args])
  else
    AddLn('%s = function %s: %s;',[FN,Args,RT])
end;

function TBaseWebIDLToPas.WriteFunctionDefinition(
  aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition): Boolean;
begin
  Result:=true;
  if aDef=nil then exit;
  if aParent=nil then ;
end;

function TBaseWebIDLToPas.WriteDictionaryDefs(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  DD: TIDLDictionaryDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLDictionaryDefinition then
      if not TIDLDictionaryDefinition(D).IsPartial then
        if ConvertDef(D) then
          if WriteDictionaryDef(DD) then
            Inc(Result);
end;

function TBaseWebIDLToPas.WriteInterfaceDefs(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  ID: TIDLInterfaceDefinition absolute D;
  total : integer;
  ok : Boolean;
  Msg : string;
begin
  Result:=0;
  Msg:='';
  Total:=0;
  OK:=False;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLInterfaceDefinition then
      if not ID.IsPartial then
        if ConvertDef(D) then
          Inc(total);
  try
    for D in aList do
      if D is TIDLInterfaceDefinition then
        if not ID.IsPartial then
          if ConvertDef(D) then
            if WriteInterfaceDef(ID) then
              Inc(Result);
     OK:=True;
  finally
    if not OK then
      Msg:=SErrBeforeException;
    if verbose then
      DoLog('Wrote %d out of %d interface definitions%s.',[Result,Total,Msg]);
  end;
end;

function TBaseWebIDLToPas.WriteNamespaceDefs(aList: TIDLDefinitionList): Integer;
Var
  D: TIDLDefinition;
  ND: TIDLNamespaceDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLNamespaceDefinition then
      if not ND.IsPartial then
        if ConvertDef(D) then
          if WriteNamespaceDef(ND) then
            Inc(Result);
end;

procedure TBaseWebIDLToPas.GetOptions(L: TStrings; Full: boolean);

  function CountLines(const s: string): integer;
  var
    p: Integer;
  begin
    Result:=1;
    p:=1;
    while p<=length(s) do
      case s[p] of
      #10:
        begin
          inc(p);
          inc(Result);
        end;
      #13:
        begin
          inc(p);
          inc(Result);
          if (p<=length(s)) and (s[p]=#10) then inc(p);
        end;
      else
        inc(p);
      end;
  end;

  function CodeInfo(Src: TStrings): string;
  var
    LineCount, i: Integer;
  begin
    Result:='';
    if Src.Count=0 then
      exit;
    LineCount:=0;
    for i:=0 to Src.Count-1 do
      inc(LineCount,CountLines(Src[i]));
    Result:=Result+IntToStr(Src.Count)+' chunks in '+IntToStr(LineCount)+' lines';
  end;

Var
  S: String;
  I: Integer;

begin
  L.Add('Used command-line options: ');
  For I:=1 to ParamCount do
    L.Add(ParamStr(i));
  L.Add('');
  L.Add('Command-line options translated to: ');
  L.Add('');
  if Full then
    begin
    L.Add('Verbose: '+BoolToStr(Verbose,true));
    L.Add('Converter: '+ClassName);
    L.Add('InputFileName: '+InputFileName);
    L.Add('OutputFileName: '+OutputFileName);
    end;
  L.Add('Keyword prefix: '+KeywordPrefix);
  L.Add('Keyword suffix: '+KeywordSuffix);
  L.Add('Class prefix: '+ClassPrefix);
  L.Add('Class suffix: '+ClassSuffix);
  L.Add('Field prefix: '+FieldPrefix);
  L.Add('Getter prefix: '+GetterPrefix);
  L.Add('Setter prefix: '+SetterPrefix);
  Str(WebIDLVersion,S);
  L.Add('WebIDL version: '+S);
  if TypeAliases.Count>0 then
    begin
    L.Add('Type aliases:');
    L.AddStrings(Self.TypeAliases);
    end;
  L.Add('Dictionary class parent: '+DictionaryClassParent);
  if Full then
    begin
    L.Add('Include interface code: '+CodeInfo(IncludeInterfaceCode));
    L.Add('Include implementation code: '+CodeInfo(IncludeImplementationCode));
    end;
  L.Add('Base Options: '+BaseConversionOptionsToStr(BaseOptions));
end;

procedure TBaseWebIDLToPas.AddOptionsToHeader;

Var
  L: TStrings;
begin
  L:=TStringList.Create;
  try
    L.Add('Automatically generated file by '+ClassName+' on '+FormatDateTime('yyyy-mm-dd hh:nn:ss',Now));
    L.Add('');
    GetOptions(L,false);
    Comment(L);
  finally
    L.Free;
  end;
end;

procedure TBaseWebIDLToPas.WriteIncludeInterfaceCode;

Var
  S: String;

begin
  For S in IncludeInterfaceCode do
    Addln(S);
end;

procedure TBaseWebIDLToPas.WritePascal;
var
  i: Integer;
  Line: String;
  aList : TIDLDefinitionList;

begin
  CreateUnitClause;
  CreateHeader;
  if coAddOptionsToHeader in BaseOptions then
    AddOptionsToHeader;
  EnsureSection(csType);
  Indent;
  DoLog('Writing interface section.');
  DoLog('Generating forward class/interface definitions');
  WriteForwardClassDefs(Context.Definitions);
  DoLog('Generating enumerated definitions');
  WriteEnumDefs(Context.Definitions);
  // Callbacks
  DoLog('Generating types definitions');
  WriteFunctionImplicitTypes(Context.Definitions);
  DoLog('Generating typedefs and callback definitions');
  WriteTypeDefsAndCallbacks(Context.Definitions);
  DoLog('Generating dictionary definitions');
  aList:=Context.GetDictionariesTopologically;
  try
    WriteDictionaryDefs(aList);
  finally
    aList.Free;
  end;
  DoLog('Generating interface definitions');
  aList:=Context.GetInterfacesTopologically;
  try
    WriteInterfaceDefs(aList);
  finally
    aList.Free;
  end;
  DoLog('Generating namespace definitions');
  WriteNamespaceDefs(Context.Definitions);
  Undent;
  WriteGlobalVars;
  WriteIncludeInterfaceCode;
  Addln('');
  AddLn('implementation');
  WriteImplementation;
  AddLn('end.');
  if OutputStream<>nil then
    begin
    for i:=0 to Source.Count-1 do
      begin
      Line:=Source[i]+sLineBreak;
      OutputStream.Write(Line[1],length(Line));
      end;
    end
  else
    Source.SaveToFile(OutputFileName);
end;

function TBaseWebIDLToPas.CreatePasData(aName: String; aNativetype: TPascalNativeType; D: TIDLBaseObject; Escape: boolean
  ): TPasData;
begin
  if Escape then
    aName:=EscapeKeyWord(aName);
  Result:=PasDataClass.Create(aName,D);
  Result.NativeType:=aNativeType;
  FPasNameList.Add(Result);
end;

function TBaseWebIDLToPas.ClonePasData(Data: TPasData; OwnerDef: TIDLBaseObject
  ): TPasData;
begin
  Result:=PasDataClass.Create(Data.PasName,OwnerDef);
  Result.Resolved:=Data.Resolved;
  Result.NativeType:=Data.NativeType;
  FPasNameList.Add(Result);
end;


function TBaseWebIDLToPas.AllocateInterfacePasName(D: TIDLInterfaceDefinition; ParentName: String; Recurse : Boolean): TPasData;

var
  CN : String;

begin
  if (ParentName='') then ; // Silence compiler warning
  CN:=D.Name;
  if CN='' then
    raise EConvertError.Create('[20220725184324] at '+GetDefPos(D));
  CN:=ClassPrefix+CN+ClassSuffix;
  if D.Data=Nil then
    D.Data:=CreatePasData(CN,ntObject,D,true);
  if Recurse then
    AllocatePasNames(D.Members,D.Name);
  Result:=TPasData(D.Data);
end;

function TBaseWebIDLToPas.AllocateNamespacePasName(D: TIDLNameSpaceDefinition; ParentName: String; Recurse : Boolean): TPasData;

var
  CN : String;

begin
  if (ParentName='') then ; // Silence compiler warning
  CN:=D.Name;
  if CN='' then
    raise EConvertError.Create('[20220725184324] at '+GetDefPos(D));
  CN:=ClassPrefix+CN+ClassSuffix;
  if D.Data=Nil then
    D.Data:=CreatePasData(CN,ntObject,D,true);
  if Recurse then
    AllocatePasNames(D.Members,D.Name);
  Result:=TPasData(D.Data);
end;

function TBaseWebIDLToPas.AllocateDictionaryPasName(D: TIDLDictionaryDefinition; ParentName: String; Recurse : Boolean): TPasData;

var
  CN : String;

begin
  if (ParentName='') then ; // Silence compiler warning
  CN:=D.Name;
  if CN='' then
    raise EConvertError.Create('[20220725184410] at '+GetDefPos(D));
  if coDictionaryAsClass in BaseOptions then
    CN:=ClassPrefix+CN+ClassSuffix;
  if D.Data=nil then
    D.Data:=CreatePasData(EscapeKeyWord(CN),ntObject,D,true);
  if Recurse then
    AllocatePasNames(D.Members,D.Name);
  Result:=TPasData(D.Data);
end;

Function ConcatNames(const ParentName,CN : string) : string;

begin
  Result:=CN;
  if (Result<>'') and (ParentName<>'') then
    Result:='_'+Result;
  Result:=ParentName+Result;
end;

function TBaseWebIDLToPas.AllocateSequencePasName(D: TIDLSequenceTypeDefDefinition; ParentName: String; Recurse : Boolean): TPasData;

var
  CN : String;
  sDef : TIDLDefinition;
begin
  Result:=Nil;
  CN:=D.Name;
  if Recurse then
    begin
    // Should be passed in first
    AllocatePasName(D.ElementType,ConcatNames(ParentName,CN),True);
    if CN='' then
      CN:=ConstructSequenceTypeName(TIDLSequenceTypeDefDefinition(D),False)
    else
      CN:=ArrayPrefix+CN+ArraySuffix;
    if D.Data=Nil then
      begin
      sDef:=FindGlobalDef(CN);
      if (SDef=Nil) or (sDef.Data=Nil) then
        D.Data:=CreatePasData(EscapeKeyWord(CN),ntArray,D,true)
      else
        D.Data:=ClonePasData(TPasData(sDef.Data),D);
      end;
    end;
  Result:=TPasData(D.Data);
end;

function TBaseWebIDLToPas.AllocatePromisePasName(D: TIDLPromiseTypeDefDefinition; ParentName: String; Recurse: Boolean): TPasData;
var
  CN : String;
  sDef : TIDLDefinition;
begin
  Result:=Nil;
  CN:=D.Name;
  if CN='' then
    CN:='IJSPromise';
  if D.Data=Nil then
    begin
    sDef:=FindGlobalDef(CN);
    if (SDef=Nil) or (sDef.Data=Nil) then
      D.Data:=CreatePasData(EscapeKeyWord(CN),ntArray,D,true)
    else
      D.Data:=ClonePasData(TPasData(sDef.Data),D);
    end;
  if Recurse then
    AllocatePasName(D.ReturnType,ConcatNames(ParentName,CN+'Result'),True);
  Result:=TPasData(D.Data);
end;

function TBaseWebIDLToPas.AllocateDictionaryMemberPasName(D: TIDLDictionaryMemberDefinition; ParentName: String; Recurse : Boolean): TPasData;

Var
  CN: String;
begin
  Result:=Nil;
  CN:=D.Name;
  CN:=StringReplace(CN,'-','_',[rfReplaceAll]);
  if (D.Data=Nil) then
    D.Data:=CreatePasData(EscapeKeyWord(CN),ntNone,D,true);
  Result:=TPasData(D.Data);
  if Recurse then
    AllocatePasName(D.MemberType,ConcatNames(ParentName,D.Name),True);
end;

function TBaseWebIDLToPas.AllocateArgumentPasName(D: TIDLArgumentDefinition; ParentName: String; Recurse : Boolean): TPasData;

Var
  CN: String;
begin
  CN:=D.Name;
  if D.Data=Nil then
    D.Data:=CreatePasData(CN,ntNone,D,true);
  if Recurse then
    begin
    AllocatePasName(D.ArgumentType,ConcatNames(ParentName,D.Name),True);
    end;
  Result:=TPasData(D.Data);
end;

function TBaseWebIDLToPas.AllocateUnionPasName(D: TIDLUnionTypeDefDefinition; ParentName: String; Recurse : Boolean): TPasData;

var
  CN: String;
  sDef : TIDLDefinition;

begin
  CN:=D.Name;
  // This happens when there is an inline type declaration in a function definition.
  if CN='' then
    CN:=TypePrefix+ParentName+'_Type'
  else
    CN:=TypePrefix+CN;
  sDef:=FindGlobalDef(CN);
  if (SDef=Nil) or (sDef.Data=Nil) then
    Result:=CreatePasData(EscapeKeyWord(CN),ntVariant,D,true)
  else
    Result:=ClonePasData(TPasData(sDef.Data),D);
  D.Data:=Result;
  If Recurse then
    AllocatePasNames((D as TIDLUnionTypeDefDefinition).Union,CN)
end;

function TBaseWebIDLToPas.AllocateMapLikePasName(D: TIDLMapLikeDefinition; ParentName: String; Recurse: Boolean): TPasData;

Var
  CN: String;
begin
  CN:=D.Name;
  if CN='' then
     CN:=ParentName+'Type';
  CN:=TypePrefix+CN;
  if D.Data=Nil then
    D.Data:=CreatePasData(CN,ntNone,D,true);
  Result:=TPasData(D.Data);
  if Recurse then
    begin
    if assigned(D.KeyType) then
      AllocatePasName(D.KeyType,ConcatNames(ParentName,D.Name),True);
    if assigned(D.ValueType) then
      AllocatePasName(D.ValueType,ConcatNames(ParentName,D.Name),True);
    end;
end;

function TBaseWebIDLToPas.AllocateEnumeratedPasName(D: TIDLEnumDefinition; ParentName: String; Recurse: Boolean): TPasData;

var
  CN : String;

begin
  if (ParentName='') and Recurse then ; // Silence compiler warning
  CN:=D.Name;
  Result:=TPasData(D.Data);
  if Result=Nil then
    begin
    CN:=TypePrefix+CN;
    Result:=CreatePasData(CN,ntUnicodeString,D,true);
    D.Data:=Result;
    end;
end;

function TBaseWebIDLToPas.AllocateCallbackPasName(D: TIDLCallBackDefinition; ParentName: String; Recurse : Boolean): TPasData;

Var
  CN: String;

begin
  CN:=D.Name;
  if CN='' then
    CN:=ParentName+'Type';
  CN:=TypePrefix+CN;
  if D.Data=nil then
    D.Data:=CreatePasData(CN,ntMethod,D,true);
  Result:=TPasData(D.Data);
  if Recurse then
    AllocatePasName(D.FunctionDef,'',True)
end;

function TBaseWebIDLToPas.AllocateAttributePasName(aParent: TIDLStructuredDefinition; D: TIDLAttributeDefinition;
  ParentName: String; Recurse: Boolean): TPasData;

Var
  CN: String;

begin
  if (aParent=Nil) then ; // Silence compiler warning
  CN:=D.Name;
  if CN='' then
    CN:=ParentName+'Type';
  //CN:=TypePrefix+CN;
  if D.Data=Nil then
    D.Data:=CreatePasData(CN,ntNone,D,true);
  Result:=TPasData(D.Data);
  if Recurse and assigned(D.AttributeType) then
    AllocatePasName(D.AttributeType,Concatnames(ParentName,D.Name),True);
end;

function TBaseWebIDLToPas.AllocateFunctionPasName(D: TIDLFunctionDefinition; ParentName: String; Recurse : Boolean): TPasData;

Var
  CN : String;


begin
  CN:=D.name;
  if CN='' then
    begin
    if foGetter in D.options then
      CN:=SDefaultGetterName
    else if foSetter in D.options then
      CN:=SDefaultSetterName
    else
      CN:=ParentName+'Type';
    end;
  if (D.Data=Nil) then
    D.Data:=CreatePasData(CN,ntNone,D,true);
  Result:=TPasData(D.Data);
  if Recurse then
    begin
    AllocatePasNames(D.Arguments,ConcatNames(ParentName,D.Name));
    if Assigned(D.ReturnType) then
      AllocatePasName(D.ReturnType,ConcatNames(ParentName,D.Name),True);
    end;
end;

function TBaseWebIDLToPas.GetAliasPascalType(D: TIDLDefinition; out PascalTypeName: string): TPascalNativeType;

var
  NativeName: TIDLString;

begin
  NativeName:=D.Name;
  if (NativeName='') and (D is TIDLTypeDefinition) then
    NativeName:=TIDLTypeDefinition(D).GetJSTypeName;
  Result:=GetAliasPascalType(NativeName,PascalTypeName);
end;

function TBaseWebIDLToPas.GetAliasPascalType(aNativeTypeName : String; out PascalTypeName: string): TPascalNativeType;

var
  NT,S : String;
  P,I : Integer;

begin
  result:=ntunknown;
  S:=TypeAliases.Values[aNativeTypeName];
  if S='' then
    exit;
  Result:=ntObject;
  P:=Pos(',',S);
  if P>0 then
    begin
    NT:=Copy(S,P+1);
    if LowerCase(copy(nt,1,2))<>'nt' then
      nt:='nt'+nt;
    I:=GetEnumValue(TypeInfo(TPascalNativeType),nt);
    if (I<>-1) then
      Result:=TPascalNativeType(I)
    else
      begin
      if Verbose then
        DoLog('Warning: unknown native type in alias %s: %s',[S,NT]);
      SetLength(S,P-1);
      end;
    end;
  PascalTypeName:=S;
end;

function TBaseWebIDLToPas.AllocateConstPasName(D: TIDLConstDefinition; ParentName: String; Recurse : Boolean): TPasData;

var
  PN,CN,TN : String;
  aNativeType : TPascalNativeType;

begin
  if (ParentName='') and Recurse then ; // Silence compiler warning
  CN:=D.Name;
  TN:=D.TypeName;
  aNativeType:=GetPascalTypeAndName(TN,PN);
  if aNativeType=ntUnknown then
    aNativeType:=GetAliasPascalType(D,PN);
  if D.Data=Nil then
    D.Data:=CreatePasData(CN,aNativeType,D,true);
  Result:=TPasData(D.Data);
end;


function TBaseWebIDLToPas.AllocateDefaultPasName(D: TIDLDefinition; ParentName: String; Recurse : Boolean): TPasData;

var
  TN,CN,PN : String;
  aNativeType : TPascalNativeType;
  IsTypeDef,IsNamedTypeDef : Boolean;
  gDef : TIDLDefinition;

begin
  if (ParentName='') and Recurse then ; // Silence compiler warning
  {
    We are actually doing 2 things. We allocate a pascal name for an identifier,
    and we determine the native pascal type of the identifier, if possible.

  }
  isTypeDef:=(D is TIDLTypeDefDefinition);
  isNamedTypeDef:=IsTypedef and (TIDLTypeDefDefinition(D).IsTypeDef);
  if isNamedTypeDef then
    CN:=D.Name
  else
    CN:='';
  if IsTypeDef then
    TN:=TIDLTypeDefDefinition(D).TypeName
  else
    TN:=CN;
  aNativeType:=GetPascalTypeAndName(TN,PN);
  if aNativeType=ntUnknown then
    aNativeType:=GetAliasPascalType(D,PN);
  // We have a name
  if CN<>'' then
    CN:=TypePrefix+CN
  else if (aNativeType<>ntUnknown) then
    // Reuse native name
    CN:=PN
  else
    // Not native, not known:
    // If it is a globally defined type, reuse the name
    begin
    gDef:=FindGlobalDef(TN);
    if (gDef<>nil) then
      begin
      if Not assigned(gDef.Data) then
        AllocatePasName(gDef,'',True);
      // It should have the type prefix...
      CN:=GetPasName(gDef)
      end
    else
      begin
      // if we have a type alias, use that.
      CN:=TypeAliases.Values[TN];
      if CN='' then
        begin
        CN:=ParentName+'Type';
        CN:=TypePrefix+CN;
        end;
      end;
    end;
  if (CN='') and not (aNativeType in [ntUnknown,ntNone, ntError]) then
    Raise Exception.CreateFmt('No name for %s (TN: %s, Parent : %s)',[D.Name,TN,ParentName]);
  if D.Data=Nil then
    D.Data:=CreatePasData(CN,aNativeType,D,true);
  Result:=TPasData(D.Data);
end;


function TBaseWebIDLToPas.AllocatePasName(D: TIDLDefinition; ParentName: String; Recurse : Boolean): TPasData;

{
  Here we make sure every definition for which code will be generated has a pascal (type) name.
}

Var
  CN: String;

begin
  Result:=Nil;
  //writeln('TBaseWebIDLToPas.AllocatePasName ',ParentName,'.',D.Name,':',D.ClassName);
  if D Is TIDLInterfaceDefinition then
    Result:=AllocateInterfacePasName(TIDLInterfaceDefinition(D),ParentName,Recurse)
  else if D Is TIDLNamespaceDefinition then
    Result:=AllocateNameSpacePasName(TIDLNamespaceDefinition(D),ParentName,Recurse)
  else if D Is TIDLDictionaryDefinition then
    Result:=AllocateDictionaryPasName(TIDLDictionaryDefinition(D),ParentName,Recurse)
  else if D Is TIDLDictionaryMemberDefinition then
    Result:=AllocateDictionaryMemberPasName(TIDLDictionaryMemberDefinition(D),ParentName,Recurse)
  else if (D Is TIDLSequenceTypeDefDefinition) then
    Result:=AllocateSequencePasName(TIDLSequenceTypeDefDefinition(D),ParentName,Recurse)
  else if (D Is TIDLPromiseTypeDefDefinition) then
    Result:=AllocatePromisePasName(TIDLPromiseTypeDefDefinition(D),ParentName,Recurse)
  else if D Is TIDLArgumentDefinition then
    Result:=AllocateArgumentPasName(TIDLArgumentDefinition(D),ParentName,Recurse)
  else if D Is TIDLUnionTypeDefDefinition then
    Result:=AllocateUnionPasName(TIDLUnionTypeDefDefinition(D),ParentName,Recurse)
  else if D Is TIDLMapLikeDefinition then
    Result:=AllocateMapLikePasName(TIDLMapLikeDefinition(D),ParentName,Recurse)
  else if D Is TIDLCallBackDefinition then
    Result:=AllocateCallBackPasName(TIDLCallBackDefinition(D),ParentName,Recurse)
  else if D is TIDLAttributeDefinition then
    Result:=AllocateAttributePasName(D.Parent as TIDLStructuredDefinition,TIDLAttributeDefinition(D),ParentName,Recurse)
  else if D is TIDLFunctionDefinition then
    Result:=AllocateFunctionPasName(TIDLFunctionDefinition(D),ParentName,Recurse)
  else if D is TIDLEnumDefinition then
    Result:=AllocateEnumeratedPasName(TIDLEnumDefinition(D),ParentName,Recurse)
  else if D is TIDLConstDefinition then
    Result:=AllocateConstPasName(TIDLConstDefinition(D),ParentName,Recurse)
  else
    Result:=AllocateDefaultPasName(D,ParentName,Recurse);
  if Verbose and Assigned(Result) and (Result.PasName<>D.Name) then
    begin
    CN:=D.Name;
    if CN='' then
      CN:='<anonymous>';
    if (ParentName<>'') then
      CN:=ParentName+'.'+CN;
    if Verbose then
      DoLog('Renamed %s to %s at %s',[CN,Result.PasName,GetPasDataPos(Result)]);
    end;
end;

procedure TBaseWebIDLToPas.AddGlobalJSIdentifier(D: TIDLDefinition);

  function IsPartial : Boolean; inline;

  begin
    Result:=(D is TIDLStructuredDefinition) and (TIDLStructuredDefinition(D).IsPartial);
  end;

  function IsInclude : Boolean; inline;

  begin
    Result:=(D is TIDLIncludesDefinition);
  end;

var
  Old: TIDLDefinition;
begin
  if (not (IsPartial or IsInclude)) then
    begin
    Old:=FindGlobalDef(D.Name);
    if (Old<>nil) then
      raise EWebIDLParser.Create('Duplicate identifier '+D.Name+' at '+GetDefPos(D)+' and '+GetDefPos(Old)+' (20220718185400)');
    // AllocatePasName(D,'',False);
    FGlobalDefs.Add(D.Name,D);
    end
end;

procedure TBaseWebIDLToPas.ResolveParentInterfaces(aList: TIDLDefinitionList);
var
  D: TIDLDefinition;
begin
  For D in aList do
    if D is TIDLInterfaceDefinition then
      ResolveParentInterface(TIDLInterfaceDefinition(D))
    else if D is TIDLDictionaryDefinition then
      ResolveParentInterface(TIDLDictionaryDefinition(D));
end;

procedure TBaseWebIDLToPas.ResolveParentInterface(Intf: TIDLInterfaceDefinition
  );
var
  aDef: TIDLDefinition;
begin
  if Intf.ParentInterface<>nil then exit;
  if Intf.ParentName='' then exit;
  aDef:=FindGlobalDef(Intf.ParentName);
  if aDef is TIDLInterfaceDefinition then
    Intf.ParentInterface:=TIDLInterfaceDefinition(aDef);
end;

procedure TBaseWebIDLToPas.ResolveParentInterface(Intf: TIDLDictionaryDefinition
  );
var
  aDef: TIDLDefinition;
begin
  if Intf.ParentDictionary<>nil then exit;
  if Intf.ParentName='' then exit;
  aDef:=FindGlobalDef(Intf.ParentName);
  if aDef is TIDLDictionaryDefinition then
    Intf.ParentDictionary:=TIDLDictionaryDefinition(aDef);
end;

procedure TBaseWebIDLToPas.ResolveTypeDefs(aList: TIDLDefinitionList);
var
  D: TIDLDefinition;
begin
  For D in aList do
    ResolveTypeDef(D);
end;

procedure TBaseWebIDLToPas.ResolveTypeDef(D: TIDLDefinition);
{
  Here we make sure every type name is resolved to
  - Either a Javascript base type
  - a TIDLTypeDefinition instance.
  In the latter case the resulting resolved TIDLTypeDefinition instance is stored in the Resolved field of a TPasData() element.

  Conceivably, we can create type defs for all base types, so every type results in a TIDLTypeDefinition,
  regardless of whether it is a base type or not.
}

  procedure ResolveTypeName(const aTypeName: string);
  var
    Def: TIDLDefinition;
    Data: TPasData;

  begin
    if (D.Data is TPasData) and (TPasData(D.Data).Resolved<>nil) then
      exit;

    Def:=FindGlobalDef(aTypeName);
    if Def=nil then
      begin
      if (NameToWebIDLBaseType(aTypeName)=wibtNone)
          and (TypeAliases.Values[aTypeName]='') then
        raise EConvertError.Create('[20220725172231] type "'+aTypeName+'" of "'+D.Name+'" not found at '+GetDefPos(D));
      end
    else if not (Def is TIDLTypeDefinition) then
      begin
      raise EConvertError.Create('[20220725172231] type "'+D.ClassName+'" of "'+D.Name+'" is not a type at '+GetDefPos(D));
      end
    else
      begin
      if (D.Data=nil) then
        begin
        if not (Def.Data is TPasData) then
          raise EConvertError.Create('[20240417092301] type "'+D.ClassName+'" of "'+D.Name+'" does not have pascal data associated at'+GetDefPos(D));
        D.Data:=ClonePasData(TPasData(Def.Data),D);;
        end;
      Data:=TPasData(D.Data);
      if Def<>D then
        Data.Resolved:=Def as TIDLTypeDefinition;
      //writeln('ResolveTypeName Resolved D=',D.Name,':',D.ClassName,' at ',GetDefPos(D),' Data.Resolved=',Def.Name,':',Def.ClassName,' at ',GetDefPos(Def));
      end;
  end;

var
  DMD: TIDLDictionaryMemberDefinition;
  IT: TIDLIterableDefinition;
  SerializerD: TIDLSerializerDefinition;
  FD: TIDLFunctionDefinition;
begin
  if D=nil then exit;
  if not ConvertDef(D) then
    exit;
  // writeln('TBaseWebIDLToPas.ResolveTypeDef START ',D.Name,':',D.ClassName,' at ',GetDefPos(D),' D=',hexstr(ptruint(D),sizeof(ptruint)*2));
  if D Is TIDLInterfaceDefinition then
    ResolveTypeDefs(TIDLInterfaceDefinition(D).Members)
  else if D Is TIDLNamespaceDefinition then
    ResolveTypeDefs(TIDLNamespaceDefinition(D).Members)
  else if D Is TIDLDictionaryDefinition then
    ResolveTypeDefs(TIDLDictionaryDefinition(D).Members)
  else if D is TIDLIncludesDefinition then
    //
  else if D Is TIDLFunctionDefinition then
    begin
    FD:=TIDLFunctionDefinition(D);
    ResolveTypeDefs(FD.Arguments);
    ResolveTypeDef(FD.ReturnType);
    end
  else if D is TIDLAttributeDefinition then
    ResolveTypeDef(TIDLAttributeDefinition(D).AttributeType)
  else if D is TIDLArgumentDefinition then
    ResolveTypeDef(TIDLArgumentDefinition(D).ArgumentType)
  else if D is TIDLSequenceTypeDefDefinition then
    ResolveTypeDef(TIDLSequenceTypeDefDefinition(D).ElementType)
  else if D is TIDLPromiseTypeDefDefinition then
    ResolveTypeDef(TIDLPromiseTypeDefDefinition(D).ReturnType)
  else if D is TIDLMapLikeDefinition then
    begin
    ResolveTypeDef(TIDLMapLikeDefinition(D).KeyType);
    ResolveTypeDef(TIDLMapLikeDefinition(D).ValueType);
    end
  else if D is TIDLTypeDefDefinition then
    begin
    ResolveTypeName(TIDLTypeDefDefinition(D).TypeName)
    end
  else if D is TIDLConstDefinition then
    begin
    if TIDLConstDefinition(D).TypeName<>'' then
      ResolveTypeName(TIDLConstDefinition(D).TypeName);
    end
  else if D is TIDLSerializerDefinition then
    begin
    SerializerD:=TIDLSerializerDefinition(D);
    ResolveTypeDef(SerializerD.SerializerFunction);
    end
  else if D is TIDLDictionaryMemberDefinition then
    begin
    DMD:=TIDLDictionaryMemberDefinition(D);
    ResolveTypeDef(DMD.MemberType);
    ResolveTypeDef(DMD.DefaultValue);
    end
  else if D is TIDLEnumDefinition then
    //
  else if D is TIDLCallBackDefinition then
    ResolveTypeDef(TIDLCallBackDefinition(D).FunctionDef)
  else if D is TIDLSetlikeDefinition then
    ResolveTypeDef(TIDLSetlikeDefinition(D).ElementType)
  else if D is TIDLImplementsOrIncludesDefinition then
    //
  else if D is TIDLIterableDefinition then
    begin
    IT:=TIDLIterableDefinition(D);
    ResolveTypeDef(IT.ValueType);
    ResolveTypeDef(IT.KeyType);
    end
  else {if Verbose then}
    raise EConvertError.Create('[20220725172214] TBaseWebIDLToPas.ResolveTypeDef unknown '+D.Name+':'+D.ClassName+' at '+GetDefPos(D));
end;

procedure TBaseWebIDLToPas.RemoveInterfaceForwards(aList: TIDLDefinitionList);

Var
  L: TFPObjectHashTable;

  Procedure DeleteIntf(Def: TIDLInterfaceDefinition);
  begin
    if Verbose then
      DoLog('removing interface '+Def.Name+' at '+GetDefPos(Def));
    aList.Delete(Def);
  end;

  Procedure CheckDuplicateInterfaceDef(Def: TIDLInterfaceDefinition);
  var
    aName: UTF8String;
    OldDef: TIDLInterfaceDefinition;
  begin
    if Def.IsPartial then exit;
    aName:=Def.Name;
    OldDef:=TIDLInterfaceDefinition(L.Items[aName]);
    if OldDef=nil then
      L.add(aName,Def)
    else
      begin
      if OldDef.IsForward then
        begin
        L.Delete(OldDef.Name);
        DeleteIntf(OldDef);
        L.Add(aName,Def);
        end
      else if Def.IsForward then
        DeleteIntf(Def)
      else
        raise EConvertError.Create('[20220725172236] Duplicate interface '+GetDefPos(Def)+' and '+GetDefPos(OldDef)+' (20220718184717)');
      end;
  end;

var
  i: Integer;
begin
  L:=TFPObjectHashTable.Create(False);
  try
    For i:=aList.Count-1 downto 0 do
      if (aList[i] is TIDLInterfaceDefinition) then
        CheckDuplicateInterfaceDef(TIDLInterfaceDefinition(aList[i]));
  finally
    L.Free;
  end;
end;

function TBaseWebIDLToPas.ConvertDef(D: TIDLDefinition): Boolean;


var
  AD : TIDLAttributeDefinition absolute D;
  FD : TIDLFunctionDefinition;
  A,RT : TIDLDefinition;
  FAD : TIDLArgumentDefinition absolute A;
  RN,N : String;
  ANT : TPascalNativeType;
  isChrome : Boolean;

begin
  isChrome:=False;
  Result:=(coChromeWindow in BaseOptions) or Not D.HasSimpleAttribute('ChromeOnly');
  if not Result then
    exit;
  if Result and (coOnlyUsed in BaseOptions) then
    if (D.Data is TPasData) and not TPasData(D.Data).Used then
      exit(False);
end;

function TBaseWebIDLToPas.FindGlobalDef(const aName: UTF8String
  ): TIDLDefinition;
begin
  Result:=TIDLDefinition(FGlobalDefs.Items[aName]);
end;

function TBaseWebIDLToPas.GetDefPos(Def: TIDLBaseObject; WithoutFile: boolean
  ): string;
begin
  Result:='('+IntToStr(Def.Line)+','+IntToStr(Def.Column)+')';
  if not WithoutFile then
    Result:=Def.SrcFile+Result;
end;

function TBaseWebIDLToPas.GetPasDataPos(D: TPasData; WithoutFile: boolean
  ): string;
begin
  Result:='('+IntToStr(D.Line)+','+IntToStr(D.Column)+')';
  if not WithoutFile then
    Result:=D.SrcFile+Result;
end;

procedure TBaseWebIDLToPas.SetTypeAliases(AValue: TStrings);
begin
  if FTypeAliases.Equals(AValue) then Exit;
  FTypeAliases.Assign(AValue);
  TrimList(FTypeAliases);
end;

procedure TBaseWebIDLToPas.TrimList(List: TStrings);
var
  i: Integer;
begin
  for i:=List.Count-1 downto 0 do
    if Trim(List[i])='' then
       List.Delete(i);
end;

procedure TBaseWebIDLToPas.SetIncludeInterfaceCode(AValue: TStrings);
begin
  if FIncludeInterfaceCode=AValue then Exit;
  FIncludeInterfaceCode.Assign(AValue);
end;

procedure TBaseWebIDLToPas.SetOutputFileName(const AValue: String);
var
  aName, Ext: String;
begin
  if FOutputFileName=AValue then Exit;
  FOutputFileName:=AValue;
  if OutputUnitName='' then
    begin
    aName:=ExtractFileName(AValue);
    Ext:=ExtractFileExt(AName);
    if Ext<>'' then
      aName:=LeftStr(aName,length(aName)-length(Ext));
    OutputUnitName:=aName;
    end;
end;

procedure TBaseWebIDLToPas.SetIncludeImplementationCode(AValue: TStrings);
begin
  if FIncludeImplementationCode=AValue then Exit;
  FIncludeImplementationCode.Assign(AValue);
end;

procedure TBaseWebIDLToPas.SetGlobalVars(const AValue: TStrings);
begin
  if FGlobalVars.Equals(AValue) then Exit;
  FGlobalVars.Assign(AValue);
  TrimList(FGlobalVars);
end;

procedure TBaseWebIDLToPas.AllocatePasNames(aList: TIDLDefinitionList; ParentName: String = '');


var
  D: TIDLDefinition;

begin
  For D in aList do
    AllocatePasName(D,ParentName,False);
  For D in aList do
    AllocatePasName(D,ParentName,True);
end;


function TBaseWebIDLToPas.CreateCallBackFromInterface(aDef: TIDLInterfaceDefinition): TIDLCallBackDefinition;

var
  I,Idx,Count : Integer;

begin
  if Verbose then
    DoLog('Converting callback interface %s to callback',[aDef.Name]);
  Count:=0;
  For I:=0 to aDef.Members.Count-1 do
    if (aDef.Member[I] is TIDLFunctionDefinition) then
      begin
      Idx:=I;
      Inc(Count);
      end;
  if (Count<>1)  then
    Raise EWebIDLParser.CreateFmt('Callback Interface %s has wrong function member count',[aDef.Name]);
  if not (aDef.Member[Idx] is TIDLFunctionDefinition) then
    Raise EWebIDLParser.CreateFmt('Callback Interface %s member %s is not a function',[aDef.Name,aDef.Members[Idx].Name]);
  Result:=TIDLCallBackDefinition(FContext.Add(TIDLCallBackDefinition,aDef.Name,aDef.SrcFile,aDef.Line,aDef.Column));
  Result.FunctionDef:=TIDLFunctionDefinition(aDef.Members.Extract(aDef.Member[Idx]));
  Result.FunctionDef.Name:=Result.Name;
  Result.FunctionDef.Parent:=Result;
end;

procedure TBaseWebIDLToPas.ResolveCallbackInterfaces;

var
  D : TIDLDefinition;
  DI : TIDLInterfaceDefinition absolute D;

begin
  For D In FContext.Definitions do
    if (D is TIDLInterfaceDefinition) and DI.IsCallBack then
      begin
      CreateCallBackFromInterface(DI);
      FContext.Definitions.Delete(D);
      end;

end;

function TBaseWebIDLToPas.GetUsed(D: TIDLDefinition) : Boolean;

begin
  Result:=False;
  Result:=(not (D.Data is TPasData)) or TPasData(D.Data).Used;
end;

function TBaseWebIDLToPas.InUsedList(D: TIDLDefinition) : Boolean;

begin
  Result:=FUsedDefs.Items[D.Name]<>Nil;
end;

function TBaseWebIDLToPas.MarkUsed(D: TIDLDefinition; ParentIsUsed : Boolean) : Boolean;

  // Return true if the definition 'used' status was change to true
  function DoMark : Boolean;

  begin
    Result:=False;
    if (D.Data=nil) and not (D is TIDLTypeDefDefinition) then
      begin
      if Verbose then
        DoLog('[202406021006] type "'+D.ClassName+'" of "'+D.Name+'" has no pascal name assigned, cannot check used');
      Exit;
      end;
    if GetUsed(D) then
      exit;
    if ParentIsUsed or InUsedList(D) then
      begin
      // Writeln('Marking ',D.GetNamePath,' as used');
      TPasData(D.Data).Used:=True;
      Result:=True;
      end;
  end;

  function MarkAlias(const aTypeName: string) : Boolean;

  var
    lDef: TIDLDefinition;

  begin
    lDef:=FindGlobalDef(aTypeName);
    Result:=(lDef<>nil) and MarkUsed(lDef,True);
  end;

var
  DMD: TIDLDictionaryMemberDefinition;
  IT: TIDLIterableDefinition;
  SerializerD: TIDLSerializerDefinition;
  FD: TIDLFunctionDefinition;
  P : TIDLInterfaceDefinition;
  I : Integer;

begin
  Result:=False;
  if D=nil then exit;
  // Writeln('Checking ',D.GetNamePath,' for used');
  if not DoMark then
    exit;
  // Mark sub-classes as used
  if D Is TIDLInterfaceDefinition then
    begin
    MarkUsedDefinitions(TIDLInterfaceDefinition(D).Members,True);
    P:=TIDLInterfaceDefinition(D).ParentInterface;
    While Assigned(P) do
      begin
      MarkUsed(P,True);
      P:=P.ParentInterface;
      end;
    P:=TIDLInterfaceDefinition(D);
    For I:=0 to P.Partials.Count-1 do
      MarkUsed(P.Partial[i],True);
    end
  else if D Is TIDLNamespaceDefinition then
    begin
    MarkUsedDefinitions(TIDLNamespaceDefinition(D).Members,True);
    end
  else if D Is TIDLDictionaryDefinition then
    begin
    MarkUsedDefinitions(TIDLDictionaryDefinition(D).Members,True);
    MarkUsed(TIDLDictionaryDefinition(D).ParentDictionary,True);
    end
  else if D is TIDLIncludesDefinition then
    begin
    //
    end
  else if D Is TIDLFunctionDefinition then
    begin
    FD:=TIDLFunctionDefinition(D);
    MarkUsedDefinitions(FD.Arguments,True);
    MarkUsed(FD.ReturnType,True);
    end
  else if D Is TIDLUnionTypeDefDefinition then
    MarkUsedDefinitions(TIDLUnionTypeDefDefinition(D).Union,True)
  else if D is TIDLAttributeDefinition then
    MarkUsed(TIDLAttributeDefinition(D).AttributeType,True)
  else if D is TIDLArgumentDefinition then
    MarkUsed(TIDLArgumentDefinition(D).ArgumentType,True)
  else if D is TIDLSequenceTypeDefDefinition then
    MarkUsed(TIDLSequenceTypeDefDefinition(D).ElementType,True)
  else if D is TIDLPromiseTypeDefDefinition then
    MarkUsed(TIDLPromiseTypeDefDefinition(D).ReturnType,True)
  else if D is TIDLMapLikeDefinition then
    begin
    MarkUsed(TIDLMapLikeDefinition(D).KeyType,True);
    MarkUsed(TIDLMapLikeDefinition(D).ValueType,True);
    end
  else if D is TIDLTypeDefDefinition then
    begin
    MarkAlias(TIDLTypeDefDefinition(D).TypeName)
    end
  else if D is TIDLConstDefinition then
    begin
    if TIDLConstDefinition(D).TypeName<>'' then
      MarkAlias(TIDLConstDefinition(D).TypeName);
    end
  else if D is TIDLSerializerDefinition then
    begin
    SerializerD:=TIDLSerializerDefinition(D);
    MarkUsed(SerializerD.SerializerFunction,True);
    end
  else if D is TIDLDictionaryMemberDefinition then
    begin
    DMD:=TIDLDictionaryMemberDefinition(D);
    MarkUsed(DMD.MemberType,True);
    // MarkUsed(DMD.DefaultValue,True);
    end
  else if D is TIDLEnumDefinition then
    //
  else if D is TIDLCallBackDefinition then
    MarkUsed(TIDLCallBackDefinition(D).FunctionDef,True)
  else if D is TIDLSetlikeDefinition then
    MarkUsed(TIDLSetlikeDefinition(D).ElementType,True)
  else if D is TIDLImplementsOrIncludesDefinition then
    //
  else if D is TIDLIterableDefinition then
    begin
    IT:=TIDLIterableDefinition(D);
    MarkUsed(IT.ValueType,True);
    MarkUsed(IT.KeyType,True);
    end
  else {if Verbose then}
    raise EConvertError.Create('[20220725172214] TBaseWebIDLToPas.ResolveTypeDef unknown '+D.Name+':'+D.ClassName+' at '+GetDefPos(D));

end;

procedure TBaseWebIDLToPas.MarkUsedDefinitions(aList : TIDLDefinitionList; ParentIsUsed : Boolean);

var
  D : TIDLDefinition;

begin
  For D In aList do
    begin
    MarkUsed(D,ParentIsUsed);
    end;
end;

Function TBaseWebIDLToPas.CheckChromeOnly(D : TIDLDefinition) : Boolean;

  Function IsChromeOnly(D : TIDLDefinition) : boolean; inline;

  begin
    Result:=Assigned(D) and D.HasSimpleAttribute('ChromeOnly');
  end;

  function CheckAlias(const aTypeName: string) : Boolean;

  var
    lDef: TIDLDefinition;

  begin
    lDef:=FindGlobalDef(aTypeName);
    Result:=(lDef<>nil) and CheckChromeOnly(lDef);
  end;


var
  AD : TIDLAttributeDefinition absolute D;
  FD : TIDLFunctionDefinition;
  A,RT : TIDLDefinition;
  FAD : TIDLArgumentDefinition absolute A;
  RN,N : String;
  ANT : TPascalNativeType;
  isChrome : Boolean;
  SerializerD: TIDLSerializerDefinition;
  DMD: TIDLDictionaryMemberDefinition;
  IT : TIDLIterableDefinition;

begin
  Result:=False;
  isChrome:=False;
  if (D=Nil) then
    exit;
  Result:=IsChromeOnly(D);
  if Result then
    exit;
  if (D.Data is TPasData) then
    begin
    if TPasData(D.Data).ChromeChecked then exit;
    TPasData(D.Data).ChromeChecked:=True;
    end;
  // Check sub definitions
  if D Is TIDLInterfaceDefinition then
    PropagateChromeOnly(TIDLInterfaceDefinition(D).Members)
  else if D Is TIDLNamespaceDefinition then
    PropagateChromeOnly(TIDLNamespaceDefinition(D).Members)
  else if D Is TIDLDictionaryDefinition then
    PropagateChromeOnly(TIDLDictionaryDefinition(D).Members)
  else if D is TIDLIncludesDefinition then
    //
  else if D is TIDLArgumentDefinition then
    begin
    IsChrome:=CheckChromeOnly(TIDLArgumentDefinition(D).ArgumentType);
    if IsChrome and Verbose then
      DoLog('Marking argument %s as "ChromeOnly" because the argument type is marked "ChromeOnly"',[D.Name]);
    end
  else if D is TIDLSequenceTypeDefDefinition then
    begin
    IsChrome:=CheckChromeOnly(TIDLSequenceTypeDefDefinition(D).ElementType);
    if IsChrome and Verbose then
      DoLog('Marking sequence %s as "ChromeOnly" because the element type is marked "ChromeOnly"',[D.Name]);
    end
  else if D is TIDLPromiseTypeDefDefinition then
    begin
    IsChrome:=CheckChromeOnly(TIDLPromiseTypeDefDefinition(D).ReturnType);
    if IsChrome and Verbose then
      DoLog('Marking map %s as "ChromeOnly" because the promise result type is marked "ChromeOnly"',[D.Name]);
    end
  else if D is TIDLMapLikeDefinition then
    begin
    isChrome:=CheckChromeOnly(TIDLMapLikeDefinition(D).KeyType);
    isChrome:=CheckChromeOnly(TIDLMapLikeDefinition(D).ValueType) or IsChrome;
    if IsChrome and Verbose then
      DoLog('Marking map %s as "ChromeOnly" because the map key or value type is marked "ChromeOnly"',[D.Name]);
    end
  else if D is TIDLTypeDefDefinition then
    begin
    CheckAlias(TIDLTypeDefDefinition(D).TypeName)
    end
  else if D is TIDLConstDefinition then
    begin
    if TIDLConstDefinition(D).TypeName<>'' then
      IsChrome:=CheckAlias(TIDLConstDefinition(D).TypeName);
    if IsChrome and Verbose then
      DoLog('Marking const %s as "ChromeOnly" because the const type is marked "ChromeOnly"',[D.Name]);
    end
  else if D is TIDLSerializerDefinition then
    begin
    SerializerD:=TIDLSerializerDefinition(D);
    IsChrome:=CheckChromeOnly(SerializerD.SerializerFunction);
    if IsChrome and Verbose then
      DoLog('Marking serializer %s as "ChromeOnly" because the function type is marked "ChromeOnly"',[D.Name]);
    end
  else if D is TIDLDictionaryMemberDefinition then
    begin
    DMD:=TIDLDictionaryMemberDefinition(D);
    IsChrome:=CheckChromeOnly(DMD.MemberType);
    IsChrome:=CheckChromeOnly(DMD.DefaultValue) or IsChrome;
    if IsChrome and Verbose then
      DoLog('Marking dictionary member %s as "ChromeOnly" because the member type or the default value is marked "ChromeOnly"',[D.Name]);
    end
  else if D is TIDLEnumDefinition then
    //
  else if D is TIDLCallBackDefinition then
    begin
    IsChrome:=CheckChromeOnly(TIDLCallBackDefinition(D).FunctionDef);
    if IsChrome and Verbose then
      DoLog('Marking callback definition %s as "ChromeOnly" because the function type is marked "ChromeOnly"',[D.Name]);
    end
  else if D is TIDLSetlikeDefinition then
    begin
    IsChrome:=CheckChromeOnly(TIDLSetlikeDefinition(D).ElementType);
    if IsChrome and Verbose then
      DoLog('Marking set %s as "ChromeOnly" because the member type is marked "ChromeOnly"',[D.Name]);
    end
  else if D is TIDLImplementsOrIncludesDefinition then
    //
  else if D is TIDLIterableDefinition then
    begin
    IT:=TIDLIterableDefinition(D);
    IsChrome:=CheckChromeOnly(IT.ValueType);
    IsChrome:=CheckChromeOnly(IT.KeyType) or IsChrome;
    if IsChrome and Verbose then
      DoLog('Marking iterable %s as "ChromeOnly" because the key or value type is marked "ChromeOnly"',[D.Name]);
    end
  else if (D is TIDLAttributeDefinition) and Assigned(AD.AttributeType) then
    begin

    ResolveTypeDef(AD.AttributeType);
    RT:=GetResolvedType(AD.AttributeType,ANT,N,RN);

    isChrome:=CheckChromeOnly(RT);
    if isChrome and Verbose then
      DoLog('Marking attribute %s as "ChromeOnly" because attribute type "%s" is marked "ChromeOnly"',[D.Name,N{AD.AttributeType.Name}]);
    end
  else if (D is TIDLFunctionDefinition) then
    begin
    FD:=TIDLFunctionDefinition(D);
    RT:=GetResolvedType(FD.ReturnType,ANT,N,RN);
    isChrome:=CheckChromeOnly(RT);
    if isChrome and Verbose then
      DoLog('Marking function %s as "ChromeOnly" because return type %s is marked "ChromeOnly"',[D.Name, RT.Name]);
    For A in FD.Arguments do
      begin
      ResolveTypeDef(FAD.ArgumentType);
      RT:=GetResolvedType(FAD.ArgumentType,ANT,N,RN);
      if CheckChromeOnly(RT) then
        begin
        IsChrome:=True;
        if Verbose then
          DoLog('Marking function "%s" as "ChromeOnly" because argument "%s" (type "%s") is marked "ChromeOnly"',[D.Name,A.Name, RT.Name]);
        end;
      end;
    end
  else if (D is TIDLCallbackDefinition) then
    begin
    FD:=TIDLCallbackDefinition(D).FunctionDef;
    RT:=GetResolvedType(FD.ReturnType,ANT,N,RN);
    isChrome:=CheckChromeOnly(RT);
    if isChrome and Verbose then
      DoLog('Marking callback function %s as "ChromeOnly" because return type %s is marked "ChromeOnly"',[D.Name, RT.Name]);
    For A in FD.Arguments do
      begin
      ResolveTypeDef(FAD.ArgumentType);
      RT:=GetResolvedType(FAD.ArgumentType,Ant,N,RN);
      if CheckChromeOnly(RT) then
        begin
        IsChrome:=True;
        if Verbose then
          DoLog('Marking callback function %s as "ChromeOnly" because argument "%s" (type "%s") is marked "ChromeOnly"',[D.Name,A.Name, RT.Name]);
        end;
      end;
    end;
  if IsChrome then
    begin
    D.Attributes.Add('ChromeOnly');
    Result:=True;
    end;
end;

procedure TBaseWebIDLToPas.PropagateChromeOnly(aList : TIDLDefinitionList);

var
  D : TIDLDefinition;

begin
  For D in aList do
    CheckChromeOnly(D);
end;


procedure TBaseWebIDLToPas.ProcessDefinitions;

var
  D : TIDLDefinition;

begin
  DoLog('Resolving callback interfaces.');
  ResolveCallbackInterfaces;
  DoLog('Removing interface forwards.');
  RemoveInterfaceForwards(FContext.Definitions);
  DoLog('Appending partials to interfaces.');
  FContext.AppendPartials;
  DoLog('Appending includes to interfaces.');
  FContext.AppendIncludes;
  DoLog('Adding global identifiers.');
  For D in FContext.Definitions do
    if D.Name<>'' then
      AddGlobalJSIdentifier(D);
  DoLog('Allocating pascal names.');
  AllocatePasNames(FContext.Definitions);
  DoLog('Resolving parent interfaces.');
  ResolveParentInterfaces(FContext.Definitions);
  // We need to do this before ResolveTypeDefs, because ResolveTypeDefs uses ConvertDef()
  if (coOnlyUsed in BaseOptions) then
    begin
    DoLog('Marking used type definitions.');
    MarkUsedDefinitions(FContext.Definitions,False);
    end;
  if Not (coChromeWindow in BaseOptions) then
    begin
    DoLog('Propagating ChromeOnly attribute.');
    PropagateChromeOnly(FContext.Definitions);
    end;
  DoLog('Resolving type definitions.');
  ResolveTypeDefs(FContext.Definitions);
  DoLog('Done processing definitions.');
end;


procedure TBaseWebIDLToPas.Execute;

begin
  if Verbose then
    begin
    WriteOptions;
    DoLog('');
    end;

  FContext:=CreateContext;
  try
    FContext.Aliases:=Self.TypeAliases;
    Parse;
    if Verbose then
      DoLog('Parsed %d definitions.',[Context.Definitions.Count]);
    ProcessDefinitions;
    if Verbose then
      DoLog('Processed %d definitions.',[Context.Definitions.Count]);
    WritePascal;
  finally
    FreeAndNil(FContext);
  end;
end;

procedure TBaseWebIDLToPas.WriteOptions;
var
  i: Integer;
  L: TStringList;
begin
  L:=TStringList.Create;
  try
    GetOptions(L,true);
    for i:=0 to L.Count-1 do
      DoLog(L[i]);
  finally
    L.Free;
  end;
end;

procedure TBaseWebIDLToPas.SetUsedList(aList: TStrings);

var
  S : String;

begin
  if (aList=Nil) or (aList.Count=0) then
    exit;
  Include(FBaseOptions,coOnlyUsed);
  if not Assigned(FUsedDefs) then
    FUsedDefs:=TFPObjectHashTable.Create(False)
  else
    FUsedDefs.Clear;
  // We just need to know if a name is in the list
  For S in aList do
    FUsedDefs.Add(S,Self);
end;

function TBaseWebIDLToPas.IsKeyWord(const S: String): Boolean;
Const
   KW=';class;classname;finalization;function;initialization;procedure;';
begin
  Result:=inherited IsKeyWord(S);
  if Result then exit;
  Result:=Pos(';'+lowercase(S)+';',KW)<>0;
end;

end.

