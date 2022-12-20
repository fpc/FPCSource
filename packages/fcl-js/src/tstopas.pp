{
    This file is part of the Free Component Library

    Typedescript declarations to pascal code converter
    Copyright (c) 2021 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tstopas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, jsbase, jstree, jsscanner, jsparser,pascodegen;

Type
  ETSToPas = Class(Exception);

  TJSFuncDefArray = Array of TJSFuncDef;

  { TPasData }

  TPasData = Class(TObject)
  private
    FOriginalName: TJSString;
    FPasName: String;
  Public
    Constructor Create(const aOriginalName : jsBase.TJSString; const APasName : String);
    Destructor destroy; override;
    Property PasName : String read FPasName;
    Property OriginalName : TJSString Read FOriginalName;
  end;

  TConversionOption = (coRaw,coGenericArrays,coUseNativeTypeAliases,coLocalArgumentTypes, coUntypedTuples, coDynamicTuples,
                       coExternalConst,coExpandUnionTypeArgs,coaddOptionsToheader,coInterfaceAsClass,coSkipImportStatements);
  TConversionOptions = Set of TConversionOption;

  TTypescriptToPas = Class;
  TScope = Record
    Source : TJSSourceElements;
    Forwards : TStringList;
  end;

  { TTSContext }

  TTSContext = class(TObject)
  Private
    FCurrentScopeIdx: Integer;
    FTypeMap : TFPObjectHashTable;
    FTypeDeclarations : TFPObjectList;
    FConverter : TTypescriptToPas;
    FScopes : Array of TScope;
    function GetCurrentForwards: TStringList;
    function GetCurrentScope: TJSSourceElements;
  Protected
    procedure TypesToMap; virtual;
  Public
    Constructor Create(aConverter : TTypescriptToPas);
    Destructor Destroy; override;
    procedure DoGlobalFree(aEl: TJSElement);
    Procedure AddAliases(aAliases : TStrings);
    Procedure PushScope(aScope : TJSSourceElements; aForwards : TStringList);
    Procedure PopScope(aScope : TJSSourceElements; aForwards : TStringList);
    function ResolveTypeRef(D: TJSTypeDef): TJSTypeDef;
    function GetTypeName(const aTypeName: jsBase.TJSString; ForTypeDef: Boolean; UsePascal : Boolean): String;
    Function FindInNodes(aNodes: TJSElementNodes; const aName: String): TJSTypeDeclaration;
    Function FindInScope(aScope: TJSSourceElements; const aName: String): TJSTypeDef;
    Function FindTypeDef(const aName : String) : TJSTypeDef;
    Function FindTypeAlias(const aName : jsbase.TJSString) : String;
    Procedure AddToTypeMap(const aName : UTF8String; const aPasName : String);
    Procedure AddToTypeMap(const aName : jsbase.TJSString; const aPasName : String);
    Procedure AddToTypeMap(aType : TJSElement);
    Procedure RemoveFromTypeMap(aType : TJSElement);
    Property TypeMap : TFPObjectHashTable Read FTypeMap;
    Property CurrentScopeIdx : Integer Read FCurrentScopeIdx;
    Property CurrentScope : TJSSourceElements Read GetCurrentScope;
    Property CurrentForwards : TStringList Read GetCurrentForwards;
  end;

  { TTSJSScanner }

  TTSJSScanner = class(TJSScanner)
  private
    FContext: TTSContext;
  Public
    Property Context : TTSContext Read FContext Write FContext;
  end;

  { TTSJSParser }

  TTSJSParser = class(TJSParser)
  private
    FContext: TTSContext;
  Protected
    Procedure FreeElement(aElement : TJSElement); override;
    Function CreateElement(AElementClass : TJSElementClass)  : TJSElement; override;
  Public
    Property Context : TTSContext Read FContext Write FContext;
  end;

  // List of TJSTypedParams

  { TFunctionOverLoadArgumentsList }

  TFunctionOverLoadArgumentsList = Class(TFPObjectList)
    Procedure AddOverload(aTypedParams : TJSTypedParams);
    Procedure RemoveDuplicates(aContext : TTSContext);
  end;


  { TTypescriptToPas }

  TTypescriptToPas = Class(TPascalCodeGenerator)
  private
    FClassPrefix: String;
    FClassSuffix: String;
    FContext: TTSContext;
    FDefaultClassParent: String;
    FDictionaryClassParent: String;
    FElements: TJSFunctionBody;
    FFieldPrefix: String;
    FIncludeImplementationCode: TStrings;
    FIncludeInterfaceCode: TStrings;
    FInputFileName: String;
    FInputStream: TStream;
    FLinkStatements: TStrings;
    FOptions: TConversionOptions;
    FOutputFileName: String;
    FTypeAliases: TStrings;
    FVerbose: Boolean;
    FECMAVersion: TECMAVersion;
    FPasNameList : TFPObjectList;
    FScopeNameList : Array[0..16] of TFPStringHashTable;
    FScopeIdx : Integer;
    FCurrentNameSpace : String;
    FForwards : TStrings;
    procedure CheckUnitName(SourceElements: TJSSourceElements);
    procedure DumpElements;
    function GetAccessName(aAccess: TAccessibility): string;
    function GetFixedValueTypeName(ATypeDef: TJSFixedValueReference): String;
    function GetIsRaw: Boolean;
    function HasReadOnlyPropFields(aTypeDef: TJSObjectTypeDef): Boolean;
    function HaveClass(const aName: TJSString): Boolean;
    function HaveModule(const aName: TJSString): Boolean;
    function NamespaceExtendsClass(aNs: TJSNamespaceDeclaration): Boolean;
    function NamespaceExtendsModule(aNs: TJSNamespaceDeclaration): Boolean;
    function ResolveTypeRef(D: TJSTypeDef): TJSTypeDef;
    procedure SetFLinkStatements(AValue: TStrings);
    procedure SetIncludeImplementationCode(AValue: TStrings);
    procedure SetIncludeInterfaceCode(AValue: TStrings);
    procedure SetTypeAliases(AValue: TStrings);
  Protected
    function GetGenericParams(aTypeParams: TJSElementNodes): String; virtual;
    procedure AddOptionsToHeader;
    Procedure PushNameScope;
    Procedure PopNameScope;
    function NameScopeHas(const aName : string) : Boolean;
    procedure AddToNameScope(const aName : String; aData : jsbase.TJSString);
    Procedure Parse; virtual;
    Procedure WritePascal; virtual;
    Function NeedsTypeMap(El : TJSElement) : Boolean;
    function CreateParser(aContext: TTSContext; S: TJSScanner): TJSParser; virtual;
    function CreateScanner(aContext : TTSContext; S: TStream): TJSScanner;virtual;
    Function CreateContext : TTSContext; virtual;
    Function BaseUnits : String; override;
    procedure WriteLinkStatements(aList: TStrings);
    // Auxiliary routines
    procedure Getoptions(L: TStrings); virtual;
    procedure ProcessDefinitions; virtual;
    Function ExportNode(aNode : TJSElementNode) : Boolean;
    function CheckUnionTypeDefinition(D: TJSTypeDef): TJSUnionTypeDef;
    function CreatePasName(const aOriginal : jsBase.TJSString; const aName: String): TPasData;virtual;
    function TypeNeedsTypeName(aType: TJSElement; IgnoreData : Boolean; IsResultType : Boolean = False): Boolean;
    procedure AllocatePasNames(FD: TJSFuncDef; const aPrefix: String='');
    procedure AllocatePasNames(aList: TJSSourceElements; const ParentName: String=''); virtual;
    procedure AllocatePasNames(aList: TJSElementNodes; Const ParentName: String=''); virtual;
    Function AllocatePasName(D: TJSElement; Const ParentName: String='') : TPasData;virtual;
    procedure EnsureUniqueNames(ML: TJSSourceElements);virtual;
    function GetExternalMemberName(const aName: jsBase.TJSString): string;
    function GetName(ADef: TJSElement): String;virtual;
    function GetName(ADef: TJSTypedParam): String;virtual;
    function GetName(ADef: TJSFuncDef): String;virtual;
    function HaveConsts(aList: TJSSourceElements): Boolean;virtual;
    function GetTypeName(Const aTypeName: JSBase.TJSString; ForTypeDef: Boolean=False): String;virtual;
    function GetTypeName(aTypeDef: TJSTypeDef; ForTypeDef: Boolean=False): String;virtual;
    // Functions
    // Overload handling
    function GetOverloads(const aDefs: TJSFuncDefArray): TFunctionOverLoadArgumentsList;
    procedure AddOverloadParams(aList: TFunctionOverLoadArgumentsList; adef: TJSFuncDef; aIdx: Integer);
    procedure AddUnionOverloads(aList: TFunctionOverLoadArgumentsList; const AName: TJSString; UT: TJSUnionTypeDef);
    procedure AddParameterToOverloads(aList: TFunctionOverLoadArgumentsList; const AName: TJSString; ATypeDef: TJSTypeDef);
    procedure AddParameterToOverloads(aList: TFunctionOverLoadArgumentsList; const aParam : TJSTypedParam);
    function CloneNonPartialParameterList(aList: TFunctionOverLoadArgumentsList; ADest: TFunctionOverLoadArgumentsList = Nil; AsPartial: Boolean = True): integer;
    function GetArguments(aList: TJSTypedParams; ForceBrackets: Boolean): String;
    function WriteFunctionDefinition(const aName: String; const aDefs: TJSFuncDefArray; UseExternal : Boolean): Boolean;
    function WriteFunctionDefs(aElements : TJSElementNodes; UseExternal : Boolean) : Integer;

    // Classes
    // Actual definitions. Return true if a definition was written.

    function WritePrivateReadOnlyField(P: TJSPropertyDeclaration): Boolean;
    function WritePrivateReadOnlyField(M: TJSMethodDeclaration): Boolean;
    function WriteReadonlyProperty(aProp: TJSPropertyDeclaration): Boolean;
    function WritePropertyDef(aProp: TJSPropertyDeclaration): Boolean;
    function WriteReadOnlyPropFields(aTypeDef: TJSObjectTypeDef): Integer;
    function WriteAmbientClassDef(const aPasName: String; aOrgName: TJSString; aTypeParams: TJSElementNodes; aClass: TJSAmbientClassDeclarationArray): Boolean;
    function WriteClassDefs(aClasses: TJSElementNodes) : Integer;

    // Forwards
    function WriteForwardClass(const aName: string): Boolean;
    function WriteForwardClassDef(aIntf: TJSInterfaceDeclaration): Boolean;
    function WriteForwardClassDef(aObj: TJSTypeDeclaration): Boolean;
    function WriteForwardClassDef(aClass: TJSClassDeclaration): Boolean;
    function WriteForwardClassDef(aModule: TJSModuleDeclaration): Boolean;
    function WriteForwardClassDef(aNamespace: TJSNameSpaceDeclaration): Boolean;
    function WriteForwardClassDefs(aClassList: TJSElementNodes): Integer;


    Function WriteNamespaceDef(aNameSpace: TJSNamespaceDeclaration): Boolean;
    Function WriteNamespaceDefs(aNameSpaces: TJSElementNodes): Integer;

    Function WriteModuleDef(aModule: TJSModuleDeclaration): Boolean;
    Function WriteModuleDefs(aModules: TJSElementNodes): Integer;


    // Interfaces
    function WriteInterfaceDef(Intfs: TJSInterfaceDeclarationArray): Boolean;
    function WriteInterfaceDefs(aList: TJSElementNodes): Integer;

    // Properties
    procedure WritePropertyDeclaration(D: TJSVariableStatement);
    function WriteProperties(aClass: TJSClassDeclaration): Integer;
    function WriteProperties(aAccess : TAccessibility; aMembers: TJSElementNodes): Integer;
    function WriteObjectMethods(aAccess: TAccessibility; aTypeDef: TJSObjectTypeDef): Integer;
    procedure WriteIndexSignature(aSign: TJSIndexSignatureDeclaration);

    // Variables
    procedure WriteVariable(aVar: TJSVarDeclaration);
    procedure WriteVariables(Vars: TJSElementNodes); virtual;

    // Get type defs as string
    function GetTypeAsString(aType: TJSTypeDef; asPascal, asSubType: Boolean): String;
    function GetArrayTypeAsString(aTypeDef: TJSArrayTypeDef; asPascal, asSubType: Boolean): String;
    function GetAliasTypeAsString(aTypeDef: TJSTypeReference; asPascal, asSubType: Boolean): string;
    function GetIntersectionTypeAsString(aTypeDef: TJSIntersectionTypeDef; asPascal, asSubType: Boolean): String;
    function GetUnionTypeAsString(aTypeDef: TJSUnionTypeDef; asPascal, asSubType: Boolean): String;
    function GetEnumTypeAsString(aTypeDef: TJSEnumTypeDef; asPascal, asSubType: Boolean): String;
    function GetFixedValueTypeAsString(aTypeDef : TJSFixedValueReference; asPascal,asSubType : Boolean) : string;
    function GetTupleTypeAsString(aTypeDef: TJSTupleTypeDef; asPascal, asSubType: Boolean): String;
    // Write types
    procedure WriteTypeDefs(Types: TJSElementNodes); virtual;
    procedure WriteObjectTypeMembers(const aPasName: String; const aOrigName: jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSObjectTypeDef);
    procedure WriteObjectTypedef(const aPasName: String; const aOrigName : jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSObjectTypeDef); virtual;
    procedure WriteAliasTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes;  aTypeDef: TJSTypeReference); virtual;
    procedure WriteUnionTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSUnionTypeDef); virtual;
    procedure WriteTupleTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSTupleTypeDef); virtual;
    procedure WriteIntersectionTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSIntersectionTypeDef); virtual;
    procedure WriteArrayTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes;  aTypeDef: TJSArrayTypeDef); virtual;
    procedure WriteEnumTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes;  aTypeDef: TJSEnumTypeDef); virtual;
    function  WriteFunctionTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes; aDef: TJSFuncDef): Boolean; virtual;
    procedure WriteTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSTypeDef); virtual;
    // Indirect type handling
    Function HasIndirectTypeDefs(aParams: TJStypedParams): Boolean;
    Function HasIndirectTypeDefs(aElements: TJSElementNodes): Boolean;
    function AllocateIndirectTypeDef(El: TJSElement; const aPrefix, aName: String): Integer;
    Function AllocateIndirectTypeDefs(aElements : TJSElementNodes; const aPrefix : String) : Integer;
    function AllocateIndirectTypeDefs(FD: TJSFuncDef; const aPrefix: String): Integer;
    Function AllocateIndirectTypeDefs(aParams : TJSTypedParams; const aPrefix : String) : Integer;
    function AllocateTypeName(aType: TJSElement; const aPrefix, aName: String): Integer;
    function WriteIndirectTypeDefs(aEl: TJSElement): Integer;
    function WriteIndirectTypeDefs(FD: TJSFuncDef): Integer;
    function WriteIndirectTypeDefs(aParams: TJStypedParams): Integer; overload; virtual;
    Function WriteIndirectTypeDefs(aElements : TJSElementNodes) : Integer; overload; virtual;
    function WriteClassIndirectTypeDefs(aElements: TJSElementNodes; isClassLocal: Boolean): Integer;
    function WritePropertyTypeDefs(aElements: TJSElementNodes; Const SectionName: String=''): Integer;
    function WriteMethodParameterDefs(aElements: TJSElementNodes; Const SectionName : String = ''): Integer;

    // List of identifiers: global, namespace or class
    procedure WriteSourceElements(SourceElements: TJSSourceElements; aNamespace: TJSString);
    // Extra interface/Implementation code.
    procedure WriteImports(SourceElements: TJSSourceElements);
    procedure WriteImplementation; virtual;
    procedure WriteIncludeInterfaceCode; virtual;
    Property Elements : TJSFunctionBody Read FElements;
    Property Context : TTSContext Read FContext;
    Property IsRaw : Boolean Read GetIsRaw;
  Public
    Constructor Create(Aowner : TComponent); override;
    Destructor Destroy; override;
    Procedure Execute;
    Property InputStream : TStream Read FInputStream Write FInputStream;
  Published
    Property InputFileName : String Read FInputFileName Write FInputFileName;
    Property OutputFileName : String Read FOutputFileName Write FOutputFileName;
    Property Verbose : Boolean Read FVerbose Write FVerbose;
    Property FieldPrefix : String Read FFieldPrefix Write FFieldPrefix;
    Property ClassPrefix : String Read FClassPrefix Write FClassPrefix;
    Property ClassSuffix : String Read FClassSuffix Write FClassSuffix;
    Property Options : TConversionOptions Read FOptions Write FOptions;
    Property ECMAVersion : TECMAVersion Read FECMAVersion Write FECMAVersion;
    Property TypeAliases : TStrings Read FTypeAliases Write SetTypeAliases;
    Property IncludeInterfaceCode : TStrings Read FIncludeInterfaceCode Write SetIncludeInterfaceCode;
    Property IncludeImplementationCode : TStrings Read FIncludeImplementationCode Write SetIncludeImplementationCode;
    Property DictionaryClassParent : String Read FDictionaryClassParent Write FDictionaryClassParent;
    Property DefaultClassParent : String Read FDefaultClassParent Write FDefaultClassParent;
    Property LinkStatements : TStrings Read FLinkStatements Write SetFLinkStatements;
  end;

implementation

uses typinfo, strutils;

Resourcestring
  SErrorCannotPopNilScope = 'Cannot pop nil scope';
  SErrCannotPushNilScope = 'Cannot push nil scope';
  SErrCanOnlyPopToplevelScope = 'Can only pop toplevel scope/forwards';
  SErrIgnoringDuplicateTypeName = 'Ignoring duplicate type name %s -> %s (%s)';
  SErrParseResultIsNotFunctionBody = 'Parse result is not a function body';
  SErrCannotGetTypeNameFromType = 'Cannot get type name from %s at row %d, col %d.';
  SErrUnsupportedNamedParamType = 'Unsupported named type parameter: "%s"';
  ResUnsupportedTypeParameter = 'Unsupported type parameter: "%s"';
  SCommentImportFile = 'Import file : %s';
  SCommentRequiredImportFile = 'Import (require) file : ';
  SLogRenamedType = 'Renamed %s to %s';
  SLogRenamingUnitCompile = 'Renaming unit %s to %s to allow compilation.';
  SErrRenamingUnitConflict = 'Renaming unit %s to %s to avoid name conflict.';
  SLogParsedNDefinitions = 'Parsed %d type definitions.';
  SErrUnsupportedTupleElementType = 'Unsupported tuple element type: %s';
  SCommentIgnoringDuplicateType = 'Ignoring duplicate type %s (%s)';
  SErrUnsupportedType = '%s (%s) has unsupported type "%s" : ';
  SErrNoNameAllocatedForFunctionResult = 'No name allocated for function %s (%d,%d) result type %s';
  SErrElementWithoutTypeName = 'Element without allocated typename: %s %s';
  SLogFoldingClassDefinitions = 'Folding %d definitions to 1 class for %s';
  SLogIgnoringEmptyMethod = 'Ignoring empty method';
  SLogIgnoringEmptyFunction = 'Ignoring empty function definition';
  SLogIgnoreDoubleClassDefinition = 'Ignore double class definition: "%s"';
  SForwardClassDefinitions = 'Forward class definitions';
  SLogFoldingInterfaceDefinitions = 'Folding %d definitions to 1 interface for %s';

{ TFunctionOverLoadArgumentsList }

procedure TFunctionOverLoadArgumentsList.AddOverload(aTypedParams: TJSTypedParams);
begin
  Add(aTypedParams);
end;

procedure TFunctionOverLoadArgumentsList.RemoveDuplicates(aContext: TTSContext);

  Function GetName(aDef : TJSTypeDef) : TJSString;

  begin
    Result:='';
    if aDef is TJSFixedValueReference then
      begin
      Case TJSFixedValueReference(aDef).FixedValue.Value.ValueType of
        jstString : Result:='string';
        jstNumber : Result:='number';
        jstBoolean : Result:='boolean';
      else
        Result:='';
      end
      end
    else if aDef is TJSTypeReference then
      Result:=(aDef as TJSTypeReference).Name
    else if aDef is TJSUnionOrIntersectTypeDef then
      Result:='jsvalue';
  end;

  Function IdenticalTypes(Src,Dest : TJSTypeDef) : boolean;


  Var
    N1,N2 : TJSString;

  begin
    Result:=Src=Dest;
    If Result then exit;
    Src:=aContext.ResolveTypeRef(Src);
    Dest:=aContext.ResolveTypeRef(Dest);
    Result:=Src=Dest;
    if Result then
      exit;
    N1:=GetName(Src);
    N2:=GetName(Dest);
    Result:=(N1=N2) and (N1<>'')
  end;

  Function IdenticalParams(Src,Dest : TJSTypedParams) : boolean;

  Var
    I : Integer;

  begin
    Result:=(Src.Count=Dest.Count);
    I:=Src.Count-1;
    While Result and (I>=0) do
      begin
      Result:=IdenticalTypes(Src.Types[i] as TJSTypeDef,Dest.Types[i] as TJSTypeDef);
      Dec(I);
      end;
  end;

  Function HasDuplicate(MaxIndex : Integer; aParamList :TJSTypedParams) : Boolean;

  Var
    I : Integer;

  begin
    Result:=False;
    I:=MaxIndex;
    While (Not Result) and (I>=0) do
      begin
      Result:=IdenticalParams(Items[i] as TJSTypedParams, aParamList);
      Dec(I);
      end
  end;

Var
  I : Integer;

begin
  For I:=Count-1 downto 1 do
    If HasDuplicate(I-1,Items[I] as TJSTypedParams) then
      Delete(I);
end;

{ TTSJSParser }
Procedure TTSJSParser.FreeElement(aElement : TJSElement);

begin
  if Assigned(aElement) then
    FContext.RemoveFromTypeMap(aElement);
  Inherited;
end;


function TTSJSParser.CreateElement(AElementClass: TJSElementClass): TJSElement;
begin
  Result:=inherited CreateElement(AElementClass);
  If Result is TJSTypeDeclaration then
    FContext.AddToTypeMap(Result)
  else If Result is TJSObjectTypeDef then
    FContext.AddToTypeMap(Result)
  else If (Result is TJSClassDeclaration) then
    FContext.AddToTypeMap(Result);
end;

{ TTSContext }

constructor TTSContext.Create(aConverter : TTypescriptToPas);
begin
  TJSElement.GlobalFreeHook:=@DoGlobalFree;
  FCurrentScopeIdx:=-1;
  FConverter:=aConverter;
  FTypeMap:=TFPObjectHashTable.Create(False);
  FTypeDeclarations:=TFPObjectList.Create(False);
  SetLength(FScopes,10);
end;

destructor TTSContext.Destroy;

begin
  TJSElement.GlobalFreeHook:=Nil;
  FreeAndNil(FTypeDeclarations);
  FreeAndNil(FTypeMap);
  inherited Destroy;
end;

procedure TTSContext.AddAliases(aAliases: TStrings);

Var
  I : Integer;
  N,V : String;

begin
  For I:=0 to aAliases.Count-1 do
    begin
    aAliases.GetNameValue(I,N,V);
    if FTypeMap.Find(UTF8String(N))=Nil then
      AddToTypeMap(UTF8String(N),V);
    end;
end;

procedure TTSContext.PushScope(aScope: TJSSourceElements; aForwards : TStringList);
begin
  if aScope=Nil then
    raise ETSToPas.Create(SErrCannotPushNilScope);
  Inc(FCurrentScopeIdx);
  if FCurrentScopeIdx>=Length(FScopes) then
    SetLength(FScopes,Length(FScopes)*2);
  FScopes[FCurrentScopeIdx].Source:= aScope;
  FScopes[FCurrentScopeIdx].Forwards:=aForwards;
end;


procedure TTSContext.PopScope(aScope: TJSSourceElements; aForwards : TStringList);
begin
  if (aScope=Nil) then
    Raise ETSToPas.Create(SErrorCannotPopNilScope);
  if (aScope<>CurrentScope) or (aForwards<>CurrentForwards) then
    raise ETSToPas.Create(SErrCanOnlyPopToplevelScope);
  Dec(FCurrentScopeIdx);
end;

function TTSContext.ResolveTypeRef(D: TJSTypeDef): TJSTypeDef;
begin
  Result:=D;
  While Result is TJSTypeReference do
    Result:=FindTypeDef(UTF8Encode((Result as TJSTypeReference).Name));
  if Result=Nil then
    Result:=D;
end;

function TTSContext.GetTypeName(const aTypeName: jsBase.TJSString; ForTypeDef: Boolean; UsePascal: Boolean): String;

  Function UsePascalType(Const aPascalType : string) : String;

  begin
    if UsePascal and ForTypeDef then
      Result:=StringReplace(UTF8Encode(aTypeName),' ','',[rfReplaceAll])
    else
      Result:=aPascalType;
  end;

Var
  TN : UTF8String;

begin
  Case aTypeName of
    'union': TN:='JSValue';
    'short': TN:=UsePascalType('Integer');
    'long': TN:=UsePascalType('Integer');
    'long long': TN:=UsePascalType('NativeInt');
    'unsigned short': TN:=UsePascalType('Cardinal');
    'unrestricted float': TN:=UsePascalType('Double');
    'unrestricted double': TN:=UsePascalType('Double');
    'unsigned long': TN:=UsePascalType('NativeInt');
    'unsigned long long': TN:=UsePascalType('NativeInt');
    'octet': TN:=UsePascalType('Byte');
    'any' : TN:=UsePascalType('JSValue');
    'number' : TN:=UsePascalType('Double');
    'float' : TN:=UsePascalType('Double');
    'double' : TN:=UsePascalType('Double');
    'DOMString',
    'USVString',
    'ByteString' : TN:=UsePascalType('String');
    'object' : TN:=UsePascalType('TJSObject');
    'Error' : TN:=UsePascalType('TJSError');
    'DOMException' : TN:=UsePascalType('TJSError');
    'ArrayBuffer',
    'DataView',
    'Int8Array',
    'Int16Array',
    'Int32Array',
    'Uint8Array',
    'Uint16Array',
    'Uint32Array',
    'Uint8ClampedArray',
    'Float32Array',
    'Float64Array' : TN:='TJS'+UTF8Encode(aTypeName);
  else
    TN:=FindTypeAlias(aTypeName);
  end;
  Result:=TN;
end;

Function TTSContext.FindInNodes(aNodes : TJSElementNodes; const aName: String) : TJSTypeDeclaration;

Var
  I : integer;
  N : TJSString;


begin
  Result:=Nil;
  N:=UTF8Decode(aName);
  I:=aNodes.Count-1;
  While (Result=Nil) and (I>=0) do
    begin
    If aNodes[i].Node is TJSTypeDeclaration then
      begin
      Result:=aNodes[i].Node as TJSTypeDeclaration;
      if Result.Name<>N then
        Result:=Nil;
      end;
    Dec(I);
    end;
end;

function TTSContext.FindInScope(aScope : TJSSourceElements; const aName: String): TJSTypeDef;

Var
  Decl :TJSTypeDeclaration;

begin
  Result:=Nil;
  Decl:=FindInNodes(aScope.Enums,aName);
  if Decl=Nil then
    Decl:=FindInNodes(aScope.Types,aName);
  if Decl=Nil then
    Decl:=FindInNodes(aScope.Classes,aName);
  if Decl=Nil then
    Decl:=FindInNodes(aScope.Interfaces,aName);
  if Decl<>Nil then
    Result:=Decl.TypeDef;
end;

function TTSContext.FindTypeDef(const aName: String): TJSTypeDef;

Var
  I : Integer;

begin
  Result:=Nil;
  I:=FCurrentScopeIdx;
  While (Result=nil) and (I>=0) do
    begin
    Result:=FindInscope(FScopes[i].Source,aName);
    Dec(I);
    end;
end;

function TTSContext.FindTypeAlias(const aName: jsbase.TJSString): String;

Var
  S : UTF8String;
  Parts : TStringArray;
  Obj : TObject;

begin
  Result:='';
  if FTypeDeclarations.Count>0 then
    TypesToMap;
  S:=UTF8Encode(aName);
  Parts:=SplitString(S,'.');
  For S in Parts do
    begin
    Obj:=FTypeMap.Items[S];
    if Result<>'' then
      Result:=Result+'.';
    if (Obj is TPasData) then
      Result:=Result+TPasData(Obj).PasName
    else
      Result:=Result+S;
    end;
end;

procedure TTSContext.TypesToMap;

Var
  I : Integer;
  el : TJSElement;
  N : String;

begin
  For I:=0 to FTypeDeclarations.Count-1 do
    begin
    El:=TJSElement(FTypeDeclarations[i]);
    if El.Data=Nil then
      begin
      FConverter.AllocatePasName(El,'');
      end;
    if EL.Data<>Nil then
      begin
      if FConverter.NeedsTypeMap(El) then
        begin
        N:=UTF8Encode(TPasData(El.Data).OriginalName);
        if FTypeMap.Find(N)<>Nil then
          FConverter.DoLog(SErrIgnoringDuplicateTypeName, [N, TPasData(El.Data).PasName, EL.ClassName])
        else
          FTypeMap.Add(N,El.Data) ;
        end;
      end;
    end;
  FTypeDeclarations.Clear;
end;

function TTSContext.GetCurrentScope: TJSSourceElements;

begin
  if CurrentScopeIdx>=0 then
    Result:=FScopes[CurrentScopeIdx].Source
  else
    Result:=Nil;
end;

function TTSContext.GetCurrentForwards: TStringList;
begin
  if CurrentScopeIdx>=0 then
    Result:=FScopes[CurrentScopeIdx].Forwards
  else
    Result:=Nil;
end;

procedure TTSContext.DoGlobalFree(aEl: TJSElement);
begin
  FTypeDeclarations.Extract(aEl);
end;

procedure TTSContext.AddToTypeMap(const aName: UTF8String; const aPasName: String);
begin
  FTypeMap.Add(aName,FConverter.CreatePasName(UTF8Decode(aName),aPasName));
end;

procedure TTSContext.AddToTypeMap(const aName: jsbase.TJSString; const aPasName: String);
begin
  AddToTypeMap(UTF8Encode(aName),aPasName);
end;

procedure TTSContext.AddToTypeMap(aType: TJSElement);
begin
//  Writeln('aType : ',FTypeDeclarations.Count,': ',aType.Classname);
  FTypeDeclarations.Add(aType);
end;

procedure TTSContext.RemoveFromTypeMap(aType: TJSElement);
begin
//  Writeln('Removing : ',FTypeDeclarations.Count,': ',aType.Classname, ' at ',FTypeDeclarations.IndexOf(aTYpe));
  FTypeDeclarations.Extract(aType);
end;

{ TPasData }

constructor TPasData.Create(const aOriginalName : jsBase.TJSString; const APasName : String);
begin
  FOriginalName:=aOriginalName;
  FPasName:=APasName;
end;

destructor TPasData.destroy;
begin
  // Writeln('Destroying ',Self.FOriginalName,'->',Self.Pasname);
  inherited destroy;
end;

{ TTypescriptToPas }

function TTypescriptToPas.CreateContext: TTSContext;
begin
  Result:=TTSContext.Create(Self);
end;

function TTypescriptToPas.CreateScanner(aContext : TTSContext; S : TStream) :  TJSScanner;

begin
  Result:=TTSJSScanner.Create(S,FECMAVersion);
  Result.IsTypeScript:=True;
end;

function TTypescriptToPas.CreateParser(aContext : TTSContext;S : TJSScanner) :  TJSParser;

begin
  Result:=TTSJSParser.Create(S);
  TTSJSParser(Result).Context:=aContext;
end;

procedure TTypescriptToPas.DumpElements;

  Procedure DumpNodes(Const aSection : String; aList: TJSElementNodes);
  Var
    I : Integer;
    N : TJSElementNode;

  begin
    {AllowWriteln}
    Writeln(aSection,': ',aList.Count,' elements');
    For I:=0 to aList.Count-1 do
      begin
      N:=Alist[i];
      Writeln(aSection,' element ',I,' : ',N.Node.ClassName);
      end;
    {AllowWriteln-}
  end;

Var
  Els : TJSSourceElements;

begin
  Els:=FElements.A as TJSSourceElements;
  DumpNodes('vars',Els.Vars);
  DumpNodes('statements',Els.Statements);
  DumpNodes('classes',Els.Classes);
  DumpNodes('types',Els.Types);
  DumpNodes('enums',Els.Enums);
  DumpNodes('functions',Els.Functions);
  DumpNodes('namespaces',Els.Namespaces);
  DumpNodes('modules',Els.Modules);
end;

function TTypescriptToPas.ResolveTypeRef(D: TJSTypeDef): TJSTypeDef;

begin
  Result:=Context.ResolveTypeRef(D);
end;

procedure TTypescriptToPas.SetFLinkStatements(AValue: TStrings);
begin
  if FLinkStatements=AValue then Exit;
  FLinkStatements.Assign(AValue);
end;

function TTypescriptToPas.CheckUnionTypeDefinition(D: TJSTypeDef): TJSUnionTypeDef;

begin
  Result:=Nil;
  D:=ResolveTypeRef(D);
  If (D is TJSUnionTypeDef) then
    Result:=D as TJSUnionTypeDef;
end;


procedure TTypescriptToPas.Parse;

Var
  F : TStream;
  S : TJSScanner;
  P : TJSParser;
  El : TJSElement;

begin
  FreeAndNil(FElements); // In case parse is called multiple times
  P:=Nil;
  F:=InputStream;
  if (F=Nil) then
    F:=TFileStream.Create(InputFileName,fmOpenRead or fmShareDenyWrite);
  try
    S:=CreateScanner(Context,F);
    P:=CreateParser(Context,S);
    El:=P.Parse;
    if not (El is TJSFunctionBody) then
      begin
      EL.Free;
      raise ETStoPas.Create(SErrParseResultIsNotFunctionBody);
      end;
    FElements:=El as TJSFunctionBody;
    // DumpElements;
  finally
    P.Free;
    S.Free;
    if F<>InputStream then
      F.Free;
  end;
end;

function TTypescriptToPas.GetExternalMemberName(const aName : jsBase.TJSString) : string;

begin
  if FCurrentNameSpace<>'' then
    Result:=FCurrentNameSpace+'.'+UTF8Encode(aName)
  else
    Result:=UTF8Encode(aName);
end;

function TTypescriptToPas.GetName(ADef: TJSElement): String;

begin
  If Assigned(ADef) and (TObject(ADef.Data) is TPasData) then
    Result:=TPasData(ADef.Data).PasName
  else if aDef is TJSNamedElement then
    Result:=EscapeKeyWord(UTF8Encode(TJSNamedElement(ADef).Name))
  else
    Result:='';

end;

function TTypescriptToPas.GetName(ADef: TJSTypedParam): String;
begin
  Result:=EscapeKeyWord(UTF8Encode(aDef.Name));
end;

function TTypescriptToPas.GetName(ADef: TJSFuncDef): String;
begin
  Result:=EscapeKeyWord(UTF8Encode(aDef.Name));
end;

function TTypescriptToPas.HaveConsts(aList: TJSSourceElements): Boolean;

Var
  I : Integer;
  D : TJSVariableStatement;

begin
  Result:=False;
  For I:=0 to aList.Vars.Count-1 do
    begin
    D:=aList.Vars[i].Node as TJSVariableStatement;
    if (D.VarType=vtConst) then
      Exit(True);
    end;
end;

function TTypescriptToPas.GetTypeName(const aTypeName: jsBase.TJSString; ForTypeDef: Boolean): String;


begin
  Result:=Context.GetTypeName(aTypeName,ForTypeDef,(coUseNativeTypeAliases in Options));
end;

function TTypescriptToPas.GetFixedValueTypeName(ATypeDef : TJSFixedValueReference) : String;

begin
  if Not (Assigned(ATypeDef.FixedValue) and Assigned(ATypeDef.FixedValue.Value)) then
    Result:='JSValue'
  else
    Case ATypeDef.FixedValue.Value.ValueType of
      jstBoolean : Result:='Boolean';
      jstNumber : Result:='Double';
      jstString : Result:='String';
      jstObject : Result:='TJSObject';
    else
      Result:='JSValue';
    end;
end;

function TTypescriptToPas.GetTypeName(aTypeDef : TJSTypeDef; ForTypeDef : Boolean = False): String;

Var
  S : jsbase.TJSString;

begin
  if (aTypeDef.Data is TPasData) then
    Result:=TPasData(aTypeDef.Data).PasName
  else if ATypeDef is TJSTypeReference then
    begin
    S:=TJSTypeReference(aTypeDef).Name;
    Result:=GetTypeName(S,ForTypeDef)
    end
  else if ATypeDef is TJSArrayTypeDef then
    Result:='array of '+GetTypeName(TJSArrayTypeDef(aTypeDef).BaseType,ForTypeDef)
  else if ATypeDef is TJSUnionOrIntersectTypeDef then
    Result:='jsvalue'
  else if ATypeDef is TJSGenericTypeRef then
    Result:=GetTypeName(TJSGenericTypeRef(aTypeDef).BaseType,ForTypeDef)
  else if ATypeDef is TJSArrowFunctionTypeDef then
    Result:='procedure'
  else if ATypeDef is TJSFixedValueReference then
    Result:=GetFixedValueTypeName(ATypeDef as TJSFixedValueReference)
  else
    raise ETSToPas.CreateFmt(SErrCannotGetTypeNameFromType, [aTypeDef.ClassName, aTypeDef.Line, aTypeDef.Column]);
end;


function TTypescriptToPas.WriteProperties(aClass: TJSClassDeclaration): Integer;

Var
  I : Integer;
  D : TJSVariableStatement;

begin
  Result:=0;
  For I:=0 to aClass.Members.Vars.Count-1 do
    if ExportNode(aClass.Members.Vars[i]) then
      begin
      D:=aClass.Members.Vars[i].Node as TJSVariableStatement;
      if (D.VarType=vtVar) then
        begin
        WritePropertyDeclaration(D);
        Inc(Result);
        end;
      end;
end;

function TTypescriptToPas.GetAccessName(aAccess : TAccessibility) : string;

Const
  AccessNames : Array[TAccessibility] of string
              = ('','Private','Protected','Public');

begin
  Result:=AccessNames[aAccess];
end;

function TTypescriptToPas.WriteProperties(aAccess: TAccessibility; aMembers: TJSElementNodes): Integer;

Var
  EN : TJSElementNode;
  P : TJSPropertyDeclaration;
  OK : Boolean;

begin
  Result:=0;
  For EN in aMembers do
   begin
   if EN.Node is TJSPropertyDeclaration then
     begin
     P:=TJSPropertyDeclaration(EN.Node);
     if (P.Accessibility=aAccess) then
       begin
       if P.IsReadOnly then
         OK:=WriteReadOnlyProperty(P)
       else
         OK:=WritePropertyDef(P);
       if Ok then
         Inc(Result);
       end;
     end;
   end;
end;


function TTypescriptToPas.GetGenericParams(aTypeParams: TJSElementNodes) : String;

Var
  I : Integer;
  aName: jsBase.TJSString;
  N : TJSTypeDef;

begin
  Result:='';
  if aTypeParams=nil then exit;
  For I:=0 to aTypeParams.Count-1 do
    if (aTypeParams[i].Node is TJSTypeReference) then
      begin
      aName:=(aTypeParams[i].Node as TJSTypeReference).Name;
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+UTF8Encode(aName);
      end
    else if (aTypeParams[i].Node is TJSNamedParamTypeDef) then
      begin
      N:=(aTypeParams[i].Node as TJSNamedParamTypeDef).ParamName;
      if (N is TJSTypeReference) then
        aName:=(N as TJSTypeReference).Name
      else
        raise ETSToPas.CreateFmt(SErrUnsupportedNamedParamType, [ATypeParams[I].Node.ClassName]);
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+UTF8Encode(aName);
      end
    else
      raise ETSToPas.CreateFmt(ResUnsupportedTypeParameter, [ATypeParams[I].Node.ClassName]);
  if Result<>'' then
    Result:='<'+Result+'>';
end;

Function TTypescriptToPas.GetAliasTypeAsString(aTypeDef : TJSTypeReference; asPascal, asSubType: Boolean) : string;

begin
  if asPascal then
    Result:=GetTypeName(aTypeDef.Name,True)
  else
    Result:=UTF8Encode(aTypeDef.Name);
end;

procedure TTypescriptToPas.WriteAliasTypeDef(const aPasName : string; const aOrgName : jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef : TJSTypeReference);

Var
  TN, gen, genparams: String;

begin
  TN:=GetAliasTypeAsString(aTypeDef,True,False);
  genparams:=GetGenericParams(aTypeParams);
  if (genparams<>'') then
    gen:='generic ';
  AddLn('%s%s%s = %s;',[gen,aPasName,genparams,TN]);
end;

procedure TTypescriptToPas.WriteImplementation;
begin

end;

Procedure TTypescriptToPas.WritePropertyDeclaration(D : TJSVariableStatement);

begin

end;

procedure TTypescriptToPas.Getoptions(L : TStrings);

Var
  S : String;
  I : Integer;

begin
  L.Add('Automatically generated file by '+ClassName+' on '+FormatDateTime('yyyy-mm-dd hh:nn:ss',Now));
  L.Add('');
  L.Add('Used command-line options : ');
  For I:=1 to ParamCount do
    L.Add(ParamStr(i));
  L.Add('');
  L.Add('Command-line options translate to: ');
  L.Add('');
  S:=SetToString(PtypeInfo(TypeInfo(TConversionOptions)),Integer(OPtions),True);
  L.Add('Options : '+S);
  L.Add('Keyword prefix : '+KeywordPrefix);
  L.Add('Keyword suffix : '+KeywordSuffix);
  L.Add('Class prefix : '+ClassPrefix);
  L.Add('Class suffix : '+ClassSuffix);
  L.Add('Field prefix : '+FieldPrefix);
  Str(ECMAversion,S);
  L.Add('ECMALversion : '+S);
  if TypeAliases.Count>0 then
    begin
    L.Add('Type aliases:');
    L.AddStrings(Self.TypeAliases);
    end;
end;

procedure TTypescriptToPas.AddOptionsToHeader;

Var
  L : TStrings;
begin
  L:=TStringList.Create;
  try
    GetOptions(L);
    Comment(L);
  finally
    L.Free;
  end;
end;

procedure TTypescriptToPas.PushNameScope;
begin
  Inc(FScopeIdx);
  FScopeNameList[FScopeIdx]:=TFPStringHashTable.Create;
end;

procedure TTypescriptToPas.PopNameScope;
begin
  if FScopeIdx<0 then
    exit;
  FreeAndNil(FScopeNameList[FScopeIdx]);
  Dec(FScopeIdx);

end;

function TTypescriptToPas.NameScopeHas(const aName: string): Boolean;
begin
  Result:=FScopeIdx>=0;
  if Result then
    Result:=Assigned(FScopeNameList[FScopeIdx].Find(aName));
end;

procedure TTypescriptToPas.AddToNameScope(const aName: String; aData: jsbase.TJSString);
begin
  if FScopeIdx>=0 then
    FScopeNameList[FScopeIdx].Add(aName,UTF8Encode(aData));
end;

procedure TTypescriptToPas.WriteIncludeInterfaceCode;

Var
  S : String;

begin
  For S in IncludeInterfaceCode do
    Addln(S);
end;

constructor TTypescriptToPas.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  ECMaVersion:=ecma2021;
  FieldPrefix:='F';
  ClassPrefix:='T';
  ClassSuffix:='';
  Switches.Add('modeswitch externalclass');
  FTypeAliases:=TStringList.Create;
  TStringList(FTypeAliases).Sorted:=true;
  TStringList(FTypeAliases).Duplicates:=dupIgnore;
  FPasNameList:=TFPObjectList.Create(True);
  FIncludeInterfaceCode:=TStringList.Create;
  FIncludeImplementationCode:=TStringList.Create;
  FLinkStatements:=TStringList.Create;
  FForwards:=TStringList.Create;
  DefaultClassParent:='TJSObject';
  FOptions:=[];
end;


destructor TTypescriptToPas.Destroy;
begin
  FreeAndNil(FForwards);
  FreeAndNil(FLinkStatements);
  FreeAndNil(FElements);
  FreeAndNil(FIncludeInterfaceCode);
  FreeAndNil(FIncludeImplementationCode);
  FreeAndNil(FTypeAliases);
  FreeAndNil(FPasNameList);
  inherited Destroy;
end;


procedure TTypescriptToPas.WriteVariable(aVar : TJSVarDeclaration);

Var
  Src,aPasName,aTypeName: String;
  aExportName : TJSString;

begin
  aPasName:=GetName(aVar);
  aExportName:=aVar.Name;
  aTypeName:=GetTypeName(aVar.Typed,False);
  Src:=aPasName + ' : '+aTypeName+';';
  Src:=Src+' external name '''+Utf8Encode(aExportName)+''';';
  AddLn(Src);
end;

procedure TTypescriptToPas.WriteVariables(Vars : TJSElementNodes);

Var
  I : Integer;

begin
  For I:=0 to Vars.Count-1 do
    if ExportNode(Vars.Nodes[i]) then
      WriteVariable(Vars.Nodes[i].Node as TJSVarDeclaration);
end;

procedure TTypescriptToPas.WriteSourceElements(SourceElements : TJSSourceElements; aNamespace : TJSString);

Var
  NS : String;
  HasTypes : Boolean;
  Written : Integer;
  Fwds : TStringList;

begin
  NS:=FCurrentNameSpace;
  Fwds:=TStringList.Create;
  try
    if (FCurrentNameSpace<>'') then
      FCurrentNameSpace:=FCurrentNameSpace+'.';
    FCurrentNameSpace:=FCurrentNameSpace+NS;
    Context.PushScope(SourceElements,Fwds);
    HasTypes:=(SourceElements.Types.Count>0) or (SourceElements.Enums.Count>0);
    HasTypes:=HasTypes or (SourceElements.Namespaces.Count>0) or (SourceElements.Modules.Count>0);
    HasTypes:=HasTypes or (SourceElements.Classes.Count>0) or (SourceElements.Interfaces.Count>0);
    HasTypes:=HasTypes or HasIndirectTypeDefs(SourceElements.Functions);
    HasTypes:=HasTypes or HasIndirectTypeDefs(SourceElements.Types);
    HasTypes:=HasTypes or HasIndirectTypeDefs(SourceElements.Vars);
    if HasTypes then
      begin
      EnsureSection(csType);
      Indent;
      Written:=WriteForwardClassDefs(SourceElements.Interfaces);
      Written:=Written+WriteForwardClassDefs(SourceElements.Classes);
      Written:=Written+WriteForwardClassDefs(SourceElements.Namespaces);
      Written:=Written+WriteForwardClassDefs(SourceElements.Modules);
      Written:=Written+WriteForwardClassDefs(SourceElements.Types); // object types
      If Written>0 then
        AddLn('');
      WriteIndirectTypeDefs(SourceElements.Types);
      WriteIndirectTypeDefs(SourceElements.Vars);
      WriteTypeDefs(SourceElements.Types);
      WriteTypeDefs(SourceElements.Enums);
      WriteIndirectTypeDefs(SourceElements.Functions);
      WriteClassDefs(SourceElements.Classes);

      //
      WriteNamespaceDefs(SourceElements.Namespaces);
      WriteModuleDefs(SourceElements.Modules);
      WriteInterfaceDefs(SourceElements.Interfaces);
      {
      WriteEnumDefs(Context.Definitions);
      WriteCallbackDefs(Context.Definitions);
      WriteDictionaryDefs(Context.Definitions);
      }
      Undent;
      AddLn('');
      end;
    if SourceElements.Vars.Count>0 then
      begin
      EnsureSection(csVar);
      Indent;
      WriteVariables(SourceElements.Vars);
      Undent;
      end;
    if SourceElements.Functions.Count>0 then
      begin
      WriteFunctionDefs(SourceElements.Functions,aNameSpace='');
      end;

  finally
    Context.PopScope(SourceElements,fwds);
    Fwds.Free;
    FCurrentNamespace:=NS;
  end;
end;

procedure TTypescriptToPas.WriteLinkStatements(aList : TStrings);

Var
  i : Integer;

begin
  For I:=0 to aList.Count-1 do
   AddLn('{$linklib '+aList[i]+'}');
end;

procedure TTypescriptToPas.WriteImports(SourceElements : TJSSourceElements);

Var
  I : integer;
  Imps : TJSImportStatement;
  PE : TJSPrimaryExpressionIdent;
  CE : TJSCallExpression;

begin
  For I:=0 to SourceElements.Statements.Count-1 do
   if SourceElements.Statements[i].Node is TJSImportStatement then
     begin
     Imps:=TJSImportStatement(SourceElements.Statements[i].Node);
     if (Imps.Expression is TJSCallExpression) then
       begin
       CE:=Imps.Expression as TJSCallExpression;
       if CE.Expr is TJSPrimaryExpressionIdent then
         begin
         PE:=CE.Expr as TJSPrimaryExpressionIdent;
         if (Pe.Name='require')
             and (CE.Args.Count=1)
             and (CE.Args.Elements[0].Expr is TJSLiteral) then
           begin
             Comment(SCommentRequiredImportFile+Utf8Encode((CE.Args.Elements[0].expr as TJSLiteral).Value.AsString));
           end;
         end;
       end
     else
       Comment(Format(SCommentImportFile, [Imps.ModuleName]))
     end;
end;

procedure TTypescriptToPas.WritePascal;

Var
  SourceElements : TJSSourceElements;

begin
  SourceElements:=FElements.A as TJSSourceElements;
  if Not IsRaw then
    begin
    CreateUnitClause;
    if not (coSkipImportStatements in Options) then
      WriteImports(SourceElements);
    CreateHeader;
    if coaddOptionsToheader in Options then
      AddOptionsToHeader;
    Addln('{$INTERFACES CORBA}');
    WriteLinkStatements(FLinkStatements);
    end;
  WriteSourceElements(SourceElements,'');
  if not IsRaw then
    begin
    WriteIncludeInterfaceCode;
    Addln('');
    AddLn('implementation');
    WriteImplementation;
    AddLn('end.');
    end;
  if OutputFileName<>'' then
    Source.SaveToFile(OutputFileName);
end;

function TTypescriptToPas.NeedsTypeMap(El: TJSElement): Boolean;
begin
  Result:=true;
  if El is TJSInterfaceDeclaration then
    Result:=not HaveClass(TJSInterfaceDeclaration(El).Name)
  else if El is TJSNameSpaceDeclaration then
    Result:=not (HaveClass(TJSNameSpaceDeclaration(El).Name)
                 or HaveModule(TJSNameSpaceDeclaration(El).Name))
end;

function TTypescriptToPas.BaseUnits: String;

begin
  Result:='SysUtils, JS'
end;

function TTypescriptToPas.CreatePasName(const aOriginal: jsBase.TJSString; const aName: String): TPasData;


begin
  Result:=TPasData.Create(aOriginal,aName);
  FPasNameList.Add(Result);
end;

function TTypescriptToPas.AllocatePasName(D: TJSElement; const ParentName: String): TPasData;

Var
  Org : TJSString;
  lParentName,
  CN : String;
  CD : TJSClassDeclaration absolute D;
  AD : TJSAmbientClassDeclaration absolute D;
  ID : TJSInterfaceDeclaration absolute D;
  VD : TJSVarDeclaration absolute D;
  TD : TJSTypeDeclaration absolute D;
  FS : TJSFunctionStatement absolute D;
  ND : TJSNameSpaceDeclaration absolute D;
  MD : TJSModuleDeclaration absolute D;
  OE : TJSObjectTypeElementDef absolute D;
  OO : TJSObjectTypeDef absolute D;

begin
  Result:=Nil;
  if D Is TJSAmbientClassDeclaration then
    begin
    Org:=AD.Name;
    CN:=ClassPrefix+UTF8Encode(Org)+ClassSuffix;
    Result:=CreatePasname(Org,CN);
    AllocatePasNames(AD.ClassDef.Values,UTF8Encode(AD.Name));
    end
  else if D Is TJSClassDeclaration then
    begin
    Org:=CD.Name;
    CN:=ClassPrefix+UTF8Encode(Org)+ClassSuffix;
    Result:=CreatePasname(Org,CN);
    AllocatePasNames(CD.members,UTF8Encode(CD.Name));
    end
  else if D Is TJSInterfaceDeclaration then
    begin
    Org:=ID.Name;
    CN:=ClassPrefix+UTF8Encode(Org)+ClassSuffix;
    Result:=CreatePasname(Org,CN);
    AllocatePasNames(ID.Values,EscapeKeyWord(UTF8Encode(ID.Name)));
    end
  else if D Is TJSVarDeclaration then
    begin
    Org:=VD.Name;
    Result:=CreatePasName(Org, EscapeKeyWord(UTF8Encode(Org)));
    end
  else if D Is TJSFunctionStatement then
    begin
    Org:=FS.aFunction.Name;
    Result:=CreatePasName(Org, EscapeKeyWord(UTF8Encode(Org)));
    end
  else if D Is TJSTypeDeclaration then
    begin
    Org:=TD.Name;
    Result:=CreatePasName(Org, EscapeKeyWord('T'+UTF8Encode(Org)));
    end
  else if D Is TJSNameSpaceDeclaration then
    begin
    Org:=UTF8Decode(ClassPrefix)+ND.Name+UTF8Decode(ClassSuffix);
    Result:=CreatePasName(Org, EscapeKeyWord(UTF8Encode(Org)));
    end
  else if D Is TJSModuleDeclaration then
    begin
    Org:=UTF8Decode(ClassPrefix)+MD.Name+UTF8Decode(ClassSuffix);
    Result:=CreatePasName(Org, EscapeKeyWord(UTF8Encode(Org)));
    end
  else if D Is TJSObjectTypeElementDef then
    begin
    Org:=OE.Name;
    Result:=CreatePasName(Org, EscapeKeyWord(UTF8Encode(Org)));
    end
  else if D Is TJSObjectTypeDef then
    begin
    Org:=OO.Name;
    if Org<>'' then
      Result:=CreatePasName(Org, EscapeKeyWord(UTF8Encode(Org)));
    end
  else
    Raise ETSToPas.CreateFmt('Unsupported type to get name from: "%s"',[D.ClassName]);
  D.Data:=Result;
  if Verbose and (Result<>Nil) and (Result.PasName<>UTF8Encode(Org)) then
    begin
    lParentName:=ParentName;
    if (lParentName<>'') then
      lParentName:=lParentName+'.';
    DoLog(SLogRenamedType, [lParentName+UTF8Encode(Org), TPasData(D.Data).PasName]);
    end;
end;

Function TTypescriptToPas.TypeNeedsTypeName(aType: TJSElement; IgnoreData : Boolean; IsResultType : Boolean = False): Boolean;

begin
  if (aType=Nil) then // For example a parameter can have no type.
    exit(False);
  Result:=IgnoreData or (aType.Data=Nil);
  if Result then
    Result:=(aType is TJSArrowFunctionTypeDef)
            or (aType is TJSObjectTypeDef)
            or (aType is TJSTupleTypeDef)
            or ((aType is TJSArrayTypeDef)
                 and (IsResultType or TypeNeedsTypeName(TJSArrayTypeDef(aType).BaseType,IgnoreData,True)));
end;

Function TTypescriptToPas.AllocateTypeName(aType: TJSElement; const aPrefix,aName : String): Integer;

Var
  aTypeName : String;

begin
  Result:=1;
  aTypeName:=aPrefix+aName;
  // Writeln('AITD Typename : ',aPrefix,', Parn: ',UTF8Decode(aName), ' Typen : ',aTypeName,' esc : ',EscapeKeyWord('T'+aTypeName));
  aType.Data:=CreatePasName(UTF8Decode(aName), EscapeKeyWord('T'+aTypeName));
end;


function TTypescriptToPas.AllocateIndirectTypeDef(El : TJSElement; const aPrefix,aName : String) : Integer;

var
  FD : TJSFuncDef;
  SubPrefix : String;
begin
  // Writeln('AITD element: ',El.ClassName,' Prefix: ',aPrefix);
  SubPrefix:=aPrefix;
  if aName<>'' then
    SubPrefix:=SubPrefix+aName+'_';
  Result:=0;
  if (el is TJSArrowFunctionTypeDef) then
    begin
    if el.Data=Nil then
      AllocateTypeName(El,aPrefix,aName);
    FD:=TJSArrowFunctionTypeDef(El).aFunction;
    Result:=AllocateIndirectTypeDefs(FD,SubPrefix);
    end
  else if (el is TJSObjectTypeDef) then
    begin
    Inc(Result);
    if el.Data=Nil then
      AllocateTypeName(El,aPrefix,aName);
    Result:=Result+AllocateIndirectTypeDefs(TJSObjectTypeDef(El).Values,SubPrefix);
    end
  else if (el is TJSTupleTypeDef) then
    begin
    Inc(Result);
    AllocateTypeName(El,aPrefix,aName);
    end
  else if (el is TJSArrayTypeDef) then
    begin
    Inc(Result);
    if TypeNeedsTypeName(TJSArrayTypeDef(el).BaseType,False,True) then
       Result:=Result+AllocateIndirectTypeDef(TJSArrayTypeDef(el).BaseType,SubPrefix,'Item');
    AllocateTypeName(El,aPrefix,aName);
    end;
end;

function TTypescriptToPas.AllocateIndirectTypeDefs(aElements: TJSElementNodes; const aPrefix : String): Integer;

var
  PD : TJSPropertyDeclaration;
  VD : TJSVarDeclaration;
  EN : TJSElementNode;
  FD : TJSFuncDef;

begin
  Result:=0;
  // Writeln('AITD List, prefix : ',aPrefix);
  For EN in aElements do
    begin
    FD:=Nil;
    if EN.Node is TJSFunctionStatement then
      begin
      FD:=TJSFunctionStatement(EN.Node).AFunction;
      AllocateIndirectTypeDefs(FD,aPrefix);
      end
    else if EN.Node is TJSMethodDeclaration then
      begin
      FD:=TJSMethodDeclaration(EN.Node).FuncDef;
      AllocateIndirectTypeDefs(FD,aPrefix);
      end
    else if (EN.Node is TJSPropertyDeclaration) then
      begin
      PD:=EN.Node as TJSPropertyDeclaration;
      if TypeNeedsTypeName(PD.ElementType,False,True) then
        Result:=Result+AllocateIndirectTypeDef(PD.ElementType,aPrefix,GetName(PD));
      end
    else if (EN.Node is TJSVarDeclaration) then
      begin
      VD:=EN.Node as TJSVarDeclaration;
      if (VD.Typed is TJSObjectTypeDef) then
        Result:=Result+AllocateIndirectTypeDef(VD.Typed,aPrefix,GetName(VD));
      end;
    end;
end;

Function TTypescriptToPas.AllocateIndirectTypeDefs(aParams: TJSTypedParams; const aPrefix : String): Integer;

Var
  I : Integer;
  aParam : TJSTypedParam;

begin
  // Writeln('AITD params prefix : ',aPrefix);
  Result:=0;
  For I:=0 to aParams.Count-1 do
    begin
    aParam:=aParams[i];
    if TypeNeedsTypeName(aParam.Node,False) then
      begin
      AllocateIndirectTypeDef(aParam.Node,aPrefix,UTF8Encode(aParam.Name));
//      Result:=Result+AllocateTypeName(aParam.Node,aPrefix,UTF8Encode(aParam.Name));
      end;
    end;
end;


function TTypescriptToPas.AllocateIndirectTypeDefs(FD : TJSFuncDef; const aPrefix : String): Integer;

Var
  fn,aTypePrefix : String;

begin
  fn:=UTF8Encode(FD.Name);
  if fn<>'' then
    FN:=FN+'_';
  aTypePrefix:=aPrefix+FN;
  // Writeln('AITD func (',fd.Name,') prefix : ',aPrefix,' Type prefix: ',aTypePrefix);
  Result:=AllocateIndirectTypeDefs(FD.TypedParams,aTypePrefix);
  if TypeNeedsTypeName(FD.ResultType,False,True) then
    Result:=Result+AllocateIndirectTypeDef(FD.ResultType,aTypePrefix,'Result');
end;

procedure TTypescriptToPas.SetTypeAliases(AValue: TStrings);
begin
  if FTypeAliases=AValue then Exit;
  FTypeAliases.Assign(AValue);
end;

procedure TTypescriptToPas.SetIncludeInterfaceCode(AValue: TStrings);
begin
  if FIncludeInterfaceCode=AValue then Exit;
  FIncludeInterfaceCode.Assign(AValue);
end;

procedure TTypescriptToPas.SetIncludeImplementationCode(AValue: TStrings);
begin
  if FIncludeImplementationCode=AValue then Exit;
  FIncludeImplementationCode.Assign(AValue);
end;

function TTypescriptToPas.GetIsRaw: Boolean;
begin
  Result:=coRaw in Options;
end;

procedure TTypescriptToPas.AllocatePasNames(FD : TJSFuncDef; const aPrefix: String = '');

begin
  AllocateIndirectTypeDefs(FD.TypedParams,aPrefix);
  if TypeNeedsTypeName(FD.ResultType,False,True) then
    AllocateIndirectTypeDef(FD.ResultType,aPrefix,'Result');
end;

procedure TTypescriptToPas.AllocatePasNames(aList : TJSElementNodes; const ParentName: String = '');

Var
  I : Integer;
  N : TJSElement;
  TD : TJSTypeDeclaration absolute N;
  MD : TJSMethodDeclaration absolute N;
  AD : TJSArrowFunctionTypeDef;
  PD : TJSPropertyDeclaration absolute N;
  lParentName,aPrefix : String;


begin
  lParentName:=ParentName;
  if lParentName<>'' then
    lParentName:=lParentName+'_';
  For I:=0 to aList.Count-1 do
    begin
    APrefix:='';
    N:=aList.Nodes[i].Node;
    AllocatePasName(N,ParentName);
    if N is TJSAmbientClassDeclaration then
      AllocatePasNames(TJSAmbientClassDeclaration(N).ClassDef.Values,lParentName)
    else if N is TJSMembersDeclaration then
       AllocatePasNames(TJSMembersDeclaration(N).Members)
    else if (N is TJSTypeDeclaration) then
       begin
       if (TD.TypeDef is TJSArrowFunctionTypeDef) then
         begin
         aPrefix:=StringReplace(GetName(TD),'&','',[rfReplaceAll])+'_';
         AD:=TD.TypeDef as TJSArrowFunctionTypeDef;
         AllocatePasNames(AD.aFunction,aPrefix);
         end;
       end
    else if (N is TJSMethodDeclaration) then
      begin
      if Assigned(MD.FuncDef) then
        begin
        aPrefix:=StringReplace(GetName(MD),'&','',[rfReplaceAll])+'_';
        if (lParentName<>'') and not (coLocalArgumentTypes in Options) then
          aPrefix:=lParentName+aPrefix;
        AllocatePasNames(MD.FuncDef,aPrefix);
        end;
      end
    else if (N is TJSPropertyDeclaration) then
      begin
      if Assigned(PD.ElementType) then
        if TypeNeedsTypeName(PD.ElementType,False,True) then
          begin
          AllocateTypeName(PD.ElementType,lParentName,GetName(PD));
          aPrefix:=StringReplace(GetName(PD),'&','',[rfReplaceAll]);
          AllocateIndirectTypeDef(PD.ElementType,lParentName,aPrefix);
          end;
      end;
    end;
end;

procedure TTypescriptToPas.AllocatePasNames(aList : TJSSourceElements; const ParentName: String = '');

begin
  AllocatePasNames(aList.Types,ParentName);
  AllocatePasNames(aList.Enums,ParentName);
  AllocatePasNames(aList.Vars,ParentName);
  AllocateIndirectTypeDefs(aList.Vars,'');
  AllocatePasNames(aList.Functions,ParentName);
  AllocateIndirectTypeDefs(aList.Functions,'');
  AllocatePasNames(aList.Classes,ParentName);
  AllocatePasNames(aList.Interfaces,ParentName);
  AllocatePasNames(aList.NameSpaces,ParentName);
  AllocatePasNames(aList.Modules,ParentName);
end;


procedure TTypescriptToPas.EnsureUniqueNames(ML: TJSSourceElements);
begin

end;

procedure TTypescriptToPas.ProcessDefinitions;

begin
  AllocatePasNames((FElements.A as TJSSourceElements));
end;

function TTypescriptToPas.ExportNode(aNode: TJSElementNode): Boolean;
begin
  With aNode do
    Result:=IsAmbient or IsExport;
end;

procedure TTypescriptToPas.CheckUnitName(SourceElements:TJSSourceElements);

Var
  I : integer;
  NN : String;

begin
  NN:=OutputUnitName;
  if (NN<>'') and (NN[1] in ['0'..'9']) then
    begin
    Dolog(SLogRenamingUnitCompile, [OutputUnitName, NN]);
    NN:='_'+NN;
    end;
  For I:=0 to SourceElements.Functions.Count-1 do
    if UTF8Encode((SourceElements.Functions[i].Node as TJSFunctionStatement).AFunction.Name)=OutputUnitName then
      begin
      NN:=NN+'_';
      Dolog(SErrRenamingUnitConflict, [OutputUnitName, NN]);
      end;
  if OutputUnitName<>NN then
    OutputUnitName:=NN;
end;

procedure TTypescriptToPas.Execute;

Var
  SourceElements:TJSSourceElements;
  Fwds : TStringList;

begin
  FContext:=CreateContext;
  try
    PushNameScope;
    Parse;
    SourceElements:=FElements.A as TJSSourceElements;
    Fwds:=TStringList.Create;
    try
      Context.PushScope(SourceElements,fwds);
      ProcessDefinitions;
      CheckUnitName(SourceElements);
      FContext.TypesToMap;
      if Verbose then
        DoLog(SLogParsedNDefinitions, [FContext.FTypeMap.Count]);
    finally
      Context.PopScope(SourceElements,Fwds);
      Fwds.Free;
    end;
    if Assigned(TypeAliases) then
      FContext.AddAliases(TypeAliases);
    WritePascal;
    if OutputFileName<>'' then
      Source.SaveToFile(OutputFileName);
  finally
    PopNameScope;
    FreeAndNil(FContext);
  end;
end;

{ ----------------------------------------------------------------------
  Simple types
  ----------------------------------------------------------------------}


Function TTypescriptToPas.GetArrayTypeAsString(aTypeDef : TJSArrayTypeDef; asPascal,asSubType : Boolean) : String;

begin
  if Assigned(aTypeDef.BaseType.Data) then
    Result:=TPasData(aTypeDef.BaseType.Data).PasName
  else
    Result:=GetTypeAsString(aTypeDef.BaseType,asPascal,True);
  if coGenericArrays in Options then
    Result:='TArray<'+Result+'>'
  else
    Result:='array of '+Result;
  if (not asPascal) and AsSubType then
    Result:='('+Result+')'
end;


Function TTypescriptToPas.GetTypeAsString(aType : TJSTypeDef; asPascal,asSubType : Boolean) : String;

begin
  Result:='';
  if aType is TJSTypeReference then
    Result:=GetAliasTypeAsString(TJSTypeReference(aType),asPascal,asSubType)
  else if aType is TJSUnionTypeDef then
    Result:=GetUnionTypeAsString(TJSUnionTypeDef(aType),asPascal,asSubType)
  else if aType is TJSIntersectionTypeDef then
    Result:=GetIntersectionTypeAsString(TJSIntersectionTypeDef(aType),asPascal,asSubType)
  else if aType is TJSArrayTypeDef then
    Result:=GetArrayTypeAsString(TJSArrayTypeDef(aType),asPascal,asSubType)
  else if aType is TJSEnumTypeDef then
    Result:=GetEnumTypeAsString(TJSEnumTypeDef(aType),asPascal,asSubType)
  else if aType is TJSTupleTypeDef then
    Result:=GetTupleTypeAsString(TJSTupleTypeDef(aType),asPascal,True)
  else if aType is TJSFixedValueReference then
    Result:=GetFixedValueTypeAsString(TJSFixedValueReference(aType),asPascal,asSubType)
  else
    if asPascal then
      if Assigned(aType.Data) then
        Result:=TPasData(aType.Data).PasName;
end;

Function TTypescriptToPas.GetUnionTypeAsString(aTypeDef : TJSUnionTypeDef; asPascal,asSubType : Boolean) : String;

Var
  I : Integer;

begin
  Result:='';
  For I:=0 to aTypeDef.TypeCount-1 do
    begin
    if Result<>'' then
      Result:=Result+' | ';
    Result:=Result+GetTypeAsString(aTypeDef.Types[I],asPascal,True);
    end;
  if AsSubType then
    Result:='('+Result+')';
end;

function TTypescriptToPas.GetEnumTypeAsString(aTypeDef: TJSEnumTypeDef; asPascal, asSubType: Boolean): String;
Var
  I : Integer;
  N : String;

begin
  Result:='';
  For I:=0 to aTypeDef.NameCount-1 do
    begin
    if Result<>'' then
      Result:=Result+', ';
    N:=UTF8Encode(aTypeDef.Names[I]);
    if IsKeyWord(N) then
      N:='&'+N;
    Result:=Result+N;
    end;
  Result:='('+Result+')';
  if AsSubType then
    Result:='('+Result+')';
end;

function TTypescriptToPas.GetFixedValueTypeAsString(aTypeDef: TJSFixedValueReference; asPascal, asSubType: Boolean): string;
begin
  case aTypeDef.FixedValue.Value.ValueType of
    jstUNDEFINED : Result:='jsValue';
    jstNull : Result:='jsValue';
    jstBoolean : Result:='Boolean';
    jstNumber : Result:='Double';
    jstString : Result:='string';
    jstObject : Result:='TJSObject';
    jstReference : Result:='jsValue';
    jstCompletion : Result:='jsValue';
  end;
end;

Function TTypescriptToPas.GetIntersectionTypeAsString(aTypeDef : TJSIntersectionTypeDef; asPascal,asSubType : Boolean) : String;

Var
  I : Integer;

begin
  Result:='';
  For I:=0 to aTypeDef.TypeCount-1 do
    begin
    if Result<>'' then
      Result:=Result+' & ';
    Result:=Result+GetTypeAsString(aTypeDef.Types[I],asPascal,True);
    end;
  if AsSubType then
    Result:='('+Result+')';
end;

Procedure TTypescriptToPas.WriteUnionTypeDef(const aPasName : string; const aOrgName : jsBase.TJSString; aTypeParams: TJSElementNodes;aTypeDef : TJSUnionTypeDef);

var
  TN, gen, genparams, tcomment: String;

begin
  TN:='jsvalue';
  if aTypeDef.GetOnlyConstants=ocAllSameTypes then
    begin
    TN:=GetTypeAsString((aTypeDef.Values[0].Node as TJSFixedValueReference),True,False);
    tcomment:=' // Restricted values';
    end
  else
    tcomment:=' // '+GetTypeAsString(aTypeDef,False,False);
  genparams:=GetGenericParams(aTypeParams);
  if (genparams<>'') then
    gen:='generic ';
  AddLn('%s%s%s = %s;%s',[gen,aPasName,genparams,TN,tcomment]);
end;

function TTypescriptToPas.GetTupleTypeAsString(aTypeDef: TJSTupleTypeDef; asPascal,asSubType : Boolean) : String;

Var
  N :TJSTypeReference;
  elName : string;

begin
  Result:='jsvalue';
  if aTypeDef.Values.Count=0 then
    exit;
  if (Not aTypeDef.GetEqualTypes) or (coUntypedTuples in Options) then
    begin
    if coDynamicTuples in Options then
      Result:='TJSValueDynArray'
    else
      Result:=Format('Array[0..%d] of JSValue',[aTypeDef.Values.Count-1]);
    end
  else if  aTypeDef.Values[0].Node is TJSTypeReference then
    begin
    N:=aTypeDef.Values[0].Node as TJSTypeReference;
    ElName:=GetTypeAsString(N,True,False);
    if coDynamicTuples in Options then
      Result:=Format('Array of %s',[ElName])
    else
      Result:=Format('Array[0..%d] of %s',[aTypeDef.Values.Count-1,elName]);
    end
  else
    raise ETSToPas.CreateFmt(SErrUnsupportedTupleElementType, [aTypeDef.Values[0].Node.ClassName]);
end;
procedure TTypescriptToPas.WriteTupleTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString;
  aTypeParams: TJSElementNodes; aTypeDef: TJSTupleTypeDef);

var
  TN, gen, genparams: String;


begin
  genparams:=GetGenericParams(aTypeParams);
  if (genparams<>'') then
    gen:='generic ';
  TN:=GetTupleTypeAsString(aTypeDef,True,False);
  AddLn('%s%s%s = %s;',[gen,aPasName,genparams,TN]);
end;


Procedure TTypescriptToPas.WriteIntersectionTypeDef(const aPasName : string; const aOrgName : jsBase.TJSString; aTypeParams: TJSElementNodes;aTypeDef : TJSIntersectionTypeDef);

var
  TN, gen, genparams: String;

begin
  TN:='jsvalue';
  genparams:=GetGenericParams(aTypeParams);
  if (genparams<>'') then
    gen:='generic ';
  AddLn('%s%s%s = %s; // %s',[gen,aPasName,genparams,TN,GetTypeAsString(aTypeDef,False,false)]);
end;

Procedure TTypescriptToPas.WriteArrayTypeDef(const aPasName : string; const aOrgName : jsBase.TJSString; aTypeParams: TJSElementNodes;aTypeDef : TJSArrayTypeDef);

var
  arr,gen, genparams: String;

begin
  genparams:=GetGenericParams(aTypeParams);
  if (genparams<>'') then
    gen:='generic ';
  arr:=GetArrayTypeAsString(aTypeDef,True,False);
  AddLn('%s%s%s = %s;',[gen,aPasName,genparams,arr]);
end;

procedure TTypescriptToPas.WriteEnumTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes;
  aTypeDef: TJSEnumTypeDef);
var
 arr,gen, genparams: String;

begin
  genparams:=GetGenericParams(aTypeParams);
  if (genparams<>'') then
    gen:='generic ';
  arr:=GetEnumTypeAsString(aTypeDef,True,False);
  AddLn('%s%s%s = %s;',[gen,aPasName,genparams,arr]);
end;


Procedure TTypescriptToPas.WriteTypeDef(const aPasName : string; const aOrgName : jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef : TJSTypeDef);

begin
  if NameScopeHas(aPasName) then
    begin
    Comment(Format(SCommentIgnoringDuplicateType, [aPasName, UTF8Encode(aOrgName)]));
    exit;
    end;
  AddToNameScope(aPasName,aOrgName);
  If aTypeDef is TJSTypeReference then
    WriteAliasTypeDef(aPasName,aOrgName,aTypeParams,TJSTypeReference(aTypeDef))
  else if aTypeDef is TJSUnionTypeDef then
    WriteUnionTypeDef(aPasName,aOrgName,aTypeParams,TJSUnionTypeDef(aTypeDef))
  else if aTypeDef is TJSIntersectionTypeDef then
    WriteIntersectionTypeDef(aPasName,aOrgName,aTypeParams,TJSIntersectionTypeDef(aTypeDef))
  else if aTypeDef is TJSArrayTypeDef then
    WriteArrayTypeDef(aPasName,aOrgName,aTypeParams,TJSArrayTypeDef(aTypeDef))
  else if aTypeDef is TJSEnumTypeDef then
    WriteEnumTypeDef(aPasName,aOrgName,aTypeParams,TJSEnumTypeDef(aTypeDef))
  else if aTypeDef is TJSArrowFunctionTypeDef then
    WriteFunctionTypeDef(aPasName,aOrgName,aTypeParams,TJSArrowFunctionTypeDef(aTypeDef).aFunction)
  else if aTypeDef is TJSObjectTypeDef then
    WriteObjectTypedef(aPasName,aOrgName,aTypeParams,TJSObjectTypeDef(aTypeDef))
  else if aTypeDef is TJSTupleTypeDef then
    WriteTupleTypedef(aPasName,aOrgName,aTypeParams,TJSTupleTypeDef(aTypeDef))
  else
    Comment(Format(SErrUnsupportedType, [aPasName, aOrgName, aTypeDef.ClassName]));
end;

function TTypescriptToPas.WriteIndirectTypeDefs(aParams: TJStypedParams): Integer;

Var
  I : Integer;
  aParam : TJSTypedParam;
  FuncDef : TJSFuncDef;
  PD : TPasData;

begin
  // Writeln('WITD params');
  Result:=0;
  For I:=0 to aParams.Count-1 do
    begin
    aParam:=aParams[i];
    if TypeNeedsTypeName(aParam.Node,True)  then
      begin
      Inc(Result);
      PD:=TPasData(aParam.Node.Data);
      // Recurse
      if aParam.Node is TJSArrowFunctionTypeDef then
        begin
        FuncDef:=(aParam.Node as TJSArrowFunctionTypeDef).aFunction;
        Result:=Result+WriteIndirectTypeDefs(FuncDef.TypedParams);
        if TypeNeedsTypeName(FuncDef.ResultType,True) then
          begin
          PD:=TPasData(aParam.Node.Data);
          Inc(Result);
          WriteTypeDef(PD.PasName,PD.OriginalName,nil, FuncDef.ResultType);
          end
        end
      else if aParam.Node is TJSArrayTypeDef then
        begin
        if TypeNeedsTypeName(TJSArrayTypeDef(aParam.Node).BaseType,True,True) then
          begin
          PD:=TPasData(TJSArrayTypeDef(aParam.Node).BaseType.Data);
          Inc(Result);
          WriteTypeDef(PD.PasName,PD.OriginalName,nil, TJSArrayTypeDef(aParam.Node).BaseType);
          end
        end;
      PD:=TPasData(aParam.Node.Data);
      WriteTypeDef(PD.PasName,PD.OriginalName,nil,(aParam.Node as TJSTypeDef));
      end;
    end;
end;

function TTypescriptToPas.HasIndirectTypeDefs(aParams: TJStypedParams): Boolean;

Var
  I : Integer;
  aParam : TJSTypedParam;

begin
  Result:=False;
  I:=0;
  While (Not Result) and (I<aParams.Count) do
    begin
    aParam:=aParams[i];
    Result:=Assigned(aParam.Node) and Assigned(aParam.Node.Data);
    Inc(I);
    end;
end;

function TTypescriptToPas.HasIndirectTypeDefs(aElements: TJSElementNodes): Boolean;

var
  EN : TJSElementNode;
  FD : TJSFuncDef;

begin
  Result:=False;
  For EN in aElements do
    if ExportNode(EN) then
      begin
      if (EN.Node is TJSFunctionStatement) then
        begin
        FD:=TJSFunctionStatement(EN.Node).AFunction;
        Result:=HasIndirectTypeDefs(FD.TypedParams);
        if Result then
          Exit;
        end;
      if (EN.Node is TJSObjectTypeDef) then
        begin
        Result:=HasIndirectTypeDefs(TJSObjectTypeDef(EN.Node).Values);
        if Result then
          Exit;
        end;
      if (EN.Node is TJSVarDeclaration) then
        begin
        Result:=TJSVarDeclaration(EN.Node).Typed is TJSObjectTypeDef;
        if Result then
          Exit;
        end;
      end;
end;

function TTypescriptToPas.WriteIndirectTypeDefs(aEl : TJSElement): Integer;

Var
  PD : TPasData;

begin
  Result:=0;
  if aEl is TJSArrowFunctionTypeDef then
    Result:=WriteIndirectTypeDefs((aEl as TJSArrowFunctionTypeDef).aFunction)
  else if aEl is TJSArrayTypeDef then
    begin
    Result:=WriteIndirectTypeDefs((aEl as TJSArrayTypeDef).BaseType);
    PD:=TPasData((aEl as TJSArrayTypeDef).BaseType.Data);
    if assigned(PD) then
      WriteTypeDef(PD.PasName,PD.OriginalName,Nil,(aEl as TJSArrayTypeDef).BaseType);
    end
  else if aEl is TJSObjectTypeDef then
    Result:=WriteIndirectTypeDefs((aEl as TJSObjectTypeDef).Values);
end;

function TTypescriptToPas.WriteIndirectTypeDefs(FD : TJSFuncDef): Integer;

var
  PD : TPasData;
begin
  // Writeln('WIDT Func : ',FD.Name);
  Result:=WriteIndirectTypeDefs(FD.TypedParams);
  if TypeNeedsTypeName(FD.ResultType,True,True) then
    begin
    WriteIndirectTypeDefs(FD.ResultType);
    PD:=TPasData(FD.ResultType.Data);
    if PD=Nil then
      raise ETSToPas.CreateFmt(SErrNoNameAllocatedForFunctionResult, [FD.Name, FD.ResultType.Line, FD.ResultType.Column,
        FD.ResultType.ClassName]);
    WriteTypeDef(PD.PasName,PD.OriginalName,nil,FD.ResultType);
    end;
end;

function TTypescriptToPas.WriteIndirectTypeDefs(aElements: TJSElementNodes): Integer;

var
  EN : TJSElementNode;
  FD : TJSFuncDef;

begin
  // Writeln('WIDT elements: ');
  Result:=0;
  For EN in aElements do
    begin
    FD:=Nil;
    if (EN.Node is TJSFunctionStatement) then
      FD:=TJSFunctionStatement(EN.Node).AFunction
    else if (EN.Node is TJSTypeDeclaration) and (TJSTypeDeclaration(EN.Node).TypeDef is TJSArrowFunctionTypeDef) then
      FD:=TJSArrowFunctionTypeDef(TJSTypeDeclaration(En.Node).TypeDef).aFunction;
    if Assigned(FD) then
      Result:=Result+WriteIndirectTypeDefs(FD)
    end;
  WritePropertyTypeDefs(aElements,'');
end;

function TTypescriptToPas.WritePropertyTypeDefs(aElements: TJSElementNodes; const SectionName: String): Integer;

Var
  P : TJSPropertyDeclaration;
  aName : TJSString;
  PD : TPasData;
  EN : TJSElementNode;
  TD : TJSTypeDef;
  DidIndent : Boolean;

begin
  Result:=0;
  DidIndent:=False;
  For EN in aElements do
    begin
    TD:=Nil;
    aName:='';
    if EN.Node is TJSPropertyDeclaration then
      begin
      P:=TJSPropertyDeclaration(EN.Node);
      aName:=P.Name;
      TD:=P.ElementType;
      If not TypeNeedsTypeName(TD,True,True) then
        TD:=Nil
      end
    else if EN.Node is TJSVarDeclaration then
      begin
      aName:=TJSVarDeclaration(EN.Node).Name;
      TD:=TJSVarDeclaration(EN.Node).Typed;
      if not (TD is TJSObjectTypeDef) then
        TD:=nil;
      end;
    if Assigned(TD) then
      begin
      if (Result=0) and (SectionName<>'') then
        begin
        AddLn(SectionName);
        Indent;
        AddLn('Type');
        Indent;
        DidIndent:=True;
        end;
      PD:=TPasData(TD.Data);
      if TD is TJSArrowFunctionTypeDef then
        Result:=Result+WriteIndirectTypeDefs((TD as TJSArrowFunctionTypeDef).aFunction)
      else if TD is TJSObjectTypeDef then
        Result:=Result+WriteIndirectTypeDefs((TD as TJSObjectTypeDef).Values);
      if PD=Nil then
        raise ETSToPas.CreateFmt(SErrElementWithoutTypeName, [aName, TD.ClassName]);
      WriteTypeDef(PD.PasName,PD.OriginalName,Nil,TD);
      Inc(Result);
      end;
    end;
  if DidIndent then
    begin
    Undent;
    Undent;
    end;
end;

function TTypescriptToPas.WriteMethodParameterDefs(aElements: TJSElementNodes; const SectionName: String): Integer;

var
  EN : TJSElementNode;
  FD : TJSFuncDef;
  Didindent : Boolean;

begin
  Result:=0;
  DidIndent:=False;
  For EN in aElements do
    if EN.Node is TJSMethodDeclaration then
      begin
      FD:=TJSMethodDeclaration(EN.Node).FuncDef;
      if (Result=0) and (SectionName<>'') then
        begin
        AddLn(SectionName);
        Indent;
        AddLn('Type');
        Indent;
        DidIndent:=True;
        end;
      WriteIndirectTypeDefs(FD);
      end;
  if DidIndent then
    begin
    Undent;
    Undent;
    end;
end;


Procedure TTypescriptToPas.WriteTypeDefs(Types: TJSElementNodes);

Var
  I : Integer;
  N : TJSElement;
  Decl : TJSTypeDeclaration absolute N;
  aName : String;

begin
  EnsureSection(csType);
  for I:=0 to Types.Count-1 do
    if ExportNode(Types[i]) then
      begin
      N:=Types[I].Node;
      // TJSEnumDeclaration is a descendent
      if N is TJSTypeDeclaration then
        begin
        aName:=GetName(Decl);
        WriteTypeDef(aName, Decl.Name, Decl.TypeParams, Decl.TypeDef);
        end
      end;
end;


function TTypescriptToPas.WritePrivateReadOnlyField(P : TJSPropertyDeclaration) : Boolean;
Var
  FN : String;

begin
  Result:=True;
  FN:=StringReplace(GetName(P),'&','',[rfReplaceAll]);
  AddLn('%s%s : %s; external name ''%s''; ',[FieldPrefix,FN,GetTypeName(P.ElementType),P.Name]);
end;

function TTypescriptToPas.WritePrivateReadOnlyField(M : TJSMethodDeclaration) : Boolean;

Var
  FN : String;

begin
  Result:=True;
  FN:=StringReplace(GetName(M),'&','',[rfReplaceAll]);
  AddLn('%s%s : %s; external name ''%s''; ',[FieldPrefix,FN,GetTypeName(M.FuncDef.ResultType),M.Name]);
end;


Function TTypescriptToPas.HasReadOnlyPropFields(aTypeDef : TJSObjectTypeDef) : Boolean;

Var
  I : Integer;
  aEl : TJSObjectTypeElementDef;
  P : TJSPropertyDeclaration;

begin
  Result:=False;
  I:=0;
  While (Not Result) and (I<aTypeDef.ElementCount) do
    begin
    aEl:=aTypeDef.Elements[i];
    if aEl is TJSPropertyDeclaration then
      begin
      P:=TJSPropertyDeclaration(aTypeDef.Elements[i]);
      Result:=P.IsReadOnly;
      end
    else if aEl is TJSMethodDeclaration then
      Result:=TJSMethodDeclaration(aEl).IsGet and not aTypeDef.HasSetter(ael.Name);
    Inc(I);
    end;
end;

Function TTypescriptToPas.WriteReadOnlyPropFields(aTypeDef : TJSObjectTypeDef) : Integer;


Var
  I : Integer;
  aEl : TJSObjectTypeElementDef;
  P : TJSPropertyDeclaration;

begin
  Result:=0;
  For I:=0 to aTypeDef.ElementCount-1 do
    begin
    aEl:=aTypeDef.Elements[i];
    if aEl is TJSPropertyDeclaration then
      begin
      P:=TJSPropertyDeclaration(aTypeDef.Elements[i]);
      if P.IsReadOnly then
        WritePrivateReadonlyField(P);
      end
    else if aEl is TJSMethodDeclaration then
      if TJSMethodDeclaration(aEl).IsGet and not aTypeDef.HasSetter(ael.Name) then
        WritePrivateReadonlyField(TJSMethodDeclaration(aEl));
    end;
end;

function TTypescriptToPas.WriteClassIndirectTypeDefs(aElements: TJSElementNodes; isClassLocal : Boolean) : Integer;

Var
  Sect : String;

begin
  Result:=0;
  if Not IsClassLocal then
    begin
    Result:=WritePropertyTypeDefs(aElements);
    Result:=Result+WriteMethodParameterDefs(aElements);
    end
  else
    begin
    Result:=WriteMethodParameterDefs(aElements,'Public');
    if Result>0 then
      Sect:=''
    else
      Sect:='Public';
    Result:=Result+WritePropertyTypeDefs(aElements,Sect);
    end;
end;
function TTypescriptToPas.WriteAmbientClassDef(const aPasName: String; aOrgName: TJSString; aTypeParams: TJSElementNodes;
  aClass: TJSAmbientClassDeclarationArray): Boolean;

Type
  TMembers = array of TJSSourceElements;

  Procedure AddNameSpaceMembers(var AMembers : TMembers);

  Var
    I : Integer;
    NS : TJSNameSpaceDeclaration;

  begin
    Result:=False;
    I:=Context.CurrentScope.NameSpaces.Count-1;
    While  (I>=0) do
      begin
      NS:=TJSNameSpaceDeclaration(Context.CurrentScope.NameSpaces[i].Node);
      If (aOrgName = NS.Name) then
        aMembers:=Concat(aMembers,[NS.Members]);
      Dec(I);
      end;
  end;


Var
  aParentName : string;
  aCount : Integer;
  Members : TMembers;
  M : TJSSourceElements;
  C,C0 : TJSAmbientClassDeclaration;

begin
  Result:=True;
  C0:=aClass[0];
  if C0.Extends is TJSTypeReference then
    aParentName:=GetTypeName(C0.Extends)
  else
    aParentName:=DefaultClassParent;
  Members:=[];
  AddNameSpaceMembers(Members);
  if not (coLocalArgumentTypes in Options) then
    for C in aClass do
      WriteClassIndirectTypeDefs(C.ClassDef.Values,False);
  AddLn('%s = class external name ''%s'' (%s)',[aPasName,aOrgName,aParentName]);
  if (coLocalArgumentTypes in Options) then
    begin
    For C in aClass do
      aCount:=WriteClassIndirectTypeDefs(C.ClassDef.Values,True)
    end
  else
    aCount:=0;
  for M in Members do
    begin
    if aCount=0 then
      begin
      Addln('Public');
      Indent;
      Addln('Type');
      end;
      WriteSourceElements(M,aOrgName);
      Undent;
      Addln('Public');
      end;
  For C in aClass do
    WriteObjectTypeMembers(aPasName,aOrgName,aTypeParams,C.ClassDef);
  AddLn('end;');
  AddLn('');
end;

function TTypescriptToPas.WriteClassDefs(aClasses: TJSElementNodes): Integer;

  Function GetClasses(const aName : String) : TJSAmbientClassDeclarationArray;

  Var
    I,aCount : Integer;
    N : TJSElement;

  begin
    aCount:=0;
    Result:=[];
    SetLength(Result,aClasses.Count);
    For I:=0 to aClasses.Count-1 do
      begin
      N:=aClasses[I].Node;
      if N is TJSAmbientClassDeclaration then
        if aName=GetName(N) then
          begin
          Result[aCount]:=TJSAmbientClassDeclaration(N);
          Inc(aCount);
          end;
      end;
    SetLength(Result,aCount);
  end;


Var
  I : Integer;
  N : TJSElement;
  AmbientDecl : TJSAmbientClassDeclarationArray;
  // ClassDecl : TJSClassDeclaration absolute N;
  aName : String;
  L : TStringList;

begin
  Result:=0;
  EnsureSection(csType);
  L:=TStringList.Create;
  try
    L.Duplicates:=DupIgnore;
    for I:=0 to aClasses.Count-1 do
      if ExportNode(aClasses[i]) then
        begin
        N:=aClasses[I].Node;
        // TJSEnumDeclaration is a descendent
        if N is TJSAmbientClassDeclaration then
          L.Add(GetName(N));
        end;
    For I:=0 to L.Count-1 do
     begin
     aName:=L[I];
     AmbientDecl:=GetClasses(aName);
     if Length(AmbientDecl)>0 then
       begin
       if Length(AmbientDecl)>1 then
         DoLog(SLogFoldingClassDefinitions, [Length(AmbientDecl), aName]);
       if WriteAmbientClassDef(aName, AmbientDecl[0].Name, AmbientDecl[0].TypeParams, AmbientDecl) then
         Inc(Result);
       end;
     end;
  finally
    L.Free;
  end;
end;


function TTypescriptToPas.WritePropertyDef(aProp: TJSPropertyDeclaration): Boolean;

Var
  Def,TN,FN,aName : String;

begin
  Result:=True;
  FN:=GetName(aProp);
  TN:=GetTypeName(aProp.ElementType);
  if TN='record' then
    TN:='TJSObject';
  if SameText(FN,TN) then
    FN:=FN+'_';
  Def:=Format('%s : %s;',[FN,TN]);
  aName:=UTF8Encode(aProp.Name);
  if (FN<>aName) then
    Def:=Def+Format('external name ''%s'';',[aName]);
  AddLn(Def);
end;

function TTypescriptToPas.WriteReadonlyProperty(aProp: TJSPropertyDeclaration): Boolean;

Var
  TN,N,PN : String;

begin
  Result:=True;
  N:=StringReplace(GetName(aProp),'&','',[rfReplaceAll]);
  PN:=N;
  TN:=GetTypeName(aProp.ElementType);
  if SameText(PN,TN) then
    PN:='_'+PN;
  AddLn('Property %s : %s Read %s%s; ',[PN,TN,FieldPrefix,N]);
end;

Function TTypescriptToPas.WriteObjectMethods(aAccess : TAccessibility; aTypeDef: TJSObjectTypeDef) : Integer;


Var
  L : TStringList;
  I,aCount : Integer;
  FN : String;
  aDefs : Array of TJSFuncDef;

begin
  Result:=0;
  L:=TStringList.Create;
  try
    L.Sorted:=true;
    L.Duplicates:=dupIgnore;
    For I:=0 to aTypeDef.ElementCount-1 do
      if (aTypeDef.Elements[I].Accessibility=aAccess) and
         (aTypeDef.Elements[I] is TJSMethodDeclaration) then
        L.Add(GetName(aTypeDef.Elements[I]));
    For FN in L do
      begin
      aCount:=0;
      aDefs:=[];
      SetLength(aDefs,aTypeDef.ElementCount);
      For I:=0 to aTypeDef.ElementCount-1 do
        if (aTypeDef.Elements[I].Accessibility=aAccess) and
           (aTypeDef.Elements[I] is TJSMethodDeclaration) and
           (GetName(aTypeDef.Elements[I])=FN) then
             begin
             if TJSMethodDeclaration(aTypeDef.Elements[I]).FuncDef=nil then
               DoLog(SLogIgnoringEmptyMethod)
             else
               begin
               aDefs[aCount]:=TJSMethodDeclaration(aTypeDef.Elements[I]).FuncDef;
               inc(aCount);
               end;
             end;
      SetLength(aDefs,aCount);
      I:=Length(aDefs);
      WriteFunctionDefinition(FN,aDefs,False);
      end;
  finally
    L.Free;
  end;
end;

procedure TTypescriptToPas.WriteIndexSignature(aSign : TJSIndexSignatureDeclaration);

begin
  If aSign=Nil then
    exit;
end;

procedure TTypescriptToPas.WriteObjectTypeMembers(const aPasName: String; const aOrigName: jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSObjectTypeDef);

Var
  I : Integer;
  EmitAccessibility : Boolean;

begin
  EmitAccessibility:=Not (aTypeDef is TJSInterfaceDeclaration);

  if HasReadOnlyPropFields(aTypeDef) or aTypeDef.HasAccessMembers(accPrivate) then
    begin
    if EmitAccessibility then
      AddLn(GetAccessName(accPrivate));
    Indent;
    WriteReadOnlyPropFields(aTypeDef);
    WriteObjectMethods(accPrivate,aTypeDef);
    WriteProperties(accPrivate,aTypeDef.Values);
    Undent;
    end;
  if aTypeDef.HasAccessMembers(accProtected) then
    begin
    if EmitAccessibility then
      AddLn(GetAccessName(accProtected));
    Indent;
    WriteObjectMethods(accProtected,aTypeDef);
    WriteProperties(accProtected,aTypeDef.Values);
    Undent;
    end;
  if aTypeDef.HasAccessMembers(accPublic) then
    begin
    if EmitAccessibility then
      AddLn(GetAccessName(accPublic));
    Indent;
    WriteObjectMethods(accPublic,aTypeDef);
    WriteProperties(accPublic,aTypeDef.Values);
    undent;
    end;
  if aTypeDef.HasAccessMembers(accDefault) then
    begin
    if EmitAccessibility then
      AddLn(GetAccessName(accPublic));
    Indent;
    WriteObjectMethods(accDefault,aTypeDef);
    WriteProperties(accDefault,aTypeDef.Values);
    undent;
    end;
  For I:=0 to aTypeDef.ElementCount-1 do
    if aTypeDef.Elements[I] is TJSIndexSignatureDeclaration then
      begin
      Indent;
      WriteIndexSignature(aTypeDef.Elements[I] as TJSIndexSignatureDeclaration);
      Undent;
      end;
end;

procedure TTypescriptToPas.WriteObjectTypedef(const aPasName: String; const aOrigName: jsBase.TJSString;
  aTypeParams: TJSElementNodes; aTypeDef: TJSObjectTypeDef);

Var
  I : Integer;
  aName : string;

begin
  aName:='Object';
  For I:=0 to aTypeDef.ElementCount-1 do
   if (aTypeDef.Elements[I].Name='new') and (aTypeDef.Elements[I] is TJSMethodDeclaration) then
     aName:=UTF8Encode(aOrigName);
  AddLn('%s = class external name ''%s'' (TJSObject)',[aPasName,aName]);
  WriteObjectTypeMembers(aPasName,aOrigName,aTypeParams,aTypeDef);
  AddLn('end;');
  AddLn('');
end;

{ ----------------------------------------------------------------------
  Functions
  ----------------------------------------------------------------------}


function TTypescriptToPas.GetArguments(aList: TJSTypedParams; ForceBrackets: Boolean): String;

Var
  E : TJSElementNode;
  aParam : TJSTypedParam absolute E;
  aType : TJSTypeDef;
  Arg,aArgType : string;

begin
  Result:='';
  For E in aList do
    begin
    Arg:=GetName(aParam);
    if Not Assigned(aParam.Type_) then
      aArgType:='jsvalue'
    else
      begin
      aType:=aParam.Type_ as TJSTypeDef;
      aArgType:=GetTypeName(AType);
      end;
    Arg:=Arg+' : '+aArgType;
    if Result<>'' then
      Result:=Result+'; ';
    Result:=Result+Arg;
    end;
  if (Result<>'') or ForceBrackets then
    Result:='('+Result+')';
end;

Type
  // A partial params list is a list which has been generated for a optional argument.
  // This is how we distinguish lists that can be added to from lists that cannot be added to:
  // Additional parameters can never be added to a partial list.
  TJSPartialParams = Class(TJSTypedParams);


procedure TTypescriptToPas.AddUnionOverloads(aList: TFunctionOverLoadArgumentsList; const AName: TJSString; UT: TJSUnionTypeDef);

Var
  L,L2 : TFunctionOverLoadArgumentsList;
  I,J : Integer;
  D : TJSTypedParams;
  Dups : TStringList;

begin
  L2:=Nil;
  L:=Nil;
  Dups:=TStringList.Create;
  try
    Dups.Sorted:=True;
    Dups.Duplicates:=dupIgnore;
    L:=TFunctionOverLoadArgumentsList.Create(False);
    L2:=TFunctionOverLoadArgumentsList.Create(False);
    // Collect non partial argument lists
    for I:=0 to AList.Count-1 do
      begin
      D:=TJSTypedParams(alist[i]);
      if Not (D is TJSPartialParams) then
        L.AddOverload(D);
      end;
    // Collect unique pascal types. Note that this can reduce the list to 1 element...
    For I:=0 to UT.TypeCount-1 do
      Dups.AddObject(GetTypeName(UT.Types[I]),UT.Types[I]);
    // First, clone list and add argument to cloned lists
    For I:=1 to Dups.Count-1 do
      begin
      // Clone list
      CloneNonPartialParameterList(L,L2,False);
      // Add argument to cloned list
      AddParameterToOverloads(L2,aName,Dups.Objects[i] as TJSTypeDef);
      // Add overloads to original list
      For J:=0 to L2.Count-1 do
        aList.Add(L2[J]);
      L2.Clear;
      end;
    // Add first Union to original list
    AddParameterToOverloads(L,aName,Dups.Objects[0] as TJSTypeDef);
  finally
    Dups.Free;
    L2.Free;
    L.Free;
  end;
end;


function TTypescriptToPas.CloneNonPartialParameterList(aList: TFunctionOverLoadArgumentsList; ADest: TFunctionOverLoadArgumentsList = Nil; AsPartial: Boolean = True): integer;

Var
  I : Integer;
  DL,CL : TJSTypedParams;

begin
  Result:=0;
  if ADest=Nil then
    ADest:=aList;
  I:=aList.Count-1;
  While (I>=0) do
    begin
    DL:=TJSTypedParams(alist[i]);
    if Not (DL is TJSPartialParams) then
      begin
      Inc(Result);
      if AsPartial then
        CL:=TJSPartialParams.CreateTransient
      else
        CL:=TJSTypedParams.CreateTransient;
      CL.Assign(DL);
      aDest.AddOverload(CL);
      end;
    Dec(I);
    end;
end;

procedure TTypescriptToPas.AddParameterToOverloads(aList: TFunctionOverLoadArgumentsList; const AName : TJSString; ATypeDef : TJSTypeDef);

Var
  I : Integer;
  aParam : TJSTypedParam;
  aParams : TJSTypedParams;

begin
  For I:=0 to aList.Count-1 do
    begin
    aParams:=TJSTypedParams(alist[i]);
    if Not (aParams is TJSPartialParams) then
      begin
      aParam:=aParams.Add as TJSTypedParam;
      aParam.Name:=aName;
      aParam.Node:=ATypeDef;
      end;
    end;
end;

procedure TTypescriptToPas.AddParameterToOverloads(aList: TFunctionOverLoadArgumentsList; const aParam: TJSTypedParam);

Var
  I : Integer;
  aClonedParam : TJSTypedParam;
  aParams : TJSTypedParams;

begin
  For I:=0 to aList.Count-1 do
    begin
    aParams:=TJSTypedParams(alist[i]);
    if Not (aParams is TJSPartialParams) then
      begin
      aClonedParam:=aParams.Add as TJSTypedParam;
      aClonedParam.Assign(aParam);
      end;
    end;
end;


procedure TTypescriptToPas.AddOverloadParams(aList: TFunctionOverLoadArgumentsList; adef: TJSFuncDef; aIdx: Integer);

Var
  aParam : TJSTypedParam;
  D : TJSTypeDef;
  UT : TJSUnionTypeDef;

begin
 if aIdx>=ADef.TypedParams.Count then
    Exit;
  aParam:=ADef.TypedParams[aIdx];
  if aParam.IsOptional then
    CloneNonPartialParameterList(aList);
  // Add current to list.
  D:=aParam.Node as TJSTypeDef;
  UT:=Nil;
  if coExpandUnionTypeArgs in Options then
    UT:=CheckUnionTypeDefinition(D);
  if UT=Nil then
    AddParameterToOverloads(aList,aParam)
  else
    AddUnionOverLoads(aList,aParam.Name,UT);
  AddOverloadParams(aList,aDef,aIdx+1);
end;


function TTypescriptToPas.GetOverloads(const aDefs: TJSFuncDefArray): TFunctionOverLoadArgumentsList;


Var
  aDef : TJSFuncDef;
  aFunc : TFunctionOverLoadArgumentsList;
  I : Integer;

begin
  Result:=TFunctionOverLoadArgumentsList.Create;
  try
    aFunc:=TFunctionOverLoadArgumentsList.Create(False);
    try
      For aDef in aDefs do
        begin
        aFunc.Clear;
        aFunc.Add(TJSTypedParams.CreateTransient);
        AddOverloadParams(aFunc,adef,0);
        For I:=0 to aFunc.Count-1 do
          Result.Add(aFunc[I]);
        end;
    finally
      aFunc.Free;
    end;
    Result.RemoveDuplicates(Self.Context);
  except
    Result.Free;
    Raise;
  end;
end;

function TTypescriptToPas.WriteFunctionTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes; aDef: TJSFuncDef): Boolean;

Var
  FN,RT,Args : String;

begin
  Result:=True;
  if aPasName<>'' then
    FN:=aPasName
  else
    FN:=GetName(aDef);
  RT:=GetTypeName(aDef.ResultType,False);
  if (RT='void') then
    RT:='';
  Args:=GetArguments(aDef.TypedParams,False);
  if Args<>'' then
    Args:=' '+Args;
  if (RT='') then
    AddLn('%s = Procedure%s;',[FN,Args])
  else
    AddLn('%s = Function%s: %s;',[FN,Args,RT])
end;

function TTypescriptToPas.WriteFunctionDefinition(const aName : String; const aDefs: TJSFuncDefArray; UseExternal : Boolean): Boolean;

Var
  PN, FN,RT,Suff,Args : String;
  Overloads : TFPObjectList;
  I : Integer;

begin
  Result:=True;
  RT:='';
  if (aDefs[0].IsConstructor) or (aName='&constructor') then
    begin
    PN:='New'
    end
  else
    begin
    PN:=aName;
    FN:=UTF8Encode(aDefs[0].Name);
    if (FN<>'') and ((FN<>StringReplace(aName,'&','',[rfReplaceAll])) or UseExternal) then
      Suff:=Format('; external name ''%s''',[FN]);
    if Assigned(aDefs[0].ResultType) then
      RT:=GetTypeName(aDefs[0].ResultType,False);
    if (RT='void') then
      RT:='';
    end;
  Overloads:=GetOverloads(ADefs);
  try
    if Overloads.Count>1 then
      Suff:=Suff+'; overload';
    For I:=0 to Overloads.Count-1 do
      begin
      Args:=GetArguments(TJSTypedParams(Overloads[i]),False);
      if (RT='') then
        begin
        if (aDefs[0].IsConstructor) then
          AddLn('Constructor %s%s%s;',[PN,Args,Suff])
        else
          AddLn('Procedure %s%s%s;',[PN,Args,Suff]);
        end
      else
        AddLn('Function %s%s: %s%s;',[PN,Args,RT,Suff])
      end;
  finally
    Overloads.Free;
  end;
end;

function TTypescriptToPas.WriteFunctionDefs(aElements: TJSElementNodes; UseExternal : Boolean): Integer;

Var
  aList : TStringList;
  EN : TJSElementNode;
  FN : String;
  aDefs : TJSFuncDefArray;
  aCount : Integer;

begin
  Result:=0;
  aList:=TStringList.Create;
  try
    aList.Sorted:=True;
    aList.Duplicates:=dupIgnore;
    // Get Unique names
    For EN in aElements do
      if ExportNode(EN) then
        aList.Add(GetName(EN.Node));
    // Generate function definition for each unique name
    For FN in aList do
      begin
      // Collect all function defs for this name
      aDefs:=[];
      aCount:=0;
      SetLength(aDefs,aElements.Count);
      For EN in aElements do
        if ExportNode(EN) and (GetName(EN.Node)=FN) then
          begin
          if (EN.Node as TJSFunctionDeclarationStatement).AFunction = Nil then
            DoLog(SLogIgnoringEmptyFunction)
          else
            begin
            aDefs[aCount]:=(EN.Node as TJSFunctionDeclarationStatement).AFunction;
            inc(aCount)
            end
          end;
      SetLength(aDefs,aCount);
      WriteFunctionDefinition(FN,aDefs, UseExternal);
      Inc(Result);
      end;
  finally
    aList.Free;
  end;
end;


{ ----------------------------------------------------------------------
  Classes
  ----------------------------------------------------------------------}

function TTypescriptToPas.WriteForwardClass(const aName : string) : Boolean;

begin
  Result:=FContext.CurrentForwards.IndexOf(aName)=-1;
  if Result then
    AddLn('%s = Class;',[aName])
  else
    DoLog(SLogIgnoreDoubleClassDefinition, [aName]);
end;

function TTypescriptToPas.WriteForwardClassDef(aIntf: TJSInterfaceDeclaration): Boolean;

Var
  N : String;

begin
  N:=GetName(aIntf);
  if Context.CurrentForwards.indexOf(N)=-1 then
    if (coInterfaceAsClass in Options) or (aIntf.HasProperties) then
      AddLn('%s = Class;',[N])
    else
      AddLn('%s = Interface;',[N]);
  Result:=True
end;

function TTypescriptToPas.WriteForwardClassDef(aObj: TJSTypeDeclaration): Boolean;

begin
  Result:=WriteForwardClass(GetName(aObj));
end;

function TTypescriptToPas.WriteForwardClassDef(aClass: TJSClassDeclaration): Boolean;

begin
  Result:=WriteForwardClass(GetName(aClass));
end;

function TTypescriptToPas.WriteForwardClassDef(aModule: TJSModuleDeclaration): Boolean;
begin
  Result:=WriteForwardClass(GetName(aModule));
end;

function TTypescriptToPas.WriteForwardClassDef(aNamespace: TJSNameSpaceDeclaration): Boolean;
begin
  Result:=WriteForwardClass(GetName(aNamespace));
end;

function TTypescriptToPas.WriteForwardClassDefs(aClassList: TJSElementNodes): Integer;

  Procedure MaybeComment;
  begin
    if Result=0 then
      Comment(SForwardClassDefinitions);
  end;

Var
  D : TJSElementNode;


begin
  Result:=0;
  For D in aClassList do
    if (D.Node is TJSTypeDeclaration) and (TJSTypeDeclaration(D.Node).TypeDef is TJSObjectTypeDef) then
      begin
      MaybeComment;
      if WriteForwardClassDef(TJSTypeDeclaration(D.Node)) then
        Inc(Result);
      end
    else if D.Node is TJSClassDeclaration then
      begin
      MaybeComment;
      if WriteForwardClassDef(D.Node as TJSClassDeclaration) then
        Inc(Result);
      end
    else if (D.Node is TJSModuleDeclaration) then
      begin
      MaybeComment;
      if WriteForwardClassDef(D.Node as TJSModuleDeclaration) then
        Inc(Result);
      end
    else if (D.Node is TJSNameSpaceDeclaration)
            and not (NamespaceExtendsClass(D.Node as TJSNamespaceDeclaration))
            and not (NamespaceExtendsModule(D.Node as TJSNamespaceDeclaration))then
      begin
      MaybeComment;
      if WriteForwardClassDef(D.Node as TJSNamespaceDeclaration) then
        Inc(Result);
      end
    else if (D.Node is TJSInterfaceDeclaration) and not TJSInterfaceDeclaration(D.Node).IsFunctionDef then
      begin
      MaybeComment;
      if WriteForwardClassDef(D.Node as TJSInterfaceDeclaration) then
        Inc(Result);
      end;
    // Ignore other types
end;

{ ----------------------------------------------------------------------
  Namespaces
  ----------------------------------------------------------------------}


function TTypescriptToPas.WriteNamespaceDef(aNameSpace: TJSNamespaceDeclaration): Boolean;

Var
  aPasName,aName : String;

begin
  Result:=True;
  aPasName:=GetName(aNameSpace);
  aName:=GetExternalMemberName(aNamespace.Name);
  AddLn('');
  AddLn(Format('%s = class external name ''%s'' (TJSObject)',[aPasName,aName]));
  Addln('Public');
  Indent;
    PushSection();
    WriteSourceElements(aNameSpace.Members,aNamespace.Name);
    PopSection;
  Undent;
  AddLn('end;');
  AddLn('');
end;

Function TTypescriptToPas.NamespaceExtendsClass(aNs : TJSNamespaceDeclaration) : Boolean;

begin
  Result:=HaveClass(aNS.Name);
end;

function TTypescriptToPas.NamespaceExtendsModule(aNs: TJSNamespaceDeclaration): Boolean;
begin
  Result:=HaveModule(aNS.Name);
end;

function TTypescriptToPas.HaveClass(const aName: TJSString): Boolean;

Var
  I : Integer;

begin
  Result:=False;
  I:=Context.CurrentScope.Classes.Count-1;
  While (Not Result) and (I>=0) do
    begin
    Result:=(aName) = TJSClassDeclaration(Context.CurrentScope.Classes[i].Node).Name;
    Dec(I);
    end;
end;

function TTypescriptToPas.HaveModule(const aName: TJSString): Boolean;
Var
  I : Integer;

begin
  Result:=False;
  I:=Context.CurrentScope.Modules.Count-1;
  While (Not Result) and (I>=0) do
    begin
    Result:=(aName) = TJSClassDeclaration(Context.CurrentScope.Modules[i].Node).Name;
    Dec(I);
    end;
end;

function TTypescriptToPas.WriteNamespaceDefs(aNameSpaces: TJSElementNodes): Integer;


Var
  EN : TJSElementNode;
  NSDef : TJSNamespaceDeclaration;


begin
  Result:=0;
  For EN in aNameSpaces do
   begin
   NSDef:=EN.Node as TJSNamespaceDeclaration;
   if Not NamespaceExtendsClass(NSDef) then
     begin
     If Result=0 then
       Comment('Namespaces');
     WriteNameSpaceDef(NSDef);
     end;
   end;
end;

{ ----------------------------------------------------------------------
  Modules
  ----------------------------------------------------------------------}


function TTypescriptToPas.WriteModuleDef(aModule: TJSModuleDeclaration): Boolean;

Var
  aPasName,aName : String;

begin
  Result:=True;
  aPasName:=GetName(aModule);
  aName:=GetExternalMemberName(aModule.Name);
  AddLn('');
  AddLn(Format('%s = class external name ''%s'' (TJSObject)',[aPasName,aName]));
  Addln('Public');
  Indent;
    PushSection();
    WriteSourceElements(aModule.Members,aModule.Name);
    PopSection;
  Undent;
  AddLn('end;');
  AddLn('');
end;

function TTypescriptToPas.WriteModuleDefs(aModules: TJSElementNodes): Integer;

  Function ExtendsClass(aNs : TJSModuleDeclaration) : Boolean;

  Var
    I : Integer;

  begin
    Result:=False;
    I:=Context.CurrentScope.Classes.Count-1;
    While (Not Result) and (I>=0) do
      begin
      Result:=(aNS.Name) = TJSClassDeclaration(Context.CurrentScope.Classes[i].Node).Name;
      Dec(I);
      end;
  end;

Var
  EN : TJSElementNode;
  NSDef : TJSModuleDeclaration;


begin
  Result:=0;
  For EN in aModules do
   begin
   NSDef:=EN.Node as TJSModuleDeclaration;
   if Not ExtendsClass(NSDef) then
     begin
     If Result=0 then
       Comment('Modules');
     WriteModuleDef(NSDef);
     end;
   end;
end;


{ ----------------------------------------------------------------------
  Interfaces
  ----------------------------------------------------------------------}

function TTypescriptToPas.WriteInterfaceDef(Intfs: TJSInterfaceDeclarationArray): Boolean;

Var
  CN,Decl,Sect : String;
  UseLocal, UseClass : Boolean;
  aCount : Integer;
  PD : TPasData;
  Func : TJSFuncDef;
  Intf0: TJSInterfaceDeclaration;
  Inf: TJSInterfaceDeclaration;

begin
  Intf0:=Intfs[0];
  if Intf0.IsFunctionDef then
    begin
    PD:=TPasData(Intf0.Data);
    Func:=intf0.FunctionDef;
    WriteMethodParameterDefs(intf0.Values);
    WriteFunctionTypeDef(PD.PasName,PD.OriginalName,Intf0.TypeParams,Func);
    Exit;
    end;
  Result:=True;
  UseClass:=False;
  CN:=GetName(Intf0);
  For Inf in Intfs do
    useClass:=useClass or (coInterfaceAsClass in Options) or Inf.HasProperties;
  UseLocal:=(coLocalArgumentTypes in Options) and UseClass;
  if not UseLocal then
    begin
    for Inf in Intfs do
      begin
      WritePropertyTypeDefs(inf.Values);
      WriteMethodParameterDefs(inf.Values);
      end;
    end;
  if UseClass then
    Decl:=Format('%s = class external name ''Object'' (TJSObject)',[CN])
  else
    Decl:=Format('%s = interface',[CN]);
  AddLn(Decl);
  if UseLocal then
    begin
    aCount:=0;
    for Inf in Intfs do
      begin
      if (aCount>0) then
        Sect:=''
      else
        Sect:='Public';
      aCount:=WritePropertyTypeDefs(inf.Values,Sect);
      if (aCount>0) then
        Sect:=''
      else
        Sect:='Public';
      WriteMethodParameterDefs(inf.Values,Sect);
      end;
    end;
  Indent;
  for inf in Intfs do
    WriteObjectTypeMembers(CN,Inf.name,Inf.TypeParams,Inf);
  Undent;
  AddLn('end;');
  AddLn('');
end;

function TTypescriptToPas.WriteInterfaceDefs(aList: TJSElementNodes): Integer;

  Function GetInterfaces(const aName : String) : TJSInterfaceDeclarationArray;

  Var
    I,aCount : Integer;
    N : TJSElement;

  begin
    aCount:=0;
    Result:=[];
    SetLength(Result,aList.Count);
    For I:=0 to aList.Count-1 do
      begin
      N:=aList[I].Node;
      if N is TJSInterfaceDeclaration then
        if aName=GetName(N) then
          begin
          Result[aCount]:=TJSInterfaceDeclaration(N);
          Inc(aCount);
          end;
      end;
    SetLength(Result,aCount);
  end;


Var
  I : Integer;
  N : TJSElement;
  IntfDecl : TJSInterfaceDeclarationArray;
  aName : String;
  L : TStringList;

begin
  Result:=0;
  EnsureSection(csType);
  L:=TStringList.Create;
  try
    L.Duplicates:=DupIgnore;
    for I:=0 to aList.Count-1 do
      if ExportNode(aList[i]) then
        begin
        N:=aList[I].Node;
        // TJSEnumDeclaration is a descendent
        if N is TJSInterfaceDeclaration then
          L.Add(GetName(N));
        end;
    For I:=0 to L.Count-1 do
     begin
     aName:=L[I];
     IntfDecl:=GetInterfaces(aName);
     if Length(IntfDecl)>0 then
       begin
       if Length(IntfDecl)>1 then
         DoLog(SLogFoldingInterfaceDefinitions, [Length(IntfDecl), aName]);
       if WriteInterfaceDef(IntfDecl) then
         Inc(Result);
       end;
     end;
  finally
    L.Free;
  end;
end;

end.

