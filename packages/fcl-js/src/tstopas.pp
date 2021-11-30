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

  { TPasData }

  TPasData = Class(TObject)
  private
    FOriginalName: String;
    FPasName: String;
  Public
    Constructor Create(const aOriginalName : jsBase.TJSString; const APasName : String);
    Destructor destroy; override;
    Property PasName : String read FPasName;
    Property OriginalName : String Read FOriginalName;
  end;

  TConversionOption = (coRaw,coGenericArrays,coUseNativeTypeAliases,coExternalConst,coExpandUnionTypeArgs,coaddOptionsToheader);
  TConversionOptions = Set of TConversionOption;

  TTypescriptToPas = Class;

  { TTSContext }

  TTSContext = class(TObject)
  Private
    FTypeMap : TFPObjectHashTable;
    FTypeDeclarations : TFPObjectList;
    FConverter : TTypescriptToPas;
    procedure TypesToMap;
  Public
    Constructor Create(aConverter : TTypescriptToPas);
    Destructor Destroy; override;
    Procedure AddAliases(aAliases : TStrings);
    Function FindTypeAlias(aName : jsbase.TJSString) : String;
    Procedure AddToTypeMap(aName : UTF8String; const aPasName : String);
    Procedure AddToTypeMap(aName : jsbase.TJSString; const aPasName : String);
    Procedure AddToTypeMap(aType : TJSElement);
    Property TypeMap : TFPObjectHashTable Read FTypeMap;
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
    Function CreateElement(AElementClass : TJSElementClass)  : TJSElement; override;
  Public
    Property Context : TTSContext Read FContext Write FContext;
  end;

  { TTypescriptToPas }

  TTypescriptToPas = Class(TPascalCodeGenerator)
  private
    FClassPrefix: String;
    FClassSuffix: String;
    FContext: TTSContext;
    FDictionaryClassParent: String;
    FElements: TJSFunctionBody;
    FFieldPrefix: String;
    FIncludeImplementationCode: TStrings;
    FIncludeInterfaceCode: TStrings;
    FInputFileName: String;
    FInputStream: TStream;
    FOptions: TConversionOptions;
    FOutputFileName: String;
    FTypeAliases: TStrings;
    FVerbose: Boolean;
    FECMAVersion: TECMAVersion;
    FPasNameList : TFPObjectList;
    FAutoTypes : TStrings;
    procedure DumpElements;
    function GetIsRaw: Boolean;
    procedure SetIncludeImplementationCode(AValue: TStrings);
    procedure SetIncludeInterfaceCode(AValue: TStrings);
    procedure SetTypeAliases(AValue: TStrings);
  Protected
    function GetGenericParams(aTypeParams: TJSElementNodes): String; virtual;
    procedure AddOptionsToHeader;
    Procedure Parse; virtual;
    Procedure WritePascal; virtual;
    function CreateParser(aContext: TTSContext; S: TJSScanner): TJSParser; virtual;
    function CreateScanner(aContext : TTSContext; S: TStream): TJSScanner;virtual;
    Function CreateContext : TTSContext; virtual;
    Function BaseUnits : String; override;
    // Auxiliary routines
    procedure Getoptions(L: TStrings); virtual;
    procedure ProcessDefinitions; virtual;
    function CreatePasName(const aOriginal : jsBase.TJSString; const aName: String): TPasData;virtual;
    procedure AllocatePasNames(aList: TJSSourceElements; ParentName: String=''); virtual;
    procedure AllocatePasNames(aList: TJSElementNodes; ParentName: String=''); virtual;
    Function AllocatePasName(D: TJSElement; ParentName: String='') : TPasData;virtual;
    procedure EnsureUniqueNames(ML: TJSSourceElements);virtual;
    function GetName(ADef: TJSElement): String;virtual;
    function HaveConsts(aList: TJSSourceElements): Boolean;virtual;
    function GetTypeName(Const aTypeName: JSBase.TJSString; ForTypeDef: Boolean=False): String;virtual;
    function GetTypeName(aTypeDef: TJSTypeDef; ForTypeDef: Boolean=False): String;virtual;
{    function AddSequenceDef(ST: TIDLSequenceTypeDefDefinition): Boolean; virtual;
    function CheckUnionTypeDefinition(D: TIDLDefinition): TIDLUnionTypeDefDefinition;virtual;
    procedure AddArgumentToOverloads(aList: TFPObjectlist; AName, ATypeName: String);virtual;
    procedure AddUnionOverloads(aList: TFPObjectlist; AName: String;  UT: TIDLUnionTypeDefDefinition);virtual;
    procedure AddArgumentToOverloads(aList: TFPObjectlist; adef: TIDLArgumentDefinition);virtual;
    procedure AddOverloads(aList: TFPObjectlist; adef: TIDLFunctionDefinition; aIdx: Integer);virtual;
    function CloneNonPartialArgumentList(aList: TFPObjectlist; ADest: TFPObjectlist= Nil; AsPartial: Boolean=True): integer;virtual;
    function GetOverloads(aDef: TIDLFunctionDefinition): TFPObjectlist;virtual;
    function GetArguments(aList: TIDLDefinitionList; ForceBrackets: Boolean): String;virtual;
    // Actual code generation routines
    // Lists. Return the number of actually written defs.
    function WriteCallBackDefs(aList: TIDLDefinitionList): Integer; virtual;
    Function WriteDictionaryDefs(aList: TIDLDefinitionList) : Integer;virtual;
    Function WriteForwardClassDefs(aList: TIDLDefinitionList) : Integer;virtual;
    Function WriteInterfaceDefs(aList: TIDLDefinitionList) : Integer;virtual;
    Function WriteMethodDefs(aList: TIDLDefinitionList) : Integer;virtual;
    Function WriteEnumDefs(aList: TIDLDefinitionList) : Integer;virtual;
    function WriteConsts(aList: TIDLDefinitionList): Integer;virtual;
    function WritePlainFields(aList: TIDLDefinitionList): Integer;virtual;
    function WriteDictionaryFields(aList: TIDLDefinitionList): Integer;virtual;
    function WritePrivateReadOnlyFields(aList: TIDLDefinitionList): Integer;virtual;
    // Actual definitions. Return true if a definition was written.
    Function WriteForwardClassDef(D: TIDLStructuredDefinition) : Boolean;virtual;
    function WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition): Boolean;virtual;
    function WriteFunctionDefinition(aDef: TIDLFunctionDefinition): Boolean;virtual;
    function WriteTypeDef(aDef: TIDLTypeDefDefinition): Boolean; virtual;
    function WriteRecordDef(aDef: TIDLRecordDefinition): Boolean; virtual;
    function WriteEnumDef(aDef: TIDLEnumDefinition): Boolean; virtual;
    function WriteDictionaryField(aField: TIDLDictionaryMemberDefinition): Boolean;virtual;
    Function WritePrivateReadOnlyField(aAttr: TIDLAttributeDefinition) : Boolean;virtual;
    Function WriteField(aAttr: TIDLAttributeDefinition) : Boolean;virtual;
    Function WriteReadonlyProperty(aAttr: TIDLAttributeDefinition) : Boolean;virtual;
    Function WriteConst(aConst: TIDLConstDefinition) : Boolean ;virtual;
    function WriteInterfaceDef(Intf: TIDLInterfaceDefinition): Boolean; virtual;
    function WriteDictionaryDef(aDict: TIDLDictionaryDefinition): Boolean; virtual;
    // Additional
    procedure WritePromiseDef(aDef: TIDLPromiseTypeDefDefinition);virtual;
    procedure WriteSequenceDef(aDef: TIDLSequenceTypeDefDefinition);virtual;
    procedure WriteUnionDef(aDef: TIDLUnionTypeDefDefinition);virtual;
    }
    // Properties
    procedure WritePropertyDeclaration(D: TJSVariableStatement);
    function WriteProperties(aClass: TJSClassDeclaration): Integer;
    // Variables
    procedure WriteVariable(aVar: TJSVarDeclaration);
    procedure WriteVariables(Vars: TJSElementNodes); virtual;
    // Get type defs as string
    function GetTypeAsString(aType: TJSTypeDef; asPascal, asSubType: Boolean): String;
    function GetArrayTypeAsString(aTypeDef: TJSArrayTypeDef; asPascal, asSubType: Boolean): String;
    function GetAliasTypeAsString(aTypeDef: TJSTypeReference; asPascal, asSubType: Boolean): string;
    function GetIntersectionTypeAsString(aTypeDef: TJSIntersectionTypeDef; asPascal, asSubType: Boolean): String;
    function GetUnionTypeAsString(aTypeDef: TJSUnionTypeDef; asPascal, asSubType: Boolean): String;

    // Write types
    procedure WriteTypeDefs(Types: TJSElementNodes); virtual;
    procedure WriteAliasTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes;  aTypeDef: TJSTypeReference); virtual;
    procedure WriteTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSTypeDef);
    procedure WriteUnionTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSUnionTypeDef);
    procedure WriteIntersectionTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef: TJSIntersectionTypeDef);
    procedure WriteArrayTypeDef(const aPasName: string; const aOrgName: jsBase.TJSString; aTypeParams: TJSElementNodes;  aTypeDef: TJSArrayTypeDef);

    // Extra interface/Implementation code.
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
  end;

implementation

uses typinfo;

{ TTSJSParser }

function TTSJSParser.CreateElement(AElementClass: TJSElementClass): TJSElement;
begin
  Result:=inherited CreateElement(AElementClass);
  If Result is TJSTypeDeclaration then
    FContext.AddToTypeMap(Result);
end;

{ TTSContext }

constructor TTSContext.Create(aConverter : TTypescriptToPas);
begin
  FConverter:=aConverter;
  FTypeMap:=TFPObjectHashTable.Create(False);
  FTypeDeclarations:=TFPObjectList.Create(False);
end;

destructor TTSContext.Destroy;
begin
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
    AddToTypeMap(UTF8String(N),V);
    end;
end;

function TTSContext.FindTypeAlias(aName: jsbase.TJSString): String;

Var
  S : UTF8String;
  Obj : TObject;

begin
  S:=UTF8Encode(aName);
  if FTypeDeclarations.Count>0 then
    TypesToMap;
  Obj:=FTypeMap.Items[S];
  if (Obj is TPasData) then
    Result:=TPasData(Obj).PasName
  else
    Result:=S;
end;

procedure TTSContext.TypesToMap;

Var
  I : Integer;
  el : TJSElement;

begin
  For I:=0 to FTypeDeclarations.Count-1 do
    begin
    El:=TJSElement(FTypeDeclarations[i]);
    if El.Data=Nil then
      FConverter.AllocatePasName(El,'');
    FTypeMap.Add(TPasData(El.Data).OriginalName,El.Data) ;
    end;
  FTypeDeclarations.Clear;
end;

procedure TTSContext.AddToTypeMap(aName: UTF8String; const aPasName: String);
begin
  FTypeMap.Add(aName,FConverter.CreatePasName(aName,aPasName));
end;

procedure TTSContext.AddToTypeMap(aName: jsbase.TJSString; const aPasName: String);
begin
  AddToTypeMap(UTF8Encode(aName),aPasName);
end;

procedure TTSContext.AddToTypeMap(aType: TJSElement);
begin
  FTypeDeclarations.Add(aType);
end;

{ TPasData }

constructor TPasData.Create(const aOriginalName : jsBase.TJSString; const APasName : String);
begin
  FOriginalName:=aOriginalName;
  FPasName:=APasName;
end;

destructor TPasData.destroy;
begin
  Writeln('Destroying ',Self.FOriginalName,'->',Self.Pasname);
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
    Writeln(aSection,': ',aList.Count,' elements');
    For I:=0 to aList.Count-1 do
      begin
      N:=Alist[i];
      Writeln(aSection,' element ',I,' : ',N.Node.ClassName);
      end;
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
      Raise ETStoPas.Create('Parse result is not a function body');
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

function TTypescriptToPas.GetName(ADef: TJSElement): String;

begin
  If Assigned(ADef) and (TObject(ADef.Data) is TPasData) then
    Result:=TPasData(ADef.Data).PasName
  else if aDef is TJSNamedElement then
    Result:=TJSNamedElement(ADef).Name
  else
    Result:='';
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


  Function UsePascalType(Const aPascalType : string) : String;

  begin
    if (coUseNativeTypeAliases in Options) and ForTypeDef then
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
    TN:=FContext.FindTypeAlias(aTypeName);
  end;
  Result:=TN;
end;


function TTypescriptToPas.GetTypeName(aTypeDef : TJSTypeDef; ForTypeDef : Boolean = False): String;

begin
  if (aTypeDef.Data is TPasData) then
    Result:=TPasData(aTypeDef.Data).PasName
  else if ATypeDef is TJSTypeReference then
    Result:=GetTypeName(TJSTypeReference(aTypeDef).Name,ForTypeDef)
  else
    Raise ETSToPas.Create('Cannot get type name from '+aTypeDef.ClassName);
end;


function TTypescriptToPas.WriteProperties(aClass: TJSClassDeclaration): Integer;

Var
  I : Integer;
  D : TJSVariableStatement;

begin
  For I:=0 to aClass.Members.Vars.Count-1 do
    begin
    D:=aClass.Members.Vars[i].Node as TJSVariableStatement;
    if (D.VarType=vtVar) then
      WritePropertyDeclaration(D);
    end;
end;

function TTypescriptToPas.GetGenericParams(aTypeParams: TJSElementNodes) : String;

Var
  I : Integer;
  aName: jsBase.TJSString;

begin
  Result:='';
  if aTypeParams=nil then exit;
  For I:=0 to aTypeParams.Count-1 do
    begin
    aName:=(aTypeParams[i].Node as TJSTypeReference).Name;
    if Result<>'' then
      Result:=Result+',';
    Result:=Result+aName;
    end;
  if Result<>'' then
    Result:='<'+Result+'>';
end;

Function TTypescriptToPas.GetAliasTypeAsString(aTypeDef : TJSTypeReference; asPascal, asSubType: Boolean) : string;

begin
  if asPascal then
    Result:=GetTypeName(aTypeDef.Name,True)
  else
    Result:=aTypeDef.Name
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

(*
function TTypescriptToPas.WriteConst(aConst: T): Boolean;

Const
  ConstTypes : Array[TConstType] of String =
     ('Double','NativeInt','Boolean','JSValue','JSValue','JSValue','JSValue','String','JSValue','JSValue');
Var
  S : String;

begin
  Result:=True;
  // Consts cannot be strings
  if coExternalConst in Options then
    begin
    S:=ConstTypes[aConst.ConstType];
    Addln('%s : %s;',[GetName(aConst),S])
    end
  else
    begin
    S:=aConst.Value;
    if aConst.ConstType=ctInteger then
      S:=StringReplace(S,'0x','$',[]);
    Addln('%s = %s;',[GetName(aConst),S])
    end;
end;

function TTypescriptToPas.WriteConsts(aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;

begin
  EnsureSection(csConst);
  Indent;
  Result:=0;
  For D in aList do
    if D is TIDLConstDefinition then
      if WriteConst(D as TIDLConstDefinition) then
        Inc(Result);
  Undent;
end;

function TTypescriptToPas.WritePlainFields(aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;
  A : TIDLAttributeDefinition absolute D;

begin
  EnsureSection(csDeclaration);
  Indent;
  Result:=0;
  For D in aList do
    if D is TIDLAttributeDefinition then
      if Not (aoReadOnly in A.Options) then
        if WriteField(A) then
          Inc(Result);
  Undent;
end;

function TTypescriptToPas.WriteDictionaryField(
  aField: TIDLDictionaryMemberDefinition): Boolean;

Var
  Def,N,TN : String;

begin
  Result:=True;
  N:=GetName(aField);
  TN:=GetTypeName(aField.MemberType);
  if TN='record' then
    TN:='TJSObject';
  if SameText(N,TN) then
    N:='_'+N;
  Def:=Format('%s : %s;',[N,TN]);
  if (N<>aField.Name) then
    Def:=Def+Format('external name ''%s'';',[aField.Name]);
  AddLn(Def);
end;

function TTypescriptToPas.WriteDictionaryFields(aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;
  M : TIDLDictionaryMemberDefinition absolute D;

begin
  Indent;
  Result:=0;
  For D in aList do
    if D is TIDLDictionaryMemberDefinition then
      if WriteDictionaryField(M) then
        Inc(Result);
  Undent;
end;

function TTypescriptToPas.WriteMethodDefs(aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;
  FD : TIDLFunctionDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if D is TIDLFunctionDefinition then
      if Not (foCallBack in FD.Options) then
         if WriteFunctionDefinition(FD) then
           Inc(Result);
end;

function TTypescriptToPas.AddSequenceDef(ST: TIDLSequenceTypeDefDefinition
  ): Boolean;

var
  TN : String;
begin
  TN:=GetTypeName(ST);
  Result:=FAutoTypes.IndexOf(TN)=-1;
  if Result then
    begin
    FAutoTypes.Add(TN);
    DoLog('Automatically adding %s sequence definition.',[TN]);
    AddLn('%s = Array of %s;',[TN,GetTypeName(ST.ElementType)]);
    ST.Data:=CreatePasName(TN);
    end;
end;

function TTypescriptToPas.WriteFunctionImplicitTypes(aList: TIDLDefinitionList): Integer;

Var
  D,D2,D3 : TIDLDefinition;
  FD : TIDLFunctionDefinition absolute D;
  DA : TIDLArgumentDefinition absolute D2;
  UT : TIDLUnionTypeDefDefinition;

begin
  Result:=0;
  for D in aList do
    if D is TIDLFunctionDefinition then
      if Not (foCallBack in FD.Options) then
        begin
        if (FD.ReturnType is TIDLSequenceTypeDefDefinition) then
          if AddSequenceDef(FD.ReturnType as TIDLSequenceTypeDefDefinition) then
            Inc(Result);
        For D2 in FD.Arguments do
          if (DA.ArgumentType is TIDLSequenceTypeDefDefinition) then
            begin
            if AddSequenceDef(DA.ArgumentType as TIDLSequenceTypeDefDefinition) then
              Inc(Result);
            end
          else
            begin
            UT:=CheckUnionTypeDefinition(DA.ArgumentType);
            if Assigned(UT) then
              For D3 in UT.Union do
                if (D3 is TIDLSequenceTypeDefDefinition) then
                  if AddSequenceDef(D3 as TIDLSequenceTypeDefDefinition) then
                    Inc(Result);
            end;
        end;
  if Result>0 then
    AddLn('');
end;

function TTypescriptToPas.WriteAttributeImplicitTypes(aList: TIDLDefinitionList
  ): Integer;
Var
  D : TIDLDefinition;
  FA : TIDLAttributeDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if D is TIDLAttributeDefinition then
      if (FA.AttributeType is TIDLSequenceTypeDefDefinition) then
        if AddSequenceDef(FA.AttributeType as TIDLSequenceTypeDefDefinition) then
          Inc(Result);
end;

function TTypescriptToPas.WriteDictionaryMemberImplicitTypes(
  aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;
  FD : TIDLDictionaryMemberDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if D is TIDLDictionaryMemberDefinition then
      if (FD.MemberType is TIDLSequenceTypeDefDefinition) then
        if AddSequenceDef(FD.MemberType as TIDLSequenceTypeDefDefinition) then
          Inc(Result);
end;

procedure TTypescriptToPas.EnsureUniqueNames(ML : TIDLDefinitionList);

Var
  L : TFPObjectHashTable;

  Procedure CheckRename(aD : TIDLDefinition);

  var
    I : integer;
    NOrig,N,N2 : String;
    isDup : Boolean;
    D2 : TIDLDefinition;

  begin
    NOrig:=GetName(aD);
    N:=LowerCase(NOrig);
    N2:=N;
    I:=0;
    isDup:=False;
    Repeat
      D2:=TIDLDefinition(L.Items[N2]);
      if (D2<>Nil) then
        // Overloads
        begin
        isDup:=((D2 is TIDLFunctionDefinition) and (ad is TIDLFunctionDefinition));
        if IsDup then
          D2:=Nil
        else
          begin
          inc(I);
          N2:=KeywordPrefix+N+KeywordSuffix;
          Norig:=KeywordPrefix+NOrig+KeywordSuffix;
          end;
        end;
    Until (D2=Nil);
    if (N<>N2) then
      begin
      N:=GetName(aD);
      DoLog('Renaming duplicate identifier (%s) %s to %s',[aD.ClassName,N,Norig]);
      // Original TPasName is in list, will be freed automatically
      aD.Data:=CreatePasName(NOrig);
      end;
    if not IsDup then
      L.Add(N2,aD);
  end;

var
  D : TIDLDefinition;

begin
  L:=TFPObjectHashTable.Create(False);
  try
    For D in ML Do
      if not (D is TIDLConstDefinition) then
        CheckRename(D);
    For D in ML Do
      if (D is TIDLConstDefinition) then
        CheckRename(D);
  finally
    L.Free;
  end;
end;

function TTypescriptToPas.WriteInterfaceDef(Intf: TIDLInterfaceDefinition): Boolean;

Var
  CN,PN : String;
  Decl : String;
  ML : TIDLDefinitionList;

begin
  Result:=True;
  ML:=TIDLDefinitionList.Create(Nil,False);
  try
    Intf.GetFullMemberList(ML);
    EnsureUniqueNames(ML);
    CN:=GetName(Intf);
    ClassHeader(CN);
    WriteFunctionImplicitTypes(ML);
    WriteAttributeImplicitTypes(ML);
    Decl:=Format('%s = class external name %s ',[CN,MakePascalString(Intf.Name,True)]);
    if Assigned(Intf.ParentInterface) then
      PN:=GetName(Intf.ParentInterface)
    else
      PN:=GetTypeName(Intf.ParentName);
    if PN<>'' then
      Decl:=Decl+Format(' (%s)',[PN]);
    AddLn(Decl);
    AddLn('Private');
    Indent;
    WritePrivateReadOnlyFields(ML);
    Undent;
    AddLn('Public');
    if HaveConsts(ML) then
      begin
      Indent;
      PushSection(csUnknown);
      WriteConsts(ML);
      PopSection;
      Undent;
      AddLn('Public');
      end;
    Indent;
    WritePlainFields(ML);
    WriteMethodDefs(ML);
    WriteProperties(ML);
    Undent;
    AddLn('end;');
  finally
    ML.Free;
  end;
end;

function TTypescriptToPas.WriteDictionaryDef(aDict: TIDLDictionaryDefinition
  ): Boolean;

Var
  CN,CP : String;
  ML : TIDLDefinitionList;
  PD: TIDLDictionaryDefinition;

begin
  Result:=True;
  ML:=TIDLDefinitionList.Create(Nil,False);
  try
    PD:=aDict;
    While PD<>Nil do
      begin
      PD.GetFullMemberList(ML);
      PD:=PD.ParentDictionary;
      end;
    CN:=GetName(aDict);
    CP:=DictionaryClassParent;
    if CP='' then
      CP:='TJSObject';
    ClassHeader(CN);
    WriteDictionaryMemberImplicitTypes(ML);
    if (coDictionaryAsClass in Options) then
      Addln('%s = class(%s)',[CN,CP])
    else
      Addln('%s = record',[CN]);
    WriteDictionaryFields(ML);
    AddLn('end;');
  finally
    ML.Free;
  end;
end;

procedure TTypescriptToPas.WriteImplementation;

Var
  S : String;

begin
  Addln('');
  For S in FIncludeImplementationCode do
    Addln(S);
  Addln('');
end;

function TTypescriptToPas.GetTypeName(aTypeDef : TIDLTypeDefDefinition; ForTypeDef : Boolean = False): String;

begin
  if ATypeDef is TIDLSequenceTypeDefDefinition then
    begin
    if Assigned(aTypeDef.Data) then
      Result:=GetName(aTypeDef)
    else
      begin
      Result:=GetTypeName(TIDLSequenceTypeDefDefinition(aTypeDef).ElementType,ForTypeDef);
      Result:='T'+Result+'DynArray';
      end
    end
  else
    Result:=GetTypeName(aTypeDef.TypeName,ForTypeDef);
end;

function TTypescriptToPas.GetTypeName(const aTypeName: String; ForTypeDef: Boolean
  ): String;


  Function UsePascalType(Const aPascalType : string) : String;

  begin
    if (coUseNativeTypeAliases in Options) and ForTypeDef then
      Result:=StringReplace(aTypeName,' ','',[rfReplaceAll])
    else
      Result:=aPascalType;
  end;

Var
  A,TN : UTF8String;
  D : TIDLDefinition;

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
    'Float64Array' : TN:='TJS'+aTypeName;
  else
    TN:=aTypeName;
    D:=FContext.FindDefinition(TN);
    if D<>Nil then
      TN:=GetName(D)
    else
      begin
      A:=FTypeAliases.Values[TN];
      If (A<>'') then
        TN:=A;
      end;
  end;
  Result:=TN;
end;

function TTypescriptToPas.WritePrivateReadOnlyField(aAttr: TIDLAttributeDefinition
  ): Boolean;

begin
  AddLn('%s%s : %s; external name ''%s''; ',[FieldPrefix,GetName(aAttr),GetTypeName(aAttr.AttributeType),aAttr.Name]);
end;

function TTypescriptToPas.WriteField(aAttr: TIDLAttributeDefinition): Boolean;

Var
  Def,TN,N : String;

begin
  Result:=True;
  N:=GetName(aAttr);
  TN:=GetTypeName(aAttr.AttributeType);
  if TN='record' then
    TN:='TJSObject';
  if SameText(N,TN) then
    N:='_'+N;
  Def:=Format('%s : %s;',[N,TN]);
  if (N<>aAttr.Name) then
    Def:=Def+Format('external name ''%s'';',[aAttr.Name]);
  AddLn(Def);
end;

function TTypescriptToPas.WriteReadonlyProperty(aAttr: TIDLAttributeDefinition
  ): Boolean;

Var
  TN,N,PN : String;

begin
  Result:=True;
  N:=GetName(aAttr);
  PN:=N;
  TN:=GetTypeName(aAttr.AttributeType);
  if SameText(PN,TN) then
    PN:='_'+PN;
  AddLn('Property %s : %s Read %s%s; ',[PN,TN,FieldPrefix,N]);
end;


function TTypescriptToPas.WriteForwardClassDef(D: TIDLStructuredDefinition): Boolean;

begin
  Result:=not D.IsPartial;
  if Result then
    AddLn('%s = Class;',[GetName(D)]);
end;

function TTypescriptToPas.WriteForwardClassDefs(aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;

begin
  Result:=0;
  Comment('Forward class definitions');
  For D in aList do
    if D is TIDLInterfaceDefinition then
      if WriteForwardClassDef(D as TIDLInterfaceDefinition) then
        Inc(Result);
  if coDictionaryAsClass in Options then
    For D in aList do
      if D is TIDLDictionaryDefinition then
        if WriteForwardClassDef(D as TIDLDictionaryDefinition) then
          Inc(Result);
end;

procedure TTypescriptToPas.WriteSequenceDef(aDef : TIDLSequenceTypeDefDefinition);

begin
  Addln('%s = array of %s;',[GetName(aDef),GetTypeName(aDef.ElementType)])
end;


procedure TTypescriptToPas.WriteUnionDef(aDef : TIDLUnionTypeDefDefinition);

Var
  S : UTF8String;
  D : TIDLDefinition;
begin
  S:='';
  For D in adef.Union do
    begin
    if (S<>'') then
      S:=S+', ';
    S:=S+(D as TIDLTypeDefDefinition).TypeName;
    end;
  Comment('Union of '+S);
  AddLn('%s = JSValue; ',[GetName(aDef)])
end;


procedure TTypescriptToPas.WritePromiseDef(aDef : TIDLPromiseTypeDefDefinition);

begin
  AddLn('%s = TJSPromise;',[GetName(aDef)]);
end;

procedure TTypescriptToPas.WriteAliasTypeDef(aDef : TJSTypeDeclaration);

Var
  TN : String;

begin
  TN:=GetTypeName((aDef.TypeDef as TJSTypeReference).Name,True);
  AddLn('%s = %s;',[GetName(aDef),TN]);
end;

function TTypescriptToPas.WriteTypeDef(aDef: TJSTypeDeclaration): Boolean;

begin
  Result:=True;
  if ADef is TIDLSequenceTypeDefDefinition then
    WriteSequenceDef(aDef as TIDLSequenceTypeDefDefinition)
  else if ADef is TIDLUnionTypeDefDefinition then
    WriteUnionDef(aDef as TIDLUnionTypeDefDefinition)
  else if ADef is TIDLPromiseTypeDefDefinition then
    WritePromiseDef(aDef as TIDLPromiseTypeDefDefinition)
  else if ADef is TIDLRecordDefinition then
    WriteRecordDef(aDef as TIDLRecordDefinition)
  else
    WriteAliasTypeDef(aDef);
end;

function TTypescriptToPas.WriteRecordDef(aDef: TIDLRecordDefinition): Boolean;

Var
  KT,VT : String;

begin
  Result:=True;
  KT:=GetTypeName(aDef.KeyType);
  VT:=GetTypeName(aDef.ValueType);
  AddLn('%s = Class(TJSObject)',[GetName(aDef)]);
  AddLn('private');
  Indent;
  AddLn('function GetValue(aKey: %s): %s; external name ''[]'';',[KT,VT]);
  AddLn('procedure SetValue(aKey: %s; const AValue: %s); external name ''[]'';',[KT,VT]);
  undent;
  AddLn('public');
  Indent;
  AddLn('property Values[Name: %s]: %s read GetProperties write SetProperties; default;',[KT,VT]);
  undent;
  AddLn('end;');
end;


function TTypescriptToPas.WriteEnumDef(aDef: TIDLEnumDefinition): Boolean;

begin
  Result:=True;
  AddLn('%s = String;',[GetName(aDef)]);
end;

function TTypescriptToPas.WriteEnumDefs(aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;
  ED : TIDLEnumDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLEnumDefinition then
      if WriteEnumDef(ED) then
        Inc(Result);
end;

function TTypescriptToPas.GetArguments(aList: TIDLDefinitionList;
  ForceBrackets: Boolean): String;

Var
  I : TIDLDefinition;
  A : TIDLArgumentDefinition absolute I;
  Arg : string;

begin
  Result:='';
  For I in aList do
    begin
    Arg:=GetName(A);
    Arg:=Arg+' : '+GetTypeName(A.ArgumentType);
    if Result<>'' then
      Result:=Result+'; ';
    Result:=Result+Arg;
    end;
  if (Result<>'') or ForceBrackets then
    Result:='('+Result+')';
end;

Type
  // A partial argument list is a list which has been generated for a optional argument.
  // Additional arguments can never be added to a partial list...
  TIDLPartialDefinitionList = Class(TIDLDefinitionList);

function TTypescriptToPas.CloneNonPartialArgumentList(aList: TFPObjectlist;
  ADest: TFPObjectlist; AsPartial: Boolean): integer;

Var
  I,J : Integer;
  CD : TIDLDefinition;
  DL,CL : TIDLDefinitionList;

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
        CD:=(DL.Definitions[J] as TIDLArgumentDefinition).Clone(Nil);
        CL.Add(CD);
        AllocatePasName(CD);
        end;
      end;
    Dec(I);
    end;
end;

procedure TTypescriptToPas.AddArgumentToOverloads(aList: TFPObjectlist; AName,ATypeName : String);

Var
  I : Integer;
  CD : TIDLArgumentDefinition;
  DL : TIDLDefinitionList;

begin
  For I:=0 to aList.Count-1 do
    begin
    DL:=TIDLDefinitionList(alist[i]);
    if Not (DL is TIDLPartialDefinitionList) then
      begin
      CD:=TIDLArgumentDefinition.Create(Nil,aName);
      CD.ArgumentType:=TIDLTypeDefDefinition.Create(CD,'');
      CD.ArgumentType.TypeName:=aTypeName;
      DL.Add(CD);
      AllocatePasName(cd,'');
      end;
    end;
end;

procedure TTypescriptToPas.AddArgumentToOverloads(aList: TFPObjectlist; adef: TIDLArgumentDefinition);

Var
  I : Integer;
  CD : TIDLDefinition;
  DL : TIDLDefinitionList;

begin
  For I:=0 to aList.Count-1 do
    begin
    DL:=TIDLDefinitionList(alist[i]);
    if Not (DL is TIDLPartialDefinitionList) then
      begin
      CD:=aDef.Clone(Nil);
      DL.Add(CD);
      if aDef.Data<>Nil then
        CD.Data:=CreatePasName(TPasData(aDef.Data).PasName)
      else
        AllocatePasName(cd,'');
      end;
    end;
end;

procedure TTypescriptToPas.AddUnionOverloads(aList: TFPObjectlist; AName : String; UT : TIDLUnionTypeDefDefinition);

Var
  L,L2 : TFPObjectList;
  I,J : Integer;
  D : TIDLDefinitionList;
  Dups : TStringList;

begin
  L2:=Nil;
  Dups:=TStringList.Create;
  Dups.Sorted:=True;
  Dups.Duplicates:=dupIgnore;
  L:=TFPObjectList.Create(False);
  try
    L2:=TFPObjectList.Create(False);
    // Collect non partial argument lists
    for I:=0 to AList.Count-1 do
      begin
      D:=TIDLDefinitionList(alist[i]);
      if Not (D is TIDLPartialDefinitionList) then
        L.Add(D);
      end;
    // Collect unique pascal types. Note that this can reduce the list to 1 element...
    For I:=0 to UT.Union.Count-1 do
      Dups.AddObject(GetTypeName(UT.Union[I] as TIDLTypeDefDefinition),UT.Union[I]);
    // First, clone list and add argument to cloned lists
    For I:=1 to Dups.Count-1 do
      begin
      // Clone list
      CloneNonPartialArgumentList(L,L2,False);
      // Add argument to cloned list
      AddArgumentToOverloads(L2,aName,Dups[i]);
      // Add overloads to original list
      For J:=0 to L2.Count-1 do
        aList.Add(L2[J]);
      L2.Clear;
      end;
    // Add first Union to original list
    AddArgumentToOverloads(L,aName,Dups[0]);
  finally
    Dups.Free;
    L2.Free;
    L.Free;
  end;
end;

function TTypescriptToPas.CheckUnionTypeDefinition(D: TIDLDefinition
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

procedure TTypescriptToPas.AddOverloads(aList: TFPObjectlist;
  adef: TIDLFunctionDefinition; aIdx: Integer);

Var
  Arg : TIDLArgumentDefinition;
  D : TIDLDefinition;
  UT : TIDLUnionTypeDefDefinition;

begin
 if aIdx>=ADef.Arguments.Count then
    exit;
  Arg:=ADef.Argument[aIdx];
  if Arg.IsOptional then
    CloneNonPartialArgumentList(aList);
  // Add current to list.
  D:=Arg.ArgumentType;
  UT:=Nil;
  if coExpandUnionTypeArgs in Options then
    UT:=CheckUnionTypeDefinition(D);
  if UT=Nil then
    AddArgumentToOverloads(aList,Arg)
  else
    AddUnionOverLoads(aList,Arg.Name,UT);
  AddOverloads(aList,aDef,aIdx+1);
end;

function TTypescriptToPas.GetOverloads(aDef: TIDLFunctionDefinition): TFPObjectlist;

begin
  Result:=TFPObjectList.Create;
  try
    Result.Add(TIDLDefinitionList.Create(Nil,True));
    AddOverloads(Result,adef,0);
  except
    Result.Free;
    Raise;
  end;
end;

function TTypescriptToPas.WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition): Boolean;

Var
  FN,RT,Args : String;

begin
  Result:=True;
  FN:=GetName(aDef);
  RT:=GetTypeName(aDef.ReturnType,False);
  if (RT='void') then
    RT:='';
  Args:=GetArguments(aDef.Arguments,False);
  if (RT='') then
    AddLn('%s = Procedure %s;',[FN,Args])
  else
    AddLn('%s = function %s: %s;',[FN,Args,RT])
end;

function TTypescriptToPas.WriteFunctionDefinition(aDef: TIDLFunctionDefinition): Boolean;

Var
  FN,RT,Suff,Args : String;
  Overloads : TFPObjectList;
  I : Integer;

begin
  Result:=True;
  if not (foConstructor in aDef.Options) then
    begin
    FN:=GetName(aDef);
    if FN<>aDef.Name then
      Suff:=Format('; external name ''%s''',[aDef.Name]);
    RT:=GetTypeName(aDef.ReturnType,False);
    if (RT='void') then
      RT:='';
    end
  else
    FN:='New';
  Overloads:=GetOverloads(ADef);
  try
    for I:=0 to aDef.Arguments.Count-1 do
      if aDef.Argument[i].HasEllipsis then
        Suff:='; varargs';
    if Overloads.Count>1 then
      Suff:=Suff+'; overload';
    For I:=0 to Overloads.Count-1 do
      begin
      Args:=GetArguments(TIDLDefinitionList(Overloads[i]),False);
      if (RT='') then
        begin
        if not (foConstructor in aDef.Options) then
          AddLn('Procedure %s%s%s;',[FN,Args,Suff])
        else
          AddLn('constructor %s%s%s;',[FN,Args,Suff]);
        end
      else
        AddLn('function %s%s: %s%s;',[FN,Args,RT,Suff])
      end;
  finally
    Overloads.Free;
  end;
end;

function TTypescriptToPas.WriteCallBackDefs(aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;
  FD : TIDLFunctionDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLFunctionDefinition then
      if (foCallBack in FD.Options) then
         if WriteFunctionTypeDefinition(FD) then
           Inc(Result);
end;

function TTypescriptToPas.WriteDictionaryDefs(aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;
  DD : TIDLDictionaryDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLDictionaryDefinition then
      if not TIDLDictionaryDefinition(D).IsPartial then
        if WriteDictionaryDef(DD) then
          Inc(Result);
end;

function TTypescriptToPas.WriteInterfaceDefs(aList: TIDLDefinitionList): Integer;

Var
  D : TIDLDefinition;
  ID : TIDLInterfaceDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLInterfaceDefinition then
      if not TIDLInterfaceDefinition(D).IsPartial then
        if WriteInterfaceDef(ID) then
          Inc(Result);
end;
*)

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
  FPasNameList:=TFPObjectList.Create(True);
  FAutoTypes:=TStringList.Create;
  FIncludeInterfaceCode:=TStringList.Create;
  FIncludeImplementationCode:=TStringList.Create;
end;


destructor TTypescriptToPas.Destroy;
begin
  FreeAndNil(FElements);
  FreeAndNil(FIncludeInterfaceCode);
  FreeAndNil(FIncludeImplementationCode);
  FreeAndNil(FAutoTypes);
  FreeAndNil(FTypeAliases);
  FreeAndNil(FPasNameList);
  inherited Destroy;
end;


procedure TTypescriptToPas.WriteVariable(aVar : TJSVarDeclaration);

Var
  Src,aPasName,aTypeName,aExportName : String;

begin
  aPasName:=GetName(aVar);
  aExportName:=aVar.Name;
  aTypeName:=GetTypeName(aVar.Typed,False);
  Src:=aPasName + ' : '+aTypeName+';';
  if aExportName<>aPasName then
    Src:=Src+' external name '''+aExportName+''';';
  AddLn(Src);
end;

procedure TTypescriptToPas.WriteVariables(Vars : TJSElementNodes);

Var
  I : Integer;

begin
  For I:=0 to Vars.Count-1 do
    WriteVariable(Vars.Nodes[i].Node as TJSVarDeclaration);
end;

procedure TTypescriptToPas.WritePascal;

Var
  SourceElements : TJSSourceElements;

begin
  SourceElements:=FElements.A as TJSSourceElements;
  if Not IsRaw then
    begin
    CreateUnitClause;
    CreateHeader;
    if coaddOptionsToheader in Options then
      AddOptionsToHeader;
    end;
  if SourceElements.Types.Count>0 then
    begin
    EnsureSection(csType);
    Indent;
    WriteTypeDefs(SourceElements.Types);
    {
    WriteForwardClassDefs(Context.Definitions);
    WriteEnumDefs(Context.Definitions);
    WriteCallbackDefs(Context.Definitions);
    WriteDictionaryDefs(Context.Definitions);
    WriteInterfaceDefs(Context.Definitions);
    }
    Undent;
    end;
  if SourceElements.Vars.Count>0 then
    begin
    EnsureSection(csVar);
    WriteVariables(SourceElements.Vars);
    end;
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

function TTypescriptToPas.BaseUnits: String;

begin
  Result:='SysUtils, JS'
end;

function TTypescriptToPas.CreatePasName(const aOriginal: jsBase.TJSString; const aName: String): TPasData;

begin
  Result:=TPasData.Create(aOriginal,EscapeKeyWord(aName));
  FPasNameList.Add(Result);
end;

function TTypescriptToPas.AllocatePasName(D: TJSElement; ParentName: String): TPasData;

Var
  Org : TJSString;
  CN : String;
  CD : TJSClassDeclaration absolute D;
  ID : TJSInterfaceDeclaration absolute D;
  VD : TJSVarDeclaration absolute D;
  TD : TJSTypeDeclaration absolute D;

begin
  Result:=Nil;
  if D Is TJSClassDeclaration then
    begin
    Org:=CD.Name;
    CN:=ClassPrefix+Org+ClassSuffix;
    Result:=CreatePasname(Org,CN);
    AllocatePasNames(CD.members,CD.Name);
    end
  else if D Is TJSInterfaceDeclaration then
    begin
    Org:=ID.Name;
    CN:=ClassPrefix+Org+ClassSuffix;
    Result:=CreatePasname(Org,CN);
    AllocatePasNames(ID.Values,EscapeKeyWord(UTF8Encode(ID.Name)));
    end
  else if D Is TJSVarDeclaration then
    begin
    Org:=VD.Name;
    Result:=CreatePasName(Org, EscapeKeyWord(UTF8Encode(Org)));
    end
  else if D Is TJSTypeDeclaration then
    begin
    Org:=TD.Name;
    Result:=CreatePasName(Org, EscapeKeyWord('T'+UTF8Encode(Org)));
    end
  else
    Raise ETSToPas.CreateFmt('Unsupported type to get name from: "%s"',[D.ClassName]);
  D.Data:=Result;
  if Verbose and (Result<>Nil) and (Result.PasName<>UTF8Encode(Org)) then
    begin
    if (ParentName<>'') then
      ParentName:=ParentName+'.';
    DoLog('Renamed %s to %s',[ParentName+UTF8Encode(Org),TPasData(D.Data).PasName]);
    end;
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

procedure TTypescriptToPas.AllocatePasNames(aList : TJSElementNodes; ParentName: String = '');

Var
  I : Integer;

begin
  For I:=0 to aList.Count-1 do
    AllocatePasName(aList.Nodes[i].Node,ParentName);
end;

procedure TTypescriptToPas.AllocatePasNames(aList : TJSSourceElements; ParentName: String = '');

Var
  I : Integer;

begin
  For I:=0 to aList.Types.Count-1 do
    AllocatePasNames(aList.Types,ParentName);
  For I:=0 to aList.Vars.Count-1 do
    AllocatePasNames(aList.Vars,ParentName);
end;


procedure TTypescriptToPas.EnsureUniqueNames(ML: TJSSourceElements);
begin

end;

procedure TTypescriptToPas.ProcessDefinitions;

begin
  AllocatePasNames((FElements.A as TJSSourceElements));
end;

procedure TTypescriptToPas.Execute;

begin
  FContext:=CreateContext;
  try
    if Assigned(TypeAliases) then
      FContext.AddAliases(TypeAliases);
    Parse;
    if Verbose then
      DoLog('Parsed %d definitions.',[]);
    ProcessDefinitions;
    WritePascal;
  finally
    FreeAndNil(FContext);
  end;
end;

Function TTypescriptToPas.GetArrayTypeAsString(aTypeDef : TJSArrayTypeDef; asPascal,asSubType : Boolean) : String;

begin
  Result:=GetTypeAsString(aTypeDef,asPascal,True);
  if coGenericArrays in Options then
    Result:='TArray<'+Result+'>'
  else
    Result:='array of '+Result;
  if AsSubType then
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
end;

Function TTypescriptToPas.GetUnionTypeAsString(aTypeDef : TJSUnionTypeDef; asPascal,asSubType : Boolean) : String;

Var
  I : Integer;

begin
  Result:='';
  For I:=0 to aTypeDef.TypeCount-1 do
    begin
    if Result<>'' then
      Result:=Result+'&';
    GetTypeAsString(aTypeDef.Types[I],asPascal,True);
    end;
  if AsSubType then
    Result:='('+Result+')';
end;

Function TTypescriptToPas.GetIntersectionTypeAsString(aTypeDef : TJSIntersectionTypeDef; asPascal,asSubType : Boolean) : String;

Var
  I : Integer;

begin
  Result:='';
  For I:=0 to aTypeDef.TypeCount-1 do
    begin
    if Result<>'' then
      Result:=Result+'|';
    GetTypeAsString(aTypeDef.Types[I],asPascal,True);
    end;
  if AsSubType then
    Result:='('+Result+')';
end;

Procedure TTypescriptToPas.WriteUnionTypeDef(const aPasName : string; const aOrgName : jsBase.TJSString; aTypeParams: TJSElementNodes;aTypeDef : TJSUnionTypeDef);

var
  TN, gen, genparams: String;

begin
  TN:='jsvalue';
  genparams:=GetGenericParams(aTypeParams);
  if (genparams<>'') then
    gen:='generic ';
  AddLn('%s%s%s = %s; // %s',[gen,aPasName,genparams,TN,GetTypeAsString(aTypeDef,False,False)]);
end;


Procedure TTypescriptToPas.WriteIntersectionTypeDef(const aPasName : string; const aOrgName : jsBase.TJSString; aTypeParams: TJSElementNodes;aTypeDef : TJSIntersectionTypeDef);

var
  TN, gen, genparams: String;

begin
  TN:='jsvalue';
  genparams:=GetGenericParams(aTypeParams);
  if (genparams<>'') then
    gen:='generic ';
  AddLn('%s%s%s = %s; // Intersection type: %s',[gen,aPasName,genparams,TN,GetTypeAsString(aTypeDef,False,false)]);
end;

Procedure TTypescriptToPas.WriteArrayTypeDef(const aPasName : string; const aOrgName : jsBase.TJSString; aTypeParams: TJSElementNodes;aTypeDef : TJSArrayTypeDef);

var
  TN, arr,gen, genparams: String;

begin
  TN:='jsvalue';
  genparams:=GetGenericParams(aTypeParams);
  if (genparams<>'') then
    gen:='generic ';
  arr:=GetArrayTypeAsString(aTypeDef,True,False);
  AddLn('%s%s%s = %s;',[gen,aPasName,genparams,arr]);
end;


Procedure TTypescriptToPas.WriteTypeDef(const aPasName : string; const aOrgName : jsBase.TJSString; aTypeParams: TJSElementNodes; aTypeDef : TJSTypeDef);

begin
  If aTypeDef is TJSTypeReference then
    WriteAliasTypeDef(aPasName,aOrgName,aTypeParams,TJSTypeReference(aTypeDef))
  else if aTypeDef is TJSUnionTypeDef then
    WriteUnionTypeDef(aPasName,aOrgName,aTypeParams,TJSUnionTypeDef(aTypeDef))
  else if aTypeDef is TJSIntersectionTypeDef then
    WriteIntersectionTypeDef(aPasName,aOrgName,aTypeParams,TJSIntersectionTypeDef(aTypeDef))
  else if aTypeDef is TJSArrayTypeDef then
    WriteArrayTypeDef(aPasName,aOrgName,aTypeParams,TJSArrayTypeDef(aTypeDef))
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
    begin
    N:=Types[0].Node;
    if N is TJSTypeDeclaration then
      begin
      aName:=GetName(Decl);
      WriteTypeDef(aName, Decl.Name, Decl.TypeParams, Decl.TypeDef);
      end;
    end;
end;


end.

