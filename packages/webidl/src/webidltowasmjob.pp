{
    This file is part of the Free Component Library

    WEBIDL to pascal code converter
    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit webidltowasmjob;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, WebIdl.Defs, WebIdl.ToPascal, WebIdl.Scanner, WebIdl.Parser, System.Contnrs;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, webidldefs, webidltopas, webidlscanner, webidlparser, Contnrs;
{$ENDIF FPC_DOTTEDUNITS}

{
  Todo:
  - Allocate Aliased types (TIDLUserTypeDefinition) and simple types (TIDLSimpleTypeDefinition) as TIDLTypeDefinition descendants.
    (so no more special cases are needed)
  - Allocate Interface names so no more pasintfname etc. is needed
}


type
  TJOB_JSValueKind = (
    jjvkUndefined,
    jjvkBoolean,
    jjvkDouble,
    jjvkString,
    jjvkObject,
    jivkMethod,
    jjvkDictionary,
    jjvkArray
    );
  TJOB_JSValueKinds = set of TJOB_JSValueKind;

const
  JOB_JSValueKindNames: array[TJOB_JSValueKind] of TIDLString = (
    'Undefined',
    'Boolean',
    'Double',
    'TIDLString',
    'Object',
    'Method',
    'Dictionary',
    'Array'
    );
  JOB_JSValueTypeNames: array[TJOB_JSValueKind] of TIDLString = (
    'TJOB_JSValue',
    'TJOB_Boolean',
    'TJOB_Double',
    'TJOB_String',
    'TJOB_Object',
    'TJOB_Method',
    'TJOB_Dictionary',
    'TJOB_Array'
    );

type
  TMethodCallInfo = record
    FuncName,
    ReturnTypeName,
    ResolvedReturnTypeName,
    InvokeName,
    InvokeClassName : TIDLString;
    ReturnDef : TIDLDefinition;
    ProcKind : String;
  end;

  TAccessorInfo = Record
    PropType : TIDLDefinition;
    NativeType: TPascalNativeType;
    NativeTypeName,
    ResolvedTypeName,
    FuncName: TIDLString;
  end;

  TPasDataWasmJob = class(TPasData)
    PropertyGetterName : String;
    PropertySetterName : String;
  end;

  { TWebIDLToPasWasmJob }

  TWebIDLToPasWasmJob = class(TBaseWebIDLToPas)
  private
    FPasInterfacePrefix: TIDLString;
    FPasInterfaceSuffix: TIDLString;
    FGeneratingInterface : Boolean;
    procedure AllocatePropertyGetterSetter(aParent: TIDLStructuredDefinition; aAttr: TIDLPropertyDefinition);
    procedure AllocatePropertyGetterSetters;
    function GetAccessorNames(Attr: TIDLPropertyDefinition; out aGetter, aSetter: TIDLString): Boolean;
    function GetArgName(d: TIDLDefinition): string;
    function GetFunctionSuffix(aDef: TIDLFunctionDefinition; Overloads: TFPObjectList): String;
    function ExtractAliasName(aTypeName: string): String;
    function ExtractAliasInvokeClass(aName: String): string;
    function ExtractAliasInvokeFunction(aName: String): string;
    function GetInvokeClassName(aMethodInfo : TMethodCallInfo; aDef: TIDLFunctionDefinition=nil): TIDLString;
    function GetInvokeClassName(aResultDef: TIDLDefinition; aName: TIDLString; aDef: TIDLFunctionDefinition=nil): TIDLString;
    function GetInvokeClassNameFromTypeAlias(aName: TIDLString; aDef: TIDLDefinition): TIDLString;
    function GetInvokeNameFromAliasName(const aTypeName: TIDLString; aType: TIDLDefinition): string;
    function GetInvokeNameFromNativeType(aNativeType: TPascalNativeType): String;
    function GetInvokeNameFromTypeName(const aTypeName: TIDLString; aType: TIDLDefinition):  String;
    Procedure GetInvokeNameFromTypeName(var aInfo: TMethodCallInfo);
    function GetKnownArgumentGetter(aDef: TIDLTypeDefinition; ArgTypeName, ArgResolvedTypename: String): string;
    function GetKnownResultAllocator(aDef: TIDLTypeDefinition; ArgTypeName, ArgResolvedTypename: String): string;
    function GetNativeTypeHelperAllocatorName(aNativeType: TPascalNativeType): string;
    function GetNativeTypeHelperGetterName(aNativeType: TPascalNativeType): string;
    function OnlyConstants(D: TIDLStructuredDefinition): Boolean;

  Protected
    function BaseUnits: String; override;
    function DottedBaseUnits: String; override;
    function IsStub : Boolean; virtual;
    // Auxiliary routines
    function DefaultForNativeType(aNativeType: TPascalNativeType; aReturnTypeName: String): String;
    function GetAliasPascalType(D: TIDLDefinition; out PascalTypeName : string): TPascalNativeType; override;
    function GetPasClassName(const aName: String): String; overload; override; // convert to PasInterfacePrefix+X+FPasInterfaceSuffix
    function IntfToPasClassName(const aName: TIDLString): TIDLString; virtual;
    function ComputeGUID(const Prefix: TIDLString; aList: TIDLDefinitionList): TIDLString; virtual;
    procedure GetOptions(L: TStrings; Full: boolean); override;
    function GetPascalTypeName(const aTypeName: String; ForTypeDef: Boolean=False): String; override;
    function GetPasIntfName(Intf: TIDLDefinition): TIDLString;
    function GetResolvedType(aDef: TIDLTypeDefDefinition; Out PascalNativeType : TPascalNativeType; out aTypeName, aResolvedTypename: String): TIDLTypeDefinition; overload; override;
{$IF SIZEOF(CHAR)=1}      
    function GetResolvedType(aDef: TIDLTypeDefDefinition; Out PascalNativeType : TPascalNativeType; out aTypeName, aResolvedTypename: TIDLString): TIDLDefinition; overload;
{$ENDIF}      
    function GetInterfaceDefHead(Intf: TIDLInterfaceDefinition): String; override;
    function GetNamespaceDefHead(aNamespace: TIDLNamespaceDefinition): String; override;
    function GetDictionaryDefHead(const CurClassName: String; Dict: TIDLDictionaryDefinition): String; override;
    function GetDictionaryClassHead(const CurClassName: String; Dict: TIDLDictionaryDefinition): String; virtual;
    function GetDictionaryIntfHead(const CurClassName: String; Dict: TIDLDictionaryDefinition): String; virtual;
    function WriteOtherImplicitTypes(Intf: TIDLStructuredDefinition; aMemberList: TIDLDefinitionList): Integer; override;
    // Code generation routines. Return the number of actually written defs.
    function WriteDictionaryPrivateFields(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WriteGetters(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; override;
    function WriteSetters(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; override;
    function WriteProperties(aParent: TIDLDefinition; aList: TIDLDefinitionList): Integer; override;
    function WriteUtilityMethods(Intf: TIDLStructuredDefinition): Integer;  override;
    // Maplike
    function WriteMapLikeProperties(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer; override;
    function WriteMapLikePrivateReadOnlyFields(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer; override;
    function WriteMapLikeGetters(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition): Integer; override;
    // Definitions. Return true if a definition was written.
    function WriteEnumDef(aDef: TIDLEnumDefinition): Boolean; override;
    function WriteDictionaryDef(aDict: TIDLDictionaryDefinition): Boolean; override;
    function WriteDictionaryField(aDict: TIDLDictionaryDefinition;  aField: TIDLDictionaryMemberDefinition): Boolean; override;
    function WriteForwardClassDef(D: TIDLStructuredDefinition): Boolean; override;
    function WriteFunctionDefinition(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition): Boolean; override;
    function WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition; aName : string = ''): Boolean; override;
    function WritePrivateGetter(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition): boolean; virtual;
    function WritePrivateSetter(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition): boolean; virtual;
    function WriteProperty(aParent: TIDLDefinition; aProp: TIDLPropertyDefinition): boolean; virtual;
    function WriteRecordDef(aDef: TIDLRecordDefinition): Boolean; override;
    procedure WriteSequenceDef(aDef: TIDLSequenceTypeDefDefinition); override;
    // Extra interface/Implementation code.
    function GetPrivateGetterInfo(aProp: TIDLPropertyDefinition; out aAccessInfo : TAccessorInfo): Boolean;
    function GetPrivateSetterInfo(aProp: TIDLPropertyDefinition; out aAccessInfo : TAccessorInfo): Boolean;
    function GetReadPropertyCall(aInfo : TAccessorInfo; aMemberName: String): string;
    function GetWritePropertyCall(aInfo : TAccessorInfo; aMemberName: String): string;
    function GetFunctionSignature(aDef: TIDLFunctionDefinition; aInfo: TMethodCallInfo; aSuffix: TIDLString; ArgDefList: TIDLDefinitionList; out ProcKind: TIDLString): String;
    function GetMethodInfo(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition; out MethodInfo: TMethodCallInfo): Boolean;
    function AllocateAttributePasName(aParent : TIDLStructuredDefinition; D: TIDLAttributeDefinition; ParentName: String; Recurse: Boolean): TPasData; override;
    Procedure ProcessDefinitions; override;
    // Implementation writing
    procedure WriteImplementation; override;
    // Implementation, per type
    procedure WriteDefinitionImplementation(D: TIDLDefinition); override;
    procedure WriteDictionaryImplemention(aDef: TIDLDictionaryDefinition); virtual;
    procedure WriteEnumImplementation(aDef: TIDLEnumDefinition); virtual;
    procedure WriteInterfaceImplemention(aDef: TIDLInterfaceDefinition); virtual;
    procedure WriteNamespaceImplemention(aDef: TIDLNamespaceDefinition); virtual;
    procedure WriteTypeDefsAndCallbackImplementations(aList: TIDLDefinitionList); override;
    Procedure WriteFunctionTypeCallBackImplementation(aDef: TIDLCallBackDefinition);
    // Implementation, per member
    procedure WriteMethodImplementations(aDef: TIDLStructuredDefinition; ML: TIDLDefinitionList); virtual;
    Procedure WriteFunctionImplementation(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition); virtual;
    Procedure WriteFunctionInvokeCodeStub(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition; aInfo : TMethodCallInfo); virtual;
    procedure WritePrivateGetterImplementations(aDef: TIDLStructuredDefinition; ML: TIDLDefinitionList); virtual;
    procedure WritePrivateSetterImplementations(aDef: TIDLStructuredDefinition; ML: TIDLDefinitionList); virtual;
    procedure WriteUtilityMethodImplementations(aDef: TIDLStructuredDefinition; ML: TIDLDefinitionList);virtual;
    Procedure WritePrivateGetterImplementation(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition); virtual;
    Procedure WritePrivateSetterImplementation(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition);virtual;
    procedure WriteDictionaryConstructor(aDict: TIDLDictionaryDefinition); virtual;
    // MapLike
    procedure WriteMapLikePrivateSetterImplementation(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition); virtual;
    procedure WriteMapLikePrivateGetterImplementation(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition); virtual;
    procedure WriteMapLikeFunctionImplementations(aDef: TIDLStructuredDefinition; MD: TIDLMapLikeDefinition);
    procedure WriteMapLikeEntriesFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);virtual;
    procedure WriteMapLikeGetFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);virtual;
    procedure WriteMapLikeSetFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);virtual;
    procedure WriteMapLikeClearFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);virtual;
    procedure WriteMapLikeHasFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);virtual;
    procedure WriteMapLikeDeleteFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);virtual;
    procedure WriteMapLikeKeysFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);virtual;
    procedure WriteMapLikeValuesFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);virtual;

    procedure WriteNamespaceVars; override;
    procedure WriteGlobalVar(aDef : String); override;
  Public
    constructor Create(ThOwner: TComponent); override;
    function SplitGlobalVar(Line: TIDLString; out PasVarName, JSClassName, JOBRegisterName: TIDLString): boolean; virtual;
  Published
    Property BaseOptions;
    Property ClassPrefix;
    Property ClassSuffix;
    Property PasInterfacePrefix: TIDLString read FPasInterfacePrefix write FPasInterfacePrefix;
    Property PasInterfaceSuffix: TIDLString read FPasInterfaceSuffix write FPasInterfaceSuffix;
    Property DictionaryClassParent;
    Property FieldPrefix;
    Property GetterPrefix;
    Property SetterPrefix;
    Property IncludeImplementationCode;
    Property IncludeInterfaceCode;
    Property InputFileName;
    Property OutputFileName;
    Property TypeAliases;
    Property Verbose;
    Property WebIDLVersion;
  end;

implementation

{ TWebIDLToPasWasmJob }


function TWebIDLToPasWasmJob.BaseUnits: String;
begin
  Result:='SysUtils, Job.JS';
end;

function TWebIDLToPasWasmJob.DottedBaseUnits: String;
begin
  Result:='System.SysUtils, Wasm.Job.Js';
end;

function TWebIDLToPasWasmJob.IsStub: Boolean;
begin
  Result:=False;
end;

function TWebIDLToPasWasmJob.GetAliasPascalType(D: TIDLDefinition; out PascalTypeName: string): TPascalNativeType;

var
  S : String;

begin
  Result:=inherited GetAliasPascalType(D,PascalTypeName);
  if Result<>ntUnknown then
    exit;
  S:=LowerCase(PascalTypeName);
  if pos('array',S)>0 then
    Result:=ntArray
  else if pos(FPasInterfaceSuffix,S)=1 then
    Result:=ntObject
  else if pos('string',S)>0 then
    Result:=ntUnicodeString;
end;




function TWebIDLToPasWasmJob.GetPasClassName(const aName: String): String;
begin
  Result:=aName;
  if (LeftStr(Result,length(ClassPrefix))=ClassPrefix)
  and (RightStr(Result,length(ClassSuffix))=ClassSuffix)
  then
    Result:=copy(Result,length(ClassPrefix)+1,length(Result)-length(ClassPrefix)-length(ClassSuffix));
  if Result='' then
    raise EConvertError.Create('[20220725184518]');
  if LeftStr(Result,length(PasInterfacePrefix)) <> PasInterfacePrefix then
    Result:=PasInterfacePrefix+Result;
  if RightStr(Result,length(PasInterfaceSuffix)) <> PasInterfaceSuffix then
    Result:=Result+PasInterfaceSuffix;
end;

function TWebIDLToPasWasmJob.IntfToPasClassName(const aName: TIDLString): TIDLString;
begin
  Result:=aName;
  if (LeftStr(Result,length(PasInterfacePrefix))=PasInterfacePrefix)
  and (RightStr(Result,length(PasInterfaceSuffix))=PasInterfaceSuffix)
  then
    Result:=copy(Result,length(PasInterfacePrefix)+1,length(Result)-length(PasInterfacePrefix)-length(PasInterfaceSuffix));
  if Result='' then
    raise EConvertError.Create('[20220725184440] cannot convert interface name '+aName+' to class name');
  if LeftStr(Result,Length(ClassPrefix))<>ClassPrefix then
    Result:=ClassPrefix+Result+ClassSuffix;
end;

function TWebIDLToPasWasmJob.ComputeGUID(const Prefix: TIDLString;
  aList: TIDLDefinitionList): TIDLString;
var
  List: TStringList;
  D: TIDLDefinition;
  Attr: TIDLAttributeDefinition;
  i, BytePos, BitPos, v: Integer;
  Bytes: array[0..15] of byte;
  GUIDSrc, aTypeName: TIDLString;
begin
  List:=TStringList.Create;
  for D in aList do
    begin
    GUIDSrc:=D.Name;
    if GUIDSrc='' then continue;
    if D is TIDLAttributeDefinition then
      begin
      Attr:=TIDLAttributeDefinition(D);
      if Attr.AttributeType<>nil then
        aTypeName:=GetJSTypeName(Attr.AttributeType);
        GUIDSrc:=GUIDSrc+':'+aTypeName;
      end;
    List.Add(GUIDSrc);
    end;
  List.Sort;
  GUIDSrc:=Prefix+',';
  for i:=0 to List.Count-1 do
    GUIDSrc:=GUIDSrc+','+List[i];
  List.Free;

  BytePos:=0;
  BitPos:=0;
  {$IFDEF fpc}
  FillByte({%H-}Bytes[0],16,0);
  {$ENDIF}
  for i:=1 to length(GUIDSrc) do
    begin
    // read 16-bit
    v:=(Bytes[BytePos] shl 8)+Bytes[(BytePos+1) and 15];
    // change some bits
    v:=v+integer((ord(GUIDSrc[i]) shl (11-BitPos)));
    // write 16 bit
    Bytes[BytePos]:=(v shr 8) and $ff;
    Bytes[(BytePos+1) and 15]:=v and $ff;
    inc(BitPos,5);
    if BitPos>7 then
      begin
      dec(BitPos,8);
      BytePos:=(BytePos+1) and 15;
      end;
    end;
  // set version 3
  Bytes[6]:=(Bytes[6] and $f)+(3 shl 4);
  // set variant 2
  Bytes[8]:=(Bytes[8] and $3f)+(2 shl 6);

  Result:='{';
  for i:=0 to 3 do Result:=Result+HexStr(Bytes[i],2);
  Result:=Result+'-';
  for i:=4 to 5 do Result:=Result+HexStr(Bytes[i],2);
  Result:=Result+'-';
  for i:=6 to 7 do Result:=Result+HexStr(Bytes[i],2);
  Result:=Result+'-';
  for i:=8 to 9 do Result:=Result+HexStr(Bytes[i],2);
  Result:=Result+'-';
  for i:=10 to 15 do Result:=Result+HexStr(Bytes[i],2);
  Result:=Result+'}';
end;

procedure TWebIDLToPasWasmJob.GetOptions(L: TStrings; Full: boolean);
begin
  inherited GetOptions(L, Full);
end;

function TWebIDLToPasWasmJob.GetPascalTypeName(const aTypeName: String; ForTypeDef: Boolean): String;
begin
  Case aTypeName of
    'union',
    'any': Result:='Variant';
    'void',
    'undefined': Result:=aTypeName;
  else
    //writeln('TWebIDLToPasWasmJob.GetJSTypeName ',aTypeName,' ',Def<>nil);
    Result:=inherited GetPascalTypeName(aTypeName,ForTypeDef);
    if (Result=aTypeName)
    and (LeftStr(Result,length(PasInterfacePrefix))<>PasInterfacePrefix)
    and (RightStr(Result,length(PasInterfaceSuffix))<>PasInterfaceSuffix)
    then
      begin
      if Result='' then
        raise EConvertError.Create('[20220725184536]');
      Result:=PasInterfacePrefix+Result+PasInterfaceSuffix;
      end;
  end;
end;

function TWebIDLToPasWasmJob.GetPasIntfName(Intf: TIDLDefinition): TIDLString;
begin
  Result:=GetPasName(Intf);
  if Result='' then
    raise EConvertError.Create('[20220725184653] missing name at '+GetDefPos(Intf));
  Result:=GetPasClassName(Result);
end;

{$IF SIZEOF(CHAR)=1}
function TWebIDLToPasWasmJob.GetResolvedType(aDef: TIDLTypeDefDefinition; out PascalNativeType: TPascalNativeType; out aTypeName,
  aResolvedTypename: TIDLString): TIDLDefinition;

Var
  TN,RTN : String;
  
begin
  Result:=GetResolvedType(aDef,PascalNativeType,TN,RTN);
  aTypeName:=TN;
  aResolvedTypeName:=RTN;
end;
{$ENDIF}

function TWebIDLToPasWasmJob.GetResolvedType(aDef: TIDLTypeDefDefinition; out PascalNativeType: TPascalNativeType; out aTypeName,
  aResolvedTypename: String): TIDLTypeDefinition;
begin
  Result:=inherited GetResolvedType(aDef, PascalNativeType, aTypeName, aResolvedTypename);
  if Result is TIDLInterfaceDefinition then
    aTypeName:=GetPasIntfName(Result)
  else if Result is TIDLDictionaryDefinition then
    aTypeName:=GetPasIntfName(Result)
  else if Result is TIDLPromiseTypeDefDefinition then
    aTypeName:=PasInterfacePrefix+'Promise'+PasInterfaceSuffix;
end;

function TWebIDLToPasWasmJob.GetInterfaceDefHead(Intf: TIDLInterfaceDefinition
  ): String;
var
  aParentName, aPasIntfName: TIDLString;
begin
  Result:='class(';
  if Assigned(Intf.ParentInterface) then
    aParentName:=GetPasName(Intf.ParentInterface)
  else
    aParentName:=GetPascalTypeName(Intf.ParentName);
  if aParentName='' then
    aParentName:=ClassPrefix+'Object'+ClassSuffix;
  if aParentName<>'' then
    Result:=Result+aParentName;
  aPasIntfName:=GetPasIntfName(Intf);
  Result:=Result+','+aPasIntfName+')';
end;

function TWebIDLToPasWasmJob.GetNamespaceDefHead(aNamespace: TIDLNamespaceDefinition): String;

var
  aPasIntfName: TIDLString;
begin
  Result:='class('+ClassPrefix+'Object'+ClassSuffix;
  aPasIntfName:=GetPasIntfName(aNameSpace);
  Result:=Result+','+aPasIntfName+')';
end;

function TWebIDLToPasWasmJob.GetDictionaryIntfHead(const CurClassName: String; Dict: TIDLDictionaryDefinition): String;

var
  CurParent: String;

begin
  if CurClassName='' then ; // Silence compiler warning
  CurParent:='';
  if Assigned(Dict.ParentDictionary) then
    CurParent:= GetPasIntfName(Dict.ParentDictionary);
  if CurParent='' then
    CurParent:='IJSObject';
  Result:='Interface ('+CurParent+')';
  Result:=GetPasIntfName(Dict)+' = '+Result;
end;

function TWebIDLToPasWasmJob.GetDictionaryClassHead(const CurClassName: String; Dict: TIDLDictionaryDefinition): String;

var
  CurParent: String;

begin
  CurParent:='';
  if Assigned(Dict.ParentDictionary) then
    CurParent:=GetPasName(Dict.ParentDictionary);
  if CurParent='' then
    CurParent:='TJSObject';
  Result:='class('+CurParent+','+GetPasIntfName(Dict)+')';
  Result:=CurClassName+' = '+Result;
end;

function TWebIDLToPasWasmJob.GetDictionaryDefHead(const CurClassName: String; Dict: TIDLDictionaryDefinition): String;
begin
  Result:='';
  if Dict<>nil then
    Result:=CurClassName+'Rec = record'
end;

function TWebIDLToPasWasmJob.WriteOtherImplicitTypes(Intf: TIDLStructuredDefinition; aMemberList: TIDLDefinitionList): Integer;
var
  iIntf : TIDLInterfaceDefinition absolute Intf;
  dDict : TIDLDictionaryDefinition absolute Intf;
  aPasIntfName, Decl, ParentName: TIDLString;
  StructType : TStructuredDefinitionType;

begin
  Result:=1;
  ParentName:='';

  // Pascal interface and ancestor
  aPasIntfName:=GetPasIntfName(Intf);
  StructType:=Intf.StructuredType;
  FGeneratingInterface:=True;
  try
    Decl:=aPasIntfName+' = interface';
    Case StructType of
    sdInterface:
      if Assigned(iIntf.ParentInterface) then
        ParentName:=GetPasIntfName(iIntf.ParentInterface as TIDLInterfaceDefinition)
      else
        ParentName:=GetPascalTypeName(Intf.ParentName);
    sdDictionary:
       if Assigned(dDict.ParentDictionary) then
        ParentName:=GetPasIntfName(dDict.ParentDictionary as TIDLDictionaryDefinition)
       else
        ParentName:=GetPascalTypeName(dDict.ParentName);
    else
      ParentName:='';
    end;
    if ParentName='' then
      ParentName:=PasInterfacePrefix+'Object'+PasInterfaceSuffix;
    if ParentName<>'' then
      Decl:=Decl+'('+ParentName+')';
    AddLn(Decl);

    Indent;

    // GUID
    AddLn('['''+ComputeGUID(Decl,aMemberList)+''']');

    // private members
    WriteGetters(Intf,aMemberList);
    WriteSetters(Intf,aMemberList);

    // public members
    if StructType<>sdDictionary then
      WriteMethodDefs(Intf,aMemberList);
    WriteProperties(Intf,aMemberList);

    Undent;
    AddLn('end;');
    AddLn('');

  finally
    FGeneratingInterface:=False;
  end;

end;

function TWebIDLToPasWasmJob.WriteDictionaryPrivateFields(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer;
begin
  Result:=0;
  if Aparent=Nil then;
  if Alist=Nil then;
  // Do nothing, used in stub
end;

function TWebIDLToPasWasmJob.WriteGetters(aParent: TIDLStructuredDefinition;
  aList: TIDLDefinitionList): Integer;
var
  D: TIDLDefinition;
begin
  Result:=Inherited WriteGetters(aParent,aList);
  for D in aList do
    if D is TIDLPropertyDefinition then
      if ConvertDef(D) then
        if WritePrivateGetter(aParent,TIDLPropertyDefinition(D)) then
          inc(Result);
end;

function TWebIDLToPasWasmJob.WriteSetters(
  aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer;
var
  D: TIDLDefinition;
begin
  Result:=Inherited WriteSetters(aParent,aList);
  for D in aList do
    if D is TIDLPropertyDefinition then
      if ConvertDef(D) then
        if WritePrivateSetter(aParent,TIDLPropertyDefinition(D)) then
          inc(Result);
end;

function TWebIDLToPasWasmJob.WriteProperties(aParent: TIDLDefinition;
  aList: TIDLDefinitionList): Integer;
var
  D: TIDLDefinition;
begin
  Result:=Inherited WriteProperties(aParent,aList);
  Result:=0;
  for D in aList do
    if D is TIDLPropertyDefinition then
      if ConvertDef(D) then
        if WriteProperty(aParent,TIDLPropertyDefinition(D)) then
          inc(Result);
end;

function TWebIDLToPasWasmJob.WriteUtilityMethods(Intf: TIDLStructuredDefinition
  ): Integer;
var
  CurrClassName,aPasIntfName: TIDLString;
begin
  Result:=0;
  aPasIntfName:=GetPasIntfName(Intf);
  if Intf is TIDLDictionaryDefinition then
    begin
    CurrClassName:=GetPasName(TIDLDictionaryDefinition(Intf));
    AddLn('constructor create(const aDict : '+CurrClassName+'Rec); overload;');
    end;
  AddLn('class function JSClassName: UnicodeString; override;');
  AddLn('class function Cast(const Intf: IJSObject): '+aPasIntfName+';');

end;

function TWebIDLToPasWasmJob.WriteMapLikeProperties(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer;
begin
  if (aParent=Nil) and (aMap=Nil) then ; // Silence compiler warning
  AddLn('property size : LongInt read _Getsize;');
  Result:=1;
end;

function TWebIDLToPasWasmJob.WriteMapLikePrivateReadOnlyFields(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer;
begin
  if (aParent=Nil) and (aMap=Nil) then ; // Silence compiler warning
  Result:=0;
end;

function TWebIDLToPasWasmJob.WriteMapLikeGetters(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition): Integer;

begin
  if (aParent=Nil) and (aMap=Nil) then ; // Silence compiler warning
  Result:=1;
  AddLn('function _Getsize : LongInt;');
end;

function TWebIDLToPasWasmJob.WriteEnumDef(aDef: TIDLEnumDefinition): Boolean;
begin
  Result:=True;
  AddLn(GetPasName(aDef)+' = UnicodeString;');
end;

function TWebIDLToPasWasmJob.WriteDictionaryDef(aDict: TIDLDictionaryDefinition): Boolean;

Var
  CurClassName, Decl: String;
  DefList: TIDLDefinitionList;

begin
  // Write record;
  Result:=inherited WriteDictionaryDef(aDict);
  AddLn('');
  DefList:=GetFullMemberList(aDict);
  WriteOtherImplicitTypes(aDict,DefList);
  CurClassName:=GetPasName(aDict);
  // class and ancestor
  Decl:=GetDictionaryClassHead(CurClassName,aDict);
  AddLn(Decl);
  AddLn('Private');
  Indent;
  WriteDictionaryPrivateFields(aDict,DefList);
  if not (coPrivateMethods in BaseOptions) then
    begin
    Undent;
    AddLn('Protected');
    Indent;
    end;
  WriteGetters(aDict,DefList);
  WriteSetters(aDict,DefList);
  Undent;
  AddLn('Public');
  Indent;
  WriteUtilityMethods(aDict);
  WriteProperties(aDict,DefList);
  Undent;
  AddLn('end;');

end;

function TWebIDLToPasWasmJob.WriteDictionaryField(aDict: TIDLDictionaryDefinition; aField: TIDLDictionaryMemberDefinition): Boolean;
var
  N, TN: TIDLString;
begin
  if (aDict=Nil) then ; // Silence compiler warning
  Result:=True;
  N:=GetPasName(aField);
  TN:=GetPasName(aField.MemberType);
  if SameText(N,TN) then
    N:='_'+N;
  AddLn(N+': '+TN+';');
end;

function TWebIDLToPasWasmJob.WriteForwardClassDef(D: TIDLStructuredDefinition
  ): Boolean;
begin
  if D.IsPartial then
    exit;
  if D is TIDLDictionaryDefinition then
    begin
    AddLn(GetPasIntfName(D)+' = interface;');
    Result:=inherited WriteForwardClassDef(D);
    end
  else
    begin
    if ((D is TIDLInterfaceDefinition) or (D is TIDLNamespaceDefinition)) then
      AddLn(GetPasIntfName(D)+' = interface;');
    Result:=inherited WriteForwardClassDef(D);
    end;
end;

function TWebIDLToPasWasmJob.GetInvokeNameFromAliasName(const aTypeName : TIDLString; aType : TIDLDefinition) : string;
// Heuristic to determine what the base type of an aliased type is.
var
  aLower : String;
begin
  if aType=nil then ; // Silence compiler warning;
  Result:=ExtractAliasInvokeFunction(aTypeName);
  if Result<>'' then
    exit;
  aLower:=LowerCase(aTypeName);
  if Pos('bool',aLower)>0 then
    Result:='InvokeJSBooleanResult'
  else if Pos('array',aLower)>0 then
    Result:='InvokeJSObjectResult'
  else if Pos('string',aLower)>0 then
    Result:='InvokeJSUnicodeStringResult'
  else if Pos(LowerCase(PasInterfacePrefix),aLower)=1 then
    Result:='InvokeJSObjectResult'
  else
    Result:='';
end;

function TWebIDLToPasWasmJob.GetInvokeNameFromNativeType(aNativeType : TPascalNativeType) : String;

begin
  case aNativeType of
    ntBoolean : Result:='InvokeJSBooleanResult';
    ntShortInt,
    ntByte,
    ntSmallInt,
    ntWord,
    ntCardinal,
    ntLongint: Result:='InvokeJSLongIntResult';
    ntInt64,
    ntQWord : Result:='InvokeJSMaxIntResult';
    ntSingle,
    ntDouble : Result:='InvokeJSDoubleResult';
    ntUTF8String : Result:='InvokeJSUTF8StringResult';
    ntUnicodeString : Result:='InvokeJSUnicodeStringResult';
    ntVariant: Result:='InvokeJSVariantResult';
    ntNone: Result:='InvokeJSNoResult';
  else
    Result:='';
  end;
end;

procedure TWebIDLToPasWasmJob.GetInvokeNameFromTypeName(var aInfo: TMethodCallInfo);

begin
  aInfo.InvokeName:=GetInvokeNameFromTypeName(aInfo.ResolvedReturnTypeName,aInfo.ReturnDef);
end;

function TWebIDLToPasWasmJob.GetInvokeNameFromTypeName(const aTypeName : TIDLString; aType : TIDLDefinition):  String;


var
  aPascaltypeName : String;
  NT : TPascalNativeType;

begin
  NT:=GetPasNativeTypeAndName(aType,aPascaltypeName);
  Result:=GetInvokeNameFromNativeType(NT);
  if Result<>'' then
    Exit;
  if (aPascalTypeName='TJOB_JSValue') then
    Result:='InvokeJSValueResult'
  else if (aTypeName='undefined') then
    Result:='InvokeJSNoResult'
  else if (aType is TIDLTypeDefDefinition) then
    begin
    if (TypeAliases.IndexOfName(aTypeName)<>-1) then
      Result:=GetInvokeNameFromAliasName(aTypeName,aType);
    if (Result='') and (TypeAliases.IndexOfName((aType as TIDLTypeDefDefinition).TypeName)<>-1) then
      Result:=GetInvokeNameFromAliasName((aType as TIDLTypeDefDefinition).TypeName,aType);
    if (Result='') and (TypeAliases.IndexOfName(GetPasName(aType))<>-1) then
      Result:=GetInvokeNameFromAliasName(GetPasName(aType),aType)
    else if Result='' then
      Result:='InvokeJSObjectResult';
    if Result='' then
      Raise EConvertError.CreateFmt('Unable to determine invoke name from alias type %s',[aTypeName]);
    end
  else if aType is TIDLEnumDefinition then
    Result:='InvokeJSUnicodeStringResult'
  else
    Result:='InvokeJSObjectResult';

end;

function TWebIDLToPasWasmJob.ExtractAliasInvokeClass(aName :String) : string;

// Alias is encoded as:
// aType=aAlias[,InvokeClass[:InvokeFunctionName]]

var
  P : Integer;

begin
  Result:=TypeAliases.Values[aName];
  P:=Pos(',',Result);
  if P>0 then
    begin
    Result:=Copy(Result,P+1);
    P:=Pos(':',Result);
    if P>0 then
      Result:=Copy(Result,1,P-1);
    end
  else
    // if it is an interface, we can simply assume the class is the same but with IJS -> TJS
    if (LeftStr(Result,length(PasInterfacePrefix))=PasInterfacePrefix) then
      Result:=IntfToPasClassName(Result)
    else
      Result:='';
end;

function TWebIDLToPasWasmJob.ExtractAliasInvokeFunction(aName: String): string;
// Alias is encoded as:
// aType=aAlias[,InvokeClass[:InvokeFunctionName]]

var
  P : Integer;

begin
  Result:=TypeAliases.Values[aName];
  P:=Pos(',',Result);
  if P>0 then
    begin
    Result:=Copy(Result,P+1);
    P:=Pos(':',Result);
    if P>0 then
      Result:=Copy(Result,P+1);
    end
  else
    // if it is an interface, we can simply assume 'InvokeJSObjectResult'
    if (LeftStr(Result,length(PasInterfacePrefix))=PasInterfacePrefix) then
      Result:='InvokeJSObjectResult'
    else
      Result:='';
end;

function TWebIDLToPasWasmJob.GetInvokeClassNameFromTypeAlias(aName : TIDLString; aDef : TIDLDefinition): TIDLString;
// Heuristic to determine what the base type of an aliased type is.
var
  aLower : String;
begin
  if aDef<>Nil then ; // Silence compiler warning
  Result:=ExtractAliasInvokeClass(aName);
  if Result<>'' then
    exit;
  aLower:=LowerCase(aName);
  if Pos('array',aLower)>0 then
    Result:='TJSArray'
  else if Pos(PasInterfacePrefix,aLower)=1 then
    Result:='TJSObject'
  else
    Result:='';
end;


function TWebIDLToPasWasmJob.GetInvokeClassName(aMethodInfo: TMethodCallInfo; aDef: TIDLFunctionDefinition): TIDLString;

begin
  Result:=GetInvokeClassName(aMethodInfo.ReturnDef,aMethodInfo.ResolvedReturnTypeName,aDef);
end;

function TWebIDLToPasWasmJob.GetInvokeClassName(aResultDef: TIDLDefinition; aName: TIDLString; aDef: TIDLFunctionDefinition=nil): TIDLString;

  Procedure UnsupportedReturnType;

  var
    Msg : string;

  begin
    Msg:=GetPasName(aDef);
    Msg:='[20220725172242] not yet supported: function "'+Msg+'" return type: '+aName;
    if assigned(aDef) then
      Msg:=Msg+' at '+GetDefPos(aDef);
    raise EConvertError.Create(Msg);
  end;

var
  aTypeName : String;
  sDef : TIDLDefinition;
begin
  Result:='';
  if aResultDef is TIDLSequenceTypeDefDefinition then
    Result:=ClassPrefix+'Array'+ClassSuffix
  else if aResultDef is TIDLPromiseTypeDefDefinition then
    Result:=ClassPrefix+'Promise'+ClassSuffix
  else if aResultDef is TIDLInterfaceDefinition then
    Result:=GetPasName(aResultDef)
  else if aResultDef is TIDLDictionaryDefinition then
    Result:=GetPasName(aResultDef)
  else if aName=PasInterfacePrefix+'Object'+PasInterfaceSuffix then
    begin
    Result:=ClassPrefix+'Object'+ClassSuffix;
    end
  else if aResultDef is TIDLTypeDefDefinition then
    begin
      aTypeName:=GetJSTypeName(TIDLTypeDefDefinition(aResultDef));
      sDef:=FindGlobalDef(aTypeName);
      if assigned(sDef) then
        Result:=GetPasName(sDef)
      else
        begin
        if TypeAliases.IndexOfName(aTypeName)=-1 then
          UnsupportedReturnType
        else
          Result:=GetInvokeClassNameFromTypeAlias(aTypeName,aResultDef);
        end;
    end
  else
    UnsupportedReturnType
end;


function TWebIDLToPasWasmJob.GetMethodInfo(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition; out MethodInfo : TMethodCallInfo): Boolean;

var
  RNT : TPascalNativeType;

begin
  if (aParent=Nil) then ; // Silence compiler warning
  Result:=True;
  MethodInfo.ReturnDef:=GetResolvedType(aDef.ReturnType,RNT,MethodInfo.ReturnTypeName,MethodInfo.ResolvedReturnTypeName);
  MethodInfo.InvokeName:='';
  MethodInfo.InvokeClassName:='';
  if (foConstructor in aDef.Options) then
    begin
    MethodInfo.FuncName:='New';
    MethodInfo.InvokeName:= 'JOBCreate';
    MethodInfo.ResolvedReturnTypeName:='';
    MethodInfo.ReturnTypeName:='';
    MethodInfo.InvokeClassName:='';
    MethodInfo.ReturnDef:=Nil;
    end
  else
    begin

    MethodInfo.FuncName:=GetPasName(aDef);
    GetInvokeNameFromTypeName(MethodInfo);
    case MethodInfo.InvokeName of
    'InvokeJSNoResult' :
       begin
       MethodInfo.ReturnTypeName:='';
       MethodInfo.ResolvedReturnTypeName:='';
       end;
    'InvokeJSObjectResult':
      MethodInfo.InvokeClassName:=GetInvokeClassName(MethodInfo,aDef);
    else
       ;
    end;
    end;
end;

function TWebIDLToPasWasmJob.AllocateAttributePasName(aParent: TIDLStructuredDefinition; D: TIDLAttributeDefinition;
  ParentName: String; Recurse: Boolean): TPasData;

begin
  Result:=inherited AllocateAttributePasName(aParent, D, ParentName, Recurse);
end;

procedure TWebIDLToPasWasmJob.AllocatePropertyGetterSetter(aParent : TIDLStructuredDefinition; aAttr : TIDLPropertyDefinition);

var
  Full : TIDLDefinitionList;
  aDef : TIDLDefinition;
  aCount : integer;
  DJob : TPasDataWasmJob;
  BaseName : string;
begin
  if not (aAttr.Data is TPasDataWasmJob) then
    Raise EWebIDLError.CreateFmt('No data assigned for attribute %s of %s',[aAttr.Name,aParent.Name]);
  DJob:=TPasDataWasmJob(aAttr.Data);
  Full:=GetParentsMemberList(aParent);
  aCount:=1;
  BaseName:=GetPasName(aAttr);
  For aDef in Full do
    if (aDef is TIDLAttributeDefinition) and ConvertDef(aDef) then
      if (aAttr<>aDef) and (BaseName=GetPasName(aDef)) then
        inc(aCount);
  if aCount>1 then
    BaseName:=BaseName+IntToStr(aCount);
  DJob.PropertyGetterName:=GetterPrefix+BaseName;
  DJob.PropertySetterName:=SetterPrefix+BaseName;
end;

procedure TWebIDLToPasWasmJob.AllocatePropertyGetterSetters;


var
  D,MD : TIDLDefinition;
  SD : TIDLStructuredDefinition absolute D;
  AD : TIDLPropertyDefinition absolute MD;

begin
  DoLog('Allocating property getters and setters');
  For D in Context.Definitions do
    if D is TIDLStructuredDefinition then
      For MD in GetFullMemberList(SD) do
        if MD is TIDLPropertyDefinition then
          AllocatePropertyGetterSetter(SD,AD);
  DoLog('Done allocating property getters and setters');
end;

procedure TWebIDLToPasWasmJob.ProcessDefinitions;
begin
  Inherited ProcessDefinitions;
  AllocatePropertyGetterSetters;
end;


function TWebIDLToPasWasmJob.GetFunctionSignature(aDef: TIDLFunctionDefinition; aInfo : TMethodCallInfo; aSuffix: TIDLString; ArgDefList: TIDLDefinitionList; out ProcKind: TIDLString): String;

var
  Args : String;

begin
  Result:='';
  Args:=GetArguments(ArgDefList,False);
  if (foConstructor in aDef.Options) then
    begin
    ProcKind:='constructor';
    Result:='Create'+Args;
    end
  else if (aInfo.ReturnTypeName='') then
    begin
    ProcKind:='procedure';
    Result:=aInfo.FuncName+Args;
    end
  else
    begin
    ProcKind:='function';
    Result:=aInfo.FuncName+Args+': '+aInfo.ReturnTypeName;
    end;
  Result:=Result+aSuffix+';';
  if aInfo.ReturnDef is TIDLPromiseTypeDefDefinition then
    Result:=Result+' // Promise<'+TIDLPromiseTypeDefDefinition(aInfo.ReturnDef).ReturnType.TypeName+'>';
end;

function TWebIDLToPasWasmJob.GetArgName(d : TIDLDefinition) : string;
begin
  Result:=GetPasName(d);
  if IsKeyWord(Result) then
    Result:=Result+'_';
end;

procedure TWebIDLToPasWasmJob.WriteFunctionImplementation(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition);

var
  ArgNames: TStringList;

  function CreateLocal(aName: TIDLString): TIDLString;
  var
    i: Integer;
  begin
    Result:=aName;
    if ArgNames.IndexOf(Result)>=0 then
      begin
      i:=2;
      while ArgNames.IndexOf(Result+IntToStr(i))>=0 do inc(i);
      Result:=Result+IntToStr(i);
      end;
    ArgNames.Add(Result);
  end;


Var
  Data: TPasDataWasmJob;
  MethodInfo : TMethodCallInfo;
  Suff, Args, ProcKind, Sig, aClassName,
  InvokeCode,  LocalName, WrapperFn,
  ArgName, ArgTypeName,ArgResolvedTypeName: TIDLString;
  Overloads: TFPObjectList;
  I: Integer;
  ArgDefList: TIDLDefinitionList;
  CurDef, ArgType : TIDLDefinition;
  ArgDef: TIDLArgumentDefinition absolute CurDef;
  FinallyCode, TryCode,VarSection : Array of string;
  ANT : TPascalNativeType;

begin

  Data:=aDef.Data as TPasDataWasmJob;
  if Data.PasName='' then
    begin
    DoLog('Note: skipping Getter of '+aDef.Parent.Name+' at '+GetDefPos(aDef));
    exit;
    end;
  Suff:='';
  GetMethodInfo(aParent,aDef,MethodInfo);
  aClassName:=GetPasName(aParent);

  Overloads:=GetOverloads(ADef);
  try
    Suff:=GetFunctionSuffix(aDef,Overloads);
    For I:=0 to Overloads.Count-1 do
      begin
      ArgDefList:=TIDLDefinitionList(Overloads[i]);
      Sig:=GetFunctionSignature(aDef,MethodInfo,Suff,ArgDefList,ProcKind);

      ArgNames:=TStringList.Create;
      try
        for CurDef in ArgDefList do
          ArgNames.Add(GetArgName(ArgDef));

        AddLn(ProcKind+' '+aClassName+'.'+Sig);

        InvokeCode:='';
        if MethodInfo.ReturnTypeName<>'' then
          InvokeCode:='Result:=';

        VarSection:=[];
        TryCode:=[];
        FinallyCode:=[];
        Args:='';
        for CurDef in ArgDefList do
          begin
          if Args<>'' then
            Args:=Args+',';
          ArgName:=GetArgName(ArgDef);
          ArgType:=GetResolvedType(ArgDef.ArgumentType,ANT,ArgTypeName,ArgResolvedTypeName);
          if (ArgType is TIDLCallbackDefinition)  then
            begin
            if not (Assigned(TIDLCallbackDefinition(ArgType).FunctionDef)) then
              Raise EWebIDLParser.Create('[20220725181726] callback definition in '+GetPasName(aDef)+'without function signature type '+GetDefPos(ArgType));
            LocalName:=CreateLocal('m');
            VarSection:=Concat(VarSection,[ (LocalName+': '+JOB_JSValueTypeNames[jivkMethod]+';')]);
            WrapperFn:='JOBCall'+GetPasName(TIDLCallbackDefinition(ArgType).FunctionDef);
            TryCode:=Concat(TryCode,[LocalName+':='+JOB_JSValueTypeNames[jivkMethod]+'.Create(TMethod('+ArgName+'),@'+WrapperFn+');']);
            FinallyCode:=Concat(FinallyCode,[LocalName+'.free;']);
            ArgName:=LocalName;
            end;
          Args:=Args+ArgName;
          end;


        if foConstructor in aDef.Options then
          InvokeCode:=InvokeCode+MethodInfo.InvokeName+'(['+Args+'])'
        else
          begin
          Args:=',['+Args+']';
          InvokeCode:=InvokeCode+MethodInfo.InvokeName+'('''+aDef.Name+''''+Args;
          if MethodInfo.InvokeClassName<>'' then
            InvokeCode:=InvokeCode+','+MethodInfo.InvokeClassName+') as '+MethodInfo.ReturnTypeName
          else
            InvokeCode:=InvokeCode+')';
          end;
        if Length(VarSection)>0 then
          begin
          AddLn('var');
          Indent;
          AddLn(VarSection);
          undent;
          end;
        AddLn('begin');
        Indent;
        if IsStub then
          WriteFunctionInvokeCodeStub(aParent,aDef,MethodInfo)
        else
          begin
          if Length(TryCode)=0 then
            AddLn(InvokeCode+';')
          else
            begin
            AddLn(TryCode);
            AddLn('try');
              Indent;
              AddLn(InvokeCode+';');
              Undent;
            AddLn('finally');
              Indent;
              AddLn(FinallyCode);
              Undent;
            AddLn('end;');
            end;
          end;
        Undent;
        AddLn('end;');
      finally
        ArgNames.Free;
      end;
      end;
  finally
    Overloads.Free;
  end;
end;

procedure TWebIDLToPasWasmJob.WriteFunctionInvokeCodeStub(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition; aInfo : TMethodCallInfo);
begin
  //
end;

function TWebIDLToPasWasmJob.GetFunctionSuffix(aDef: TIDLFunctionDefinition; Overloads : TFPObjectList): String;

begin
  Result:='';
  if (aDef.Arguments.Count>0)
      and aDef.Argument[aDef.Arguments.Count-1].HasEllipsis then
    Result:='{; ToDo:varargs}';
  if not (FGeneratingInterface or GeneratingImplementation)  then
    Result:=Result+'; overload';
end;

function TWebIDLToPasWasmJob.WriteFunctionDefinition(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition): Boolean;

Var
  Data: TPasDataWasmJob;
  Suff, ProcKind, Sig : TIDLString;
  Overloads: TFPObjectList;
  I: Integer;
  ArgDefList: TIDLDefinitionList;
  MethodInfo : TMethodCallInfo;

begin
  Result:=True;
  Data:=aDef.Data as TPasDataWasmJob;
  if Data.PasName='' then
    begin
    DoLog('Note: skipping Getter of '+aDef.Parent.Name+' at '+GetDefPos(aDef));
    exit(false);
    end;
  if FGeneratingInterface and (([foConstructor, foStatic] * aDef.Options)<>[]) then
    exit;
  Suff:='';
  if (ADef.Name='createImageBitmap') then
    Writeln('Name');
  GetMethodInfo(aParent,aDef,MethodInfo);
  Overloads:=GetOverloads(ADef);
  try
    Suff:=GetFunctionSuffix(aDef,Overloads);
    For I:=0 to Overloads.Count-1 do
      begin
      ArgDefList:=TIDLDefinitionList(Overloads[i]);
      Sig:=GetFunctionSignature(aDef,MethodInfo,Suff,ArgDefList,ProcKind);
      AddLn(ProcKind+' '+Sig);
      end;
  finally
    Overloads.Free;
  end;
end;

function TWebIDLToPasWasmJob.WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition; aName: string): Boolean;
var
  FuncName, ReturnTypeName, ResolvedReturnTypeName: TIDLString;
  Params: TIDLString;
  ReturnDef: TIDLDefinition;
  ANT : TPascalNativeType;

begin
  Result:=True;
  FuncName:=aName;
  if FuncName='' then
    FuncName:=GetPasName(aDef);
  ReturnDef:=GetResolvedType(aDef.ReturnType,ANT,ReturnTypeName,ResolvedReturnTypeName);
  if ANT in [ntNone,ntUnknown] then
    begin
    ReturnTypeName:='';
    ResolvedReturnTypeName:='';
    end;
  if ReturnDef is TIDLSequenceTypeDefDefinition then
    ReturnTypeName:=PasInterfacePrefix+'Array'+PasInterfaceSuffix
  else if ReturnDef is TIDLPromiseTypeDefDefinition then
    ReturnTypeName:=PasInterfacePrefix+'Promise'+PasInterfaceSuffix;

  Params:=GetArguments(aDef.Arguments,False);
  if (ResolvedReturnTypeName='') then
    AddLn(FuncName+' = procedure '+Params+' of object;')
  else
    AddLn(FuncName+' = function '+Params+': '+ReturnTypeName+' of object;');

end;

procedure TWebIDLToPasWasmJob.WriteTypeDefsAndCallbackImplementations(aList: TIDLDefinitionList);

Var
  D: TIDLDefinition;
  CD: TIDLCallbackDefinition absolute D;
  cnt,total : integer;
  OK : Boolean;
  Msg : string;

begin
  Msg:='';
  Total:=0;
  for D in aList do
    if D is TIDLCallbackDefinition then
      if ConvertDef(D) then
        inc(Total);
  try
    OK:=False;
    Cnt:=0;
    for D in aList do
      if D is TIDLCallbackDefinition then
        if ConvertDef(D) then
          begin
          Inc(Cnt);
          WriteFunctionTypeCallbackImplementation(CD);
          end;
    OK:=True;
  finally
    if not OK then
      Msg:=SErrBeforeException;
    DoLog('Wrote %d of %d callback implementations%s.',[Cnt,Total,Msg]);
  end;
end;

function TWebIDLToPasWasmJob.GetKnownArgumentGetter(aDef : TIDLTypeDefinition; ArgTypeName, ArgResolvedTypename : String) : string;

begin
  if ArgResolvedTypeName='' then ; // Silence compiler warning;
  Result:='';
  if Pos('IJS',ArgTypeName)=1 then
    Result:='GetObject('+GetPasName(aDef)+') as '+ArgTypeName
  else if Pos('Array',ArgTypeName)>0 then
    Result:='GetObject('+GetPasName(aDef)+') as IJSArray';
end;

function TWebIDLToPasWasmJob.GetKnownResultAllocator(aDef : TIDLTypeDefinition; ArgTypeName, ArgResolvedTypename : String) : string;

begin
  if ArgResolvedTypeName='' then ; // Silence compiler warning;
  Result:='';
  if Pos('IJS',ArgTypeName)=1 then
    Result:='Result:=AllocIntf('+GetPasName(aDef)+' as '+ArgTypeName
  else if Pos('Array',ArgTypeName)>0 then
    Result:='Result:=AllocIntf('+GetPasName(aDef)+' as IJSArray';
end;

function TWebIDLToPasWasmJob.GetNativeTypeHelperGetterName(aNativeType : TPascalNativeType) : string;

begin
  Result:='';
  case aNativeType of
    ntBoolean: Result:='GetBoolean';
    ntShortInt,
    ntByte,
    ntSmallInt,
    ntWord,
    ntLongInt: Result:='GetLongInt';
    ntCardinal,
    ntInt64,
    ntQWord: Result:='GetMaxInt';
    ntSingle,
    ntDouble: Result:='GetDouble';
    ntUTF8String,
    ntUnicodeString: Result:='GetString';
    ntObject,
    ntArray : Result:='GetObject';
    ntVariant: Result:='GetVariant';
  else
    Result:='';
  end;
end;

function TWebIDLToPasWasmJob.GetNativeTypeHelperAllocatorName(aNativeType : TPascalNativeType) : string;

begin
  Result:='';
  case aNativeType of
    ntNone : Result:='AllocUndefined';
    ntBoolean: Result:='AllocBool';
    ntShortInt,
    ntByte,
    ntSmallInt,
    ntWord,
    ntLongInt: Result:='AllocLongInt';
    ntCardinal,
    ntInt64,
    ntQWord,
    ntSingle,
    ntDouble: Result:='AllocDouble';
    ntUTF8String,
    ntUnicodeString: Result:='AllocString';
    ntObject,
    ntArray : Result:='AllocIntf';
    ntVariant: Result:='AllocVariant';
  else
    Result:='';
  end;
end;

procedure TWebIDLToPasWasmJob.WriteFunctionTypeCallBackImplementation(aDef: TIDLCallBackDefinition);

var
  CallbackTypeName,FuncName, ReturnTypeName, ResolvedReturnTypeName: TIDLString;
  ArgName, ArgTypeName, ArgResolvedTypename: TIDLString;
  Params, Call, GetFunc: TIDLString;
  FetchArgs, VarSection : Array of string;
  Msg : String;
  Args: TIDLDefinitionList;
  ArgDef: TIDLArgumentDefinition;
  ArgNames: TStringList;
  j, i: Integer;
  ReturnDef, ArgType: TIDLDefinition;
  RNT,ANT : TPascalNativeType;
  FD : TIDLFunctionDefinition;

begin
  FD:=aDef.FunctionDef;
  FuncName:=GetPasName(FD);
  CallbackTypeName:=GetPasName(aDef);
  ReturnDef:=GetResolvedType(FD.ReturnType,RNT,ReturnTypeName,ResolvedReturnTypeName);
  if RNT in [ntNone,ntUnknown] then
    begin
    ReturnTypeName:='';
    ResolvedReturnTypeName:='';
    end;
  if ReturnDef is TIDLSequenceTypeDefDefinition then
    ReturnTypeName:=PasInterfacePrefix+'Array'+PasInterfaceSuffix
  else if ReturnDef is TIDLPromiseTypeDefDefinition then
    ReturnTypeName:=PasInterfacePrefix+'Promise'+PasInterfaceSuffix;

  Args:=FD.Arguments;

  Params:=GetArguments(Args,False);
  ArgNames:=TStringList.Create;
  try
    // create wrapper callback
    AddLn('function JOBCall%s(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;',[FuncName]);
    ArgNames.Add('aMethod');
    ArgNames.Add('h');
    VarSection:=[];
    FetchArgs:=[];
    Params:='';
    for i:=0 to Args.Count-1 do
      begin
      ArgDef:=Args[i] as TIDLArgumentDefinition;
      ArgName:=GetPasName(ArgDef);
      if ArgNames.IndexOf(ArgName)>=0 then
        begin
        j:=2;
        while ArgNames.IndexOf(ArgName+IntToStr(j))>=0 do inc(j);
        ArgName:=ArgName+IntToStr(j);
        end;
      ArgType:=GetResolvedType(ArgDef.ArgumentType,aNT,ArgTypeName,ArgResolvedTypename);
      GetFunc:=GetNativeTypeHelperGetterName(ANT);
      if aNt=ntObject then
        begin
        if argType is TIDLDictionaryDefinition then
          ArgResolvedTypename:='TJSObject'
        else
          ArgResolvedTypename:=IntfToPasClassName(ArgResolvedTypename);
        GetFunc:='GetObject('+ArgResolvedTypename+') as '+ArgTypeName
        end
      else if aNt=ntArray then
        GetFunc:='GetObject(TJSArray) as IJSArray'
      else if GetFunc='' then
        begin
        if argResolvedTypeName='TJOB_JSValue' then
          GetFunc:='GetValue'
        else if (ArgType is TIDLEnumDefinition)  then
          GetFunc:='GetString'
        else if (ArgType is TIDLSequenceTypeDefDefinition)  then
          GetFunc:='GetArray'
        else if argType is TIDLTypeDefinition then
          begin
          GetFunc:=GetKnownArgumentGetter(argType as TIDLTypeDefinition, ArgTypeName, ArgResolvedTypename);
          if GetFunc='' then
            begin
            if ArgType<>nil then
              Msg:=Format('%s (%s)',[ArgDef.ArgumentType.TypeName,ArgType.ClassName])
            else
              Msg:='No type';
            raise EWebIDLParser.Create('[20220725181732] not yet supported: function type arg['+IntToStr(I)+'] type '+Msg+' at '+GetDefPos(ArgDef));
            end;
          end
        else
          begin
          if ArgType<>nil then
            Msg:=Format('%s (%s)',[ArgDef.ArgumentType.TypeName,ArgType.ClassName])
          else
            Msg:='No type';
          raise EWebIDLParser.Create('[20220725181732] not yet supported: function type arg['+IntToStr(I)+'] type '+Msg+' at '+GetDefPos(ArgDef));
          end;
        end;
      // declare: var ArgName: ArgTypeName;
      VarSection:=Concat(VarSection,[ArgName+': '+ArgTypeName+';']);

      // get: ArgName:=H.GetX;
      FetchArgs:=Concat(FetchArgs,[ArgName+':=H.'+GetFunc+';']);

      // pass: ArgName
      if Params<>'' then
        Params:=Params+',';
      Params:=Params+ArgName;
      end;
    if Length(VarSection)>0 then
      begin
      AddLn('var');
      Indent;
      AddLn(VarSection);
      Undent;
      end;

    AddLn('begin');
    Indent;
    if Length(FetchArgs)>0 then
      AddLn(FetchArgs);
    Call:=CallBackTypeName+'(aMethod)('+Params+')';
    GetFunc:=GetNativeTypeHelperAllocatorName(RNT);
    if RNT=ntNone then
      begin
      AddLn(Call+';');
      GetFunc:='Result:=H.'+GetFunc+';';
      end
    else if GetFunc<>'' then
      GetFunc:='Result:=H.'+GetFunc+'('+Call+');'
    else
      if ReturnDef is TIDLInterfaceDefinition then
        GetFunc:='Result:=H.AllocIntf('+Call+');'
      else if ReturnDef is TIDLTypeDefinition then
        begin
        GetFunc:=GetKnownResultAllocator(ReturnDef as TIDLTypeDefinition,ReturnTypeName,ResolvedReturnTypeName);
        if GetFunc='' then
          raise EWebIDLParser.Create('[20220725181735] not yet supported: function type result type "'+ResolvedReturnTypeName+'" at '+GetDefPos(aDef));
        end
      else
        raise EWebIDLParser.Create('[20220725181735] not yet supported: function type result type "'+ResolvedReturnTypeName+'" at '+GetDefPos(aDef));
    AddLn(GetFunc);
    undent;
    AddLn('end;');
  finally
    ArgNames.Free;
  end;
end;

function TWebIDLToPasWasmJob.ExtractAliasName(aTypeName : string) : String;

var
  P : Integer;

begin
  Result:=TypeAliases.Values[aTypeName];
  P:=Pos(',',Result);
  if P>0 then
    Result:=Copy(Result,1,P-1);
end;
function TWebIDLToPasWasmJob.GetReadPropertyCall(aInfo : TAccessorInfo; aMemberName: String): string;

var
  TypeName,
  ObjClassName,
  ReadFuncName : string;

begin
  Result:='';
  Case aInfo.NativeType of
  ntBoolean: ReadFuncName:='ReadJSPropertyBoolean';
  ntShortInt,
  ntByte,
  ntSmallInt,
  ntWord,
  ntLongInt: ReadFuncName:='ReadJSPropertyLongInt';
  ntCardinal,
  ntInt64,
  ntQWord: ReadFuncName:='ReadJSPropertyInt64';
  ntSingle,
  ntDouble: ReadFuncName:='ReadJSPropertyDouble';
  ntUTF8String: ReadFuncName:='ReadJSPropertyUTF8String';
  ntUnicodeString: ReadFuncName:='ReadJSPropertyUnicodeString';
  ntVariant: ReadFuncName:='ReadJSPropertyVariant';
  ntMethod:  Result:='('+aInfo.ResolvedTypeName+'(ReadJSPropertyMethod('''+aMemberName+''')))';
  else
    if aInfo.ResolvedTypeName = 'TJOB_JSValue' then
      ReadFuncName:='ReadJSPropertyValue'
    else if aInfo.PropType is TIDLSequenceTypeDefDefinition then
      ObjClassName:=ClassPrefix+'Array'+ClassSuffix
    else if aInfo.PropType is TIDLPromiseTypeDefDefinition then
      ObjClassName:=ClassPrefix+'Promise'+ClassSuffix
    else
      begin
      ObjClassName:=GetPasName(aInfo.PropType);
      if (ObjClassName='') or (Pos(PasInterfacePrefix,ObjClassName)=1) then
        ObjClassName:=IntfToPasClassName(ObjClassName)
      else if (aInfo.PropType is TIDLTypeDefDefinition) then
        begin
        // Check if we have a typedef for an aliased type. Example: BigInteger = Uint8Array
        // must result in TJSUint8Array.
        TypeName:=TIDLTypeDefDefinition(aInfo.PropType).TypeName;
        TypeName:=ExtractAliasName(TypeName);
        if TypeName<>'' then
          ObjClassName:=IntfToPasClassName(TypeName)
        end;

      end;
    Result:='ReadJSPropertyObject('''+aMemberName+''','+ObjClassName+') as '+aInfo.NativeTypeName;
  end;

  if Result='' then
    Result:=ReadFuncName+'('''+aMemberName+''')';
end;



function TWebIDLToPasWasmJob.GetPrivateGetterInfo(aProp: TIDLPropertyDefinition; out aAccessInfo : TAccessorInfo): Boolean;

var
  D : TIDLString;
  aType : TIDLDefinition;

begin
  Result:=False;
  aAccessInfo:=Default(TAccessorInfo);
  if aProp.PropertyType=nil then
    exit;
  GetAccessorNames(aProp,aAccessinfo.FuncName,D);
  aType:=GetResolvedType(aProp.PropertyType,aAccessinfo.NativeType, aAccessinfo.NativeTypeName,aAccessinfo.ResolvedTypeName);
  aAccessInfo.PropType:=aType;
  if aType is TIDLInterfaceDefinition then
    aAccessInfo.NativeTypeName:=GetPasIntfName(aType)
  else if aType is TIDLDictionaryDefinition then
      aAccessInfo.NativeTypeName:=GetPasIntfName(aType)
  else if aType is TIDLFunctionDefinition then
    // exit // not supported yet
  else if aType is TIDLEnumDefinition then
    aAccessInfo.ResolvedTypeName:='UnicodeString';
  Result:=True;
end;

procedure TWebIDLToPasWasmJob.WritePrivateGetterImplementation(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition);

var
  aClassName, Call : String;
  Info : TAccessorInfo;

begin
  aClassName:=GetPasName(aParent);
  // case
  // stringifier ;
  // is equivalent to toString : DOMString
  // no n
  if aProp.PropertyType=nil then
    Exit;
  if (aProp.Name='') and (paStringifier in aProp.PropertyAccess) then
    Exit;

  if not GetPrivateGetterInfo(aProp,Info) then
    exit;
  Call:=GetReadPropertyCall(Info,aProp.Name);
  Addln('function '+aClassName+'.'+info.FuncName+': '+Info.NativeTypeName+';');
  Addln('begin');
  Addln('  Result:='+Call+';');
  Addln('end;');
end;

function TWebIDLToPasWasmJob.WritePrivateGetter(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition): boolean;

var
  Info : TAccessorInfo;

begin
  if (aParent=Nil) then ; // Silence compiler warning
  Result:=true;
  if (aProp.Name='') and not (paWrite in aProp.PropertyAccess) then
    Exit;
  if aProp.PropertyType=nil then
    exit;
  GetPrivateGetterInfo(aProp,Info);
  AddLn('function '+Info.FuncName+': '+Info.NativeTypeName+'; '{overload;'});
end;

function TWebIDLToPasWasmJob.GetAccessorNames(Attr: TIDLPropertyDefinition; out aGetter, aSetter: TIDLString): Boolean;

var
  D : TPasDataWasmJob;
begin
  Result:=Attr.Data is TPasDataWasmJob;
  if Result then
    begin
    D:=Attr.Data as TPasDataWasmJob;
    aGetter:=D.PropertyGetterName;
    aSetter:=D.PropertySetterName;
    end;
end;

function TWebIDLToPasWasmJob.GetPrivateSetterInfo(aProp: TIDLPropertyDefinition; out aAccessInfo: TAccessorInfo): Boolean;

var
  D : TIDLString;
  aType : TIDLDefinition;

begin
  Result:=False;
  if (aProp.PropertyType=nil) then
    exit;
  if (aProp.Name='') and not (paWrite in aProp.PropertyAccess) then
    Exit;
  GetAccessorNames(aProp,D,aAccessInfo.FuncName);
  aType:=GetResolvedType(aProp.PropertyType,aAccessInfo.NativeType,aAccessInfo.NativeTypeName,aAccessInfo.ResolvedTypeName);
  aAccessInfo.PropType:=aType;
  if aType is TIDLInterfaceDefinition then
    aAccessInfo.NativeTypeName:=GetPasIntfName(aType)
  else if aType is TIDLDictionaryDefinition then
    aAccessInfo.NativeTypeName:=GetPasIntfName(aType)
  else if aType is TIDLFunctionDefinition then
    // exit // not supported yet
  else if aType is TIDLEnumDefinition then
    aAccessInfo.ResolvedTypeName:='UnicodeString';
  Result:=True;
end;



function TWebIDLToPasWasmJob.GetWritePropertyCall(aInfo: TAccessorInfo; aMemberName: String): string;

var
  WriteFuncName : String;

begin
  Result:='';
  case aInfo.NativeType of
  ntBoolean: WriteFuncName:='WriteJSPropertyBoolean';
  ntShortInt,
  ntByte,
  ntSmallInt,
  ntWord,
  ntLongInt: WriteFuncName:='WriteJSPropertyLongInt';
  ntCardinal,
  ntInt64,
  ntQWord: WriteFuncName:='WriteJSPropertyDouble';
  ntSingle,
  ntDouble: WriteFuncName:='WriteJSPropertyDouble';
  ntUTF8String: WriteFuncName:='WriteJSPropertyUTF8String';
  ntUnicodeString: WriteFuncName:='WriteJSPropertyUnicodeString';
  ntVariant: WriteFuncName:='WriteJSPropertyVariant';
  ntMethod:  Result:='WriteJSPropertyMethod('''+aMemberName+''',TMethod(aValue))';
  else
    if aInfo.ResolvedTypeName='TJOB_JSValue' then
      WriteFuncName:='WriteJSPropertyValue'
    else
      WriteFuncName:='WriteJSPropertyObject';
  end;
  if Result='' then
    Result:=Format('%s(''%s'',aValue)',[WriteFuncName,aMemberName]);
end;

procedure TWebIDLToPasWasmJob.WritePrivateSetterImplementation(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition);

var
  aClassName, Call : String;
  Info : TAccessorInfo;

begin
  if Not (paWrite in aProp.PropertyAccess) then
    exit;
  if aProp.PropertyType=nil then
    exit;
  aClassName:=GetPasName(aParent);
  if not GetPrivateSetterInfo(aProp,Info) then
    exit;
  Call:=GetWritePropertyCall(Info, aProp.Name);
  Addln('procedure %s.%s(const aValue : %s);',[aClassName,info.FuncName,Info.NativeTypeName]);
  Addln('begin');
  indent;
  Addln(Call+';');
  undent;
  Addln('end;');
end;


procedure TWebIDLToPasWasmJob.WriteMapLikePrivateSetterImplementation(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition
  );


begin
  if (aMap=Nil) and (aParent=Nil) then ; // Silence compiler warning
  // None
end;

procedure TWebIDLToPasWasmJob.WriteMapLikePrivateGetterImplementation(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition
  );
var
  call, aClassName : string;
  Info : TAccessorInfo;

begin
  if (aMap=Nil) and (aParent=Nil) then ; // Silence compiler warning
  aClassName:=GetPasName(aParent);
  Info:=Default(TAccessorInfo);
  Info.NativeTypeName:='Integer';
  Info.ResolvedTypeName:='LongInt';
  Info.NativeType:=ntLongint;
  Addln('function '+aClassName+'._Getsize: LongInt;');
  Addln('begin');
  Addln('  Result:=0;');
  Addln('end;');
end;


function TWebIDLToPasWasmJob.WritePrivateSetter(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition): boolean;

var
  Info : TAccessorInfo;

begin
  if (aParent=Nil) then ; // Silence compiler warning
  if aProp.PropertyType=nil then
    exit;
  if not (paWrite  in aProp.PropertyAccess) then
    exit(false);
  if not GetPrivateSetterInfo(aProp,Info) then exit;
  AddLn('procedure '+Info.FuncName+'(const aValue: '+Info.NativeTypeName+');' {overload;'});
end;

function TWebIDLToPasWasmJob.WriteProperty(aParent: TIDLDefinition; aProp: TIDLPropertyDefinition): boolean;
var
  PropName, Code, aTypeName, aResolvedTypeName: TIDLString;
  aType: TIDLDefinition;
  ANT : TPascalNativeType;
  GetterName,SetterName : TIDLString;

begin
  if aParent=nil then ;
  if (aProp.PropertyType=nil) then
    begin
    if not (paStringifier in aProp.PropertyAccess) then
      DoLog('Note: skipping field "'+AProp.Name+'" without type at '+GetDefPos(aProp));
    exit;
    end;
  PropName:=GetPasName(aProp);
  aType:=GetResolvedType(aProp.PropertyType,ANT,aTypeName,aResolvedTypeName);
  if aType is TIDLInterfaceDefinition then
    aTypeName:=GetPasIntfName(aType)
  else if aType is TIDLDictionaryDefinition then
    aTypeName:=GetPasIntfName(aType);
  GetAccessorNames(aProp,GetterName,SetterName);
  Code:='property '+PropName+': '+aTypeName+' read '+GetterName;
  if (paWrite in aProp.PropertyAccess) then
    Code:=Code+' write '+SetterName;
  Code:=Code+';';
  if aType is TIDLFunctionDefinition then
    Code:='// '+Code;
  AddLn(Code);
  Result:=true;
end;

function TWebIDLToPasWasmJob.WriteRecordDef(aDef: TIDLRecordDefinition): Boolean;
begin
  Result:=true;
  AddLn(GetPasName(aDef)+' = '+ClassPrefix+'Object'+ClassSuffix+';');
end;

procedure TWebIDLToPasWasmJob.WriteSequenceDef(
  aDef: TIDLSequenceTypeDefDefinition);

var
  N,aLine : String;

begin
  N:=GetPasName(aDef);
  aLine:=N+' = '+PasInterfacePrefix+'Array'+PasInterfaceSuffix+'; // array of '+GetJSTypeName(aDef.ElementType);
  Addln(aLine);
end;

procedure TWebIDLToPasWasmJob.WriteNamespaceVars;

var
  i: Integer;
  VarName, VarType: String;
  NS : TIDLNamespaceDefinition;
  HaveNamespaces : Boolean;

begin
  HaveNameSpaces:=False;
  I:=0;
  While (Not HaveNameSpaces) and (I<Context.Definitions.Count) do
    begin
    HaveNameSpaces:=Context.Definitions[i] is TIDLNamespaceDefinition;
    Inc(I);
    end;
  if HaveNameSpaces then
    Comment('Namespaces');
  for I:=0 to Context.Definitions.Count-1 do
    if Context.Definitions[i] is TIDLNamespaceDefinition then
      begin
      NS:=Context.Definitions[i] as TIDLNamespaceDefinition;
      if (not NS.IsPartial) and ConvertDef(NS) then
        begin
        VarName:=Context.Definitions[i].Name;
        VarType:=GetPasIntfName(Context.Definitions[i]);
        AddLn(VarName+': '+VarType+';');
        end;
      end;
end;

procedure TWebIDLToPasWasmJob.WriteGlobalVar(aDef : String);
var
  PasVarName, JSClassName, JOBRegisterName: TIDLString;
  iDef: TIDLDefinition;
begin
  if not SplitGlobalVar(aDef,PasVarName,JSClassName,JOBRegisterName) then
    raise EConvertError.Create('invalid global var "'+aDef+'"');
  iDef:=FindGlobalDef(JSClassName);
  if iDef=nil then
    raise EConvertError.Create('missing global var "'+PasVarName+'" type "'+JSClassName+'"');
  if ConvertDef(iDef) then
    AddLn(PasVarName+': '+GetPasName(iDef)+';');
end;

procedure TWebIDLToPasWasmJob.WriteEnumImplementation(aDef : TIDLEnumDefinition);

begin
  if (aDef=Nil) then ; // Silence compiler warning
end;

procedure TWebIDLToPasWasmJob.WriteDictionaryImplemention(aDef : TIDLDictionaryDefinition);


Var
  ML: TIDLDefinitionList;

begin
  ML:=TIDLDefinitionList.Create(Nil,False);
  try
    Adef.GetFullMemberList(ML);
    WritePrivateGetterImplementations(aDef,ML);
    WritePrivateSetterImplementations(aDef,ML);
    WriteUtilityMethodImplementations(aDef,ML);
  finally
    ML.Free;
  end;
end;

procedure TWebIDLToPasWasmJob.WritePrivateGetterImplementations(aDef : TIDLStructuredDefinition; ML : TIDLDefinitionList);

var
  D : TIDLDefinition;
  PD : TIDLPropertyDefinition absolute D;
  MD : TIDLMapLikeDefinition absolute D;

begin
  for D in ML do
    if ConvertDef(D) then
      begin
      if D is TIDLPropertyDefinition then
        WritePrivateGetterImplementation(aDef,PD)
      else if D is TIDLMapLikeDefinition then
        WriteMapLikePrivateGetterImplementation(aDef,MD);
      end;
end;

procedure TWebIDLToPasWasmJob.WritePrivateSetterImplementations(aDef : TIDLStructuredDefinition; ML : TIDLDefinitionList);

var
  D : TIDLDefinition;
  PD : TIDLPropertyDefinition absolute D;
  MD : TIDLMapLikeDefinition absolute D;

begin
  for D in ML do
    if ConvertDef(D) then
      begin
      if D is TIDLPropertyDefinition then
        WritePrivateSetterImplementation(aDef,PD)
      else if D is TIDLMapLikeDefinition then
        WriteMapLikePrivateSetterImplementation(aDef,MD);
      end;
end;

procedure TWebIDLToPasWasmJob.WriteMethodImplementations(aDef : TIDLStructuredDefinition; ML : TIDLDefinitionList);

var
  D : TIDLDefinition;
  DF : TIDLFunctionDefinition absolute D;
  DM : TIDLMapLikeDefinition absolute D;

begin
  For D in ML do
    if ConvertDef(D) then
      if D Is TIDLFunctionDefinition then
        WriteFunctionImplementation(aDef,DF)
      else If D Is TIDLMapLikeDefinition then
        WriteMapLikeFunctionImplementations(aDef,DM);
end;

function TWebIDLToPasWasmJob.DefaultForNativeType(aNativeType : TPascalNativeType; aReturnTypeName: String) : String;
var
  S,N : string;

begin
  Case aNativeType of
    ntUnknown, // unknown
    ntNone,    // None -> void
    ntError : Result:='';   // Special : error condition
    ntBoolean : Result:='False';
    ntShortInt,
    ntByte,
    ntSmallInt,
    ntWord,
    ntLongint,
    ntCardinal,
    ntInt64,
    ntQWord : Result:='0';
    ntSingle,
    ntDouble : Result:='0.0';
    ntUnicodeString,
    ntUTF8String : Result:='''''';
    ntVariant:  Result:='null';
    ntObject :
      Result:=StringReplace(aReturnTypeName,'IJS','TJS',[])+'.CreateEmpty()';
    ntInterface : Result:='nil';
    ntArray :
      begin
      S:=Copy(aReturnTypeName,1,Length(PasInterfacePrefix));
      N:=Copy(aReturnTypeName,Length(PasInterfacePrefix)+1);
      if (S=PasInterfacePrefix) and (TypeAliases.Values[N]<>'') then
        Result:=IntfToPasClassName(aReturnTypeName)+'.CreateEmpty()'
      else
        Result:='TJSArray.CreateEmpty()';
      end;
    ntMethod : Result:='Nil';
    end;
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeGetFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  D,aResolvedKeyTypeName,aResolvedValueTypeName: String;
  Func,InvokeClass,aClassName : string;
  KNT,VNT : TPascalNativeTYpe;

begin
  aClassName:=GetPasName(aDef);
  GetResolvedType(ML.KeyType,KNT,D,aResolvedKeyTypeName);
  GetResolvedType(ML.ValueType,VNT,D,aResolvedValueTypeName);
  Func:=GetInvokeNameFromTypeName(aResolvedValueTypeName,ML.ValueType);
  if VNT=ntObject then
    InvokeClass:=GetInvokeClassName(ML.ValueType,aResolvedValueTypeName,Nil);
  AddLn('function %s.get(key: %s) : %s;',[aClassName,aResolvedKeyTypeName,aResolvedValueTypeName]);
  AddLn('begin');
  Indent;
  if IsStub then
    AddLn('Result:='+DefaultForNativeType(vnt,InvokeClass)+';')
  else if VNT=ntObject then
    AddLn('Result:='+Func+'(''get'',[key],'+InvokeClass+') as '+aResolvedValueTypeName+';')
  else
    AddLn('Result:='+Func+'(''get'',[key]);');
  Undent;
  AddLn('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeDeleteFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  D,aResolvedKeyTypeName,aResolvedValueTypeName: String;
  aClassName : string;
  KNT,VNT : TPascalNativeTYpe;

begin
  aClassName:=GetPasName(aDef);
  GetResolvedType(ML.KeyType,KNT,D,aResolvedKeyTypeName);
  GetResolvedType(ML.ValueType,VNT,D,aResolvedValueTypeName);
  AddLn('Procedure %s.delete(key: %s);',[aClassName,aResolvedKeyTypeName]);
  AddLn('begin');
  Indent;
  if not IsStub then
    AddLn('InvokeJSNoResult(''delete'',[key]);');
  Undent;
  AddLn('end;');
end;


procedure TWebIDLToPasWasmJob.WriteMapLikeSetFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);
var
  D,aResolvedKeyTypeName,aResolvedValueTypeName: String;
  aClassName : string;
  KNT,VNT : TPascalNativeTYpe;

begin
  aClassName:=GetPasName(aDef);
  GetResolvedType(ML.KeyType,KNT,D,aResolvedKeyTypeName);
  GetResolvedType(ML.ValueType,VNT,D,aResolvedValueTypeName);
  AddLn('Procedure %s.set_(key: %s; value : %s);',[aClassName,aResolvedKeyTypeName,aResolvedValueTypeName]);
  AddLn('begin');
  Indent;
  if not IsStub then
    AddLn('InvokeJSNoResult(''set'',[key,Value]);');
  Undent;
  AddLn('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeClearFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);
var
  aClassName : string;
begin
  if (ML=Nil)  then ; // Silence compiler warning
  aClassName:=GetPasName(aDef);
  AddLn('Procedure %s.clear;',[aClassName]);
  AddLn('begin');
  Indent;
  if not IsStub then
    AddLn('InvokeJSNoResult(''clear'',[]);');
  Undent;
  AddLn('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeHasFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  D,aResolvedKeyTypeName: String;
  aClassName : string;
  KNT : TPascalNativeTYpe;

begin
  aClassName:=GetPasName(aDef);
  GetResolvedType(ML.KeyType,KNT,D,aResolvedKeyTypeName);
  AddLn('function %s.has(key: %s) : Boolean;',[aClassName,aResolvedKeyTypeName]);
  AddLn('begin');
  Indent;
  if IsStub then
    AddLn('Result:=False;')
  else
    AddLn('Result:=InvokeJSBooleanResult(''has'',[key]);');
  Undent;
  AddLn('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeEntriesFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  aClassName : string;

begin
  if (ML=Nil) then ; // Silence compiler warning
  aClassName:=GetPasName(aDef);
  AddLn('function %s.entries : IJSIterator;',[aClassName]);
  AddLn('begin');
  Indent;
  if IsStub then
    AddLn('Result:=TJSIterator.CreateEmpty;')
  else
    AddLn('Result:=InvokeJSObjectResult(''entries'',[],TJSIterator) as IJSIterator;');
  Undent;
  AddLn('end;');
end;


procedure TWebIDLToPasWasmJob.WriteMapLikeKeysFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  aClassName : string;

begin
  if (ML=Nil) then ; // Silence compiler warning
  aClassName:=GetPasName(aDef);
  AddLn('function %s.keys : IJSIterator;',[aClassName]);
  AddLn('begin');
  Indent;
  if IsStub then
    AddLn('Result:=TJSIterator.CreateEmpty;')
  else
    AddLn('Result:=InvokeJSObjectResult(''keys'',[],TJSIterator) as IJSIterator;');
  Undent;
  AddLn('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeValuesFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  aClassName : string;

begin
  if (ML=Nil)  then ; // Silence compiler warning
  aClassName:=GetPasName(aDef);
  AddLn('function %s.values : IJSIterator;',[aClassName]);
  AddLn('begin');
  Indent;
  if IsStub then
    AddLn('Result:=TJSIterator.CreateEmpty;')
  else
    AddLn('Result:=InvokeJSObjectResult(''values'',[],TJSIterator) as IJSIterator;');
  Undent;
  AddLn('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeFunctionImplementations(aDef : TIDLStructuredDefinition; MD : TIDLMapLikeDefinition);

Var
  L : TIDLDefinitionList;
  lReadOnly : Boolean;

begin
  lReadOnly:=MD.IsReadonly;
  L:=TIDLDefinitionList.Create(Nil,False);
  try
    aDef.GetFullMemberList(L);
    if not L.HasName('get') then
      WriteMapLikeGetFunctionImplementation(aDef,MD);
    if not L.HasName('has') then
      WriteMapLikeHasFunctionImplementation(aDef,MD);
    if not L.HasName('entries') then
      WriteMapLikeEntriesFunctionImplementation(aDef,MD);
    if not L.HasName('keys') then
      WriteMapLikeKeysFunctionImplementation(aDef,MD);
    if not L.HasName('values') then
      WriteMapLikeValuesFunctionImplementation(aDef,MD);
    if not lReadOnly then
      begin
      if Not L.HasName('set') then
        WriteMapLikeSetFunctionImplementation(aDef,MD);
      if Not L.HasName('clear') then
        WriteMapLikeClearFunctionImplementation(aDef,MD);
      if Not L.HasName('delete') then
        WriteMapLikeDeleteFunctionImplementation(aDef,MD);
      end;
  finally
    L.Free;
  end;
end;

procedure TWebIDLToPasWasmJob.WriteDictionaryConstructor(aDict: TIDLDictionaryDefinition);

var
  CurrClassName: TIDLString;
  IDL : TIDLDefinition;
  MD : TIDLDictionaryMemberDefinition absolute IDL;
  aName : string;

begin
  CurrClassName:=GetPasName(aDict);
  AddLn('constructor %s.create(const aDict : %sRec); overload;';[CurrClassName,CurrClassName]);
  Addln('begin');
  Indent;
  For IDl in aDict.Members do
    if IDL is TIDLDictionaryMemberDefinition then
      begin
      aName:=GetPasName(MD);
      AddLn('Self.%s:=aDict.%s;',[aName,aName]);
      end;
  Undent;
  AddLn('end;');
  AddLn('');
end;

procedure TWebIDLToPasWasmJob.WriteUtilityMethodImplementations(aDef : TIDLStructuredDefinition; ML : TIDLDefinitionList);

var
  aJSClassName,aClassName, aPasIntfName: TIDLString;

begin
  if (ML=Nil) then ; // Silence compiler warning
  aClassName:=GetPasName(aDef);
  aPasIntfName:=GetPasIntfName(aDef);
  if aDef.StructuredType=sdDictionary then
    begin
    WriteDictionaryConstructor(aDef as TIDLDictionaryDefinition);
    aJSClassName:='Object'
    end
  else
    aJSClassName:=aDef.Name;
  AddLn('class function %s.JSClassName: UnicodeString;',[aClassName]);
  AddLn('begin');
  Indent;
  AddLn('Result:=''%s'';',[aJSClassName]);
  Undent;
  AddLn('end;');
  AddLn('');
  AddLn('class function %s.Cast(const Intf: IJSObject): %s;',[aClassName,aPasIntfName]);
  AddLn('begin');
  Indent;
  AddLn('Result:=%s.JOBCast(Intf);',[aClassName]);
  Undent;
  AddLn('end;');
end;


procedure TWebIDLToPasWasmJob.WriteInterfaceImplemention(aDef : TIDLInterfaceDefinition);

Var
  ML: TIDLDefinitionList;

begin
  ML:=TIDLDefinitionList.Create(Nil,False);
  try
    Adef.GetFullMemberList(ML);
    WritePrivateGetterImplementations(aDef,ML);
    WritePrivateSetterImplementations(aDef,ML);
    WriteMethodImplementations(aDef,ML);
    WriteUtilityMethodImplementations(aDef,ML);
  finally
    ML.Free;
  end;
end;

procedure TWebIDLToPasWasmJob.WriteNamespaceImplemention(aDef : TIDLNamespaceDefinition);

Var
  ML: TIDLDefinitionList;

begin
  ML:=TIDLDefinitionList.Create(Nil,False);
  try
    ADef.GetFullMemberList(ML);
    WritePrivateGetterImplementations(aDef,ML);
    WritePrivateSetterImplementations(aDef,ML);
    WriteMethodImplementations(aDef,ML);
    WriteUtilityMethodImplementations(aDef,ML);
  finally
    ML.Free;
  end;
end;


procedure TWebIDLToPasWasmJob.WriteDefinitionImplementation(D: TIDLDefinition);

begin
  if D is TIDLEnumDefinition then
    WriteEnumImplementation(D as TIDLEnumDefinition)
  else if D is TIDLDictionaryDefinition then
    WriteDictionaryImplemention(D as TIDLDictionaryDefinition)
  else if D is TIDLInterfaceDefinition then
    WriteInterfaceImplemention(D as TIDLInterfaceDefinition)
  else if D is TIDLNamespaceDefinition then
    WriteNamespaceImplemention(D as TIDLNamespaceDefinition);
end;

function  TWebIDLToPasWasmJob.OnlyConstants(D : TIDLStructuredDefinition) : Boolean;

var
  i,aCount : Integer;

begin
  Result:=True;
  I:=0;
  aCount:=D.Members.Count;
  While Result and (I<aCount) do
    begin
    Result:=D.Members[i] is TIDLConstDefinition;
    Inc(I);
    end;
end;

procedure TWebIDLToPasWasmJob.WriteImplementation;
var
  i: Integer;
  aDef: TIDLDefinition;
  nsDef : TIDLNamespaceDefinition absolute aDef;
  PasVarName, JSClassName, JOBRegisterName: TIDLString;
begin
  inherited WriteImplementation;
  if (GlobalVars.Count>0) or Context.HaveNameSpaces then
    begin
    AddLn('initialization');
    Indent;
    for i:=0 to GlobalVars.Count-1 do
      begin
      SplitGlobalVar(GlobalVars[i],PasVarName,JSClassName,JOBRegisterName);
      aDef:=FindGlobalDef(JSClassName);
      if IsStub then
        AddLn(PasVarName+':='+GetPasName(aDef)+'.Create();')
      else if ConvertDef(aDef) then
        AddLn(PasVarName+':='+GetPasName(aDef)+'.JOBCreateGlobal('''+JOBRegisterName+''');');
      end;
    for I:=0 to Context.Definitions.Count-1 do
      begin
      aDef:=Context.Definitions[i];
      if aDef is TIDLNamespaceDefinition then
        if not NSDef.IsPartial and ConvertDef(aDef) then
          if not (OnlyConstants(NSDef) or NSDef.HasPrefAttribute) then
            begin
            PasVarName:=Context.Definitions[i].Name;
            if IsStub then
              AddLn(PasVarName+':='+GetPasName(aDef)+'.Create();')
            else if ConvertDef(aDef) then
              AddLn(PasVarName+':='+GetPasName(aDef)+'.JOBCreateGlobal('''+PasVarName+''');');
            end;
      end;
    Undent;

    AddLn('finalization');
    Indent;
    for i:=0 to GlobalVars.Count-1 do
      begin
      SplitGlobalVar(GlobalVars[i],PasVarName,JSClassName,JOBRegisterName);
      aDef:=FindGlobalDef(JSClassName);
      if ConvertDef(aDef) then
        AddLn(PasVarName+'.Free;');
      end;
    for I:=0 to Context.Definitions.Count-1 do
      begin
      aDef:=Context.Definitions[i];
      if aDef is TIDLNamespaceDefinition then
        if not NSDef.IsPartial and ConvertDef(aDef) then
          if not (OnlyConstants(NSDef) or NSDef.HasPrefAttribute) then
            begin
            PasVarName:=Context.Definitions[i].Name;
            AddLn(PasVarName+':=Nil;');
            end;
      end;
    Undent;
    end;
end;

constructor TWebIDLToPasWasmJob.Create(ThOwner: TComponent);
begin
  inherited Create(ThOwner);
  // Switches.Add('modeswitch FunctionReferences');
  PasDataClass:=TPasDataWasmJob;
  ClassPrefix:='TJS';
  PasInterfacePrefix:='IJS';
  GetterPrefix:='_Get';
  SetterPrefix:='_Set';
  KeywordSuffix:='_';
  BaseOptions:=BaseOptions+[coExpandUnionTypeArgs,coDictionaryAsClass];
end;

function TWebIDLToPasWasmJob.SplitGlobalVar(Line: TIDLString; out PasVarName,
  JSClassName, JOBRegisterName: TIDLString): boolean;
var
  p: SizeInt;
begin
  PasVarName:='';
  JSClassName:='';
  JOBRegisterName:='';
  p:=Pos('=',Line);
  PasVarName:=LeftStr(Line,p-1);
  if not IsValidIdent(PasVarName) then exit(false);
  System.Delete(Line,1,p);
  p:=Pos(',',Line);
  JSClassName:=LeftStr(Line,p-1);
  if not IsValidIdent(JSClassName) then exit(false);
  JOBRegisterName:=copy(Line,p+1,length(Line));
  Result:=IsValidIdent(JOBRegisterName);
end;

end.

