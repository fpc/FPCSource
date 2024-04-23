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

  TPasDataWasmJob = class(TPasData)
  end;

  { TWebIDLToPasWasmJob }

  TWebIDLToPasWasmJob = class(TBaseWebIDLToPas)
  private
    FPasInterfacePrefix: TIDLString;
    FPasInterfaceSuffix: TIDLString;
    FGeneratingInterface : Boolean;
    function GetArgName(d: TIDLDefinition): string;
    function GetFunctionSuffix(aDef: TIDLFunctionDefinition; Overloads: TFPObjectList): String;
    function GetInvokeClassName(aResultDef: TIDLDefinition; aName: TIDLString; aDef: TIDLFunctionDefinition=nil): TIDLString;
    function GetInvokeClassNameFromTypeAlias(aName: TIDLString; aDef: TIDLDefinition): TIDLString;
    function GetInvokeNameFromAliasName(const aTypeName: TIDLString; aType: TIDLDefinition): string;
    function GetInvokeNameFromNativeType(aNativeType: TPascalNativeType): String;
    function GetInvokeNameFromTypeName(const aTypeName: TIDLString; aType: TIDLDefinition): TIDLString;
    function GetKnownArgumentGetter(aDef: TIDLTypeDefinition; ArgTypeName, ArgResolvedTypename: String): string;
    function GetKnownResultAllocator(aDef: TIDLTypeDefinition; ArgTypeName, ArgResolvedTypename: String): string;
    function GetNativeTypeHelperAllocatorName(aNativeType: TPascalNativeType): string;
    function GetNativeTypeHelperGetterName(aNativeType: TPascalNativeType): string;

  Protected
    function BaseUnits: String; override;
    // Auxiliary routines
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
    function GetDictionaryDefHead(const CurClassName: String;  Dict: TIDLDictionaryDefinition): String; override;
    function WriteOtherImplicitTypes(Intf: TIDLStructuredDefinition; aMemberList: TIDLDefinitionList): Integer; override;
    // Code generation routines. Return the number of actually written defs.
    function WritePrivateGetters(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; override;
    function WritePrivateSetters(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; override;
    function WriteProperties(aParent: TIDLDefinition; aList: TIDLDefinitionList): Integer; override;
    function WriteUtilityMethods(Intf: TIDLStructuredDefinition): Integer;  override;
    // Maplike
    function WriteMapLikeProperties(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer; override;
    function WriteMapLikePrivateReadOnlyFields(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer; override;
    function WriteMapLikePrivateGetters(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition): Integer; override;
    // Definitions. Return true if a definition was written.
    function WriteEnumDef(aDef: TIDLEnumDefinition): Boolean; override;
    function WriteDictionaryField(aDict: TIDLDictionaryDefinition;  aField: TIDLDictionaryMemberDefinition): Boolean; override;
    function WriteForwardClassDef(D: TIDLStructuredDefinition): Boolean; override;
    function WriteFunctionDefinition(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition): Boolean; override;
    function WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition; aName : string = ''): Boolean; override;
    function WritePrivateGetter(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition): boolean; virtual;
    function WritePrivateSetter(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition): boolean; virtual;
    function WriteProperty(aParent: TIDLDefinition; Attr: TIDLAttributeDefinition): boolean; virtual;
    function WriteRecordDef(aDef: TIDLRecordDefinition): Boolean; override;
    procedure WriteSequenceDef(aDef: TIDLSequenceTypeDefDefinition); override;
    // Extra interface/Implementation code.
    function GetPrivateGetterInfo(Attr: TIDLAttributeDefinition; out aNativeType: TPascalNativeType; out AttrTypeName,
      AttrResolvedTypeName, FuncName: TIDLString): TIDLDefinition;
    function GetPrivateSetterInfo(Attr: TIDLAttributeDefinition; out aNativeType: TPascalNativeType; out AttrTypeName, AttrResolvedTypeName, FuncName: TIDLString): TIDLDefinition;
    function GetReadPropertyCall(aNativeType : TPascalnativeType; AttrResolvedTypeName, aNativeTypeName: TIDLString; aMemberName: String; aType: TIDLDefinition): string;
    function GetWritePropertyCall(aNativeType : TPascalnativeType; AttrResolvedTypeName, aNativeTypeName: TIDLString; aMemberName: String; aType: TIDLDefinition): string;
    function GetFunctionSignature(aDef: TIDLFunctionDefinition; aReturnDef: TIDLDefinition; aFuncname, aReturnTypeName,
      aSuffix: TIDLString; ArgDefList: TIDLDefinitionList; out ProcKind: TIDLString): String;
    function GetMethodInfo(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition; out FuncName, ReturnTypeName, ResolvedReturnTypeName, InvokeName, InvokeClassName: TIDLString): TIDLDefinition;
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
    procedure WritePrivateGetterImplementations(aDef: TIDLStructuredDefinition; ML: TIDLDefinitionList); virtual;
    procedure WritePrivateSetterImplementations(aDef: TIDLStructuredDefinition; ML: TIDLDefinitionList); virtual;
    procedure WriteUtilityMethodImplementations(aDef: TIDLStructuredDefinition; ML: TIDLDefinitionList);virtual;
    Procedure WritePrivateGetterImplementation(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition); virtual;
    Procedure WritePrivateSetterImplementation(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition);virtual;
    // MapLike
    procedure WriteMapLikePrivateSetterImplementation(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition); virtual;
    procedure WriteMapLikePrivateGetterImplementation(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition); virtual;
    procedure WriteMapLikeFunctionImplementations(aDef: TIDLStructuredDefinition; MD: TIDLMapLikeDefinition);
    procedure WriteMapLikeEntriesFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);
    procedure WriteMapLikeGetFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);
    procedure WriteMapLikeHasFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);
    procedure WriteMapLikeKeysFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);
    procedure WriteMapLikeValuesFunctionImplementation(aDef: TIDLStructuredDefinition; ML: TIDLMapLikeDefinition);

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
  Result:='SysUtils, JOB_JS';
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
    aTypeName:=GetPasClassName(aTypeName)
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

function TWebIDLToPasWasmJob.GetDictionaryDefHead(const CurClassName: String;
  Dict: TIDLDictionaryDefinition): String;
begin
  Result:=CurClassName+'Rec = record';
  if Dict=nil then ;
end;

function TWebIDLToPasWasmJob.WriteOtherImplicitTypes(Intf: TIDLStructuredDefinition; aMemberList: TIDLDefinitionList): Integer;
var
  iIntf : TIDLInterfaceDefinition absolute Intf;
  aPasIntfName, Decl, ParentName: TIDLString;
  isNamespace : Boolean;

begin
  Result:=1;
  isNameSpace:=Intf is TIDLNamespaceDefinition;
  ParentName:='';

  // Pascal interface and ancestor
  aPasIntfName:=GetPasIntfName(Intf);
  FGeneratingInterface:=True;
  try
    Decl:=aPasIntfName+' = interface';
    if (not IsNamespace) then
      if Assigned(iIntf.ParentInterface) then
        ParentName:=GetPasIntfName(iIntf.ParentInterface as TIDLInterfaceDefinition)
      else
        ParentName:=GetPascalTypeName(Intf.ParentName);
    if ParentName='' then
      ParentName:=PasInterfacePrefix+'Object'+PasInterfaceSuffix;
    if ParentName<>'' then
      Decl:=Decl+'('+ParentName+')';
    AddLn(Decl);

    Indent;

    // GUID
    AddLn('['''+ComputeGUID(Decl,aMemberList)+''']');

    // private members
    WritePrivateGetters(Intf,aMemberList);
    WritePrivateSetters(Intf,aMemberList);

    // public members
    WriteMethodDefs(Intf,aMemberList);
    WriteProperties(Intf,aMemberList);

    Undent;
    AddLn('end;');
    AddLn('');

  finally
    FGeneratingInterface:=False;
  end;

end;

function TWebIDLToPasWasmJob.WritePrivateGetters(aParent: TIDLStructuredDefinition;
  aList: TIDLDefinitionList): Integer;
var
  D: TIDLDefinition;
begin
  Result:=Inherited WritePrivateGetters(aParent,aList);
  for D in aList do
    if D is TIDLAttributeDefinition then
      if ConvertDef(D) then
        if WritePrivateGetter(aParent,TIDLAttributeDefinition(D)) then
          inc(Result);
end;

function TWebIDLToPasWasmJob.WritePrivateSetters(
  aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer;
var
  D: TIDLDefinition;
begin
  Result:=Inherited WritePrivateSetters(aParent,aList);
  for D in aList do
    if D is TIDLAttributeDefinition then
      if ConvertDef(D) then
        if WritePrivateSetter(aParent,TIDLAttributeDefinition(D)) then
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
    if D is TIDLAttributeDefinition then
      if ConvertDef(D) then
        if WriteProperty(aParent,TIDLAttributeDefinition(D)) then
          inc(Result);
end;

function TWebIDLToPasWasmJob.WriteUtilityMethods(Intf: TIDLStructuredDefinition
  ): Integer;
var
  aPasIntfName: TIDLString;
begin
  Result:=0;
  aPasIntfName:=GetPasIntfName(Intf);
  AddLn('class function Cast(const Intf: IJSObject): '+aPasIntfName+';');
end;

function TWebIDLToPasWasmJob.WriteMapLikeProperties(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer;
begin
  AddLn('property size : LongInt read _Getsize;');
  Result:=1;
end;

function TWebIDLToPasWasmJob.WriteMapLikePrivateReadOnlyFields(aParent: TIDLDefinition; aMap: TIDLMapLikeDefinition): Integer;
begin
  Result:=0;
end;

function TWebIDLToPasWasmJob.WriteMapLikePrivateGetters(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition): Integer;

begin
  Result:=1;
  AddLn('function _Getsize : LongInt;');
end;

function TWebIDLToPasWasmJob.WriteEnumDef(aDef: TIDLEnumDefinition): Boolean;
begin
  Result:=True;
  AddLn(GetPasName(aDef)+' = UnicodeString;');
end;

function TWebIDLToPasWasmJob.WriteDictionaryField(
  aDict: TIDLDictionaryDefinition; aField: TIDLDictionaryMemberDefinition
  ): Boolean;
var
  N, TN: TIDLString;
begin
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
    AddLn(GetPasName(D)+' = IJSObject; //'+JOB_JSValueTypeNames[jjvkDictionary]+';')
  else
    begin
    if ((D is TIDLInterfaceDefinition) or (D is TIDLNamespaceDefinition)) then
      AddLn(GetPasIntfName(D)+' = interface;');
    Result:=inherited WriteForwardClassDef(D);
    end;
end;

function TWebIDLToPasWasmJob.GetInvokeNameFromAliasName(const aTypeName : TIDLString; aType : TIDLDefinition) : string;
// Heuristic to determine what the base type of an aliased type is.
// We could enhance this by having support for aType=aAlias,InvokeType:InvokeClass
var
  aLower : String;
begin
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

function TWebIDLToPasWasmJob.GetInvokeNameFromTypeName(const aTypeName : TIDLString; aType : TIDLDefinition):  TIDLString;

var
  aPascaltypeName : String;
  NT : TPascalNativeType;

begin
  Result:='';
  NT:=GetPasNativeTypeAndName(aType,aPascaltypeName);
  Result:=GetInvokeNameFromNativeType(NT);
  if Result<>'' then
    exit;
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
    else
      Result:='InvokeJSObjectResult';
    if Result='' then
      Raise EConvertError.CreateFmt('Unable to determine invoke name from alias type %s',[aTypeName]);
    end
  else if aType is TIDLEnumDefinition then
    Result:='InvokeJSUnicodeStringResult'
  else
    Result:='InvokeJSObjectResult';

end;

function TWebIDLToPasWasmJob.GetInvokeClassNameFromTypeAlias(aName : TIDLString; aDef : TIDLDefinition): TIDLString;

// Heuristic to determine what the base type of an aliased type is.
// We could enhance this by having support for aType=aAlias,InvokeType:InvokeClass
var
  aLower : String;
begin
  aLower:=LowerCase(aName);
  if Pos('array',aLower)>0 then
    Result:='TJSArray'
  else if Pos(PasInterfacePrefix,aLower)=1 then
    Result:='TJSObject'
  else
    Result:='';
end;

function TWebIDLToPasWasmJob.GetInvokeClassName(aResultDef : TIDLDefinition; aName : TIDLString; aDef : TIDLFunctionDefinition = Nil): TIDLString;

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
//  ResolvedReturnTypeName
  Result:='';
  if aResultDef is TIDLSequenceTypeDefDefinition then
    Result:=ClassPrefix+'Array'+ClassSuffix
  else if aResultDef is TIDLPromiseTypeDefDefinition then
    Result:=ClassPrefix+'Promise'+ClassSuffix
  else if aResultDef is TIDLInterfaceDefinition then
    Result:=GetPasName(aResultDef)
  else if aResultDef is TIDLDictionaryDefinition then
    Result:='TJSObject'
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

function TWebIDLToPasWasmJob.GetMethodInfo(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition; out FuncName,ReturnTypeName,ResolvedReturnTypeName,InvokeName,InvokeClassName : TIDLString): TIDLDefinition;

var
  RNT : TPascalNativeType;

begin
  Result:=GetResolvedType(aDef.ReturnType,RNT,ReturnTypeName,ResolvedReturnTypeName);
  InvokeName:='';
  InvokeClassName:='';
  if (foConstructor in aDef.Options) then
    begin
    FuncName:='New';
    InvokeName:= 'InvokeJSObjectResult';
    ResolvedReturnTypeName:=aParent.Name;
    ReturnTypeName:=GetPasName(aParent);
    InvokeClassName:=ReturnTypeName;
    exit(Nil);
    end
  else
    begin

    FuncName:=GetPasName(aDef);
    InvokeName:=GetInvokeNameFromTypeName(ResolvedReturnTypeName,Result);
    case InvokeName of
    'InvokeJSNoResult' :
       begin
       ReturnTypeName:='';
       ResolvedReturnTypeName:='';
       end;
    'InvokeJSObjectResult':
      InvokeClassName:=GetInvokeClassName(Result,ResolvedReturnTypeName,aDef);
    else
       ;
    end;
    end;
end;

function TWebIDLToPasWasmJob.GetFunctionSignature(aDef: TIDLFunctionDefinition; aReturnDef: TIDLDefinition; aFuncname,
  aReturnTypeName, aSuffix: TIDLString; ArgDefList: TIDLDefinitionList; out ProcKind: TIDLString): String;

var
  Args : String;

begin
  Result:='';
  Args:=GetArguments(ArgDefList,False);
  if (foConstructor in aDef.Options) then
    begin
    ProcKind:='class function';
    Result:='Create'+Args+' : '+aReturnTypeName;
    end
  else if (aReturnTypeName='') then
    begin
    ProcKind:='procedure';
    Result:=aFuncName+Args;
    end
  else
    begin
    ProcKind:='function';
    Result:=aFuncName+Args+': '+aReturnTypeName;
    end;
  Result:=Result+aSuffix+';';
  if aReturnDef is TIDLPromiseTypeDefDefinition then
    Result:=Result+' // Promise<'+TIDLPromiseTypeDefDefinition(aReturnDef).ReturnType.TypeName+'>';
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
  FuncName, Suff, Args, ProcKind, Sig, aClassName, InvokeName,
  InvokeCode,  LocalName, WrapperFn,
  ArgName, ArgTypeName, ReturnTypeName, ResolvedReturnTypeName,
  InvokeClassName, ArgResolvedTypeName: TIDLString;
  Overloads: TFPObjectList;
  I: Integer;
  ArgDefList: TIDLDefinitionList;
  CurDef, ArgType, ReturnDef: TIDLDefinition;
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
  ReturnDef:=GetMethodInfo(aParent,aDef,FuncName,ReturnTypeName,ResolvedReturnTypeName,InvokeName,InvokeClassName);
  aClassName:=GetPasName(aParent);

  Overloads:=GetOverloads(ADef);
  try
    Suff:=GetFunctionSuffix(aDef,Overloads);
    For I:=0 to Overloads.Count-1 do
      begin
      ArgDefList:=TIDLDefinitionList(Overloads[i]);
      Sig:=GetFunctionSignature(aDef,ReturnDef,FuncName,ReturnTypeName,Suff,ArgDefList,ProcKind);

      ArgNames:=TStringList.Create;
      try
        for CurDef in ArgDefList do
          ArgNames.Add(GetArgName(ArgDef));

        AddLn(ProcKind+' '+aClassName+'.'+Sig);

        InvokeCode:='';
        if ReturnTypeName<>'' then
          InvokeCode:='Result:=';
        if foConstructor in aDef.Options then
          InvokeCode:=InvokeCode+'Nil; // ';

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
        Args:=',['+Args+']';

        if foConstructor in aDef.Options then
          InvokeCode:=InvokeCode+InvokeName+'('''+ResolvedReturnTypeName+''''+Args
        else
          InvokeCode:=InvokeCode+InvokeName+'('''+aDef.Name+''''+Args;
        if InvokeClassName<>'' then
          InvokeCode:=InvokeCode+','+InvokeClassName+') as '+ReturnTypeName
        else
          InvokeCode:=InvokeCode+')';

        if Length(VarSection)>0 then
          begin
          AddLn('var');
          Indent;
          AddLn(VarSection);
          undent;
          end;
        AddLn('begin');
        Indent;
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

function TWebIDLToPasWasmJob.GetFunctionSuffix(aDef: TIDLFunctionDefinition; Overloads : TFPObjectList): String;

begin
  Result:='';
  if (aDef.Arguments.Count>0)
      and aDef.Argument[aDef.Arguments.Count-1].HasEllipsis then
    Result:='{; ToDo:varargs}';
  if Overloads.Count>1 then
    Result:=Result+'; overload';
end;

function TWebIDLToPasWasmJob.WriteFunctionDefinition(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition): Boolean;

Var
  Data: TPasDataWasmJob;
  FuncName, Suff, ProcKind, Sig, InvokeName,
  ReturnTypeName, ResolvedReturnTypeName, InvokeClassName : TIDLString;
  Overloads: TFPObjectList;
  I: Integer;
  ArgDefList: TIDLDefinitionList;
  ReturnDef: TIDLDefinition;
  // ArgDef: TIDLArgumentDefinition absolute CurDef;
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
  ReturnDef:=GetMethodInfo(aParent,aDef,FuncName,ReturnTypeName,ResolvedReturnTypeName,InvokeName,InvokeClassName);
  Overloads:=GetOverloads(ADef);
  try
    Suff:=GetFunctionSuffix(aDef,Overloads);
    For I:=0 to Overloads.Count-1 do
      begin
      ArgDefList:=TIDLDefinitionList(Overloads[i]);
      Sig:=GetFunctionSignature(aDef,ReturnDef,FuncName,ReturnTypeName,Suff,ArgDefList,ProcKind);
      if not FGeneratingInterface then
        Sig:=Sig+' overload;';
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
  Result:='';
  if Pos('IJS',ArgTypeName)=1 then
    Result:='GetObject('+GetPasName(aDef)+') as '+ArgTypeName
  else if Pos('Array',ArgTypeName)>0 then
    Result:='GetObject('+GetPasName(aDef)+') as IJSArray';
end;

function TWebIDLToPasWasmJob.GetKnownResultAllocator(aDef : TIDLTypeDefinition; ArgTypeName, ArgResolvedTypename : String) : string;

begin
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
  case RNT of
  ntNone,
  ntUnknown:
    begin
    ReturnTypeName:='';
    ResolvedReturnTypeName:='';
    end;
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
  end;
end;

function TWebIDLToPasWasmJob.GetReadPropertyCall(aNativeType: TPascalnativeType; AttrResolvedTypeName, aNativeTypeName: TIDLString;
  aMemberName: String; aType: TIDLDefinition): string;

var
  ObjClassName,
  ReadFuncName : string;

begin
  Result:='';
  Case aNativeType of


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
  ntMethod:  Result:='('+AttrResolvedTypeName+'(ReadJSPropertyMethod('''+aMemberName+''')))';
  else
    if AttrResolvedTypeName = 'TJOB_JSValue' then
      ReadFuncName:='ReadJSPropertyValue'
    else if aType is TIDLSequenceTypeDefDefinition then
      ObjClassName:=ClassPrefix+'Array'+ClassSuffix
    else if aType is TIDLPromiseTypeDefDefinition then
      ObjClassName:=ClassPrefix+'Promise'+ClassSuffix
    else
      begin
      ObjClassName:=GetPasName(aType);
      if (ObjClassName='') or (Pos(PasInterfacePrefix,ObjClassName)=1) then
        ObjClassName:=IntfToPasClassName(ObjClassName);
      end;
    Result:='ReadJSPropertyObject('''+aMemberName+''','+ObjClassName+') as '+aNativeTypeName;
  end;

  if Result='' then
    Result:=ReadFuncName+'('''+aMemberName+''')';
end;


function TWebIDLToPasWasmJob.GetPrivateGetterInfo(Attr: TIDLAttributeDefinition;out  aNativeType : TPascalNativeType; out AttrTypeName, AttrResolvedTypeName, FuncName: TIDLString) : TIDLDefinition;

begin
  Result:=nil;
  if Attr.AttributeType=nil then
    exit;
  FuncName:=GetterPrefix+GetPasName(Attr);
  Result:=GetResolvedType(Attr.AttributeType,aNativeType, AttrTypeName,AttrResolvedTypeName);
  if Result is TIDLInterfaceDefinition then
    AttrTypeName:=GetPasIntfName(Result)
  else if Result is TIDLFunctionDefinition then
    // exit // not supported yet
  else if Result is TIDLEnumDefinition then
    AttrResolvedTypeName:='UnicodeString';
end;

procedure TWebIDLToPasWasmJob.WritePrivateGetterImplementation(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition);

var
  FuncName, aClassName, Call,
  AttrTypeName, AttrResolvedTypeName: TIDLString;
  AttrType: TIDLDefinition;
  aNT : TPascalNativeType;

begin
  aClassName:=GetPasName(aParent);
  // case
  // stringifier ;
  // is equivalent to toString : DOMString
  // no n
  if (Attr.Name='') and (aoStringifier in Attr.Options) then
    Exit;
  if Attr.AttributeType=nil then
    Exit;

  AttrType:=GetPrivateGetterInfo(Attr,aNT,AttrTypeName,AttrResolvedTypeName,FuncName);
  Call:=GetReadPropertyCall(aNT,AttrResolvedTypeName, AttrTypeName, Attr.Name, AttrType);
  Addln('function '+aClassName+'.'+FuncName+': '+AttrTypeName+';');
  Addln('begin');
  Addln('  Result:='+Call+';');
  Addln('end;');
end;

function TWebIDLToPasWasmJob.WritePrivateGetter(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition): boolean;

var
  FuncName,
  AttrTypeName, AttrResolvedTypeName: TIDLString;
  AttrType: TIDLDefinition;
  aNT : TPascalNativeType;

begin
  Result:=true;
  if (Attr.Name='') and (aoStringifier in Attr.Options) then
    Exit;
  if Attr.AttributeType=nil then
    exit;
  AttrType:=GetPrivateGetterInfo(Attr,ant,AttrTypeName,AttrResolvedTypeName,FuncName);
  AddLn('function '+FuncName+': '+AttrTypeName+'; overload;');
end;

function TWebIDLToPasWasmJob.GetPrivateSetterInfo(Attr: TIDLAttributeDefinition; out aNativeType: TPascalNativeType; out AttrTypeName, AttrResolvedTypeName, FuncName: TIDLString) : TIDLDefinition;

begin
  Result:=nil;
  if (Attr.Name='') and (aoStringifier in Attr.Options) then
    Exit;
  if Attr.AttributeType=nil then
    exit;
  FuncName:=SetterPrefix+GetPasName(Attr);
  Result:=GetResolvedType(Attr.AttributeType,aNativeType,AttrTypeName,AttrResolvedTypeName);
  if Result is TIDLInterfaceDefinition then
    AttrTypeName:=GetPasIntfName(Result)
  else if Result is TIDLFunctionDefinition then
    // exit // not supported yet
  else if Result is TIDLEnumDefinition then
    AttrResolvedTypeName:='UnicodeString';
end;

function TWebIDLToPasWasmJob.GetWritePropertyCall(aNativeType : TPascalnativeType; AttrResolvedTypeName,aNativeTypeName : TIDLString;  aMemberName: String; aType :TIDLDefinition) : string;

var
  WriteFuncName : String;

begin
  Result:='';
  case aNativeType of
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
    if AttrResolvedTypeName='TJOB_JSValue' then
      WriteFuncName:='WriteJSPropertyValue'
    else
      WriteFuncName:='WriteJSPropertyObject';
  end;
  if Result='' then
    Result:=Format('%s(''%s'',aValue)',[WriteFuncName,aMemberName]);
end;

procedure TWebIDLToPasWasmJob.WritePrivateSetterImplementation(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition);

var
  FuncName, aClassName, Call,
  AttrTypeName, AttrResolvedTypeName : TIDLString;
  AttrType: TIDLDefinition;
  aNT: TPascalNativeType;

begin
  if aoReadOnly in Attr.Options then
    exit;
  if Attr.AttributeType=nil then
    exit;
  aClassName:=GetPasName(aParent);
  AttrType:=GetPrivateSetterInfo(Attr,aNT,AttrTypeName,AttrResolvedTypeName,FuncName);
  Call:=GetWritePropertyCall(aNt,AttrResolvedTypeName, AttrTypeName, Attr.Name, AttrType);

  Addln('procedure %s.%s(const aValue : %s);',[aClassName,FuncName,AttrTypeName]);
  Addln('begin');
  indent;
  Addln(Call+';');
  undent;
  Addln('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikePrivateSetterImplementation(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition
  );


begin
  // None
end;

procedure TWebIDLToPasWasmJob.WriteMapLikePrivateGetterImplementation(aParent: TIDLStructuredDefinition; aMap: TIDLMapLikeDefinition
  );
var
  call, aClassName : string;

begin
  aClassName:=GetPasName(aParent);
  Call:=GetReadPropertyCall(ntLongint,'Integer', 'LongInt', 'size', Nil);
  Addln('function '+aClassName+'._Getsize: LongInt;');
  Addln('begin');
  Addln('  Result:='+Call+';');
  Addln('end;');
end;


function TWebIDLToPasWasmJob.WritePrivateSetter(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition): boolean;

var
  FuncName, AttrTypeName, AttrResolvedTypeName: TIDLString;
  aNT : TPascalNativeType;

begin
  if aoReadOnly in Attr.Options then
    exit(false);
  if Attr.AttributeType=nil then
    exit;
  GetPrivateSetterInfo(Attr,aNT,AttrTypeName,AttrResolvedTypeName,FuncName);
  AddLn('procedure '+FuncName+'(const aValue: '+AttrTypeName+');overload;');
end;

function TWebIDLToPasWasmJob.WriteProperty(aParent: TIDLDefinition;
  Attr: TIDLAttributeDefinition): boolean;
var
  PropName, Code, AttrTypeName, AttrResolvedTypeName: TIDLString;
  AttrType: TIDLDefinition;
  ANT : TPascalNativeType;

begin
  if aParent=nil then ;
  if (Attr.AttributeType=nil) then
    begin
    if not (aoStringifier in Attr.Options) then
      DoLog('Note: skipping field "'+Attr.Name+'" without type at '+GetDefPos(Attr));
    exit;
    end;
  PropName:=GetPasName(Attr);
  AttrType:=GetResolvedType(Attr.AttributeType,ANT,AttrTypeName,AttrResolvedTypeName);
  if AttrType is TIDLInterfaceDefinition then
    AttrTypeName:=GetPasIntfName(AttrType);
  Code:='property '+PropName+': '+AttrTypeName+' read '+GetterPrefix+PropName;
  if not (aoReadOnly in Attr.Options) then
    Code:=Code+' write '+SetterPrefix+PropName;
  Code:=Code+';';
  if AttrType is TIDLFunctionDefinition then
    Code:='// '+Code;
  AddLn(Code);
  Result:=true;
end;

function TWebIDLToPasWasmJob.WriteRecordDef(aDef: TIDLRecordDefinition
  ): Boolean;
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

begin
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
  AddLn(PasVarName+': '+GetPasName(iDef)+';');
end;

procedure TWebIDLToPasWasmJob.WriteEnumImplementation(aDef : TIDLEnumDefinition);

begin
end;

procedure TWebIDLToPasWasmJob.WriteDictionaryImplemention(aDef : TIDLDictionaryDefinition);

begin
end;

procedure TWebIDLToPasWasmJob.WritePrivateGetterImplementations(aDef : TIDLStructuredDefinition; ML : TIDLDefinitionList);

var
  D : TIDLDefinition;
  AD : TIDLAttributeDefinition absolute D;
  MD : TIDLMapLikeDefinition absolute D;

begin
  for D in ML do
    if ConvertDef(D) then
      begin
      if D is TIDLAttributeDefinition then
        WritePrivateGetterImplementation(aDef,AD)
      else if D is TIDLMapLikeDefinition then
        WriteMapLikePrivateGetterImplementation(aDef,MD);
      end;
end;

procedure TWebIDLToPasWasmJob.WritePrivateSetterImplementations(aDef : TIDLStructuredDefinition; ML : TIDLDefinitionList);

var
  D : TIDLDefinition;
  AD : TIDLAttributeDefinition absolute D;
  MD : TIDLMapLikeDefinition absolute D;

begin
  for D in ML do
    if ConvertDef(D) then
      begin
      if D is TIDLAttributeDefinition then
        WritePrivateSetterImplementation(aDef,AD)
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

procedure TWebIDLToPasWasmJob.WriteMapLikeGetFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  D,aResolvedKeyTypeName,aResolvedValueTypeName: String;
  func,InvokeClass,aClassName : string;
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
  if VNT=ntObject then
    AddLn('Result:='+Func+'(''get'',[key],'+InvokeClass+') as '+aResolvedValueTypeName+';')
  else
    AddLn('Result:='+Func+'(''get'',[key]);');
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
  AddLn('Result:=InvokeJSBooleanResult(''has'',[key]);');
  Undent;
  AddLn('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeEntriesFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  aClassName : string;

begin
  aClassName:=GetPasName(aDef);
  AddLn('function %s.entries : IJSIterator;',[aClassName]);
  AddLn('begin');
  Indent;
  AddLn('Result:=InvokeJSObjectResult(''entries'',[],TJSIterator) as IJSIterator;');
  Undent;
  AddLn('end;');
end;


procedure TWebIDLToPasWasmJob.WriteMapLikeKeysFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  aClassName : string;

begin
  aClassName:=GetPasName(aDef);
  AddLn('function %s.keys : IJSIterator;',[aClassName]);
  AddLn('begin');
  Indent;
  AddLn('Result:=InvokeJSObjectResult(''keys'',[],TJSIterator) as IJSIterator;');
  Undent;
  AddLn('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeValuesFunctionImplementation(aDef : TIDLStructuredDefinition; ML : TIDLMapLikeDefinition);

var
  aClassName : string;

begin
  aClassName:=GetPasName(aDef);
  AddLn('function %s.values : IJSIterator;',[aClassName]);
  AddLn('begin');
  Indent;
  AddLn('Result:=InvokeJSObjectResult(''values'',[],TJSIterator) as IJSIterator;');
  Undent;
  AddLn('end;');
end;

procedure TWebIDLToPasWasmJob.WriteMapLikeFunctionImplementations(aDef : TIDLStructuredDefinition; MD : TIDLMapLikeDefinition);

Var
  L : TIDLDefinitionList;

begin
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
  finally
    L.Free;
  end;
end;

procedure TWebIDLToPasWasmJob.WriteUtilityMethodImplementations(aDef : TIDLStructuredDefinition; ML : TIDLDefinitionList);

var
  aClassName, aPasIntfName: TIDLString;

begin
  aClassName:=GetPasName(aDef);
  aPasIntfName:=GetPasIntfName(aDef);
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
      AddLn(PasVarName+':='+GetPasName(aDef)+'.JOBCreateGlobal('''+JOBRegisterName+''');');
      end;
    for I:=0 to Context.Definitions.Count-1 do
      begin
      aDef:=Context.Definitions[i];
      if aDef is TIDLNamespaceDefinition then
        if not NSDef.IsPartial and ConvertDef(aDef) then
          begin
          PasVarName:=Context.Definitions[i].Name;
          AddLn(PasVarName+':='+GetPasName(aDef)+'.JOBCreateGlobal('''+PasVarName+''');');
          end;
      end;
    Undent;

    AddLn('finalization');
    Indent;
    for i:=0 to GlobalVars.Count-1 do
      begin
      SplitGlobalVar(GlobalVars[i],PasVarName,JSClassName,JOBRegisterName);
      AddLn(PasVarName+'.Free;');
      end;
    for I:=0 to Context.Definitions.Count-1 do
      begin
      aDef:=Context.Definitions[i];
      if aDef is TIDLNamespaceDefinition then
        if not NSDef.IsPartial and ConvertDef(aDef) then
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

