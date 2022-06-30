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
unit webidltopas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, WebIDLParser, WebIDLScanner, WebIDLDefs, pascodegen;

Type

  { TBaseWebIDLToPas }

  { TPasData }

  TPasData = Class(TObject)
  private
    FPasName: String;
  Public
    IDL: TIDLBaseObject;
    Line, Column: integer;
    SrcFile: string;
    Resolved: TIDLDefinition;
    Constructor Create(APasName: String; D: TIDLBaseObject);
    Property PasName: String read FPasName;
  end;
  TPasDataClass = class of TPasData;

  TBaseConversionOption = (
    coAddOptionsToHeader,
    coExpandUnionTypeArgs,
    coDictionaryAsClass
    );
  TBaseConversionOptions = Set of TBaseConversionOption;

const
  BaseConversionOptionName: array[TBaseConversionOption] of string = (
    'AddOptionsToHeader',
    'ExpandUnionTypeArgs',
    'DictionaryAsClass'
    );

type
  TBaseWebIDLToPas = Class(TPascalCodeGenerator)
  private
    FAutoTypes: TStrings;
    FBaseOptions: TBaseConversionOptions;
    FClassPrefix: String;
    FClassSuffix: String;
    FContext: TWebIDLContext;
    FDictionaryClassParent: String;
    FFieldPrefix: String;
    FFuncTypePrefix: String;
    FGetterPrefix: String;
    FIncludeImplementationCode: TStrings;
    FIncludeInterfaceCode: TStrings;
    FInputFileName: String;
    FGlobalDefs: TFPHashList;
    FOutputFileName: String;
    FPasDataClass: TPasDataClass;
    FPasNameList: TFPObjectList; // list TPasData
    FSetterPrefix: String;
    FTypeAliases: TStrings; // user defined type maping name to name
    FVerbose: Boolean;
    FWebIDLVersion: TWebIDLVersion;
    procedure SetIncludeImplementationCode(AValue: TStrings);
    procedure SetIncludeInterfaceCode(AValue: TStrings);
    procedure SetTypeAliases(AValue: TStrings);
  Protected
    procedure AddOptionsToHeader;
    Procedure Parse; virtual;
    Procedure WritePascal; virtual;
    function CreateParser(aContext: TWebIDLContext; S: TWebIDLScanner): TWebIDLParser; virtual;
    function CreateScanner(S: TStream): TWebIDLScanner; virtual;
    Function CreateContext: TWebIDLContext; virtual;
    // Auxiliary routines
    procedure GetOptions(L: TStrings; Full: boolean); virtual;
    procedure ProcessDefinitions; virtual;
    function CreatePasName(aName: String; D: TIDLBaseObject): TPasData; virtual;
    procedure AllocatePasNames(aList: TIDLDefinitionList; ParentName: String=''); virtual;
    function AllocatePasName(D: TIDLDefinition; ParentName: String=''): TPasData; virtual;
    procedure AddJSIdentifier(D: TIDLDefinition); virtual;
    procedure ResolveTypeDefs(aList: TIDLDefinitionList); virtual;
    procedure ResolveTypeDef(D: TIDLDefinition); virtual;
    function FindGlobalDef(const aName: UTF8String): TIDLDefinition; virtual;
    function GetDefPos(Def: TIDLBaseObject; WithoutFile: boolean = false): string; virtual;
    function GetPasDataPos(D: TPasData; WithoutFile: boolean = false): string; virtual;
    procedure EnsureUniqueNames(ML: TIDLDefinitionList); virtual;
    function AddSequenceDef(ST: TIDLSequenceTypeDefDefinition): Boolean; virtual;
    function GetName(ADef: TIDLDefinition): String; virtual;
    function GetTypeName(Const aTypeName: String; ForTypeDef: Boolean=False): String; overload; virtual;
    function GetTypeName(aTypeDef: TIDLTypeDefDefinition; ForTypeDef: Boolean=False): String; overload; virtual;
    function GetInterfaceDefHead(Intf: TIDLInterfaceDefinition): String; virtual;
    function GetDictionaryDefHead(const CurClassName: string; Dict: TIDLDictionaryDefinition): String; virtual;
    function CheckUnionTypeDefinition(D: TIDLDefinition): TIDLUnionTypeDefDefinition; virtual;
    procedure AddArgumentToOverloads(aList: TFPObjectlist; AName, ATypeName: String; PosEl: TIDLBaseObject); overload; virtual;
    procedure AddArgumentToOverloads(aList: TFPObjectlist; aDef: TIDLArgumentDefinition); overload; virtual;
    procedure AddUnionOverloads(aList: TFPObjectlist; AName: String;  UT: TIDLUnionTypeDefDefinition); virtual;
    procedure AddOverloads(aList: TFPObjectlist; adef: TIDLFunctionDefinition; aIdx: Integer); virtual;
    function CloneNonPartialArgumentList(aList: TFPObjectlist; ADest: TFPObjectlist= Nil; AsPartial: Boolean=True): integer; virtual;
    function GetOverloads(aDef: TIDLFunctionDefinition): TFPObjectlist; virtual;
    function GetArguments(aList: TIDLDefinitionList; ForceBrackets: Boolean): String; virtual;
    function HaveConsts(aList: TIDLDefinitionList): Boolean; virtual;
    // Code generation routines. Return the number of actually written defs.
    function WriteFunctionImplicitTypes(aList: TIDLDefinitionList): Integer; virtual;
    function WriteAttributeImplicitTypes(aList: TIDLDefinitionList): Integer; virtual;
    function WriteOtherImplicitTypes(Intf: TIDLInterfaceDefinition; aMemberList: TIDLDefinitionList): Integer; virtual;
    function WriteDictionaryMemberImplicitTypes(aDict: TIDLDictionaryDefinition; aList: TIDLDefinitionList): Integer; virtual;
    function WriteCallBackDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteDictionaryDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteForwardClassDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteInterfaceDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteMethodDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteUtilityMethods(Intf: TIDLInterfaceDefinition): Integer; virtual;
    function WriteTypeDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteEnumDefs(aList: TIDLDefinitionList): Integer; virtual;
    function WriteConsts(aList: TIDLDefinitionList): Integer; virtual;
    function WriteProperties(aList: TIDLDefinitionList): Integer; virtual;
    function WritePlainFields(aList: TIDLDefinitionList): Integer; virtual;
    function WriteDictionaryFields(aList: TIDLDefinitionList): Integer; virtual;
    function WritePrivateReadOnlyFields(aList: TIDLDefinitionList): Integer; virtual;
    function WritePrivateGetters(aList: TIDLDefinitionList): Integer; virtual;
    function WritePrivateSetters(aList: TIDLDefinitionList): Integer; virtual;
    // Definitions. Return true if a definition was written.
    function WriteForwardClassDef(D: TIDLStructuredDefinition): Boolean; virtual;
    function WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition): Boolean; virtual;
    function WriteFunctionDefinition(aDef: TIDLFunctionDefinition): Boolean; virtual;
    function WriteTypeDef(aDef: TIDLTypeDefDefinition): Boolean; virtual;
    function WriteRecordDef(aDef: TIDLRecordDefinition): Boolean; virtual;
    function WriteEnumDef(aDef: TIDLEnumDefinition): Boolean; virtual;
    function WriteDictionaryField(aField: TIDLDictionaryMemberDefinition): Boolean; virtual;
    function WriteField(aAttr: TIDLAttributeDefinition): Boolean; virtual;
    function WriteConst(aConst: TIDLConstDefinition): Boolean ; virtual;
    function WriteInterfaceDef(Intf: TIDLInterfaceDefinition): Boolean; virtual;
    function WriteDictionaryDef(aDict: TIDLDictionaryDefinition): Boolean; virtual;
    // Additional
    procedure WriteAliasTypeDef(aDef: TIDLTypeDefDefinition); virtual;
    procedure WritePromiseDef(aDef: TIDLPromiseTypeDefDefinition); virtual;
    procedure WriteSequenceDef(aDef: TIDLSequenceTypeDefDefinition); virtual;
    procedure WriteUnionDef(aDef: TIDLUnionTypeDefDefinition); virtual;
    // Extra interface/Implementation code.
    procedure WriteImplementation; virtual;
    procedure WriteIncludeInterfaceCode; virtual;
    Property Context: TWebIDLContext Read FContext;
  Public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure WriteOptions; virtual;
  Public
    Property InputFileName: String Read FInputFileName Write FInputFileName;
    Property OutputFileName: String Read FOutputFileName Write FOutputFileName;
    Property Verbose: Boolean Read FVerbose Write FVerbose;
    Property FieldPrefix: String Read FFieldPrefix Write FFieldPrefix;
    Property ClassPrefix: String Read FClassPrefix Write FClassPrefix;
    Property ClassSuffix: String Read FClassSuffix Write FClassSuffix;
    Property GetterPrefix: String read FGetterPrefix write FGetterPrefix;
    Property SetterPrefix: String read FSetterPrefix write FSetterPrefix;
    Property FuncTypePrefix: String read FFuncTypePrefix write FFuncTypePrefix;
    Property WebIDLVersion: TWebIDLVersion Read FWebIDLVersion Write FWebIDLVersion;
    Property TypeAliases: TStrings Read FTypeAliases Write SetTypeAliases;
    Property IncludeInterfaceCode: TStrings Read FIncludeInterfaceCode Write SetIncludeInterfaceCode;
    Property IncludeImplementationCode: TStrings Read FIncludeImplementationCode Write SetIncludeImplementationCode;
    Property DictionaryClassParent: String Read FDictionaryClassParent Write FDictionaryClassParent;
    Property BaseOptions: TBaseConversionOptions read FBaseOptions write FBaseOptions;
    Property PasDataClass: TPasDataClass read FPasDataClass write FPasDataClass;
  end;

function BaseConversionOptionsToStr(Opts: TBaseConversionOptions): string;

implementation

uses typinfo;

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
  ms:=TMemoryStream.Create;
  try
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

function TBaseWebIDLToPas.GetName(ADef: TIDLDefinition): String;

begin
  If Assigned(ADef) and (TObject(ADef.Data) is TPasData) then
    Result:=TPasData(ADef.Data).PasName
  else
    Result:=ADef.Name;
end;

function TBaseWebIDLToPas.HaveConsts(aList: TIDLDefinitionList): Boolean;

Var
  D: TIDLDefinition;

begin
  Result:=False;
  For D in aList do
    if D is TIDLConstDefinition then
      Exit(True);
end;

function TBaseWebIDLToPas.WriteFunctionImplicitTypes(aList: TIDLDefinitionList): Integer;

Var
  D,D2,D3: TIDLDefinition;
  FD: TIDLFunctionDefinition absolute D;
  DA: TIDLArgumentDefinition absolute D2;
  UT: TIDLUnionTypeDefDefinition;

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

function TBaseWebIDLToPas.WriteAttributeImplicitTypes(aList: TIDLDefinitionList
  ): Integer;
Var
  D: TIDLDefinition;
  FA: TIDLAttributeDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if D is TIDLAttributeDefinition then
      if (FA.AttributeType is TIDLSequenceTypeDefDefinition) then
        if AddSequenceDef(FA.AttributeType as TIDLSequenceTypeDefDefinition) then
          Inc(Result);
end;

function TBaseWebIDLToPas.WriteOtherImplicitTypes(
  Intf: TIDLInterfaceDefinition; aMemberList: TIDLDefinitionList): Integer;
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
      if (FD.MemberType is TIDLSequenceTypeDefDefinition) then
        if AddSequenceDef(FD.MemberType as TIDLSequenceTypeDefDefinition) then
          Inc(Result);
end;

function TBaseWebIDLToPas.WritePrivateReadOnlyFields(aList: TIDLDefinitionList): Integer;
begin
  Result:=0;
  if aList=nil then ;
end;

function TBaseWebIDLToPas.WritePrivateGetters(aList: TIDLDefinitionList
  ): Integer;
begin
  Result:=0;
  if aList=nil then ;
end;

function TBaseWebIDLToPas.WritePrivateSetters(aList: TIDLDefinitionList
  ): Integer;
begin
  Result:=0;
  if aList=nil then ;
end;

function TBaseWebIDLToPas.WriteProperties(aList: TIDLDefinitionList): Integer;
begin
  Result:=0;
  if aList=nil then ;
end;

function TBaseWebIDLToPas.WriteConst(aConst: TIDLConstDefinition): Boolean;
var
  S: UTF8String;
begin
  Result:=true;
  S:=aConst.Value;
  if aConst.ConstType=ctInteger then
    S:=StringReplace(S,'0x','$',[]);
  Addln('%s = %s;',[GetName(aConst),S])
end;

function TBaseWebIDLToPas.WriteConsts(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;

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

function TBaseWebIDLToPas.WritePlainFields(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  A: TIDLAttributeDefinition absolute D;

begin
  EnsureSection(csDeclaration);
  Result:=0;
  For D in aList do
    if D is TIDLAttributeDefinition then
      if Not (aoReadOnly in A.Options) then
        if WriteField(A) then
          Inc(Result);
end;

function TBaseWebIDLToPas.WriteDictionaryField(
  aField: TIDLDictionaryMemberDefinition): Boolean;

Var
  Def,N,TN: String;

begin
  Result:=True;
  N:=GetName(aField);
  TN:=GetTypeName(aField.MemberType);
  if TN='record' then
    TN:='TJSObject';
  if SameText(N,TN) then
    N:='_'+N;
  Def:=Format('%s: %s;',[N,TN]);
  if (N<>aField.Name) then
    Def:=Def+Format('external name ''%s'';',[aField.Name]);
  AddLn(Def);
end;

function TBaseWebIDLToPas.WriteDictionaryFields(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  M: TIDLDictionaryMemberDefinition absolute D;

begin
  Indent;
  Result:=0;
  For D in aList do
    if D is TIDLDictionaryMemberDefinition then
      if WriteDictionaryField(M) then
        Inc(Result);
  Undent;
end;

function TBaseWebIDLToPas.WriteMethodDefs(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  FD: TIDLFunctionDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if D is TIDLFunctionDefinition then
      if Not (foCallBack in FD.Options) then
         if WriteFunctionDefinition(FD) then
           Inc(Result);
end;

function TBaseWebIDLToPas.WriteUtilityMethods(Intf: TIDLInterfaceDefinition
  ): Integer;
begin
  Result:=0;
  if Intf=nil then ;
end;

function TBaseWebIDLToPas.AddSequenceDef(ST: TIDLSequenceTypeDefDefinition
  ): Boolean;

var
  TN: String;
begin
  TN:=GetTypeName(ST);
  Result:=FAutoTypes.IndexOf(TN)=-1;
  if Result then
    begin
    FAutoTypes.Add(TN);
    DoLog('Automatically adding %s sequence definition for %s.',[TN,GetDefPos(ST)]);
    AddLn('%s = Array of %s;',[TN,GetTypeName(ST.ElementType)]);
    ST.Data:=CreatePasName(TN,ST);
    end;
end;

procedure TBaseWebIDLToPas.EnsureUniqueNames(ML: TIDLDefinitionList);

Var
  L: TFPObjectHashTable;

  Procedure CheckRename(Def: TIDLDefinition);

  var
    I: integer;
    OrigName,BaseName,NewName: String;
    IsOverload: Boolean;
    CurDef , ConflictDef: TIDLDefinition;

  begin
    OrigName:=GetName(Def);
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
            raise EConvertError.Create('Duplicate identifier '+GetDefPos(Def)+' and '+GetDefPos(CurDef)+' (20220620073704)');
          NewName:=KeywordPrefix+BaseName+KeywordSuffix;
          OrigName:=KeywordPrefix+OrigName+KeywordSuffix;
          end;
        end;
    Until (CurDef=Nil);
    if (BaseName<>NewName) then
      begin
      BaseName:=GetName(Def);
      DoLog('Renaming duplicate identifier (%s) %s at %s to %s, other at %s',[Def.ClassName,BaseName,GetDefPos(Def),OrigName,GetDefPos(ConflictDef)]);
      // Original TPasName is in list, will be freed automatically
      Def.Data:=CreatePasName(OrigName,Def);
      end;
    if not IsOverload then
      L.Add(NewName,Def);
  end;

var
  D: TIDLDefinition;

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

function TBaseWebIDLToPas.WriteInterfaceDef(Intf: TIDLInterfaceDefinition): Boolean;

Var
  aClassName: String;
  Decl: String;
  ML: TIDLDefinitionList;

begin
  Result:=True;
  ML:=TIDLDefinitionList.Create(Nil,False);
  try
    Intf.GetFullMemberList(ML);
    EnsureUniqueNames(ML);
    aClassName:=GetName(Intf);
    // class comment
    ClassComment(aClassName);
    // sub types
    WriteFunctionImplicitTypes(ML);
    WriteAttributeImplicitTypes(ML);
    WriteOtherImplicitTypes(Intf,ML);
    // class and ancestor
    Decl:=aClassName+' = '+GetInterfaceDefHead(Intf);
    AddLn(Decl);
    // private section
    AddLn('Private');
    Indent;
    WritePrivateReadOnlyFields(ML);
    WritePrivateGetters(ML);
    WritePrivateSetters(ML);
    Undent;
    // write public section
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
    WriteUtilityMethods(Intf);
    WriteProperties(ML);
    Undent;
    AddLn('end;');
  finally
    ML.Free;
  end;
end;

function TBaseWebIDLToPas.WriteDictionaryDef(aDict: TIDLDictionaryDefinition
  ): Boolean;

Var
  CurClassName, Decl: String;
  DefList: TIDLDefinitionList;
  CurDefs: TIDLDictionaryDefinition;

begin
  Result:=True;
  DefList:=TIDLDefinitionList.Create(Nil,False);
  try
    CurDefs:=aDict;
    While CurDefs<>Nil do
      begin
      CurDefs.GetFullMemberList(DefList);
      CurDefs:=CurDefs.ParentDictionary;
      end;
    CurClassName:=GetName(aDict);
    ClassComment(CurClassName);
    WriteDictionaryMemberImplicitTypes(aDict, DefList);
    // class and ancestor
    Decl:=GetDictionaryDefHead(CurClassName,aDict);
    AddLn(Decl);
    WriteDictionaryFields(DefList);
    AddLn('end;');
  finally
    DefList.Free;
  end;
end;

constructor TBaseWebIDLToPas.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  WebIDLVersion:=v2;
  FieldPrefix:='F';
  ClassPrefix:='T';
  ClassSuffix:='';
  GetterPrefix:='Get';
  SetterPrefix:='Set';
  FuncTypePrefix:='T';
  FTypeAliases:=TStringList.Create;
  FPasNameList:=TFPObjectList.Create(True);
  FPasDataClass:=TPasData;
  FAutoTypes:=TStringList.Create;
  FIncludeInterfaceCode:=TStringList.Create;
  FIncludeImplementationCode:=TStringList.Create;
  FGlobalDefs:=TFPHashList.Create;
end;


destructor TBaseWebIDLToPas.Destroy;
begin
  FreeAndNil(FGlobalDefs);
  FreeAndNil(FIncludeInterfaceCode);
  FreeAndNil(FIncludeImplementationCode);
  FreeAndNil(FAutoTypes);
  FreeAndNil(FTypeAliases);
  FreeAndNil(FPasNameList);
  inherited Destroy;
end;

procedure TBaseWebIDLToPas.WriteImplementation;

Var
  S: String;

begin
  Addln('');
  For S in FIncludeImplementationCode do
    Addln(S);
  Addln('');
end;

function TBaseWebIDLToPas.GetTypeName(aTypeDef: TIDLTypeDefDefinition; ForTypeDef: Boolean = False): String;

begin
  if ATypeDef is TIDLSequenceTypeDefDefinition then
    begin
    if Assigned(aTypeDef.Data) then
      Result:=GetName(aTypeDef)
    else
      begin
      Result:=GetTypeName(TIDLSequenceTypeDefDefinition(aTypeDef).ElementType,ForTypeDef);
      if Result[1]<>'T' then
        Result:='T'+Result;
      Result:=Result+'DynArray';
      end
    end
  else
    Result:=GetTypeName(aTypeDef.TypeName,ForTypeDef);
end;

function TBaseWebIDLToPas.GetInterfaceDefHead(Intf: TIDLInterfaceDefinition
  ): String;
begin
  Result:='class';
  if Intf=nil then ;
end;

function TBaseWebIDLToPas.GetDictionaryDefHead(const CurClassName: string;
  Dict: TIDLDictionaryDefinition): String;
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

function TBaseWebIDLToPas.GetTypeName(const aTypeName: String; ForTypeDef: Boolean
  ): String;

  function GetClassName(const aClassName: string): string;
  begin
    Result:=ClassPrefix+aClassName+ClassSuffix;
  end;

Var
  A: UTF8String;
  D: TIDLDefinition;

begin
  Case aTypeName of
    'boolean': Result:='Boolean';

    'byte': Result:='ShortInt';
    'octet': Result:='Byte';
    'short': Result:='SmallInt';
    'unsigned short': Result:='Word';
    'long': Result:='Integer';
    'unsigned long': Result:='LongWord';
    'long long': Result:='Int64';
    'unsigned long long': Result:='QWord';

    'float',
    'unrestricted float': Result:='Single';
    'double',
    'unrestricted double': Result:='Double';

    'union',
    'any': Result:='JSValue';

    'DOMString',
    'USVString',
    'ByteString': Result:='UnicodeString';

    'record',
    'object': Result:=GetClassName('Object');
    'Error',
    'DOMException': Result:=GetClassName('Error');

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
    'Float64Array': Result:=GetClassName(aTypeName);

    'void': Result:=aTypeName;
  else
    if ForTypeDef then ;

    Result:=aTypeName;
    D:=FContext.FindDefinition(Result);
    if D<>Nil then
      Result:=GetName(D)
    else
      begin
      A:=FTypeAliases.Values[Result];
      If (A<>'') then
        Result:=A;
      end;
  end;
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
    AddLn('%s = Class;',[GetName(D)]);
end;

function TBaseWebIDLToPas.WriteForwardClassDefs(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;

begin
  Result:=0;
  Comment('Forward class definitions');
  For D in aList do
    if D is TIDLInterfaceDefinition then
      if WriteForwardClassDef(D as TIDLInterfaceDefinition) then
        Inc(Result);
  if coDictionaryAsClass in BaseOptions then
    For D in aList do
      if D is TIDLDictionaryDefinition then
        if WriteForwardClassDef(D as TIDLDictionaryDefinition) then
          Inc(Result);
end;

procedure TBaseWebIDLToPas.WriteSequenceDef(aDef: TIDLSequenceTypeDefDefinition);

begin
  Addln('%s = array of %s;',[GetName(aDef),GetTypeName(aDef.ElementType)])
end;


procedure TBaseWebIDLToPas.WriteUnionDef(aDef: TIDLUnionTypeDefDefinition);

Var
  S: UTF8String;
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
  AddLn('%s = JSValue; ',[GetName(aDef)])
end;


procedure TBaseWebIDLToPas.WritePromiseDef(aDef: TIDLPromiseTypeDefDefinition);

begin
  AddLn('%s = TJSPromise;',[GetName(aDef)]);
end;

procedure TBaseWebIDLToPas.WriteAliasTypeDef(aDef: TIDLTypeDefDefinition);

Var
  TN: String;

begin
  TN:=GetTypeName(aDef,True);
  AddLn('%s = %s;',[GetName(aDef),TN]);
end;

function TBaseWebIDLToPas.WriteTypeDef(aDef: TIDLTypeDefDefinition): Boolean;

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

function TBaseWebIDLToPas.WriteRecordDef(aDef: TIDLRecordDefinition): Boolean;

Var
  KT,VT: String;

begin
  Result:=True;
  KT:=GetTypeName(aDef.KeyType);
  VT:=GetTypeName(aDef.ValueType);
  AddLn('%s = Class(TJSObject)',[GetName(aDef)]);
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

function TBaseWebIDLToPas.WriteTypeDefs(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  TD: TIDLTypeDefDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLTypeDefDefinition then
      if WriteTypeDef(TD) then
        Inc(Result);
end;

function TBaseWebIDLToPas.WriteEnumDef(aDef: TIDLEnumDefinition): Boolean;

begin
  Result:=True;
  AddLn('%s = String;',[GetName(aDef)]);
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
      if WriteEnumDef(ED) then
        Inc(Result);
end;

function TBaseWebIDLToPas.GetArguments(aList: TIDLDefinitionList;
  ForceBrackets: Boolean): String;

Var
  I, Def: TIDLDefinition;
  A: TIDLArgumentDefinition absolute I;
  Arg, aTypeName: string;

begin
  Result:='';
  For I in aList do
    begin
    Arg:=GetName(A);
    aTypeName:=GetTypeName(A.ArgumentType);
    Arg:=Arg+': '+aTypeName;
    Def:=FindGlobalDef(A.ArgumentType.TypeName);
    if (Def is TIDLFunctionDefinition)
        or (Def is TIDLDictionaryDefinition)
        or SameText(aTypeName,'UnicodeString') then
      Arg:='const '+Arg;
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
        CD:=(DL.Definitions[J] as TIDLArgumentDefinition).Clone(Nil);
        CL.Add(CD);
        AllocatePasName(CD);
        end;
      end;
    Dec(I);
    end;
end;

procedure TBaseWebIDLToPas.AddArgumentToOverloads(aList: TFPObjectlist; AName,
  ATypeName: String; PosEl: TIDLBaseObject);

Var
  I: Integer;
  CD: TIDLArgumentDefinition;
  DL: TIDLDefinitionList;

begin
  For I:=0 to aList.Count-1 do
    begin
    DL:=TIDLDefinitionList(alist[i]);
    if Not (DL is TIDLPartialDefinitionList) then
      begin
      CD:=TIDLArgumentDefinition.Create(Nil,aName,PosEl.SrcFile,PosEl.Line,PosEl.Column);
      CD.ArgumentType:=TIDLTypeDefDefinition.Create(CD,'',PosEl.SrcFile,PosEl.Line,PosEl.Column);
      CD.ArgumentType.TypeName:=aTypeName;
      DL.Add(CD);
      AllocatePasName(cd,'');
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
      CD:=aDef.Clone(Nil);
      DL.Add(CD);
      if aDef.Data<>Nil then
        CD.Data:=CreatePasName(TPasData(aDef.Data).PasName,CD)
      else
        AllocatePasName(cd,'');
      end;
    end;
end;

procedure TBaseWebIDLToPas.AddUnionOverloads(aList: TFPObjectlist; AName: String; UT: TIDLUnionTypeDefDefinition);

Var
  L,L2: TFPObjectList;
  I,J: Integer;
  D: TIDLDefinitionList;
  Dups: TStringList;

begin
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
      Dups.Add(GetTypeName(UT.Union[I] as TIDLTypeDefDefinition));
    // First, clone list and add argument to cloned lists
    For I:=1 to Dups.Count-1 do
      begin
      // Clone list
      CloneNonPartialArgumentList(L,L2,False);
      // Add argument to cloned list
      AddArgumentToOverloads(L2,aName,Dups[i],UT.Union[I]);
      // Add overloads to original list
      For J:=0 to L2.Count-1 do
        aList.Add(L2[J]);
      L2.Clear;
      end;
    // Add first Union to original list
    AddArgumentToOverloads(L,aName,Dups[0],UT.Union[0]);
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

procedure TBaseWebIDLToPas.AddOverloads(aList: TFPObjectlist;
  adef: TIDLFunctionDefinition; aIdx: Integer);

Var
  Arg: TIDLArgumentDefinition;
  D: TIDLDefinition;
  UT: TIDLUnionTypeDefDefinition;

begin
 if aIdx>=ADef.Arguments.Count then
    exit;
  Arg:=ADef.Argument[aIdx];
  if Arg.IsOptional then
    CloneNonPartialArgumentList(aList);
  // Add current to list.
  D:=Arg.ArgumentType;
  UT:=Nil;
  if coExpandUnionTypeArgs in BaseOptions then
    UT:=CheckUnionTypeDefinition(D);
  if UT=Nil then
    AddArgumentToOverloads(aList,Arg)
  else
    AddUnionOverLoads(aList,Arg.Name,UT);
  AddOverloads(aList,aDef,aIdx+1);
end;

function TBaseWebIDLToPas.GetOverloads(aDef: TIDLFunctionDefinition): TFPObjectlist;

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

function TBaseWebIDLToPas.WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition): Boolean;

Var
  FN,RT,Args: String;

begin
  Result:=True;
  FN:=GetName(aDef);
  RT:=GetTypeName(aDef.ReturnType,False);
  if (RT='void') then
    RT:='';
  Args:=GetArguments(aDef.Arguments,False);
  if (RT='') then
    AddLn('%s = procedure %s;',[FN,Args])
  else
    AddLn('%s = function %s: %s;',[FN,Args,RT])
end;

function TBaseWebIDLToPas.WriteFunctionDefinition(aDef: TIDLFunctionDefinition): Boolean;
begin
  Result:=true;
  if aDef=nil then exit;
end;

function TBaseWebIDLToPas.WriteCallBackDefs(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  FD: TIDLFunctionDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLFunctionDefinition then
      if (foCallBack in FD.Options) then
         if WriteFunctionTypeDefinition(FD) then
           Inc(Result);
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
        if WriteDictionaryDef(DD) then
          Inc(Result);
end;

function TBaseWebIDLToPas.WriteInterfaceDefs(aList: TIDLDefinitionList): Integer;

Var
  D: TIDLDefinition;
  ID: TIDLInterfaceDefinition absolute D;

begin
  Result:=0;
  EnsureSection(csType);
  for D in aList do
    if D is TIDLInterfaceDefinition then
      if not TIDLInterfaceDefinition(D).IsPartial then
        if WriteInterfaceDef(ID) then
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

begin
  CreateUnitClause;
  CreateHeader;
  if coAddOptionsToHeader in BaseOptions then
    AddOptionsToHeader;
  EnsureSection(csType);
  Indent;
  WriteForwardClassDefs(Context.Definitions);
  WriteEnumDefs(Context.Definitions);
  WriteTypeDefs(Context.Definitions);
  WriteCallbackDefs(Context.Definitions);
  WriteDictionaryDefs(Context.Definitions);
  WriteInterfaceDefs(Context.Definitions);
  Undent;
  WriteIncludeInterfaceCode;
  Addln('');
  AddLn('implementation');
  WriteImplementation;
  AddLn('end.');
  Source.SaveToFile(OutputFileName);
end;

function TBaseWebIDLToPas.CreatePasName(aName: String; D: TIDLBaseObject
  ): TPasData;
begin
  Result:=PasDataClass.Create(EscapeKeyWord(aName),D);
  FPasNameList.Add(Result);
end;

function TBaseWebIDLToPas.AllocatePasName(D: TIDLDefinition; ParentName: String): TPasData;

Var
  CN: String;
  aData: TPasData;

begin
  CN:=D.Name;
  if D Is TIDLInterfaceDefinition then
    begin
    if not TIDLInterfaceDefinition(D).IsPartial then
      AddJSIdentifier(D);
    CN:=ClassPrefix+CN+ClassSuffix;
    Result:=CreatePasName(CN,D);
    D.Data:=Result;
    AllocatePasNames((D as TIDLInterfaceDefinition).Members,D.Name);
    end
  else if D Is TIDLDictionaryDefinition then
    begin
    if not TIDLDictionaryDefinition(D).IsPartial then
      AddJSIdentifier(D);
    if coDictionaryAsClass in BaseOptions then
      CN:=ClassPrefix+CN+ClassSuffix;
    Result:=CreatePasName(EscapeKeyWord(CN),D);
    D.Data:=Result;
    AllocatePasNames((D as TIDLDictionaryDefinition).Members,D.Name);
    end
  else
    begin
    if (D Is TIDLFunctionDefinition) and (foCallBack in TIDLFunctionDefinition(D).Options) then
      begin
      CN:=FuncTypePrefix+CN;
      AddJSIdentifier(D);
      end;
    Result:=CreatePasName(CN,D);
    D.Data:=Result;
    if D Is TIDLFunctionDefinition then
      AllocatePasNames((D as TIDLFunctionDefinition).Arguments,D.Name);
    end;
  aData:=TPasData(D.Data);
  if Verbose and (aData.PasName<>D.Name) then
    begin
    if (ParentName<>'') then
      ParentName:=ParentName+'.';
    DoLog('Renamed %s to %s for %s',[ParentName+D.Name,aData.PasName,GetPasDataPos(aData)]);
    end;
end;

procedure TBaseWebIDLToPas.AddJSIdentifier(D: TIDLDefinition);
var
  Old: TIDLDefinition;
begin
  if (D.Parent=nil)
      or ((D is TIDLInterfaceDefinition) and TIDLInterfaceDefinition(D).IsMixin) then
    begin
    Old:=FindGlobalDef(D.Name);
    if Old<>nil then
      raise EWebIDLParser.Create('Duplicate identifier '+D.Name+' at '+GetDefPos(D)+' and '+GetDefPos(Old));
    FGlobalDefs.Add(D.Name,D);
    end
  else
    writeln('TBaseWebIDLToPas.AddJSIdentifier SubIdentifier: '+D.Name+' at '+GetDefPos(D)+' Parent=',D.Parent.Name,':',D.Parent.ClassName,' at ',GetDefPos(D.Parent));
end;

procedure TBaseWebIDLToPas.ResolveTypeDefs(aList: TIDLDefinitionList);
var
  D: TIDLDefinition;
begin
  For D in aList do
    ResolveTypeDef(D);
end;

procedure TBaseWebIDLToPas.ResolveTypeDef(D: TIDLDefinition);

  procedure ResolveTypeName(const aTypeName: string);
  var
    Def: TIDLDefinition;
    Data: TPasData;
  begin
    Def:=FindGlobalDef(aTypeName);
    if Def=nil then
      begin
      if NameToWebIDLBaseType(aTypeName)=wibtNone then
        writeln('Type ',aTypeName,' not found at ',GetDefPos(D));
      end
    else
      begin
      Data:=TPasData(D.Data);
      if Data=nil then
        Data:=CreatePasName('',D);
      Data.Resolved:=Def;
      end;
  end;

var
  DMD: TIDLDictionaryMemberDefinition;
  IT: TIDLIterableDefinition;
  SerializerD: TIDLSerializerDefinition;
begin
  if D=nil then exit;
  //writeln('TBaseWebIDLToPas.ResolveTypeDef START ',D.Name,':',D.ClassName,' at ',GetDefPos(D));
  if D Is TIDLInterfaceDefinition then
    ResolveTypeDefs((D as TIDLInterfaceDefinition).Members)
  else if D Is TIDLDictionaryDefinition then
    ResolveTypeDefs((D as TIDLDictionaryDefinition).Members)
  else if D is TIDLIncludesDefinition then
  else if D Is TIDLFunctionDefinition then
    ResolveTypeDefs((D as TIDLFunctionDefinition).Arguments)
  else if D is TIDLAttributeDefinition then
    ResolveTypeDef(TIDLAttributeDefinition(D).AttributeType)
  else if D is TIDLArgumentDefinition then
    ResolveTypeDef(TIDLArgumentDefinition(D).ArgumentType)
  else if D is TIDLTypeDefDefinition then
    ResolveTypeName(TIDLTypeDefDefinition(D).TypeName)
  else if D is TIDLConstDefinition then
    ResolveTypeName(TIDLConstDefinition(D).TypeName)
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
  else if D is TIDLSetlikeDefinition then
    ResolveTypeDef(TIDLSetlikeDefinition(D).ElementType)
  else if D is TIDLImplementsOrIncludesDefinition then
  else if D is TIDLIterableDefinition then
    begin
    IT:=TIDLIterableDefinition(D);
    ResolveTypeDef(IT.ValueType);
    ResolveTypeDef(IT.KeyType);
    end
  else {if Verbose then}
    writeln('TBaseWebIDLToPas.ResolveTypeDef unknown ',D.Name,':',D.ClassName,' at ',GetDefPos(D));
end;

function TBaseWebIDLToPas.FindGlobalDef(const aName: UTF8String
  ): TIDLDefinition;
begin
  Result:=TIDLDefinition(FGlobalDefs.Find(aName));
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
  if FTypeAliases=AValue then Exit;
  FTypeAliases.Assign(AValue);
end;

procedure TBaseWebIDLToPas.SetIncludeInterfaceCode(AValue: TStrings);
begin
  if FIncludeInterfaceCode=AValue then Exit;
  FIncludeInterfaceCode.Assign(AValue);
end;

procedure TBaseWebIDLToPas.SetIncludeImplementationCode(AValue: TStrings);
begin
  if FIncludeImplementationCode=AValue then Exit;
  FIncludeImplementationCode.Assign(AValue);
end;

procedure TBaseWebIDLToPas.AllocatePasNames(aList: TIDLDefinitionList; ParentName: String = '');

var
  D: TIDLDefinition;

begin
  For D in aList do
    AllocatePasName(D,ParentName);
end;

procedure TBaseWebIDLToPas.ProcessDefinitions;

begin
  FContext.AppendPartials;
  FContext.AppendIncludes;
  AllocatePasNames(FContext.Definitions);
  ResolveTypeDefs(FContext.Definitions);
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

end.

