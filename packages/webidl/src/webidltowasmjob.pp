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
unit webidltowasmjob;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, webidldefs, webidltopas, webidlscanner, webidlparser, Contnrs;

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
  public
  end;

  { TWebIDLToPasWasmJob }

  TWebIDLToPasWasmJob = class(TBaseWebIDLToPas)
  private
    FPasInterfacePrefix: TIDLString;
    FPasInterfaceSuffix: TIDLString;
  Protected
    FWritingPasInterface: boolean;
    function BaseUnits: String; override;
    // Auxiliary routines
    function GetPasClassName(const aName: String): String; overload; // convert to PasInterfacePrefix+X+FPasInterfaceSuffix
      override;
    function IntfToPasClassName(const aName: TIDLString): TIDLString; virtual;
    function ComputeGUID(const Prefix: TIDLString; aList: TIDLDefinitionList): TIDLString; virtual;
    procedure GetOptions(L: TStrings; Full: boolean); override;
    function GetTypeName(const aTypeName: String; ForTypeDef: Boolean=False): String; override;
    function GetPasIntfName(Intf: TIDLDefinition): TIDLString;
    function GetResolvedType(aDef: TIDLTypeDefDefinition; out aTypeName,
      aResolvedTypename: String): TIDLDefinition; overload; override;
{$IF SIZEOF(CHAR)=1}      
    function GetResolvedType(aDef: TIDLTypeDefDefinition; out aTypeName,
      aResolvedTypename: TIDLString): TIDLDefinition; overload; 
{$ENDIF}      
    function GetInterfaceDefHead(Intf: TIDLInterfaceDefinition): String;
      override;
    function GetDictionaryDefHead(const CurClassName: String;
      Dict: TIDLDictionaryDefinition): String; override;
    function WriteOtherImplicitTypes(Intf: TIDLInterfaceDefinition; aMemberList: TIDLDefinitionList): Integer;
      override;
    // Code generation routines. Return the number of actually written defs.
    function WritePrivateGetters(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; override;
    function WritePrivateSetters(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; override;
    function WriteProperties(aParent: TIDLDefinition; aList: TIDLDefinitionList): Integer; override;
    function WriteUtilityMethods(Intf: TIDLInterfaceDefinition): Integer;
      override;
    // Definitions. Return true if a definition was written.
    function WriteEnumDef(aDef: TIDLEnumDefinition): Boolean; override;
    function WriteDictionaryField(aDict: TIDLDictionaryDefinition;
      aField: TIDLDictionaryMemberDefinition): Boolean; override;
    function WriteForwardClassDef(D: TIDLStructuredDefinition): Boolean;
      override;
    function WriteFunctionDefinition(aParent: TIDLInterfaceDefinition; aDef: TIDLFunctionDefinition): Boolean;
      override;
    function WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition
      ): Boolean; override;
    function WritePrivateGetter(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition): boolean; virtual;
    function WritePrivateSetter(aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition): boolean; virtual;
    function WriteProperty(aParent: TIDLDefinition; Attr: TIDLAttributeDefinition): boolean; virtual;
    function WriteRecordDef(aDef: TIDLRecordDefinition): Boolean; override;
    procedure WriteSequenceDef(aDef: TIDLSequenceTypeDefDefinition); override;
    // Extra interface/Implementation code.
    procedure WriteGlobalVars; override;
    procedure WriteImplementation; override;
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

function TWebIDLToPasWasmJob.GetPasClassName(const aName: String): String;
begin
  Result:=aName;
  if (LeftStr(Result,length(ClassPrefix))=ClassPrefix)
  and (RightStr(Result,length(ClassSuffix))=ClassSuffix)
  then
    Result:=copy(Result,length(ClassPrefix)+1,length(Result)-length(ClassPrefix)-length(ClassSuffix));
  if Result='' then
    raise EConvertError.Create('[20220725184518]');
  Result:=PasInterfacePrefix+Result+PasInterfaceSuffix;
end;

function TWebIDLToPasWasmJob.IntfToPasClassName(const aName: TIDLString): TIDLString;
begin
  Result:=aName;
  if (LeftStr(Result,length(PasInterfacePrefix))=PasInterfacePrefix)
  and (RightStr(Result,length(PasInterfaceSuffix))=PasInterfaceSuffix)
  then
    Result:=copy(Result,length(PasInterfacePrefix)+1,length(Result)-length(PasInterfacePrefix)-length(PasInterfaceSuffix));
  if Result='' then
    raise EConvertError.Create('[20220725184440]');
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
        aTypeName:=GetTypeName(Attr.AttributeType);
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

function TWebIDLToPasWasmJob.GetTypeName(const aTypeName: String;
  ForTypeDef: Boolean): String;
begin
  Case aTypeName of
    'union',
    'any': Result:='Variant';
    'void','undefined': Result:=aTypeName;
  else
    //writeln('TWebIDLToPasWasmJob.GetTypeName ',aTypeName,' ',Def<>nil);
    Result:=inherited GetTypeName(aTypeName,ForTypeDef);
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
  Result:=GetName(Intf);
  if Result='' then
    raise EConvertError.Create('[20220725184653] missing name at '+GetDefPos(Intf));
  Result:=GetPasClassName(Result);
end;

{$IF SIZEOF(CHAR)=1}
function TWebIDLToPasWasmJob.GetResolvedType(aDef: TIDLTypeDefDefinition; out
  aTypeName, aResolvedTypename: TIDLString): TIDLDefinition;

Var
  TN,RTN : String;
  
begin
  Result:=GetResolvedType(aDef,TN,RTN);
  aTypeName:=TN;
  aResolvedTypeName:=RTN;
end;
{$ENDIF}

function TWebIDLToPasWasmJob.GetResolvedType(aDef: TIDLTypeDefDefinition; out
  aTypeName, aResolvedTypename: String): TIDLDefinition;
begin
  Result:=inherited GetResolvedType(aDef, aTypeName, aResolvedTypename);
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
    aParentName:=GetName(Intf.ParentInterface)
  else
    aParentName:=GetTypeName(Intf.ParentName);
  if aParentName='' then
    aParentName:=ClassPrefix+'Object'+ClassSuffix;
  if aParentName<>'' then
    Result:=Result+aParentName;
  aPasIntfName:=GetPasIntfName(Intf);
  Result:=Result+','+aPasIntfName+')';
end;

function TWebIDLToPasWasmJob.GetDictionaryDefHead(const CurClassName: String;
  Dict: TIDLDictionaryDefinition): String;
begin
  Result:=CurClassName+'Rec = record';
  if Dict=nil then ;
end;

function TWebIDLToPasWasmJob.WriteOtherImplicitTypes(
  Intf: TIDLInterfaceDefinition; aMemberList: TIDLDefinitionList): Integer;
var
  aPasIntfName, Decl, ParentName: TIDLString;
begin
  Result:=1;

  FWritingPasInterface:=true;
  try

    // Pascal interface and ancestor
    aPasIntfName:=GetPasIntfName(Intf);

    Decl:=aPasIntfName+' = interface';
    if Assigned(Intf.ParentInterface) then
      ParentName:=GetPasIntfName(Intf.ParentInterface as TIDLInterfaceDefinition)
    else
      ParentName:=GetTypeName(Intf.ParentName);
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
    FWritingPasInterface:=false;
  end;
end;

function TWebIDLToPasWasmJob.WritePrivateGetters(aParent: TIDLStructuredDefinition;
  aList: TIDLDefinitionList): Integer;
var
  D: TIDLDefinition;
begin
  Result:=0;
  for D in aList do
    if D is TIDLAttributeDefinition then
      if WritePrivateGetter(aParent,TIDLAttributeDefinition(D)) then
        inc(Result);
end;

function TWebIDLToPasWasmJob.WritePrivateSetters(
  aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer;
var
  D: TIDLDefinition;
begin
  Result:=0;
  for D in aList do
    if D is TIDLAttributeDefinition then
      if WritePrivateSetter(aParent,TIDLAttributeDefinition(D)) then
        inc(Result);
end;

function TWebIDLToPasWasmJob.WriteProperties(aParent: TIDLDefinition;
  aList: TIDLDefinitionList): Integer;
var
  D: TIDLDefinition;
begin
  Result:=0;
  for D in aList do
    if D is TIDLAttributeDefinition then
      if WriteProperty(aParent,TIDLAttributeDefinition(D)) then
        inc(Result);
end;

function TWebIDLToPasWasmJob.WriteUtilityMethods(Intf: TIDLInterfaceDefinition
  ): Integer;
var
  aClassName, aPasIntfName, Code: TIDLString;
begin
  Result:=0;
  aClassName:=GetName(Intf);
  aPasIntfName:=GetPasIntfName(Intf);
  AddLn('class function Cast(const Intf: IJSObject): '+aPasIntfName+';');
  Code:='class function '+aClassName+'.Cast(const Intf: IJSObject): '+aPasIntfName+';'+sLineBreak;
  Code:=Code+'begin'+sLineBreak;
  Code:=Code+'  Result:='+aClassName+'.JOBCast(Intf);'+sLineBreak;
  Code:=Code+'end;'+sLineBreak;
  IncludeImplementationCode.Add(Code);
end;

function TWebIDLToPasWasmJob.WriteEnumDef(aDef: TIDLEnumDefinition): Boolean;
begin
  Result:=True;
  AddLn(GetName(aDef)+' = UnicodeString;');
end;

function TWebIDLToPasWasmJob.WriteDictionaryField(
  aDict: TIDLDictionaryDefinition; aField: TIDLDictionaryMemberDefinition
  ): Boolean;
var
  N, TN: TIDLString;
begin
  if aDict<>nil then ;
  Result:=True;
  N:=GetName(aField);
  TN:=GetTypeName(aField.MemberType);
  if SameText(N,TN) then
    N:='_'+N;
  AddLn(N+': '+TN+';');
end;

function TWebIDLToPasWasmJob.WriteForwardClassDef(D: TIDLStructuredDefinition
  ): Boolean;
begin
  if D is TIDLDictionaryDefinition then
    AddLn(GetName(D)+' = '+JOB_JSValueTypeNames[jjvkDictionary]+';')
  else
    begin
    if (not D.IsPartial) and (D is TIDLInterfaceDefinition) then
      AddLn(GetPasIntfName(D)+' = interface;');
    Result:=inherited WriteForwardClassDef(D);
    end;
end;

function TWebIDLToPasWasmJob.WriteFunctionDefinition(
  aParent: TIDLInterfaceDefinition; aDef: TIDLFunctionDefinition): Boolean;
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
  FuncName, Suff, Args, ProcKind, Sig, aClassName, Code, InvokeName,
    InvokeCode, TryCode, VarSection, FinallyCode, LocalName, WrapperFn,
    ArgName, ArgTypeName, ReturnTypeName, ResolvedReturnTypeName,
    InvokeClassName, ArgResolvedTypeName: TIDLString;
  Overloads: TFPObjectList;
  I: Integer;
  AddFuncBody: Boolean;
  ArgDefList: TIDLDefinitionList;
  CurDef, ArgType, ReturnDef: TIDLDefinition;
  ArgDef: TIDLArgumentDefinition absolute CurDef;
begin
  Result:=True;
  Data:=aDef.Data as TPasDataWasmJob;
  if Data.PasName='' then
    begin
    writeln('Note: skipping Getter of '+aDef.Parent.Name+' at '+GetDefPos(aDef));
    exit(false);
    end;

  Suff:='';
  ReturnDef:=GetResolvedType(aDef.ReturnType,ReturnTypeName,ResolvedReturnTypeName);
  InvokeName:='';
  InvokeClassName:='';
  if (foConstructor in aDef.Options) then
    begin
    FuncName:='New';
    writeln('Note: skipping constructor of '+aDef.Parent.Name+' at '+GetDefPos(aDef));
    exit(false);
    end
  else
    begin
    FuncName:=GetName(aDef);
    case ResolvedReturnTypeName of
    'Boolean': InvokeName:='InvokeJSBooleanResult';
    'ShortInt',
    'Byte',
    'SmallInt',
    'Word',
    'Integer': InvokeName:='InvokeJSLongIntResult';
    'LongWord',
    'Int64',
    'QWord': InvokeName:='InvokeJSMaxIntResult';
    'Single',
    'Double': InvokeName:='InvokeJSDoubleResult';
    'UTF8String': InvokeName:='InvokeJSUTF8StringResult';
    'UnicodeString': InvokeName:='InvokeJSUnicodeStringResult';
    'Variant': InvokeName:='InvokeJSVariantResult';
    'TJOB_JSValue': InvokeName:='InvokeJSValueResult';
    'void','undefined':
      begin
      ReturnTypeName:='';
      ResolvedReturnTypeName:='';
      InvokeName:='InvokeJSNoResult';
      end;
    else
      InvokeName:='InvokeJSObjectResult';
      if ReturnDef is TIDLSequenceTypeDefDefinition then
        InvokeClassName:=ClassPrefix+'Array'+ClassSuffix
      else if ReturnDef is TIDLPromiseTypeDefDefinition then
        InvokeClassName:=ClassPrefix+'Promise'+ClassSuffix
      else if ReturnDef is TIDLInterfaceDefinition then
        InvokeClassName:=GetName(ReturnDef)
      else if ResolvedReturnTypeName=PasInterfacePrefix+'Object'+PasInterfaceSuffix then
        begin
        InvokeClassName:=ClassPrefix+'Object'+ClassSuffix;
        end
      else
        raise EConvertError.Create('[20220725172242] not yet supported: function return type '+ResolvedReturnTypeName+' '+ReturnDef.ClassName+' at '+GetDefPos(aDef));
    end;

    end;
  aClassName:=GetName(aParent);
  AddFuncBody:=not FWritingPasInterface;

  Overloads:=GetOverloads(ADef);
  try
    if (aDef.Arguments.Count>0)
        and aDef.Argument[aDef.Arguments.Count-1].HasEllipsis then
      Suff:='{; ToDo:varargs}';
    if Overloads.Count>1 then
      Suff:=Suff+'; overload';
    For I:=0 to Overloads.Count-1 do
      begin
      ArgDefList:=TIDLDefinitionList(Overloads[i]);
      Args:=GetArguments(ArgDefList,False);
      if (ReturnTypeName='') then
        begin
        if not (foConstructor in aDef.Options) then
          ProcKind:='procedure'
        else
          ProcKind:='constructor';
        Sig:=FuncName+Args+Suff+';';
        end
      else
        begin
        ProcKind:='function';
        Sig:=FuncName+Args+': '+ReturnTypeName+Suff+';';
        end;
      if ReturnDef is TIDLPromiseTypeDefDefinition then
        Sig:=Sig+' // Promise<'+TIDLPromiseTypeDefDefinition(ReturnDef).ReturnType.TypeName+'>';
      AddLn(ProcKind+' '+Sig);

      if not AddFuncBody then continue;

      ArgNames:=TStringList.Create;
      try
        for CurDef in ArgDefList do
          ArgNames.Add(GetName(ArgDef));

        InvokeCode:='';
        if ReturnTypeName<>'' then
          InvokeCode:='Result:=';
        VarSection:='';
        TryCode:='';
        FinallyCode:='';
        Args:='';
        for CurDef in ArgDefList do
          begin
          if Args<>'' then
            Args:=Args+',';
          ArgName:=GetName(ArgDef);
          ArgType:=GetResolvedType(ArgDef.ArgumentType,ArgTypeName,ArgResolvedTypeName);
          //writeln('TWebIDLToPasWasmJob.WriteFunctionDefinition ',ArgType.Name,':',ArgType.ClassName,' ',ArgResolvedTypeName,' ArgType=',hexstr(ptruint(ArgType),sizeof(ptruint)*2));
          if ArgType is TIDLSequenceTypeDefDefinition then
            begin
            ArgTypeName:=TIDLSequenceTypeDefDefinition(ArgType).ElementType.TypeName;
            if Verbose then
              writeln('Hint: TWebIDLToPasWasmJob.WriteFunctionDefinition sequence of ',ArgTypeName);
            raise EConvertError.Create('[20220725172246] not yet supported: passing an array of '+ArgTypeName+' as argument at '+GetDefPos(ArgDef));
            end
          else if (ArgType is TIDLFunctionDefinition) and (foCallBack in TIDLFunctionDefinition(ArgType).Options) then
            begin
            LocalName:=CreateLocal('m');
            VarSection:=VarSection+'  '+LocalName+': '+JOB_JSValueTypeNames[jivkMethod]+';'+sLineBreak;
            WrapperFn:='JOBCall'+GetName(TIDLFunctionDefinition(ArgType));
            TryCode:=TryCode+'  '+LocalName+':='+JOB_JSValueTypeNames[jivkMethod]+'.Create(TMethod('+ArgName+'),@'+WrapperFn+');'+sLineBreak;
            FinallyCode:=FinallyCode+'    '+LocalName+'.free;'+sLineBreak;
            ArgName:=LocalName;
            end;
          Args:=Args+ArgName;
          end;
        Args:=',['+Args+']';

        InvokeCode:=InvokeCode+InvokeName+'('''+aDef.Name+''''+Args;
        if InvokeClassName<>'' then
          InvokeCode:=InvokeCode+','+InvokeClassName+') as '+ReturnTypeName
        else
          InvokeCode:=InvokeCode+')';

        Code:=ProcKind+' '+aClassName+'.'+Sig+sLineBreak;
        if VarSection<>'' then
          Code:=Code+'var'+sLineBreak+VarSection;
        Code:=Code+'begin'+sLineBreak;
        if TryCode<>'' then
          begin
          Code:=Code+TryCode;
          Code:=Code+'  try'+sLineBreak;
          Code:=Code+'    '+InvokeCode+';'+sLineBreak;
          Code:=Code+'  finally'+sLineBreak;
          Code:=Code+FinallyCode;
          Code:=Code+'  end;'+sLineBreak;
          end
        else
          begin
          Code:=Code+'  '+InvokeCode+';'+sLineBreak;
          end;
        Code:=Code+'end;'+sLineBreak;

        IncludeImplementationCode.Add(Code);
      finally
        ArgNames.Free;
      end;

      end;
  finally
    Overloads.Free;
  end;
end;

function TWebIDLToPasWasmJob.WriteFunctionTypeDefinition(
  aDef: TIDLFunctionDefinition): Boolean;
var
  FuncName, ReturnTypeName, ResolvedReturnTypeName: TIDLString;
  ArgName, ArgTypeName, ArgResolvedTypename: TIDLString;
  VarSection, FetchArgs, Params, Call, Code, GetFunc: TIDLString;
  Args: TIDLDefinitionList;
  ArgDef: TIDLArgumentDefinition;
  ArgNames: TStringList;
  j, i: Integer;
  ReturnDef, ArgType: TIDLDefinition;
begin
  Result:=True;
  FuncName:=GetName(aDef);

  ReturnDef:=GetResolvedType(aDef.ReturnType,ReturnTypeName,ResolvedReturnTypeName);
  case ResolvedReturnTypeName of
  'void','undefined':
    begin
    ReturnTypeName:='';
    ResolvedReturnTypeName:='';
    end;
  end;
  if ReturnDef is TIDLSequenceTypeDefDefinition then
    ReturnTypeName:=PasInterfacePrefix+'Array'+PasInterfaceSuffix
  else if ReturnDef is TIDLPromiseTypeDefDefinition then
    ReturnTypeName:=PasInterfacePrefix+'Promise'+PasInterfaceSuffix;

  Args:=aDef.Arguments;

  Params:=GetArguments(aDef.Arguments,False);
  if (ResolvedReturnTypeName='') then
    AddLn(FuncName+' = procedure '+Params+' of object;')
  else
    AddLn(FuncName+' = function '+Params+': '+ReturnTypeName+' of object;');

  if FWritingPasInterface then exit;

  ArgNames:=TStringList.Create;
  try
    // create wrapper callback
    Code:='function JOBCall'+FuncName+'(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;'+sLineBreak;
    ArgNames.Add('aMethod');
    ArgNames.Add('h');
    VarSection:='';
    FetchArgs:='';
    Params:='';
    for i:=0 to Args.Count-1 do
      begin
      ArgDef:=Args[i] as TIDLArgumentDefinition;
      ArgName:=GetName(ArgDef);
      if ArgNames.IndexOf(ArgName)>=0 then
        begin
        j:=2;
        while ArgNames.IndexOf(ArgName+IntToStr(j))>=0 do inc(j);
        ArgName:=ArgName+IntToStr(j);
        end;
      ArgType:=GetResolvedType(ArgDef.ArgumentType,ArgTypeName,ArgResolvedTypename);

      case ArgResolvedTypename of
      '': raise EWebIDLParser.Create('[20220725181726] not yet supported: function type arg['+IntToStr(I)+'] type void/undefined at '+GetDefPos(ArgDef));
      'Boolean': GetFunc:='GetBoolean';
      'ShortInt',
      'Byte',
      'SmallInt',
      'Word',
      'Integer': GetFunc:='GetLongInt';
      'LongWord',
      'Int64',
      'QWord': GetFunc:='GetMaxInt';
      'Single',
      'Double': GetFunc:='GetDouble';
      'UTF8String',
      'UnicodeString': GetFunc:='GetString';
      'Variant': GetFunc:='GetVariant';
      'TJOB_JSValue': GetFunc:='GetValue';
      else
        if ArgType is TIDLInterfaceDefinition then
          GetFunc:='GetObject('+GetName(ArgType)+') as '+ArgTypeName
        else
          begin
          if ArgType<>nil then
            writeln('TWebIDLToPasWasmJob.WriteFunctionTypeDefinition ArgType=',ArgType.ClassName);
          raise EWebIDLParser.Create('[20220725181732] not yet supported: function type arg['+IntToStr(I)+'] type '+ArgDef.ArgumentType.TypeName+' at '+GetDefPos(ArgDef));
          end;
      end;

      // declare: var ArgName: ArgTypeName;
      VarSection:=VarSection+'  '+ArgName+': '+ArgTypeName+';'+sLineBreak;

      // get: ArgName:=H.GetX;
      FetchArgs:=FetchArgs+'  '+ArgName+':=H.'+GetFunc+';'+sLineBreak;

      // pass: ArgName
      if Params<>'' then
        Params:=Params+',';
      Params:=Params+ArgName;

      end;
    if VarSection<>'' then
      Code:=Code+'var'+sLineBreak+VarSection;

    Code:=Code+'begin'+sLineBreak;
    if FetchArgs<>'' then
      Code:=Code+FetchArgs;

    Call:=FuncName+'(aMethod)('+Params+')';
    case ResolvedReturnTypeName of
    '':
      begin
      Code:=Code+'  '+Call+';'+sLineBreak;
      GetFunc:='Result:=H.AllocUndefined;';
      end;
    'Boolean': GetFunc:='Result:=H.AllocBool('+Call+');';
    'ShortInt',
    'Byte',
    'SmallInt',
    'Word',
    'Integer': GetFunc:='Result:=H.AllocLongint('+Call+');';
    'LongWord',
    'Int64',
    'QWord',
    'Single',
    'Double': GetFunc:='Result:=H.AllocDouble('+Call+');';
    'UTF8String': GetFunc:='Result:=H.AllocString('+Call+');';
    'UnicodeString': GetFunc:='Result:=H.AllocString('+Call+');';
    'Variant': GetFunc:='Result:=H.AllocVariant('+Call+');';
    'TJOB_JSValue': GetFunc:='Result:=H.AllocJSValue('+Call+');';
    else
      if ReturnDef is TIDLInterfaceDefinition then
        GetFunc:='Result:=H.AllocIntf('+Call+');'
      else
        begin
        if ReturnDef<>nil then
          writeln('TWebIDLToPasWasmJob.WriteFunctionTypeDefinition ReturnDef=',ReturnDef.ClassName);
        raise EWebIDLParser.Create('[20220725181735] not yet supported: function type result type "'+ResolvedReturnTypeName+'" at '+GetDefPos(aDef));
        end;
    end;
    Code:=Code+'  '+GetFunc+sLineBreak;
    Code:=Code+'end;'+sLineBreak;

    IncludeImplementationCode.Add(Code);
  finally
  end;
end;

function TWebIDLToPasWasmJob.WritePrivateGetter(
  aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition): boolean;
var
  FuncName, aClassName, Code, ReadFuncName, Call,
    AttrTypeName, AttrResolvedTypeName, ObjClassName: TIDLString;
  AttrType: TIDLDefinition;
begin
  Result:=true;
  if Attr.AttributeType=nil then
    exit;

  FuncName:=GetterPrefix+GetName(Attr);
  AttrType:=GetResolvedType(Attr.AttributeType,AttrTypeName,AttrResolvedTypeName);

  if AttrType is TIDLInterfaceDefinition then
    AttrTypeName:=GetPasIntfName(AttrType)
  else if AttrType is TIDLFunctionDefinition then
    exit // not supported yet
  else if AttrType is TIDLEnumDefinition then
    AttrResolvedTypeName:='UnicodeString';

  AddLn('function '+FuncName+': '+AttrTypeName+';');

  if FWritingPasInterface then exit;

  aClassName:=GetName(aParent);
  Call:='';

  case AttrResolvedTypeName of
  'Boolean': ReadFuncName:='ReadJSPropertyBoolean';
  'ShortInt',
  'Byte',
  'SmallInt',
  'Word',
  'Integer': ReadFuncName:='ReadJSPropertyLongInt';
  'LongWord',
  'Int64',
  'QWord': ReadFuncName:='ReadJSPropertyInt64';
  'Single',
  'Double': ReadFuncName:='ReadJSPropertyDouble';
  'UTF8String': ReadFuncName:='ReadJSPropertyUTF8String';
  'UnicodeString': ReadFuncName:='ReadJSPropertyUnicodeString';
  'Variant': ReadFuncName:='ReadJSPropertyVariant';
  'TJOB_JSValue': ReadFuncName:='ReadJSPropertyValue';
  else
    if AttrType is TIDLSequenceTypeDefDefinition then
      ObjClassName:=ClassPrefix+'Array'+ClassSuffix
    else if AttrType is TIDLPromiseTypeDefDefinition then
      ObjClassName:=ClassPrefix+'Promise'+ClassSuffix
    else
      begin
      ObjClassName:=GetName(AttrType);
      if ObjClassName='' then
        ObjClassName:=IntfToPasClassName(AttrTypeName);
      end;
    Call:='ReadJSPropertyObject('''+Attr.Name+''','+ObjClassName+') as '+AttrTypeName;
  end;

  if Call='' then
    Call:=ReadFuncName+'('''+Attr.Name+''')';

  Code:='function '+aClassName+'.'+FuncName+': '+AttrTypeName+';'+sLineBreak;
  Code:=Code+'begin'+sLineBreak;
  Code:=Code+'  Result:='+Call+';'+sLineBreak;
  Code:=Code+'end;'+sLineBreak;

  IncludeImplementationCode.Add(Code);
end;

function TWebIDLToPasWasmJob.WritePrivateSetter(
  aParent: TIDLStructuredDefinition; Attr: TIDLAttributeDefinition): boolean;
var
  FuncName, aClassName, WriteFuncName, Code, Call,
    AttrTypeName, AttrResolvedTypeName: TIDLString;
  AttrType: TIDLDefinition;
begin
  if aoReadOnly in Attr.Options then
    exit(false);
  if Attr.AttributeType=nil then
    exit;

  Result:=true;
  FuncName:=SetterPrefix+GetName(Attr);
  AttrType:=GetResolvedType(Attr.AttributeType,AttrTypeName,AttrResolvedTypeName);
  if AttrType is TIDLInterfaceDefinition then
    AttrTypeName:=GetPasIntfName(AttrType)
  else if AttrType is TIDLFunctionDefinition then
    exit // not supported yet
  else if AttrType is TIDLEnumDefinition then
    AttrResolvedTypeName:='UnicodeString';

  AddLn('procedure '+FuncName+'(const aValue: '+AttrTypeName+');');

  if FWritingPasInterface then exit;

  aClassName:=GetName(aParent);
  Call:='';

  case AttrResolvedTypeName of
  'Boolean': WriteFuncName:='WriteJSPropertyBoolean';
  'ShortInt',
  'Byte',
  'SmallInt',
  'Word',
  'Integer': WriteFuncName:='WriteJSPropertyLongInt';
  'LongWord',
  'Int64',
  'QWord': WriteFuncName:='WriteJSPropertyDouble';
  'Single',
  'Double': WriteFuncName:='WriteJSPropertyDouble';
  'UTF8String': WriteFuncName:='WriteJSPropertyUTF8String';
  'UnicodeString': WriteFuncName:='WriteJSPropertyUnicodeString';
  'Variant': WriteFuncName:='WriteJSPropertyVariant';
  'TJOB_JSValue': WriteFuncName:='WriteJSPropertyValue';
  else
    WriteFuncName:='WriteJSPropertyObject';
  end;

  if Call='' then
    Call:=WriteFuncName+'('''+Attr.Name+''',aValue)';

  Code:='procedure '+aClassName+'.'+FuncName+'(const aValue: '+AttrTypeName+');'+sLineBreak;
  Code:=Code+'begin'+sLineBreak;
  Code:=Code+'  '+Call+';'+sLineBreak;
  Code:=Code+'end;'+sLineBreak;

  IncludeImplementationCode.Add(Code);
end;

function TWebIDLToPasWasmJob.WriteProperty(aParent: TIDLDefinition;
  Attr: TIDLAttributeDefinition): boolean;
var
  PropName, Code, AttrTypeName, AttrResolvedTypeName: TIDLString;
  AttrType: TIDLDefinition;
begin
  if aParent=nil then ;
  if Attr.AttributeType=nil then
    begin
    writeln('Note: skipping field "'+Attr.Name+'" without type at '+GetDefPos(Attr));
    exit;
    end;
  PropName:=GetName(Attr);
  AttrType:=GetResolvedType(Attr.AttributeType,AttrTypeName,AttrResolvedTypeName);
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
  AddLn(GetName(aDef)+' = '+ClassPrefix+'Object'+ClassSuffix+';');
end;

procedure TWebIDLToPasWasmJob.WriteSequenceDef(
  aDef: TIDLSequenceTypeDefDefinition);
begin
  Addln(GetName(aDef)+' = '+PasInterfacePrefix+'Array'+PasInterfaceSuffix+'; // array of '+GetTypeName(aDef.ElementType));
end;

procedure TWebIDLToPasWasmJob.WriteGlobalVars;
var
  i: Integer;
  PasVarName, JSClassName, JOBRegisterName: TIDLString;
  aDef: TIDLDefinition;
begin
  if GlobalVars.Count=0 then exit;
  AddLn('');
  AddLn('var');
  Indent;
  for i:=0 to GlobalVars.Count-1 do
    begin
    if not SplitGlobalVar(GlobalVars[i],PasVarName,JSClassName,JOBRegisterName) then
      raise EConvertError.Create('invalid global var "'+GlobalVars[i]+'"');
    aDef:=FindGlobalDef(JSClassName);
    if aDef=nil then
      raise EConvertError.Create('missing global var "'+PasVarName+'" type "'+JSClassName+'"');
    AddLn(PasVarName+': '+GetName(aDef)+';');
    end;
  Undent;
end;

procedure TWebIDLToPasWasmJob.WriteImplementation;
var
  i: Integer;
  aDef: TIDLDefinition;
  PasVarName, JSClassName, JOBRegisterName: TIDLString;
begin
  inherited WriteImplementation;
  if GlobalVars.Count>0 then
    begin
    AddLn('initialization');
    Indent;
    for i:=0 to GlobalVars.Count-1 do
      begin
      SplitGlobalVar(GlobalVars[i],PasVarName,JSClassName,JOBRegisterName);
      aDef:=FindGlobalDef(JSClassName);
      AddLn(PasVarName+':='+GetName(aDef)+'.JOBCreateGlobal('''+JOBRegisterName+''');');
      end;
    Undent;

    AddLn('finalization');
    Indent;
    for i:=0 to GlobalVars.Count-1 do
      begin
      SplitGlobalVar(GlobalVars[i],PasVarName,JSClassName,JOBRegisterName);
      AddLn(PasVarName+'.Free;');
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

