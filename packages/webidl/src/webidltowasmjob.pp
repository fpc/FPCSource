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
  Classes, SysUtils, webidldefs, webidltopas, webidlparser, Contnrs;

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
  JOB_JSValueKindNames: array[TJOB_JSValueKind] of string = (
    'Undefined',
    'Boolean',
    'Double',
    'String',
    'Object',
    'Method',
    'Dictionary',
    'Array'
    );
  JOB_JSValueTypeNames: array[TJOB_JSValueKind] of string = (
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
    GetterBody: String; // also used for Function body
    SetterBody: String;
    HasFuncBody: boolean;
  end;

  { TWebIDLToPasWasmJob }

  TWebIDLToPasWasmJob = class(TBaseWebIDLToPas)
  private
    FPasInterfacePrefix: String;
    FPasInterfaceSuffix: String;
  Protected
    function BaseUnits: String; override;
    // Auxiliary routines
    function GetPasClassName(const aName: string): string; overload;
      override;
    function IntfToPasClassName(const aName: string): string; virtual;
    function ComputeGUID(const Prefix: string; aList: TIDLDefinitionList): string; virtual;
    procedure GetOptions(L: TStrings; Full: boolean); override;
    function GetTypeName(const aTypeName: String; ForTypeDef: Boolean=False
      ): String; override;
    function GetPasIntfName(Intf: TIDLDefinition): string;
    function GetInterfaceDefHead(Intf: TIDLInterfaceDefinition): String;
      override;
    function GetDictionaryDefHead(const CurClassName: string;
      Dict: TIDLDictionaryDefinition): String; override;
    function WriteOtherImplicitTypes(Intf: TIDLInterfaceDefinition; aMemberList: TIDLDefinitionList): Integer;
      override;
    // Code generation routines. Return the number of actually written defs.
    function WritePrivateGetters(aList: TIDLDefinitionList): Integer; override;
    function WritePrivateSetters(aList: TIDLDefinitionList): Integer; override;
    function WriteProperties(aList: TIDLDefinitionList): Integer; override;
    function WriteUtilityMethods(Intf: TIDLInterfaceDefinition): Integer;
      override;
    // Definitions. Return true if a definition was written.
    function WriteDictionaryField(aField: TIDLDictionaryMemberDefinition
      ): Boolean; override;
    function WriteForwardClassDef(D: TIDLStructuredDefinition): Boolean;
      override;
    function WriteFunctionDefinition(aDef: TIDLFunctionDefinition): Boolean;
      override;
    function WriteFunctionTypeDefinition(aDef: TIDLFunctionDefinition
      ): Boolean; override;
    function WritePrivateGetter(Attr: TIDLAttributeDefinition): boolean; virtual;
    function WritePrivateSetter(Attr: TIDLAttributeDefinition): boolean; virtual;
    function WriteProperty(Attr: TIDLAttributeDefinition): boolean; virtual;
    function WriteRecordDef(aDef: TIDLRecordDefinition): Boolean; override;
  Public
    constructor Create(ThOwner: TComponent); override;
  Published
    Property BaseOptions;
    Property ClassPrefix;
    Property ClassSuffix;
    Property PasInterfacePrefix: String read FPasInterfacePrefix write FPasInterfacePrefix;
    Property PasInterfaceSuffix: String read FPasInterfaceSuffix write FPasInterfaceSuffix;
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

function TWebIDLToPasWasmJob.GetPasClassName(const aName: string): string;
begin
  Result:=aName;
  if (LeftStr(Result,length(ClassPrefix))=ClassPrefix)
  and (RightStr(Result,length(ClassSuffix))=ClassSuffix)
  then
    Result:=copy(Result,length(ClassPrefix)+1,length(Result)-length(ClassPrefix)-length(ClassSuffix));
  Result:=PasInterfacePrefix+Result+PasInterfaceSuffix;
end;

function TWebIDLToPasWasmJob.IntfToPasClassName(const aName: string): string;
begin
  Result:=aName;
  if (LeftStr(Result,length(PasInterfacePrefix))=PasInterfacePrefix)
  and (RightStr(Result,length(PasInterfaceSuffix))=PasInterfaceSuffix)
  then
    Result:=copy(Result,length(PasInterfacePrefix)+1,length(Result)-length(PasInterfacePrefix)-length(PasInterfaceSuffix));
  Result:=ClassPrefix+Result+ClassSuffix;
end;

function TWebIDLToPasWasmJob.ComputeGUID(const Prefix: string;
  aList: TIDLDefinitionList): string;
var
  List: TStringList;
  D: TIDLDefinition;
  Attr: TIDLAttributeDefinition;
  i, BytePos, BitPos, v: Integer;
  Bytes: array[0..15] of byte;
  GUIDSrc, aTypeName: String;
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
var
  Def: TIDLDefinition;
begin
  Case aTypeName of
    'union',
    'any': Result:=JOB_JSValueTypeNames[jjvkUndefined];
    'void': Result:=aTypeName;
  else
    Def:=FindGlobalDef(aTypeName);
    //writeln('TWebIDLToPasWasmJob.GetTypeName ',aTypeName,' ',Def<>nil);
    if Def is TIDLSequenceTypeDefDefinition then
      Result:=GetSequenceTypeName(TIDLSequenceTypeDefDefinition(Def))
    else
      begin
      Result:=inherited GetTypeName(aTypeName,ForTypeDef);
      if (Result=aTypeName)
      and (LeftStr(Result,length(PasInterfacePrefix))<>PasInterfacePrefix)
      and (RightStr(Result,length(PasInterfaceSuffix))<>PasInterfaceSuffix)
      then
        Result:=PasInterfacePrefix+Result+PasInterfaceSuffix;
      end;
  end;
end;

function TWebIDLToPasWasmJob.GetPasIntfName(Intf: TIDLDefinition): string;
begin
  Result:=GetPasClassName(GetName(Intf));
end;

function TWebIDLToPasWasmJob.GetInterfaceDefHead(Intf: TIDLInterfaceDefinition
  ): String;
var
  aParentName, aPasIntfName: String;
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

function TWebIDLToPasWasmJob.GetDictionaryDefHead(const CurClassName: string;
  Dict: TIDLDictionaryDefinition): String;
begin
  Result:=CurClassName+'Rec = record';
  if Dict=nil then ;
end;

function TWebIDLToPasWasmJob.WriteOtherImplicitTypes(
  Intf: TIDLInterfaceDefinition; aMemberList: TIDLDefinitionList): Integer;
var
  aPasIntfName, Decl, ParentName: String;
begin
  Result:=1;

  // Pascal interface and ancestor
  aPasIntfName:=GetPasIntfName(Intf);

  Decl:=aPasIntfName+' = interface';
  if Assigned(Intf.ParentInterface) then
    ParentName:=GetPasIntfName(Intf.ParentInterface as TIDLInterfaceDefinition)
  else
    ParentName:=GetTypeName(Intf.ParentName);
  if ParentName<>'' then
    Decl:=Decl+'('+ParentName+')';
  AddLn(Decl);

  Indent;

  // GUID
  AddLn('['''+ComputeGUID(Decl,aMemberList)+''']');

  // private members
  WritePrivateGetters(aMemberList);
  WritePrivateSetters(aMemberList);

  // public members
  WriteMethodDefs(aMemberList);
  // type cast function Cast:
  AddLn('function Cast(Intf: IJSObject): '+aPasIntfName+';');
  WriteProperties(aMemberList);

  Undent;
  AddLn('end;');
  AddLn('');
end;

function TWebIDLToPasWasmJob.WritePrivateGetters(aList: TIDLDefinitionList
  ): Integer;
var
  D: TIDLDefinition;
begin
  Result:=0;
  for D in aList do
    if D is TIDLAttributeDefinition then
      if WritePrivateGetter(TIDLAttributeDefinition(D)) then
        inc(Result);
end;

function TWebIDLToPasWasmJob.WritePrivateSetters(aList: TIDLDefinitionList
  ): Integer;
var
  D: TIDLDefinition;
begin
  Result:=0;
  for D in aList do
    if D is TIDLAttributeDefinition then
      if WritePrivateSetter(TIDLAttributeDefinition(D)) then
        inc(Result);
end;

function TWebIDLToPasWasmJob.WriteProperties(aList: TIDLDefinitionList
  ): Integer;
var
  D: TIDLDefinition;
begin
  Result:=0;
  for D in aList do
    if D is TIDLAttributeDefinition then
      if WriteProperty(TIDLAttributeDefinition(D)) then
        inc(Result);
end;

function TWebIDLToPasWasmJob.WriteUtilityMethods(Intf: TIDLInterfaceDefinition
  ): Integer;
var
  aClassName, aPasIntfName, Code: String;
begin
  Result:=0;
  aClassName:=GetName(Intf);
  aPasIntfName:=GetPasIntfName(Intf);
  AddLn('class function Cast(Intf: IJSObject): '+aPasIntfName+';');
  Code:='class function '+aClassName+'.Cast(Intf: IJSObject): '+aPasIntfName+';'+sLineBreak;
  Code:=Code+'begin'+sLineBreak;
  Code:=Code+'  Result:='+aClassName+'.Cast(Intf);'+sLineBreak;
  Code:=Code+'end;'+sLineBreak;
  IncludeImplementationCode.Add(Code);
end;

function TWebIDLToPasWasmJob.WriteDictionaryField(
  aField: TIDLDictionaryMemberDefinition): Boolean;
var
  N, TN: String;
begin
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
    Result:=inherited WriteForwardClassDef(D);
end;

function TWebIDLToPasWasmJob.WriteFunctionDefinition(
  aDef: TIDLFunctionDefinition): Boolean;
var
  ArgNames: TStringList;

  function CreateLocal(aName: string): string;
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
  FN, RT, Suff, Args, ProcKind, Sig, aClassName, Code, InvokeName,
    InvokeCode, ArgName, TryCode, VarSection, FinallyCode, LocalName,
    WrapperFn, ArgTypeName: String;
  Overloads: TFPObjectList;
  I: Integer;
  AddFuncBody: Boolean;
  ArgDefList: TIDLDefinitionList;
  CurDef, ArgType: TIDLDefinition;
  ArgDef: TIDLArgumentDefinition absolute CurDef;
begin
  Result:=True;
  Data:=aDef.Data as TPasDataWasmJob;
  Suff:='';
  RT:='';
  if (foConstructor in aDef.Options) then
    FN:='New'
  else
    begin
    FN:=GetName(aDef);
    RT:=GetTypeName(aDef.ReturnType,False);
    case RT of
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
    'UnicodeString': InvokeName:='InvokeJSUnicodeStringResult';
    'TJOB_JSValue': InvokeName:='InvokeJSValueResult';
    'void':
      begin
      RT:='';
      InvokeName:='InvokeJSNoResult';
      end;
    else
      InvokeName:='InvokeJSObjectResult';
    end;

    end;
  aClassName:=GetName(aDef.Parent);
  AddFuncBody:=not Data.HasFuncBody;

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
      if (RT='') then
        begin
        if not (foConstructor in aDef.Options) then
          ProcKind:='procedure'
        else
          ProcKind:='constructor';
        Sig:=FN+Args+Suff+';';
        end
      else
        begin
        ProcKind:='function';
        Sig:=FN+Args+': '+RT+Suff+';';
        end;
      AddLn(ProcKind+' '+Sig);

      if not AddFuncBody then continue;

      ArgNames:=TStringList.Create;
      try
        for CurDef in ArgDefList do
          ArgNames.Add(GetName(ArgDef));

        InvokeCode:='';
        if RT<>'' then
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
          if ArgDef.ArgumentType is TIDLSequenceTypeDefDefinition then
            begin
            ArgTypeName:=TIDLSequenceTypeDefDefinition(ArgDef.ArgumentType).ElementType.TypeName;
            ArgType:=FindGlobalDef(ArgTypeName);
            writeln('TWebIDLToPasWasmJob.WriteFunctionDefinition sequence of ',ArgTypeName,' Element=',ArgType<>nil);
            raise EConvertError.Create('not yet supported: passing an array of '+ArgTypeName+' as argument at '+GetDefPos(ArgDef));
            end
          else
            begin
            ArgType:=FindGlobalDef(ArgDef.ArgumentType.TypeName);
            if (ArgType is TIDLFunctionDefinition) and (foCallBack in TIDLFunctionDefinition(ArgType).Options) then
              begin
              LocalName:=CreateLocal('m');
              VarSection:=VarSection+'  '+LocalName+': '+JOB_JSValueTypeNames[jivkMethod]+';'+sLineBreak;
              WrapperFn:='JOBCall'+GetName(TIDLFunctionDefinition(ArgType));
              TryCode:=TryCode+'  '+LocalName+':='+JOB_JSValueTypeNames[jivkMethod]+'.Create(TMethod('+ArgName+'),@'+WrapperFn+');'+sLineBreak;
              FinallyCode:=FinallyCode+'    '+LocalName+'.free;'+sLineBreak;
              ArgName:=LocalName;
              end;
            end;
          Args:=Args+ArgName;
          end;
        Args:=',['+Args+']';

        InvokeCode:=InvokeCode+InvokeName+'('''+aDef.Name+''''+Args+')';

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
    Data.HasFuncBody:=true;
  finally
    Overloads.Free;
  end;
end;

function TWebIDLToPasWasmJob.WriteFunctionTypeDefinition(
  aDef: TIDLFunctionDefinition): Boolean;
var
  FN, RT, ArgName, VarSection, FetchArgs, Params, Call, Code,
    ArgTypeName, GetFunc: String;
  Data: TPasDataWasmJob;
  Args: TIDLDefinitionList;
  ArgDef: TIDLArgumentDefinition;
  ArgNames: TStringList;
  j, i: Integer;
  CurDef, ReturnDef: TIDLDefinition;
begin
  Result:=True;
  FN:=GetName(aDef);
  RT:=GetResolvedTypeName(aDef.ReturnType.TypeName);
  if (RT='void') then
    RT:='';
  ReturnDef:=FindGlobalDef(aDef.ReturnType.TypeName);
  Args:=aDef.Arguments;

  Params:=GetArguments(aDef.Arguments,False);
  if (RT='') then
    AddLn(FN+' = procedure '+Params+';')
  else
    AddLn(FN+' = function '+Params+': '+RT+';');

  Data:=TPasDataWasmJob(aDef.Data);
  if Data.HasFuncBody then exit;
  Data.HasFuncBody:=true;

  ArgNames:=TStringList.Create;
  try
    // create wrapper callback
    Code:='function JOBCall'+Fn+'(const aMethod: TMethod; const H: TJOBCallbackHelper): PByte;'+sLineBreak;
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
      ArgTypeName:=GetResolvedTypeName(ArgDef.ArgumentType.TypeName);

      case ArgTypeName of
      '': raise EWebIDLParser.Create('not yet supported: function type arg['+IntToStr(I)+'] type void at '+GetDefPos(ArgDef));
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
      'UnicodeString': GetFunc:='GetString';
      'TJOB_JSValue': GetFunc:='GetValue';
      else
        CurDef:=FindGlobalDef(ArgDef.ArgumentType.TypeName);
        if CurDef is TIDLInterfaceDefinition then
          GetFunc:='GetObject('+IntfToPasClassName(ArgTypeName)+') as '+ArgTypeName
        else
          begin
          if CurDef<>nil then
            writeln('TWebIDLToPasWasmJob.WriteFunctionTypeDefinition CurDef=',CurDef.ClassName);
          raise EWebIDLParser.Create('not yet supported: function type arg['+IntToStr(I)+'] type '+ArgDef.ArgumentType.TypeName+' at '+GetDefPos(ArgDef));
          end;
      end;

      // declare: var ArgName: ArgTypeName;
      VarSection:=VarSection+'  '+ArgName+': '+ArgTypeName+';'+sLineBreak;

      // get: ArgName:=H.GetX;
      FetchArgs:=FetchArgs+'  '+ArgName+':=H.'+GetFunc+';';

      // pass: ArgName
      if Params<>'' then
        Params:=Params+',';
      Params:=Params+ArgName;

      end;
    if VarSection<>'' then
      Code:=Code+'var'+sLineBreak+VarSection;

    Code:=Code+'begin'+sLineBreak;
    Code:=Code+FetchArgs+sLineBreak;

    Call:=FN+'(aMethod)('+Params+')';
    case RT of
    '': GetFunc:='Result:=H.AllocUndefined('+Call+');';
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
    'UnicodeString': GetFunc:='Result:=H.AllocString('+Call+');';
    //'TJOB_JSValue': ;
    else
      if ReturnDef is TIDLInterfaceDefinition then
        GetFunc:='Result:=H.AllocIntf('+Call+');'
      else
        begin
        if ReturnDef<>nil then
          writeln('TWebIDLToPasWasmJob.WriteFunctionTypeDefinition ReturnDef=',ReturnDef.ClassName);
        raise EWebIDLParser.Create('not yet supported: function type result type "'+RT+'" at '+GetDefPos(aDef));
        end;
    end;
    Code:=Code+'  '+GetFunc+sLineBreak;
    Code:=Code+'end;'+sLineBreak;

    IncludeImplementationCode.Add(Code);
  finally
  end;
end;

function TWebIDLToPasWasmJob.WritePrivateGetter(Attr: TIDLAttributeDefinition
  ): boolean;
var
  FuncName, TypeName, aClassName, Code, ReadFuncName, Call: String;
  Data: TPasDataWasmJob;
begin
  Result:=true;
  if Attr.AttributeType=nil then
    exit;
  Data:=Attr.Data as TPasDataWasmJob;

  FuncName:=GetterPrefix+GetName(Attr);
  TypeName:=GetTypeName(Attr.AttributeType);

  AddLn('function '+FuncName+': '+TypeName+';');

  if Data.GetterBody<>'' then exit;

  aClassName:=GetName(Attr.Parent);
  Call:='';

  case TypeName of
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
  'UnicodeString': ReadFuncName:='ReadJSPropertyUnicodeString';
  'TJOB_JSValue': ReadFuncName:='ReadJSPropertyValue';
  else
    Call:='ReadJSPropertyObject('+Attr.Name+','''+GetTypeName(Attr.AttributeType)+''')';
  end;

  if Call='' then
    Call:=ReadFuncName+'('''+Attr.Name+''')';

  Code:='function '+aClassName+'.'+FuncName+': '+TypeName+';'+sLineBreak;
  Code:=Code+'begin'+sLineBreak;
  Code:=Code+'  Result:='+Call+';'+sLineBreak;
  Code:=Code+'end;'+sLineBreak;

  Data.GetterBody:=Code;
  IncludeImplementationCode.Add(Code);
end;

function TWebIDLToPasWasmJob.WritePrivateSetter(Attr: TIDLAttributeDefinition
  ): boolean;
var
  FuncName, TypeName, aClassName, WriteFuncName, Code, Call: String;
  Data: TPasDataWasmJob;
begin
  if aoReadOnly in Attr.Options then
    exit(false);
  if Attr.AttributeType=nil then
    exit;
  Data:=Attr.Data as TPasDataWasmJob;

  Result:=true;
  FuncName:=SetterPrefix+GetName(Attr);
  TypeName:=GetTypeName(Attr.AttributeType);
  AddLn('Procedure '+FuncName+'(const aValue: '+TypeName+');');

  if Data.SetterBody<>'' then exit;

  aClassName:=GetName(Attr.Parent);
  Call:='';

  case TypeName of
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
  'UnicodeString': WriteFuncName:='WriteJSPropertyUnicodeString';
  'TJOB_JSValue': WriteFuncName:='WriteJSPropertyValue';
  else
    WriteFuncName:='WriteJSPropertyObject';
  end;

  if Call='' then
    Call:=WriteFuncName+'('''+Attr.Name+''',aValue)';

  Code:='Procedure '+aClassName+'.'+FuncName+'(const aValue: '+TypeName+');'+sLineBreak;
  Code:=Code+'begin'+sLineBreak;
  Code:=Code+'  '+Call+';'+sLineBreak;
  Code:=Code+'end;'+sLineBreak;

  Data.SetterBody:=Code;
  IncludeImplementationCode.Add(Code);
end;

function TWebIDLToPasWasmJob.WriteProperty(Attr: TIDLAttributeDefinition
  ): boolean;
var
  PropName, TypeName, Code: String;
begin
  if Attr.AttributeType=nil then
    begin
    AddLn('skipping field without type: "'+Attr.Name+'"');
    exit;
    end;
  PropName:=GetName(Attr);
  TypeName:=GetTypeName(Attr.AttributeType);
  Code:='Property '+PropName+': '+TypeName+' read '+GetterPrefix+PropName;
  if not (aoReadOnly in Attr.Options) then
    Code:=Code+' write '+SetterPrefix+PropName;
  AddLn(Code+';');
  Result:=true;
end;

function TWebIDLToPasWasmJob.WriteRecordDef(aDef: TIDLRecordDefinition
  ): Boolean;
begin
  Result:=true;
  AddLn(GetName(aDef)+' = '+ClassPrefix+'Object'+ClassSuffix+';');
end;

constructor TWebIDLToPasWasmJob.Create(ThOwner: TComponent);
begin
  inherited Create(ThOwner);
  PasDataClass:=TPasDataWasmJob;
  FPasInterfacePrefix:='IJS';
  BaseOptions:=BaseOptions+[coDictionaryAsClass];
end;

end.

