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
  Classes, SysUtils, webidldefs, webidltopas;

type
  TJOB_JSValueKind = (
    jjvkUndefined,
    jjvkBoolean,
    jjvkDouble,
    jjvkString,
    jjvkObject,
    jivkMethod
    );
  TJOB_JSValueKinds = set of TJOB_JSValueKind;

const
  JOB_JSValueKindNames: array[TJOB_JSValueKind] of string = (
    'Undefined',
    'Boolean',
    'Double',
    'String',
    'Object',
    'Method'
    );
  JOB_JSValueTypeNames: array[TJOB_JSValueKind] of string = (
    'TJOB_JSValue',
    'TJOB_JSValueBoolean',
    'TJOB_JSValueDouble',
    'TJOB_JSValueString',
    'TJOB_JSValueObject',
    'TJOB_JSValueMethod'
    );
type
  TPasDataWasmJob = class(TPasData)
  public
    GetterBody: String;
    SetterBody: String;
  end;

  { TWebIDLToPasWasmJob }

  TWebIDLToPasWasmJob = class(TBaseWebIDLToPas)
  private
    FPasInterfacePrefix: String;
  Protected
    function BaseUnits: String; override;
    // Auxiliary routines
    function ClassToPasIntfName(const CN: string): string; virtual;
    function ComputeGUID(const Prefix: string; aList: TIDLDefinitionList): string; virtual;
    procedure GetOptions(L: TStrings; Full: boolean); override;
    function GetTypeName(const aTypeName: String; ForTypeDef: Boolean=False
      ): String; override;
    function GetPasIntfName(Intf: TIDLDefinition): string;
    function GetInterfaceDefHead(Intf: TIDLInterfaceDefinition): String;
      override;
    function WriteOtherImplicitTypes(Intf: TIDLInterfaceDefinition; aMemberList: TIDLDefinitionList): Integer;
      override;
    // Code generation routines. Return the number of actually written defs.
    function WritePrivateGetters(aList: TIDLDefinitionList): Integer; override;
    function WritePrivateSetters(aList: TIDLDefinitionList): Integer; override;
    function WriteProperties(aList: TIDLDefinitionList): Integer; override;
    function WriteUtilityMethods(Intf: TIDLInterfaceDefinition): Integer;
      override;
    // Definitions. Return true if a definition was written.
    function WritePrivateGetter(Attr: TIDLAttributeDefinition): boolean; virtual;
    function WritePrivateSetter(Attr: TIDLAttributeDefinition): boolean; virtual;
    function WriteProperty(Attr: TIDLAttributeDefinition): boolean; virtual;
  Public
    constructor Create(ThOwner: TComponent); override;
  Published
    Property BaseOptions;
    Property ClassPrefix;
    Property ClassSuffix;
    Property PasInterfacePrefix: String read FPasInterfacePrefix write FPasInterfacePrefix;
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
  Result:='SysUtils, JOB_WAsm';
end;

function TWebIDLToPasWasmJob.ClassToPasIntfName(const CN: string): string;
begin
  Result:=CN;
  if LeftStr(Result,length(ClassPrefix))=ClassPrefix then
    System.Delete(Result,1,length(ClassPrefix));
  Result:=PasInterfacePrefix+Result;
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
begin
  Case aTypeName of
    'union',
    'any': Result:=JOB_JSValueTypeNames[jjvkUndefined];
  else
    Result:=inherited GetTypeName(aTypeName,ForTypeDef);
    if (Result=aTypeName) and (LeftStr(Result,length(ClassPrefix))<>ClassPrefix) then
      Result:=ClassPrefix+Result+ClassSuffix;
  end;
end;

function TWebIDLToPasWasmJob.GetPasIntfName(Intf: TIDLDefinition): string;
begin
  Result:=ClassToPasIntfName(GetName(Intf));
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
  if aParentName<>'' then
    Result:=Result+aParentName;
  aPasIntfName:=GetPasIntfName(Intf);
  Result:=Result+','+aPasIntfName+')';
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
    ParentName:=GetName(Intf.ParentInterface)
  else
    ParentName:=GetTypeName(Intf.ParentName);
  if ParentName<>'' then
    begin
    ParentName:=ClassToPasIntfName(ParentName);
    Decl:=Decl+'('+ParentName+')';
    end;
  AddLn(Decl);

  Indent;

  // GUID
  AddLn('['''+ComputeGUID(Decl,aMemberList)+''']');

  // private members
  WritePrivateGetters(aMemberList);
  WritePrivateSetters(aMemberList);

  // type cast function Cast:
  AddLn('Function Cast(Intf: IJSObject): '+aPasIntfName+';');

  // public members
  WriteMethodDefs(aMemberList);
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
  AddLn('Function Cast(Intf: IJSObject): '+aPasIntfName+';');
  Code:='Function '+aClassName+'.Cast(Intf: IJSObject): '+aPasIntfName+';'+sLineBreak;
  Code:=Code+'begin'+sLineBreak;
  Code:=Code+'  Result:='+aClassName+'.CreateCast(Intf);'+sLineBreak;
  Code:=Code+'end;'+sLineBreak;
  IncludeImplementationCode.Add(Code);
end;

function TWebIDLToPasWasmJob.WritePrivateGetter(Attr: TIDLAttributeDefinition
  ): boolean;
var
  FuncName, TypeName, aClassName, Code, ReadFuncName: String;
  Data: TPasDataWasmJob;
begin
  Result:=true;
  if Attr.AttributeType=nil then
    exit;
  Data:=Attr.Data as TPasDataWasmJob;

  FuncName:=GetterPrefix+GetName(Attr);
  TypeName:=GetTypeName(Attr.AttributeType);
  AddLn('Function '+FuncName+': '+TypeName+';');

  if Data.GetterBody<>'' then exit;

  aClassName:=GetName(Attr.Parent);

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
    raise EConvertError.Create('not yet implemented: Getter '+Typename);
  end;

  Code:='Function '+aClassName+'.'+FuncName+': '+TypeName+';'+sLineBreak;
  Code:=Code+'begin'+sLineBreak;
  Code:=Code+'  Result:='+ReadFuncName+'('''+Attr.Name+''');'+sLineBreak;
  Code:=Code+'end;'+sLineBreak;

  Data.GetterBody:=Code;
  IncludeImplementationCode.Add(Code);
end;

function TWebIDLToPasWasmJob.WritePrivateSetter(Attr: TIDLAttributeDefinition
  ): boolean;
var
  FuncName, TypeName, aClassName, WriteFuncName, Code: String;
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
    raise EConvertError.Create('not yet implemented: Setter '+Typename);
  end;

  Code:='Procedure '+aClassName+'.'+FuncName+'(const aValue: '+TypeName+');'+sLineBreak;
  Code:=Code+'begin'+sLineBreak;
  Code:=Code+'  '+WriteFuncName+'('''+Attr.Name+''',aValue);'+sLineBreak;
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

constructor TWebIDLToPasWasmJob.Create(ThOwner: TComponent);
begin
  inherited Create(ThOwner);
  PasDataClass:=TPasDataWasmJob;
  FPasInterfacePrefix:='IJS';
end;

end.

