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

  { TWebIDLToPasWasmJob }

  TWebIDLToPasWasmJob = class(TBaseWebIDLToPas)
  Protected
    function BaseUnits: String; override;
    // Auxiliary routines
    procedure GetOptions(L: TStrings; Full: boolean); override;
    function GetTypeName(const aTypeName: String; ForTypeDef: Boolean=False
      ): String; override;
    // Code generation routines. Return the number of actually written defs.
    function WritePrivateGetters(aList: TIDLDefinitionList): Integer; override;
    function WritePrivateSetters(aList: TIDLDefinitionList): Integer; override;
    function WriteProperties(aList: TIDLDefinitionList): Integer; override;
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

function TWebIDLToPasWasmJob.WritePrivateGetter(Attr: TIDLAttributeDefinition
  ): boolean;
var
  FuncName, TypeName, aClassName, Code, ReadFuncName: String;
begin
  Result:=true;
  if Attr.AttributeType=nil then
    exit;
  FuncName:=GetterPrefix+GetName(Attr);
  TypeName:=GetTypeName(Attr.AttributeType);
  AddLn('Function '+FuncName+': '+TypeName+';');

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

  IncludeImplementationCode.Add(Code);
end;

function TWebIDLToPasWasmJob.WritePrivateSetter(Attr: TIDLAttributeDefinition
  ): boolean;
var
  FuncName, TypeName, aClassName, WriteFuncName, Code: String;
begin
  if aoReadOnly in Attr.Options then
    exit(false);
  if Attr.AttributeType=nil then
    exit;

  Result:=true;
  FuncName:=SetterPrefix+GetName(Attr);
  TypeName:=GetTypeName(Attr.AttributeType);
  AddLn('Procedure '+FuncName+'(const aValue: '+TypeName+');');

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
end;

end.

