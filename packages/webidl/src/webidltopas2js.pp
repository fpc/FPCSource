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
unit webidltopas2js;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, webidldefs, webidltopas, Contnrs;

type
  TPas2jsConversionOption = (
    p2jcoUseNativeTypeAliases,
    p2jcoExternalConst
    );
  TPas2jsConversionOptions = Set of TPas2jsConversionOption;

const
  Pas2jsConversionOptionNames: array[TPas2jsConversionOption] of string = (
    'UseNativeTypeAliases',
    'ExternalConst'
    );

type

  { TWebIDLToPas2js }

  TWebIDLToPas2js = class(TBaseWebIDLToPas)
  Private
    FPas2jsOptions: TPas2jsConversionOptions;
  Protected
    Function BaseUnits: String; override;
    // Auxiliary routines
    procedure GetOptions(L: TStrings; Full: boolean); override;
    function GetTypeName(const aTypeName: String; ForTypeDef: Boolean=False
      ): String; override;
    function GetInterfaceDefHead(Intf: TIDLInterfaceDefinition): String;
      override;
    // Code generation routines. Return the number of actually written defs.
    function WriteFunctionDefinition(aDef: TIDLFunctionDefinition): Boolean;
      override;
    function WritePrivateReadOnlyFields(aList: TIDLDefinitionList): Integer;
      override;
    function WriteProperties(aList: TIDLDefinitionList): Integer; override;
    // Definitions. Return true if a definition was written.
    function WriteConst(aConst: TIDLConstDefinition): Boolean; override;
    function WriteField(aAttr: TIDLAttributeDefinition): Boolean; override;
    function WritePrivateReadOnlyField(aAttr: TIDLAttributeDefinition): Boolean; virtual;
    function WriteReadonlyProperty(aAttr: TIDLAttributeDefinition): Boolean; virtual;
  Public
    constructor Create(TheOwner: TComponent); override;
    Property Pas2jsOptions: TPas2jsConversionOptions Read FPas2jsOptions Write FPas2jsOptions;
  Published
    Property BaseOptions;
    Property ClassPrefix;
    Property ClassSuffix;
    Property DictionaryClassParent;
    Property FieldPrefix;
    Property IncludeImplementationCode;
    Property IncludeInterfaceCode;
    Property InputFileName;
    Property OutputFileName;
    Property TypeAliases;
    Property Verbose;
    Property WebIDLVersion;
  end;

function Pas2jsConversionOptionsToStr(Opts: TPas2jsConversionOptions): string;

implementation

function Pas2jsConversionOptionsToStr(Opts: TPas2jsConversionOptions): string;
var
  o: TPas2jsConversionOption;
begin
  Result:='';
  for o in Opts do
    begin
    if Result<>'' then Result:=Result+',';
    Result:=Result+Pas2jsConversionOptionNames[o];
    end;
  Result:='['+Result+']';
end;

{ TWebIDLToPas2js }

function TWebIDLToPas2js.BaseUnits: String;
begin
  Result:='SysUtils, JS';
end;

procedure TWebIDLToPas2js.GetOptions(L: TStrings; Full: boolean);
begin
  inherited GetOptions(L, Full);
  L.Add('Extended Options: '+Pas2jsConversionOptionsToStr(Pas2jsOptions));
end;

function TWebIDLToPas2js.GetTypeName(const aTypeName: String;
  ForTypeDef: Boolean): String;

  Function UsePascalType(Const aPascalType: string): String;

  begin
    if (p2jcoUseNativeTypeAliases in Pas2jsOptions) and ForTypeDef then
      Result:=StringReplace(aTypeName,' ','',[rfReplaceAll])
    else
      Result:=aPascalType;
  end;

begin
  Case aTypeName of
    'union': Result:='JSValue';
    'short': Result:=UsePascalType('Integer');
    'long': Result:=UsePascalType('Integer');
    'long long': Result:=UsePascalType('NativeInt');
    'unsigned short': Result:=UsePascalType('Cardinal');
    'unrestricted float': Result:=UsePascalType('Double');
    'unrestricted double': Result:=UsePascalType('Double');
    'unsigned long': Result:=UsePascalType('NativeInt');
    'unsigned long long': Result:=UsePascalType('NativeInt');
    'octet': Result:=UsePascalType('Byte');
    'any': Result:=UsePascalType('JSValue');
    'float': Result:=UsePascalType('Double');
    'double': Result:=UsePascalType('Double');
    'DOMString',
    'USVString',
    'ByteString': Result:=UsePascalType('String');
  else
    Result:=inherited GetTypeName(aTypeName,ForTypeDef);
  end;
end;

function TWebIDLToPas2js.GetInterfaceDefHead(Intf: TIDLInterfaceDefinition
  ): String;
var
  aParentName: String;
begin
  Result:='class external name '+MakePascalString(Intf.Name,True);
  if Assigned(Intf.ParentInterface) then
    aParentName:=GetName(Intf.ParentInterface)
  else
    aParentName:=GetTypeName(Intf.ParentName);
  if aParentName<>'' then
    Result:=Result+' ('+aParentName+')';
end;

function TWebIDLToPas2js.WriteFunctionDefinition(aDef: TIDLFunctionDefinition
  ): Boolean;

Var
  FN,RT,Suff,Args: String;
  Overloads: TFPObjectList;
  I: Integer;

begin
  Result:=True;
  Suff:='';
  RT:='';
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

function TWebIDLToPas2js.WritePrivateReadOnlyFields(aList: TIDLDefinitionList
  ): Integer;

Var
  D: TIDLDefinition;
  A: TIDLAttributeDefinition absolute D;

begin
  Result:=0;
  For D in aList do
    if (D is TIDLAttributeDefinition) then
      if (aoReadOnly in A.Options) then
        if WritePrivateReadOnlyField(A) then
          Inc(Result);
end;

function TWebIDLToPas2js.WriteProperties(aList: TIDLDefinitionList): Integer;
Var
  D: TIDLDefinition;
  A: TIDLAttributeDefinition absolute D;
begin
  Result:=0;
  For D in aList do
    if (D is TIDLAttributeDefinition) then
      if (aoReadOnly in A.Options) then
        if WriteReadOnlyProperty(A) then
          Inc(Result);
end;

function TWebIDLToPas2js.WriteConst(aConst: TIDLConstDefinition): Boolean;

Const
  ConstTypes: Array[TConstType] of String =
     ('Double','NativeInt','Boolean','JSValue','JSValue','JSValue','JSValue','String','JSValue','JSValue');
Var
  S: String;

begin
  Result:=True;
  // Consts cannot be strings
  if p2jcoExternalConst in Pas2jsOptions then
    begin
    S:=ConstTypes[aConst.ConstType];
    Addln('%s: %s;',[GetName(aConst),S])
    end
  else
    Result:=inherited WriteConst(aConst);
end;

function TWebIDLToPas2js.WriteField(aAttr: TIDLAttributeDefinition): Boolean;
Var
  Def,TN,N: String;

begin
  Result:=True;
  N:=GetName(aAttr);
  if aAttr.AttributeType=nil then
    begin
    AddLn('skipping field without type: "'+N+'"');
    exit;
    end;
  TN:=GetTypeName(aAttr.AttributeType);
  if TN='record' then
    TN:='TJSObject';
  if SameText(N,TN) then
    N:='_'+N;
  Def:=Format('%s: %s;',[N,TN]);
  if (N<>aAttr.Name) then
    Def:=Def+Format('external name ''%s'';',[aAttr.Name]);
  AddLn(Def);
end;

function TWebIDLToPas2js.WritePrivateReadOnlyField(
  aAttr: TIDLAttributeDefinition): Boolean;
begin
  AddLn('%s%s: %s; external name ''%s''; ',[FieldPrefix,GetName(aAttr),GetTypeName(aAttr.AttributeType),aAttr.Name]);
  Result:=true;
end;

function TWebIDLToPas2js.WriteReadonlyProperty(aAttr: TIDLAttributeDefinition
  ): Boolean;

Var
  TN,N,PN: String;

begin
  Result:=True;
  N:=GetName(aAttr);
  PN:=N;
  TN:=GetTypeName(aAttr.AttributeType);
  if SameText(PN,TN) then
    PN:='_'+PN;
  AddLn('Property %s: %s Read %s%s; ',[PN,TN,FieldPrefix,N]);
end;

constructor TWebIDLToPas2js.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Switches.Add('modeswitch externalclass');
end;

end.

