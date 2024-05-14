{$IFNDEF FPC_DOTTEDUNITS}
unit webidltowasmstub;
{$ENDIF}

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, WebIdl.Defs, WebIdl.ToPascal, WebIdl.Scanner, WebIdl.ToWasmJob;
  {$ELSE}
  Classes, SysUtils, webidltopas, webidldefs, webidlscanner, webidltowasmjob;
  {$ENDIF}

Type

  { TWebIDLToPasWasmJobStub }

  TWebIDLToPasWasmJobStub = class (TWebIDLToPasWasmJob)
  private
  Protected
    function isStub : Boolean; override;
    procedure WriteTypeDefsAndCallbackImplementations(aList: TIDLDefinitionList); override;
    function BaseUnits: String; override;
    function DottedBaseUnits: String; override;
    procedure WritePropertyField(aParent : TIDLDefinition; aProp :TIDLPropertyDefinition); virtual;
    function WriteDictionaryPrivateFields(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList): Integer; override;
    function WritePrivateReadOnlyFields(aParent: TIDLDefinition;aList: TIDLDefinitionList): Integer; override;
    Procedure WriteFunctionInvokeCodeStub(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition; aInfo : TMethodCallInfo); override;
    Procedure WritePrivateGetterImplementation(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition); override;
    Procedure WritePrivateSetterImplementation(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition); override;
    function WriteUtilityMethods(Intf: TIDLStructuredDefinition): Integer; override;
    procedure WriteUtilityMethodImplementations(aDef: TIDLStructuredDefinition; ML: TIDLDefinitionList); override;
  end;

implementation


function TWebIDLToPasWasmJobStub.BaseUnits: String;
begin
  Result:='SysUtils, Job.Stub';
end;

function TWebIDLToPasWasmJobStub.DottedBaseUnits: String;
begin
  Result:='System.SysUtils, Wasm.Job.Stub';
end;

procedure TWebIDLToPasWasmJobStub.WritePropertyField(aParent : TIDLDefinition; aProp: TIDLPropertyDefinition);
var
  PropName, Code, aTypeName, aResolvedTypeName: TIDLString;
  aType: TIDLDefinition;
  ANT : TPascalNativeType;

begin
  if (aProp.PropertyType=nil) then
    begin
    if not (paStringifier in aProp.PropertyAccess) then
      DoLog('Note: skipping field "'+AProp.Name+'" without type at '+GetDefPos(aProp));
    exit;
    end;
  PropName:=GetPasName(aProp)+IntToStr(ptrint(aProp));
  aType:=GetResolvedType(aProp.PropertyType,ANT,aTypeName,aResolvedTypeName);
  if aType is TIDLInterfaceDefinition then
    aTypeName:=GetPasIntfName(aType);
  Code:='_F'+PropName+': '+aTypeName+';';
  AddLn(Code);
end;


function TWebIDLToPasWasmJobStub.WriteDictionaryPrivateFields(aParent: TIDLStructuredDefinition; aList: TIDLDefinitionList
  ): Integer;
var
  D : TIDLDefinition;
  PD : TIDLPropertyDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if D is TIDLPropertyDefinition then
      if convertdef(d) then
        begin
        WritePropertyField(aParent,PD);
        inc(Result);
        end;
end;

function TWebIDLToPasWasmJobStub.WritePrivateReadOnlyFields(aParent: TIDLDefinition;  aList: TIDLDefinitionList): Integer;

var
  D : TIDLDefinition;
  PD : TIDLPropertyDefinition absolute D;

begin
  Result:=0;
  for D in aList do
    if D is TIDLPropertyDefinition then
      if convertdef(d) then
        WritePropertyField(aParent,PD);
end;


procedure TWebIDLToPasWasmJobStub.WriteFunctionInvokeCodeStub(aParent: TIDLStructuredDefinition; aDef: TIDLFunctionDefinition; aInfo: TMethodCallInfo);

var
  Res : String;
begin
  if Assigned(aDef.ReturnType) then
    Res:=DefaultForNativeType(TPasData(aDef.ReturnType.Data).NativeType,aInfo.ReturnTypeName)
  else
    Res:='';
  if Res<>'' then
    AddLn('Result:='+Res+';');
end;

procedure TWebIDLToPasWasmJobStub.WritePrivateGetterImplementation(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition
  );
var
  TypeName,PropName,aClassName : String;
  Info : TAccessorInfo;
  aTypeName, aResolvedTypeName: TIDLString;
  aType: TIDLDefinition;
  ANT : TPascalNativeType;
  ObjClassName : String;

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
//  Call:=GetReadPropertyCall(Info,aProp.Name);
  aType:=GetResolvedType(aProp.PropertyType,ANT,aTypeName,aResolvedTypeName);
  PropName:=GetPasName(aProp)+IntToStr(ptrint(aProp));
  Addln('function '+aClassName+'.'+info.FuncName+': '+Info.NativeTypeName+';');
  Addln('begin');
  Indent;
  if aType is TIDLInterfaceDefinition then
    begin
    ObjClassName:=GetPasName(Info.PropType);
    if (ObjClassName='') or (Pos(PasInterfacePrefix,ObjClassName)=1) then
      ObjClassName:=IntfToPasClassName(ObjClassName)
    else if (Info.PropType is TIDLTypeDefDefinition) then
      begin
      // Check if we have a typedef for an aliased type. Example: BigInteger = Uint8Array
      // must result in TJSUint8Array.
      TypeName:=TIDLTypeDefDefinition(Info.PropType).TypeName;
      TypeName:=TypeAliases.Values[TypeName];
      if TypeName<>'' then
        ObjClassName:=IntfToPasClassName(TypeName)
      end;
    Addln('If (Nil=_F'+PropName+') then');
    Indent;
    Addln('_F'+PropName+':='+ObjClassName+'.CreateEmpty();');
    Undent;
    end;
  Addln('Result:=_F'+PropName+';');
  Undent;
  Addln('end;');
end;

procedure TWebIDLToPasWasmJobStub.WritePrivateSetterImplementation(aParent: TIDLStructuredDefinition; aProp: TIDLPropertyDefinition);

var
  aClassName, PropName : String;
  Info : TAccessorInfo;
begin
  if Not (paWrite in aProp.PropertyAccess) then
    exit;
  if aProp.PropertyType=nil then
    exit;
  aClassName:=GetPasName(aParent);
  if not GetPrivateSetterInfo(aProp,Info) then
    exit;
  Addln('procedure %s.%s(const aValue : %s);',[aClassName,info.FuncName,Info.NativeTypeName]);
  Addln('begin');
  indent;
  PropName:=GetPasName(aProp)+IntToStr(ptrint(aProp));
  Addln('_F'+PropName+':=aValue;');
  undent;
  Addln('end;');

end;

function TWebIDLToPasWasmJobStub.isStub: Boolean;
begin
  Result:=True;
end;

procedure TWebIDLToPasWasmJobStub.WriteTypeDefsAndCallbackImplementations(aList: TIDLDefinitionList);

begin
  // Do nothing
end;

function TWebIDLToPasWasmJobStub.WriteUtilityMethods(Intf: TIDLStructuredDefinition ): Integer;
var
  aPasIntfName: TIDLString;
begin
  Result:=0;
  aPasIntfName:=GetPasIntfName(Intf);
  AddLn('class function Cast(const Intf: IJSObject): '+aPasIntfName+';');
end;

procedure TWebIDLToPasWasmJobStub.WriteUtilityMethodImplementations(aDef : TIDLStructuredDefinition; ML : TIDLDefinitionList);

var
  aClassName, aPasIntfName: TIDLString;

begin
  if (ML=Nil) then ; // Silence compiler warning
  aClassName:=GetPasName(aDef);
  aPasIntfName:=GetPasIntfName(aDef);
  AddLn('class function %s.Cast(const Intf: IJSObject): %s;',[aClassName,aPasIntfName]);
  AddLn('begin');
  Indent;
  AddLn('Result:=Intf as %s;',[aPasIntfName]);
  Undent;
  AddLn('end;');
end;



end.

