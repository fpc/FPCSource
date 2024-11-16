{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    Open API code generators

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpopenapi.generators;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dateutils, contnrs, pascodegen,
  fpjson.schema.types,
  fpjson.schema.Pascaltypes,
  fpjson.schema.codegen,
  fpopenapi.objects,
  fpopenapi.types,
  fpopenapi.pascaltypes;

type

  { TJSONSchemaCodeGeneratorHelper }

  // Helper class to generate an API unit header
  // And to get access
  TJSONSchemaCodeGeneratorHelper = class helper for TJSONSchemaCodeGenerator
    procedure GenerateAPIheader;
    function ApiData: TAPIData;
  end;

  { TOpenApiPascalCodeGen }

  TOpenApiPascalCodeGen = class(TJSONSchemaCodeGenerator)
  private
    function GetData: TAPIData;
  protected
    procedure GenerateHeader; override;
  public
    property APIData: TAPIData read GetData;
  end;

  { TDtoCodeGen }

  TDtoCodeGen = class(TTypeCodeGenerator)
  protected
    procedure GenerateHeader; override;
  end;

  { TSerializerCodeGen }

  TSerializerCodeGen = class(TSerializerCodeGenerator)
  protected
    procedure GenerateHeader; override;
  end;

  { TIntfCodeGen }

  { TOpenAPIServiceCodeGen }

  TOpenAPIServiceCodeGen = class(TOpenApiPascalCodeGen)
  private
    FDefineServiceResultType: boolean;
    FDtoUnit: string;
    FSerializerUnit: string;
    FServiceName: string;
    FAsync: boolean;
    FServiceRequestIDType: string;
    FServiceResultType: string;
    function GetServiceRequestIDType: string;
    function GetServiceResultType: string;
  protected
    procedure WriteResultTypes; virtual;
    procedure WriteCallbackTypes; virtual;
    procedure GenerateAuxiliaryTypes; virtual;
    procedure GenerateServiceResultType; virtual;
    function GenerateClientServiceMethodDecl(aMethod: TAPIServiceMethod; const aClassName: string): string; virtual;
    procedure GetMethodCallbackTypeNames(aTypes: TStrings); virtual;
    procedure GetMethodResultTypeNames(aTypes: TStrings); virtual;
    function GetMethodResultType(aMethod: TAPIServiceMethod): string; virtual;
    function MethodResultCallBackName(aMethod: TAPIServiceMethod): string; virtual;
    function ParameterToArg(Idx: integer; aParam: TAPIServiceMethodParam): string;  virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property ServiceName: string read FServiceName write FServiceName;
    property DtoUnit: string read FDtoUnit write FDtoUnit;
    property SerializerUnit: string read FSerializerUnit write FSerializerUnit;
    property AsyncService: boolean read FAsync write FAsync;
    property ServiceResultType: string read GetServiceResultType write FServiceResultType;
    property DefineServiceResultType: boolean read FDefineServiceResultType write FDefineServiceResultType;
    property ServiceRequestIDType: string read GetServiceRequestIDType write FServiceRequestIDType;
  end;

  { TServiceInterfaceCodeGen }

  TServiceInterfaceCodeGen = class(TOpenAPIServiceCodeGen)
  protected
    procedure GenerateServiceInterface(aService: TAPIService); virtual;
  public
    procedure Execute(aData: TAPIData); virtual;
  end;

  { TServiceImplementationCodeGen }

  TServiceImplementationCodeGen = class(TOpenAPIServiceCodeGen)
  private
    FParentHasCancelRequest: boolean;
    FServiceInterfaceUnit: string;
    FServiceParentClass: string;
    FServiceParentUnit: string;
    procedure SetServiceInterfaceUnit(AValue: string);
  protected
    procedure GenerateCancelRequest(aService: TAPIService); virtual;
    procedure GenerateConstructor(aService: TAPIService); virtual;
    procedure GenerateServiceImplementationImpl(aService: TAPIService); virtual;
    procedure GenerateServiceMethodImpl(aService: TAPIService; aMethod: TAPIServiceMethod); virtual;
    procedure GenerateURLConstruction(aService: TAPIService; aMethod: TAPIServiceMethod); virtual;
    procedure GenerateServiceImplementationDecl(aService: TAPIService); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute(aData: TAPIData); virtual;
    property ServiceInterfaceUnit: string read FServiceInterfaceUnit write SetServiceInterfaceUnit;
    property ServiceParentClass: string read FServiceParentClass write FServiceParentClass;
    property ServiceParentUnit: string read FServiceParentUnit write FServiceParentUnit;
    property ParentHasCancelRequest: boolean read FParentHasCancelRequest write FParentHasCancelRequest;
  end;

  { TServerModuleCodeGen }

  { TServerCodeGen }

  TServerCodeGen = class(TOpenAPIServiceCodeGen)
  private
    FModuleParentUnit: string;
    function GetModuleParentUnit: string;
  protected
    procedure GenerateServerServiceMethodImpl(lMethod: TAPIserviceMethod; const aClassName: string);
    // needed for service registration
    function GetMethodHandleRequestName(aMethod: TAPIServiceMethod; const aClassName: string): string; virtual;
    function GetServerServiceHandleMethodDecl(aMethod: TAPIServiceMethod; const aClassName: string = ''): string; virtual;
    // Methods for the actual implementation
    function GetMethodParameterDecl(aMethod: TAPIServiceMethod; aParam: TAPIServiceMethodParam): string; virtual;
    function GetMethodArgs(aMethod: TAPIServiceMethod): string; virtual;
    function GetServerServiceMethodDecl(aMethod: TAPIServiceMethod; const aClassName: string = ''): string; virtual;
    procedure GenerateServerServiceImplementationImpl(aService: TAPIService; const aModuleName: string; isAbstract: boolean); virtual;
    procedure GenerateServerServiceImplementationDecl(aService: TAPIService;
      aParentModule, aModuleName: string; isAbstract: boolean; isHandler: boolean); virtual;
  public
    property ModuleParentUnit: string read GetModuleParentUnit write FModuleParentUnit;
  end;

  // This module generates a complete module that handles the HTTP Requests and hands them off.

  { TServerModuleHandlerCodeGen }

  TServerModuleHandlerCodeGen = class(TServerCodeGen)
  private
    FAbstractServiceCalls: boolean;
    FModuleParentClass: string;
    class function ConvertToRouteParams(const aPath: string): string;
    function GetModuleParentCLass: string;
  protected
    procedure GenerateRegisterAPIRoutes(aClassName: string; aService: TAPIService); virtual;
    procedure GenerateServerServiceImplementationImpl(aService: TAPIService; const aModuleName: string; isAbstract: boolean); override;
    procedure WriteConvertArgument(aMethod: TAPIServiceMethod; aParam: TAPIServiceMethodParam); virtual;
    procedure GenerateServerServiceHandleMethodImpl(lMethod: TAPIserviceMethod; const aClassName: string); virtual;
  public
    procedure Execute(aData: TAPIData); virtual;
    property AbstractServiceCalls: boolean read FAbstractServiceCalls write FAbstractServiceCalls;
    property ModuleParentClass: string read GetModuleParentClass write FModuleParentClass;
  end;

  // This module generates a descendant of the server module.
  // Can be used when TServerModuleCodeGen is used with AbstractServiceCalls = True

  { TServerImplementationModuleCodeGen }

  TServerImplementationModuleCodeGen = class(TServerCodeGen)
    //  private
    //    FServerModuleInterfaceUnit: String;
  public
    procedure Execute(aData: TAPIData); virtual;
    //    property ServerModuleInterfaceUnit : String Read FServerModuleInterfaceUnit Write FServerModuleInterfaceUnit;
  end;

implementation

{ TJSONSchemaCodeGeneratorHelper }

procedure TJSONSchemaCodeGeneratorHelper.GenerateAPIheader;
var
  S, lTitle, lDate, lVersion: string;
  lDescription: TStrings;
  I: integer;
begin
  lDescription:=nil;
  lDate:=FormatDateTime('yyyy"-"mm"-"dd hh":"nn', Now);
  lVersion:=APIData.API.Info.Version;
  lTitle:=APIData.API.Info.Title;
  if VerboseHeader and (APIData.API.Info.Description<>'') then
    begin
    lDescription:=TStringList.Create;
    lDescription.Text:=APIData.API.Info.Description;
    end;
  Addln('{ -----------------------------------------------------------------------');
  Indent;
  Addln('Do not edit !');
  Addln('');
  Addln('This file was automatically generated on %s.', [lDate]);
  S:='';
  for I:=1 to ParamCount do
    S:=S+' '+ParamStr(i);
  Addln('Used command-line parameters:');
  Indent;
  Addln(S);
  Undent;
  Addln('Source OpenAPI document data:');
  Indent;
  if lTitle<>'' then
    Addln('Title: %s', [lTitle]);
  if lVersion<>'' then
    Addln('Version: %s', [lVersion]);
  if Assigned(lDescription) then
    begin
    Addln('Description:');
    for S in lDescription do
      AddLn(S);
    end;
  Undent;
  Undent;
  Addln('  -----------------------------------------------------------------------}');
  FreeAndNil(lDescription);
end;

function TJSONSchemaCodeGeneratorHelper.ApiData: TAPIData;
begin
  Result:=TypeData as TAPIData;
end;

{ TOpenAPICodeGen }


function TOpenApiPascalCodeGen.GetData: TAPIData;
begin
  Result:=TypeData as TAPIData;
end;

procedure TOpenApiPascalCodeGen.GenerateHeader;
begin
  GenerateAPIheader;
end;

{ TDtoCodeGen }

procedure TDtoCodeGen.GenerateHeader;
begin
  GenerateAPIheader;
end;

{ TSerializerCodeGen }

procedure TSerializerCodeGen.GenerateHeader;
begin
  GenerateAPIheader;
end;

{ TOpenAPIerviceCodeGen }

function TOpenAPIServiceCodeGen.MethodResultCallBackName(aMethod:
  TAPIServiceMethod): string;
begin
  Result:=GetMethodResultType(aMethod);
  if Result<>'' then
    Result:=Result+'Callback';
end;

function TOpenAPIServiceCodeGen.ParameterToArg(Idx: integer;
  aParam: TAPIServiceMethodParam): string;
begin
  Result:=Format('%s : %s', [aParam.Name, aParam.TypeName]);
  if aParam.DefaultValue<>'' then
    Result:=Result+' = '+aParam.DefaultValue;
end;

constructor TOpenAPIServiceCodeGen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefineServiceResultType:=False;
end;

function TOpenAPIServiceCodeGen.GetServiceResultType: string;
begin
  Result:=FServiceResultType;
  if Result = '' then
    Result:='TServiceResult';
end;

function TOpenAPIServiceCodeGen.GetServiceRequestIDType: string;
begin
  Result:=FServiceRequestIDType;
  if Result = '' then
    Result:='TRESTServiceRequestID';
end;

procedure TOpenAPIServiceCodeGen.GenerateServiceResultType;
begin
  if AsyncService then
    begin
    Addln('%s = string;', [ServiceRequestIDType]);
    Addln('');
    end;
  if not DelphiCode then
    Addln('generic %s<T> = record', [ServiceResultType])
  else
    Addln(' %s<T> = record', [ServiceResultType]);
  Indent;
  Addln('StatusCode : Integer;');
  Addln('StatusText : String;');
  if AsyncService then
    Addln('RequestID : %s;', [ServiceRequestIDType]);
  Addln('Value : T;');
  Undent;
  AddLn('end;');
  AddLn('');
end;

function TOpenAPIServiceCodeGen.GenerateClientServiceMethodDecl(aMethod: TAPIServiceMethod; const aClassName: string): string;

  procedure AddTo(var S: string; const T: string);
  begin
    if T = '' then exit;
    if S<>'' then
      S:=S+'; ';
    S:=S+T;
  end;

var
  lBodyType, lResultType, lName, lParams: string;
  I: integer;

begin
  lParams:='';
  // Non-optional
  for I:=0 to aMethod.ParamCount-1 do
    if aMethod.Param[I].DefaultValue = '' then
      AddTo(lParams, ParameterToArg(I, aMethod.Param[I]));
  if Assigned(aMethod.RequestBodyDataType) then
    lBodyType:=aMethod.RequestBodyDataType.GetTypeName(ntPascal);
  if lBodyType<>'' then
    AddTo(lParams, 'aBody : '+lBodyType);
  if AsyncService then
    AddTo(lParams, 'aCallback : '+MethodResultCallbackName(aMethod));
  // Optional
  for I:=0 to aMethod.ParamCount-1 do
    if aMethod.Param[I].DefaultValue<>'' then
      AddTo(lParams, ParameterToArg(I, aMethod.Param[I]));
  lName:=aMethod.MethodName;
  if aClassName<>'' then
    lName:=aClassName+'.'+lName;
  if AsyncService then
    Result:=Format('Function %s(%s) : %s;', [lName, lParams, ServiceRequestIDType])
  else
    begin
    lResultType:=GetMethodResultType(aMethod);
    Result:=Format('Function %s(%s) : %s;', [lName, lParams, lResultType]);
    end;
end;

procedure TOpenAPIServiceCodeGen.GetMethodCallbackTypeNames(aTypes: TStrings);

var
  I, J: integer;
  lName: string;
  lService: TAPIService;
  lMethod: TAPIServiceMethod;

begin
  for I:=0 to APIData.ServiceCount-1 do
    begin
    lService:=APIData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      for J:=0 to lService.MethodCount-1 do
        begin
        lMethod:=lService.Methods[J];
        if lMethod.ResultDataType<>nil then
          begin
          lName:=MethodResultCallBackName(lMethod);
          if lName<>'TVoidResultCallBack' then
            aTypes.AddObject(lName, lMethod);
          end;
        end;
    end;
end;

procedure TOpenAPIServiceCodeGen.GetMethodResultTypeNames(aTypes: TStrings);

var
  I, J: integer;
  lName: string;
  lService: TAPIService;
  lMethod: TAPIServiceMethod;

begin
  for I:=0 to APIData.ServiceCount-1 do
    begin
    lService:=APIData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      for J:=0 to lService.MethodCount-1 do
        begin
        lMethod:=lService.Methods[J];
        if lMethod.ResultDataType<>nil then
          begin
          lName:=GetMethodResultType(lMethod);
          if lName<>'' then
            aTypes.AddObject(lName, lMethod);
          end;
        end;
    end;
end;

function TOpenAPIServiceCodeGen.GetMethodResultType(aMethod: TAPIServiceMethod): string;

begin
  Result:=aMethod.ResultDtoType;
  if Result<>'' then
    Result:=Result+'ServiceResult'
  else
    Result:='TVoidServiceResult';
end;


{ TServiceInterfaceCodeGen }

procedure TServiceInterfaceCodeGen.GenerateServiceInterface(aService: TAPIService);

var
  I: integer;
  lDecl, lParent: string;
  lMethod: TAPIServiceMethod;

begin
  DoLog('Generating service interface %s (UUID: %s)',
    [aService.ServiceName, aService.ServiceUUID]);
  lParent:=aService.ServiceParentInterface;
  if lParent<>'' then
    lParent:='('+lParent+')';
  Addln('// Service %s', [aService.ServiceInterfaceName]);
  Addln('');
  Addln('%s = interface %s [''%s'']', [aService.ServiceInterfaceName,
    lParent, aService.ServiceUUID]);
  indent;
  for I:=0 to aService.MethodCount-1 do
    begin
    lMethod:=aService.Methods[I];
    lDecl:=GenerateClientServiceMethodDecl(lMethod, '');
    Addln(lDecl);
    end;
  if AsyncService then
    Addln('Procedure CancelRequest(aRequestID : %s);', [ServiceRequestIDType]);
  undent;
  Addln('end;');
  Addln('');
end;


procedure TOpenAPIServiceCodeGen.WriteResultTypes;

var
  I: integer;
  lName, lDef, lResType: string;
  lTypes: TStringList;

begin
  Addln('// Service result types');
  lTypes:=TStringList.Create;
  try
    lTypes.Sorted:=True;
    lTypes.Duplicates:=dupIgnore;
    GetMethodResultTypeNames(lTypes);
    lTypes.Sorted:=False;
    for I:=0 to lTypes.Count-1 do
      begin
      lName:=lTypes[I];
      lResType:=TAPIServiceMethod(lTypes.objects[I]).ResultDtoType;
      lDef:=Format('%s<%s>', [ServiceResultType, lResType]);
      if not DelphiCode then
        lDef:='specialize '+lDef;
      Addln('%s = %s;', [lName, lDef]);
      end;
    Addln('');
  finally
    lTypes.Free;
  end;
end;

procedure TOpenAPIServiceCodeGen.WriteCallbackTypes;

var
  I: integer;
  lName, lDef: string;
  lTypes: TStringList;

begin
  Addln('// Service Callback types');
  lTypes:=TStringList.Create;
  try
    lTypes.Sorted:=True;
    lTypes.Duplicates:=dupIgnore;
    GetMethodCallbackTypeNames(lTypes);
    lTypes.Sorted:=False;
    for I:=0 to lTypes.Count-1 do
      begin
      lName:=lTypes[I];
      lDef:=Format('reference to procedure(aResult : %s)', [lName]);
      Addln('%s = %s;', [lName, lDef]);
      end;
    Addln('');
  finally
    lTypes.Free;
  end;
end;


procedure TOpenAPIServiceCodeGen.GenerateAuxiliaryTypes;

begin
  if DefineServiceResultType then
    GenerateServiceResultType;
  WriteResultTypes;
  if AsyncService then
    WriteCallbackTypes;
end;


procedure TServiceInterfaceCodeGen.Execute(aData: TAPIData);

var
  I: integer;
  lService: TAPIService;

begin
  SetTypeData(aData);
  try
    GenerateHeader;
    Addln('unit %s;', [Self.OutputUnitName]);
    Addln('');
    if AsyncService then
      GenerateFPCDirectives(['functionreferences'])
    else
      GenerateFPCDirectives();
    Addln('interface');
    Addln('');
    Addln('uses');
    indent;
    Addln(' fpopenapiclient, %s;', [DtoUnit]);
    undent;
    Addln('');
    EnsureSection(csType);
    indent;
    GenerateAuxiliaryTypes;
    for I:=0 to aData.ServiceCount-1 do
      begin
      lService:=aData.Services[I];
      if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
        GenerateServiceInterface(lService);
      end;
    undent;
    Addln('');
    Addln('implementation');
    Addln('');
    Addln('end.');
  finally
    SetTypeData(nil);
  end;
end;

{ TServiceImplementationCodeGen }

procedure TServiceImplementationCodeGen.GenerateServiceImplementationDecl(aService: TAPIService);

var
  I: integer;
  lDecl, lParent: string;
  lMethod: TAPIServiceMethod;
  lName: string;

begin
  lName:=aService.ServiceProxyImplementationClassName;
  DoLog('Generating class %s to implement service interface %s', [lName,
    aService.ServiceName]);
  lParent:=ServiceParentClass;
  Addln('// Service %s', [aService.ServiceInterfaceName]);
  Addln('');
  if ServiceInterfaceUnit<>'' then
    Addln('%s = Class (%s,%s)', [lName, lParent, aService.ServiceInterfaceName])
  else
    Addln('%s = Class (%s)', [lName, lParent]);
  Indent;
  for I:=0 to aService.MethodCount-1 do
  begin
    lMethod:=aService.Methods[I];
    lDecl:=GenerateClientServiceMethodDecl(lMethod, '');
    Addln(lDecl);
  end;
  if not ParentHasCancelRequest then
    Addln('Procedure CancelRequest(aRequestID : TServiceRequestID);');
  undent;
  Addln('end;');
  Addln('');
end;

constructor TServiceImplementationCodeGen.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  ServiceParentClass:='TFPOpenAPIServiceClient';
  ServiceParentUnit:='fpopenapiclient';
end;


procedure TServiceImplementationCodeGen.GenerateConstructor(aService: TAPIService);

var
  lName: string;

begin
  lName:=aService.ServiceProxyImplementationClassName;
  Addln('Constructor %s.Create(aOwner : TComponent; aWebClient : TFPAbstractWebClient);',
    [lName]);
  Addln('begin');
  indent;
  Addln('Inherited Create(aOwner);');
  // We can try to put http/authenticator in a parent class ?
  Addln('WebClient:=aWebClient;');
  undent;
  Addln('end;');
  Addln('');
end;


procedure TServiceImplementationCodeGen.SetServiceInterfaceUnit(AValue: string);

begin
  if FServiceInterfaceUnit = AValue then Exit;
  FServiceInterfaceUnit:=AValue;
end;


procedure TServiceImplementationCodeGen.GenerateCancelRequest(aService: TAPIService);

var
  lName: string;

begin
  lName:=aService.ServiceProxyImplementationClassName;
  Addln('Procedure %s.CancelRequest(aRequestID : TServiceRequestID);', [lName]);
  Addln('');
  Addln('begin');
  indent;
  Addln('WebClient.CancelRequest(aRequestID);');
  undent;
  Addln('end;');
  Addln('');
end;


procedure TServiceImplementationCodeGen.GenerateURLConstruction(aService: TAPIService; aMethod: TAPIServiceMethod);

var
  I: integer;
  lParam: TAPIServiceMethodParam;
  lParamName: string;

begin
  Addln('lURL:=BuildEndPointURL(lMethodURL);');
  if aMethod.HasQueryParam then
    Addln('lQuery:='''';');
  if aMethod.Operation.HasKeyWord(okParameters) then
    begin
    for I:=0 to aMethod.Operation.Parameters.Count-1 do
      begin
      lParam:=aMethod.Param[I];
      if lParam.Location = plPath then
        begin
        lParamName:=lParam.OriginalName;
        Addln('lUrl:=ReplacePathParam(lURL,''%s'',%s);',
          [lParamName, lParam.AsStringValue]);
        end;
      end;
    for I:=0 to aMethod.Operation.Parameters.Count-1 do
      begin
      lParam:=aMethod.Param[I];
      if lParam.Location = plQuery then
        begin
        lParamName:=lParam.OriginalName;
        Addln('lQuery:=ConcatRestParam(lQuery,''%s'',%s);', [lParamName, lParam.AsStringValue]);
        end;
      end;
    end;
  if aMethod.HasQueryParam then
    Addln('lURL:=lURL+lQuery;');
end;


procedure TServiceImplementationCodeGen.GenerateServiceMethodImpl(aService: TAPIService; aMethod: TAPIServiceMethod);

var
  lDecl: string;
  lHTTPMethod: string;
  lBodyArg: string;
  lName: string;

begin
  lName:=aService.ServiceProxyImplementationClassName;
  lDecl:=GenerateClientServiceMethodDecl(aMethod, lName);
  Addln(lDecl);
  Addln('');
  Addln('const');
  indent;
  Addln('lMethodURL = ''%s'';', [aMethod.Path.PathComponent]);
  undent;
  Addln('');
  Addln('var');
  indent;
  Addln('lURL : String;');
  Addln('lResponse : TServiceResponse;');
  if aMethod.HasQueryParam then
    Addln('lQuery : String;');
  undent;
  Addln('');
  Addln('begin');
  indent;
  Addln('Result:=Default(%s);', [GetMethodResultType(aMethod)]);
  GenerateURLConstruction(aService, aMethod);
  lHTTPMethod:=aMethod.Operation.PathComponent;
  if aMethod.RequestBodyDataType<>nil then
    lBodyArg:='aBody.Serialize'
  else
    lBodyArg:='''''';
  Addln('lResponse:=ExecuteRequest(''%s'',lURL,%s);', [lHTTPMethod, lBodyArg]);
  AddLn('Result:=%s.Create(lResponse);', [GetMethodResultType(aMethod)]);
  if aMethod.ResultDataType<>nil then
    begin
    Addln('if Result.Success then');
    indent;
    Addln('Result.Value:=%s.Deserialize(lResponse.Content);', [aMethod.ResultDtoType]);
    Undent;
    end
  else
    Addln('Result.Value:=Result.Success;');
  undent;
  Addln('end;');
  Addln('');
end;

procedure TServiceImplementationCodeGen.GenerateServiceImplementationImpl(aService: TAPIService);

var
  I: integer;
  lName: string;

begin
  lName:=aService.ServiceProxyImplementationClassName;
  DoLog('Generating implementation for class %s', [lName]);
  if AsyncService then
    GenerateCancelRequest(aService);
  for I:=0 to aService.MethodCount-1 do
    GenerateServiceMethodImpl(aService, aService.Methods[I]);
end;

procedure TServiceImplementationCodeGen.Execute(aData: TAPIData);

var
  I: integer;
  lService: TAPIService;

begin
  SetTypeData(aData);
  GenerateHeader;
  Addln('unit %s;', [Self.OutputUnitName]);
  Addln('');
  if AsyncService then
    GenerateFPCDirectives(['functionreferences, anonymousfunctions'])
  else
    GenerateFPCDirectives();
  Addln('interface');
  Addln('');
  Addln('uses');
  indent;
  AddLn('fpopenapiclient');
  if ServiceInterfaceUnit<>'' then
    Addln(', %s                     // Service definition ', [ServiceInterfaceUnit]);
  if (ServiceParentUnit<>'') and not SameText(ServiceParentUnit, 'fpopenapiclient') then
    Addln(', %s                     // Service parent class ', [ServiceParentUnit]);
  Addln(', %s;', [DtoUnit]);
  undent;
  Addln('');
  EnsureSection(csType);
  indent;
  if ServiceInterfaceUnit = '' then
    GenerateAuxiliaryTypes;
  for I:=0 to aData.ServiceCount-1 do
    begin
    lService:=aData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      GenerateServiceImplementationDecl(lService);
    end;
  undent;
  Addln('');
  Addln('implementation');
  Addln('');
  Addln('uses');
  indent;
  if DelphiCode then
    Addln('System.SysUtils')
  else
    Addln('SysUtils');
  Addln(', %s;', [SerializerUnit]);
  undent;
  Addln('');
  for I:=0 to aData.ServiceCount-1 do
    begin
    lService:=aData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      GenerateServiceImplementationImpl(lService);
    end;
  Addln('');
  Addln('end.');
end;

{ TServerModuleCodeGen }

function TServerCodeGen.GetMethodHandleRequestName(aMethod: TAPIServiceMethod; const aClassName: string): string;

var
  lMethodName: string;

begin
  lMethodName:=aMethod.MethodName;
  lMethodName:='Handle'+lMethodName+'Request';
  if aClassName<>'' then
    lMethodName:=aClassName+'.'+lMethodName;
  Result:=lMethodName;
end;

function TServerCodeGen.GetServerServiceHandleMethodDecl(aMethod: TAPIServiceMethod; const aClassName: string): string;

var
  lMethodName: string;

begin
  lMethodName:=GetMethodHandleRequestName(aMethod, aClassName);
  Result:=Format('Procedure %s(aRequest : TRequest; aResponse : TResponse);',
    [lMethodName]);
  if aclassName = '' then
    Result:=Result+' virtual;';
end;


function TServerCodeGen.GetMethodParameterDecl(aMethod: TAPIServiceMethod;
  aParam: TAPIServiceMethodParam): string;

begin
  Result:=aParam.Name+': ';
  Result:=Result+aParam.TypeName;
end;


function TServerCodeGen.GetMethodArgs(aMethod: TAPIServiceMethod): string;

var
  I: integer;

begin
  Result:='';
  for I:=0 to aMethod.ParamCount-1 do
    begin
    if Result<>'' then
      Result:=Result+'; ';
    Result:=Result+GetMethodParameterDecl(aMethod, aMethod.Param[i]);
    end;
  if aMethod.RequestBodyDataType<>nil then
    begin
    if Result<>'' then
      Result:=Result+'; ';
    Result:=Result+'aBody : '+aMethod.RequestBodyDataType.PascalName;
    end;
end;

function TServerCodeGen.GetServerServiceMethodDecl(aMethod: TAPIServiceMethod; const aClassName: string): string;
var
  lMethodArgs: string;
  lMethodName: string;
  lResultType: string;
begin
  lMethodName:=aMethod.MethodName;
  if aClassName<>'' then
    lMethodName:=aClassName+'.'+lMethodName;
  lResultType:=aMethod.ResultDtoType;
  lMethodArgs:=GetMethodArgs(aMethod);
  if lResultType = '' then
    Result:=Format('procedure %s(%s);', [lMethodName, lMethodArgs])
  else
    Result:=Format('function %s(%s) : %s;', [lMethodName, lMethodArgs, lResultType]);
end;


procedure TServerCodeGen.GenerateServerServiceImplementationDecl(aService: TAPIService;
  aParentModule, aModuleName: string; isAbstract: boolean; isHandler: boolean);

var
  lDecl: string;
  lMethod: TAPIServiceMethod;
  I: integer;

begin
  Addln('%s = class(%s)', [aModuleName, aParentModule]);
  Addln('Public');
  Indent;
  if IsHandler then
    begin
    Addln('class Procedure RegisterAPIRoutes(aBaseURL : String; aUseStreaming : Boolean = False); override;');
    for I:=0 to aService.MethodCount-1 do
      begin
      lMethod:=aService.Methods[i];
      lDecl:=GetServerServiceHandleMethodDecl(lMethod, '');
      Addln(lDecl);
      end;
    end;
  AddLn('');

  for I:=0 to aService.MethodCount-1 do
    begin
    lMethod:=aService.Methods[i];
    lDecl:=GetServerServiceMethodDecl(lMethod, '');
    if isHandler then
      begin
      lDecl:=lDecl+' virtual;';
      if isAbstract then
        lDecl:=lDecl+' abstract;';
      end
    else
      lDecl:=lDecl+' override;';

    Addln(lDecl);
    end;
  undent;
  AddLn('end;');
  AddLn('');
end;

class function TServerModuleHandlerCodeGen.ConvertToRouteParams(const aPath: string): string;

begin
  Result:=StringReplace(aPath, '{', ':', [rfReplaceAll]);
  Result:=StringReplace(Result, '}', '', [rfReplaceAll]);
end;

procedure TServerModuleHandlerCodeGen.GenerateRegisterAPIRoutes(aClassName: string; aService: TAPIService);

const
  lRegisterCall = 'RegisterOpenAPIRoute(aBaseURL,''%s'',@%s,aUseStreaming);';

var
  I: integer;
  lMethod: TAPIServiceMethod;
  lDecl, lEndPoint: string;

begin
  Addln('class Procedure %s.RegisterAPIRoutes(aBaseURL : String; aUseStreaming : Boolean = False);', [aClassName]);
  Addln('begin');
  Indent;
  for I:=0 to aService.MethodCount-1 do
    begin
    lMethod:=aService.Methods[i];
    lDecl:=GetMethodHandleRequestName(lMethod, '');
    lEndPoint:=ConvertToRouteParams(lMethod.Path.PathComponent);
    Addln(lRegisterCall, [lEndPoint, lDecl]);
    end;
  Undent;
  Addln('end;');
  Addln('');
end;


procedure TServerModuleHandlerCodeGen.WriteConvertArgument(aMethod: TAPIServiceMethod;
  aParam: TAPIServiceMethodParam);

const
  LocationNames: array[TParamLocation] of string =
    ('alQuery', 'alPath', 'alHeader', 'alCookie');

var
  lDefault: string;
  lLocation: string;
  lLocalName: string;
  lParamName: string;

begin
  lParamName:=aParam.OriginalName;
  lLocalName:='_'+aParam.Name;
  lDefault:=aParam.DefaultValue;
  if lDefault = '' then
    case aParam.ArgType of
      ptString: lDefault:='''''';
      ptInteger: lDefault:='0';
      ptInt64: lDefault:='Int64(0)';
      ptDateTime: lDefault:='TDateTime(0.0)';
      ptFloat32: lDefault:='0.0';
      ptFloat64: lDefault:='0.0';
    end;
  lLocation:=LocationNames[aParam.Location];
  AddLn('%s:=ExtractRequestArgument(aRequest,%s,''%s'',%s);',
    [lLocalName, lLocation, lParamName, lDefault]);
end;

procedure TServerModuleHandlerCodeGen.GenerateServerServiceHandleMethodImpl(lMethod: TAPIserviceMethod; const aClassName: string);

var
  lResultType: string;
  lCallArgs: string;
  i: integer;

  procedure AddToArgs(aName: string);
  begin
    if lCallArgs<>'' then
      lCallargs:=lCallArgs+';';
    lCallargs:=lCallArgs+aName;
  end;

begin
  AddLn(GetServerServiceHandleMethodDecl(lMethod, aClassName));
  lResultType:=lMethod.ResultDtoType;
  Addln('');
  Addln('var');
  indent;
  Addln('lResult : %s;', [lResultType]);
  for I:=0 to lMethod.ParamCount-1 do
    begin
    Addln('_%s;', [GetMethodParameterDecl(lMethod, lMethod.Param[i])]);
    AddToArgs('_'+lMethod.Param[I].Name);
    end;
  if lMethod.RequestBodyDataType<>nil then
    begin
    Addln('_Body : %s;', [lMethod.RequestBodyDataType.PascalName]);
    AddToArgs('_lBody');
    end;
  undent;
  Addln('');
  Addln('begin');
  indent;
  Addln('lResult:=Default(%s);', [lResultType]);
  Addln('try');
  Indent;
  Addln('if PrepareRequest(aRequest,aResponse) then');
  Indent;
  Addln('begin');
  if lResultType<>'' then
    begin
    for I:=0 to lMethod.ParamCount-1 do
      WriteConvertArgument(lMethod, lMethod.Param[i]);
    if lMethod.RequestBodyDataType<>nil then
      AddLn('_lBody:=%s.Deserialize;', [lMethod.RequestBodyDataType.PascalName]);
    Addln('lResult:=%s(%s);', [lMethod.MethodName, lCallArgs]);
    if WriteClassType then
      begin
      Addln('try');
      Indent;
      Addln('aResponse.Content:=lResult.Serialize;');
      end;
    end
  else
    Addln('%s;', [lMethod.MethodName]);
  if (lResultType<>'') and WriteClassType then
    begin
    Undent;
    Addln('finally');
    Indent;
    Addln('FreeAndNil(lResult);');
    Undent;
    Addln('end;'); // Finally
    end;
  Addln('end;'); // if PrepareRequest
  Undent;
  Addln('ProcessResponse(aRequest,aResponse);');
  Undent;
  Addln('except');
  Indent;
  Addln('on E : Exception do');
  Indent;
  Addln('HandleRequestError(E,aRequest,aResponse);');
  Undent;
  Undent;
  Addln('end;'); // except
  undent;
  Addln('end;'); // handlerequest
  Addln('');
end;


procedure TServerCodeGen.GenerateServerServiceMethodImpl(lMethod: TAPIserviceMethod; const aClassName: string);

var
  lResultType, lDecl: string;

begin
  lDecl:=GetServerServiceMethodDecl(lMethod, aClassName);
  lResultType:=lMethod.ResultDtoType;
  AddLn(lDecl);
  Addln('');
  Addln('begin');
  Indent;
  AddLn('Result:=Default(%s);', [lResultType]);
  Undent;
  Addln('end;');
  Addln('');
end;

procedure TServerCodeGen.GenerateServerServiceImplementationImpl(aService: TAPIService; const aModuleName: string; isAbstract: boolean);

var
  lMethod: TAPIServiceMethod;
  I: integer;

begin
  AddLn('');
  if not IsAbstract then
    begin
    for I:=0 to aService.MethodCount-1 do
      begin
      lMethod:=aService.Methods[i];
      GenerateServerServiceMethodImpl(lMethod, aModuleName);
      end;
    AddLn('');
    end;
end;


function TServerModuleHandlerCodeGen.GetModuleParentCLass: string;

begin
  Result:=FModuleParentClass;
  if Result = '' then
    Result:='TFPOpenAPIModule';
end;


function TServerCodeGen.GetModuleParentUnit: string;

begin
  Result:=FModuleParentUnit;
  if Result = '' then
    Result:='fpopenapimodule';
end;


procedure TServerModuleHandlerCodeGen.GenerateServerServiceImplementationImpl(aService: TAPIService; const aModuleName: string; IsAbstract: boolean);

var
  I: integer;
  lMethod: TAPIServiceMethod;

begin
  GenerateRegisterAPIRoutes(aModuleName, aService);
  for I:=0 to aService.MethodCount-1 do
   begin
    lMethod:=aService.Methods[i];
    GenerateServerServiceHandleMethodImpl(lMethod, aModuleName);
   end;
  inherited GenerateServerServiceImplementationImpl(aService, aModuleName, isAbstract);
end;


procedure TServerModuleHandlerCodeGen.Execute(aData: TAPIData);

var
  I: integer;
  lService: TAPIService;
  lName: string;

begin
  SetTypeData(aData);
  GenerateHeader;
  GenerateFPCDirectives();
  Addln('unit %s;', [Self.OutputUnitName]);
  Addln('');
  if AsyncService then
    GenerateFPCDirectives();
  Addln('interface');
  Addln('');
  Addln('uses');
  indent;
  AddLn('%s, httpprotocol, httpdefs, fphttpapp, httproute, %s;',
    [ModuleParentUnit, DtoUnit]);
  undent;
  Addln('');
  EnsureSection(csType);
  indent;
  for I:=0 to aData.ServiceCount-1 do
    begin
    lService:=aData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      begin
      if AbstractServiceCalls then
        lName:='TAbstract'+lService.ServiceName+'Module'
      else
        lName:='T'+lService.ServiceName+'Module';
      GenerateServerServiceImplementationDecl(
        lService, ModuleParentClass, lName, AbstractServiceCalls, True);
      end;
    end;
  undent;
  Addln('');
  Addln('implementation');
  Addln('');
  Addln('uses');
  indent;
  if DelphiCode then
    Addln('System.SysUtils')
  else
    Addln('SysUtils');
  Addln(', %s;', [SerializerUnit]);
  undent;
  Addln('');
  for I:=0 to aData.ServiceCount-1 do
    begin
    lService:=aData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      begin
      if AbstractServiceCalls then
        lName:='TAbstract'+lService.ServiceName+'Module'
      else
        lName:='T'+lService.ServiceName+'Module';
      GenerateServerServiceImplementationImpl(lService, lName, AbstractServiceCalls);
      end;
    end;
  Addln('');
  Addln('end.');
end;

{ TServerImplementationModuleCodeGen }

procedure TServerImplementationModuleCodeGen.Execute(aData: TAPIData);

var
  I: integer;
  lService: TAPIService;
  lName, lParentName: string;

begin
  SetTypeData(aData);
  GenerateHeader;
  GenerateFPCDirectives();
  Addln('unit %s;', [Self.OutputUnitName]);
  Addln('');
  if AsyncService then
    GenerateFPCDirectives();
  Addln('interface');
  Addln('');
  Addln('uses');
  indent;
  AddLn('%s, %s;', [ModuleParentUnit, DtoUnit]);
  undent;
  Addln('');
  EnsureSection(csType);
  indent;
  for I:=0 to aData.ServiceCount-1 do
    begin
    lService:=aData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      begin
      lName:='T'+lService.ServiceName+'Module';
      lParentName:='TAbstract'+lService.ServiceName+'Module';
      GenerateServerServiceImplementationDecl(lService, lParentName, lName, False, False);
      end;
    end;
  undent;
  Addln('');
  Addln('implementation');
  Addln('');
  Addln('uses');
  indent;
  if DelphiCode then
    Addln('System.SysUtils')
  else
    Addln('SysUtils');
  Addln(', %s;', [SerializerUnit]);
  undent;
  Addln('');
  for I:=0 to aData.ServiceCount-1 do
    begin
    lService:=aData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      begin
      lName:='T'+lService.ServiceName+'Module';
      GenerateServerServiceImplementationImpl(lService, lName, False);
      end;
    end;
  Addln('');
  Addln('end.');
end;


end.
