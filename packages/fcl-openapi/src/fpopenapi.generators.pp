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
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.DateUtils, System.Contnrs, Pascal.CodeGenerator,
  {$ELSE}
  Classes, SysUtils, strutils, dateutils,  pascodegen,
  {$ENDIF}
  fpjson.schema.types,
  fpjson.schema.Pascaltypes,
  fpjson.schema.codegen,
  fpopenapi.objects,
  fpopenapi.types,
  fpopenapi.pascaltypes;

Const
  DefaultServerProxyName = 'TServerProxy';
  DefaultServerProxyParent = 'TDataModule';
  DefaultServerProxyParentUnit = 'Classes';

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
    function MustSerializeType(aType : TPascalTypeData) : boolean; override;
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
    // Complex response handling
    procedure WriteComplexResultTypes; virtual;
    procedure GenerateResponseKindEnum(aMethod: TAPIServiceMethod); virtual;
    procedure GenerateComplexResultRecord(aMethod: TAPIServiceMethod); virtual;
    procedure GenerateComplexResultRecordImpl(aData: TAPIData); virtual;
    function GetComplexMethodResultType(aMethod: TAPIServiceMethod): string; virtual;
    function GetResponseKindEnumName(aMethod: TAPIServiceMethod): string; virtual;
    function GetResponseFieldName(aResponse: TAPIResponseInfo): string; virtual;
    function NeedsComplexResultTypes: Boolean; virtual;
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
    procedure GenerateComplexResultHandling(aMethod: TAPIServiceMethod); virtual;
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
  public
    procedure Execute(aData: TAPIData); virtual;
  end;

  { TServerServiceModule }

  { TServerProxyServiceModule }

  { TServerProxyServiceModuleCodeGen }

  TServerProxyServiceModuleCodeGen = class(TOpenApiPascalCodeGen)
  private
    FFormFile: Boolean;
    FProxyClassName: string;
    FProxyParentClass: string;
    FProxyParentUnit: string;
    FProxyVarName: String;
    FServiceImplementationUnit: string;
    FServiceInterfaceUnit: string;
    FUseInterfaceType: Boolean;
    FForm : TStrings;
    procedure CheckDefaults;
    function GetProxyVarName: String;
    procedure SetProxyClassName(const aValue: string);
  Protected
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure GenerateModule;
    procedure GenerateFormFile;
    procedure Execute(aData: TAPIData); virtual;
    property ServiceInterfaceUnit: string read FServiceInterfaceUnit write FServiceInterfaceUnit;
    property ServiceImplementationUnit: string read FServiceImplementationUnit write FServiceImplementationUnit;
    property ProxyParentClass: string read FProxyParentClass write FProxyParentClass;
    property ProxyParentUnit: string read FProxyParentUnit write FProxyParentUnit;
    Property UseInterfaceType : Boolean Read FUseInterfaceType Write FUseInterfaceType;
    Property ProxyClassName : string Read FProxyClassName Write SetProxyClassName;
    Property ProxyVarName : String Read GetProxyVarName Write FProxyVarName;
    Property FormFile : Boolean Read FFormFile Write FFormFile;
    Property Form : TStrings Read FForm Write FForm;
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

function TSerializerCodeGen.MustSerializeType(aType: TPascalTypeData): boolean;
begin
  Result:=inherited MustSerializeType(aType);
  if Result and (aType is TAPITypeData) then
     Result:=Not TAPITypeData(aType).BinaryData;
end;

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
    AddTo(lParams, 'aRequest : '+lBodyType);
  if Assigned(aMethod.ResultDataType) and aMethod.ResultDataType.BinaryData then
    AddTo(lParams, 'aResponseStream : TStream');
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
  // For complex responses, use the generated result type
  if aMethod.NeedsComplexResultType then
    Result:=GetComplexMethodResultType(aMethod)
  else
  begin
    // Simple response - use TServiceResult<T>
    Result:=aMethod.ResultDtoType;
    if Result<>'' then
      Result:=Result+'ServiceResult'
    else
      Result:='TVoidServiceResult';
  end;
end;

function TOpenAPIServiceCodeGen.NeedsComplexResultTypes: Boolean;
var
  I, J: Integer;
  lService: TAPIService;
  lMethod: TAPIServiceMethod;
begin
  Result:=False;
  for I:=0 to APIData.ServiceCount-1 do
  begin
    lService:=APIData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      for J:=0 to lService.MethodCount-1 do
      begin
        lMethod:=lService.Methods[J];
        if lMethod.NeedsComplexResultType then
          Exit(True);
      end;
  end;
end;

function TOpenAPIServiceCodeGen.GetComplexMethodResultType(aMethod: TAPIServiceMethod): string;
begin
  Result:='T'+aMethod.Service.ServiceName+aMethod.MethodName+'Result';
end;

function TOpenAPIServiceCodeGen.GetResponseKindEnumName(aMethod: TAPIServiceMethod): string;
begin
  Result:='T'+aMethod.Service.ServiceName+aMethod.MethodName+'ResponseKind';
end;

function TOpenAPIServiceCodeGen.GetResponseFieldName(aResponse: TAPIResponseInfo): string;
begin
  // Generate field name based on status code
  if aResponse.StatusCode = 'default' then
    Result:='DefaultResponse'
  else if aResponse.IsSuccess then
    Result:='Value'
  else
    Result:='Error'+aResponse.StatusCode;
end;

procedure TOpenAPIServiceCodeGen.GenerateResponseKindEnum(aMethod: TAPIServiceMethod);
var
  I: Integer;
  lResp: TAPIResponseInfo;
  lEnumName, lKindName, lPrefix: String;
  lKinds: TStringList;
begin
  lEnumName:=GetResponseKindEnumName(aMethod);
  // Create unique prefix for this method's enum values
  lPrefix:=aMethod.Service.ServiceName+aMethod.MethodName;
  lKinds:=TStringList.Create;
  try
    lKinds.Sorted:=True;
    lKinds.Duplicates:=dupIgnore;
    // Collect unique response kinds
    for I:=0 to aMethod.ResponseCount-1 do
    begin
      lResp:=aMethod.Responses[I];
      if lResp.StatusCode = 'default' then
        lKindName:=lPrefix+'rkDefault'
      else if lResp.IsSuccess then
        lKindName:=lPrefix+'rkSuccess'
      else
        lKindName:=lPrefix+'rkError'+lResp.StatusCode;
      lKinds.Add(lKindName);
    end;
    // Add unexpected kind
    lKinds.Add(lPrefix+'rkUnexpected');
    // Reset sorted so items appear in logical order
    lKinds.Sorted:=False;

    // Generate enum
    Addln('%s = (', [lEnumName]);
    Indent;
    for I:=0 to lKinds.Count-1 do
    begin
      if I < lKinds.Count-1 then
        Addln('%s,', [lKinds[I]])
      else
        Addln('%s', [lKinds[I]]);
    end;
    Undent;
    Addln(');');
    Addln('');
  finally
    lKinds.Free;
  end;
end;

procedure TOpenAPIServiceCodeGen.GenerateComplexResultRecord(aMethod: TAPIServiceMethod);
var
  I: Integer;
  lResp: TAPIResponseInfo;
  lResultName, lEnumName, lFieldName, lTypeName: String;
  lFields: TStringList;
  lStreamFields: TStringList;
begin
  lResultName:=GetComplexMethodResultType(aMethod);
  lEnumName:=GetResponseKindEnumName(aMethod);

  lFields:=TStringList.Create;
  lStreamFields:=TStringList.Create;
  try
    lFields.Duplicates:=dupIgnore;
    lStreamFields.Duplicates:=dupIgnore;

    Addln('%s = record', [lResultName]);
    Indent;
    Addln('public');

    // Public fields (prefixed with F for convention, but accessible for proxy implementation)
    Addln('FStatusCode: Integer;');
    Addln('FStatusText: String;');
    Addln('FContentType: String;');
    Addln('FResponseKind: %s;', [lEnumName]);
    Addln('FRawContent: String;');
    Addln('FRawStream: TStream;');
    lStreamFields.Add('RawStream');  // Track TStream fields for Extract methods

    // Fields for each response type
    for I:=0 to aMethod.ResponseCount-1 do
    begin
      lResp:=aMethod.Responses[I];
      lFieldName:='F'+GetResponseFieldName(lResp);
      if lFields.IndexOf(lFieldName) >= 0 then
        Continue; // Skip duplicate fields
      lFields.Add(lFieldName);

      if lResp.TypeData <> nil then
      begin
        lTypeName:=lResp.TypeData.PascalName;
        Addln('%s: %s;', [lFieldName, lTypeName]);
        // Track TStream fields for Extract methods
        if lResp.TypeData.BinaryData then
          lStreamFields.Add(GetResponseFieldName(lResp));
      end;
    end;

    // Properties (provide cleaner read access to fields)
    Addln('property StatusCode: Integer read FStatusCode;');
    Addln('property StatusText: String read FStatusText;');
    Addln('property ContentType: String read FContentType;');
    Addln('property ResponseKind: %s read FResponseKind;', [lEnumName]);
    Addln('property RawContent: String read FRawContent;');
    Addln('property RawStream: TStream read FRawStream;');

    // Response-specific properties
    lFields.Clear;
    for I:=0 to aMethod.ResponseCount-1 do
    begin
      lResp:=aMethod.Responses[I];
      lFieldName:=GetResponseFieldName(lResp);
      if lFields.IndexOf(lFieldName) >= 0 then
        Continue;
      lFields.Add(lFieldName);

      if lResp.TypeData <> nil then
      begin
        lTypeName:=lResp.TypeData.PascalName;
        Addln('property %s: %s read F%s;', [lFieldName, lTypeName, lFieldName]);
      end;
    end;

    // Helper methods
    Addln('function Success: Boolean; inline;');
    Addln('function IsClientError: Boolean; inline;');
    Addln('function IsServerError: Boolean; inline;');
    Addln('procedure Clear;');

    // Extract methods for TStream fields (transfer ownership to caller)
    for I:=0 to lStreamFields.Count-1 do
      Addln('function Extract%s: TStream;', [lStreamFields[I]]);

    Undent;
    Addln('end;');
    Addln('');
  finally
    lFields.Free;
    lStreamFields.Free;
  end;
end;

procedure TOpenAPIServiceCodeGen.WriteComplexResultTypes;
var
  I, J: Integer;
  lService: TAPIService;
  lMethod: TAPIServiceMethod;
begin
  if not NeedsComplexResultTypes then
    Exit;

  Addln('// Complex response result types');
  Addln('');

  for I:=0 to APIData.ServiceCount-1 do
  begin
    lService:=APIData.Services[I];
    if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
      for J:=0 to lService.MethodCount-1 do
      begin
        lMethod:=lService.Methods[J];
        if lMethod.NeedsComplexResultType then
        begin
          GenerateResponseKindEnum(lMethod);
          GenerateComplexResultRecord(lMethod);
        end;
      end;
  end;
end;

procedure TOpenAPIServiceCodeGen.GenerateComplexResultRecordImpl(aData: TAPIData);
var
  I, J, K: Integer;
  lService: TAPIService;
  lMethod: TAPIServiceMethod;
  lResultName, lFieldName, lTypeName, lEnumName: String;
  lResp: TAPIResponseInfo;
  lFields: TStringList;
  lStreamFields: TStringList;
  lObjectFields: TStringList;
begin
  lFields:=TStringList.Create;
  lStreamFields:=TStringList.Create;
  lObjectFields:=TStringList.Create;
  try
    lFields.Duplicates:=dupIgnore;
    lStreamFields.Duplicates:=dupIgnore;
    lObjectFields.Duplicates:=dupIgnore;

    for I:=0 to aData.ServiceCount-1 do
    begin
      lService:=aData.Services[I];
      if (Self.ServiceName = '') or SameText(lService.ServiceName, Self.ServiceName) then
        for J:=0 to lService.MethodCount-1 do
        begin
          lMethod:=lService.Methods[J];
          if lMethod.NeedsComplexResultType then
          begin
            lResultName:=GetComplexMethodResultType(lMethod);
            lEnumName:=GetResponseKindEnumName(lMethod);

            // Collect field information for Clear method
            lFields.Clear;
            lStreamFields.Clear;
            lObjectFields.Clear;
            lStreamFields.Add('RawStream');  // Always present

            for K:=0 to lMethod.ResponseCount-1 do
            begin
              lResp:=lMethod.Responses[K];
              lFieldName:=GetResponseFieldName(lResp);
              if lFields.IndexOf(lFieldName) >= 0 then
                Continue;
              lFields.Add(lFieldName);

              if lResp.TypeData <> nil then
              begin
                if lResp.TypeData.BinaryData then
                  lStreamFields.Add(lFieldName)
                else if lResp.TypeData.PascalType in [ptSchemaStruct, ptAnonStruct] then
                  lObjectFields.Add(lFieldName);
              end;
            end;

            // Generate Success method
            Addln('function %s.Success: Boolean;', [lResultName]);
            Addln('begin');
            Indent;
            Addln('Result:=(FStatusCode >= 200) and (FStatusCode < 300);');
            Undent;
            Addln('end;');
            Addln('');

            // Generate IsClientError method
            Addln('function %s.IsClientError: Boolean;', [lResultName]);
            Addln('begin');
            Indent;
            Addln('Result:=(FStatusCode >= 400) and (FStatusCode < 500);');
            Undent;
            Addln('end;');
            Addln('');

            // Generate IsServerError method
            Addln('function %s.IsServerError: Boolean;', [lResultName]);
            Addln('begin');
            Indent;
            Addln('Result:=(FStatusCode >= 500) and (FStatusCode < 600);');
            Undent;
            Addln('end;');
            Addln('');

            // Generate Clear method
            Addln('procedure %s.Clear;', [lResultName]);
            Addln('begin');
            Indent;
            Addln('FStatusCode:=0;');
            Addln('FStatusText:='''';');
            Addln('FContentType:='''';');
            Addln('FResponseKind:=Default(%s);', [lEnumName]);
            Addln('FRawContent:='''';');
            // Free TStream fields
            for K:=0 to lStreamFields.Count-1 do
              Addln('FreeAndNil(F%s);', [lStreamFields[K]]);
            // Free object fields
            for K:=0 to lObjectFields.Count-1 do
              Addln('FreeAndNil(F%s);', [lObjectFields[K]]);
            Undent;
            Addln('end;');
            Addln('');

            // Generate Extract methods for TStream fields
            for K:=0 to lStreamFields.Count-1 do
            begin
              lFieldName:=lStreamFields[K];
              Addln('function %s.Extract%s: TStream;', [lResultName, lFieldName]);
              Addln('begin');
              Indent;
              Addln('Result:=F%s;', [lFieldName]);
              Addln('F%s:=nil;', [lFieldName]);
              Undent;
              Addln('end;');
              Addln('');
            end;
          end;
        end;
    end;
  finally
    lFields.Free;
    lStreamFields.Free;
    lObjectFields.Free;
  end;
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
  lMethod: TAPIServiceMethod;

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
      lMethod:=TAPIServiceMethod(lTypes.objects[I]);
      // Skip methods that use complex result types (already defined in WriteComplexResultTypes)
      if lMethod.NeedsComplexResultType then
        Continue;
      lResType:=lMethod.ResultDtoType;
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
  WriteComplexResultTypes;  // Generate complex result types first (enums and records)
  WriteResultTypes;         // Generate simple result type aliases
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
    if AsyncService and NeedsComplexResultTypes then
      GenerateFPCDirectives(['functionreferences', 'advancedrecords'])
    else if AsyncService then
      GenerateFPCDirectives(['functionreferences'])
    else if NeedsComplexResultTypes then
      GenerateFPCDirectives(['advancedrecords'])
    else
      GenerateFPCDirectives();
    Addln('interface');
    Addln('');
    Addln('uses');
    indent;
    Addln(' SysUtils, classes, fpopenapiclient, %s;', [DtoUnit]);
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
    // Generate implementations for complex result record methods
    GenerateComplexResultRecordImpl(aData);
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
  S,lName: string;
  lResultType : TAPITypeData;

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
  if (aMethod.RequestBodyDataType=nil) then
    lBodyArg:=''''''
  else if (not aMethod.RequestBodyDataType.BinaryData) then
    lBodyArg:='aRequest.Serialize'
  else
    lBodyArg:='aRequest';
  if Assigned(aMethod.ResultDataType) and aMethod.ResultDataType.BinaryData then
      Addln('lResponse:=ExecuteRequest(''%s'',lURL,%s,aResponseStream);', [lHTTPMethod, lBodyArg])
    else
      Addln('lResponse:=ExecuteRequest(''%s'',lURL,%s);', [lHTTPMethod, lBodyArg]);

  // Check if we need complex result handling
  if aMethod.NeedsComplexResultType then
    GenerateComplexResultHandling(aMethod)
  else
  begin
    // Simple result handling (backward compatible)
    AddLn('Result:=%s.Create(lResponse);', [GetMethodResultType(aMethod)]);
    lResultType:=aMethod.ResultDataType;
    if (aMethod.ResultDataType=nil) then
      Addln('Result.Value:=Result.Success;')
    else if lResultType.BinaryData then
      Addln('Result.Value:=aResponseStream;')
    else
      begin
      Addln('if Result.Success then');
      indent;
      S:=lResultType.PascalName;
      Case lResultType.Pascaltype of
      ptSchemaStruct,ptAnonStruct,ptArray:
        Addln('Result.Value:=%s.Deserialize(lResponse.Content);', [S]);
      ptString,
      ptJSON:
        Addln('Result.Value:=lResponse.Content;');
      ptBoolean:
        Addln('Result.Value:=StrToBoolDef(lResponse.Content,False);');
      ptInteger:
        Addln('Result.Value:=StrToIntDef(lResponse.Content,0);');
      ptInt64:
        Addln('Result.Value:=StrToInt64Def(lResponse.Content,0);');
      ptDateTime:
        Addln('Result.Value:=ISO8601ToDateDef(lResponse.Content,0);');
      ptFloat32,
      ptFloat64:
        Addln('Result.Value:=StrToFloatDef(lResponse.Content,0.0);');
      ptEnum:
        begin
        Raise EOpenAPi.Create('Enum result not supported');
        end;
      end;
      Undent;
      end;
  end;
  undent;
  Addln('end;');
  Addln('');
end;

procedure TServiceImplementationCodeGen.GenerateComplexResultHandling(aMethod: TAPIServiceMethod);
var
  I: Integer;
  lResp: TAPIResponseInfo;
  lEnumName, lFieldName, lTypeName, lKindName: String;
  lStatusCodes: TStringList;
begin
  lEnumName:=GetResponseKindEnumName(aMethod);

  // Set basic response fields
  Addln('Result.FStatusCode:=lResponse.StatusCode;');
  Addln('Result.FStatusText:=lResponse.StatusText;');
  Addln('Result.FContentType:=lResponse.ContentType;');
  Addln('Result.FRawContent:=lResponse.Content;');
  Addln('Result.FRawStream:=lResponse.ContentStream;');
  Addln('');

  // Generate case statement for status codes
  Addln('case lResponse.StatusCode of');
  Indent;

  lStatusCodes:=TStringList.Create;
  try
    lStatusCodes.Duplicates:=dupIgnore;

    for I:=0 to aMethod.ResponseCount-1 do
    begin
      lResp:=aMethod.Responses[I];
      if lResp.StatusCode = 'default' then
        Continue; // Handle default at the end

      if lStatusCodes.IndexOf(lResp.StatusCode) >= 0 then
        Continue;
      lStatusCodes.Add(lResp.StatusCode);

      // Determine kind name (with method-unique prefix)
      if lResp.IsSuccess then
        lKindName:=aMethod.Service.ServiceName+aMethod.MethodName+'rkSuccess'
      else
        lKindName:=aMethod.Service.ServiceName+aMethod.MethodName+'rkError'+lResp.StatusCode;

      Addln('%s: begin', [lResp.StatusCode]);
      Indent;
      Addln('Result.FResponseKind:=%s.%s;', [lEnumName, lKindName]);

      // Deserialize if we have a type
      if lResp.TypeData <> nil then
      begin
        lFieldName:='F'+GetResponseFieldName(lResp);
        lTypeName:=lResp.TypeData.PascalName;

        if Pos('application/json', LowerCase(lResp.ContentType)) > 0 then
        begin
          case lResp.TypeData.PascalType of
            ptSchemaStruct, ptAnonStruct, ptArray:
              Addln('Result.%s:=%s.Deserialize(lResponse.Content);', [lFieldName, lTypeName]);
            ptString, ptJSON:
              Addln('Result.%s:=lResponse.Content;', [lFieldName]);
            ptBoolean:
              Addln('Result.%s:=StrToBoolDef(lResponse.Content, False);', [lFieldName]);
            ptInteger:
              Addln('Result.%s:=StrToIntDef(lResponse.Content, 0);', [lFieldName]);
            ptInt64:
              Addln('Result.%s:=StrToInt64Def(lResponse.Content, 0);', [lFieldName]);
          end;
        end;
      end;

      Undent;
      Addln('end;');
    end;

    // Default case
    Addln('else begin');
    Indent;

    // Check for 'default' response
    lResp:=aMethod.ResponseByStatusCode('default');
    if lResp <> nil then
    begin
      Addln('Result.FResponseKind:=%s.%s;', [lEnumName, aMethod.Service.ServiceName+aMethod.MethodName+'rkDefault']);
      if lResp.TypeData <> nil then
      begin
        lFieldName:='F'+GetResponseFieldName(lResp);
        lTypeName:=lResp.TypeData.PascalName;
        if Pos('application/json', LowerCase(lResp.ContentType)) > 0 then
          Addln('Result.%s:=%s.Deserialize(lResponse.Content);', [lFieldName, lTypeName]);
      end;
    end
    else
      Addln('Result.FResponseKind:=%s.%s;', [lEnumName, aMethod.Service.ServiceName+aMethod.MethodName+'rkUnexpected']);

    Undent;
    Addln('end;');

  finally
    lStatusCodes.Free;
  end;

  Undent;
  Addln('end;');
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

  AddLn('classes, fpopenapiclient');
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
    Addln('System.SysUtils, System.DateUtils')
  else
    Addln('SysUtils, DateUtils');
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
    Result:=Result+'aRequest : '+aMethod.RequestBodyDataType.PascalName;
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

{ TServerServiceModule }

function TServerProxyServiceModuleCodeGen.GetProxyVarName: String;
begin
  Result:=FProxyVarName;
  if Result='' then
    Result:=Copy(ProxyClassName,2,Length(ProxyClassName)-1);
end;

procedure TServerProxyServiceModuleCodeGen.SetProxyClassName(const aValue: string);
begin
  if FProxyClassName=aValue then Exit;
  FProxyClassName:=aValue;
  CheckDefaults;
end;

constructor TServerProxyServiceModuleCodeGen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm:=TStringList.Create;
  CheckDefaults;
end;

destructor TServerProxyServiceModuleCodeGen.Destroy;
begin
  FreeAndNil(FForm);
  inherited Destroy;
end;

procedure TServerProxyServiceModuleCodeGen.CheckDefaults;
begin
  if FProxyClassName='' then
    FProxyClassName:=DefaultServerProxyName;
  if FProxyParentClass='' then
    FProxyParentClass:=DefaultServerProxyParent;
  if FProxyParentUnit='' then
    FProxyParentUnit:=DefaultServerProxyParentUnit;
end;

procedure TServerProxyServiceModuleCodeGen.GenerateModule;

var
  I: integer;
  lClass,lUnits : String;
  lService: TAPIService;

begin
  GenerateFPCDirectives();
  CheckDefaults;
  Addln('unit %s;', [Self.OutputUnitName]);
  Addln('');
  Addln('interface');
  Addln('');
  Addln('uses');
  indent;
  lUnits:=ServiceInterfaceUnit+', '+ServiceImplementationUnit;
  if not (SameText(ProxyParentUnit,'Classes') or SameText(ProxyParentUnit,'System.Classes')) then
    if DelphiCode then
      lUnits:='System.Classes, '+lUnits
    else
      lUnits:='Classes, fpWebClient, '+lUnits;
  AddLn('%s, %s;', [ProxyParentUnit, lUnits]);
  undent;
  Addln('');
  EnsureSection(csType);
  indent;
  Addln('%s = class(%s)',[ProxyClassName,ProxyParentClass]);
  Addln('private');
  indent;
  Addln('FWebClient : TAbstractWebClient;');
  Addln('FBaseURL : TAbstractWebClient;');
  for I:=0 to APIData.ServiceCount-1 do
    begin
    lService:=APIData.Services[I];
    lClass:=lService.ServiceProxyImplementationClassName;
    Addln('F%s : %s;',[lService.ServiceName,lClass]);
    end;
  if UseInterfaceType then
    for I:=0 to APIData.ServiceCount-1 do
      begin
      lService:=APIData.Services[I];
      lClass:=lService.ServiceProxyImplementationClassName;
      Addln('function Get%s : %s;',[lService.ServiceName,lService.ServiceInterfaceName]);
      end;
  Addln('Procedure SetBaseURL(const aValue : string);');
  undent;
  Addln('protected');
  indent;
  Addln('Procedure CreateServices; virtual;');
  undent;
  Addln('public');
  indent;
  Addln('constructor Create(aOwner : TComponent); override;');
  for I:=0 to APIData.ServiceCount-1 do
    begin
    lService:=APIData.Services[I];
    if UseInterfaceType then
      Addln('Property %s : %s read Get%s;',[lService.ServiceName,lService.ServiceInterfaceName,lService.ServiceName])
    else
      Addln('Property %s : %s read F%s;',[lService.ServiceName,lService.ServiceProxyImplementationClassName,lService.ServiceName]);
    end;
  Addln('Property BaseURL : String Read FBaseURL Write SetBaseURL;',[lService.ServiceName,lService.ServiceInterfaceName,lService.ServiceName]);
  undent;
  Addln('end;');
  undent;
  Addln('');
  if FormFile then
    begin
    Addln('var %s : %s;',[ProxyVarName,ProxyClassName]);
    Addln('');
    end;
  Addln('implementation');
  Addln('');
  Addln('uses');
  indent;
  if DelphiCode then
    Addln('System.SysUtils;')
  else
    Addln('SysUtils;');
  undent;
  if FormFile then
    begin
    Addln('');
    Addln('{$R *.lfm}');
    end;
  Addln('');
  Addln('constructor %s.Create(aOwner : TComponent);',[ProxyClassName]);
  Addln('');
  Addln('begin');
  indent;
  Addln('Inherited;');
  Addln('FWebClient:=DefaultWebClientClass.Create(Self);');
  Addln('CreateServices;');
  undent;
  Addln('end;');
  Addln('');
  Addln('');
  Addln('procedure %s.CreateServices;',[ProxyClassName]);
  Addln('');
  Addln('begin');
  Indent;
  for I:=0 to APIData.ServiceCount-1 do
    begin
    lService:=APIData.Services[I];
    lClass:=lService.ServiceProxyImplementationClassName;
    Addln('F%s:=%s.create(Self);',[lService.ServiceName,lClass]);
    Addln('%s(F%s).WebClient:=FWebClient',[lClass,lService.ServiceName]);
    end;
  undent;
  Addln('end;');
  Addln('');
  Addln('');
  Addln('Procedure %s.SetBaseURL(const aValue : string);',[ProxyClassName]);
  Addln('');
  Addln('begin');
  Indent;
  Addln('FBaseURL:=aValue;');
  for I:=0 to APIData.ServiceCount-1 do
    begin
    lService:=APIData.Services[I];
    Addln('F%s.BaseURL:=aValue;',[lService.ServiceName]);
    end;
  undent;
  Addln('end;');
  Addln('');
  Addln('');
  for I:=0 to APIData.ServiceCount-1 do
    begin
    lService:=APIData.Services[I];
    lClass:=lService.ServiceProxyImplementationClassName;
    Addln('function %s.Get%s : %s;',[ProxyClassName,lService.ServiceName,lService.ServiceInterfaceName]);
    Addln('');
    Addln('begin');
    Indent;
    Addln('Result:=F%s;',[lService.ServiceName]);
    Undent;
    Addln('end;');
    Addln('');
    Addln('');
    end;
  Addln('');
  Addln('end.');
end;

procedure TServerProxyServiceModuleCodeGen.GenerateFormFile;

begin
  With FForm Do
    begin
    Add('object %s: %s',[ProxyVarName,ProxyClassName]);
    Add('  OldCreateOrder = False');
    Add('  Height = 150');
    Add('  HorizontalOffset = 547');
    Add('  VerticalOffset = 323');
    Add('  Width = 150');
    Add('end');
    end;
end;

procedure TServerProxyServiceModuleCodeGen.Execute(aData: TAPIData);
begin
  SetTypeData(aData);
  GenerateModule;
  if FFormFile then
    GenerateFormFile;
end;


end.
