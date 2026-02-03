{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    Master Open API code generator

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpopenapi.codegen;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.StrUtils, System.DateUtils, Pascal.CodeGenerator, System.IniFiles,
  {$ELSE}
  Classes, SysUtils, strutils, dateutils,  pascodegen, inifiles,
  {$ENDIF}
  fpjson.schema.types,
  fpjson.schema.Pascaltypes,
  fpjson.schema.codegen,
  fpopenapi.objects,
  fpopenapi.types,
  fpopenapi.pascaltypes;

Type
  TUnitKind = (ukDto, ukSerialize, ukClientServiceIntf, ukServerServiceHandler, ukClientServiceImpl, ukServerServiceImpl, ukClientParent, ukServerParent, ukServerProxy);

const
  DefaultUnitSuffix = '.{kind}';
  DefaultUnitExtension = '.pas';
  DefaultDtoSuffix = 'Dto';
  DefaultSerializerSuffix = 'Serializer';
  DefaultClientServiceIntfSuffix = 'Service.Intf';
  DefaultClientServiceImplSuffix = 'Service.Impl';
  DefaultServerServiceHandlerSuffix = 'Module.Handler';
  DefaultServerServiceImplSuffix = 'Module.Impl';
  DefaultServiceNamePrefix = '';
  DefaultServiceNameSuffix = 'Service';
  DefaultServerProxyModuleName = 'TServerProxy';
  DefaultServerProxyModuleParentName = 'TDataModule';
  DefaultServerProxyModuleParentUnit = ''; // Depends on Delphicode or not
  DefaultServerProxyUseServiceInterface = True;


  Suffixes : Array[TUnitKind] of string = (
     DefaultDtoSuffix,
     DefaultSerializerSuffix,
     DefaultClientServiceIntfSuffix,
     DefaultServerServiceHandlerSuffix,
     DefaultClientServiceImplSuffix,
     DefaultServerServiceImplSuffix,
     '',
     '',
     'ServerProxy');

type
  { TOpenAPICodeGen }

  TOpenAPICodeGen = class(TComponent)
  private
    FAbstractServiceCalls: Boolean;
    FReservedTypeBehaviour: TReservedTypeBehaviour;
    FReservedTypes: TStrings;
    FAPI: TOpenAPI;
    FAsyncService: boolean;
    FBaseOutputFileName: string;
    FClientParentClass: String;
    FDelphiCode: boolean;
    FGenerateClient: boolean;
    FGenerateServer: boolean;
    FGenerateServerProxyModule: Boolean;
    FOnLog: TSchemaCodeGenLogEvent;
    FParentHasCancelRequest: Boolean;
    FServerParentClass: String;
    FServerProxyFormFile: Boolean;
    FServerProxyModuleName: String;
    FServerProxyModuleParentName: String;
    FServerProxyModuleParentUnit: String;
    FServerProxyUseServiceInterface: Boolean;
    FServiceMap: TStrings;
    FServiceNamePrefix: String;
    FServiceNameSuffix: String;
    FSkipServerServiceImplementationModule: Boolean;
    FUnitExtension: String;
    FUnitSuffix: String;
    FUseEnums: boolean;
    FUUIDMap: TStrings;
    FTypeAliases: TStrings;
    FVerboseHeader: boolean;
    FUnitNames : Array [TUnitKind] of string;
    procedure CleanMaps;
    function GetBaseOutputUnitName: string;
    function GetServerProxyModuleName: String;
    function GetServerProxyModuleParentUnit: String;
    function GetUnitName(AIndex: TUnitKind): String;
    function GetUnitSuffix(aKind: TUnitKind): String;
    procedure SetUnitName(AIndex: TUnitKind; AValue: String);
  protected
    procedure DoLog(const aType: TEventType; const aMessage: string); virtual;
    procedure DoCodeLog(Sender: TObject; LogType: TCodegenLogType; const Msg: string);
    procedure DoLog(const aType: TEventType; const aFmt: string; aArgs: array of const);
    function ResolveUnit(aKind: TUnitKind; FullPath : Boolean = False): String;
    procedure Configure(aCodegen: TJSONSchemaCodeGenerator); virtual;
    function CreateAPIData(aAPI: TOpenAPI): TAPIData; virtual;

    procedure GenerateRecordDefs(aData: TAPIData); virtual;
    procedure GenerateSerializerDefs(aData: TAPIData); virtual;
    procedure GenerateServiceInterface(aData: TAPIData); virtual;
    procedure GenerateServiceImplementation(aData: TAPIData); virtual;
    procedure GenerateServerHandlerModule(aData: TAPIData); virtual;
    procedure GenerateServerModuleImplementation(aData: TAPIData); virtual;
    procedure GenerateServerProxy(aData: TAPIData); virtual;
    procedure GetUUIDMap(aData: TAPIData);
    procedure PrepareAPIData(aData: TAPIData); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Called during create, use to reset.
    procedure DefaultSettings;
    // Load various properties from ini file.
    procedure LoadConfig(aIni: TCustomIniFile; const aSection: String); virtual;
    // Load various properties from ini file using the default section.
    procedure LoadConfig(aConfigFile: String);
    // Write configuration to .ini
    procedure SaveConfig(aIni: TCustomIniFile; const aSection: String); virtual;
    // Write configuration to file
    procedure SaveConfig(aConfigFile: String);
    // Generate code.
    procedure Execute;
    // The OpenAPI description to work with. Set before calling Execute
    property API: TOpenAPI read FAPI write FAPI;
    // Base unit filename
    property BaseOutputFileName: string read FBaseOutputFileName write FBaseOutputFileName;
    // Output filename
    property BaseOutputUnitName: string read GetBaseOutputUnitName;
    // Aliases for types
    property TypeAliases: TStrings read FTypeAliases;
    // InterfaceName:UUID to reuse UUIDS for interfaces.
    property UUIDMap: TStrings read FUUIDMap;
    // Map operationId or verb:path to a ServiceName.MethodName pair.
    property ServiceMap: TStrings read FServiceMap;
    // Generate Dto/Serializer code compilable with Delphi
    property DelphiCode: boolean read FDelphiCode write FDelphiCode;
    // Write command-line options into header
    property VerboseHeader: boolean read FVerboseHeader write FVerboseHeader;
    // User enumerateds (default is to use string)
    property UseEnums: boolean read FUseEnums write FUseEnums;
    // Generate a service implementation
    property GenerateServer: boolean read FGenerateServer write FGenerateServer;
    // Generate a service implementation
    property GenerateClient: boolean read FGenerateClient write FGenerateClient;
    // Use async service calls.
    property AsyncService : boolean Read FAsyncService Write FAsyncService;
    // Diagnostic messages are written to the log
    property OnLog: TSchemaCodeGenLogEvent read FOnLog write FOnLog;
    // Client Service parent has a cancel request method.
    Property ParentHasCancelRequest : Boolean Read FParentHasCancelRequest Write FParentHasCancelRequest;
    // How to construct various unit name suffixes: {kind} placeholder. Default is .{Kind};
    Property UnitSuffix : String read FUnitSuffix Write FUnitSuffix;
    // Extension for unit files.
    Property UnitExtension : String Read FUnitExtension Write FUnitExtension;
    // Dto definition unit file
    Property DtoUnit : String index ukDto Read GetUnitName Write SetUnitName;
    // Dto serializer unit file
    Property SerializeUnit : String index ukSerialize Read GetUnitName Write SetUnitName;
    // Client service interface definition unit name
    Property ClientServiceInterfaceUnit : String index ukClientServiceIntf Read GetUnitName Write SetUnitName;
    // Server service interface definition unit name
    Property ServerServiceInterfaceUnit : String index ukServerServiceHandler Read GetUnitName Write SetUnitName;
    // Client service implementation unit name
    Property ClientServiceImplementationUnit : String index ukClientServiceImpl Read GetUnitName Write SetUnitName;
    // Server service implementation unit name
    Property ServerServiceImplementationUnit : String index ukServerServiceImpl Read GetUnitName Write SetUnitName;
    // Unit containing Client service parent class
    Property ClientServiceParentUnit : String index ukClientParent Read GetUnitName Write SetUnitName;
    // Unit containing Server service parent class
    Property ServerServiceParentUnit : String index ukServerParent Read GetUnitName Write SetUnitName;
    // Client service parent class name
    Property ServerServiceParentClass : String Read FServerParentClass Write FServerParentClass;
    // Server service parent class name
    Property ClientServiceParentClass : String Read FClientParentClass Write FClientParentClass;
    // Should the server service implement the methods as abstract ?
    Property AbstractServiceCalls : Boolean Read FAbstractServiceCalls Write FAbstractServiceCalls;
    // Skip generation of implementation module (only used when AbstractServiceCalls is True
    Property SkipServerServiceImplementationModule : Boolean Read FSkipServerServiceImplementationModule Write FSkipServerServiceImplementationModule;
    // In case of multiple services modules, generate a "server proxy" TDataModule that contains each service as a property?
    Property GenerateServerProxyModule : Boolean Read FGenerateServerProxyModule Write FGenerateServerProxyModule;
    // Server proxy unit name serservice parent class name
    Property ServerProxyUnit : String index ukServerProxy Read GetUnitName Write SetUnitName;
    // Class name for server proxy datamodule.
    Property ServerProxyModuleName : String Read GetServerProxyModuleName Write FServerProxyModuleName;
    // Class name for server proxy parent class.
    Property ServerProxyModuleParentName : String Read FServerProxyModuleParentName Write FServerProxyModuleParentName;
    // Unit name where server proxy parent class is defined.
    Property ServerProxyModuleParentUnit : String Read GetServerProxyModuleParentUnit Write FServerProxyModuleParentUnit;
    // Define service properties using their interface definition.
    Property ServerProxyUseServiceInterface : Boolean Read FServerProxyUseServiceInterface Write FServerProxyUseServiceInterface;
    // Generate form file for ServerProxy module
    Property ServerProxyFormFile : Boolean Read FServerProxyFormFile Write FServerProxyFormFile;
    // Prefix for client/server service name
    Property ServiceNameSuffix : String Read FServiceNameSuffix Write FServiceNameSuffix;
    // Prefix for client/server service name
    Property ServiceNamePrefix : String Read FServiceNamePrefix Write FServiceNamePrefix;
    // How to handle reserved type names that conflict with standard library types
    Property ReservedTypeBehaviour : TReservedTypeBehaviour Read FReservedTypeBehaviour Write FReservedTypeBehaviour;
    // List of reserved type names (one per line, without T prefix)
    Property ReservedTypes : TStrings Read FReservedTypes;
  end;


implementation

uses fpopenapi.generators;

{ TOpenAPICodeGen }

Const
  DefaultSection                 = 'CodeGen';
  KeyUnitSuffix                  = 'UnitSuffix';
  KeyUnitExtension               = 'UnitExtension';
  KeyDTOUnit                     = 'DtoUnit';
  KeySerializeUnit               = 'SerializerUnit';
  KeyClientServiceInterface      = 'ClientServiceInterfaceUnit';
  KeyServerServiceInterface      = 'ServerServiceInterfaceUnit';
  KeyClientServiceImplementation = 'ClientServiceImplementationUnit';
  KeyServerServiceImplementation = 'ServerServiceImplementationUnit';
  KeyClientParentClass           = 'ClientParentClass';
  KeyClientParentUnit            = 'ClientParentUnit';
  KeyServerParentClass           = 'ServerParentClass';
  KeyServerParentUnit            = 'ServerParentUnit';
  KeyParentHasCancelRequest      = 'ParentHasCancelRequest';
  KeyAbstractServiceCalls        = 'AbstractServiceCalls';
  KeyGenerateServerProxyModule   = 'GenerateServerProxyModule';
  KeyServiceNameSuffix           = 'ServiceNameSuffix';
  KeyServiceNamePrefix           = 'ServiceNamePrefix';
  KeyServerProxyModuleName       = 'ServerProxyModuleName';
  KeyServerProxyModuleParentName = 'ServerProxyModuleParentName';
  KeyServerProxyModuleParentUnit = 'ServerProxyModuleParentUnit';
  KeyServerProxyUseServiceInterface = 'ServerProxyModuleUseInterface';
  KeyServerProxyFormFile            = 'ServerProxyFormFile';
  KeyServerProxyUnit                = 'ServerProxyUnit' ;

{ TOpenAPICodeGen }

constructor TOpenAPICodeGen.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FTypeAliases := TStringList.Create;
  FUUIDMap := TStringList.Create;
  FServiceMap := TStringList.Create;
  FReservedTypes := TStringList.Create;
  DefaultSettings;
end;


destructor TOpenAPICodeGen.Destroy;

begin
  FreeAndNil(FTypeAliases);
  FreeAndNil(FUUIDMap);
  FreeAndNil(FServiceMap);
  FreeAndNil(FReservedTypes);
  inherited Destroy;
end;


procedure TOpenAPICodeGen.DefaultSettings;

var
  aKind : TUnitKind;

begin
  GenerateServerProxyModule:=False;
  GenerateServer:=False;
  GenerateClient:=True;
  AbstractServiceCalls:=True;
  ParentHasCancelRequest:=True;
  UnitSuffix:=DefaultUnitSuffix;
  UnitExtension:=DefaultUnitExtension;
  For aKind in TUnitKind do
    FUnitNames[aKind]:='';
  ClientServiceParentClass:='TFPOpenAPIServiceClient';
  ServerServiceParentClass:='TFPOpenAPIModule';
  ClientServiceParentUnit:= 'fpopenapiclient';
  ServerServiceParentUnit:='fpopenapimodule';
  ServiceNamePrefix:=DefaultServiceNamePrefix;
  ServiceNameSuffix:=DefaultServiceNameSuffix;
  ServerProxyModuleName:=DefaultServerProxyModuleName;
  ServerProxyModuleParentName:=DefaultServerProxyModuleParentName;
  ServerProxyModuleParentUnit:=DefaultServerProxyModuleParentUnit;
  ServerProxyUseServiceInterface:=DefaultServerProxyUseServiceInterface;
end;

procedure TOpenAPICodeGen.LoadConfig(aIni : TCustomIniFile; const aSection : String);

var
  lSection: String;

begin
  lSection:=aSection;
  if lSection='' then
    lSection:=DefaultSection;
  with aIni do
    begin
    UnitSuffix:=ReadString(lSection,KeyUnitSuffix,UnitSuffix);
    UnitExtension:=ReadString(lSection,KeyUnitExtension,UnitExtension);
    DtoUnit:=ReadString(lSection,KeyDtoUnit,DtoUnit);
    SerializeUnit:=ReadString(lSection,KeySerializeUnit,SerializeUnit);
    ClientServiceInterfaceUnit:=ReadString(lSection,KeyClientServiceInterface,ClientServiceInterfaceUnit);
    ServerServiceInterfaceUnit:=ReadString(lSection,KeyServerServiceInterface,ServerServiceInterfaceUnit);
    ClientServiceImplementationUnit:=ReadString(lSection,KeyClientServiceImplementation,ClientServiceImplementationUnit);
    ServerServiceImplementationUnit:=ReadString(lSection,KeyServerServiceImplementation,ServerServiceImplementationUnit);
    ClientServiceParentClass:=ReadString(lSection,KeyClientParentClass,ClientServiceParentClass);
    ClientServiceParentUnit:=ReadString(lSection,KeyClientParentUnit,ClientServiceParentUnit);
    ServerServiceParentClass:=ReadString(lSection,KeyServerParentClass,ServerServiceParentClass);
    ServerServiceParentUnit:=ReadString(lSection,KeyServerParentUnit,ServerServiceParentUnit);
    ParentHasCancelRequest:=ReadBool(lSection,KeyParentHasCancelRequest,ParentHasCancelRequest);
    AbstractServiceCalls:=ReadBool(lSection,KeyAbstractServiceCalls,AbstractServiceCalls);
    ServiceNameSuffix:=ReadString(lSection,KeyServiceNameSuffix,ServiceNameSuffix);
    ServiceNamePrefix:=ReadString(lSection,KeyServiceNamePrefix,ServiceNamePrefix);

    GenerateServerProxyModule:=ReadBool(lSection,KeyGenerateServerProxyModule,GenerateServerProxyModule);
    ServerProxyModuleName:=ReadString(lSection,KeyServerProxyModuleName,ServerProxyModuleName);
    ServerProxyModuleParentName:=ReadString(lSection,KeyServerProxyModuleParentName,ServerProxyModuleParentName);
    ServerProxyModuleParentUnit:=ReadString(lSection,KeyServerProxyModuleParentName,ServerProxyModuleParentUnit);
    ServerProxyUseServiceInterface:=ReadBool(lSection,KeyServerProxyUseServiceInterface,ServerProxyUseServiceInterface);
    ServerProxyUnit:=ReadString(lSection,KeyServerProxyUnit,ServerProxyUnit);
    ServerProxyFormFile:=ReadBool(lSection,KeyServerProxyFormFile,ServerProxyFormFile);
    end;
end;

procedure TOpenAPICodeGen.LoadConfig(aConfigFile: String);

var
  lIni : TMemIniFile;

begin
  lIni:=TMemIniFile.Create(aConfigFile);
  try
    LoadConfig(lIni,'');
  finally
    lIni.Free;
  end;
end;

procedure TOpenAPICodeGen.SaveConfig(aIni : TCustomIniFile; const aSection : String);

var
  lSection : String;

begin
  lSection:=aSection;
  if lSection='' then
    lSection:=DefaultSection;
  with aIni do
    begin
    WriteString(lSection,KeyUnitSuffix,UnitSuffix);
    WriteString(lSection,KeyUnitExtension,UnitExtension);
    WriteString(lSection,KeyDtoUnit,DtoUnit);
    WriteString(lSection,KeySerializeUnit,SerializeUnit);
    WriteString(lSection,KeyClientServiceInterface,ClientServiceInterfaceUnit);
    WriteString(lSection,KeyServerServiceInterface,ServerServiceInterfaceUnit);
    WriteString(lSection,KeyClientServiceImplementation,ClientServiceImplementationUnit);
    WriteString(lSection,KeyServerServiceImplementation,ServerServiceImplementationUnit);
    WriteString(lSection,KeyClientParentClass,ClientServiceParentClass);
    WriteString(lSection,KeyClientParentUnit,ClientServiceParentUnit);
    WriteString(lSection,KeyServerParentClass,ServerServiceParentClass);
    WriteString(lSection,KeyServerParentUnit,ServerServiceParentUnit);
    WriteBool(lSection,KeyParentHasCancelRequest,ParentHasCancelRequest);
    WriteBool(lSection,KeyAbstractServiceCalls,AbstractServiceCalls);
    WriteString(lSection,KeyServiceNameSuffix,ServiceNameSuffix);
    WriteString(lSection,KeyServiceNamePrefix,ServiceNamePrefix);
    WriteString(lSection,KeyServiceNameSuffix,ServiceNameSuffix);

    WriteString(lSection,KeyServerProxyModuleName,ServerProxyModuleName);
    WriteString(lSection,KeyServerProxyModuleParentUnit,ServerProxyModuleParentUnit);
    WriteString(lSection,KeyServerProxyUnit,ServerProxyUnit);
    WriteBool(lSection,KeyServerProxyUseServiceInterface,ServerProxyUseServiceInterface);
    WriteBool(lSection,KeyServerProxyFormFile,ServerProxyFormFile);
    WriteBool(lSection,KeyGenerateServerProxyModule, GenerateServerProxyModule);
    end;

end;

procedure TOpenAPICodeGen.SaveConfig(aConfigFile: String);

var
  lIni : TMemIniFile;

begin
  lIni:=TMemIniFile.Create(aConfigFile);
  try
    SaveConfig(lIni,'');
    lIni.UpdateFile;
  finally
    lIni.Free;
  end;
end;


procedure TOpenAPICodeGen.DoCodeLog(Sender: TObject; LogType: TCodegenLogType;
  const Msg: string);
begin
  if LogType = cltInfo then
    DoLog(etInfo, Msg)
  else
    DoLog(etDebug, Msg);
end;


function TOpenAPICodeGen.GetUnitSuffix (aKind : TUnitKind) : String;


var
  lSuff : String;

begin
  lSuff:=UnitSuffix;
  if lSuff='' then
    lSuff:=DefaultUnitSuffix;
  Result:=StringReplace(lSuff,'{kind}',Suffixes[aKind],[]);
end;

function TOpenAPICodeGen.ResolveUnit(aKind: TUnitKind; FullPath: Boolean): String;

begin
  Result := FUnitNames[aKind];
  if Result = '' then
    Result := BaseOutputUnitName + GetUnitSuffix(aKind);
  if FullPath then
    Result:=ExtractFilePath(BaseOutputFileName)+Result+UnitExtension;
end;


function TOpenAPICodeGen.GetBaseOutputUnitName: string;
begin
  Result := ExtractFileName(BaseOutputFileName);
end;

function TOpenAPICodeGen.GetServerProxyModuleName: String;
begin
  Result:=FServerProxyModuleName;
  if Result='' then
    Result:='TServerProxy';
end;

function TOpenAPICodeGen.GetServerProxyModuleParentUnit: String;
begin
  Result:=FServerProxyModuleParentUnit;
  if Result='' then
    if DelphiCode then
      Result:='System.Classes'
    else
      Result:='Classes';
end;

function TOpenAPICodeGen.GetUnitName(AIndex: TUnitKind): String;
begin
  Result:=FUnitNames[aIndex];
  if Result='' then

end;

procedure TOpenAPICodeGen.SetUnitName(AIndex: TUnitKind; AValue: String);
begin
  FUnitNames[aIndex]:=aValue;
end;

procedure TOpenAPICodeGen.DoLog(const aType: TEventType; const aMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(aType, aMessage);
end;

procedure TOpenAPICodeGen.DoLog(const aType: TEventType; const aFmt: string;
  aArgs: array of const);
begin
  if Assigned(FOnLog) then
    FOnLog(aType, Format(aFmt, aArgs));
end;

function TOpenAPICodeGen.CreateAPIData(aAPI: TOpenAPI): TAPIData;
begin
  Result := TAPIData.Create(aAPI);
end;

procedure TOpenAPICodeGen.CleanMaps;

  procedure CleanMap(aMap: TStrings);
  var
    I, P: integer;
    S: string;
  begin
    for I := aMap.Count - 1 downto 0 do
    begin
      S := aMap[I];
      P := Pos('#', S);
      if P > 0 then
      begin
        S := Trim(Copy(S, 1, P - 1));
        if S <> '' then
          aMap[I] := S
        else
          aMap.Delete(I);
      end;
    end;
  end;

begin
  CleanMap(FUUIDMap);
  CleanMap(FServiceMap);
end;

procedure TOpenAPICodeGen.PrepareAPIData(aData: TAPIData);
var
  I: integer;
  N, A: string;
begin
  for I := 0 to FTypeAliases.Count - 1 do
  begin
    FTypeAliases.GetNameValue(I, N, A);
    if (N <> '') and (A <> '') then
      aData.AddAliasToTypeMap(ptSchemaStruct, A, N, A, nil);
  end;
  CleanMaps;
  aData.UseEnums := Self.UseEnums;
  aData.CreateDefaultTypeMaps;
  aData.CreateDefaultAPITypeMaps(Self.GenerateServer);
  aData.VoidResultCallbackType:='TVoidResultCallBack';
  if FServiceMap.Count > 0 then
    aData.RecordMethodNameMap(FServiceMap);
  aData.CreateServiceDefs;
  if (FUUIDMap.Count > 0) then
    aData.ApplyUUIDMap(FUUIDMap);
  DoLog(etInfo, 'Found %d Dto types', [aData.APITypeCount]);
  DoLog(etInfo, 'Created %d services', [aData.ServiceCount]);
end;


procedure TOpenAPICodeGen.GetUUIDMap(aData: TAPIData);

var
  lType: TAPITypeData;
  lService: TAPIService;
  I: integer;

begin
  for I := 0 to aData.APITypeCount - 1 do
  begin
    lType := aData.APITypes[I];
    FUUIDMap.Values[lType.SchemaName] := lType.InterfaceUUID;
  end;
  for I := 0 to aData.ServiceCount - 1 do
  begin
    lService := aData.Services[I];
    FUUIDMap.Values[lService.ServiceName] := lService.ServiceUUID;
  end;
end;


procedure TOpenAPICodeGen.Execute;

var
  lAPIData: TAPIData;

begin
  if not assigned(FAPI) then
    Raise EGenAPI.Create('API not set');
  lAPIData := CreateAPIData(FAPI);
  try
    lAPIData.OnLog := Self.OnLog;
    lAPIData.DelphiTypes := Self.DelphiCode;
    lAPIData.ServiceNamePrefix := ServiceNamePrefix;
    lAPIData.ServiceNameSuffix := ServiceNameSuffix;
    lAPIData.ReservedTypeBehaviour := Self.ReservedTypeBehaviour;
    if FReservedTypes.Count > 0 then
      lAPIData.ReservedTypes := Self.FReservedTypes;
    PrepareAPIData(lAPIData);
    GenerateRecordDefs(lAPIData);
    GenerateSerializerDefs(lAPIData);
    if GenerateClient then
      begin
      GenerateServiceInterface(lAPIData);
      GenerateServiceImplementation(lAPIData);
      end;
    if GenerateServer then
      begin
      GenerateServerHandlerModule(lAPIData);
      if AbstractServiceCalls and not SkipServerServiceImplementationModule then
        GenerateServerModuleImplementation(lAPIData);
      end;
    if GenerateServerProxyModule then
      GenerateServerProxy(lAPIData);

    GetUUIDMap(lAPIData);
  finally
    lAPIData.Free;
  end;
end;


procedure TOpenAPICodeGen.Configure(aCodegen: TJSONSchemaCodeGenerator);

begin
  acodegen.OnLog := @DoCodeLog;
  acodegen.DelphiCode := Self.DelphiCode;
  acodegen.VerboseHeader := Self.VerboseHeader;
  acodegen.WriteClassType := True;
  if acodegen is TOpenAPIServiceCodeGen then
    TOpenAPIServiceCodeGen(aCodegen).AsyncService:=Self.AsyncService
end;


procedure TOpenAPICodeGen.GenerateRecordDefs(aData: TAPIData);

var
  codegen: TDtoCodeGen;
  lFileName : String;

begin
  lFileName:=ResolveUnit(ukDto,True);
  DoLog(etInfo, 'Writing Dto definitions to file "%s"', [lFileName]);
  codegen := TDtoCodeGen.Create(Self);
  try
    Configure(codegen);
    codegen.OutputUnitName := ResolveUnit(ukDto);
    codegen.Execute(aData);
    codegen.Source.SaveToFile(lFileName);
  finally
    codegen.Free;
  end;
end;


procedure TOpenAPICodeGen.GenerateSerializerDefs(aData: TAPIData);

var
  codegen: TSerializerCodeGen;
  lFileName : String;

begin
  lFileName:=ResolveUnit(ukSerialize,True);
  DoLog(etInfo, 'Writing serialize helpers to file "%s"', [lFileName]);
  codegen := TSerializerCodeGen.Create(Self);
  try
    Configure(codegen);
    codegen.OutputUnitName := ResolveUnit(ukSerialize);
    codegen.DataUnitName := ResolveUnit(ukDto);
    codegen.Execute(aData);
    codegen.Source.SaveToFile(lFileName);
  finally
    codegen.Free;
  end;
end;


procedure TOpenAPICodeGen.GenerateServiceInterface(aData: TAPIData);

var
  codegen: TServiceInterfaceCodeGen;
  lFileName : String;

begin
  lFileName:=ResolveUnit(ukClientServiceIntf,True);
  DoLog(etInfo, 'Writing service interface to file "%s"', [lFileName]);
  codegen := TServiceInterfaceCodeGen.Create(Self);
  try
    Configure(codegen);
    codegen.OutputUnitName := ResolveUnit(ukClientServiceIntf);
    codegen.DtoUnit := ResolveUnit(ukDto);
    codegen.Execute(aData);
    codegen.Source.SaveToFile(lFileName);
  finally
    codegen.Free;
  end;
end;


procedure TOpenAPICodeGen.GenerateServiceImplementation(aData: TAPIData);

var
  codegen: TServiceImplementationCodeGen;
  lFileName : String;

begin
  lFileName:=ResolveUnit(ukClientServiceImpl,True);
  DoLog(etInfo, 'Writing service implementation to file "%s"', [lFileName]);
  codegen := TServiceImplementationCodeGen.Create(Self);
  try
    Configure(codegen);
    codegen.OutputUnitName := ResolveUnit(ukClientServiceImpl);
    codegen.DtoUnit := ResolveUnit(ukDto);
    codegen.SerializerUnit := ResolveUnit(ukSerialize);
    codegen.ServiceParentClass := ClientServiceParentClass;
    codegen.ServiceParentUnit := ClientServiceParentUnit;
    codegen.ServiceInterfaceUnit := ResolveUnit(ukClientServiceIntf);
    codegen.ParentHasCancelRequest:=Self.ParentHasCancelRequest;
    codegen.Execute(aData);
    codegen.Source.SaveToFile(lFileName);
  finally
    codegen.Free;
  end;
end;

procedure TOpenAPICodeGen.GenerateServerHandlerModule(aData: TAPIData);

var
  codegen: TServerModuleHandlerCodeGen;
  lFileName : string;
  lKind : TUnitKind;

begin
  if Self.AbstractServiceCalls then
    lKind:=ukServerServiceHandler
  else
    lKind:=ukServerServiceImpl;
  lFileName:=ResolveUnit(lKind,True);
  DoLog(etInfo, 'Writing server HTTP handler module implementation to file "%s"', [lFileName]);
  codegen := TServerModuleHandlerCodeGen.Create(Self);
  try
    Configure(codegen);
    codegen.OutputUnitName := ResolveUnit(lKind);
    codegen.DtoUnit := ResolveUnit(ukDto);
    codegen.SerializerUnit := ResolveUnit(ukSerialize);
    codegen.ModuleParentClass := ServerServiceParentClass;
    codegen.AbstractServiceCalls := Self.AbstractServiceCalls;
    codegen.ModuleParentUnit := ServerServiceParentUnit;
    codegen.Execute(aData);
    codegen.Source.SaveToFile(lFileName);
  finally
    codegen.Free;
  end;
end;



procedure TOpenAPICodeGen.GenerateServerModuleImplementation(aData: TAPIData);

var
  codegen: TServerImplementationModuleCodeGen;
  lFileName : string;

begin
  lFileName:=ResolveUnit(ukServerServiceImpl,True);
  DoLog(etInfo, 'Writing server HTTP module implementation to file "%s"', [lFileName]);
  codegen := TServerImplementationModuleCodeGen.Create(Self);
  try
    Configure(codegen);
    codegen.OutputUnitName := ResolveUnit(ukServerServiceImpl);
    codegen.DtoUnit := ResolveUnit(ukDto);
    codegen.SerializerUnit := ResolveUnit(ukSerialize);
    codegen.ModuleParentUnit := ResolveUnit(ukServerServiceHandler);
    codegen.Execute(aData);
    codegen.Source.SaveToFile(lFileName);
  finally
    codegen.Free;
  end;
end;

procedure TOpenAPICodeGen.GenerateServerProxy(aData: TAPIData);
var
  codegen: TServerProxyServiceModuleCodeGen;
  lFileName : string;

begin
  lFileName:=ResolveUnit(ukServerProxy,True);
  DoLog(etInfo, 'Writing server proxy module implementation to file "%s"', [lFileName]);
  codegen := TServerProxyServiceModuleCodeGen.Create(Self);
  try
    Configure(codegen);
    codegen.OutputUnitName := ResolveUnit(ukServerProxy);
    codegen.ProxyParentClass := ServerProxyModuleParentName;
    codegen.ProxyParentUnit := ServerProxyModuleParentUnit;
    codegen.ProxyClassName := ServerProxyModuleName;
    codegen.UseInterfaceType:=ServerProxyUseServiceInterface;
    codegen.ServiceImplementationUnit := ResolveUnit(ukClientServiceImpl);
    codegen.ServiceInterfaceUnit := ResolveUnit(ukClientServiceIntf);
    codegen.FormFile:=FServerProxyFormFile;
    codegen.Execute(aData);
    codegen.Source.SaveToFile(lFileName);
    if codegen.FormFile then
      codegen.Form.SaveToFile(ChangeFileExt(lFileName,'.lfm'));
  finally
    codegen.Free;
  end;

end;


end.
