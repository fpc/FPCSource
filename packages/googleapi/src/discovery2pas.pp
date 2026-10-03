{
  discovery2pas - Convert Google Discovery documents to Pascal code

  This tool:
  1. Fetches/reads a Google Discovery document
  2. Converts it to OpenAPI 3.1.0 JSON format
  3. Auto-generates a ServiceMap for dotted operationIds
  4. Generates Pascal code using fcl-openapi

  Usage:
    discovery2pas -s drive -o ./output/drive
    discovery2pas -f discovery.json -o ./output/myapi
    discovery2pas -s calendar -w calendar.map  # Write service map only
}
program discovery2pas;

{$mode objfpc}{$H+}

uses
  {$IFDEF FPC_DOTTEDUNITS}
  {$IFDEF UNIX}
  UnixApi.CThreads,
  {$ENDIF}
  System.Classes, System.SysUtils, Fcl.CustApp, FpJson.Data,
  {$ELSE}
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, fpjson,
  {$ENDIF}
  // GoogleDiscovery units
  GoogleDiscovery.Types,
  GoogleDiscovery.Http,
  GoogleDiscovery.Parser,
  GoogleDiscovery.Generate,
  GoogleDiscovery.Main,
  GoogleDiscovery.Logging,
  GoogleDiscovery.ServiceMap,
  // fcl-openapi units
  fpjson.schema.pascaltypes,
  fpopenapi.objects,
  fpopenapi.pascaltypes,
  fpopenapi.reader,
  fpopenapi.codegen,
  // Compatibility generator
  googleapi.compat.generator;

const
  APP_VERSION = '1.0.0';

type
  { Main application class }
  TDiscovery2PasApp = class(TCustomApplication)
  private
    FCodeGen: TOpenAPICodeGen;
    FQuiet: Boolean;
    FKeepOpenAPI: Boolean;
    FOpenAPIFile: string;
    FServiceMapFile: string;
    FWriteMapFile: string;
    FUUIDMapFile: string;
    FConfigFile: string;
    FReservedTypesFile: string;
    FTypeAliasesFile: string;
    FCompatUnitFile: string;
    FInputFile: string;
    FServiceName: string;
    FApiVersion: string;
    FOutputFile: string;
    FProvider: TProviderType;

    procedure DoCodeGenLog(EventType: TEventType; const Msg: RTLString);
    function FetchDiscoveryDocument: TJSONObject;
    function ConvertToOpenAPI(const ADiscoveryJson: TJSONObject): TJSONObject;
    function LoadOrGenerateServiceMap(const AOpenAPIJson: TJSONObject): TStrings;
    procedure GenerateTypeAliasesForLongNames(API: TOpenAPI);
    procedure GeneratePascalCode(const AOpenAPIJson: TJSONObject; AServiceMap: TStrings);
    procedure GenerateCompatibilityUnit(API: TOpenAPI);
    procedure GenerateCompatibilityUnitFromJson(const AOpenAPIJson: TJSONObject);
    procedure WriteOpenAPIFile(const AOpenAPIJson: TJSONObject; const AFileName: string);
  protected
    procedure DoLog(EventType: TEventType; const Msg: RTLString); override;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowHelp;
    procedure ShowVersion;
  end;

{ TDiscovery2PasApp }

constructor TDiscovery2PasApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FCodeGen := TOpenAPICodeGen.Create(Self);
  FCodeGen.GenerateClient := True;
  FCodeGen.GenerateServer := False;
  FProvider := ptGoogleApis;
  FQuiet := False;
  FKeepOpenAPI := False;
end;

destructor TDiscovery2PasApp.Destroy;
begin
  FreeAndNil(FCodeGen);
  inherited Destroy;
end;

procedure TDiscovery2PasApp.DoLog(EventType: TEventType; const Msg: RTLString);
begin
  if FQuiet and (EventType = etInfo) then
    Exit;
  WriteLn(Msg);
end;

procedure TDiscovery2PasApp.DoCodeGenLog(EventType: TEventType; const Msg: RTLString);
begin
  DoLog(EventType, Msg);
end;

procedure TDiscovery2PasApp.ShowHelp;
begin
  WriteLn('discovery2pas v', APP_VERSION, ' - Convert Google Discovery to Pascal');
  WriteLn('');
  WriteLn('Usage: discovery2pas [options]');
  WriteLn('');
  WriteLn('Input Options:');
  WriteLn('  -s, --service=NAME       Service name to fetch (e.g., drive, calendar)');
  WriteLn('  -V, --api-version=VER    API version (default: preferred version)');
  WriteLn('  -f, --file=FILE          Use local discovery JSON file');
  WriteLn('  -p, --provider=NAME      Provider: googleapis (default), firebase, googleworkspace');
  WriteLn('');
  WriteLn('Output Options:');
  WriteLn('  -o, --output=FILE        Base filename for Pascal output (required)');
  WriteLn('  -k, --keep-openapi       Keep intermediate OpenAPI JSON file');
  WriteLn('  -j, --openapi-file=FILE  Specify OpenAPI JSON filename (implies -k)');
  WriteLn('');
  WriteLn('Code Generation Options:');
  WriteLn('  -c, --client             Generate client-side service (default)');
  WriteLn('  -r, --server             Generate server-side module');
  WriteLn('  -d, --delphi             Generate Delphi-compatible code');
  WriteLn('  -e, --enumerated         Use Pascal enumerations');
  WriteLn('  -a, --async              Generate asynchronous service calls');
  WriteLn('  -L, --local-time         Convert date-time values to/from local time');
  WriteLn('                           (default: UTC; date-only values are never converted)');
  WriteLn('  -Q, --qualify-types     Use fully qualified names for reserved types');
  WriteLn('                           (default: escape reserved types with suffix)');
  WriteLn('  -R, --reserved-types=FILE  Load reserved type names from file');
  WriteLn('                           (one name per line, without T prefix)');
  WriteLn('  -T, --type-aliases=FILE  Load type name aliases for shortening long names');
  WriteLn('                           (format: SchemaTypeName=AliasName per line)');
  WriteLn('  -m, --service-map=FILE   Load custom service map');
  WriteLn('  -w, --write-map=FILE     Write auto-generated service map and exit');
  WriteLn('  -u, --uuid-map=FILE      Load/save interface UUIDs');
  WriteLn('  -C, --config=FILE        Load code generator configuration');
  WriteLn('  -b, --compat-unit=FILE   Generate backwards-compatible unit with old-style API');
  WriteLn('                           (type aliases and resource wrapper classes)');
  WriteLn('');
  WriteLn('Other:');
  WriteLn('  -q, --quiet              Less verbose output');
  WriteLn('  -h, --help               Show this help');
  WriteLn('  --version                Show version');
  WriteLn('');
  WriteLn('Examples:');
  WriteLn('  discovery2pas -s drive -o ./output/drive');
  WriteLn('  discovery2pas -s calendar -o ./output/calendar -k');
  WriteLn('  discovery2pas -s sheets -w sheets.map');
  WriteLn('  discovery2pas -f local-discovery.json -o ./output/myapi');
end;

procedure TDiscovery2PasApp.ShowVersion;
begin
  WriteLn('discovery2pas v', APP_VERSION);
  WriteLn('Google Discovery to Pascal code generator');
  WriteLn('');
  WriteLn('Uses:');
  WriteLn('  - GoogleDiscovery library for Discovery->OpenAPI conversion');
  WriteLn('  - fcl-openapi for OpenAPI->Pascal code generation');
end;

function TDiscovery2PasApp.FetchDiscoveryDocument: TJSONObject;
var
  Services: TServiceEntryArray;
  Service: TServiceEntry;
  DiscoveryUrl: string;
  Stream: TFileStream;
  Data: TJSONData;
begin
  Result := nil;

  if FInputFile <> '' then
  begin
    // Load from local file
    Log(etInfo, 'Loading discovery document from: %s', [FInputFile]);
    try
      Stream := TFileStream.Create(FInputFile, fmOpenRead or fmShareDenyWrite);
      try
        Data := GetJSON(Stream);
      finally
        Stream.Free;
      end;
      if Data is TJSONObject then
        Result := TJSONObject(Data)
      else
      begin
        Data.Free;
        Log(etError, 'File %s does not contain a JSON object', [FInputFile]);
      end;
    except
      on E: Exception do
        Log(etError, 'Failed to load file %s: %s', [FInputFile, E.Message]);
    end;
    Exit;
  end;

  if FServiceName = '' then
  begin
    Log(etError, 'No service name or input file specified');
    Exit;
  end;

  // Fetch service list
  Log(etInfo, 'Fetching service list...');
  Services := ListAvailableServices(FProvider);

  if Length(Services) = 0 then
  begin
    Log(etError, 'Failed to fetch service list');
    Exit;
  end;

  // Find the service
  Service := FindService(Services, FServiceName, FApiVersion);

  if Service.Name = '' then
  begin
    Log(etError, 'Service not found: %s %s', [FServiceName, FApiVersion]);
    Exit;
  end;

  Log(etInfo, 'Found service: %s %s', [Service.Name, Service.Version]);

  DiscoveryUrl := Service.DiscoveryRestUrl;
  if DiscoveryUrl = '' then
  begin
    Log(etError, 'No discovery URL for service');
    Exit;
  end;

  // Fetch the discovery document
  Log(etInfo, 'Fetching discovery document from: %s', [DiscoveryUrl]);
  Result := HttpGetJsonObject(DiscoveryUrl);

  if Result = nil then
    Log(etError, 'Failed to fetch discovery document');
end;

function TDiscovery2PasApp.ConvertToOpenAPI(const ADiscoveryJson: TJSONObject): TJSONObject;
var
  Options: TGenerateOptions;
begin
  Log(etInfo, 'Converting to OpenAPI format...');

  Options := TGenerateOptions.Create;
  Options.Provider := FProvider;

  Result := GenerateOpenAPIFromJson(ADiscoveryJson, Options);

  if Result <> nil then
    Log(etInfo, 'OpenAPI conversion complete')
  else
    Log(etError, 'OpenAPI conversion failed');
end;

function TDiscovery2PasApp.LoadOrGenerateServiceMap(const AOpenAPIJson: TJSONObject): TStrings;
begin
  if (FServiceMapFile <> '') and FileExists(FServiceMapFile) then
  begin
    Log(etInfo, 'Loading service map from: %s', [FServiceMapFile]);
    Result := TStringList.Create;
    Result.LoadFromFile(FServiceMapFile);
  end
  else
  begin
    Log(etInfo, 'Auto-generating service map from operationIds...');
    Result := GenerateServiceMap(AOpenAPIJson);
    Log(etInfo, 'Generated %d service mappings', [Result.Count]);
  end;
end;

procedure TDiscovery2PasApp.WriteOpenAPIFile(const AOpenAPIJson: TJSONObject;
  const AFileName: string);
var
  JsonStr: string;
  F: TFileStream;
begin
  Log(etInfo, 'Writing OpenAPI file: %s', [AFileName]);

  JsonStr := AOpenAPIJson.FormatJSON;
  F := TFileStream.Create(AFileName, fmCreate);
  try
    if Length(JsonStr) > 0 then
      F.WriteBuffer(JsonStr[1], Length(JsonStr));
  finally
    F.Free;
  end;
end;

procedure TDiscovery2PasApp.GenerateTypeAliasesForLongNames(API: TOpenAPI);
const
  MaxIdentifierLength = 120;  // Leave room for Array suffix under FPC's 127 limit
  ObjectTypePrefix = 'T';
  ArrayTypeSuffix = 'Array';
var
  I: Integer;
  SchemaName, PascalName, ShortName: string;
  AliasCount: Integer;

  function ApplyGoogleAbbreviations(const aName: string): string;
  begin
    Result := aName;
    // Apply Google-specific abbreviations
    Result := StringReplace(Result, 'GoogleCloud', 'GC', []);
    Result := StringReplace(Result, 'Google', 'G', []);
    // Abbreviate version suffixes
    Result := StringReplace(Result, 'V1alpha1', 'V1a1', []);
    Result := StringReplace(Result, 'V1alpha2', 'V1a2', []);
    Result := StringReplace(Result, 'V1beta1', 'V1b1', []);
    Result := StringReplace(Result, 'V1beta2', 'V1b2', []);
    Result := StringReplace(Result, 'V1main', 'V1m', []);
    Result := StringReplace(Result, 'V2alpha1', 'V2a1', []);
    Result := StringReplace(Result, 'V2beta1', 'V2b1', []);
    // Common long service name abbreviations
    Result := StringReplace(Result, 'Contactcenterinsights', 'CCI', []);
    Result := StringReplace(Result, 'Discoveryengine', 'DE', []);
    Result := StringReplace(Result, 'Certificatemanager', 'CM', []);
    Result := StringReplace(Result, 'Contentwarehouse', 'CW', []);
    Result := StringReplace(Result, 'Networkconnectivity', 'NC', []);
    Result := StringReplace(Result, 'Securitycenter', 'SC', []);
    Result := StringReplace(Result, 'Recommendationengine', 'RE', []);
  end;

begin
  if not Assigned(API.Components) or not Assigned(API.Components.Schemas) then
    Exit;

  AliasCount := 0;
  for I := 0 to API.Components.Schemas.Count - 1 do
  begin
    SchemaName := API.Components.Schemas.Names[I];
    // Calculate what the Pascal type name would be (including Array variant)
    PascalName := ObjectTypePrefix + SchemaName + ArrayTypeSuffix;

    // Check if it would exceed the max length
    if Length(PascalName) > MaxIdentifierLength then
    begin
      // Apply Google-specific abbreviations
      ShortName := ApplyGoogleAbbreviations(SchemaName);

      // Only add alias if abbreviation actually shortened the name
      if (ShortName <> SchemaName) and
         (Length(ObjectTypePrefix + ShortName + ArrayTypeSuffix) <= MaxIdentifierLength) then
      begin
        // Check if alias already exists (from user file)
        if FCodeGen.TypeAliases.IndexOfName(SchemaName) < 0 then
        begin
          FCodeGen.TypeAliases.Add(SchemaName + '=' + ShortName);
          Inc(AliasCount);
        end;
      end;
    end;
  end;

  if AliasCount > 0 then
    Log(etInfo, 'Generated %d type aliases for long schema names', [AliasCount]);
end;

procedure TDiscovery2PasApp.GeneratePascalCode(const AOpenAPIJson: TJSONObject;
  AServiceMap: TStrings);
var
  API: TOpenAPI;
  Reader: TOpenAPIReader;
  JsonStr: TJSONStringType;
begin
  Log(etInfo, 'Generating Pascal code...');

  // Convert JSON to string for reader
  JsonStr := AOpenAPIJson.FormatJSON;

  // Create and populate OpenAPI object
  API := TOpenAPI.Create;
  try
    Reader := TOpenAPIReader.Create(Self);
    try
      Reader.ReadFromString(API, JsonStr);
    finally
      Reader.Free;
    end;

    // Configure code generator
    FCodeGen.OnLog := @DoCodeGenLog;
    FCodeGen.BaseOutputFileName := FOutputFile;
    FCodeGen.API := API;

    // Load UUID map if specified
    if (FUUIDMapFile <> '') and FileExists(FUUIDMapFile) then
    begin
      Log(etInfo, 'Loading UUID map from: %s', [FUUIDMapFile]);
      FCodeGen.UUIDMap.LoadFromFile(FUUIDMapFile);
    end;

    // Apply service map
    if AServiceMap.Count > 0 then
    begin
      FCodeGen.ServiceMap.Assign(AServiceMap);
      Log(etInfo, 'Applied %d service map entries', [AServiceMap.Count]);
    end;

    // Generate type aliases for long Google schema names
    GenerateTypeAliasesForLongNames(API);

    // Execute code generation
    FCodeGen.Execute;

    // Save UUID map if specified
    if FUUIDMapFile <> '' then
    begin
      Log(etInfo, 'Saving UUID map to: %s', [FUUIDMapFile]);
      FCodeGen.UUIDMap.SaveToFile(FUUIDMapFile);
    end;

    Log(etInfo, 'Pascal code generation complete');
  finally
    API.Free;
  end;
end;

procedure TDiscovery2PasApp.GenerateCompatibilityUnit(API: TOpenAPI);
var
  APIData: TAPIData;
  CompatGen: TCompatibilityUnitGenerator;
  DtoUnit, IntfUnit, ImplUnit: string;
  ServiceName: string;
begin
  if FCompatUnitFile = '' then
    Exit;

  Log(etInfo, 'Generating compatibility unit: %s', [FCompatUnitFile]);

  // Determine unit names from output file
  DtoUnit := ExtractFileName(FOutputFile) + '.Dto';
  IntfUnit := ExtractFileName(FOutputFile) + '.Service.Intf';
  ImplUnit := ExtractFileName(FOutputFile) + '.Service.Impl';

  // Determine service name
  if FServiceName <> '' then
    ServiceName := FServiceName
  else
    ServiceName := ExtractFileName(FOutputFile);

  // Create API data for compatibility generator
  APIData := FCodeGen.CreateAPIData(API);
  try
    // Configure the API data the same way Execute does
    APIData.OnLog := FCodeGen.OnLog;
    APIData.DelphiTypes := FCodeGen.DelphiCode;
    APIData.ServiceNamePrefix := FCodeGen.ServiceNamePrefix;
    APIData.ServiceNameSuffix := FCodeGen.ServiceNameSuffix;
    APIData.ReservedTypeBehaviour := FCodeGen.ReservedTypeBehaviour;

    // Prepare the API data (generates types and services)
    APIData.CreateDefaultTypeMaps;
    APIData.CreateDefaultAPITypeMaps(False);
    if FCodeGen.ServiceMap.Count > 0 then
      APIData.RecordMethodNameMap(FCodeGen.ServiceMap);
    APIData.CreateServiceDefs;

    // Create and run the compatibility generator
    CompatGen := TCompatibilityUnitGenerator.Create(APIData, ServiceName);
    try
      CompatGen.SetNewUnits(DtoUnit, IntfUnit, ImplUnit);
      CompatGen.OnLog := @DoCodeGenLog;
      CompatGen.Generate(FCompatUnitFile);
    finally
      CompatGen.Free;
    end;

    Log(etInfo, 'Compatibility unit generated: %s', [FCompatUnitFile]);
  finally
    APIData.Free;
  end;
end;

procedure TDiscovery2PasApp.GenerateCompatibilityUnitFromJson(const AOpenAPIJson: TJSONObject);
var
  API: TOpenAPI;
  Reader: TOpenAPIReader;
  JsonStr: TJSONStringType;
begin
  if FCompatUnitFile = '' then
    Exit;

  // Convert JSON to string for reader
  JsonStr := AOpenAPIJson.FormatJSON;

  // Create and populate OpenAPI object
  API := TOpenAPI.Create;
  try
    Reader := TOpenAPIReader.Create(Self);
    try
      Reader.ReadFromString(API, JsonStr);
    finally
      Reader.Free;
    end;

    GenerateCompatibilityUnit(API);
  finally
    API.Free;
  end;
end;

procedure TDiscovery2PasApp.DoRun;
const
  ShortOpts = 'hs:V:f:p:o:kj:crdeaLQR:T:m:w:u:C:qb:';
  LongOpts: array of RTLString = (
    'help', 'service:', 'api-version:', 'file:', 'provider:',
    'output:', 'keep-openapi', 'openapi-file:',
    'client', 'server', 'delphi', 'enumerated', 'async', 'local-time', 'qualify-types',
    'reserved-types:', 'type-aliases:', 'service-map:', 'write-map:', 'uuid-map:', 'config:',
    'quiet', 'version', 'compat-unit:'
  );
var
  ErrorMsg: string;
  DiscoveryJson, OpenAPIJson: TJSONObject;
  ServiceMap: TStrings;
  OpenAPIFileName: string;
  ProviderStr: string;
begin
  Terminate;

  // Check options
  ErrorMsg := CheckOptions(ShortOpts, LongOpts);
  if ErrorMsg <> '' then
  begin
    WriteLn('Error: ', ErrorMsg);
    WriteLn('Use --help for usage information');
    ExitCode := 1;
    Exit;
  end;

  // Help
  if HasOption('h', 'help') then
  begin
    ShowHelp;
    Exit;
  end;

  // Version
  if HasOption('version') then
  begin
    ShowVersion;
    Exit;
  end;

  // Parse options
  FQuiet := HasOption('q', 'quiet');
  FServiceName := GetOptionValue('s', 'service');
  FApiVersion := GetOptionValue('V', 'api-version');
  FInputFile := GetOptionValue('f', 'file');
  FOutputFile := GetOptionValue('o', 'output');
  FKeepOpenAPI := HasOption('k', 'keep-openapi');
  FOpenAPIFile := GetOptionValue('j', 'openapi-file');
  FServiceMapFile := GetOptionValue('m', 'service-map');
  FWriteMapFile := GetOptionValue('w', 'write-map');
  FUUIDMapFile := GetOptionValue('u', 'uuid-map');
  FConfigFile := GetOptionValue('C', 'config');
  FReservedTypesFile := GetOptionValue('R', 'reserved-types');
  FTypeAliasesFile := GetOptionValue('T', 'type-aliases');
  FCompatUnitFile := GetOptionValue('b', 'compat-unit');

  // Provider
  ProviderStr := GetOptionValue('p', 'provider');
  if ProviderStr <> '' then
    FProvider := TProviderType.FromString(ProviderStr);

  // Code generator options
  if FConfigFile <> '' then
    FCodeGen.LoadConfig(FConfigFile);

  FCodeGen.GenerateClient := HasOption('c', 'client') or not HasOption('r', 'server');
  FCodeGen.GenerateServer := HasOption('r', 'server');
  FCodeGen.DelphiCode := HasOption('d', 'delphi');
  FCodeGen.UseEnums := HasOption('e', 'enumerated');
  FCodeGen.AsyncService := HasOption('a', 'async');
  if HasOption('L', 'local-time') then
    FCodeGen.ConvertUTC := True;
  if HasOption('Q', 'qualify-types') then
    FCodeGen.ReservedTypeBehaviour := rtbQualify;

  // Load reserved types from file if specified
  if FReservedTypesFile <> '' then
  begin
    if not FileExists(FReservedTypesFile) then
    begin
      WriteLn('Error: Reserved types file not found: ', FReservedTypesFile);
      ExitCode := 1;
      Exit;
    end;
    Log(etInfo, 'Loading reserved types from: %s', [FReservedTypesFile]);
    FCodeGen.ReservedTypes.LoadFromFile(FReservedTypesFile);
  end;

  // Load type aliases from file if specified
  if FTypeAliasesFile <> '' then
  begin
    if not FileExists(FTypeAliasesFile) then
    begin
      WriteLn('Error: Type aliases file not found: ', FTypeAliasesFile);
      ExitCode := 1;
      Exit;
    end;
    Log(etInfo, 'Loading type aliases from: %s', [FTypeAliasesFile]);
    FCodeGen.LoadTypeAliases(FTypeAliasesFile);
  end;

  // OpenAPI file implies keep
  if FOpenAPIFile <> '' then
    FKeepOpenAPI := True;

  // Validate required options
  if (FServiceName = '') and (FInputFile = '') then
  begin
    WriteLn('Error: Either --service or --file must be specified');
    ExitCode := 1;
    Exit;
  end;

  if (FOutputFile = '') and (FWriteMapFile = '') then
  begin
    WriteLn('Error: --output is required (unless using --write-map)');
    ExitCode := 1;
    Exit;
  end;

  // Fetch/load discovery document
  DiscoveryJson := FetchDiscoveryDocument;
  if DiscoveryJson = nil then
  begin
    ExitCode := 1;
    Exit;
  end;

  try
    // Convert to OpenAPI
    OpenAPIJson := ConvertToOpenAPI(DiscoveryJson);
    if OpenAPIJson = nil then
    begin
      ExitCode := 1;
      Exit;
    end;

    try
      // Generate or load service map
      ServiceMap := LoadOrGenerateServiceMap(OpenAPIJson);
      try
        // Write map only mode
        if FWriteMapFile <> '' then
        begin
          WriteServiceMapToFile(ServiceMap, FWriteMapFile, FServiceName);
          Log(etInfo, 'Service map written to: %s', [FWriteMapFile]);
          Exit;
        end;

        // Determine OpenAPI filename
        if FOpenAPIFile <> '' then
          OpenAPIFileName := FOpenAPIFile
        else
          OpenAPIFileName := FOutputFile + '-openapi.json';

        // Keep OpenAPI file if requested
        if FKeepOpenAPI then
          WriteOpenAPIFile(OpenAPIJson, OpenAPIFileName);

        // Generate Pascal code
        GeneratePascalCode(OpenAPIJson, ServiceMap);

        // Generate compatibility unit if requested
        GenerateCompatibilityUnitFromJson(OpenAPIJson);

        Log(etInfo, 'Done.');
      finally
        ServiceMap.Free;
      end;
    finally
      OpenAPIJson.Free;
    end;
  finally
    DiscoveryJson.Free;
  end;
end;

var
  Application: TDiscovery2PasApp;

begin
  Application := TDiscovery2PasApp.Create(nil);
  Application.Title := 'discovery2pas';
  Application.Run;
  Application.Free;
end.
