{
  GoogleDiscovery.Main - Main entry point for CLI

  Provides the main processing logic for converting Google Discovery
  documents to OpenAPI specifications.
}
unit GoogleDiscovery.Main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data,
  {$ELSE}
  Classes, SysUtils, fpjson,
  {$ENDIF}
  GoogleDiscovery.Types;

type
  { Main processor class }
  TDiscoveryProcessor = class
  private
    FOptions: TGenerateOptions;
    FProcessedCount: Integer;
    FErrorCount: Integer;
    FSkippedCount: Integer;
  public
    constructor Create(const AOptions: TGenerateOptions);

    { Process all services for the provider }
    function ProcessAllServices: Boolean;

    { Process a single service }
    function ProcessService(const AService: TServiceEntry): Boolean;

    { Process a discovery document from URL }
    function ProcessDiscoveryUrl(const AUrl: string;
      const AServiceName: string = ''): Boolean;

    { Process a discovery document from file }
    function ProcessDiscoveryFile(const AFileName: string;
      const AServiceName: string = ''): Boolean;

    { Process a discovery document from JSON }
    function ProcessDiscoveryJson(const AJson: TJSONObject;
      const AServiceName: string): Boolean;

    { Get processing statistics }
    property ProcessedCount: Integer read FProcessedCount;
    property ErrorCount: Integer read FErrorCount;
    property SkippedCount: Integer read FSkippedCount;
    property Options: TGenerateOptions read FOptions write FOptions;
  end;

{ Main functions }
function RunProcessor(const AOptions: TGenerateOptions): Integer;
function FetchAndProcessService(const AServiceName, AVersion: string;
  const AOptions: TGenerateOptions): Boolean;

{ Service listing }
function ListAvailableServices(const AProvider: TProviderType): TServiceEntryArray;
function FindService(const AServices: TServiceEntryArray;
  const AName, AVersion: string): TServiceEntry;

{ Output functions }
procedure SaveOpenAPISpec(const ASpec: TJSONObject;
  const AOutputDir, AServiceName, AVersion: string;
  AAsYaml: Boolean = True);
function GetOutputFileName(const AOutputDir, AServiceName, AVersion: string;
  AAsYaml: Boolean): string;

implementation

uses
  GoogleDiscovery.Json, GoogleDiscovery.Http, GoogleDiscovery.Config,
  GoogleDiscovery.Parser, GoogleDiscovery.Generate, GoogleDiscovery.Yaml,
  GoogleDiscovery.Logging;

{ TDiscoveryProcessor }

constructor TDiscoveryProcessor.Create(const AOptions: TGenerateOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FProcessedCount := 0;
  FErrorCount := 0;
  FSkippedCount := 0;
end;

function TDiscoveryProcessor.ProcessAllServices: Boolean;
var
  Services: TServiceEntryArray;
  I: Integer;
  Config: TProviderConfig;
begin
  Result := True;
  Config := GetProviderConfig(FOptions.Provider);

  LogInfo('Fetching service list from: ' + Config.DiscoveryUrl);

  Services := ListAvailableServices(FOptions.Provider);

  if Length(Services) = 0 then
  begin
    LogError('No services found');
    Result := False;
    Exit;
  end;

  LogInfo('Found ' + IntToStr(Length(Services)) + ' services');

  for I := 0 to High(Services) do
  begin
    // Check if we should process this service
    if not ShouldIncludeService(Config, Services[I].Name, Services[I].Preferred) then
    begin
      Inc(FSkippedCount);
      Continue;
    end;

    // Check if processing specific service
    if (FOptions.SingleService <> '') and
       (Services[I].Name <> FOptions.SingleService) then
    begin
      Inc(FSkippedCount);
      Continue;
    end;

    if not ProcessService(Services[I]) then
      Inc(FErrorCount)
    else
      Inc(FProcessedCount);
  end;

  LogInfo('Processing complete: ' + IntToStr(FProcessedCount) + ' processed, ' +
    IntToStr(FErrorCount) + ' errors, ' + IntToStr(FSkippedCount) + ' skipped');
end;

function TDiscoveryProcessor.ProcessService(const AService: TServiceEntry): Boolean;
begin
  LogInfo('Processing: ' + AService.Name + ' ' + AService.Version);

  if AService.DiscoveryRestUrl = '' then
  begin
    LogWarn('No discovery URL for ' + AService.Name);
    Result := False;
    Exit;
  end;

  Result := ProcessDiscoveryUrl(AService.DiscoveryRestUrl, AService.Name);
end;

function TDiscoveryProcessor.ProcessDiscoveryUrl(const AUrl: string;
  const AServiceName: string): Boolean;
var
  Json: TJSONObject;
begin
  Result := False;

  Json := HttpGetJsonObject(AUrl);
  if Json = nil then
  begin
    LogError('Failed to fetch: ' + AUrl);
    Exit;
  end;

  try
    Result := ProcessDiscoveryJson(Json, AServiceName);
  finally
    Json.Free;
  end;
end;

function TDiscoveryProcessor.ProcessDiscoveryFile(const AFileName: string;
  const AServiceName: string): Boolean;
var
  Json: TJSONData;
begin
  Result := False;

  try
    Json := ParseJsonFile(AFileName);
    if Json = nil then
    begin
      LogError('Failed to parse file: ' + AFileName);
      Exit;
    end;

    try
      if Json.JSONType <> jtObject then
      begin
        LogError('Invalid JSON structure in file: ' + AFileName);
        Exit;
      end;

      Result := ProcessDiscoveryJson(TJSONObject(Json), AServiceName);
    finally
      Json.Free;
    end;
  except
    on E: Exception do
    begin
      LogError('Error reading file ' + AFileName + ': ' + E.Message);
      Exit;
    end;
  end;
end;

function TDiscoveryProcessor.ProcessDiscoveryJson(const AJson: TJSONObject;
  const AServiceName: string): Boolean;
var
  Doc: TDiscoveryDocument;
  OpenAPISpec: TJSONObject;
  ServiceName, Version: string;
begin
  Result := False;

  try
    // Parse discovery document
    Doc := ParseDiscoveryDocument(AJson);

    // Get service name and version
    if AServiceName <> '' then
      ServiceName := AServiceName
    else
      ServiceName := Doc.Name;

    Version := Doc.Version;

    if ServiceName = '' then
    begin
      LogError('Could not determine service name');
      Exit;
    end;

    LogDebug('Generating OpenAPI spec for ' + ServiceName + ' ' + Version);

    // Generate OpenAPI spec
    OpenAPISpec := GenerateOpenAPI(Doc, FOptions);
    try
      // Save to file
      SaveOpenAPISpec(OpenAPISpec, FOptions.OutputDir, ServiceName, Version, True);
      Result := True;
    finally
      OpenAPISpec.Free;
    end;
  except
    on E: Exception do
    begin
      LogError('Error processing ' + AServiceName + ': ' + E.Message);
      Exit;
    end;
  end;
end;

{ Main functions }

function RunProcessor(const AOptions: TGenerateOptions): Integer;
var
  Processor: TDiscoveryProcessor;
begin
  Processor := TDiscoveryProcessor.Create(AOptions);
  try
    if Processor.ProcessAllServices then
      Result := 0
    else
      Result := 1;

    if Processor.ErrorCount > 0 then
      Result := 1;
  finally
    Processor.Free;
  end;
end;

function FetchAndProcessService(const AServiceName, AVersion: string;
  const AOptions: TGenerateOptions): Boolean;
var
  Processor: TDiscoveryProcessor;
  Services: TServiceEntryArray;
  Service: TServiceEntry;
begin
  Result := False;

  Services := ListAvailableServices(AOptions.Provider);
  Service := FindService(Services, AServiceName, AVersion);

  if Service.Name = '' then
  begin
    LogError('Service not found: ' + AServiceName + ' ' + AVersion);
    Exit;
  end;

  Processor := TDiscoveryProcessor.Create(AOptions);
  try
    Result := Processor.ProcessService(Service);
  finally
    Processor.Free;
  end;
end;

{ Service listing }

function ListAvailableServices(const AProvider: TProviderType): TServiceEntryArray;
var
  Config: TProviderConfig;
  Json: TJSONObject;
begin
  SetLength(Result, 0);
  Config := GetProviderConfig(AProvider);

  Json := HttpGetJsonObject(Config.DiscoveryUrl);
  if Json = nil then
    Exit;

  try
    Result := ParseServiceEntries(Json);
  finally
    Json.Free;
  end;
end;

function FindService(const AServices: TServiceEntryArray;
  const AName, AVersion: string): TServiceEntry;
var
  I: Integer;
  LowerName, LowerVersion: string;
begin
  Result := TServiceEntry.Create;
  LowerName := LowerCase(AName);
  LowerVersion := LowerCase(AVersion);

  for I := 0 to High(AServices) do
  begin
    if (LowerCase(AServices[I].Name) = LowerName) and
       ((AVersion = '') or (LowerCase(AServices[I].Version) = LowerVersion)) then
    begin
      // If no version specified, prefer the 'preferred' version
      if AVersion = '' then
      begin
        if AServices[I].Preferred or (Result.Name = '') then
          Result := AServices[I];
      end
      else
      begin
        Result := AServices[I];
        Exit;
      end;
    end;
  end;
end;

{ Output functions }

function GetOutputFileName(const AOutputDir, AServiceName, AVersion: string;
  AAsYaml: Boolean): string;
var
  Ext: string;
begin
  if AAsYaml then
    Ext := '.yaml'
  else
    Ext := '.json';

  if AOutputDir <> '' then
    Result := IncludeTrailingPathDelimiter(AOutputDir)
  else
    Result := '';

  Result := Result + AServiceName + '-' + AVersion + Ext;
end;

procedure SaveOpenAPISpec(const ASpec: TJSONObject;
  const AOutputDir, AServiceName, AVersion: string;
  AAsYaml: Boolean);
var
  FileName, Dir: string;
  FileStream: TFileStream;
  JsonStr: string;
begin
  FileName := GetOutputFileName(AOutputDir, AServiceName, AVersion, AAsYaml);
  Dir := ExtractFileDir(FileName);

  // Ensure directory exists
  if (Dir <> '') and not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  LogInfo('Writing: ' + FileName);

  if AAsYaml then
  begin
    SaveAsYaml(ASpec, FileName);
  end
  else
  begin
    JsonStr := ASpec.FormatJSON;
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      if Length(JsonStr) > 0 then
        FileStream.WriteBuffer(JsonStr[1], Length(JsonStr));
    finally
      FileStream.Free;
    end;
  end;
end;

end.
