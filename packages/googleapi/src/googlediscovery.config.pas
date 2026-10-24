{
  GoogleDiscovery.Config - Provider configuration

  Contains provider-specific configurations for Google APIs, Firebase,
  Google Workspace, and Google Admin.
}
unit GoogleDiscovery.Config;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Regexpr,
  {$ELSE}
  Classes, SysUtils, RegExpr,
  {$ENDIF}
  GoogleDiscovery.Types;

const
  { Google Discovery API URLs }
  GOOGLE_DISCOVERY_URL = 'https://discovery.googleapis.com/discovery/v1/apis';

  { OAuth2 endpoints }
  OAUTH2_AUTH_URL = 'https://accounts.google.com/o/oauth2/auth';
  OAUTH2_TOKEN_URL = 'https://accounts.google.com/o/oauth2/token';

  { StackQL contact info }
  STACKQL_CONTACT_NAME = 'StackQL Studios';
  STACKQL_CONTACT_URL = 'https://github.com/stackql/google-discovery-to-openapi';
  STACKQL_CONTACT_EMAIL = 'info@stackql.io';

{ Provider configuration functions }
function GetProviderConfig(AProvider: TProviderType): TProviderConfig;
function GetProviderName(AProvider: TProviderType): string;

{ Service filtering }
function ShouldIncludeService(const AConfig: TProviderConfig;
  const AServiceName: string; APreferred: Boolean): Boolean;
function IsServiceExcluded(const AConfig: TProviderConfig;
  const AServiceName: string): Boolean;
function IsServiceIncluded(const AConfig: TProviderConfig;
  const AServiceName: string): Boolean;
function MatchesPattern(const AValue: string; const APatterns: TStringArray): Boolean;

{ Service name mapping }
function MapServiceName(const AConfig: TProviderConfig;
  const AServiceName: string): string;

{ Fully qualified method services }
function IsFullyQualifiedMethodService(const AServiceName: string): Boolean;

{ Provider discovery URL }
function GetProviderDiscoveryUrl(AProvider: TProviderType): string;

implementation

{ Internal constants for excluded/included services }
const
  { Services excluded from googleapis.com }
  GOOGLEAPIS_EXCLUDED_SERVICES: array[0..10] of string = (
    'iam',           // Deprecated, use iamv2
    'fcm',           // Use Firebase Cloud Messaging directly
    'testing',       // Internal testing APIs
    'prod_tt_sasportal',  // Internal
    'acceleratedmobilepageurl',  // Deprecated
    'customsearch',  // Use Programmable Search Engine
    'domainsrdap',   // RDAP protocol
    'pagespeedonline',  // Use PageSpeed Insights API
    'webfonts',      // Use Google Fonts API
    'youtubeAnalytics',  // Consolidated into YouTube Data API
    'youtubereporting'  // Consolidated into YouTube Data API
  );

  { Services excluded by pattern from googleapis.com }
  GOOGLEAPIS_EXCLUDED_PATTERNS: array[0..2] of string = (
    '^.*_v\d+beta\d*$',   // Beta versions
    '^.*_v\d+alpha\d*$',  // Alpha versions
    '^discovery$'         // The discovery API itself
  );

  { Services included for Firebase }
  FIREBASE_INCLUDED_PATTERNS: array[0..0] of string = (
    '^firebase.*$'
  );

  { Services that use fully qualified method names (25 services) }
  FULLY_QUALIFIED_SERVICES: array[0..24] of string = (
    'pubsub', 'spanner', 'logging', 'monitoring', 'cloudtasks',
    'datastore', 'firestore', 'bigtable', 'cloudbuild', 'container',
    'run', 'cloudscheduler', 'secretmanager', 'dialogflow', 'translate',
    'vision', 'speech', 'language', 'videointelligence', 'automl',
    'aiplatform', 'healthcare', 'genomics', 'lifesciences', 'notebooks'
  );

{ Build provider configuration for googleapis.com }
function BuildGoogleApisConfig: TProviderConfig;
var
  I: Integer;
begin
  Result := TProviderConfig.Create;
  Result.Name := 'googleapis.com';
  Result.ProviderType := ptGoogleApis;
  Result.DiscoveryUrl := GOOGLE_DISCOVERY_URL;
  Result.RequirePreferred := True;

  // Copy excluded services
  SetLength(Result.ExcludedServices, Length(GOOGLEAPIS_EXCLUDED_SERVICES));
  for I := 0 to High(GOOGLEAPIS_EXCLUDED_SERVICES) do
    Result.ExcludedServices[I] := GOOGLEAPIS_EXCLUDED_SERVICES[I];

  // Copy excluded patterns
  SetLength(Result.ExcludedPatterns, Length(GOOGLEAPIS_EXCLUDED_PATTERNS));
  for I := 0 to High(GOOGLEAPIS_EXCLUDED_PATTERNS) do
    Result.ExcludedPatterns[I] := GOOGLEAPIS_EXCLUDED_PATTERNS[I];
end;

{ Build provider configuration for Firebase }
function BuildFirebaseConfig: TProviderConfig;
var
  I: Integer;
begin
  Result := TProviderConfig.Create;
  Result.Name := 'firebase';
  Result.ProviderType := ptFirebase;
  Result.DiscoveryUrl := GOOGLE_DISCOVERY_URL;
  Result.RequirePreferred := True;

  // Include only firebase services
  SetLength(Result.IncludedPatterns, Length(FIREBASE_INCLUDED_PATTERNS));
  for I := 0 to High(FIREBASE_INCLUDED_PATTERNS) do
    Result.IncludedPatterns[I] := FIREBASE_INCLUDED_PATTERNS[I];

  // Service name mappings (remove 'firebase' prefix)
  SetLength(Result.ServiceNameMappings, 4);
  Result.ServiceNameMappings[0].OriginalName := 'firebasedatabase';
  Result.ServiceNameMappings[0].MappedName := 'database';
  Result.ServiceNameMappings[1].OriginalName := 'firebasehosting';
  Result.ServiceNameMappings[1].MappedName := 'hosting';
  Result.ServiceNameMappings[2].OriginalName := 'firebasestorage';
  Result.ServiceNameMappings[2].MappedName := 'storage';
  Result.ServiceNameMappings[3].OriginalName := 'firebaseml';
  Result.ServiceNameMappings[3].MappedName := 'ml';
end;

{ Build provider configuration for Google Workspace }
function BuildGoogleWorkspaceConfig: TProviderConfig;
begin
  Result := TProviderConfig.Create;
  Result.Name := 'googleworkspace';
  Result.ProviderType := ptGoogleWorkspace;
  Result.DiscoveryUrl := GOOGLE_DISCOVERY_URL;
  Result.RequirePreferred := True;

  // Include workspace-related services
  SetLength(Result.IncludedServices, 8);
  Result.IncludedServices[0] := 'admin';
  Result.IncludedServices[1] := 'calendar';
  Result.IncludedServices[2] := 'docs';
  Result.IncludedServices[3] := 'drive';
  Result.IncludedServices[4] := 'gmail';
  Result.IncludedServices[5] := 'sheets';
  Result.IncludedServices[6] := 'slides';
  Result.IncludedServices[7] := 'forms';
end;

{ Build provider configuration for Google Admin }
function BuildGoogleAdminConfig: TProviderConfig;
begin
  Result := TProviderConfig.Create;
  Result.Name := 'googleadmin';
  Result.ProviderType := ptGoogleAdmin;
  Result.DiscoveryUrl := GOOGLE_DISCOVERY_URL;
  Result.RequirePreferred := True;

  // Include admin-related services
  SetLength(Result.IncludedServices, 3);
  Result.IncludedServices[0] := 'admin';
  Result.IncludedServices[1] := 'groupssettings';
  Result.IncludedServices[2] := 'licensing';
end;

{ Provider configuration functions }

function GetProviderConfig(AProvider: TProviderType): TProviderConfig;
begin
  case AProvider of
    ptGoogleApis: Result := BuildGoogleApisConfig;
    ptFirebase: Result := BuildFirebaseConfig;
    ptGoogleWorkspace: Result := BuildGoogleWorkspaceConfig;
    ptGoogleAdmin: Result := BuildGoogleAdminConfig;
  else
    Result := BuildGoogleApisConfig;
  end;
end;

function GetProviderName(AProvider: TProviderType): string;
begin
  Result := AProvider.ToString;
end;

{ Service filtering }

function MatchesPattern(const AValue: string; const APatterns: TStringArray): Boolean;
var
  I: Integer;
  Regex: TRegExpr;
begin
  Result := False;
  if Length(APatterns) = 0 then
    Exit;

  Regex := TRegExpr.Create;
  try
    for I := 0 to High(APatterns) do
    begin
      Regex.Expression := APatterns[I];
      if Regex.Exec(AValue) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    Regex.Free;
  end;
end;

function IsServiceExcluded(const AConfig: TProviderConfig;
  const AServiceName: string): Boolean;
begin
  // Check explicit exclusion list
  if AConfig.ExcludedServices.Contains(AServiceName) then
  begin
    Result := True;
    Exit;
  end;

  // Check exclusion patterns
  Result := MatchesPattern(AServiceName, AConfig.ExcludedPatterns);
end;

function IsServiceIncluded(const AConfig: TProviderConfig;
  const AServiceName: string): Boolean;
begin
  // If no inclusion rules, include everything
  if (Length(AConfig.IncludedServices) = 0) and
     (Length(AConfig.IncludedPatterns) = 0) then
  begin
    Result := True;
    Exit;
  end;

  // Check explicit inclusion list
  if AConfig.IncludedServices.Contains(AServiceName) then
  begin
    Result := True;
    Exit;
  end;

  // Check inclusion patterns
  Result := MatchesPattern(AServiceName, AConfig.IncludedPatterns);
end;

function ShouldIncludeService(const AConfig: TProviderConfig;
  const AServiceName: string; APreferred: Boolean): Boolean;
begin
  // Check preferred requirement
  if AConfig.RequirePreferred and not APreferred then
  begin
    Result := False;
    Exit;
  end;

  // Check exclusion first
  if IsServiceExcluded(AConfig, AServiceName) then
  begin
    Result := False;
    Exit;
  end;

  // Check inclusion
  Result := IsServiceIncluded(AConfig, AServiceName);
end;

{ Service name mapping }

function MapServiceName(const AConfig: TProviderConfig;
  const AServiceName: string): string;
var
  I: Integer;
begin
  Result := AServiceName;

  for I := 0 to High(AConfig.ServiceNameMappings) do
  begin
    if AConfig.ServiceNameMappings[I].OriginalName = AServiceName then
    begin
      Result := AConfig.ServiceNameMappings[I].MappedName;
      Exit;
    end;
  end;
end;

{ Fully qualified method services }

function IsFullyQualifiedMethodService(const AServiceName: string): Boolean;
var
  I: Integer;
  LowerName: string;
begin
  Result := False;
  LowerName := LowerCase(AServiceName);

  for I := 0 to High(FULLY_QUALIFIED_SERVICES) do
  begin
    if FULLY_QUALIFIED_SERVICES[I] = LowerName then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

{ Provider discovery URL }

function GetProviderDiscoveryUrl(AProvider: TProviderType): string;
begin
  // All providers use the same Google Discovery API
  Result := GOOGLE_DISCOVERY_URL;
end;

end.
