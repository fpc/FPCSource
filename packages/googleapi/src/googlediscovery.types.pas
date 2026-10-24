{
  GoogleDiscovery.Types - Type definitions for Google Discovery to OpenAPI converter

  This unit defines all record types, enumerations, and constants used
  throughout the conversion process.
}
unit GoogleDiscovery.Types;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.FGL;
  {$ELSE}
  Classes, SysUtils, fgl;
  {$ENDIF}

const
  APP_VERSION = '1.0.0';

type
  { String array type for compatibility }
  TStringArray = array of string;

  { Type helper for TStringArray }
  TStringArrayHelper = type helper for TStringArray
    function Contains(const Value: string): Boolean;
    function IndexOf(const Value: string): Integer;
    procedure Append(const Value: string);
    function Concat(const Other: TStringArray): TStringArray;
  end;

  { Provider type enumeration }
  TProviderType = (
    ptGoogleApis,
    ptFirebase,
    ptGoogleWorkspace,
    ptGoogleAdmin
  );

  { Type helper for TProviderType }
  TProviderTypeHelper = type helper for TProviderType
    function ToString: string;
    class function FromString(const AValue: string): TProviderType; static;
  end;

  { SQL verb enumeration for StackQL resource mapping }
  TSQLVerb = (
    svSelect,
    svInsert,
    svUpdate,
    svReplace,
    svDelete,
    svExec
  );

  { Type helper for TSQLVerb }
  TSQLVerbHelper = type helper for TSQLVerb
    function ToString: string;
    class function FromString(const AValue: string): TSQLVerb; static;
  end;

  { HTTP method enumeration }
  THttpMethod = (
    hmGet,
    hmPost,
    hmPut,
    hmPatch,
    hmDelete
  );

  { Type helper for THttpMethod }
  THttpMethodHelper = type helper for THttpMethod
    function ToString: string;
    class function FromString(const AValue: string): THttpMethod; static;
  end;

  { Parameter location enumeration }
  TParamLocation = (
    plPath,
    plQuery,
    plHeader
  );

  { Type helper for TParamLocation }
  TParamLocationHelper = type helper for TParamLocation
    function ToString: string;
    class function FromString(const AValue: string): TParamLocation; static;
  end;

  { Discovery parameter structure }
  TDiscoveryParameter = record
    Name: string;
    Description: string;
    ParamType: string;
    Location: TParamLocation;
    Required: Boolean;
    DefaultValue: string;
    Pattern: string;
    EnumValues: TStringArray;
    class function Create: TDiscoveryParameter; static;
  end;
  TDiscoveryParameterArray = array of TDiscoveryParameter;

  { Discovery method structure }
  TDiscoveryMethod = record
    Id: string;
    Path: string;
    FlatPath: string;
    HttpMethod: THttpMethod;
    Description: string;
    Parameters: TDiscoveryParameterArray;
    ParameterOrder: TStringArray;
    RequestRef: string;
    ResponseRef: string;
    Scopes: TStringArray;
    SupportsMediaDownload: Boolean;
    SupportsMediaUpload: Boolean;
    class function Create: TDiscoveryMethod; static;
  end;
  TDiscoveryMethodArray = array of TDiscoveryMethod;

  { Forward declaration for recursive structure }
  PDiscoveryResource = ^TDiscoveryResource;

  { Discovery resource structure (can contain nested resources) }
  TDiscoveryResource = record
    Name: string;
    Methods: TDiscoveryMethodArray;
    Resources: array of TDiscoveryResource;
    class function Create: TDiscoveryResource; static;
  end;
  TDiscoveryResourceArray = array of TDiscoveryResource;

  { OAuth2 scope structure }
  TOAuth2Scope = record
    Url: string;
    Description: string;
  end;
  TOAuth2ScopeArray = array of TOAuth2Scope;

  { Discovery authentication structure }
  TDiscoveryAuth = record
    OAuth2Scopes: TOAuth2ScopeArray;
  end;

  { Item definition for array types - supports arbitrary nesting }
  TSchemaItemDef = record
    ItemType: string;    // 'string', 'integer', 'array', etc.
    ItemRef: string;     // $ref if referencing another schema
  end;
  TSchemaItemDefArray = array of TSchemaItemDef;

  { Schema property structure }
  TSchemaProperty = record
    Name: string;
    PropType: string;
    Description: string;
    Format: string;
    Ref: string;
    Items: TSchemaItemDefArray;  // Chain of item definitions for nested arrays
    Required: Boolean;
    ReadOnly: Boolean;
    EnumValues: TStringArray;
    EnumDescriptions: TStringArray;
  end;
  TSchemaPropertyArray = array of TSchemaProperty;

  { Schema structure }
  TDiscoverySchema = record
    Id: string;
    SchemaType: string;
    Description: string;
    Properties: TSchemaPropertyArray;
    AdditionalPropertiesType: string;
    AdditionalPropertiesRef: string;
  end;
  TDiscoverySchemaArray = array of TDiscoverySchema;

  { Complete Discovery document structure }
  TDiscoveryDocument = record
    Kind: string;
    DiscoveryVersion: string;
    Id: string;
    Name: string;
    Version: string;
    Revision: string;
    Title: string;
    Description: string;
    OwnerDomain: string;
    OwnerName: string;
    RootUrl: string;
    ServicePath: string;
    BasePath: string;
    BaseUrl: string;
    BatchPath: string;
    DocumentationLink: string;
    Auth: TDiscoveryAuth;
    Schemas: TDiscoverySchemaArray;
    Parameters: TDiscoveryParameterArray;
    Resources: TDiscoveryResourceArray;
    class function Create: TDiscoveryDocument; static;
  end;

  { Service listing entry from discovery index }
  TServiceEntry = record
    Kind: string;
    Id: string;
    Name: string;
    Version: string;
    Title: string;
    Description: string;
    DiscoveryRestUrl: string;
    DocumentationLink: string;
    Preferred: Boolean;
    class function Create: TServiceEntry; static;
  end;
  TServiceEntryArray = array of TServiceEntry;

  { Provider configuration structure }
  TServiceNameMapping = record
    OriginalName: string;
    MappedName: string;
  end;
  TServiceNameMappingArray = array of TServiceNameMapping;

  TProviderConfig = record
    Name: string;
    ProviderType: TProviderType;
    DiscoveryUrl: string;
    ExcludedServices: TStringArray;
    IncludedServices: TStringArray;
    ExcludedPatterns: TStringArray;
    IncludedPatterns: TStringArray;
    ServiceNameMappings: TServiceNameMappingArray;
    RequirePreferred: Boolean;
    class function Create: TProviderConfig; static;
  end;

  { Generation options }
  TGenerateOptions = record
    Provider: TProviderType;
    OutputDir: string;
    Debug: Boolean;
    SingleService: string;
    Overwrite: Boolean;
    class function Create: TGenerateOptions; static;
  end;

  { HTTP response structure }
  THttpResponse = record
    StatusCode: Integer;
    Body: string;
    Headers: TStringArray;
    Success: Boolean;
    ErrorMessage: string;
    class function Create: THttpResponse; static;
  end;

  { Resource identification result }
  TResourceInfo = record
    ResourceName: string;
    Action: string;
    FullPath: string;
    class function Create: TResourceInfo; static;
  end;

  { StackQL method definition }
  TStackQLMethod = record
    Name: string;
    OperationRef: string;
    MediaType: string;
    OpenAPIDocKey: string;
    ObjectKey: string;
  end;
  TStackQLMethodArray = array of TStackQLMethod;

  { StackQL resource definition }
  TStackQLResource = record
    Id: string;
    Name: string;
    Title: string;
    Methods: TStackQLMethodArray;
    SelectMethods: TStringArray;
    InsertMethods: TStringArray;
    UpdateMethods: TStringArray;
    ReplaceMethods: TStringArray;
    DeleteMethods: TStringArray;
  end;
  TStackQLResourceArray = array of TStackQLResource;

  { OpenAPI info structure }
  TOpenAPIInfo = record
    Title: string;
    Description: string;
    Version: string;
    DiscoveryRevision: string;
    GeneratedDate: string;
    ContactName: string;
    ContactUrl: string;
    ContactEmail: string;
  end;

  { OpenAPI server structure }
  TOpenAPIServer = record
    Url: string;
    Description: string;
  end;
  TOpenAPIServerArray = array of TOpenAPIServer;

  { Logging level enumeration }
  TLogLevel = (
    llDebug,
    llInfo,
    llWarn,
    llError
  );

  { Type helper for TLogLevel }
  TLogLevelHelper = type helper for TLogLevel
    function ToString: string;
  end;

implementation

{ TProviderTypeHelper }

function TProviderTypeHelper.ToString: string;
begin
  case Self of
    ptGoogleApis: Result := 'googleapis.com';
    ptFirebase: Result := 'firebase';
    ptGoogleWorkspace: Result := 'googleworkspace';
    ptGoogleAdmin: Result := 'googleadmin';
  else
    Result := 'unknown';
  end;
end;

class function TProviderTypeHelper.FromString(const AValue: string): TProviderType;
var
  LowerValue: string;
begin
  LowerValue := LowerCase(AValue);
  if (LowerValue = 'googleapis.com') or (LowerValue = 'googleapis') or (LowerValue = 'google') then
    Result := ptGoogleApis
  else if LowerValue = 'firebase' then
    Result := ptFirebase
  else if (LowerValue = 'googleworkspace') or (LowerValue = 'workspace') then
    Result := ptGoogleWorkspace
  else if (LowerValue = 'googleadmin') or (LowerValue = 'admin') then
    Result := ptGoogleAdmin
  else
    Result := ptGoogleApis;  // Default
end;

{ TSQLVerbHelper }

function TSQLVerbHelper.ToString: string;
begin
  case Self of
    svSelect: Result := 'select';
    svInsert: Result := 'insert';
    svUpdate: Result := 'update';
    svReplace: Result := 'replace';
    svDelete: Result := 'delete';
    svExec: Result := 'exec';
  else
    Result := 'exec';
  end;
end;

class function TSQLVerbHelper.FromString(const AValue: string): TSQLVerb;
var
  LowerValue: string;
begin
  LowerValue := LowerCase(AValue);
  if LowerValue = 'select' then
    Result := svSelect
  else if LowerValue = 'insert' then
    Result := svInsert
  else if LowerValue = 'update' then
    Result := svUpdate
  else if LowerValue = 'replace' then
    Result := svReplace
  else if LowerValue = 'delete' then
    Result := svDelete
  else
    Result := svExec;
end;

{ THttpMethodHelper }

function THttpMethodHelper.ToString: string;
begin
  case Self of
    hmGet: Result := 'GET';
    hmPost: Result := 'POST';
    hmPut: Result := 'PUT';
    hmPatch: Result := 'PATCH';
    hmDelete: Result := 'DELETE';
  else
    Result := 'GET';
  end;
end;

class function THttpMethodHelper.FromString(const AValue: string): THttpMethod;
var
  UpperValue: string;
begin
  UpperValue := UpperCase(AValue);
  if UpperValue = 'GET' then
    Result := hmGet
  else if UpperValue = 'POST' then
    Result := hmPost
  else if UpperValue = 'PUT' then
    Result := hmPut
  else if UpperValue = 'PATCH' then
    Result := hmPatch
  else if UpperValue = 'DELETE' then
    Result := hmDelete
  else
    Result := hmGet;  // Default
end;

{ TParamLocationHelper }

function TParamLocationHelper.ToString: string;
begin
  case Self of
    plPath: Result := 'path';
    plQuery: Result := 'query';
    plHeader: Result := 'header';
  else
    Result := 'query';
  end;
end;

class function TParamLocationHelper.FromString(const AValue: string): TParamLocation;
var
  LowerValue: string;
begin
  LowerValue := LowerCase(AValue);
  if LowerValue = 'path' then
    Result := plPath
  else if LowerValue = 'query' then
    Result := plQuery
  else if LowerValue = 'header' then
    Result := plHeader
  else
    Result := plQuery;  // Default
end;

{ TLogLevelHelper }

function TLogLevelHelper.ToString: string;
begin
  case Self of
    llDebug: Result := 'DEBUG';
    llInfo: Result := 'INFO';
    llWarn: Result := 'WARN';
    llError: Result := 'ERROR';
  else
    Result := 'INFO';
  end;
end;

{ TStringArrayHelper }

function TStringArrayHelper.Contains(const Value: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(Self) do
  begin
    if Self[I] = Value then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TStringArrayHelper.IndexOf(const Value: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Self) do
  begin
    if Self[I] = Value then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TStringArrayHelper.Append(const Value: string);
var
  Len: Integer;
begin
  Len := Length(Self);
  SetLength(Self, Len + 1);
  Self[Len] := Value;
end;

function TStringArrayHelper.Concat(const Other: TStringArray): TStringArray;
var
  Len1, Len2, I: Integer;
begin
  Len1 := Length(Self);
  Len2 := Length(Other);
  SetLength(Result, Len1 + Len2);
  for I := 0 to Len1 - 1 do
    Result[I] := Self[I];
  for I := 0 to Len2 - 1 do
    Result[Len1 + I] := Other[I];
end;

{ Record class functions }

class function TDiscoveryParameter.Create: TDiscoveryParameter;
begin
  Result := Default(TDiscoveryParameter);
end;

class function TDiscoveryMethod.Create: TDiscoveryMethod;
begin
  Result := Default(TDiscoveryMethod);
end;

class function TDiscoveryResource.Create: TDiscoveryResource;
begin
  Result := Default(TDiscoveryResource);
end;

class function TDiscoveryDocument.Create: TDiscoveryDocument;
begin
  Result := Default(TDiscoveryDocument);
end;

class function TServiceEntry.Create: TServiceEntry;
begin
  Result := Default(TServiceEntry);
end;

class function TProviderConfig.Create: TProviderConfig;
begin
  Result := Default(TProviderConfig);
end;

class function TGenerateOptions.Create: TGenerateOptions;
begin
  Result := Default(TGenerateOptions);
end;

class function THttpResponse.Create: THttpResponse;
begin
  Result := Default(THttpResponse);
end;

class function TResourceInfo.Create: TResourceInfo;
begin
  Result := Default(TResourceInfo);
end;

end.
