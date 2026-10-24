{
  GoogleDiscovery.Generate - Core OpenAPI generation

  Transforms parsed Google Discovery documents into OpenAPI 3.1.0 specifications
  with StackQL resource extensions.
}
unit GoogleDiscovery.Generate;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data,
  {$ELSE}
  Classes, SysUtils, fpjson,
  {$ENDIF}
  GoogleDiscovery.Types;

{ Main generation functions }
function GenerateOpenAPI(const ADoc: TDiscoveryDocument;
  const AOptions: TGenerateOptions): TJSONObject;
function GenerateOpenAPIFromJson(const ADiscoveryJson: TJSONObject;
  const AOptions: TGenerateOptions): TJSONObject;

{ OpenAPI structure generation }
function GenerateOpenAPIInfo(const ADoc: TDiscoveryDocument): TJSONObject;
function GenerateOpenAPIServers(const ADoc: TDiscoveryDocument): TJSONArray;
function GenerateOpenAPISecurity(const AAuth: TDiscoveryAuth): TJSONArray;
function GenerateOpenAPISecuritySchemes(const AAuth: TDiscoveryAuth): TJSONObject;
function GenerateOpenAPIExternalDocs(const ADoc: TDiscoveryDocument): TJSONObject;

{ Paths generation }
function GenerateOpenAPIPaths(const ADoc: TDiscoveryDocument;
  const AOptions: TGenerateOptions): TJSONObject;
function GeneratePathItem(const AMethod: TDiscoveryMethod;
  const AResource: TDiscoveryResource;
  const AOptions: TGenerateOptions): TJSONObject;
function GenerateOperation(const AMethod: TDiscoveryMethod;
  const AResource: TDiscoveryResource;
  const AOptions: TGenerateOptions): TJSONObject;

{ Parameters generation }
function GenerateOpenAPIParameters(const AParams: TDiscoveryParameterArray;
  const AParamOrder: TStringArray): TJSONArray;
function GenerateOpenAPIParameter(const AParam: TDiscoveryParameter): TJSONObject;

{ Request/Response generation }
function GenerateRequestBody(const AMethod: TDiscoveryMethod;
  const ASchemas: TDiscoverySchemaArray): TJSONObject;
function GenerateResponses(const AMethod: TDiscoveryMethod;
  const ASchemas: TDiscoverySchemaArray): TJSONObject;

{ Components/Schemas generation }
function GenerateOpenAPIComponents(const ADoc: TDiscoveryDocument): TJSONObject;
function GenerateOpenAPISchemas(const ASchemas: TDiscoverySchemaArray): TJSONObject;
function GenerateOpenAPISchema(const ASchema: TDiscoverySchema): TJSONObject;
function GenerateSchemaProperty(const AProp: TSchemaProperty): TJSONObject;

{ StackQL extension generation }
function GenerateStackQLResources(const ADoc: TDiscoveryDocument;
  const AOptions: TGenerateOptions): TJSONObject;
function GenerateStackQLResource(const AResource: TDiscoveryResource;
  const AResourcePath: string;
  const AOptions: TGenerateOptions): TJSONObject;
function GenerateStackQLMethod(const AMethod: TDiscoveryMethod;
  const AResource: TDiscoveryResource;
  const AOptions: TGenerateOptions): TJSONObject;

{ Utility functions }
function GetOpenAPIType(const ADiscoveryType, AFormat: string): TJSONObject;
function BuildOperationId(const AService, AResource, AMethod: string): string;
function GetSecurityRequirement(const AScopes: TStringArray): TJSONArray;

implementation

uses
  GoogleDiscovery.Json, GoogleDiscovery.Parser, GoogleDiscovery.Transform,
  GoogleDiscovery.Tagging, GoogleDiscovery.Logging;

const
  OPENAPI_VERSION = '3.1.0';

{ Type mapping from Discovery to OpenAPI }
function GetOpenAPIType(const ADiscoveryType, AFormat: string): TJSONObject;
begin
  Result := TJSONObject.Create;

  if ADiscoveryType = 'string' then
  begin
    Result.Add('type', 'string');
    if AFormat = 'int64' then
      Result.Add('format', 'int64')
    else if AFormat = 'uint64' then
      Result.Add('format', 'uint64')
    else if AFormat = 'date' then
      Result.Add('format', 'date')
    else if AFormat = 'date-time' then
      Result.Add('format', 'date-time')
    else if AFormat = 'byte' then
      Result.Add('format', 'byte')
    else if AFormat <> '' then
      Result.Add('format', AFormat);
  end
  else if ADiscoveryType = 'integer' then
  begin
    Result.Add('type', 'integer');
    if AFormat = 'int32' then
      Result.Add('format', 'int32')
    else if AFormat = 'uint32' then
      Result.Add('format', 'uint32')
    else if AFormat <> '' then
      Result.Add('format', AFormat);
  end
  else if ADiscoveryType = 'number' then
  begin
    Result.Add('type', 'number');
    if AFormat = 'double' then
      Result.Add('format', 'double')
    else if AFormat = 'float' then
      Result.Add('format', 'float')
    else if AFormat <> '' then
      Result.Add('format', AFormat);
  end
  else if ADiscoveryType = 'boolean' then
    Result.Add('type', 'boolean')
  else if ADiscoveryType = 'any' then
    // OpenAPI 3.1 - use empty schema for any
    Result.Add('type', 'object')
  else if ADiscoveryType = 'array' then
    Result.Add('type', 'array')
  else if ADiscoveryType = 'object' then
    Result.Add('type', 'object')
  else
    Result.Add('type', 'string');  // Default fallback
end;

{ Build operation ID from components }
function BuildOperationId(const AService, AResource, AMethod: string): string;
begin
  if AService <> '' then
    Result := AService + '.' + AResource + '.' + AMethod
  else
    Result := AResource + '.' + AMethod;
end;

{ Generate security requirement array }
function GetSecurityRequirement(const AScopes: TStringArray): TJSONArray;
var
  SecurityObj: TJSONObject;
  ScopesArr: TJSONArray;
  I: Integer;
begin
  Result := TJSONArray.Create;

  if Length(AScopes) > 0 then
  begin
    SecurityObj := TJSONObject.Create;
    ScopesArr := TJSONArray.Create;

    for I := 0 to High(AScopes) do
      ScopesArr.Add(AScopes[I]);

    SecurityObj.Add('oauth2', ScopesArr);
    Result.Add(SecurityObj);
  end;
end;

{ Generate OpenAPI info object }
function GenerateOpenAPIInfo(const ADoc: TDiscoveryDocument): TJSONObject;
var
  Contact: TJSONObject;
begin
  Result := TJSONObject.Create;

  Result.Add('title', ADoc.Title);
  Result.Add('version', ADoc.Version);

  if ADoc.Description <> '' then
    Result.Add('description', ADoc.Description);

  // Add contact info based on owner
  if (ADoc.OwnerName <> '') or (ADoc.OwnerDomain <> '') then
  begin
    Contact := TJSONObject.Create;
    if ADoc.OwnerName <> '' then
      Contact.Add('name', ADoc.OwnerName);
    if ADoc.OwnerDomain <> '' then
      Contact.Add('url', 'https://' + ADoc.OwnerDomain);
    Result.Add('contact', Contact);
  end;
end;

{ Generate servers array }
function GenerateOpenAPIServers(const ADoc: TDiscoveryDocument): TJSONArray;
var
  Server: TJSONObject;
  BaseUrl: string;
begin
  Result := TJSONArray.Create;

  // Determine base URL
  if ADoc.RootUrl <> '' then
  begin
    BaseUrl := ADoc.RootUrl;
    if (ADoc.ServicePath <> '') and (ADoc.ServicePath <> '/') then
    begin
      if not EndsWithStr(BaseUrl, '/') then
        BaseUrl := BaseUrl + '/';
      if StartsWithStr(ADoc.ServicePath, '/') then
        BaseUrl := BaseUrl + Copy(ADoc.ServicePath, 2, MaxInt)
      else
        BaseUrl := BaseUrl + ADoc.ServicePath;
    end;
  end
  else if ADoc.BaseUrl <> '' then
    BaseUrl := ADoc.BaseUrl
  else
    BaseUrl := 'https://www.googleapis.com';

  // Remove trailing slash
  if EndsWithStr(BaseUrl, '/') then
    BaseUrl := Copy(BaseUrl, 1, Length(BaseUrl) - 1);

  Server := TJSONObject.Create;
  Server.Add('url', BaseUrl);
  Result.Add(Server);
end;

{ Generate security requirements }
function GenerateOpenAPISecurity(const AAuth: TDiscoveryAuth): TJSONArray;
var
  SecurityObj: TJSONObject;
  ScopesArr: TJSONArray;
  I: Integer;
begin
  Result := TJSONArray.Create;

  if Length(AAuth.OAuth2Scopes) > 0 then
  begin
    SecurityObj := TJSONObject.Create;
    ScopesArr := TJSONArray.Create;

    for I := 0 to High(AAuth.OAuth2Scopes) do
      ScopesArr.Add(AAuth.OAuth2Scopes[I].Url);

    SecurityObj.Add('oauth2', ScopesArr);
    Result.Add(SecurityObj);
  end;
end;

{ Generate security schemes }
function GenerateOpenAPISecuritySchemes(const AAuth: TDiscoveryAuth): TJSONObject;
var
  OAuth2, Flows, AuthCodeFlow, Scopes: TJSONObject;
  I: Integer;
begin
  Result := TJSONObject.Create;

  if Length(AAuth.OAuth2Scopes) > 0 then
  begin
    OAuth2 := TJSONObject.Create;
    OAuth2.Add('type', 'oauth2');

    Flows := TJSONObject.Create;
    AuthCodeFlow := TJSONObject.Create;
    AuthCodeFlow.Add('authorizationUrl', 'https://accounts.google.com/o/oauth2/auth');
    AuthCodeFlow.Add('tokenUrl', 'https://oauth2.googleapis.com/token');

    Scopes := TJSONObject.Create;
    for I := 0 to High(AAuth.OAuth2Scopes) do
      Scopes.Add(AAuth.OAuth2Scopes[I].Url, AAuth.OAuth2Scopes[I].Description);

    AuthCodeFlow.Add('scopes', Scopes);
    Flows.Add('authorizationCode', AuthCodeFlow);
    OAuth2.Add('flows', Flows);

    Result.Add('oauth2', OAuth2);
  end;
end;

{ Generate external docs }
function GenerateOpenAPIExternalDocs(const ADoc: TDiscoveryDocument): TJSONObject;
begin
  Result := nil;

  if ADoc.DocumentationLink <> '' then
  begin
    Result := TJSONObject.Create;
    Result.Add('url', ADoc.DocumentationLink);
    Result.Add('description', 'API Documentation');
  end;
end;

{ Generate OpenAPI parameter }
function GenerateOpenAPIParameter(const AParam: TDiscoveryParameter): TJSONObject;
var
  Schema, EnumArr: TJSONObject;
  I: Integer;
  LocationStr: string;
begin
  Result := TJSONObject.Create;

  Result.Add('name', AParam.Name);

  // Map location
  case AParam.Location of
    plPath: LocationStr := 'path';
    plQuery: LocationStr := 'query';
  else
    LocationStr := 'query';
  end;
  Result.Add('in', LocationStr);

  if AParam.Description <> '' then
    Result.Add('description', AParam.Description);

  // Path parameters are always required
  if (AParam.Location = plPath) or AParam.Required then
    Result.Add('required', True);

  // Generate schema
  Schema := GetOpenAPIType(AParam.ParamType, '');

  // Add enum values if present
  if Length(AParam.EnumValues) > 0 then
  begin
    EnumArr := TJSONObject.Create;
    EnumArr := nil; // Will use array
    Schema.Add('enum', TJSONArray.Create);
    for I := 0 to High(AParam.EnumValues) do
      TJSONArray(Schema.Find('enum')).Add(AParam.EnumValues[I]);
  end;

  // Add default value with proper type
  if AParam.DefaultValue <> '' then
  begin
    if AParam.ParamType = 'boolean' then
    begin
      // Add as boolean
      if (LowerCase(AParam.DefaultValue) = 'true') then
        Schema.Add('default', True)
      else
        Schema.Add('default', False);
    end
    else if (AParam.ParamType = 'integer') then
    begin
      // Add as integer
      Schema.Add('default', StrToIntDef(AParam.DefaultValue, 0));
    end
    else
      // Add as string
      Schema.Add('default', AParam.DefaultValue);
  end;

  // Add pattern
  if AParam.Pattern <> '' then
    Schema.Add('pattern', AParam.Pattern);

  Result.Add('schema', Schema);
end;

{ Generate parameters array }
function GenerateOpenAPIParameters(const AParams: TDiscoveryParameterArray;
  const AParamOrder: TStringArray): TJSONArray;
var
  I, J: Integer;
  Added: array of Boolean;
begin
  Result := TJSONArray.Create;

  if Length(AParams) = 0 then
    Exit;

  SetLength(Added, Length(AParams));
  for I := 0 to High(Added) do
    Added[I] := False;

  // Add parameters in order if specified
  if Length(AParamOrder) > 0 then
  begin
    for I := 0 to High(AParamOrder) do
    begin
      for J := 0 to High(AParams) do
      begin
        if (not Added[J]) and (AParams[J].Name = AParamOrder[I]) then
        begin
          Result.Add(GenerateOpenAPIParameter(AParams[J]));
          Added[J] := True;
          Break;
        end;
      end;
    end;
  end;

  // Add remaining parameters
  for I := 0 to High(AParams) do
  begin
    if not Added[I] then
      Result.Add(GenerateOpenAPIParameter(AParams[I]));
  end;
end;

{ Generate request body }
function GenerateRequestBody(const AMethod: TDiscoveryMethod;
  const ASchemas: TDiscoverySchemaArray): TJSONObject;
var
  Content, MediaType, Schema: TJSONObject;
begin
  Result := nil;

  if AMethod.RequestRef = '' then
    Exit;

  Result := TJSONObject.Create;
  Content := TJSONObject.Create;
  MediaType := TJSONObject.Create;
  Schema := TJSONObject.Create;

  Schema.Add('$ref', '#/components/schemas/' + AMethod.RequestRef);
  MediaType.Add('schema', Schema);
  Content.Add('application/json', MediaType);
  Result.Add('content', Content);
end;

{ Generate responses }
function GenerateResponses(const AMethod: TDiscoveryMethod;
  const ASchemas: TDiscoverySchemaArray): TJSONObject;
var
  Response200, Content, MediaType, Schema: TJSONObject;
begin
  Result := TJSONObject.Create;
  Response200 := TJSONObject.Create;

  Response200.Add('description', 'Successful response');

  if AMethod.ResponseRef <> '' then
  begin
    Content := TJSONObject.Create;
    MediaType := TJSONObject.Create;
    Schema := TJSONObject.Create;

    Schema.Add('$ref', '#/components/schemas/' + AMethod.ResponseRef);
    MediaType.Add('schema', Schema);
    Content.Add('application/json', MediaType);
    Response200.Add('content', Content);
  end;

  Result.Add('200', Response200);
end;

{ Generate schema property }
{ Generate items chain recursively for nested arrays }
function GenerateItemsObject(const AItems: TSchemaItemDefArray; AIndex: Integer): TJSONObject;
var
  ItemType: string;
begin
  Result := TJSONObject.Create;

  if AIndex > High(AItems) then
  begin
    // No more items defined, default to 'any'
    Result.Add('type', 'any');
    Exit;
  end;

  // Check if this item references a schema
  if AItems[AIndex].ItemRef <> '' then
  begin
    Result.Add('$ref', '#/components/schemas/' + AItems[AIndex].ItemRef);
    Exit;
  end;

  ItemType := AItems[AIndex].ItemType;
  if ItemType = '' then
    ItemType := 'any';

  // Check if it's a schema reference (not a primitive type)
  if (Pos('.', ItemType) = 0) and (ItemType <> 'string') and
     (ItemType <> 'integer') and (ItemType <> 'number') and
     (ItemType <> 'boolean') and (ItemType <> 'object') and
     (ItemType <> 'array') and (ItemType <> 'any') then
  begin
    Result.Add('$ref', '#/components/schemas/' + ItemType);
    Exit;
  end;

  Result.Add('type', ItemType);

  // If this is an array, recursively add nested items
  if ItemType = 'array' then
    Result.Add('items', GenerateItemsObject(AItems, AIndex + 1));
end;

function GenerateSchemaProperty(const AProp: TSchemaProperty): TJSONObject;
var
  EnumArr: TJSONArray;
  I: Integer;
begin
  // Check if it's a reference
  if AProp.Ref <> '' then
  begin
    Result := TJSONObject.Create;
    Result.Add('$ref', '#/components/schemas/' + AProp.Ref);
    if AProp.ReadOnly then
      Result.Add('readOnly', True);
    Exit;
  end;

  Result := GetOpenAPIType(AProp.PropType, AProp.Format);

  if AProp.Description <> '' then
    Result.Add('description', AProp.Description);

  if AProp.ReadOnly then
    Result.Add('readOnly', True);

  // Handle array items
  if (AProp.PropType = 'array') and (Length(AProp.Items) > 0) then
    Result.Add('items', GenerateItemsObject(AProp.Items, 0));

  // Handle enum
  if Length(AProp.EnumValues) > 0 then
  begin
    EnumArr := TJSONArray.Create;
    for I := 0 to High(AProp.EnumValues) do
      EnumArr.Add(AProp.EnumValues[I]);
    Result.Add('enum', EnumArr);
  end;
end;

{ Generate OpenAPI schema from Discovery schema }
function GenerateOpenAPISchema(const ASchema: TDiscoverySchema): TJSONObject;
var
  Props, AddProps: TJSONObject;
  RequiredArr: TJSONArray;
  I: Integer;
begin
  Result := TJSONObject.Create;

  if ASchema.SchemaType <> '' then
    Result.Add('type', ASchema.SchemaType)
  else
    Result.Add('type', 'object');

  if ASchema.Description <> '' then
    Result.Add('description', ASchema.Description);

  // Generate properties
  if Length(ASchema.Properties) > 0 then
  begin
    Props := TJSONObject.Create;
    for I := 0 to High(ASchema.Properties) do
      Props.Add(ASchema.Properties[I].Name,
        GenerateSchemaProperty(ASchema.Properties[I]));
    Result.Add('properties', Props);
    // Properties required by at least one method
    RequiredArr := nil;
    for I := 0 to High(ASchema.Properties) do
      if ASchema.Properties[I].Required then
      begin
        if RequiredArr = nil then
          RequiredArr := TJSONArray.Create;
        RequiredArr.Add(ASchema.Properties[I].Name);
      end;
    if RequiredArr <> nil then
      Result.Add('required', RequiredArr);
  end;

  // Handle additionalProperties
  if ASchema.AdditionalPropertiesRef <> '' then
  begin
    AddProps := TJSONObject.Create;
    AddProps.Add('$ref', '#/components/schemas/' + ASchema.AdditionalPropertiesRef);
    Result.Add('additionalProperties', AddProps);
  end
  else if ASchema.AdditionalPropertiesType <> '' then
  begin
    AddProps := GetOpenAPIType(ASchema.AdditionalPropertiesType, '');
    Result.Add('additionalProperties', AddProps);
  end;
end;

{ Generate all schemas }
function GenerateOpenAPISchemas(const ASchemas: TDiscoverySchemaArray): TJSONObject;
var
  I: Integer;
begin
  Result := TJSONObject.Create;

  for I := 0 to High(ASchemas) do
    Result.Add(ASchemas[I].Id, GenerateOpenAPISchema(ASchemas[I]));
end;

{ Generate components }
function GenerateOpenAPIComponents(const ADoc: TDiscoveryDocument): TJSONObject;
var
  SecuritySchemes: TJSONObject;
begin
  Result := TJSONObject.Create;

  // Add schemas
  if Length(ADoc.Schemas) > 0 then
    Result.Add('schemas', GenerateOpenAPISchemas(ADoc.Schemas));

  // Add security schemes
  SecuritySchemes := GenerateOpenAPISecuritySchemes(ADoc.Auth);
  if SecuritySchemes.Count > 0 then
    Result.Add('securitySchemes', SecuritySchemes)
  else
    SecuritySchemes.Free;
end;

{ Generate operation object }
function GenerateOperation(const AMethod: TDiscoveryMethod;
  const AResource: TDiscoveryResource;
  const AOptions: TGenerateOptions): TJSONObject;
var
  Tags: TJSONArray;
  Params: TJSONArray;
  RequestBody, Responses: TJSONObject;
begin
  Result := TJSONObject.Create;

  // Add operation ID
  Result.Add('operationId', AMethod.Id);

  // Add description
  if AMethod.Description <> '' then
    Result.Add('description', AMethod.Description);

  // Add tags
  Tags := TJSONArray.Create;
  Tags.Add(AResource.Name);
  Result.Add('tags', Tags);

  // Add parameters
  Params := GenerateOpenAPIParameters(AMethod.Parameters, AMethod.ParameterOrder);
  if Params.Count > 0 then
    Result.Add('parameters', Params)
  else
    Params.Free;

  // Add request body
  RequestBody := GenerateRequestBody(AMethod, nil);
  if RequestBody <> nil then
    Result.Add('requestBody', RequestBody);

  // Add responses
  Responses := GenerateResponses(AMethod, nil);
  Result.Add('responses', Responses);

  // Add security if method has specific scopes
  if Length(AMethod.Scopes) > 0 then
    Result.Add('security', GetSecurityRequirement(AMethod.Scopes));
end;

{ Generate path item }
function GeneratePathItem(const AMethod: TDiscoveryMethod;
  const AResource: TDiscoveryResource;
  const AOptions: TGenerateOptions): TJSONObject;
var
  HttpMethodStr: string;
begin
  Result := TJSONObject.Create;

  // Map HTTP method to lowercase
  case AMethod.HttpMethod of
    hmGet: HttpMethodStr := 'get';
    hmPost: HttpMethodStr := 'post';
    hmPut: HttpMethodStr := 'put';
    hmPatch: HttpMethodStr := 'patch';
    hmDelete: HttpMethodStr := 'delete';
  else
    HttpMethodStr := 'get';
  end;

  Result.Add(HttpMethodStr, GenerateOperation(AMethod, AResource, AOptions));
end;

{ Recursively collect all methods from resources }
procedure CollectMethods(const AResource: TDiscoveryResource;
  const APathPrefix: string;
  var APaths: TJSONObject;
  const AOptions: TGenerateOptions);
var
  I: Integer;
  Path: string;
  PathItem, ExistingItem: TJSONObject;
  HttpMethodStr: string;
begin
  // Process methods in this resource
  for I := 0 to High(AResource.Methods) do
  begin
    // Determine path
    if AResource.Methods[I].FlatPath <> '' then
      Path := PathToOpenAPIPath(AResource.Methods[I].FlatPath)
    else
      Path := PathToOpenAPIPath(AResource.Methods[I].Path);

    // Ensure path starts with /
    if (Path <> '') and (Path[1] <> '/') then
      Path := '/' + Path;

    // Map HTTP method
    case AResource.Methods[I].HttpMethod of
      hmGet: HttpMethodStr := 'get';
      hmPost: HttpMethodStr := 'post';
      hmPut: HttpMethodStr := 'put';
      hmPatch: HttpMethodStr := 'patch';
      hmDelete: HttpMethodStr := 'delete';
    else
      HttpMethodStr := 'get';
    end;

    // Check if path already exists
    if APaths.Find(Path) <> nil then
    begin
      ExistingItem := TJSONObject(APaths.Find(Path));
      ExistingItem.Add(HttpMethodStr,
        GenerateOperation(AResource.Methods[I], AResource, AOptions));
    end
    else
    begin
      PathItem := TJSONObject.Create;
      PathItem.Add(HttpMethodStr,
        GenerateOperation(AResource.Methods[I], AResource, AOptions));
      APaths.Add(Path, PathItem);
    end;
  end;

  // Process nested resources
  for I := 0 to High(AResource.Resources) do
    CollectMethods(AResource.Resources[I], APathPrefix + '/' + AResource.Resources[I].Name,
      APaths, AOptions);
end;

{ Generate paths }
function GenerateOpenAPIPaths(const ADoc: TDiscoveryDocument;
  const AOptions: TGenerateOptions): TJSONObject;
var
  I: Integer;
begin
  Result := TJSONObject.Create;

  for I := 0 to High(ADoc.Resources) do
    CollectMethods(ADoc.Resources[I], '', Result, AOptions);
end;

{ Generate StackQL method }
function GenerateStackQLMethod(const AMethod: TDiscoveryMethod;
  const AResource: TDiscoveryResource;
  const AOptions: TGenerateOptions): TJSONObject;
var
  SQLVerb: TSQLVerb;
  Path, Action, HttpVerb: string;
begin
  Result := TJSONObject.Create;

  // Get action from operation ID
  Action := GetResourceAction(AMethod.Id);
  HttpVerb := AMethod.HttpMethod.ToString;

  // Get SQL verb
  SQLVerb := GetSQLVerb(AOptions.Provider.ToString, AResource.Name, Action,
    AMethod.Id, AMethod.Path, HttpVerb);
  Result.Add('sqlVerb', SQLVerb.ToString);

  // Add operation ref
  if AMethod.FlatPath <> '' then
    Path := PathToOpenAPIPath(AMethod.FlatPath)
  else
    Path := PathToOpenAPIPath(AMethod.Path);

  if (Path <> '') and (Path[1] <> '/') then
    Path := '/' + Path;

  Result.Add('path', Path);
  Result.Add('operationId', AMethod.Id);

  // Add response handling for list operations
  if IsListAction(AMethod.Id) then
  begin
    Result.Add('objectKey', GetObjectKey(AMethod.Id));
  end;
end;

{ Generate StackQL resource }
function GenerateStackQLResource(const AResource: TDiscoveryResource;
  const AResourcePath: string;
  const AOptions: TGenerateOptions): TJSONObject;
var
  Methods: TJSONObject;
  I: Integer;
  MethodName, ResourceName: string;
begin
  Result := TJSONObject.Create;

  Result.Add('id', AResourcePath);

  // Convert resource path to snake_case for the name
  // e.g., 'files' -> 'files', 'machineTypes' -> 'machine_types'
  ResourceName := CamelToSnake(AResource.Name);
  Result.Add('name', ResourceName);

  Methods := TJSONObject.Create;
  for I := 0 to High(AResource.Methods) do
  begin
    MethodName := GetMethodName(AOptions.Provider.ToString, AResource.Methods[I].Id);
    Methods.Add(MethodName,
      GenerateStackQLMethod(AResource.Methods[I], AResource, AOptions));
  end;

  Result.Add('methods', Methods);
end;

{ Recursively collect StackQL resources }
procedure CollectStackQLResources(const AResource: TDiscoveryResource;
  const APathPrefix: string;
  var AResources: TJSONObject;
  const AOptions: TGenerateOptions);
var
  I: Integer;
  ResourcePath, ResourceKey: string;
begin
  // Calculate resource path
  if APathPrefix <> '' then
    ResourcePath := APathPrefix + '.' + AResource.Name
  else
    ResourcePath := AResource.Name;

  // Use full path as key to avoid duplicates (e.g., folders.approvalRequests vs projects.approvalRequests)
  ResourceKey := CamelToSnake(StringReplace(ResourcePath, '.', '_', [rfReplaceAll]));

  // Add this resource if it has methods
  if Length(AResource.Methods) > 0 then
    AResources.Add(ResourceKey,
      GenerateStackQLResource(AResource, ResourcePath, AOptions));

  // Process nested resources
  for I := 0 to High(AResource.Resources) do
    CollectStackQLResources(AResource.Resources[I], ResourcePath, AResources, AOptions);
end;

{ Generate StackQL resources extension }
function GenerateStackQLResources(const ADoc: TDiscoveryDocument;
  const AOptions: TGenerateOptions): TJSONObject;
var
  I: Integer;
begin
  Result := TJSONObject.Create;

  for I := 0 to High(ADoc.Resources) do
    CollectStackQLResources(ADoc.Resources[I], '', Result, AOptions);
end;

{ Main generation function }
function GenerateOpenAPI(const ADoc: TDiscoveryDocument;
  const AOptions: TGenerateOptions): TJSONObject;
var
  ExternalDocs: TJSONObject;
begin
  Result := TJSONObject.Create;

  // OpenAPI version
  Result.Add('openapi', OPENAPI_VERSION);

  // Info
  Result.Add('info', GenerateOpenAPIInfo(ADoc));

  // Servers
  Result.Add('servers', GenerateOpenAPIServers(ADoc));

  // External docs
  ExternalDocs := GenerateOpenAPIExternalDocs(ADoc);
  if ExternalDocs <> nil then
    Result.Add('externalDocs', ExternalDocs);

  // Security
  if Length(ADoc.Auth.OAuth2Scopes) > 0 then
    Result.Add('security', GenerateOpenAPISecurity(ADoc.Auth));

  // Paths
  Result.Add('paths', GenerateOpenAPIPaths(ADoc, AOptions));

  // Components
  Result.Add('components', GenerateOpenAPIComponents(ADoc));

  // StackQL extensions
  Result.Add('x-stackQL-resources', GenerateStackQLResources(ADoc, AOptions));
end;

{ Generate from raw JSON }
function GenerateOpenAPIFromJson(const ADiscoveryJson: TJSONObject;
  const AOptions: TGenerateOptions): TJSONObject;
var
  Doc: TDiscoveryDocument;
begin
  Doc := ParseDiscoveryDocument(ADiscoveryJson);
  Result := GenerateOpenAPI(Doc, AOptions);
end;

end.
