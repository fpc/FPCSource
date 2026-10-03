{
  GoogleDiscovery.Parser - Parse Google Discovery documents

  Parses Google API Discovery JSON documents into Pascal record structures.
}
unit GoogleDiscovery.Parser;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data,
  {$ELSE}
  Classes, SysUtils, fpjson,
  {$ENDIF}
  GoogleDiscovery.Types;

{ Parse discovery document }
function ParseDiscoveryDocument(const AJson: TJSONObject): TDiscoveryDocument;
function ParseDiscoveryDocumentFromString(const AJsonString: string): TDiscoveryDocument;

{ Parse service listing }
function ParseServiceEntry(const AJson: TJSONObject): TServiceEntry;
function ParseServiceEntries(const AJson: TJSONObject): TServiceEntryArray;

{ Parse individual components }
function ParseDiscoveryAuth(const AJson: TJSONObject): TDiscoveryAuth;
function ParseDiscoveryParameter(const AJson: TJSONObject;
  const AName: string): TDiscoveryParameter;
function ParseDiscoveryParameters(const AJson: TJSONObject): TDiscoveryParameterArray;
function ParseDiscoveryMethod(const AJson: TJSONObject;
  const AId: string): TDiscoveryMethod;
function ParseDiscoveryResource(const AJson: TJSONObject;
  const AName: string): TDiscoveryResource;
function ParseDiscoveryResources(const AJson: TJSONObject): TDiscoveryResourceArray;
function ParseDiscoverySchema(const AJson: TJSONObject;
  const AId: string): TDiscoverySchema;
function ParseDiscoverySchemas(const AJson: TJSONObject): TDiscoverySchemaArray;

{ True if a property description marks it as output only (read-only) }
function IsOutputOnlyDescription(const ADescription: string): Boolean;

implementation

uses
  GoogleDiscovery.Json, GoogleDiscovery.Logging;

{ Parse OAuth2 scopes }
function ParseOAuth2Scopes(const AJson: TJSONObject): TOAuth2ScopeArray;
var
  I: Integer;
  ScopeName: string;
  ScopeObj: TJSONObject;
begin
  SetLength(Result, 0);

  if AJson = nil then
    Exit;

  SetLength(Result, AJson.Count);
  for I := 0 to AJson.Count - 1 do
  begin
    ScopeName := AJson.Names[I];
    Result[I].Url := ScopeName;

    if AJson.Items[I].JSONType = jtObject then
    begin
      ScopeObj := TJSONObject(AJson.Items[I]);
      Result[I].Description := JsonGetString(ScopeObj, 'description');
    end
    else
      Result[I].Description := '';
  end;
end;

{ Parse authentication }
function ParseDiscoveryAuth(const AJson: TJSONObject): TDiscoveryAuth;
var
  OAuth2Obj, ScopesObj: TJSONObject;
begin
  SetLength(Result.OAuth2Scopes, 0);

  if AJson = nil then
    Exit;

  OAuth2Obj := JsonGetObject(AJson, 'oauth2');
  if OAuth2Obj = nil then
    Exit;

  ScopesObj := JsonGetObject(OAuth2Obj, 'scopes');
  if ScopesObj <> nil then
    Result.OAuth2Scopes := ParseOAuth2Scopes(ScopesObj);
end;

{ Parse parameter }
function ParseDiscoveryParameter(const AJson: TJSONObject;
  const AName: string): TDiscoveryParameter;
var
  Location: string;
  EnumArr: TJSONArray;
begin
  Result := TDiscoveryParameter.Create;
  Result.Name := AName;

  if AJson = nil then
    Exit;

  Result.Description := JsonGetString(AJson, 'description');
  Result.ParamType := JsonGetString(AJson, 'type');
  Result.Required := JsonGetBoolean(AJson, 'required', False);
  Result.DefaultValue := JsonGetString(AJson, 'default');
  Result.Pattern := JsonGetString(AJson, 'pattern');

  Location := JsonGetString(AJson, 'location', 'query');
  Result.Location := TParamLocation.FromString(Location);

  EnumArr := JsonGetArray(AJson, 'enum');
  if EnumArr <> nil then
    Result.EnumValues := JsonArrayToStringArray(EnumArr);
end;

{ Parse parameters collection }
function ParseDiscoveryParameters(const AJson: TJSONObject): TDiscoveryParameterArray;
var
  I: Integer;
begin
  SetLength(Result, 0);

  if AJson = nil then
    Exit;

  SetLength(Result, AJson.Count);
  for I := 0 to AJson.Count - 1 do
  begin
    if AJson.Items[I].JSONType = jtObject then
      Result[I] := ParseDiscoveryParameter(TJSONObject(AJson.Items[I]), AJson.Names[I])
    else
      Result[I] := TDiscoveryParameter.Create;
  end;
end;

{ Parse method }
function ParseDiscoveryMethod(const AJson: TJSONObject;
  const AId: string): TDiscoveryMethod;
var
  HttpMethod: string;
  ParamsObj: TJSONObject;
  ParamOrderArr, ScopesArr: TJSONArray;
begin
  Result := TDiscoveryMethod.Create;
  Result.Id := AId;

  if AJson = nil then
    Exit;

  Result.Path := JsonGetString(AJson, 'path');
  Result.FlatPath := JsonGetString(AJson, 'flatPath', Result.Path);
  Result.Description := JsonGetString(AJson, 'description');

  HttpMethod := JsonGetString(AJson, 'httpMethod', 'GET');
  Result.HttpMethod := THttpMethod.FromString(HttpMethod);

  // Parse parameters
  ParamsObj := JsonGetObject(AJson, 'parameters');
  if ParamsObj <> nil then
    Result.Parameters := ParseDiscoveryParameters(ParamsObj);

  // Parse parameter order
  ParamOrderArr := JsonGetArray(AJson, 'parameterOrder');
  if ParamOrderArr <> nil then
    Result.ParameterOrder := JsonArrayToStringArray(ParamOrderArr);

  // Parse request/response refs
  if JsonHasKey(AJson, 'request') then
    Result.RequestRef := JsonPathGetString(AJson, 'request.$ref');

  if JsonHasKey(AJson, 'response') then
    Result.ResponseRef := JsonPathGetString(AJson, 'response.$ref');

  // Parse scopes
  ScopesArr := JsonGetArray(AJson, 'scopes');
  if ScopesArr <> nil then
    Result.Scopes := JsonArrayToStringArray(ScopesArr);

  // Parse media support
  Result.SupportsMediaDownload := JsonGetBoolean(AJson, 'supportsMediaDownload', False);
  Result.SupportsMediaUpload := JsonGetBoolean(AJson, 'supportsMediaUpload', False);
end;

{ Parse methods collection }
function ParseDiscoveryMethods(const AJson: TJSONObject): TDiscoveryMethodArray;
var
  I: Integer;
  MethodObj: TJSONObject;
  MethodId: string;
begin
  SetLength(Result, 0);

  if AJson = nil then
    Exit;

  SetLength(Result, AJson.Count);
  for I := 0 to AJson.Count - 1 do
  begin
    if AJson.Items[I].JSONType = jtObject then
    begin
      MethodObj := TJSONObject(AJson.Items[I]);
      MethodId := JsonGetString(MethodObj, 'id', AJson.Names[I]);
      Result[I] := ParseDiscoveryMethod(MethodObj, MethodId);
    end
    else
      Result[I] := TDiscoveryMethod.Create;
  end;
end;

{ Parse resource (recursive) }
function ParseDiscoveryResource(const AJson: TJSONObject;
  const AName: string): TDiscoveryResource;
var
  MethodsObj, ResourcesObj: TJSONObject;
begin
  Result := TDiscoveryResource.Create;
  Result.Name := AName;

  if AJson = nil then
    Exit;

  // Parse methods
  MethodsObj := JsonGetObject(AJson, 'methods');
  if MethodsObj <> nil then
    Result.Methods := ParseDiscoveryMethods(MethodsObj);

  // Parse nested resources (recursive)
  ResourcesObj := JsonGetObject(AJson, 'resources');
  if ResourcesObj <> nil then
    Result.Resources := ParseDiscoveryResources(ResourcesObj);
end;

{ Parse resources collection }
function ParseDiscoveryResources(const AJson: TJSONObject): TDiscoveryResourceArray;
var
  I: Integer;
begin
  SetLength(Result, 0);

  if AJson = nil then
    Exit;

  SetLength(Result, AJson.Count);
  for I := 0 to AJson.Count - 1 do
  begin
    if AJson.Items[I].JSONType = jtObject then
      Result[I] := ParseDiscoveryResource(TJSONObject(AJson.Items[I]), AJson.Names[I])
    else
      Result[I] := TDiscoveryResource.Create;
  end;
end;

{ Recognizes "Output only...", "[Output Only]..." and "Deprecated: Output only..." }
function IsOutputOnlyDescription(const ADescription: string): Boolean;
const
  DeprecatedPrefix = 'deprecated:';
var
  S: string;
begin
  S := LowerCase(TrimLeft(ADescription));
  if Copy(S, 1, Length(DeprecatedPrefix)) = DeprecatedPrefix then
    S := TrimLeft(Copy(S, Length(DeprecatedPrefix) + 1, Length(S)));
  Result := (Copy(S, 1, 11) = 'output only') or (Copy(S, 1, 13) = '[output only]');
end;

{ Parse schema property }
function ParseSchemaProperty(const AJson: TJSONObject;
  const AName: string): TSchemaProperty;
var
  EnumArr, EnumDescArr, RequiredArr: TJSONArray;
  ItemsObj, AnnotationsObj: TJSONObject;
  ItemDef: TSchemaItemDef;
begin
  Result.Name := AName;
  Result.PropType := '';
  Result.Description := '';
  Result.Format := '';
  Result.Ref := '';
  SetLength(Result.Items, 0);
  Result.Required := False;
  Result.ReadOnly := False;
  SetLength(Result.EnumValues, 0);
  SetLength(Result.EnumDescriptions, 0);

  if AJson = nil then
    Exit;

  Result.PropType := JsonGetString(AJson, 'type');
  Result.Description := JsonGetString(AJson, 'description');
  Result.Format := JsonGetString(AJson, 'format');
  Result.Ref := JsonGetString(AJson, '$ref');
  Result.ReadOnly := JsonGetBoolean(AJson, 'readOnly', False)
                     or IsOutputOnlyDescription(Result.Description);
  // annotations.required lists the methods that require the property
  AnnotationsObj := JsonGetObject(AJson, 'annotations');
  if AnnotationsObj <> nil then
  begin
    RequiredArr := JsonGetArray(AnnotationsObj, 'required');
    Result.Required := (RequiredArr <> nil) and (RequiredArr.Count > 0);
  end;

  // Handle array items - recursively parse nested arrays
  ItemsObj := JsonGetObject(AJson, 'items');
  while ItemsObj <> nil do
  begin
    ItemDef.ItemRef := JsonGetString(ItemsObj, '$ref');
    ItemDef.ItemType := JsonGetString(ItemsObj, 'type');
    SetLength(Result.Items, Length(Result.Items) + 1);
    Result.Items[High(Result.Items)] := ItemDef;
    // Continue to nested items if this is an array
    if ItemDef.ItemType = 'array' then
      ItemsObj := JsonGetObject(ItemsObj, 'items')
    else
      ItemsObj := nil;
  end;

  // Handle enums
  EnumArr := JsonGetArray(AJson, 'enum');
  if EnumArr <> nil then
    Result.EnumValues := JsonArrayToStringArray(EnumArr);

  EnumDescArr := JsonGetArray(AJson, 'enumDescriptions');
  if EnumDescArr <> nil then
    Result.EnumDescriptions := JsonArrayToStringArray(EnumDescArr);
end;

{ Parse schema }
function ParseDiscoverySchema(const AJson: TJSONObject;
  const AId: string): TDiscoverySchema;
var
  PropsObj, AddPropsObj: TJSONObject;
  I: Integer;
begin
  Result.Id := AId;
  Result.SchemaType := '';
  Result.Description := '';
  SetLength(Result.Properties, 0);
  Result.AdditionalPropertiesType := '';
  Result.AdditionalPropertiesRef := '';

  if AJson = nil then
    Exit;

  Result.SchemaType := JsonGetString(AJson, 'type');
  Result.Description := JsonGetString(AJson, 'description');

  // Parse properties
  PropsObj := JsonGetObject(AJson, 'properties');
  if PropsObj <> nil then
  begin
    SetLength(Result.Properties, PropsObj.Count);
    for I := 0 to PropsObj.Count - 1 do
    begin
      if PropsObj.Items[I].JSONType = jtObject then
        Result.Properties[I] := ParseSchemaProperty(TJSONObject(PropsObj.Items[I]), PropsObj.Names[I]);
    end;
  end;

  // Parse additional properties
  AddPropsObj := JsonGetObject(AJson, 'additionalProperties');
  if AddPropsObj <> nil then
  begin
    Result.AdditionalPropertiesType := JsonGetString(AddPropsObj, 'type');
    Result.AdditionalPropertiesRef := JsonGetString(AddPropsObj, '$ref');
  end;
end;

{ Parse schemas collection }
function ParseDiscoverySchemas(const AJson: TJSONObject): TDiscoverySchemaArray;
var
  I: Integer;
begin
  SetLength(Result, 0);

  if AJson = nil then
    Exit;

  SetLength(Result, AJson.Count);
  for I := 0 to AJson.Count - 1 do
  begin
    if AJson.Items[I].JSONType = jtObject then
      Result[I] := ParseDiscoverySchema(TJSONObject(AJson.Items[I]), AJson.Names[I]);
  end;
end;

{ Parse complete discovery document }
function ParseDiscoveryDocument(const AJson: TJSONObject): TDiscoveryDocument;
var
  AuthObj, ParamsObj, ResourcesObj, SchemasObj: TJSONObject;
begin
  Result := TDiscoveryDocument.Create;

  if AJson = nil then
    Exit;

  // Parse basic fields
  Result.Kind := JsonGetString(AJson, 'kind');
  Result.DiscoveryVersion := JsonGetString(AJson, 'discoveryVersion');
  Result.Id := JsonGetString(AJson, 'id');
  Result.Name := JsonGetString(AJson, 'name');
  Result.Version := JsonGetString(AJson, 'version');
  Result.Revision := JsonGetString(AJson, 'revision');
  Result.Title := JsonGetString(AJson, 'title');
  Result.Description := JsonGetString(AJson, 'description');
  Result.OwnerDomain := JsonGetString(AJson, 'ownerDomain');
  Result.OwnerName := JsonGetString(AJson, 'ownerName');
  Result.RootUrl := JsonGetString(AJson, 'rootUrl');
  Result.ServicePath := JsonGetString(AJson, 'servicePath');
  Result.BasePath := JsonGetString(AJson, 'basePath');
  Result.BaseUrl := JsonGetString(AJson, 'baseUrl');
  Result.BatchPath := JsonGetString(AJson, 'batchPath');
  Result.DocumentationLink := JsonGetString(AJson, 'documentationLink');

  // Parse auth
  AuthObj := JsonGetObject(AJson, 'auth');
  if AuthObj <> nil then
    Result.Auth := ParseDiscoveryAuth(AuthObj);

  // Parse global parameters
  ParamsObj := JsonGetObject(AJson, 'parameters');
  if ParamsObj <> nil then
    Result.Parameters := ParseDiscoveryParameters(ParamsObj);

  // Parse resources
  ResourcesObj := JsonGetObject(AJson, 'resources');
  if ResourcesObj <> nil then
    Result.Resources := ParseDiscoveryResources(ResourcesObj);

  // Parse schemas
  SchemasObj := JsonGetObject(AJson, 'schemas');
  if SchemasObj <> nil then
    Result.Schemas := ParseDiscoverySchemas(SchemasObj);
end;

function ParseDiscoveryDocumentFromString(const AJsonString: string): TDiscoveryDocument;
var
  Json: TJSONData;
begin
  Result := TDiscoveryDocument.Create;

  Json := ParseJson(AJsonString);
  try
    if Json.JSONType = jtObject then
      Result := ParseDiscoveryDocument(TJSONObject(Json));
  finally
    Json.Free;
  end;
end;

{ Parse service entry }
function ParseServiceEntry(const AJson: TJSONObject): TServiceEntry;
begin
  Result := TServiceEntry.Create;

  if AJson = nil then
    Exit;

  Result.Kind := JsonGetString(AJson, 'kind');
  Result.Id := JsonGetString(AJson, 'id');
  Result.Name := JsonGetString(AJson, 'name');
  Result.Version := JsonGetString(AJson, 'version');
  Result.Title := JsonGetString(AJson, 'title');
  Result.Description := JsonGetString(AJson, 'description');
  Result.DiscoveryRestUrl := JsonGetString(AJson, 'discoveryRestUrl');
  Result.DocumentationLink := JsonGetString(AJson, 'documentationLink');
  Result.Preferred := JsonGetBoolean(AJson, 'preferred', False);
end;

{ Parse service entries from discovery index }
function ParseServiceEntries(const AJson: TJSONObject): TServiceEntryArray;
var
  ItemsArr: TJSONArray;
  I: Integer;
begin
  SetLength(Result, 0);

  if AJson = nil then
    Exit;

  ItemsArr := JsonGetArray(AJson, 'items');
  if ItemsArr = nil then
    Exit;

  SetLength(Result, ItemsArr.Count);
  for I := 0 to ItemsArr.Count - 1 do
  begin
    if ItemsArr.Items[I].JSONType = jtObject then
      Result[I] := ParseServiceEntry(TJSONObject(ItemsArr.Items[I]))
    else
      Result[I] := TServiceEntry.Create;
  end;
end;

end.
