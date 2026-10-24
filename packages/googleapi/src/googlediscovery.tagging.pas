{
  GoogleDiscovery.Tagging - Resource naming and SQL verb mapping

  Provides functions for deriving resource names from operation IDs
  and mapping HTTP methods to SQL verbs for StackQL.
}
unit GoogleDiscovery.Tagging;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  GoogleDiscovery.Types;

{ Resource identification }
function GetResource(const AService, AOperationId: string;
  ADebug: Boolean = False): TResourceInfo;
function GetResourceName(const AService, AOperationId: string): string;
function GetResourceAction(const AOperationId: string): string;

{ SQL verb mapping }
function GetSQLVerb(const AService, AResource, AAction, AOperationId,
  AHttpPath, AHttpVerb: string; ADebug: Boolean = False): TSQLVerb;
function GetSQLVerbFromHttpMethod(const AHttpMethod: THttpMethod;
  const AAction: string): TSQLVerb;

{ Method naming }
function GetMethodName(const AService, AOperationId: string;
  ADebug: Boolean = False): string;

{ Response key extraction for list operations }
function GetObjectKey(const AResponseSchemaId: string): string;
function IsListResponse(const ASchemaId: string): Boolean;

{ IAM handling }
function IsIamOperation(const AOperationId: string): Boolean;
function GetIamResourceName(const ABaseName: string): string;

{ Special action handling }
function NormalizeAction(const AAction: string): string;
function ExtractActionSuffix(const AMethodName, AAction: string): string;

implementation

uses
  GoogleDiscovery.Transform, GoogleDiscovery.Config;

{ Internal helper: Check if action is a standard CRUD action }
function IsStandardAction(const AAction: string): Boolean;
var
  LowerAction: string;
begin
  LowerAction := LowerCase(AAction);
  Result := (LowerAction = 'get') or
            (LowerAction = 'list') or
            (LowerAction = 'create') or
            (LowerAction = 'insert') or
            (LowerAction = 'update') or
            (LowerAction = 'patch') or
            (LowerAction = 'delete') or
            (LowerAction = 'remove');
end;

{ Resource identification }

function GetResourceAction(const AOperationId: string): string;
var
  MethodName: string;
begin
  // Extract the last part of the operation ID (the method name)
  MethodName := ExtractMethodFromOperationId(AOperationId);
  // Extract the action verb from the method name
  Result := ExtractActionFromMethod(MethodName);
end;

function GetResourceName(const AService, AOperationId: string): string;
var
  Parts: TStringArray;
  ResourcePart, Action, MethodName, Suffix: string;
  I: Integer;
begin
  Parts := SplitOperationId(AOperationId);

  if Length(Parts) < 2 then
  begin
    Result := AService;
    Exit;
  end;

  // Get the resource part (second to last in the operation ID)
  ResourcePart := Parts[Length(Parts) - 2];

  // Get the method name and action
  MethodName := Parts[Length(Parts) - 1];
  Action := ExtractActionFromMethod(MethodName);

  // Check for IAM operations
  if IsIamOperation(AOperationId) then
  begin
    Result := GetIamResourceName(CamelToSnake(ResourcePart));
    Exit;
  end;

  // Convert to snake_case
  Result := CamelToSnake(ResourcePart);

  // If the action has a suffix beyond the standard verb, append it
  if not IsStandardAction(MethodName) then
  begin
    Suffix := ExtractActionSuffix(MethodName, Action);
    if Suffix <> '' then
    begin
      Suffix := CamelToSnake(Suffix);
      // Don't duplicate if suffix is already part of resource name
      if Pos(Suffix, Result) = 0 then
        Result := Result + '_' + Suffix;
    end;
  end;

  // Handle nested resources - combine parent and child
  if Length(Parts) > 3 then
  begin
    // For deeply nested resources, we might want to include parent context
    // e.g., compute.zones.machineTypes.list -> machine_types (not zones_machine_types)
    // This matches the JS implementation behavior
  end;
end;

function GetResource(const AService, AOperationId: string;
  ADebug: Boolean): TResourceInfo;
begin
  Result.ResourceName := GetResourceName(AService, AOperationId);
  Result.Action := GetResourceAction(AOperationId);
  Result.FullPath := AOperationId;
end;

{ SQL verb mapping }

function GetSQLVerbFromHttpMethod(const AHttpMethod: THttpMethod;
  const AAction: string): TSQLVerb;
var
  LowerAction: string;
begin
  LowerAction := LowerCase(AAction);

  case AHttpMethod of
    hmGet:
      begin
        if IsListAction(LowerAction) or IsGetAction(LowerAction) then
          Result := svSelect
        else
          Result := svExec;
      end;

    hmPost:
      begin
        if IsCreateAction(LowerAction) then
          Result := svInsert
        else if IsListAction(LowerAction) or IsGetAction(LowerAction) then
          Result := svSelect  // Some list operations use POST
        else if IsUpdateAction(LowerAction) then
          Result := svUpdate
        else if IsDeleteAction(LowerAction) then
          Result := svDelete
        else
          Result := svExec;
      end;

    hmPut:
      begin
        if IsUpdateAction(LowerAction) then
          Result := svReplace
        else if IsCreateAction(LowerAction) then
          Result := svInsert
        else
          Result := svReplace;
      end;

    hmPatch:
      begin
        Result := svUpdate;
      end;

    hmDelete:
      begin
        Result := svDelete;
      end;

    else
      Result := svExec;
  end;
end;

function GetSQLVerb(const AService, AResource, AAction, AOperationId,
  AHttpPath, AHttpVerb: string; ADebug: Boolean): TSQLVerb;
var
  HttpMethod: THttpMethod;
  LowerAction: string;
begin
  HttpMethod := THttpMethod.FromString(AHttpVerb);
  LowerAction := LowerCase(AAction);

  // Handle IAM operations specially
  if IsIamOperation(AOperationId) then
  begin
    if Pos('getiam', LowerAction) > 0 then
      Result := svSelect
    else if Pos('setiam', LowerAction) > 0 then
      Result := svReplace
    else if Pos('testiam', LowerAction) > 0 then
      Result := svSelect
    else
      Result := svExec;
    Exit;
  end;

  // Use the HTTP method and action to determine SQL verb
  Result := GetSQLVerbFromHttpMethod(HttpMethod, AAction);
end;

{ Method naming }

function GetMethodName(const AService, AOperationId: string;
  ADebug: Boolean): string;
var
  Parts: TStringArray;
  I: Integer;
begin
  Parts := SplitOperationId(AOperationId);

  if Length(Parts) = 0 then
  begin
    Result := '';
    Exit;
  end;

  // Check if this service uses fully qualified method names
  if IsFullyQualifiedMethodService(AService) then
  begin
    // Use full path excluding service name
    // e.g., pubsub.projects.topics.create -> projects_topics_create
    Result := '';
    for I := 1 to High(Parts) do
    begin
      if Result <> '' then
        Result := Result + '_';
      Result := Result + CamelToSnake(Parts[I]);
    end;
  end
  else
  begin
    // Just use the last part (method name) converted to snake_case
    Result := CamelToSnake(Parts[High(Parts)]);
  end;
end;

{ Response key extraction }

function GetObjectKey(const AResponseSchemaId: string): string;
begin
  // For list responses, the items are typically in an 'items' array
  // This returns the JSONPath to extract the array from list responses
  if IsListResponse(AResponseSchemaId) then
    Result := '$.items'
  else
    Result := '';
end;

function IsListResponse(const ASchemaId: string): Boolean;
var
  LowerSchema: string;
begin
  if ASchemaId = '' then
  begin
    Result := False;
    Exit;
  end;

  LowerSchema := LowerCase(ASchemaId);

  // Common patterns for list response schemas
  Result := EndsWithStr(LowerSchema, 'listresponse') or
            EndsWithStr(LowerSchema, 'list') or
            StartsWithStr(LowerSchema, 'list') or
            (Pos('list', LowerSchema) > 0);
end;

{ IAM handling }

function IsIamOperation(const AOperationId: string): Boolean;
var
  LowerOp: string;
begin
  LowerOp := LowerCase(AOperationId);
  Result := (Pos('getiampolicy', LowerOp) > 0) or
            (Pos('setiampolicy', LowerOp) > 0) or
            (Pos('testiampermissions', LowerOp) > 0);
end;

function GetIamResourceName(const ABaseName: string): string;
begin
  // IAM operations get a special resource name suffix
  // e.g., 'buckets' -> 'buckets_iam_policies'
  Result := ABaseName + '_iam_policies';
end;

{ Special action handling }

function NormalizeAction(const AAction: string): string;
begin
  // Normalize action names to standard forms
  Result := LowerCase(AAction);

  // Map aliases to standard names
  if Result = 'insert' then
    Result := 'create'
  else if Result = 'remove' then
    Result := 'delete'
  else if Result = 'patch' then
    Result := 'update';
end;

function ExtractActionSuffix(const AMethodName, AAction: string): string;
var
  ActionLen: Integer;
begin
  // Extract the part of the method name after the action verb
  // e.g., 'listSecurityPolicies' with action 'list' -> 'SecurityPolicies'
  ActionLen := Length(AAction);

  if Length(AMethodName) > ActionLen then
    Result := Copy(AMethodName, ActionLen + 1, Length(AMethodName) - ActionLen)
  else
    Result := '';
end;

end.
