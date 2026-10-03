{
  GoogleDiscovery.Transform - Core transformation functions

  Provides string transformation utilities for converting between
  different naming conventions and extracting information from paths.
}
unit GoogleDiscovery.Transform;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  GoogleDiscovery.Types;

{ Case conversion functions }
function CamelToSnake(const AName: string): string;
function SnakeToCamel(const AName: string): string;
function PascalToSnake(const AName: string): string;
function SnakeToPascal(const AName: string): string;
function ToLowerFirst(const AName: string): string;
function ToUpperFirst(const AName: string): string;

{ Path parameter extraction }
function ExtractPathParams(const APath: string): TStringArray;
function HasPathParam(const APath: string; const AParamName: string): Boolean;
function CountPathParams(const APath: string): Integer;

{ Path normalization }
function NormalizePath(const APath: string): string;
function PathToOpenAPIPath(const APath: string): string;
function EscapeOpenAPIPathRef(const APath: string): string;

{ Operation ID parsing }
function ExtractServiceFromOperationId(const AOperationId: string): string;
function ExtractResourceFromOperationId(const AOperationId: string): string;
function ExtractMethodFromOperationId(const AOperationId: string): string;
function SplitOperationId(const AOperationId: string): TStringArray;

{ Action extraction }
function ExtractActionFromMethod(const AMethodName: string): string;
function IsListAction(const AAction: string): Boolean;
function IsGetAction(const AAction: string): Boolean;
function IsCreateAction(const AAction: string): Boolean;
function IsUpdateAction(const AAction: string): Boolean;
function IsDeleteAction(const AAction: string): Boolean;

{ String utilities }
function Pluralize(const AWord: string): string;
function Singularize(const AWord: string): string;
function SplitString(const AValue: string; ADelimiter: Char): TStringArray;
function JoinStrings(const AValues: TStringArray; const ADelimiter: string): string;
function TrimPrefix(const AValue, APrefix: string): string;
function TrimSuffix(const AValue, ASuffix: string): string;
function StartsWithStr(const AValue, APrefix: string): Boolean;
function EndsWithStr(const AValue, ASuffix: string): Boolean;

implementation

{ Case conversion functions }

function CamelToSnake(const AName: string): string;
var
  I: Integer;
  C: Char;
  PrevWasUpper, PrevWasLower: Boolean;
begin
  Result := '';
  if AName = '' then
    Exit;

  PrevWasUpper := False;
  PrevWasLower := False;

  for I := 1 to Length(AName) do
  begin
    C := AName[I];

    if C in ['A'..'Z'] then
    begin
      // Add underscore before uppercase if:
      // - previous was lowercase, OR
      // - previous was uppercase but next is lowercase (for acronyms like 'XMLParser' -> 'xml_parser')
      if PrevWasLower or
         (PrevWasUpper and (I < Length(AName)) and (AName[I + 1] in ['a'..'z'])) then
        Result := Result + '_';

      Result := Result + LowerCase(C);
      PrevWasUpper := True;
      PrevWasLower := False;
    end
    else if C in ['a'..'z'] then
    begin
      Result := Result + C;
      PrevWasUpper := False;
      PrevWasLower := True;
    end
    else if C in ['0'..'9'] then
    begin
      Result := Result + C;
      PrevWasUpper := False;
      PrevWasLower := False;
    end
    else
    begin
      // Non-alphanumeric - treat as separator
      if (Length(Result) > 0) and (Result[Length(Result)] <> '_') then
        Result := Result + '_';
      PrevWasUpper := False;
      PrevWasLower := False;
    end;
  end;

  // Remove trailing underscore
  while (Length(Result) > 0) and (Result[Length(Result)] = '_') do
    Delete(Result, Length(Result), 1);

  // Remove leading underscore
  while (Length(Result) > 0) and (Result[1] = '_') do
    Delete(Result, 1, 1);
end;

function SnakeToCamel(const AName: string): string;
var
  I: Integer;
  CapNext: Boolean;
begin
  Result := '';
  if AName = '' then
    Exit;

  CapNext := False;
  for I := 1 to Length(AName) do
  begin
    if AName[I] = '_' then
      CapNext := True
    else if CapNext then
    begin
      Result := Result + UpCase(AName[I]);
      CapNext := False;
    end
    else
      Result := Result + AName[I];
  end;
end;

function PascalToSnake(const AName: string): string;
begin
  Result := CamelToSnake(AName);
end;

function SnakeToPascal(const AName: string): string;
begin
  Result := SnakeToCamel(AName);
  if Length(Result) > 0 then
    Result[1] := UpCase(Result[1]);
end;

function ToLowerFirst(const AName: string): string;
begin
  Result := AName;
  if Length(Result) > 0 then
    Result[1] := LowerCase(Result[1]);
end;

function ToUpperFirst(const AName: string): string;
begin
  Result := AName;
  if Length(Result) > 0 then
    Result[1] := UpCase(Result[1]);
end;

{ Path parameter extraction }

function ExtractPathParams(const APath: string): TStringArray;
var
  I, Start: Integer;
  InBrace: Boolean;
  ParamName: string;
begin
  SetLength(Result, 0);
  if APath = '' then
    Exit;

  InBrace := False;
  Start := 0;

  for I := 1 to Length(APath) do
  begin
    if APath[I] = '{' then
    begin
      InBrace := True;
      Start := I + 1;
    end
    else if (APath[I] = '}') and InBrace then
    begin
      ParamName := Copy(APath, Start, I - Start);
      if ParamName <> '' then
        Result.Append(ParamName);
      InBrace := False;
    end;
  end;
end;

function HasPathParam(const APath: string; const AParamName: string): Boolean;
var
  Params: TStringArray;
begin
  Params := ExtractPathParams(APath);
  Result := Params.Contains(AParamName);
end;

function CountPathParams(const APath: string): Integer;
var
  Params: TStringArray;
begin
  Params := ExtractPathParams(APath);
  Result := Length(Params);
end;

{ Path normalization }

function NormalizePath(const APath: string): string;
begin
  Result := Trim(APath);

  // Ensure path starts with /
  if (Length(Result) > 0) and (Result[1] <> '/') then
    Result := '/' + Result;

  // Remove trailing slash
  while (Length(Result) > 1) and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);

  // Replace double slashes
  while Pos('//', Result) > 0 do
    Result := StringReplace(Result, '//', '/', [rfReplaceAll]);
end;

function PathToOpenAPIPath(const APath: string): string;
begin
  // Discovery format uses {param}, which is the same as OpenAPI
  Result := NormalizePath(APath);
end;

function EscapeOpenAPIPathRef(const APath: string): string;
begin
  // For JSON pointers in $ref, / becomes ~1 and ~ becomes ~0
  Result := StringReplace(APath, '~', '~0', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '~1', [rfReplaceAll]);
end;

{ Operation ID parsing }

function SplitOperationId(const AOperationId: string): TStringArray;
begin
  Result := SplitString(AOperationId, '.');
end;

function ExtractServiceFromOperationId(const AOperationId: string): string;
var
  Parts: TStringArray;
begin
  Parts := SplitOperationId(AOperationId);
  if Length(Parts) > 0 then
    Result := Parts[0]
  else
    Result := '';
end;

function ExtractResourceFromOperationId(const AOperationId: string): string;
var
  Parts: TStringArray;
begin
  Parts := SplitOperationId(AOperationId);
  // Resource is typically the second-to-last part
  if Length(Parts) >= 2 then
    Result := Parts[Length(Parts) - 2]
  else
    Result := '';
end;

function ExtractMethodFromOperationId(const AOperationId: string): string;
var
  Parts: TStringArray;
begin
  Parts := SplitOperationId(AOperationId);
  // Method is the last part
  if Length(Parts) > 0 then
    Result := Parts[Length(Parts) - 1]
  else
    Result := '';
end;

{ Action extraction }

function ExtractActionFromMethod(const AMethodName: string): string;
var
  I: Integer;
  C: Char;
begin
  // Extract the verb from the method name
  // e.g., 'listInstances' -> 'list', 'getInstance' -> 'get'
  Result := '';
  for I := 1 to Length(AMethodName) do
  begin
    C := AMethodName[I];
    if C in ['A'..'Z'] then
      Break;
    Result := Result + C;
  end;

  if Result = '' then
    Result := LowerCase(AMethodName);
end;

function IsListAction(const AAction: string): Boolean;
var
  LowerAction: string;
begin
  LowerAction := LowerCase(AAction);
  Result := (LowerAction = 'list') or
            (LowerAction = 'search') or
            (LowerAction = 'query') or
            (LowerAction = 'fetch') or
            StartsWithStr(LowerAction, 'list') or
            StartsWithStr(LowerAction, 'search');
end;

function IsGetAction(const AAction: string): Boolean;
var
  LowerAction: string;
begin
  LowerAction := LowerCase(AAction);
  Result := (LowerAction = 'get') or
            (LowerAction = 'read') or
            (LowerAction = 'lookup') or
            StartsWithStr(LowerAction, 'get');
end;

function IsCreateAction(const AAction: string): Boolean;
var
  LowerAction: string;
begin
  LowerAction := LowerCase(AAction);
  Result := (LowerAction = 'create') or
            (LowerAction = 'insert') or
            (LowerAction = 'add') or
            StartsWithStr(LowerAction, 'create') or
            StartsWithStr(LowerAction, 'insert') or
            StartsWithStr(LowerAction, 'batch_create') or
            StartsWithStr(LowerAction, 'batchcreate');
end;

function IsUpdateAction(const AAction: string): Boolean;
var
  LowerAction: string;
begin
  LowerAction := LowerCase(AAction);
  Result := (LowerAction = 'update') or
            (LowerAction = 'patch') or
            (LowerAction = 'modify') or
            StartsWithStr(LowerAction, 'update') or
            StartsWithStr(LowerAction, 'patch') or
            StartsWithStr(LowerAction, 'batch_update') or
            StartsWithStr(LowerAction, 'batchupdate');
end;

function IsDeleteAction(const AAction: string): Boolean;
var
  LowerAction: string;
begin
  LowerAction := LowerCase(AAction);
  Result := (LowerAction = 'delete') or
            (LowerAction = 'remove') or
            StartsWithStr(LowerAction, 'delete') or
            StartsWithStr(LowerAction, 'remove') or
            StartsWithStr(LowerAction, 'batch_delete') or
            StartsWithStr(LowerAction, 'batchdelete');
end;

{ String utilities }

function Pluralize(const AWord: string): string;
var
  LowerWord: string;
begin
  if AWord = '' then
  begin
    Result := '';
    Exit;
  end;

  LowerWord := LowerCase(AWord);

  // Handle common irregular plurals
  if LowerWord = 'child' then
    Result := AWord + 'ren'
  else if LowerWord = 'person' then
    Result := 'people'
  else if LowerWord = 'index' then
    Result := AWord + 'es'
  else if LowerWord = 'status' then
    Result := AWord + 'es'
  else if LowerWord = 'analysis' then
    Result := StringReplace(AWord, 'sis', 'ses', [])
  else if EndsWithStr(LowerWord, 'y') and (Length(LowerWord) > 1) and
          not (LowerWord[Length(LowerWord) - 1] in ['a', 'e', 'i', 'o', 'u']) then
    // city -> cities, but day -> days
    Result := Copy(AWord, 1, Length(AWord) - 1) + 'ies'
  else if EndsWithStr(LowerWord, 's') or EndsWithStr(LowerWord, 'x') or
          EndsWithStr(LowerWord, 'ch') or EndsWithStr(LowerWord, 'sh') then
    Result := AWord + 'es'
  else
    Result := AWord + 's';
end;

function Singularize(const AWord: string): string;
var
  LowerWord: string;
begin
  if AWord = '' then
  begin
    Result := '';
    Exit;
  end;

  LowerWord := LowerCase(AWord);

  // Handle common irregular singulars
  if LowerWord = 'children' then
    Result := TrimSuffix(AWord, 'ren')
  else if LowerWord = 'people' then
    Result := 'person'
  else if LowerWord = 'indices' then
    Result := TrimSuffix(AWord, 'ices') + 'ex'
  else if LowerWord = 'analyses' then
    Result := TrimSuffix(AWord, 'es') + 'is'
  else if EndsWithStr(LowerWord, 'ies') and (Length(LowerWord) > 3) then
    Result := Copy(AWord, 1, Length(AWord) - 3) + 'y'
  else if EndsWithStr(LowerWord, 'ses') or EndsWithStr(LowerWord, 'xes') or
          EndsWithStr(LowerWord, 'ches') or EndsWithStr(LowerWord, 'shes') then
    Result := Copy(AWord, 1, Length(AWord) - 2)
  else if EndsWithStr(LowerWord, 's') and (Length(LowerWord) > 1) then
    Result := Copy(AWord, 1, Length(AWord) - 1)
  else
    Result := AWord;
end;

function SplitString(const AValue: string; ADelimiter: Char): TStringArray;
var
  Parts: TStringList;
  I: Integer;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := ADelimiter;
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := AValue;

    SetLength(Result, Parts.Count);
    for I := 0 to Parts.Count - 1 do
      Result[I] := Parts[I];
  finally
    Parts.Free;
  end;
end;

function JoinStrings(const AValues: TStringArray; const ADelimiter: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AValues) do
  begin
    if I > 0 then
      Result := Result + ADelimiter;
    Result := Result + AValues[I];
  end;
end;

function TrimPrefix(const AValue, APrefix: string): string;
begin
  if StartsWithStr(AValue, APrefix) then
    Result := Copy(AValue, Length(APrefix) + 1, Length(AValue) - Length(APrefix))
  else
    Result := AValue;
end;

function TrimSuffix(const AValue, ASuffix: string): string;
begin
  if EndsWithStr(AValue, ASuffix) then
    Result := Copy(AValue, 1, Length(AValue) - Length(ASuffix))
  else
    Result := AValue;
end;

function StartsWithStr(const AValue, APrefix: string): Boolean;
begin
  Result := (Length(APrefix) <= Length(AValue)) and
            (Copy(AValue, 1, Length(APrefix)) = APrefix);
end;

function EndsWithStr(const AValue, ASuffix: string): Boolean;
begin
  Result := (Length(ASuffix) <= Length(AValue)) and
            (Copy(AValue, Length(AValue) - Length(ASuffix) + 1, Length(ASuffix)) = ASuffix);
end;

end.
