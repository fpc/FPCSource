{
  GoogleDiscovery.Json - JSON utilities and path navigation

  Provides utilities for working with JSON data, including path-based
  access and manipulation similar to jsonpath/jsonpointer.
}
unit GoogleDiscovery.Json;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data, FpJson.Parser,
  {$ELSE}
  Classes, SysUtils, fpjson, jsonparser,
  {$ENDIF}
  GoogleDiscovery.Types;

type
  { Exception for JSON operations }
  EJsonException = class(Exception);
  EJsonPathException = class(EJsonException);
  EJsonParseException = class(EJsonException);

{ JSON parsing functions }
function ParseJson(const AJsonString: string): TJSONData;
function ParseJsonFile(const AFileName: string): TJSONData;
function TryParseJson(const AJsonString: string; out AData: TJSONData): Boolean;

{ JSON path access - supports simple dot notation and array indices }
{ Examples: 'name', 'user.name', 'items[0]', 'users[0].name' }
function JsonPathGet(ARoot: TJSONData; const APath: string): TJSONData;
function JsonPathGetString(ARoot: TJSONData; const APath: string;
  const ADefault: string = ''): string;
function JsonPathGetInteger(ARoot: TJSONData; const APath: string;
  const ADefault: Integer = 0): Integer;
function JsonPathGetBoolean(ARoot: TJSONData; const APath: string;
  const ADefault: Boolean = False): Boolean;
function JsonPathExists(ARoot: TJSONData; const APath: string): Boolean;

{ JSON pointer access - RFC 6901 format }
{ Examples: '/name', '/user/name', '/items/0' }
function JsonPointerGet(ARoot: TJSONData; const APointer: string): TJSONData;
function JsonPointerExists(ARoot: TJSONData; const APointer: string): Boolean;

{ JSON object helpers }
function JsonGetString(AObj: TJSONObject; const AKey: string;
  const ADefault: string = ''): string;
function JsonGetInteger(AObj: TJSONObject; const AKey: string;
  const ADefault: Integer = 0): Integer;
function JsonGetBoolean(AObj: TJSONObject; const AKey: string;
  const ADefault: Boolean = False): Boolean;
function JsonGetFloat(AObj: TJSONObject; const AKey: string;
  const ADefault: Double = 0.0): Double;
function JsonGetObject(AObj: TJSONObject; const AKey: string): TJSONObject;
function JsonGetArray(AObj: TJSONObject; const AKey: string): TJSONArray;
function JsonHasKey(AObj: TJSONObject; const AKey: string): Boolean;

{ JSON array helpers }
function JsonArrayToStringArray(AArr: TJSONArray): GoogleDiscovery.Types.TStringArray;

{ JSON creation helpers }
function CreateJsonObject: TJSONObject;
function CreateJsonArray: TJSONArray;
function CloneJson(AData: TJSONData): TJSONData;

{ JSON output }
function JsonToString(AData: TJSONData; APretty: Boolean = False): string;
procedure JsonToFile(AData: TJSONData; const AFileName: string;
  APretty: Boolean = True);

{ JSON reference handling for OpenAPI }
function IsJsonRef(AObj: TJSONObject): Boolean;
function GetJsonRef(AObj: TJSONObject): string;
function CreateJsonRef(const ARef: string): TJSONObject;

{ Path parsing utilities }
function SplitJsonPath(const APath: string): GoogleDiscovery.Types.TStringArray;
function JsonPointerToPath(const APointer: string): string;
function PathToJsonPointer(const APath: string): string;
function EscapeJsonPointer(const AToken: string): string;
function UnescapeJsonPointer(const AToken: string): string;

implementation

{ JSON parsing }

function ParseJson(const AJsonString: string): TJSONData;
begin
  try
    Result := GetJSON(AJsonString);
  except
    on E: Exception do
      raise EJsonParseException.Create('Failed to parse JSON: ' + E.Message);
  end;
end;

function ParseJsonFile(const AFileName: string): TJSONData;
var
  FileStream: TFileStream;
  Parser: TJSONParser;
begin
  if not FileExists(AFileName) then
    raise EJsonException.CreateFmt('File not found: %s', [AFileName]);

  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Parser := TJSONParser.Create(FileStream, []);
    try
      Result := Parser.Parse;
    finally
      Parser.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

function TryParseJson(const AJsonString: string; out AData: TJSONData): Boolean;
begin
  Result := False;
  AData := nil;
  try
    AData := GetJSON(AJsonString);
    Result := True;
  except
    // Ignore parse errors
  end;
end;

{ JSON path access }

function SplitJsonPath(const APath: string): GoogleDiscovery.Types.TStringArray;
var
  Parts: TStringList;
  I, Start, Len: Integer;
  InBracket: Boolean;
  C: Char;
  Token: string;
begin
  if APath = '' then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  Parts := TStringList.Create;
  try
    Start := 1;
    Len := Length(APath);
    InBracket := False;
    I := 1;

    while I <= Len do
    begin
      C := APath[I];

      if C = '[' then
      begin
        // Save current token if any
        if I > Start then
        begin
          Token := Copy(APath, Start, I - Start);
          if Token <> '' then
            Parts.Add(Token);
        end;
        InBracket := True;
        Start := I + 1;
      end
      else if (C = ']') and InBracket then
      begin
        Token := Copy(APath, Start, I - Start);
        Parts.Add('[' + Token + ']');
        InBracket := False;
        Start := I + 1;
        // Skip dot after bracket if present
        if (Start <= Len) and (APath[Start] = '.') then
          Inc(Start);
      end
      else if (C = '.') and not InBracket then
      begin
        Token := Copy(APath, Start, I - Start);
        if Token <> '' then
          Parts.Add(Token);
        Start := I + 1;
      end;

      Inc(I);
    end;

    // Add remaining token
    if Start <= Len then
    begin
      Token := Copy(APath, Start, Len - Start + 1);
      if Token <> '' then
        Parts.Add(Token);
    end;

    SetLength(Result, Parts.Count);
    for I := 0 to Parts.Count - 1 do
      Result[I] := Parts[I];
  finally
    Parts.Free;
  end;
end;

function JsonPathGet(ARoot: TJSONData; const APath: string): TJSONData;
var
  Parts: GoogleDiscovery.Types.TStringArray;
  Current: TJSONData;
  I, Index: Integer;
  Part: string;
begin
  Result := nil;

  if ARoot = nil then
    Exit;

  if APath = '' then
  begin
    Result := ARoot;
    Exit;
  end;

  Parts := SplitJsonPath(APath);
  Current := ARoot;

  for I := 0 to High(Parts) do
  begin
    Part := Parts[I];

    if Current = nil then
      Exit;

    // Check for array index: [N]
    if (Length(Part) >= 3) and (Part[1] = '[') and (Part[Length(Part)] = ']') then
    begin
      if Current.JSONType <> jtArray then
        Exit;

      Index := StrToIntDef(Copy(Part, 2, Length(Part) - 2), -1);
      if (Index < 0) or (Index >= TJSONArray(Current).Count) then
        Exit;

      Current := TJSONArray(Current).Items[Index];
    end
    else
    begin
      // Object property access
      if Current.JSONType <> jtObject then
        Exit;

      Current := TJSONObject(Current).Find(Part);
    end;
  end;

  Result := Current;
end;

function JsonPathGetString(ARoot: TJSONData; const APath: string;
  const ADefault: string): string;
var
  Data: TJSONData;
begin
  Data := JsonPathGet(ARoot, APath);
  if (Data <> nil) and (Data.JSONType = jtString) then
    Result := Data.AsString
  else
    Result := ADefault;
end;

function JsonPathGetInteger(ARoot: TJSONData; const APath: string;
  const ADefault: Integer): Integer;
var
  Data: TJSONData;
begin
  Data := JsonPathGet(ARoot, APath);
  if (Data <> nil) and (Data.JSONType in [jtNumber]) then
    Result := Data.AsInteger
  else
    Result := ADefault;
end;

function JsonPathGetBoolean(ARoot: TJSONData; const APath: string;
  const ADefault: Boolean): Boolean;
var
  Data: TJSONData;
begin
  Data := JsonPathGet(ARoot, APath);
  if (Data <> nil) and (Data.JSONType = jtBoolean) then
    Result := Data.AsBoolean
  else
    Result := ADefault;
end;

function JsonPathExists(ARoot: TJSONData; const APath: string): Boolean;
begin
  Result := JsonPathGet(ARoot, APath) <> nil;
end;

{ JSON pointer (RFC 6901) }

function EscapeJsonPointer(const AToken: string): string;
begin
  Result := StringReplace(AToken, '~', '~0', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '~1', [rfReplaceAll]);
end;

function UnescapeJsonPointer(const AToken: string): string;
begin
  Result := StringReplace(AToken, '~1', '/', [rfReplaceAll]);
  Result := StringReplace(Result, '~0', '~', [rfReplaceAll]);
end;

function JsonPointerGet(ARoot: TJSONData; const APointer: string): TJSONData;
var
  Current: TJSONData;
  Tokens: TStringList;
  I, Index: Integer;
  Token: string;
begin
  Result := nil;

  if ARoot = nil then
    Exit;

  // Empty pointer references the root
  if (APointer = '') then
  begin
    Result := ARoot;
    Exit;
  end;

  // Must start with /
  if APointer[1] <> '/' then
    Exit;

  Current := ARoot;
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := '/';
    Tokens.StrictDelimiter := True;
    Tokens.DelimitedText := Copy(APointer, 2, Length(APointer) - 1);

    for I := 0 to Tokens.Count - 1 do
    begin
      if Current = nil then
        Exit;

      Token := UnescapeJsonPointer(Tokens[I]);

      case Current.JSONType of
        jtObject:
          Current := TJSONObject(Current).Find(Token);
        jtArray:
          begin
            Index := StrToIntDef(Token, -1);
            if (Index >= 0) and (Index < TJSONArray(Current).Count) then
              Current := TJSONArray(Current).Items[Index]
            else
              Current := nil;
          end;
        else
          Current := nil;
      end;
    end;

    Result := Current;
  finally
    Tokens.Free;
  end;
end;

function JsonPointerExists(ARoot: TJSONData; const APointer: string): Boolean;
begin
  Result := JsonPointerGet(ARoot, APointer) <> nil;
end;

function JsonPointerToPath(const APointer: string): string;
var
  I: Integer;
  Parts: TStringList;
begin
  if (APointer = '') or (APointer[1] <> '/') then
  begin
    Result := APointer;
    Exit;
  end;

  Parts := TStringList.Create;
  try
    Parts.Delimiter := '/';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Copy(APointer, 2, Length(APointer) - 1);

    Result := '';
    for I := 0 to Parts.Count - 1 do
    begin
      if I > 0 then
        Result := Result + '.';
      Result := Result + UnescapeJsonPointer(Parts[I]);
    end;
  finally
    Parts.Free;
  end;
end;

function PathToJsonPointer(const APath: string): string;
var
  Parts: GoogleDiscovery.Types.TStringArray;
  I: Integer;
begin
  if APath = '' then
  begin
    Result := '';
    Exit;
  end;

  Parts := SplitJsonPath(APath);
  Result := '';

  for I := 0 to High(Parts) do
  begin
    if (Length(Parts[I]) >= 3) and (Parts[I][1] = '[') then
      // Array index - remove brackets
      Result := Result + '/' + Copy(Parts[I], 2, Length(Parts[I]) - 2)
    else
      Result := Result + '/' + EscapeJsonPointer(Parts[I]);
  end;
end;

{ JSON object helpers }

function JsonGetString(AObj: TJSONObject; const AKey: string;
  const ADefault: string): string;
var
  Data: TJSONData;
begin
  if AObj = nil then
  begin
    Result := ADefault;
    Exit;
  end;

  Data := AObj.Find(AKey);
  if (Data <> nil) and (Data.JSONType = jtString) then
    Result := Data.AsString
  else
    Result := ADefault;
end;

function JsonGetInteger(AObj: TJSONObject; const AKey: string;
  const ADefault: Integer): Integer;
var
  Data: TJSONData;
begin
  if AObj = nil then
  begin
    Result := ADefault;
    Exit;
  end;

  Data := AObj.Find(AKey);
  if (Data <> nil) and (Data.JSONType in [jtNumber]) then
    Result := Data.AsInteger
  else
    Result := ADefault;
end;

function JsonGetBoolean(AObj: TJSONObject; const AKey: string;
  const ADefault: Boolean): Boolean;
var
  Data: TJSONData;
begin
  if AObj = nil then
  begin
    Result := ADefault;
    Exit;
  end;

  Data := AObj.Find(AKey);
  if (Data <> nil) and (Data.JSONType = jtBoolean) then
    Result := Data.AsBoolean
  else
    Result := ADefault;
end;

function JsonGetFloat(AObj: TJSONObject; const AKey: string;
  const ADefault: Double): Double;
var
  Data: TJSONData;
begin
  if AObj = nil then
  begin
    Result := ADefault;
    Exit;
  end;

  Data := AObj.Find(AKey);
  if (Data <> nil) and (Data.JSONType in [jtNumber]) then
    Result := Data.AsFloat
  else
    Result := ADefault;
end;

function JsonGetObject(AObj: TJSONObject; const AKey: string): TJSONObject;
var
  Data: TJSONData;
begin
  Result := nil;
  if AObj = nil then
    Exit;

  Data := AObj.Find(AKey);
  if (Data <> nil) and (Data.JSONType = jtObject) then
    Result := TJSONObject(Data);
end;

function JsonGetArray(AObj: TJSONObject; const AKey: string): TJSONArray;
var
  Data: TJSONData;
begin
  Result := nil;
  if AObj = nil then
    Exit;

  Data := AObj.Find(AKey);
  if (Data <> nil) and (Data.JSONType = jtArray) then
    Result := TJSONArray(Data);
end;

function JsonHasKey(AObj: TJSONObject; const AKey: string): Boolean;
begin
  Result := (AObj <> nil) and (AObj.Find(AKey) <> nil);
end;

{ JSON array helpers }

function JsonArrayToStringArray(AArr: TJSONArray): GoogleDiscovery.Types.TStringArray;
var
  I: Integer;
begin
  if AArr = nil then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  SetLength(Result, AArr.Count);
  for I := 0 to AArr.Count - 1 do
  begin
    if AArr.Items[I].JSONType = jtString then
      Result[I] := AArr.Items[I].AsString
    else
      Result[I] := AArr.Items[I].AsJSON;
  end;
end;

{ JSON creation helpers }

function CreateJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

function CreateJsonArray: TJSONArray;
begin
  Result := TJSONArray.Create;
end;

function CloneJson(AData: TJSONData): TJSONData;
begin
  if AData = nil then
    Result := nil
  else
    Result := AData.Clone;
end;

{ JSON output }

function JsonToString(AData: TJSONData; APretty: Boolean): string;
begin
  if AData = nil then
    Result := 'null'
  else if APretty then
    Result := AData.FormatJSON
  else
    Result := AData.AsJSON;
end;

procedure JsonToFile(AData: TJSONData; const AFileName: string;
  APretty: Boolean);
var
  FileStream: TFileStream;
  Content: string;
begin
  Content := JsonToString(AData, APretty);
  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    if Length(Content) > 0 then
      FileStream.WriteBuffer(Content[1], Length(Content));
  finally
    FileStream.Free;
  end;
end;

{ JSON reference handling }

function IsJsonRef(AObj: TJSONObject): Boolean;
begin
  Result := (AObj <> nil) and (AObj.Find('$ref') <> nil);
end;

function GetJsonRef(AObj: TJSONObject): string;
begin
  Result := JsonGetString(AObj, '$ref', '');
end;

function CreateJsonRef(const ARef: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('$ref', ARef);
end;

end.
