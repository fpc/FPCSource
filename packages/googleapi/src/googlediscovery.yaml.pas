{
  GoogleDiscovery.Yaml - YAML output generation

  Provides functions to convert JSON data structures to YAML format
  for OpenAPI specification output.
}
unit GoogleDiscovery.Yaml;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data;
  {$ELSE}
  Classes, SysUtils, fpjson;
  {$ENDIF}

const
  YAML_INDENT_SIZE = 2;

{ YAML serialization }
function JsonToYaml(const AData: TJSONData; AIndent: Integer = 0): string;
procedure SaveAsYaml(const AData: TJSONData; const AFileName: string);

{ YAML string utilities }
function EscapeYamlString(const AValue: string): string;
function NeedsQuoting(const AValue: string): Boolean;
function FormatYamlString(const AValue: string): string;
function FormatYamlMultilineString(const AValue: string; AIndent: Integer): string;

{ YAML key formatting }
function FormatYamlKey(const AKey: string): string;
function IsValidYamlKey(const AKey: string): Boolean;

{ Indentation helpers }
function MakeIndent(ALevel: Integer): string;

implementation

uses
  GoogleDiscovery.Transform;

{ Internal: Convert JSON value to YAML string representation }
function JsonValueToYaml(const AData: TJSONData; AIndent: Integer): string; forward;
function JsonObjectToYaml(const AObj: TJSONObject; AIndent: Integer): string; forward;
function JsonArrayToYaml(const AArr: TJSONArray; AIndent: Integer): string; forward;

{ Indentation helpers }

function MakeIndent(ALevel: Integer): string;
begin
  Result := StringOfChar(' ', ALevel * YAML_INDENT_SIZE);
end;

{ YAML string utilities }

function NeedsQuoting(const AValue: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := False;

  if AValue = '' then
  begin
    Result := True;
    Exit;
  end;

  // Check for special YAML values
  if (AValue = 'true') or (AValue = 'false') or
     (AValue = 'null') or (AValue = 'yes') or (AValue = 'no') or
     (AValue = 'on') or (AValue = 'off') or
     (AValue = '~') or (AValue = 'True') or (AValue = 'False') or
     (AValue = 'TRUE') or (AValue = 'FALSE') or
     (AValue = 'Yes') or (AValue = 'No') or
     (AValue = 'YES') or (AValue = 'NO') then
  begin
    Result := True;
    Exit;
  end;

  // Check first character
  C := AValue[1];
  if C in [':', '#', '&', '*', '!', '|', '>', '''', '"', '%', '@', '`',
           '-', '?', '[', ']', '{', '}', ',', ' ', #9] then
  begin
    Result := True;
    Exit;
  end;

  // Check if it looks like a number or version string
  if C in ['0'..'9', '+', '-', '.'] then
  begin
    // Could be interpreted as a number - needs quoting
    Result := True;
    Exit;
  end;

  // Check for special characters in the string
  for I := 1 to Length(AValue) do
  begin
    C := AValue[I];
    if (C in [#0..#31]) or (C = ':') or (C = '#') or (C = '"') or (C = '''') then
    begin
      Result := True;
      Exit;
    end;
  end;

  // Check for trailing/leading spaces
  if (AValue[1] = ' ') or (AValue[Length(AValue)] = ' ') then
    Result := True;
end;

function EscapeYamlString(const AValue: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(AValue) do
  begin
    C := AValue[I];
    case C of
      '\': Result := Result + '\\';
      '"': Result := Result + '\"';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #13: Result := Result + '\r';
      #0..#8, #11, #12, #14..#31:
        Result := Result + '\x' + IntToHex(Ord(C), 2);
    else
      Result := Result + C;
    end;
  end;
end;

function FormatYamlString(const AValue: string): string;
begin
  if NeedsQuoting(AValue) then
    Result := '"' + EscapeYamlString(AValue) + '"'
  else
    Result := AValue;
end;

function FormatYamlMultilineString(const AValue: string; AIndent: Integer): string;
var
  Lines: TStringList;
  I: Integer;
  IndentStr: string;
begin
  // Check if multiline
  if (Pos(#10, AValue) = 0) and (Pos(#13, AValue) = 0) then
  begin
    Result := FormatYamlString(AValue);
    Exit;
  end;

  // Use literal block scalar for multiline
  IndentStr := MakeIndent(AIndent);
  Lines := TStringList.Create;
  try
    Lines.Text := AValue;
    Result := '|';

    for I := 0 to Lines.Count - 1 do
      Result := Result + #10 + IndentStr + Lines[I];
  finally
    Lines.Free;
  end;
end;

{ YAML key formatting }

function IsValidYamlKey(const AKey: string): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := False;

  if AKey = '' then
    Exit;

  // First character must be letter, underscore, or unicode letter
  C := AKey[1];
  if not (C in ['a'..'z', 'A'..'Z', '_']) then
    Exit;

  // Remaining characters
  for I := 2 to Length(AKey) do
  begin
    C := AKey[I];
    if not (C in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']) then
      Exit;
  end;

  Result := True;
end;

function FormatYamlKey(const AKey: string): string;
begin
  if IsValidYamlKey(AKey) then
    Result := AKey
  else
    Result := '"' + EscapeYamlString(AKey) + '"';
end;

{ JSON to YAML conversion }

function JsonValueToYaml(const AData: TJSONData; AIndent: Integer): string;
begin
  if AData = nil then
  begin
    Result := 'null';
    Exit;
  end;

  case AData.JSONType of
    jtNull:
      Result := 'null';

    jtBoolean:
      if AData.AsBoolean then
        Result := 'true'
      else
        Result := 'false';

    jtNumber:
      begin
        // Check if it's an integer or float
        if Frac(AData.AsFloat) = 0 then
          Result := IntToStr(AData.AsInt64)
        else
          Result := FloatToStr(AData.AsFloat);
      end;

    jtString:
      Result := FormatYamlMultilineString(AData.AsString, AIndent + 1);

    jtObject:
      Result := JsonObjectToYaml(TJSONObject(AData), AIndent);

    jtArray:
      Result := JsonArrayToYaml(TJSONArray(AData), AIndent);

    else
      Result := 'null';
  end;
end;

function JsonObjectToYaml(const AObj: TJSONObject; AIndent: Integer): string;
var
  I: Integer;
  Key: string;
  Value: TJSONData;
  IndentStr: string;
  ValueStr: string;
  IsFirst: Boolean;
begin
  if (AObj = nil) or (AObj.Count = 0) then
  begin
    Result := '{}';
    Exit;
  end;

  Result := '';
  IndentStr := MakeIndent(AIndent);
  IsFirst := True;

  for I := 0 to AObj.Count - 1 do
  begin
    Key := AObj.Names[I];
    Value := AObj.Items[I];

    if not IsFirst then
      Result := Result + #10;
    IsFirst := False;

    Result := Result + IndentStr + FormatYamlKey(Key) + ':';

    if Value = nil then
    begin
      Result := Result + ' null';
    end
    else if Value.JSONType in [jtObject, jtArray] then
    begin
      // Complex types go on next line with increased indent
      if ((Value.JSONType = jtObject) and (TJSONObject(Value).Count > 0)) or
         ((Value.JSONType = jtArray) and (TJSONArray(Value).Count > 0)) then
      begin
        ValueStr := JsonValueToYaml(Value, AIndent + 1);
        Result := Result + #10 + ValueStr;
      end
      else
      begin
        // Empty object or array
        if Value.JSONType = jtObject then
          Result := Result + ' {}'
        else
          Result := Result + ' []';
      end;
    end
    else
    begin
      // Simple types on same line
      ValueStr := JsonValueToYaml(Value, AIndent);
      Result := Result + ' ' + ValueStr;
    end;
  end;
end;

function JsonArrayToYaml(const AArr: TJSONArray; AIndent: Integer): string;
var
  I: Integer;
  Item: TJSONData;
  IndentStr: string;
  ValueStr: string;
  IsFirst: Boolean;
begin
  if (AArr = nil) or (AArr.Count = 0) then
  begin
    Result := '[]';
    Exit;
  end;

  Result := '';
  IndentStr := MakeIndent(AIndent);
  IsFirst := True;

  for I := 0 to AArr.Count - 1 do
  begin
    Item := AArr.Items[I];

    if not IsFirst then
      Result := Result + #10;
    IsFirst := False;

    Result := Result + IndentStr + '-';

    if Item = nil then
    begin
      Result := Result + ' null';
    end
    else if Item.JSONType in [jtObject, jtArray] then
    begin
      if ((Item.JSONType = jtObject) and (TJSONObject(Item).Count > 0)) or
         ((Item.JSONType = jtArray) and (TJSONArray(Item).Count > 0)) then
      begin
        ValueStr := JsonValueToYaml(Item, AIndent + 1);
        // For objects, put first key on same line as dash
        if Item.JSONType = jtObject then
        begin
          // Remove leading indent from first line
          ValueStr := TrimLeft(ValueStr);
          Result := Result + ' ' + ValueStr;
        end
        else
          Result := Result + #10 + ValueStr;
      end
      else
      begin
        if Item.JSONType = jtObject then
          Result := Result + ' {}'
        else
          Result := Result + ' []';
      end;
    end
    else
    begin
      ValueStr := JsonValueToYaml(Item, AIndent);
      Result := Result + ' ' + ValueStr;
    end;
  end;
end;

function JsonToYaml(const AData: TJSONData; AIndent: Integer): string;
begin
  if AData = nil then
  begin
    Result := 'null';
    Exit;
  end;

  Result := JsonValueToYaml(AData, AIndent);
end;

procedure SaveAsYaml(const AData: TJSONData; const AFileName: string);
var
  FileStream: TFileStream;
  YamlContent: string;
  Dir: string;
begin
  // Ensure directory exists
  Dir := ExtractFileDir(AFileName);
  if (Dir <> '') and not DirectoryExists(Dir) then
    ForceDirectories(Dir);

  YamlContent := JsonToYaml(AData, 0);

  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    if Length(YamlContent) > 0 then
      FileStream.WriteBuffer(YamlContent[1], Length(YamlContent));
  finally
    FileStream.Free;
  end;
end;

end.
