{$mode objfpc}
{$h+}

unit System.JSON.Utils;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Types, System.TypInfo, System.Classes, Fcl.Streams.Extra,
  {$ELSE}
  SysUtils, Types, TypInfo, Classes, StreamEx,
  {$ENDIF}
  System.JSON.Types, System.NetEncoding;

type

  { TJsonTextUtils }

  TJsonTextUtils = class
  const
    kArrayEscapedArraySize = 128;
    EscapedUnicodeText = '!';
  private
    class var FDoubleQuoteCharEscapeFlags: TBooleanDynArray;
    class var FHtmlCharEscapeFlags: TBooleanDynArray;
    class var FSingleQuoteCharEscapeFlags: TBooleanDynArray;
  public
    class constructor Create;
    class procedure WriteEscapedString(const Writer: TTextWriter; const Str: string; Delimiter: Char;
      AppendDelimiters: Boolean; const CharEscapeFlags: array of Boolean; StringEscapeHandling: TJsonStringEscapeHandling;
      var WriteBuffer: TCharArray);
    class function ShouldEscapeJavaScriptString(const S: string; const CharEscapeFlags: array of Boolean): Boolean;
    class property SingleQuoteCharEscapeFlags: TBooleanDynArray read FSingleQuoteCharEscapeFlags;
    class property DoubleQuoteCharEscapeFlags: TBooleanDynArray read FDoubleQuoteCharEscapeFlags;
    class property HtmlCharEscapeFlags: TBooleanDynArray read FHtmlCharEscapeFlags;
    class procedure ToCharAsUnicode(C: Char; var Buffer: array of Char);
    class function IsWhiteSpace(const Str: string): Boolean;
  end;

  { TJsonTypeUtils }

  TJsonTypeUtils = class
  public
    class function InheritsFrom(ATypeInfo: PTypeInfo; const AParentClass: TClass): Boolean; static;
    class function IsAssignableFrom(ATo, AFrom: PTypeInfo): Boolean; static;
    class function GetTypeName(ATypeInfo: PTypeInfo): string; static;
  end;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Character, System.DateUtils;
  {$ELSE}
  Character, DateUtils;
  {$ENDIF}

{ TJsonTextUtils }

class constructor TJsonTextUtils.Create;
var
  I: Integer;
begin
  // Initialize escape flag arrays
  SetLength(FDoubleQuoteCharEscapeFlags, kArrayEscapedArraySize);
  SetLength(FHtmlCharEscapeFlags, kArrayEscapedArraySize);
  SetLength(FSingleQuoteCharEscapeFlags, kArrayEscapedArraySize);

  // Set escape flags for non-printable ASCII characters (0-31 and 127)
  for I := 0 to 31 do
  begin
    FDoubleQuoteCharEscapeFlags[I] := True;
    FHtmlCharEscapeFlags[I] := True;
    FSingleQuoteCharEscapeFlags[I] := True;
  end;

  FDoubleQuoteCharEscapeFlags[127] := True;
  FHtmlCharEscapeFlags[127] := True;
  FSingleQuoteCharEscapeFlags[127] := True;

  // DoubleQuoteCharEscapeFlags: Escape double quotes and backslash
  FDoubleQuoteCharEscapeFlags[Ord('"')] := True;
  FDoubleQuoteCharEscapeFlags[Ord('\')] := True;

  // HtmlCharEscapeFlags: Escape HTML special characters
  FHtmlCharEscapeFlags[Ord('"')] := True;  // Double quote
  FHtmlCharEscapeFlags[Ord('''')] := True; // Single quote
  FHtmlCharEscapeFlags[Ord('<')] := True;  // Less than
  FHtmlCharEscapeFlags[Ord('>')] := True;  // Greater than
  FHtmlCharEscapeFlags[Ord('&')] := True;  // Ampersand
  FHtmlCharEscapeFlags[Ord('\')] := True; // Backslash

  // SingleQuoteCharEscapeFlags: Escape single quotes and backslash
  FSingleQuoteCharEscapeFlags[Ord('''')] := True;
  FSingleQuoteCharEscapeFlags[Ord('\')] := True;
end;

class procedure TJsonTextUtils.WriteEscapedString(const Writer: TTextWriter; const Str: string; Delimiter: Char;
  AppendDelimiters: Boolean; const CharEscapeFlags: array of Boolean; StringEscapeHandling: TJsonStringEscapeHandling;
  var WriteBuffer: TCharArray);
var
  I, BufferPos: Integer;
  CurrentChar: Char;
  CharCode: Integer;
  UnicodeBuffer: array[0..5] of Char;

  procedure WriteChar(C: Char);
  begin
    if BufferPos >= Length(WriteBuffer) then
      SetLength(WriteBuffer, Length(WriteBuffer) * 2);
    WriteBuffer[BufferPos] := C;
    Inc(BufferPos);
  end;

  procedure WriteString(const S: string);
  var
    J: Integer;
  begin
    for J := 1 to Length(S) do
      WriteChar(S[J]);
  end;

  procedure WriteUnicodeEscape(C: Char);
  var
    J: Integer;
  begin
    ToCharAsUnicode(C, UnicodeBuffer);
    for J := 0 to 5 do
      WriteChar(UnicodeBuffer[J]);
  end;

begin
  if not Assigned(Writer) then
    raise EArgumentNilException.Create('Writer cannot be nil');

  BufferPos := 0;
  if Length(WriteBuffer) = 0 then
    SetLength(WriteBuffer, 256);

  // Add opening delimiter if requested
  if AppendDelimiters then
    WriteChar(Delimiter);

  // Process each character in the string
  for I := 1 to Length(Str) do
  begin
    CurrentChar := Str[I];
    CharCode := Ord(CurrentChar);

    // Check if character needs escaping
    if (CharCode < Length(CharEscapeFlags)) and CharEscapeFlags[CharCode] then
    begin
      case CurrentChar of
        '"': WriteString('\"');
        '\': WriteString('\\');
        '/':
          if StringEscapeHandling = TJsonStringEscapeHandling.EscapeHtml then
            WriteString('\/')
          else
            WriteChar('/');
        #8: WriteString('\b');   // Backspace
        #12: WriteString('\f');  // Form feed
        #10: WriteString('\n');  // Line feed
        #13: WriteString('\r');  // Carriage return
        #9: WriteString('\t');   // Tab
        else
          WriteUnicodeEscape(CurrentChar);
      end;
    end
    else if (CharCode > 127) and (StringEscapeHandling <> TJsonStringEscapeHandling.Default) then
    begin
      // Non-ASCII characters - use Unicode escape if required
      WriteUnicodeEscape(CurrentChar);
    end
    else
    begin
      // Regular character - no escaping needed
      WriteChar(CurrentChar);
    end;
  end;

  // Add closing delimiter if requested
  if AppendDelimiters then
    WriteChar(Delimiter);

  // Write the buffer to the writer
  if BufferPos > 0 then
  begin
    SetLength(WriteBuffer, BufferPos);
    Writer.Write(WriteBuffer, 0, BufferPos);
  end;
end;

class function TJsonTextUtils.ShouldEscapeJavaScriptString(const S: string; const CharEscapeFlags: array of Boolean): Boolean;
var
  I: Integer;
  CharCode: Integer;
begin
  Result := False;

  for I := 1 to Length(S) do
  begin
    CharCode := Ord(S[I]);

    // Check if character is outside ASCII range or needs escaping
    if (CharCode > 127) or ((CharCode < Length(CharEscapeFlags)) and CharEscapeFlags[CharCode]) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

class procedure TJsonTextUtils.ToCharAsUnicode(C: Char; var Buffer: array of Char);
var
  CharCode: Integer;
  function GetHexDigit(AValue: Integer): Char;
  begin
    if (AValue >= 0) and (AValue <= 9) then
      Result := Chr(Ord('0') + AValue)
    else if (AValue >= 10) and (AValue <= 15) then
      Result := Chr(Ord('A') + AValue - 10)
    else
      Result := '0';
  end;
begin
  if Length(Buffer) < 6 then
    raise EArgumentException.Create('Buffer too small for Unicode escape sequence');

  CharCode := Ord(C);
  Buffer[0] := '\';
  Buffer[1] := 'u';
  Buffer[2] := GetHexDigit((CharCode shr 12) and $F);
  Buffer[3] := GetHexDigit((CharCode shr 8) and $F);
  Buffer[4] := GetHexDigit((CharCode shr 4) and $F);
  Buffer[5] := GetHexDigit(CharCode and $F);
end;

class function TJsonTextUtils.IsWhiteSpace(const Str: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  if Str = '' then
    Exit;

  for I := 1 to Length(Str) do
  begin
    if not TCharacter.IsWhiteSpace(Str[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

{ TJsonTypeUtils }

class function TJsonTypeUtils.InheritsFrom(ATypeInfo: PTypeInfo; const AParentClass: TClass): Boolean;
var
  ClassTypeInfo: PTypeInfo;
  ClassData: PTypeData;
begin
  Result := False;

  if not Assigned(ATypeInfo) or not Assigned(AParentClass) then
    Exit;

  if ATypeInfo^.Kind <> tkClass then
    Exit;

  ClassTypeInfo := ATypeInfo;
  while Assigned(ClassTypeInfo) and (ClassTypeInfo^.Kind = tkClass) do
  begin
    ClassData := GetTypeData(ClassTypeInfo);
    if Assigned(ClassData) and Assigned(ClassData^.ClassType) then
    begin
      if ClassData^.ClassType = AParentClass then
      begin
        Result := True;
        Exit;
      end;
      ClassTypeInfo := ClassData^.ParentInfo;
    end
    else
      Break;
  end;
end;

class function TJsonTypeUtils.IsAssignableFrom(ATo, AFrom: PTypeInfo): Boolean;
var
  ToKind, FromKind: TTypeKind;
begin
  Result := False;

  if not Assigned(ATo) or not Assigned(AFrom) then
    Exit;

  ToKind := ATo^.Kind;
  FromKind := AFrom^.Kind;

  // Same type
  if ATo = AFrom then
  begin
    Result := True;
    Exit;
  end;

  // Handle numeric type conversions
  if (ToKind in [tkInteger, tkInt64, tkFloat, tkQWord]) and
     (FromKind in [tkInteger, tkInt64, tkFloat, tkQWord]) then
  begin
    Result := True;
    Exit;
  end;

  // Handle string type conversions
  if (ToKind in [tkAString, tkUString, tkWString, tkSString, tkLString]) and
     (FromKind in [tkAString, tkUString, tkWString, tkSString, tkLString]) then
  begin
    Result := True;
    Exit;
  end;

  // Handle class inheritance
  if (ToKind = tkClass) and (FromKind = tkClass) then
  begin
    Result := InheritsFrom(AFrom, GetTypeData(ATo)^.ClassType);
    Exit;
  end;

  // Handle interface assignments
  if (ToKind = tkInterface) and (FromKind = tkInterface) then
  begin
    // For simplicity, assume interfaces with same GUID are assignable
    Result := CompareMem(@GetTypeData(ATo)^.Guid, @GetTypeData(AFrom)^.Guid, SizeOf(TGUID));
    Exit;
  end;
end;

class function TJsonTypeUtils.GetTypeName(ATypeInfo: PTypeInfo): string;
begin
  if Assigned(ATypeInfo) then
    Result := ATypeInfo^.Name
  else
    Result := 'Unknown';
end;

end.
