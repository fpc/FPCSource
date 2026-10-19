{ Decodes valid and malformed UTF-8 with lenient and MB_ERR_INVALID_CHARS encodings }
program tw41804;

{$mode delphi}{$H+}

uses
  {$ifdef unix}cwstring,{$endif}
  {$ifdef windows}Windows,{$endif}
  SysUtils, SysConst;

const
  cMbcsReplacement = {$ifdef windows}WideChar($FFFD){$else}WideChar('?'){$endif};

var
  gErrors: Integer = 0;

// Reports a failed check
procedure Fail(const aMsg: string);

begin
  Writeln('FAIL: ', aMsg);
  Inc(gErrors);
end;


// Returns the code units of aText in hex
function Dump(const aText: UnicodeString): string;

var
  I: Integer;

begin
  Result := '';
  for I := 1 to Length(aText) do
    Result := Result + IntToHex(Ord(aText[I]), 4) + ' ';
end;


// Checks that aEncoding decodes aBytes to aExpected without raising
procedure CheckDecodes(const aName: string; aEncoding: TEncoding; const aBytes: TBytes; const aExpected: UnicodeString);

var
  lText: UnicodeString;

begin
  try
    lText := aEncoding.GetString(aBytes);
    if lText <> aExpected then
      Fail(aName + ': expected ' + Dump(aExpected) + 'got ' + Dump(lText));
    if aEncoding.GetCharCount(aBytes) <> Length(aExpected) then
      Fail(aName + ': GetCharCount must be ' + IntToStr(Length(aExpected)));
  except
    on E: Exception do
      Fail(aName + ': unexpected ' + E.ClassName + ': ' + E.Message);
  end;
end;


// Checks that aEncoding rejects aBytes: count 0 and EEncodingError from GetString, GetChars and Convert
procedure CheckRejects(const aName: string; aEncoding: TEncoding; const aBytes: TBytes);

begin
  if aEncoding.GetCharCount(aBytes) <> 0 then
    Fail(aName + ': GetCharCount must be 0 for malformed input');
  try
    aEncoding.GetString(aBytes);
    Fail(aName + ': GetString must raise EEncodingError');
  except
    on E: EEncodingError do
      if E.Message <> SNoMappingForUnicodeCharacter then
        Fail(aName + ': unexpected message ' + E.Message);
  end;
  try
    aEncoding.GetChars(aBytes, 0, Length(aBytes));
    Fail(aName + ': GetChars with index must raise EEncodingError');
  except
    on EEncodingError do ;
  end;
  try
    TEncoding.Convert(aEncoding, TEncoding.Unicode, aBytes);
    Fail(aName + ': Convert must raise EEncodingError');
  except
    on EEncodingError do ;
  end;
end;


// Runs the valid and malformed inputs against one strict encoding
procedure CheckStrict(const aName: string; aEncoding: TEncoding);

begin
  CheckDecodes(aName + ' valid 2-byte', aEncoding, TBytes.Create($41, $C3, $A9, $42), 'A'#$00E9'B');
  CheckDecodes(aName + ' valid 4-byte', aEncoding, TBytes.Create($F0, $9D, $84, $9E), #$D834#$DD1E);
  CheckDecodes(aName + ' empty', aEncoding, nil, '');
  CheckRejects(aName + ' stray continuation', aEncoding, TBytes.Create($41, $80, $42));
  CheckRejects(aName + ' truncated', aEncoding, TBytes.Create($41, $C3));
  CheckRejects(aName + ' overlong 2-byte', aEncoding, TBytes.Create($C0, $AF));
  CheckRejects(aName + ' overlong 3-byte', aEncoding, TBytes.Create($E0, $80, $AF));
  CheckRejects(aName + ' surrogate', aEncoding, TBytes.Create($ED, $A0, $80));
  CheckRejects(aName + ' above U+10FFFF', aEncoding, TBytes.Create($F4, $90, $80, $80));
  CheckRejects(aName + ' invalid lead byte', aEncoding, TBytes.Create($41, $FF));
end;


// Checks that aEncoding replaces malformed input by aReplacement instead of raising
procedure CheckLenient(const aName: string; aEncoding: TEncoding; aReplacement: WideChar = '?');

begin
  CheckDecodes(aName + ' valid 2-byte', aEncoding, TBytes.Create($41, $C3, $A9, $42), 'A'#$00E9'B');
  CheckDecodes(aName + ' stray continuation', aEncoding, TBytes.Create($41, $80, $42), 'A' + aReplacement + 'B');
  CheckDecodes(aName + ' truncated', aEncoding, TBytes.Create($41, $C3), 'A' + aReplacement);
end;


var
  lUtf8, lUtf8Strict, lMbcs, lMbcsStrict, lClone, lLenientClone: TEncoding;

begin
  lUtf8 := TUTF8Encoding.Create;
  lUtf8Strict := TUTF8Encoding.Create(CP_UTF8, MB_ERR_INVALID_CHARS, 0);
  lMbcs := TMBCSEncoding.Create(CP_UTF8);
  lMbcsStrict := TMBCSEncoding.Create(CP_UTF8, MB_ERR_INVALID_CHARS, 0);
  lClone := lUtf8Strict.Clone;
  lLenientClone := lUtf8.Clone;
  try
    CheckLenient('TEncoding.UTF8', TEncoding.UTF8);
    CheckLenient('TUTF8Encoding.Create', lUtf8);
    CheckLenient('TUTF8Encoding.Create.Clone', lLenientClone);
    CheckLenient('TMBCSEncoding.Create(CP_UTF8)', lMbcs, cMbcsReplacement);
    CheckStrict('TUTF8Encoding strict', lUtf8Strict);
    CheckStrict('TUTF8Encoding strict Clone', lClone);
    CheckStrict('TMBCSEncoding strict', lMbcsStrict);
  finally
    lUtf8.Free;
    lUtf8Strict.Free;
    lMbcs.Free;
    lMbcsStrict.Free;
    lClone.Free;
    lLenientClone.Free;
  end;
  if gErrors > 0 then
  begin
    Writeln(gErrors, ' errors');
    Halt(1);
  end;
  Writeln('ok');
end.
