{ TEncoding.UTF8 conversions without a widestring manager }
program tw40588;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  Failures: Integer = 0;

// Records a failed check
procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    Inc(Failures);
    Writeln('FAIL: ', aMsg);
    end;
end;


// Returns aBytes as hexadecimal byte values
function BytesToHex(const aBytes: TBytes): string;

var
  I: Integer;

begin
  Result := '';
  for I := 0 to Length(aBytes) - 1 do
    Result := Result + IntToHex(aBytes[I], 2) + ' ';
  Result := Trim(Result);
end;


// Returns aText as hexadecimal UTF-16 code units
function CharsToHex(const aText: UnicodeString): string;

var
  I: Integer;

begin
  Result := '';
  for I := 1 to Length(aText) do
    Result := Result + IntToHex(Ord(aText[I]), 4) + ' ';
  Result := Trim(Result);
end;


// Returns a TBytes with the given values
function MakeBytes(const aValues: array of Byte): TBytes;

var
  I: Integer;

begin
  SetLength(Result, Length(aValues));
  for I := 0 to High(aValues) do
    Result[I] := aValues[I];
end;


const
  { 'A', e acute, euro sign, G clef as a surrogate pair }
  cText: UnicodeString = 'A'#$00E9#$20AC#$D834#$DD1E;
  cUtf8Hex = '41 C3 A9 E2 82 AC F0 9D 84 9E';

var
  lBytes, lBuffer, lUtf16: TBytes;
  lText: UnicodeString;
  lCount: Integer;

begin
  lBytes := TEncoding.UTF8.GetBytes(cText);
  Check(BytesToHex(lBytes) = cUtf8Hex, 'GetBytes: got ' + BytesToHex(lBytes) + ', expected ' + cUtf8Hex);
  Check(TEncoding.UTF8.GetByteCount(cText) = 10, 'GetByteCount: got ' + IntToStr(TEncoding.UTF8.GetByteCount(cText)));

  lBytes := MakeBytes([$41, $C3, $A9, $E2, $82, $AC, $F0, $9D, $84, $9E]);
  lText := TEncoding.UTF8.GetString(lBytes);
  Check(lText = cText, 'GetString: got ' + CharsToHex(lText) + ', expected ' + CharsToHex(cText));
  Check(TEncoding.UTF8.GetCharCount(lBytes) = 5, 'GetCharCount: got ' + IntToStr(TEncoding.UTF8.GetCharCount(lBytes)));

  SetLength(lBuffer, 14);
  FillChar(lBuffer[0], Length(lBuffer), $55);
  lCount := TEncoding.UTF8.GetBytes(cText, 2, 2, lBuffer, 3);
  Check(lCount = 5, 'GetBytes into buffer: returned ' + IntToStr(lCount) + ', expected 5');
  Check(BytesToHex(lBuffer) = '55 55 55 C3 A9 E2 82 AC 55 55 55 55 55 55',
    'GetBytes into buffer: buffer is ' + BytesToHex(lBuffer));

  lText := TEncoding.UTF8.GetString(MakeBytes([$41, $FF, $42]));
  Check(lText = 'A?B', 'GetString of an invalid byte: got ' + CharsToHex(lText) + ', expected 0041 003F 0042');
  lText := TEncoding.UTF8.GetString(MakeBytes([$41, $E2, $82]));
  Check(lText = 'A?', 'GetString of a truncated sequence: got ' + CharsToHex(lText) + ', expected 0041 003F');

  Check(Length(TEncoding.UTF8.GetBytes(UnicodeString(''))) = 0, 'GetBytes of an empty string is not empty');
  Check(TEncoding.UTF8.GetString(nil) = '', 'GetString of no bytes is not empty');

  lUtf16 := TEncoding.Convert(TEncoding.UTF8, TEncoding.Unicode, lBytes);
  Check(BytesToHex(lUtf16) = '41 00 E9 00 AC 20 34 D8 1E DD', 'Convert UTF-8 to UTF-16: got ' + BytesToHex(lUtf16));
  lBytes := TEncoding.Convert(TEncoding.Unicode, TEncoding.UTF8, lUtf16);
  Check(BytesToHex(lBytes) = cUtf8Hex, 'Convert UTF-16 to UTF-8: got ' + BytesToHex(lBytes));

  if Failures > 0 then
    Halt(1);
  Writeln('ok');
end.
