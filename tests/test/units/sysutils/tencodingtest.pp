program tencodingtest;

{$mode delphi}{$H+}
{$codepage cp1251}

uses
  SysUtils, Classes;

function CheckCodePage(const B: TBytes; AEncoding: TEncoding): Boolean;
var
  DetectedEncoding: TEncoding;
begin
  DetectedEncoding := nil;
  Result :=
    (TEncoding.GetBufferEncoding(B, DetectedEncoding) <> 0) and
    (DetectedEncoding = AEncoding);
end;

const
  UTF8Bytes: array[0..18] of byte = ($EF,$BB,$BF,$D0,$9F,$D1,$80,$D0,$BE,$D0,$B2,$D0,$B5,$D1,$80,$D0,$BA,$D0,$B0);
  UTF16Bytes: array[0..17] of byte = ($FF,$FE,$1F,$04,$40,$04,$3E,$04,$32,$04,$35,$04,$40,$04,$3A,$04,$30,$04);
  UTF16BEBytes: array[0..17] of byte = ($FE,$FF,$04,$1F,$04,$40,$04,$3E,$04,$32,$04,$35,$04,$40,$04,$3A,$04,$30);

type
  TCp1251String = type AnsiString(1251);
  TCp866String = type AnsiString(866);
var
  Cp866Encoding,
  Cp1251Encoding: TEncoding;
  Bytes: TBytes;
  Cp1251String: TCp1251String;
  Cp866String: Tcp866String;
  S: AnsiString;
  U8: UTF8String;
  U1, U2: UnicodeString;
begin
  // 1. check various conversions
  Cp866Encoding := TEncoding.GetEncoding('IBM866');
  Cp1251Encoding := TEncoding.GetEncoding('windows-1251');
  Cp1251String := 'Привет земляне!';
  Cp866String := Cp1251String;
  Bytes := Cp1251Encoding.GetBytes(Cp1251String);
  Bytes := TEncoding.Convert(Cp1251Encoding, Cp866Encoding, Bytes);
  SetString(S, PAnsiChar(Bytes), Length(Bytes));
  if not CompareMem(Pointer(S), Pointer(Cp866String), Length(S)) then
    halt(1);
  if StringCodePage(S)<>DefaultSystemCodePage then
    halt(11);
  SetString(Cp1251String,pchar(Cp1251String),length(Cp1251String));
  if StringCodePage(Cp1251String)<>1251 then
    halt(12);
  U1 := Cp866Encoding.GetString(Bytes);
  U2 := TEncoding.Unicode.GetString(TEncoding.Convert(Cp866Encoding, TEncoding.Unicode, Bytes));
  if U1 <> U2 then
    halt(2);
  U1 := TEncoding.BigEndianUnicode.GetString(TEncoding.Convert(Cp866Encoding, TEncoding.BigEndianUnicode, Bytes));
  if U1 <> U2 then
    halt(3);
  Bytes := TEncoding.Convert(Cp866Encoding, TEncoding.UTF8, Bytes);
  U8 := Cp866String;
  if not CompareMem(Pointer(U8), @Bytes[0], Length(U8)) then
    halt(4);
  // 2. check misc functions
  if not (TEncoding.IsStandardEncoding(TEncoding.Unicode) or TEncoding.IsStandardEncoding(TEncoding.UTF8) or TEncoding.IsStandardEncoding(TEncoding.UTF7)) or
    TEncoding.IsStandardEncoding(Cp866Encoding) or TEncoding.IsStandardEncoding(Cp1251Encoding) then
    halt(5);
  if Cp866Encoding.EncodingName = '' then
    halt(6)
  else
    WriteLn(Cp866Encoding.EncodingName);
  if TEncoding.Default.CodePage <> DefaultSystemCodePage then
    halt(7);
  // 3. check codepage detection
  SetLength(Bytes, Length(UTF8Bytes));
  Move(UTF8Bytes[0], Bytes[0], Length(UTF8Bytes));
  if not CheckCodePage(Bytes, TEncoding.UTF8) then
    halt(8);
  SetLength(Bytes, Length(UTF16Bytes));
  Move(UTF16Bytes[0], Bytes[0], Length(UTF16Bytes));
  if not CheckCodePage(Bytes, TEncoding.Unicode) then
    halt(9);
  SetLength(Bytes, Length(UTF16BEBytes));
  Move(UTF16BEBytes[0], Bytes[0], Length(UTF16BEBytes));
  if not CheckCodePage(Bytes, TEncoding.BigEndianUnicode) then
    halt(10);
  Cp866Encoding.Free;
  Cp1251Encoding.Free;
  WriteLn('ok');
end.