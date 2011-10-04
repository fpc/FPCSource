program tencodingtest;

{$mode delphi}{$H+}
{$codepage cp1251}

uses
  SysUtils, Classes, unit1;

function CheckCodePage(AFileName: String; AEncoding: TEncoding): Boolean;
var
  S: TStream;
  B: TBytes;
  DetectedEncoding: TEncoding;
begin
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(B, S.Size);
    S.Read(B[0], S.Size);
    DetectedEncoding := nil;
    Result :=
      (TEncoding.GetBufferEncoding(B, DetectedEncoding) <> 0) and
      (DetectedEncoding = AEncoding);
  finally
    S.Free;
  end;
end;

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
  if not CheckCodePage('utf8.txt', TEncoding.UTF8) then
    halt(8);
  if not CheckCodePage('utf16.txt', TEncoding.Unicode) then
    halt(9);
  if not CheckCodePage('utf16be.txt', TEncoding.BigEndianUnicode) then
    halt(10);
  Cp866Encoding.Free;
  Cp1251Encoding.Free;
  WriteLn('ok');
end.

