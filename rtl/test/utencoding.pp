unit utencoding;

{$mode delphi}{$H+}
{$codepage cp1251}

interface

uses
  SysUtils, Classes;

implementation

uses punit, utrtl;

function CheckCodePage(const B: TBytes; AEncoding: TEncoding): Boolean;
var
  DetectedEncoding: TEncoding;
begin
  DetectedEncoding := nil;
  Result :=
    (TEncoding.GetBufferEncoding(B, DetectedEncoding) <> 0) and
    (DetectedEncoding = AEncoding);
end;

Function DoEncodingTest : AnsiString;

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
  Cp1251String,
  Cp1251String2: TCp1251String;
  Cp866String: Tcp866String;
  S: AnsiString;
  U8: UTF8String;
  U1, U2: UnicodeString;
begin
  Result:='';
  // 1. check various conversions
  Cp866Encoding := TEncoding.GetEncoding('IBM866');
  Cp1251Encoding := TEncoding.GetEncoding('windows-1251');
  Cp1251String := 'Привет земляне!';
  Cp866String := Cp1251String;
  Bytes := Cp1251Encoding.GetBytes(Cp1251String);
  Bytes := TEncoding.Convert(Cp1251Encoding, Cp866Encoding, Bytes);
  SetString(S, PAnsiChar(Bytes), Length(Bytes));
  if not CompareMem(Pointer(S), Pointer(Cp866String), Length(S)) then
    Exit('Error at 1');
  if StringCodePage(S)<>CP_ACP then
    Exit('Error at 11');
  Cp1251String2:=Cp1251String;
  SetString(Cp1251String,pchar(Cp1251String2),length(Cp1251String2));
  if StringCodePage(Cp1251String)<>1251 then
    Exit('Error at 12');
  U1 := Cp866Encoding.GetString(Bytes);
  U2 := TEncoding.Unicode.GetString(TEncoding.Convert(Cp866Encoding, TEncoding.Unicode, Bytes));
  if U1 <> U2 then
    Exit('Error at 2');
  U1 := TEncoding.BigEndianUnicode.GetString(TEncoding.Convert(Cp866Encoding, TEncoding.BigEndianUnicode, Bytes));
  if U1 <> U2 then
    Exit('Error at 3');
  Bytes := TEncoding.Convert(Cp866Encoding, TEncoding.UTF8, Bytes);
  U8 := Cp866String;
  if not CompareMem(Pointer(U8), @Bytes[0], Length(U8)) then
    Exit('Error at 4');
  // 2. check misc functions
  if not (TEncoding.IsStandardEncoding(TEncoding.Unicode) or TEncoding.IsStandardEncoding(TEncoding.UTF8) or TEncoding.IsStandardEncoding(TEncoding.UTF7)) or
    TEncoding.IsStandardEncoding(Cp866Encoding) or TEncoding.IsStandardEncoding(Cp1251Encoding) then
    Exit('Error at 5');
  if Cp866Encoding.EncodingName = '' then
    Exit('Error at 6')
  else if ShowDebugOutput then
    WriteLn(Cp866Encoding.EncodingName);
  if TEncoding.Default.CodePage <> DefaultSystemCodePage then
    Exit('Error at 7');
  // 3. check codepage detection
  SetLength(Bytes, Length(UTF8Bytes));
  Move(UTF8Bytes[0], Bytes[0], Length(UTF8Bytes));
  if not CheckCodePage(Bytes, TEncoding.UTF8) then
    Exit('Error at 8');
  SetLength(Bytes, Length(UTF16Bytes));
  Move(UTF16Bytes[0], Bytes[0], Length(UTF16Bytes));
  if not CheckCodePage(Bytes, TEncoding.Unicode) then
    Exit('Error at 9');
  SetLength(Bytes, Length(UTF16BEBytes));
  Move(UTF16BEBytes[0], Bytes[0], Length(UTF16BEBytes));
  if not CheckCodePage(Bytes, TEncoding.BigEndianUnicode) then
    Exit('Error at 10');
  Cp866Encoding.Free;
  Cp1251Encoding.Free;
  Result:='';
end;

Function DoEncodingTest2 : AnsiString;

var
  ACP,StartDefaultSystemCodePage: TSystemCodePage;

begin
  StartDefaultSystemCodePage := DefaultSystemCodePage;
  ACP:=TEncoding.ANSI.CodePage;
  try
    // test creating ANSI when DefaultSystemCodePage is set to non-ANSI
    if DefaultSystemCodePage<>CP_UTF8 then
      DefaultSystemCodePage := CP_UTF8
    else
      DefaultSystemCodePage := 1250;
    if TEncoding.ANSI.CodePage<>ACP then
      Exit('AnsiCodePage changed when setting DefaultSystemCodePage to non-initial value');

    // test default
    DefaultSystemCodePage := StartDefaultSystemCodePage;
    if TEncoding.ANSI.CodePage<>TEncoding.SystemEncoding.CodePage then
      Exit('Ansi codepage not set to UTF8');

    // try utf-8
    DefaultSystemCodePage := CP_UTF8;
    if TEncoding.ANSI.CodePage<>ACP then
      Exit('AnsiCodePage changed when setting DefaultSystemCodePage to UTF8');
    if TEncoding.SystemEncoding.CodePage<>DefaultSystemCodePage then
      Exit('SystemEncoding differs from defaultsystemcodepage');

    // try a different single-byte encoding
    if StartDefaultSystemCodePage=1250 then
      DefaultSystemCodePage := 1251
    else
      DefaultSystemCodePage := 1250;

    if TEncoding.ANSI.CodePage<>ACP then
      Exit('Ansicodepage changed when setting defaultsystemcodepage to different single-byte codepage');
    if TEncoding.SystemEncoding.CodePage<>DefaultSystemCodePage then
      Exit('SystemEncoding not correctly set after changing to different single-byte codepage');

    // try start again
    DefaultSystemCodePage := StartDefaultSystemCodePage;
    if TEncoding.SystemEncoding.CodePage<>DefaultSystemCodePage then
      Exit('Systemencoding codepage not set correct when changing back to original');

  finally
    DefaultSystemCodePage:=StartDefaultSystemCodePage;
  end;
end;



begin  
  SysUtilsTest('EncodingTest',@DoEncodingTest);
  SysUtilsTest('EncodingTest2',@DoEncodingTest2);
end.Encodin
