// to have correct test result with delphi set codepage option to 65001
program tcpstr17;
{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}

{$ifdef FPC}
  {$mode delphi}
  {$codepage utf8}
{$endif}
{$apptype console}

{$ifndef USE_INTERNAL_UNICODE}
{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif unix}
{$else def USE_INTERNAL_UNICODE}
uses
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
 { The unit strings is not really used here,
   but simpifies the conditional construction
   for fpwidestring and unicodeducet use }
  strings;
{$endif def USE_INTERNAL_UNICODE}

const
{$ifdef android}
  OemCP = 1251;
{$else}
  OemCP = 866;
{$endif}

type
  TOEMStr = type AnsiString(OemCP);
{$ifndef FPC}
  TSystemCodePage = Word;
const
  CP_UTF8 = 65001;
{$endif}

procedure TestCodeConvRaw(const s: rawbytestring; const CodePage: TSystemCodePage);
begin
  WriteLn(StringCodePage(s), ' ',s);
  if CodePage <> StringCodePage(s) then
    halt(1);
end;

procedure TestCodeConvAnsi(const s: ansistring; const CodePage: TSystemCodePage);
begin
  WriteLn(StringCodePage(s), ' ',s);
  if CodePage <> StringCodePage(s) then
    halt(2);
end;

procedure TestCodeConvUTF(const s: utf8string; const CodePage: TSystemCodePage);
begin
  WriteLn(StringCodePage(s), ' ',s);
  if CodePage <> StringCodePage(s) then
    halt(3);
end;

var
  u: unicodestring;
  u8: utf8string;
  s: ansistring;
  oemstr: TOEMStr;
begin
  u := #$0141#$00F3#$0064#$017A;
  u8 := u;
  TestCodeConvRaw(u8, CP_UTF8);
  // if UTF8 codepage is set in options S will have UTF8 codepage
  s := u8;
  TestCodeConvRaw(s, CP_UTF8);
  TestCodeConvAnsi(u8, CP_UTF8);
  TestCodeConvAnsi(s, CP_UTF8);
  // converts to OemCP
  oemstr := u8;
  TestCodeConvRaw(oemstr, OemCP);
  TestCodeConvAnsi(oemstr, DefaultSystemCodePage);
  s := 'test';
  TestCodeConvRaw(s, CP_UTF8);
  // converts to System codepage
  s := oemstr;
  TestCodeConvRaw(s, DefaultSystemCodePage);
  TestCodeConvUTF(s, DefaultSystemCodePage);
  // outputs in source codepage instead of OEM
  TestCodeConvRaw('привет', CP_UTF8);
  // outputs in OEM codepage
  TestCodeConvRaw(TOEMStr('привет'), OemCP);
end.
