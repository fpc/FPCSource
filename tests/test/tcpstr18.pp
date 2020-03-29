// to have correct test result with delphi set codepage option to 866
program tcpstr18;
{$apptype console}
{$ifdef fpc}
  {$mode delphi}
  {$codepage cp866}
{$endif}

{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
{$ifndef USE_INTERNAL_UNICODE}
{$ifdef unix}
uses
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif};
{$endif unix}
{$else USE_INTERNAL_UNICODE}
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

procedure TestRawByte(const Source: RawByteString; cp: word; const reason: integer);
begin
  Writeln(StringCodePage(Source), ' ', Source);
  if StringCodePage(Source) <> cp then
    halt(reason);
end;

const
  test: array[0..4] of ansichar = 'test'#0;
var
  s: rawbytestring;
  ss: shortstring;
  c: ansichar;
  w: widechar;
begin
  s := 'test';
  ss := 'test';
  TestRawByte(s, 866, 1);
  TestRawByte(ss, DefaultSystemCodePage, 2);
  TestRawByte(AnsiChar('t'), 866, 3);
  c := 't';
  TestRawByte(c, DefaultSystemCodePage, 4);
  TestRawByte(WideChar('t'), 866, 5);
  w := 't';
  TestRawByte(w, DefaultSystemCodePage, 6);
  TestRawByte(test, DefaultSystemCodePage, 7);
  TestRawByte(PAnsiChar(@test[0]), DefaultSystemCodePage, 8);
end.
