{ Source provided for Free Pascal Bug Report 3697 }
{ Submitted by "Matthias Hryniszak" on  2005-02-26 }
{ e-mail: matthias@hryniszak.de }
{$ifdef go32v2}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
{$endif}
uses
  {$ifdef unix}{$ifdef darwin}iosxwstr{$else}cwstring{$endif}, {$endif}
{$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
{$endif}
{$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
{$endif}
  SysUtils;

var
  S: WideString;

begin
  S := WideFormat('Test %s', ['string']);
  if s<>'Test string' then
    halt(1);

  writeln('ok');
end.
