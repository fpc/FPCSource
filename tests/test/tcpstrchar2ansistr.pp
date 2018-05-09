{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif unix}
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
  sysutils;
  
type  
  ts866 = type AnsiString(866);

  procedure doerror(ANumber : Integer);
  begin
    WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  x : ts866;
  c : ansichar;
  sa : ansistring;
begin
  c := 'a';
  sa := c;
  if (StringCodePage(sa) <> DefaultSystemCodePage) then
    doerror(1);
  if (Length(sa) <> 1) then
    doerror(2);
  if (Byte(sa[1]) <> Byte(c)) then
    doerror(3);
  x := c;
  if (StringCodePage(x) <> 866) then
    doerror(4);
  if (Length(x) <> 1) then
    doerror(5);
  if (Byte(x[1]) <> Byte(c)) then
    doerror(6);

  c := #156;
  sa := c;
  if (StringCodePage(sa) <> DefaultSystemCodePage) then
    doerror(10);
  if (Length(sa) <> 1) then
    doerror(20);
  if (Byte(sa[1]) <> Byte(c)) then
    doerror(30);
  x := c;
  if (StringCodePage(x) <> 866) then
    doerror(40);
  if (Length(x) <> 1) then
    doerror(50);
  if (Byte(x[1]) <> Byte(c)) then
    doerror(60);

  WriteLn('Ok');
end.
