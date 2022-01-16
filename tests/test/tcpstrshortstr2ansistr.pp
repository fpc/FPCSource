{$mode objfpc} {$H+}
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
  ts1252 = type AnsiString(1252);

  procedure doerror(ANumber : Integer);
  begin
    WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  s : ts866;
  x : ts1252;
  ss : shortstring;
  i : Integer;
begin
  ss := #128#156#196;

  s := ss;
  if (StringCodePage(s) <> 866) then
    doerror(1);
  if (Length(s) <> Length(ss)) then
    doerror(2);
  for i := 1 to Length(s) do
    begin
      if (Byte(s[i]) <> Byte(ss[i])) then
        doerror(3)
    end;

  x := ss;
  if (StringCodePage(x) <> 1252) then
    doerror(4);
  if (Length(x) <> Length(ss)) then
    doerror(5);
  for i := 1 to Length(x) do
    begin
      if (Byte(x[i]) <> Byte(ss[i])) then
        doerror(6)
    end;

  WriteLn('Ok');
end.
