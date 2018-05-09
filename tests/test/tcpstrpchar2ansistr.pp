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
  x : ts866;
  i : Integer;
  c : Integer;
  p, pp : pansichar;
  sa : ansistring;
begin
  p := 'abc'#190#250;
  c := 5;
  sa := p;
  if (StringCodePage(sa) <> DefaultSystemCodePage) then
    doerror(1);
  if (Length(sa) <> c) then
    doerror(2);
  pp := p;
  for i := 1 to Length(sa) do
    begin
      if (Byte(sa[i]) <> Byte(pp^)) then
        doerror(3);
      Inc(pp);
    end;
  x := p;
  if (StringCodePage(x) <> 866) then
    doerror(10);
  if (Length(x) <> c) then
    doerror(20);
  pp := p;
  for i := 1 to Length(x) do
    begin
      if (Byte(x[i]) <> Byte(pp^)) then
        doerror(30);
      Inc(pp);
    end;

  WriteLn('Ok');
end.
