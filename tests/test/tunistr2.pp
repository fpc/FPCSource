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
{$endif}
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

var
  i : longint;
  w,w2 : unicodestring;
  a : ansistring;

begin
  setlength(w,1000);
  for i:=1 to 1000 do
    w[i]:=widechar(i);
  for i:=1 to 10 do
    begin
      a:=w;
      w2:=a;
    end;
  writeln('ok');
end.
