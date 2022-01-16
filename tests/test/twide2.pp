{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
uses
 {$ifndef USE_INTERNAL_UNICODE}
  {$ifdef unix}
   {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
  {$endif unix}
 {$endif ndef USE_INTERNAL_UNICODE}
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
  strings;

var
  i : longint;
  w,w2 : widestring;
  u,u2 : unicodestring;
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
  setlength(u,1000);
  for i:=1 to 1000 do
    u[i]:=widechar(i);
  for i:=1 to 10 do
    begin
      a:=u;
      u2:=a;
    end;
end.
