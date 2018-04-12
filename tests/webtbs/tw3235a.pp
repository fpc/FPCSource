program TestStrIComp;

{$ifdef go32v2}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
{$endif}

  uses
{$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
{$endif}
{$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
{$endif}
    {$ifdef unix}{$ifdef darwin}iosxwstr{$else}cwstring{$endif},{$endif}
    SysUtils;

var l: longint;
begin
  l := StrIComp(pwidechar('abcdefghijklmnopqrstuvwxyz'), pwidechar('ABCDEFGHIJKLMNOPQRSTUVWXYZ'));
  if (l <> 0) then
    begin
      writeln('error: expected 0, got ',l);
      halt(1);
    end;
  l := StrIComp(pwidechar('ABCDEFGHIJKLMNOPQRSTUVWXYZ'),pwidechar('abcdefghijklmnopqrstuvwxyz'));
  if (l <> 0) then
    begin
      writeln('error: expected 0, got ',l);
      halt(1);
    end;
end.
