{ Checks whether the %x format string supports qwords.
 This is a variation of tb0488a
}

{$ifdef go32v2}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
{$endif}
uses
{$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
{$endif}
{$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
{$endif}
{$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
{$endif}
SysUtils,erroru;

procedure Check(a,b:ansistring);
begin
  if a<>b then
    begin
      writeln(a,' should be equal to ',b);
      error;
    end;
end;

begin
 check(WideFormat('%x', [$FFFFFFF]),'FFFFFFF');
end.
