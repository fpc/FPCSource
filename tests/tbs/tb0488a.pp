{ Source provided for Free Pascal Bug Report 3478 }
{ Submitted by "Michalis Kamburelis" on  2004-12-26 }
{ e-mail: michalis@camelot.homedns.org }
{ Before fixing bug 3477 this prints
    FFFFFFF
    FFFFFFFFFFFFFFFF
    0000000FFFFFFFFF
    9999999
    FFFFFFFF99999999
    0000000999999999

  After fixing 3477 with my patch this prints
    FFFFFFF
    FFFFFFFFFFFFFFFF
    FFFFFFFFF
    9999999
    FFFFFFFF99999999
    999999999
  so part of the problems are gone, but not all.

  Then, after fixing this bug with my simple patch it correctly prints
    FFFFFFF
    FFFFFFFF
    FFFFFFFFF
    9999999
    99999999
    999999999
}

{$R-}
{$Q-}

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
 check(WideFormat('%x', [$FFFFFFFF]),'FFFFFFFF');
 check(WideFormat('%x', [$FFFFFFFFF]),'FFFFFFFFF');

 check(WideFormat('%x', [$9999999]),'9999999');
 check(WideFormat('%x', [$99999999]),'99999999');
 check(WideFormat('%x', [$999999999]),'999999999');
end.
