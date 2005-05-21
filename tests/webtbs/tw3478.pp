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

uses SysUtils,erroru;

procedure Check(a,b:ansistring);
begin
  writeln(a);
  if a<>b then
    error;
end;

begin
 check(Format('%x', [$FFFFFFF]),'FFFFFFF');
 check(Format('%x', [$FFFFFFFF]),'FFFFFFFF');
 check(Format('%x', [$FFFFFFFFF]),'FFFFFFFFF');

 check(Format('%x', [$9999999]),'9999999');
 check(Format('%x', [$99999999]),'99999999');
 check(Format('%x', [$999999999]),'999999999');
end.
