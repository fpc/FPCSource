{ Source provided for Free Pascal Bug Report 3477 }
{ Submitted by "Michalis Kamburelis" on  2004-12-26 }
{ e-mail: michalis@camelot.homedns.org }
{ Prints
    666666
    6666666
    0000000066666666
    0000000666666666
    0000006666666666
  Should print (and does after applying my simple patch)
    666666
    6666666
    66666666
    666666666
    6666666666
}

uses SysUtils,erroru;

procedure Check(a,b:ansistring);
begin
  writeln(a);
  if a<>b then
    error;
end;

begin
 Check(Format('%x', [$666666]),'666666');
 Check(Format('%x', [$6666666]),'6666666');
 Check(Format('%x', [$66666666]),'66666666');
 Check(Format('%x', [$666666666]),'666666666');
 Check(Format('%x', [$6666666666]),'6666666666');
end.
