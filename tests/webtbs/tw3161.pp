{ %target=linux }
{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 3161 }
{ Submitted by "Michalis Kamburelis" on  2004-06-12 }
{ e-mail: michalis@camelot.homedns.org }

{$mode delphi}

uses 
  Libc,
  SysUtils;

var A:Extended;
err : boolean;
begin
  err:=true;
  try
 { When I don't do "uses Libc",
   this line raises RE 205 (EOverflow). }
 A:=1e800; Writeln(Exp(A));
 except
   writeln('Exception raised');
   err:=false;
 end;
 if err then
    halt(1);
end.

