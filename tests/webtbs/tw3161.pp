{ %OPT=-CE }
{ %target=linux,haiku }

{ Source provided for Free Pascal Bug Report 3161 }
{ Submitted by "Michalis Kamburelis" on  2004-06-12 }
{ e-mail: michalis@camelot.homedns.org }

{$mode delphi}

uses
  initc,
  SysUtils,
  math;

var A:Extended;
err : boolean;
begin
  SetExceptionMask([]);
  err:=true;
  try
 { When I don't do "uses Libc",
   this line raises RE 205 (EOverflow). }
{$ifdef FPC_SUPPORT_EXTENDED}
 A:=1e800;
{$else FPC_SUPPORT_EXTENDED}
 A:=1e200;
{$endif FPC_SUPPORT_EXTENDED}
 Writeln(Exp(A));
 except
   writeln('Exception raised');
   err:=false;
 end;
 if err then
   begin
     writeln('error');
     halt(1);
   end;
end.
