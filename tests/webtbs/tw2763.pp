{ %version=1.1 }
{$mode objfpc}
{ Source provided for Free Pascal Bug Report 2763 }
{ Submitted by "Michael Van Canneyt" on  2003-11-04 }
{ e-mail: Michael.VanCanneyt@wisa.be }
program testv2;

uses Variants;

Type
  TMyEnum = (One,Two,Three);

Var
  V : Variant;
  E : TMyEnum;

begin
  E:=Two;
  V:=E;
  if v>1 then
    begin
      writeln('error 1');
      halt(1);
    end;
  E:=V;
  if ord(e)<>1 then
    begin
      writeln('error 2');
      halt(1);
    end;
end.
