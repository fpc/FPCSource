{ %fail }

{ Source provided for Free Pascal Bug Report 2657 }
{ Submitted by "Mazen NEIFER" on  2003-08-28 }
{ e-mail: mazen.neifer.01@supaero.org }
program test;
type
  LongInt=record
    x,y:cardinal;
  end;
var
  t:longint;
begin
  t:=1;
end.
