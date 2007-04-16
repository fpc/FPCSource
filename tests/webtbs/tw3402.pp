{ %norun }
{ %skiptarget=go32v2 }
{ %opt=-Cg }

{ Source provided for Free Pascal Bug Report 3402 }
{ Submitted by "Layton Davis" on  2004-11-26 }
{ e-mail: layton@layton.tk }
library test;

var
  tic : integer;

procedure SetTic(num:integer);
begin
  tic := num;
end;

begin
end.
