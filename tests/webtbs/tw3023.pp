{ Source provided for Free Pascal Bug Report 3023 }
{ Submitted by "Michael" on  2004-03-23 }
{ e-mail: Michael.VanCanneyt@wisa.be }
{$mode objfpc}

program testa;

Var
  A : Array of Integer;

begin
  If Not Assigned(A) then
    halt(0)
  else
    halt(1);
end.
