{ Source provided for Free Pascal Bug Report 3023 }
{ Submitted by "Michael" on  2004-03-23 }
{ e-mail: Michael.VanCanneyt@wisa.be }
{$mode objfpc}

program testa;

Var
  A : Array of Integer;

begin
// Commented version does work.
//  If (Length(A)=0) then
  If Not Assigned(A) then
    Writeln('All OK');
end.