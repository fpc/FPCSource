{ Source provided for Free Pascal Bug Report 2187 }
{ Submitted by "Artur Kornilowicz" on  2002-10-18 }
{ e-mail: arturk@math.uwb.edu.pl }
{$mode delphi}
type x = record
            s : string;
            a : ^byte;
         end;

var a: array [1..2] of x;

begin
end.
