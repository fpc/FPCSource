{ Source provided for Free Pascal Bug Report 3309 }
{ Submitted by "Tom Verhoeff" on  2004-09-10 }
{ e-mail: T.Verhoeff@tue.nl }
program ReadSubrange;
  { Demonstrates bug in 1.9.5 when reading into a subrange of Integer }

const
  MaxValue = 65536; { exceeds 16 bit range }

type
  Subrange = 0 .. MaxValue;

var
  i: Subrange;
  c: cardinal;

begin
  write ( 'Type an integer in the range 0 .. ', MaxValue, ': ' );
  // Only compile, don't run
  i:=1;
  if i=2 then
    begin
      readln ( c );
      readln ( i );
    end;
 writeln ( 'i = ', i );
end.
