{ %FAIL }
{$R+}
{ should not compile if range check on }
program BigRange;

const
  Limit   = 100000000; { Hundred millions }
  One     =         1;

var
  Huge: longint;

begin
    Huge := Limit + One;

    writeln(One, ' is the lower bound');
    writeln(Limit, ' is the upper bound');

    if Limit in [One .. Limit] then
      writeln(Limit, ' is within the range')
    else
      writeln(Limit, ' is out of the range');

    if Huge in [One .. Limit] then
      writeln(Huge, ' is within the range')
    else
      writeln(Huge, ' is out of the range')
end.
