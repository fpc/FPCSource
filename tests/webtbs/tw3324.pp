{ Source provided for Free Pascal Bug Report 3324 }
{ Submitted by "Tom Verhoeff" on  2004-09-15 }
{ e-mail: T.Verhoeff@tue.nl }
program ReadCharSubrange;
  { Demonstrates bug in 1.9.5 when reading into a subrange of Char }

const
  MinChar = 'a';
  MaxChar = 'z';

type
  Subrange = MinChar .. MaxChar;

var
  c: Subrange;

begin
  write ( 'Type a character in the range ', MinChar, ' .. ', MaxChar, ': ' );
  c:='b';
  if c='a' then
    begin
      readln ( c );
      writeln ( 'c = ', c );
    end;
end.
