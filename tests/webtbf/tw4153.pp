{ %fail }

{ Source provided for Free Pascal Bug Report 4153 }
{ Submitted by "Ivo Steinmann" on  2005-07-03 }
{ e-mail: isteinmann@bluewin.ch }
type
  Enum1 = (a, b, c);
  Enum2 = (x, y, z);

const
  // The next line should fail
  Foobar = [b, z];

begin
  if a in Foobar then
    WriteLn('a in Foobar'); // not printed

  if b in Foobar then
    WriteLn('b in Foobar'); // printed

  if c in Foobar then
    WriteLn('c in Foobar'); // printed
end.
