{ %fail }
{ %opt=-Sew }

{ Source provided for Free Pascal Bug Report 4139 }
{ Submitted by "Christian Iversen" on  2005-06-29 }
{ e-mail: chrivers@iversen-net.dk }

{$mode delphi}

type
  Foobar = (a, b, c, d);

const
  Test = c;

const
  List = [a, c];

{$IF Test in List}
  {$MESSAGE WARN 'This should work'}
{$IFEND}

begin
end.
