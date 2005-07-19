{ Source provided for Free Pascal Bug Report 4151 }
{ Submitted by "Christian Iversen" on  2005-07-03 }
{ e-mail: chrivers@iversen-net.dk }

{$mode delphi}

program foo;
type
  t = (a, b, c);
const bar = [];

{$IF a in bar}{$MESSAGE ERROR '1'}{$IFEND}
{$IF a in []}{$MESSAGE ERROR '2'}{$IFEND}

begin
end.
