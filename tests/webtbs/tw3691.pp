{ Source provided for Free Pascal Bug Report 3691 }
{ Submitted by "Christian Iversen" on  2005-02-25 }
{ e-mail: chrivers@iversen-net.dk }
program test;

type
  x = (a, b, c);

const
  q = b;

  {$if q in [a,c]  }{$message warn 'true'}{$else}{$message warn 'false'}{$ifend}
  {$if q in [b]    }{$message warn 'true'}{$else}{$message warn 'false'}{$ifend}
  {$if q in []     }{$message warn 'true'}{$else}{$message warn 'false'}{$ifend}
  {$if q in [a,b,c]}{$message warn 'true'}{$else}{$message warn 'false'}{$ifend}

begin
end.
