{ Source provided for Free Pascal Bug Report 3683 }
{ Submitted by "Christian Iversen" on  2005-02-22 }
{ e-mail: chrivers@iversen-net.dk }
program test;

type
  foo = (a, b);
const
{$ifdef use2byte}
  T=a;
{$else}
  T=b;
{$endif}
{$if T = a}
  {$message 'T = a'}
{$else}
  {$message 'T = b'}
{$ifend}

begin
end.
