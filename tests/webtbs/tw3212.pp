{ Source provided for Free Pascal Bug Report 3212 }
{ Submitted by "Marc Weustink" on  2004-07-15 }
{ e-mail: marc@freepascal.org }
program conststring;

{$mode objfpc}
{$H+}

const
  C1: String = 'bla';

procedure X;
const
  C: String = 'bla';
begin
  C:='bla';
  C:='bla'+C;
  WriteLN('"',C,'"');
  if C<>'blabla' then
    halt(1);
end;

begin
  X;
  X;
end.
