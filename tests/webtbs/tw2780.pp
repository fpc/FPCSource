{ Source provided for Free Pascal Bug Report 2780 }
{ Submitted by "Johannes Berg" on  2003-11-10 }
{ e-mail: bugs@johannes.sipsolutions.de }
program ifdef;

const
  a = 1;

begin
{$IF NOT DECLARED(a)}
  writeln('a not declared');
{$ELSE}
  writeln('a declared, a = ',a);
{$ENDIF}
{$IF NOT DECLARED(b)}
  writeln('b not declared');
{$ELSE}
  writeln('b declared, b = ',b);
{$ENDIF}
end.
