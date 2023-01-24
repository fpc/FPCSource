{ %opt=-Seh }
{ %norun }
program test;

const
  TheVersion = 1;

begin
  {$if TheVersion >= 1}
  writeln('Version 1 or higher');
  {$else}
  writeln('Version < 1');
  {$endif}
end.
