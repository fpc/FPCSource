{ Source provided for Free Pascal Bug Report 4540 }
{ Submitted by "Aleksa Todorovic" on  2005-11-23 }
{ e-mail: alexione@mindnever.org }
program TestAnsiEndsStr;

uses
  StrUtils;

begin
  Writeln('A/AB: ', AnsiEndsStr('A', 'AB'));
  if AnsiEndsStr('A', 'AB')<>false then
    halt(1);
    // False -> ok
  Writeln('AB/AB: ', AnsiEndsStr('AB', 'AB'));
  if AnsiEndsStr('AB', 'AB')<>true then
    halt(1);
    // True -> ok
  Writeln('ABC/AB: ', AnsiEndsStr('ABC', 'AB'));
  if AnsiEndsStr('ABC', 'AB')<>false then
    halt(1);
  writeln('ok');
    // True -> ???
end.
