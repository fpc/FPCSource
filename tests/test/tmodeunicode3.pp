{ %OPT=-Municodestrings }
{ A mode selecting the ansistring default undefines the macros of the unicodestring default for its own module
  only: a unit compiled after it, which keeps the unicodestring default of the command line, still has them. }
program tmodeunicode3;

{$mode delphi}

uses
  umodeunicode3;

begin
{$ifdef UNICODE}
  Halt(1);
{$endif}
  if CharSize <> 2 then
    Halt(2);
  WriteLn('ok');
end.
