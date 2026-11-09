{ %OPT=-Municodestrings }
{ A mode selecting the ansistring default undefines the macros of the unicodestring default, also where the
  command line or a configuration file had turned that default on. }
program tmodeunicode1;

{$mode delphi}

var
  s: string;
begin
{$ifdef UNICODE}
  Halt(1);
{$endif}
{$ifdef FPC_UNICODESTRINGS}
  Halt(2);
{$endif}
  if SizeOf(s[1]) <> 1 then
    Halt(3);
  WriteLn('ok');
end.
