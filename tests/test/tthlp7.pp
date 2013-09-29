{ Test that with $H+ and unicodestrings string constants are handled as
  UnicodeString }

program tthlp7;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$apptype console}

uses
  uthlp;

begin
  if 'Hello World'.Test <> 4 then
    Halt(1);
  Writeln('OK');
end.
