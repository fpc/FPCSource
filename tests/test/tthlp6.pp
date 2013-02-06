{ Test that with $H+ string constants are handled as AnsiString }

program tthlp6;

{$mode objfpc}{$H+}
{$apptype console}

uses
  uthlp;

begin
  if 'Hello World'.Test <> 2 then
    Halt(1);
  Writeln('OK');
end.
