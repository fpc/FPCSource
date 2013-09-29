{ Test that by default string constants are handled as ShortString }

program tthlp5;

{$mode objfpc}
{$apptype console}

uses
  uthlp;

begin
  if 'Hello World'.Test <> 1 then
    Halt(1);
  Writeln('OK');
end.
