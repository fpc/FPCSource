{ test that the correct type helper is used for String = ShortString }

program tthlp26a;

{$mode objfpc}
{$modeswitch typehelpers}

uses
  uthlp;

begin
  if String.TestClass <> 1 then
    Halt(1);
  Writeln('ok');
end.
