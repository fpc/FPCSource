{ test that the correct type helper is used for String = AnsiString }

program tthlp26b;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

uses
  uthlp;

begin
  if String.TestClass <> 2 then
    Halt(1);
  Writeln('ok');
end.
