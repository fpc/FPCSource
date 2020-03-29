{ test that the correct type helper is used for String = UnicodeString }

program tthlp26c;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$modeswitch typehelpers}

uses
  uthlp;

begin
  if String.TestClass <> 4 then
    Halt(1);
  Writeln('ok');
end.
