{ Test that "chaining" works correctly }

program tthlp8;

{$mode objfpc}{$H+}
{$apptype console}

uses
  uthlp;

begin
  if 'Hello World'.Test.Test <> - 4 then
    Halt(1);
  if 'Hello World'[5].Test <> - 1 then
    Halt(2);
  if 'Hello World'[5].Test.Test <> - 4 then
    Halt(3);
  Writeln('OK');
end.
