{ %INTERACTIVE }

program test_crt;

uses
  crt;

  var
    ch : char;

begin
  Write('Press Ctrl-C to test :');
  Read(ch);
  if ch=#3 then
    Writeln('It works correctly');
end.
