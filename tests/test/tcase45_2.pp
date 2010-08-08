program test_program;

uses
  tcase45;

var
  i: integer;

begin
  test_proc(i);
  if (i <> 2) then
    begin
      writeln('FAIL');
      halt(1);
    end
  else
    writeln('OK');
end.
