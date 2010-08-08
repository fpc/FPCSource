program test_program;

uses
  tcase48;

var
  i : integer;

begin
  test_proc(i);
  if (i <> 1) then
    begin
      writeln('FAIL');
      halt(1);
    end
  else
    writeln('OK');
end.
