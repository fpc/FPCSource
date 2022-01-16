{$OVERFLOWCHECKS+}
{$mode objfpc}
program project1;

var
  c: Cardinal;
  i: Integer;

begin
  i := 1;
  for c := 0 to i do
    WriteLn(i);
end.
