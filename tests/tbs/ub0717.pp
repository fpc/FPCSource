unit ub0717;

interface

var
  TestOk: Boolean = False;

implementation

finalization
  if not TestOk and (ExitCode = 0) then
    ExitCode := 3;
end.
