{%FAIL}
{ anonymous functions in a statement expression need a procedure variable or function reference }
{$Mode Delphi}

var
  b: Boolean;
  p: Pointer;
begin
  p:=if b then procedure begin end else procedure begin end;
  Halt(1);
end.
