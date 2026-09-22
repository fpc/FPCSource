{ %FAIL }
{ Mode fpc does not have the modeswitch typeinquiry. }
program ttypeinquiry12;

{$mode fpc}

var
  b: byte;
  v: type of b;
begin
  v:=b;
end.
