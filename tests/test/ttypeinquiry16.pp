{ %FAIL }
{ Mode delphi does not have the modeswitch typeinquiry. }
program ttypeinquiry16;

{$mode delphi}

var
  b: byte;
  v: type of b;
begin
  v:=b;
end.
