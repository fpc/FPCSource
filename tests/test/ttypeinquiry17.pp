{ %FAIL }
{ Mode delphiunicode does not have the modeswitch typeinquiry. }
program ttypeinquiry17;

{$mode delphiunicode}

var
  b: byte;
  v: type of b;
begin
  v:=b;
end.
