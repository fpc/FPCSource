{$mode objfpc}


procedure dirtystack;
var
  s: shortstring;
begin
  fillchar(s,sizeof(s),255);
end;

procedure test(const arr: array of const);
begin
  if arr[0].vinteger<>ord('$') then
    halt(1);
end;

procedure doit;
begin
  test(['$']);
end;

begin
  dirtystack;
  doit;
end.
