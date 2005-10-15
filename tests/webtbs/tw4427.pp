{$inline on}

type
  pbyte = ^byte;

procedure test(p: pchar);
begin
  if pbyte(p)^ <> 0 then
    halt(1);
end;

procedure test(const p); inline;
begin
  test(pchar(@p));
end;

begin
  test(#0);
end.
