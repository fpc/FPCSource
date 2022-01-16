{$mode objfpc}
{$assertions on}

procedure DivMod(a: Int64; b: Int64; out res, rem: Int64); inline;
begin
  res:=a div b; rem:=a-b*res;
end;

procedure Test;
var
  res, rem: Int64;
begin
  res:=5; DivMod(res {!}, 2, res {!}, rem);
  Assert(res=2); // OK
  Assert(rem=1); // "2-2*2 = -2" if inlined
end;

begin
  test;
end.

