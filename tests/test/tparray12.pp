{$packenum 2}
type
  tenum = (ea,eb,ec,ed,ee,ef:=255);
  tb = array[1..16] of byte;
const
  res: array[1..6] of byte = (0,1,2,3,4,255);
var
  a: bitpacked array[1..16] of tenum;
  i: longint;
begin
  writeln(sizeof(a));
  a[1]:=ea;
  a[2]:=eb;
  a[3]:=ec;
  a[6]:=ef;
  a[5]:=ee;
  a[4]:=ed;
  for i := 1 to 6 do
    begin writeln(tb(a)[i]);
    if (tb(a)[i] <> res[i]) then
      halt(1); end;
end.

