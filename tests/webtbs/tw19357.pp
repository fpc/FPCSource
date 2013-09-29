program tw19357;
type
  TLvl0 = bitpacked record
    a,b: longword;
  end;

  TTest = packed record
    a,b: longword;
    c: TLvl0;
  end;

var
  h: TTest absolute 100;
const
  x: pointer = @h.c.b;
begin
  if ptruint(@h.a) <> 100 then
    halt(1);
  if ptruint(@h.b) <> 104 then
    halt(2);
  if ptruint(@h.c.b) <> 112 then
    halt(3);
  if ptruint(x) <> 112 then
    halt(4);
end.