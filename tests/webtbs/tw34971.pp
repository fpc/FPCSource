type
  t1 = -1..1;
  t2 = -4..3;
  t3 = -3..4;
type
  r1 = bitpacked record
    f: t1;
  end;

  r2 = bitpacked record
    f: t2;
  end;

  r3 = bitpacked record
   f: t3;
  end;

begin
  if bitsizeof(r1.f)<>2 then
    halt(1);
  if bitsizeof(r2.f)<>3 then
    halt(2);
  if bitsizeof(r3.f)<>4 then
    halt(3);
end.
