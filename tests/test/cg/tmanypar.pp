procedure t(a,b,c,d,e,f,g,h,i,j,k,l: cardinal);
begin
  if (a <> $deadbeef) then
    halt(1);
  if (b <> $cafebabe) then
    halt(2);
  if (c <> $BeC0ffee) then
    halt(3);
  if (d <> $C001D00D) then
    halt(4);
  if (e <> $feeb1e) then
    halt(5);
  if (f <> cardinal($abba-$ceedee)) then
    halt(6);
  if (g <> $feedface) then
    halt(7);
  if (h <> $badfade5) then
    halt(8);
  if (i <> $deafb00b) then
    halt(9);
  if (j <> $badc0c0a) then
    halt(10);
  if (k <> $2badf001) then
    halt(11);
  if (l <> $defaced) then
    halt(12);
end;


begin
  t($deadbeef,$cafebabe,$BeC0ffee,$C001D00D,$feeb1e,cardinal($abba-$ceedee),
    $feedface,$badfade5,$deafb00b,$badc0c0a,$2badf001,$defaced)
end.

