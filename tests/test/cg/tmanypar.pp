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


procedure t2;
var
  a,b,c,d,e,f,g: longint;
begin
  a := 1;
  b := 2;
  c := 3;
  d := 4;
  e := 5;
  f := 6;
  g := 7;
  t($deadbeef,$cafebabe,$BeC0ffee,$C001D00D,$feeb1e,cardinal($abba-$ceedee),
    $feedface,$badfade5,$deafb00b,$badc0c0a,$2badf001,$defaced);
  if a<>1 then
    halt(13);
  if b<>2 then
    halt(14);
  if c<>3 then
    halt(15);
  if d<>4 then
    halt(16);
  if e<>5 then
    halt(17);
  if f<>6 then
    halt(18);
  if g<>7 then
    halt(19);
end;

begin
  t2;
end.
