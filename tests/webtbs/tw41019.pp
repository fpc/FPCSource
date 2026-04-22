{$packenum 1}
type
  tA = (an1 = -1, a255 = 255);
  tB = (bn1 = -1, b128 = 128);
  tC = (cn127 = -127, c128 = 128);
  tD = (dn1= -1, d65535 = 65535);
var
  a: tA; b: tB; c: tC; d: tD;
  size, failures: integer;
begin
  failures := 0;
  size := sizeOf(tA);
  if size <> sizeof(smallint) then
    inc( failures); // size = 1
  a := a255;
  if ord(a) <> 255 then
    inc( failures); // (a = -1)

  size := sizeOf(tB);
  if size <> sizeof(smallint) then
    inc( failures); // size = 1
  b := b128;
  if ord(b) <> 128 then
    inc( failures); // (b = -128)

  size := sizeOf(tC);
  if size <> sizeof(smallint) then
    inc( failures); // size = 1
  c := c128;
  if ord(c) <> 128 then
    inc( failures); // (c = -128)

  size := sizeOf(tD);
  if size <> sizeof(longint) then
    inc( failures); // size = 2
  d := d65535;
  if ord(d) <> 65535 then
    inc( failures); // (d = -1)

  assert( failures = 0); // (failures = 8)
  if failures>0 then
    halt(1);
  writeln('ok');
end.