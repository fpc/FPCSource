{ %CPU=i386 }
{$mmx+}
uses
  mmx;
var
  wa,wb : tmmxword;
  ca,cb : tmmxcardinal;
  i : longint;

begin
  for i:=low(wa) to high(wa) do
    wa[i]:=2;
  wb:=default(tmmxword);
  wb[0]:=2;
  wa:=wa shl 3;
  for i:=low(wa) to high(wa) do
    if wa[i]<>16 then
      halt(1);
  wa:=wa shl wb;
  for i:=low(wa) to high(wa) do
    if wa[i]<>64 then
      halt(1);
  wa:=(wa shr 3) shr wb;
  for i:=low(wa) to high(wa) do
    if wa[i]<>2 then
      halt(1);

  for i:=low(ca) to high(ca) do
    ca[i]:=2;
  cb:=default(tmmxcardinal);
  cb[0]:=2;
  ca:=ca shl 3;
  for i:=low(ca) to high(ca) do
    if ca[i]<>16 then
      halt(1);
  ca:=ca shl cb;
  for i:=low(wa) to high(ca) do
    if ca[i]<>64 then
      halt(1);
  ca:=(ca shr 3) shr cb;
  for i:=low(ca) to high(ca) do
    if ca[i]<>2 then
      halt(1);

  writeln('ok');
end.
