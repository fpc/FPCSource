var
  p: pointer;
  u: ptruint;
  i: cardinal;
begin
  p:=pointer(1);
  for i:=0 to 15 do
    if align(p+i,16)<>pointer(16) then
      halt(1);
  p:=pointer(41);
  for i:=0 to 39 do
    if align(p+i,40)<>pointer(80) then
      halt(2);
  p:=pointer(1);
  for i:=0 to 40 do
    if align(p+i,41)<pointer(41) then
      halt(3);
  p:=pointer(42);
  for i:=0 to 40 do
    if align(p+i,41)<>pointer(82) then
      halt(4);

  u:=1;
  for i:=0 to 15 do
    if align(u+i,16)<>16 then
      halt(101);
  u:=41;
  for i:=0 to 39 do
    if align(u+i,40)<>80 then
      halt(102);
  u:=1;
  for i:=0 to 40 do
    if align(u+i,41)<>41 then
      halt(103);
  u:=42;
  for i:=0 to 40 do
    if align(u+i,41)<>82 then
      halt(4);
end.
