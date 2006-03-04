var
  gl: longint;
  gc: cardinal;

procedure testsigned;
var
  l1, l2: longint;
  b1: byte;
  i: int64;
begin

  l1 := longint($80000000);
  gl := longint($80000000);
  l2 := $11;
  b1 := $11;

  i := int64(l1)*l2;
  if (i <> int64($fffffff780000000)) then
    halt(1);

  i := int64(l1)*$11;
  if (i <> int64($fffffff780000000)) then
    halt(2);

  i := int64(gl)*$11;
  if (i <> int64($fffffff780000000)) then
    halt(3);

  i := int64(gl)*b1;
  if (i <> int64($fffffff780000000)) then
    halt(4);
end;


procedure testunsigned;
var
  l1, l2: cardinal;
  b1: byte;
  i: qword;
begin

  l1 := $80000000;
  l2 := $11;
  gc := $80000000;
  b1 := $11;

  i := qword(l1)*l2;
  if (i <> $880000000) then
    halt(5);

  i := qword(l1)*$11;
  if (i <> $880000000) then
    halt(6);

  i := qword(gc)*$11;
  if (i <> $880000000) then
    halt(7);

  i := qword(gc)*b1;
  if (i <> $880000000) then
    halt(8);
end;


begin
  testsigned;
  testunsigned;
end.
