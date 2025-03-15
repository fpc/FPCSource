var
  s1,s2,dl,dh: qword;
begin
  s1:=1;
  s2:=1;
  dl:=UMul64x64_128(s1,s2,dh);
  if (dl<>1) or (dh<>0) then
    halt(1);

  s1:=$4000000000000001;
  s2:=$4000000000000001;
  dl:=UMul64x64_128(s1,s2,dh);
  if (dl<>qword($8000000000000001)) or (dh<>$1000000000000000) then
    halt(2);

  s1:=qword($ffffffffffffffff);
  s2:=qword($ffffffffffffffff);
  dl:=UMul64x64_128(s1,s2,dh);
  if (dl<>$0000000000000001) or (dh<>qword($FFFFFFFFFFFFFFFE)) then
    halt(3);
end.
