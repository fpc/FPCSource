{ %OPT=-Or}

procedure p(i1,i2,i3,i4,i5,i6,i7,i8 : longint;i : int64);
  begin
    if (i+i <> 22222222222) then
      halt(1);
  end;

begin
  p(1,2,3,4,5,6,7,8,11111111111)
end.
