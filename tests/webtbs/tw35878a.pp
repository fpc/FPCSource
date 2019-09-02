var
  i, rand, val: int64;
begin
  rand:=-5;
  i:=1;
  while i<=10000 do
    begin
      val:=random(rand);
      if (val<-5) or (val>0) then
        begin
          writeln('error: ', val);
          halt(1);
        end;
      Inc(i);
    end;
end.
