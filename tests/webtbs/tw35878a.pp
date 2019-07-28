var
  rand, val: int64;
  i: int32;
begin
  rand:=-5;
  for i:=1 to 10000 do
    begin
      val:=random(rand);
      if (val<-5) or (val>0) then
        begin
          writeln('error: ', val);
          halt(1);
        end;
    end;
end.
