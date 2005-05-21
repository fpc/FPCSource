{ %VERSION=1.1 }

var i: int64;
    il: longint;

begin
  for il:=-20 to 20 do
    begin
       i:=il;
       case i of
          -3:
            if (i<>-3) then
              halt(1);
          -7..-5:
            if (i<-7) or (i>-5) then
              halt(1);
          -9..-8:
            if (i<-9) or (i>-8) then
              halt(1);
          0:
            if (i<>0) then
              halt(1);
          1:
            if (i<>1) then
              halt(1);
          2:
            if (i<>2) then
              halt(1);
          3..6:
            if (i<3) or (i>6) then
              halt(1);
          8..10:
            if (i<8) or (i>10) then
              halt(1);
       end;
    end;
  halt(0);
end.
