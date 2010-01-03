var
 d1,d2,d3 : extended;
 err: longint;
begin
 d1:=105;
 d2:=1.05e2;
 if (d1<>d2) then
   halt(1);
end.
