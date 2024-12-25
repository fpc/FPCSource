{ utility to avoid the tedious typing of register numbers for RiscV Vector registers }
var
  i : longint;

begin
  for i:=0 to 31 do
    begin
      writeln('V',i,',$04,$00,$',hexstr(i,2),',v',i,',',i+96,',',i+96);
    end;
end.
