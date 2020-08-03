{ utility to avoid the tedious typing of register numbers for AArch64 VFP registers }
var
  i : longint;
  
begin
  for i:=0 to 31 do
    begin
      writeln('B',i,',$04,$01,$',hexstr(i,2),',b',i,',',i+64,',',i+64);
      writeln('H',i,',$04,$03,$',hexstr(i,2),',h',i,',',i+64,',',i+64);
      writeln('S',i,',$04,$09,$',hexstr(i,2),',s',i,',',i+64,',',i+64);
      writeln('D',i,',$04,$0a,$',hexstr(i,2),',d',i,',',i+64,',',i+64);
      writeln('Q',i,',$04,$05,$',hexstr(i,2),',q',i,',',i+64,',',i+64);
      writeln('V',i,'8B,$04,$17,$',hexstr(i,2),',v',i,'.8b,',i+64,',',i+64);
      writeln('V',i,'16B,$04,$18,$',hexstr(i,2),',v',i,'.16b,',i+64,',',i+64);
    end;
end.
