const
  bases : array[0..3] of shortint = (2,8,10,16);
  basepref : array[0..3] of shortstring = ('%','&','','$');
  maxlen : array[0..3] of byte = (31,10,9,7);
  chars : shortstring = ('0123456789AbCdEf');
  signs : shortstring = (' -');
var
  vals : array[0..1000] of string;
  base,len,baseindex : byte;
  li,i,j : longint;
  code : word;
begin
  for i:=low(vals) to high(vals) do
    begin
      baseindex:=random(4);
      base:=bases[baseindex];
      len:=random(maxlen[baseindex])+1;
      vals[i]:=signs[random(2)+1]+basepref[baseindex];
      for j:=1 to len do
        vals[i]:=vals[i]+chars[random(base)+1];
    end; 
  for i:=1 to 100000 do
    for j:=low(vals) to high(vals) do
      begin
        val(vals[j],li,code);
        if code<>0 then
          writeln(vals[j]);
      end;
end.