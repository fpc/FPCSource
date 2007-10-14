{ %opt=-O2 }

function space (b : byte): shortstring;
begin
  space[0] := chr(b);
  FillChar (Space[1],b,' ');
end;

var
  s: string;
  i: longint;
begin
  fillchar(s,sizeof(s),255);
  s:=space(255);
  if length(s)<>255 then
    halt(1);
  for i:=1 to 255 do
    if s[i]<>' ' then
      halt(2);
end.
