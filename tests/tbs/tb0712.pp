var
  yy,res : longint;
begin
  res:=0;
  yy:=random(0)+1980;
  If (yy<1980) or (yy>2099) then
    res:=1;
  if res<>0 then
    halt(res);

  yy:=random(0)+2099;
  If (yy<1980) or (yy>2099) then
    res:=2;
  if res<>0 then
    halt(res);

  yy:=random(0)+1980;
  If (yy<=1979) or (yy>2099) then
    res:=3;
  if res<>0 then
    halt(res);

  yy:=random(0)+2099;
  If (yy<1979) or (yy>=2100) then
    res:=4;
  if res<>0 then
    halt(res);
end.
