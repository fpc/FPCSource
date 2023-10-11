uses
  Sysutils,DateUtils;
var
  d1,d2 : TDateTime;
begin
  d1:=EncodeDateDay(2023,1);
  d2:=EncodeDate(2023,1,1);
  d1:=d1+0.6;
  d2:=d2+0.3;
  if DateOf(d1)<>DateOf(d2) then
    halt(1);
end.
