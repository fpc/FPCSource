
type
  TDragHandleStyle = (dhDouble, dhNone, dhSingle);


var b : boolean;
    fdraghandlestyle : TDraghandlestyle;
    i : longint;
begin
  i:=0;
  FDraghandlestyle:=dhDouble;
  for B := False to (FDragHandleStyle = dhDouble) do 
   inc(i);
  if (i<>2) then
    halt(1);
end.

