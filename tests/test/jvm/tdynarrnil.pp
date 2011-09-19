program tdynarrnil;

var
  arr: array of longint;
begin
  setlength(arr,0);
  if assigned(arr) then
    halt(1);
  if arr<>nil then
    halt(2);
  setlength(arr,1);
  if not assigned(arr) then
    halt(3);
  if arr=nil then
    halt(4);
end.
