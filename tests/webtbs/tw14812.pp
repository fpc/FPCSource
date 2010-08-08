type
  stdstrlong = string;

procedure PackStr // Convert string to packed array
   ( InStr: StdStrLong;
    var OutArr: packed array of char);
var
  i: longint;
begin
  if (low(outarr)<>0) or
     (high(outarr)<>5) then
    halt(1);
  if (instr<>'abc') then
    halt(2);
  for i:=1 to length(instr) do
    outarr[i-1]:=instr[i];
end;

var
  a: packed array[5..10] of char;
begin
  packstr('abc',a);
  if (a[5]<>'a') or
     (a[6]<>'b') or
     (a[7]<>'c') then
    halt(1);
end.
