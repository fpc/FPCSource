program tcpstr11;
type
  cp866 = type AnsiString(866);
var
  A: cp866;
  c: array[0..5] of ansichar = 'привет';
begin
  A := c;
  if StringCodePage(A) <> 866 then
    halt(1);
end.
