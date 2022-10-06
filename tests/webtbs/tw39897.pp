var
  a,b : array of longint;

begin
  b:=nil;
  SetLength(a,5);
  a[1]:=1234;
  DynArrayAssign(pointer(b),pointer(a),TypeInfo(a));
  if (length(b)<>5) or (b[1]<>1234) then
    halt(1);
end.
