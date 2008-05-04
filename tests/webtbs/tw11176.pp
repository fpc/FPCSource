type
  Tx = (one,two,three);

procedure error(number : longint);
begin
  writeln('error ', number);
  halt(number);
end;

var
  a : Tx;
  err : word;

begin
  val('one', a, err);
  if (err <> 0) or (a <> one) then error(1);
end.