{$r+}

const
  q: qword = 18446744073709551615;

var
  i: int64;
  code: longint;
begin
  val('18446744073709551615',i,code);
  if (code = 0) then
    halt(1);
  val('-9223372036854775808',i,code);
  if (code <> 0) or
     (i <> low(int64)) then
    halt(2);
  val('9223372036854775807',i,code);
  if (code <> 0) or
     (i <> high(int64)) then
    halt(3);
  val('$8000000000000000',i,code);
  if (code <> 0) or
     (i <> low(int64)) then
    halt(4);
end.
