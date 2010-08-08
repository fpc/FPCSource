program tget2;

var
  a, b: LongWord;
  
begin
  a := 307;
  b := 1 + ($FFFFFFFF mod (a - 2));
  writeln(b);
  if b <> 301 then Halt(1);
  writeln(1 + $FFFFFFFF mod (a - 2));
  if b <> 301 then Halt(1);
end.  
