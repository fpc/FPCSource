
var f:text;
    i:integer;
begin
  assign(f,'bug0224.txt');
  reset(f);
{$I-}
  readln(f,i);              { you can't avoid run-time error generation }
{$I+}
  if IOResult<>0 then writeln('error...')
                 else close(f);
end.
