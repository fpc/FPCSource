{ Old file: tbs0224.pp }
{ I/O-Error generation in readln can't be switched off OK 0.99.11 (PFV) }


var f:text;
    i:integer;
begin
  assign(f,'bug0224.txt');
  rewrite(f);
  write(f,'     ');
  reset(f);
{$I-}
  readln(f,i);              { you can't avoid run-time error generation }
{$I+}
  if IOResult<>0 then
   writeln('error...');
{$I-}
  close(f);
  erase(f);
{$I+}
  if IOResult<>0 then;
end.
