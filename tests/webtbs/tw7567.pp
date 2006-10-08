var
  a: array[0..9] of char;
  b: array[1..10] of char;
  t: text;
begin
  assign(t,'tw7567.txt');
  rewrite(t);
  writeln(t,'0123456789abcdef');
  writeln(t,'0123456789abcdef');
  close(t);
  reset(t);
  readln(t,a);
  readln(t,b);
  close(t);
  erase(t);
  if (a <> '012345678') then
    halt(1);
  if (b <> '0123456789') then
    halt(2);
end.
