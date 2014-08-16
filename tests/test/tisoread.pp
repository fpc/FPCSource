{$mode iso}
program tisoread;
{
  Test Read in ISO mode when reading real and integer numbers.
}
var
  f: text;
  i,j,k: integer;
  r,s,t: real;
begin
  assign(f,'tisoread.tmp');
  rewrite(f);
  writeln(f,'   ');
  writeln(f);
  writeln(f,'1234567890+1234567890-1234567890');
  writeln(f,'0x12345678$ABCDEF0x12345678');
  writeln(f,'0X12345678X12345678');
  writeln(f,'%10101010&12345670');
  writeln(f,'   ');
  writeln(f);
  writeln(f,'+123.-.123.123');
  writeln(f,'1e2+1e-2');
  close(f);
  reset(f);
  read(f,i,j,k);
  if not ((i = 1234567890) and (i=j) and (i=-k)) then halt(1);
  read(f,i,j,k);
  if not ((i = $12345678) and (j = $abcdef0) and (k = $12345678)) then halt(2);
  read(f,i,j);
  if not ((i = $12345678) and (j = $12345678)) then halt(3);
  read(f,i,j);
  if not((i = 170) and (j = 2739128)) then halt(4);
  read(f,r,s,t);
  if not((r=123) and (round(s*1000)=-123) and (round(t*1000)=123)) then halt(5);
  read(f,r,s);
  if not((r = 1e2) and (trunc(s*100) = 1)) then halt(6);
  close(f);
  erase(f);
  writeln('ok');
end.
