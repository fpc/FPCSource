{ %fail }

{$r-}
type
  chararray = array[0..200] of char;
var
  p: pchar;
  i: longint;
begin
  getmem(p,5);
  p[0]:='a';
  p[1]:='b';
  p[2]:='c';
  p[3]:='d';
  p[4]:=#0;
  i:=3;
  chararray(p^)[i]:=#0;
  writeln(p);
  freemem(p);
end.
