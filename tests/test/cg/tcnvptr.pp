{ %opt=-vw -Sew }

var
  p : pointer;
  b : byte;
  w : word;
  a : array[0..10] of char;
begin
  b:=1;
  w:=1;
  p:=@a;
  // This should work without warnings
  inc(p,b);
  inc(p,w);
end.
