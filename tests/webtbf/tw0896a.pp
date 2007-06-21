{ %FAIL }

var
  dat : file of byte;
  j : longint;
  Buffer : Array[0..2047] of byte;

begin
  for j:=0 to 2047 do
    Buffer[j]:=j and $ff;
  Assign(dat,'tbug896.tmp');
  Rewrite(dat,1);
  for j:= 0 to 2047 do
  { writeln should not be allowed for typed files }
    writeln (dat,Buffer[j]);
  Close(dat);
  Erase(dat);
end.
