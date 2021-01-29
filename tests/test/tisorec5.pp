{$mode iso}
type
  tr = record
    l : longint;
    case i : integer of
      1 : (s : array[0..255] of char);
      2 : (n : integer);
      3 : (w : word; case j : integer of
        1 : (t : array[0..255] of char);
        2 : (a : integer);
        );
  end;
  pr = ^tr;

var
  r : pr;
begin
  new(r,3,2);
  if r^.i<>3 then
    halt(1);
  if r^.j<>2 then
    halt(1);
  dispose(r,3,2);
  writeln('ok');
end.
