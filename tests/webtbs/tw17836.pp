  var
    w,w1: int64;
  begin
    w1:=-1;
    w:=w1 div (int64(1) shl 33);
    system.writeln(w);
    if w<>0 then
      halt(1);
    w:=w1 div (int64(1) shl 32);
    system.writeln(w);
    if w<>0 then
      halt(2);
    w:=w1 div (int64(1) shl 31);
    system.writeln(w);
    if w<>0 then
      halt(3);
    w:=w1 div (int64(1) shl 5);
    system.writeln(w);
    if w<>0 then
      halt(4);
    end.
