type TA = Array[1..2,1..2,1..2,1..2,1..2,1..2,1..2,1..2,1..2,1..2] of Byte;

var v,w: ta;
    e: longint;

Begin
  e :=1;
  v[e,e,e,e,e,e,e,e,e,e] :=1;
  w[e,e,e,e,e,e,e,e,e,v[e,e,e,e,e,e,e,e,e,e]] := v [e,e,e,e,e,e,e,e,e,e];
  writeln(w[e,e,e,e,e,e,e,e,e,e])
end.
