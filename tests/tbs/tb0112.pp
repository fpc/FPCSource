{ Old file: tbs0131.pp }
{ internal error 10 with highdimension arrays           OK 0.99.6 (MVC) }

type TA = Array[1..2,1..2,1..2,1..2,1..2,1..2,1..3,1..3,1..3,1..3] of Byte;

var v,w: ta;
    e: longint;

Begin
  e :=1;
  v[e,e,e,e,e,e,e,e,e,e] :=1;
  w[e,e,e,e,e,e,v[e,e,e,e,e,e,e,e,e,e],e,e,v[e,e,e,e,e,e,v[e,v[e,e,e,e,e,v[e,e,e,e,e,e,e,e,e,e],e,e,e,e],e,e,e,e,e,e,e,e],e,e,e]] := v [e,e,e,e,e,e,e,e,e,e];
  writeln(w[e,e,e,e,e,e,e,e,e,e])
end.
