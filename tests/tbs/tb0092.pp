{ Old file: tbs0109.pp }
{ syntax error not detected when using a set as pointer OK 0.99.1 (FK) }

Type T = (aa,bb,cc,dd,ee,ff,gg,hh);
     Tset = set of t;

Var a: Tset;

Begin
  If (aa in a) Then begin end;
  {it seems that correct code is generated, but the syntax is wrong}
End.
