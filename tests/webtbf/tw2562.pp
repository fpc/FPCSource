{ %fail }
{ Source provided for Free Pascal Bug Report 2562 }
{ Submitted by "Nikolay Nikolov" on  2003-07-06 }
{ e-mail: nickysn1983@netscape.net }
Procedure Tralala(Var q, w);

Begin
  q := w;
End;

Var
  q : Integer;
  w : Array[1..10] Of Integer;

Begin
  Tralala(q, w);
End.
