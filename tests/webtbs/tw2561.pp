{ Source provided for Free Pascal Bug Report 2561 }
{ Submitted by "Nikolay Nikolov" on  2003-07-06 }
{ e-mail: nickysn1983@netscape.net }
Procedure Tralala(Var q);

Begin
  Writeln(SizeOf(q));
  if sizeof(q)<>0 then
    halt(1);
End;

Var
  q : Integer;
  w : Array[1..10] Of Integer;

Begin
  Tralala(q);
  Tralala(w);
End.
