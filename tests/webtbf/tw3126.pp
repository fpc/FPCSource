{ %fail }

{ Source provided for Free Pascal Bug Report 3126 }
{ Submitted by "Christian Iversen" on  2004-05-30 }
{ e-mail: chrivers@iversen-net.dk }
Program test;

Var
  P : PChar;
Begin
  Case P^ Of
    'A'-'Z' : ;
  End;
End.
