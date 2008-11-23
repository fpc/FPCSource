{ %fail }
{ %opt=-Sew -vw }

{ Source provided for Free Pascal Bug Report 3553 }
{ Submitted by "Naj Kejah" on  2005-01-15 }
{ e-mail: universario@hotmail.com }

var K : integer;

BEGIN
  while FALSE do
    K:=2;{ NO warning "unreachable code" from User.txt  }

  if FALSE then
    K:=2
  else
    K:=3;

  if TRUE then
    K:=2
  else
    K:=3;
END.
