{ %opt=-Sew }

{ Source provided for Free Pascal Bug Report 3952 }
{ Submitted by "Ivo Steinmann" on  2005-05-08 }
{ e-mail: istienmann@bluewin.ch }
var
  P: Pointer;
  X: Longword;
begin
  X := 0;
  P := nil;
  P := Pointer(P + X - 12);
  P := Pointer(P + X + 12);
end.
