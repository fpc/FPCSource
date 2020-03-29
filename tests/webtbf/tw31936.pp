{ %fail }
{ %opt=-Sew }
var
  I : longint;
  Somestring : string;

begin
  Somestring:='asdf';
  I := 1;
  While Not (Somestring[I] in ['#'..#0]) Do Inc(I);
end.

