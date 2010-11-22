program test;

var
  wa, ws : set of 1..9;
begin
  wa := [1..2];
  ws := [1..3];
  if (wa <= ws) and (wa <> ws) then writeln('True') else writeln('False'); 
  if (wa <= ws) then
  if (wa <> ws) then writeln('True') else writeln('False'); 
  if (wa <= ws) then writeln('True') else writeln('False'); 
  if (wa <> ws) then writeln('True') else writeln('False'); 

end.

