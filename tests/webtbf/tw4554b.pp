{ %fail }
{ %opt=-vw -Sew }

procedure rrr;    
var bar, ear: array[0..1] of integer;
begin
  ear[1]:=bar[1];
end; 

BEGIN
  rrr ;
END.
