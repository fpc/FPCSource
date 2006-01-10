{ %fail }
{ %opt=-vw -Sew }

procedure rrr;    
var arr, nar: array[0..1] of integer;
begin
  arr:=nar; 
end; { my NEVER = no warning, regardless if in a proc or not }

BEGIN
  rrr ;
END.
