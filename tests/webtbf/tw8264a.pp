{ %fail }
{ %opt=-Sew -vw }

{$mode delphi}

function calclength1: integer;
var
  a: array of byte;
begin
  result := length(a);
end;


begin
end.
