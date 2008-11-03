{ %opt=-Sen -vn }
{$mode delphi}

function calclength1(const s1: shortstring): integer;
var
  s2: shortstring;
begin
  s2 := s1;
  result := length(s2);
end;


function calclength2(const s1: ansistring): integer;
var
  s2: ansistring;
begin
  s2 := s1;
  result := length(s2);
end;

begin
end.
