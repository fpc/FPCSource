program showbug ;

{$mode macpas}

var
  glob: integer;

function countchars: INTEGER ;
begin
  countchars:=255;
  if glob=5 then
    countchars := 0
  else
    begin
      inc(glob);
      countchars := 1 + countchars
    end
  end;

begin
  if countchars<>5 then
    halt(1);
end .

