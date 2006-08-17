{ generates iA-64 register dat templates }

uses
  sysutils;
var
  i : longint;
begin
  { generate int registers }
  for i:=0 to 127 do
    writeln(format('R%d,$01,%d,r%d,r%d',[i,i,i,i]));
  { generate fp registers }
  for i:=0 to 127 do
    writeln(format('F%d,$02,%d,r%d,r%d',[i,i,i,i]));
end.
