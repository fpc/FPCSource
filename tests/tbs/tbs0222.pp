
type TStruct = record
                 x,y: Integer;
               end;

var i: TStruct;

begin
  for i.x:=1 to 10 do
    writeln(i.x);
end.
