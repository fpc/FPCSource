{ %fail }
{ %OPT=-CO -Sewnh -vwnh }
{ changed -Seh to -Sewnh because -Cr option
  changes the hint into a warning PM }

var a:int64;
i:integer;
begin
a:=0;
for i:=a to 10 do;
end.
