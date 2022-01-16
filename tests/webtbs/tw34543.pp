{ %norun }

{$r+}
procedure range_check_fail;
var v : word;
    vTo : word;
    vNo : word;
begin
     vTo:=3;
     vNo:=0;
     for v:=vNo to vTo do {Error: range check error while evaluating constants (-1 must be between 0 and 65535)}
     begin
     end;
end;

begin
end.

