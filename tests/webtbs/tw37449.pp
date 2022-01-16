{ %OPT=-O- }
var x:integer;
begin
    x := 1;
    x := x mod -1; { buggy }
end.
