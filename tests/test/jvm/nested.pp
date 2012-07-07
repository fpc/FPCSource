{ %norun }
program nested;

function test : string;
var
 a, b : integer;

 function work : integer;
 begin
   a := 1;
   b := 2;
 end;

begin
 work;
end;

begin
end.
