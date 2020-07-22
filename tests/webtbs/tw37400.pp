{ %cpu=i386,x86_64 }
{ %opt=-Cpcoreavx2 }
function popc ( a, b: byte):byte;
var z,v: byte;
begin  
     z:=a+b;
     v:=popcnt(z); //-- this line
     popc:=v;
end;

begin
end.
