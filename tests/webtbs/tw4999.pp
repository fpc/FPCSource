program fpc203fish;
{$mode objfpc}{$H+}
uses  Classes;
var    s:String;
       i:integer;
begin
 s:='123123sdfjkysdjfklsdn3m,w45';
 for i:=0 to 15 do begin
  s:=s+s;
  writeln(i);
 end;
end.  
