{$mode objfpc}
uses sysutils;
 var i:integer;
     j : record
        x,y : longint;
     end;
begin
 i:=0;
 format('%d', [i]);
 with j do
   begin
     x:=2;
     y:=4;
     Writeln('j.x=',x,' j.y=',y);
   end;
end.
