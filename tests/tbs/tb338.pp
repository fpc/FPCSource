{$mode objfpc}

{ tests assignements and compare }

var
   o1,o2 : tobject;

begin
   o1:=nil;
   o2:=o1;
   if o2<>nil then
     halt(1);
end.
