{$mode objfpc}{$H+}
uses
 classes;

type
 stringarty = array of string;
var
 ar1,ar2: stringarty;
begin
 setlength(ar1,4);
 ar2:= copy(ar1,1,3);
 if (length(ar2) <> 3) then
   halt(1);
 ar2:= copy(ar1,1,4);
 if (length(ar2) <> 3) then
   halt(2);
 ar2:= copy(ar1,1,5);
 if (length(ar2) <> 3) then
   halt(3);
end.
