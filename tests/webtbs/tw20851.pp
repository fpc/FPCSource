program genimpl1;
{$ifdef fpc}{$mode delphi}{$else}{$apptype console}{$endif}
Type
  tbwimagegen<T> = Class 
                 Type

                    BaseUnit = T;
                 procedure alloc;
                 end;


procedure tbwimagegen<T>.alloc;
var i,j : integer;
begin
  j:=sizeof(t);
  i:=sizeof(baseunit);
end;

begin
end.