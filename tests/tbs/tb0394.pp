{ %version=1.1 }

{$ifdef fpc}{$mode objfpc}{$endif}

var
  err : boolean;
procedure Demo(x:array of longint);
 var
  i:longint;
 begin
   if high(x)<>4 then
    err:=true
   else if x[4]<>14 then
    err:=true;
  for i:=low(x)to high(x)do
   writeln(i,' ',x[i]);
 end;
var
 y:array[10..40]of longint;
 i:longint;
begin
 for i:=10 to 40 do
  y[i]:=i;
 Demo(slice(y,5));
 if err then
  begin
    writeln('ERROR!');
    halt(1);
  end;
end.
