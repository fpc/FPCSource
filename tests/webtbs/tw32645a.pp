{ %fail }

{$mode objfpc}

var myarray : array ['a'..'zz'] of integer; //signal 291


procedure myproc (myarray: array of integer);
begin
  if high(myarray)<>25 then
    halt(1);
end;

begin
  myproc(myarray);
end.
