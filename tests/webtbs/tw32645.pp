{$mode objfpc}

var myarray : array ['a'..'z'] of integer; //operator is not overloaded 'char' - 'char'
//var myarray : array ['a'..'zz'] of integer; //signal 291
//var myarray : array ['a'..'z'*5] of integer; //signal 291


procedure myproc (myarray: array of integer);
begin
  if high(myarray)<>25 then
    halt(1);
end;

begin
  myproc(myarray);
end.
