{ %FAIL }

{ @(addr(x)) should be refused in normal mode }

var
 x:function(x:longint):longint;
 v:pointer;
begin
 v:=@(addr(x));
end.
