{ %FAIL }

{ @@(x) should be refused in normal mode }

var
 x:function(x:longint):longint;
 z:pointer;
begin
 z:=@@x;
end.
