{ %FAIL }

{ addr(@(x)) should be refused in normal mode }

var
 x:function(x:longint):longint;
 w:pointer;
begin
 w:=addr(@x);
end.
