program simpletest;

var i : longint;

  function _next : longint;
    begin
      inc(i);
      _next:=i;
    end;

  procedure test(a,b : longint);
    begin
      Writeln('first arg is ',a);
      Writeln('second arg is ',b);
    end;

begin
{ this could give 
  first arg is 1
  second arg is 2 
  but FPC parses the second arg before the first one ! }
test(_next,_next);
writeln('third arg is ',_next);
writeln('fourth arg is ',_next,' fifth arg is ',_next);
end.
