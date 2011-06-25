{ %needlibrary }
{ %norun }
{ %neededafter }

library tlib3a;

  procedure p(var a : dword);
    begin
      a:=1;
    end;

  exports p;

begin
end.
