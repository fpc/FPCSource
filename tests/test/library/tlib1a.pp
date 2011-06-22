{ %target=win32,win64 }
{ %norun }
{ %neededafter }
library tlib1a;

  procedure p(var a : dword);
    begin
      a:=1;
    end;

  exports p;

begin
end.
