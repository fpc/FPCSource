{ %target=win32,win64 }
{ %skiptarget=$nosharedlib }
{ %needlibrary }
{ %norun }
{ %neededafter }

library tlib1a2;

  procedure p(var a : dword);
    begin
      a:=2;
    end;

  exports p;

begin
end.
