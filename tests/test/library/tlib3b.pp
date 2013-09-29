{ %target=win32,win64 }
{ %needlibrary }
{ %norun }
{ %neededafter }

library tlib3b;

  function p(a1, a2, a3, a4, a5, a6, a7 : dword) : dword;
    begin
      p:=a1+a2+a3+a4+a5+a6+a7;
    end;

  exports p;

begin
end.
