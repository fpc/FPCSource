{ %skiptarget=go32v2 }
{ %norun }
{$goto on}
library tlib1a;

  procedure p(var a : dword);
    begin
      a:=1;
    end;

  exports p;

begin
end.
