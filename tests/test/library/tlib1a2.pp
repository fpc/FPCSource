{ %skiptarget=go32v2 }
{ %norun }
{$goto on}
library tlib1a2;

  procedure p(var a : dword);
    begin
      a:=2;
    end;

  exports p;

begin
end.
