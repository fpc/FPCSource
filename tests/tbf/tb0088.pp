{ %FAIL }
{$mode objfpc}
label l;

begin
   try
   finally
   l:
   end;
   goto l;
end.
