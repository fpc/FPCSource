{ %FAIL }
{$mode objfpc}
label l;

begin
   try
      goto l;
   finally
   end;
   l:
end.
