{ %FAIL }
{$mode objfpc}
label l;

begin
   try
   except
      goto l;
   end;
   l:
end.
