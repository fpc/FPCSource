{ %FAIL }
{$mode objfpc}
uses
   sysutils;

label l;

begin
   try
   except
      on e : exception do
        goto l;
   end;
   l:
end.
