uses
   sysutils;

var
   s : tintset;

begin
   if sizeof(s)<>sizeof(integer) then
     begin
        writeln('Wrong size of Sysutils.TIntegerSet');
        halt(1);
     end;
end.
