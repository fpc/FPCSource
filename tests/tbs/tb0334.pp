{$mode objfpc}

uses
   sysutils;

var
   s : tintegerset;

begin
   if sizeof(s)<>sizeof(integer) then
     begin
        writeln('Wrong size of Sysutils.TIntegerSet (',sizeof(s),')');
        halt(1);
     end;
end.
