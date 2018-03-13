unit uw0701d;

  interface

  implementation

uses erroru;

var
   startmem : sizeuint;

initialization
   startmem:=0;
   DoMem(startmem);
finalization
   if DoMem(startmem)<>0 then
     begin
       writeln('Problem with ansistrings in units');
       halt(1);
     end;
end.
