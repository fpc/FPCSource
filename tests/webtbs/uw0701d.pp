unit uw0701d;

  interface

  implementation

var
   startmem : longint;

initialization
   startmem:=heapsize-memavail;
finalization
   if startmem<>heapsize-memavail then
     begin
       writeln('Problem with ansistrings in units');
       halt(1);
     end;
end.
