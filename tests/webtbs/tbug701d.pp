unit tbug701d;

  interface

  implementation 

var
   startmem : longint;

initialization
   startmem:=memavail;
finalization
   if startmem<>memavail then
     begin      
       writeln('Problem with ansistrings in units');
       halt(1);
     end;
end.
