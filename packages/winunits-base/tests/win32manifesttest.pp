{
   Small example/test of the html help OCX.
   Marco van de Voort (C) 2026

   Test manifest unit. Print codepage to see if it works
}

uses winmanutf8lfn;

begin
  writeln('Codepage (should be 65001=UTF8): ',defaultsystemcodepage); 
end.