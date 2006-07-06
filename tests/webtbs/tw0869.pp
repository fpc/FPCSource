program prueba;
uses math;
var
 i,resultado,exponente:integer;
begin
 exponente := 3;
 resultado := -1 ** exponente;
 writeln (resultado);
 if resultado<>-1 then
   Halt(1);
 exponente := 4;
 resultado := -(1 ** exponente);
 writeln (resultado);
 if resultado<>-1 then
   Halt(1);
 resultado := (-1) ** exponente;
 writeln (resultado);
 if resultado<>1 then
   Halt(1);
 i:=1;
 resultado := - i ** exponente;
 writeln (resultado);
 if resultado<>-1 then
   Halt(1);
 resultado := -1 ** exponente;
 writeln (resultado);
 if resultado<>-1 then
   Halt(1);
end.
