program prueba;
uses crt;
var
 resultado,exponente:integer;
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
 resultado := -1 ** exponente;
 writeln (resultado);
 if resultado<>-1 then
   Halt(1);
end.
