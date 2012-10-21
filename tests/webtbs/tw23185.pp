{ %target=linux }
{ %cpu=arm }
{ %norun }
{ %opt=-Cparmv7m }
begin
asm
 mrs r0, psp
end;
end.
