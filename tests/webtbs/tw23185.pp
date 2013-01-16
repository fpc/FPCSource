{ %target=linux }
{ %cpu=arm }
{ %norun }
{ %opt=-Cparmv7m -s }

{ we don't assemble, link and run because we cannot check if the different arm architectures really match }
begin
asm
 mrs r0, psp
end;
end.
