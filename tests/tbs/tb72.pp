{ %OPT=-Rintel }

{ Old file: tbs0078.pp }
{  Shows problems with longint constant in intel asm     OK 0.99.1 (CEC) }

{ shows error with asm_size_mismatch }
Begin
 asm
   mov eax, 2147483647
   mov eax, 2000000000
 end;
end.
