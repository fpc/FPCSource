{ $OPT=-Rintel }
{ shows error with asm_size_mismatch }
Begin
 asm
   mov eax, 2147483647 
   mov eax, 2000000000
 end;
end.
