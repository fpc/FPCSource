{ Old file: tbs0078.pp }
{  Shows problems with longint constant in intel asm     OK 0.99.1 (CEC) }

{ shows error with asm_size_mismatch }
Begin
{$ifdef CPUI386}
{$asmmode intel }
 asm
   mov eax, 2147483647
   mov eax, 2000000000
 end;
{$endif CPUI386}
{$ifdef CPU68K}
  asm
    move.l #2147483647,d0
    move.l #2000000000,d1
  end;
{$endif CPU68K}
end.
