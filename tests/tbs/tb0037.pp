{ %CPU=i386 }
{ %OPT= -Rintel }

{ Old file: tbs0042.pp }
{  shows assembler double operator expression problem  OK 0.99.7 (PFV) }

Begin
 asm
   mov ax,3*-4     { evaluator stack underflow }
 end;              { due to two operators following each other }
end.               { this will also happen in att syntax.       }

