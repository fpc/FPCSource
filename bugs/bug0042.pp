Program SomeCrash;
{ with pp -TDOS -Rintel bug0042.pp              }
{ I'll try to fix this for next release -- Carl }

Begin
 asm
   mov ax,3*-4     { evaluator stack underflow }
 end;              { due to two operators following each other }
end.               { this will also happen in att syntax.       }

