{ %FAIL }
{ Old file: tbf0248.pp }
{ Asm, Wrong assembler code accepted by new assembler reader OK 0.99.11 (PFV) }

{$asmmode att}

begin
  asm
     call *%eax // this is correct
     movl %esi,*%eax
  end;
end.
