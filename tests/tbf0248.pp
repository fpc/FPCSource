{$asmmode att}

begin
  asm
     call *%eax // this is correct
     movl %esi,*%eax
  end;
end.
