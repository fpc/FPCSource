{ %cpu=x86_64,i386 }

begin
  asm
    nop
    .p2align 3
    .p2align 4,,10  
    .p2align 4,0x90
    .p2align 4,0x90,10  
  end;
end.
