{ %CPU=i386 }

{$asmmode intel}

  PROCEDURE Cursor(Form: word);assembler;stdcall;
  asm
     mov cx,word ptr[Form]
     and cx,1F1Fh
     mov ah,1
  end;

begin
  Cursor($11F);
end.
