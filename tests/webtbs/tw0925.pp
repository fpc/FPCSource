{ %CPU=i386 }
{$asmmode intel}

{$ifdef go32v2}
  PROCEDURE Cursor(Form: word);assembler;
  asm
     mov cx,word ptr[Form]
     and cx,1F1Fh
     mov ah,1
     int 10h
  end;
{$else not go32v2}
  { no interrupt call on other targets }
  procedure cursor(form : word);assembler;
  asm
     mov cx,word ptr[Form]
     and cx,1F1Fh
     mov ah,1
  end;
{$endif go32v2}

begin
  Cursor($11F);
end.
