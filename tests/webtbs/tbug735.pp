{$asmmode intel}

procedure DoIt;
begin
  Writeln('DoIt was called');
end;

const
  CB : word = 5;

procedure A(B: word); assembler; inline;
asm
   MOV  AX,B
   CMP  AX,[CB]
   JZ   @OK
   CLI
   MOV  [CB],AX
   STI
   CALL DoIt
@OK:      { <-- creates labels with same name }
end;

begin
   A(5);
   A(8);
end.
