{ %opt=-Cg- }
{ %cpu=i386 }

program fpcbug;

{$asmmode intel}

type
  TBla = record
    Bla: Cardinal;
  end;

var
  b: tbla;
begin
  asm
    lea edi, b
    mov eax, $12345678
    mov TBla(edi).&Bla, eax
  end;
  if b.bla<>$12345678 then
    halt(1);
end.
