{$asmmode intel}

const
  Count=100;

type
  trec=record
    a,b : longint;
  end;

var
  r : trec;
begin
  asm
    mov [esi+r],eax
    mov fs:[2+trec.b],eax
    mov [esi+2+trec.b],eax
    mov trec[esi].b,eax
    mov fs:[046ch],eax
    mov eax,trec.b+2
    mov trec[esi].b+2,eax
    mov eax,Count
    mov eax,Count*100
  end;
end.