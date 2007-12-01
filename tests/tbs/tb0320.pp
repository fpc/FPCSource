{ %CPU=i386 }
{ %OPT=-Cg- }
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
    xor esi,esi
    mov [esi+r],eax
    lea esi,r
    mov [esi+2+trec.b],eax
    mov trec[esi].b,eax
    mov eax,trec.b+2
    mov trec[esi].b+2,eax
    mov eax,Count
    mov eax,Count*100
{$ifdef go32v2}
    mov fs:[0468+trec.b],eax
    mov fs:[046ch],eax
{$endif}
  end;
end.
