{ %CPU=i386 }
{$asmmode att}

const
  Count=100;

type
  trec=record
    a,b,c : longint;
  end;


var
  r : trec;
begin
  asm
    leal r,%edi
    leal r,%esi
    movl %es:46(%edi),%eax
    movl 2+trec.b(%esi),%eax
    movl $1,%ebx
    movl trec.b(%esi,%ebx,(2*4)),%eax
    movl r(,%ebx,(2*4)),%eax
    xorl %esi,%esi
    movl r.c(,%esi,(2*4)),%eax
    movl Count,%eax
    movl Count*100,%eax
    movl trec.b+2,%eax
    leal r,%esi
    movl trec.b+2(%esi),%eax
{$ifdef go32v2}
    movl %fs:(0x46c),%eax
{$endif}
  end;
end.
