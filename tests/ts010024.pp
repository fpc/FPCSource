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
    movl %es:46(%edi),%eax
    movl 2+trec.b(%esi),%eax
    movl trec.b(,%esi,(2*4)),%eax
    movl r.c(,%esi,(2*4)),%eax
    movl %fs:(0x46c),%eax
    movl Count,%eax
    movl Count*100,%eax
    movl trec.b+2,%eax
    movl trec.b+2(%esi),%eax
  end;
end.