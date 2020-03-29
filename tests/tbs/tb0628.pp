{ %CPU=i386,x86_64 }
{ %NORUN }

program tb0628;

{$asmmode att}

begin
  asm
    cmpsd $0,%xmm1,%xmm0
    cmpsd $1,%xmm1,%xmm0
    cmpsd $2,%xmm1,%xmm0
    cmpsd $3,%xmm1,%xmm0
    cmpsd $4,%xmm1,%xmm0
    cmpsd $5,%xmm1,%xmm0
    cmpsd $6,%xmm1,%xmm0
    cmpsd $7,%xmm1,%xmm0
  end;
end.
