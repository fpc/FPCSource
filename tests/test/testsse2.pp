{ %CPU=i386 }
{ %version=1.1}
uses
   mmx;

{ only a small test to see if it works in principal }

var
   q : array[0..15] of byte;

begin
  if is_sse2_cpu then
    asm
       movdqa    %xmm1,%xmm2
       movdqa    q,%xmm4
       psubq     %xmm1,%xmm2
       psubq     q,%xmm4
    end;
end.
