{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 5036 }
{ Submitted by "" on  2006-04-21 }
{ e-mail: snk_post@mail.ru }
procedure SSE_NormalizeRadVectorXMM0; assembler;
{$ASMMODE Intel}
asm
 shufps xmm1,xmm1,11010010b // xmm1[zz,xx,yy,dd] <- Error
end;

begin
end.
