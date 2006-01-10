{ %cpu=i386 }

{ Source provided for Free Pascal Bug Report 4613 }
{ Submitted by "darek" on  2005-12-16 }
{ e-mail: darekm@emadar.com }

{$mode delphi}

procedure p1;
asm
   @L1000:
     //movdqu  xmm0, [eax+ebx]
     db $F2 mov eax,ebx
     db $F2 db $0F db $F0 db $04 db $03 //this cause error
     //movdqa  [edx+ebx], xmm0
     //movdqu  xmm1, [eax+ebx+16]
end;

begin
end.
