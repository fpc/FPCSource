{$push}
{$asmmode intel}

procedure sha256x86AsmCompress (var Ctx:TContextBuffer; var Data:THashBuffer; aK:pointer; aMask:pointer); assembler;
{$if defined(x86_64) and not defined(windows)}nostackframe;{$endif}
{$ifndef x86_64}
var ABEF_SAVE, CDGH_SAVE : record a,b:qword end;
{$endif}
{$if defined(x86_64) and defined(windows)}
var sav6,sav7,sav11,sav12 : record a,b:qword end; {abi for win64 requires to save and restore xmm6-xmm15}
{$endif}
{
  Register usage
    xmm0 MSG
    xmm1 STATE1
    xmm2 STATE0
    xmm3 MSG3
    xmm4 MSG0
    xmm5 MSG1
    xmm6 MSG2
    xmm7 TMP & MASK
    xmm11 CDGH_SAVE (only on x64)
    xmm12 ABEF_SAVE (only on x64)
}
asm
{$if defined(x86_64) and defined(windows)}
   movdqu sav6,xmm6
   movdqu sav7,xmm7
   movdqu sav11,xmm11
   movdqu sav12,xmm12
{$endif}
   {initalize}
   movdqu xmm2,oword ptr[Ctx]
   pshufd xmm0,xmm2,$B1
   movdqu xmm2,oword ptr[Ctx+$10]
   movdqa xmm1,xmm0
   pshufd xmm2,xmm2,$1B
   palignr xmm1,xmm2,$08
   pblendw xmm2,xmm0,$F0

   {preread some data}
   {$ifdef x86_64}
   movdqu xmm7,oword ptr[aMask]
   {$else}
   push ebx
   mov ebx,aMask
   movdqu xmm7,oword ptr[ebx]
   pop ebx
   {$endif}
   movdqu xmm0,oword ptr[aK+$00]
   movdqu xmm4,oword ptr[Data+00] { MSG0 }
   movdqu xmm5,oword ptr[Data+16] { MSG1 }
   movdqu xmm6,oword ptr[Data+32] { MSG2 }
   movdqu xmm3,oword ptr[Data+48] { MSG3 }

   {$ifdef x86_64}
   movdqa xmm12,xmm2
   movdqa xmm11,xmm1
   {$else}
   movdqu oword ptr [ABEF_SAVE],xmm2
   movdqu oword ptr [CDGH_SAVE],xmm1
   {$endif}

   { do rounds 0-3 }
   pshufb xmm4,xmm7
   pshufb xmm5,xmm7
   paddd xmm0,xmm4
   pshufb xmm6,xmm7
   pshufb xmm3,xmm7
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   movdqa xmm7,xmm3
   sha256rnds2 xmm1,xmm2
   { do rounds 4-7 }
   movdqu xmm0,oword ptr[aK+$10]
   sha256msg1 xmm4,xmm5
   paddd xmm0,xmm5
   palignr xmm7,xmm6,$04
   sha256msg1 xmm5,xmm6
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   paddd xmm4,xmm7
   sha256rnds2 xmm1,xmm2
   sha256msg2 xmm4,xmm3
   { do rounds 8-11 }
   movdqu xmm0,oword ptr[aK+$20]
   movdqa xmm7,xmm4
   paddd xmm0,xmm6
   sha256msg1 xmm6,xmm3
   palignr xmm7,xmm3,$04
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   paddd xmm5,xmm7
   sha256rnds2 xmm1,xmm2
   { do rounds 12-15 }
   movdqu xmm0,oword ptr[aK+$30]
   sha256msg2 xmm5,xmm4
   paddd xmm0,xmm3
   movdqa xmm7,xmm5
   sha256msg1 xmm3,xmm4
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   add aK,$7f     { just later to shorten displacement constants }
   sha256rnds2 xmm1,xmm2
   { do rounds 16-19 }
   movdqu xmm0,oword ptr[aK-$3f]
   palignr xmm7,xmm4,$04
   paddd xmm6,xmm7
   paddd xmm0,xmm4
   sha256msg2 xmm6,xmm5
   sha256msg1 xmm4,xmm5
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   movdqa xmm7,xmm6
   sha256rnds2 xmm1,xmm2
   { do rounds 20-23 }
   movdqu xmm0,oword ptr[aK-$2f]
   palignr xmm7,xmm5,$04
   paddd xmm3,xmm7
   paddd xmm0,xmm5
   sha256msg2 xmm3,xmm6
   sha256msg1 xmm5,xmm6
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   movdqa xmm7,xmm3
   sha256rnds2 xmm1,xmm2
   { do rounds 24-27 }
   movdqu xmm0,oword ptr[aK-$1f]
   palignr xmm7,xmm6,$04
   paddd xmm4,xmm7
   paddd xmm0,xmm6
   sha256msg2 xmm4,xmm3
   sha256msg1 xmm6,xmm3
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   movdqa xmm7,xmm4
   sha256rnds2 xmm1,xmm2
   { do rounds 28-31 }
   movdqu xmm0,oword ptr[aK-$0f]
   palignr xmm7,xmm3,$04
   paddd xmm5,xmm7
   paddd xmm0,xmm3
   sha256msg2 xmm5,xmm4
   inc aK
   sha256msg1 xmm3,xmm4
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   movdqa xmm7,xmm5
   sha256rnds2 xmm1,xmm2
   { do rounds 32-35 }
   movdqu xmm0,oword ptr[aK+$00]
   palignr xmm7,xmm4,$04
   paddd xmm6,xmm7
   paddd xmm0,xmm4
   sha256msg2 xmm6,xmm5
   sha256msg1 xmm4,xmm5
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   movdqa xmm7,xmm6
   sha256rnds2 xmm1,xmm2
   { do rounds 36-39 }
   movdqu xmm0,oword ptr[aK+$10]
   palignr xmm7,xmm5,$04
   paddd xmm3,xmm7
   paddd xmm0,xmm5
   sha256msg2 xmm3,xmm6
   sha256msg1 xmm5,xmm6
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   movdqa xmm7,xmm3
   sha256rnds2 xmm1,xmm2
   { do rounds 40-43 }
   movdqu xmm0,oword ptr[aK+$20]
   palignr xmm7,xmm6,$04
   paddd xmm4,xmm7
   paddd xmm0,xmm6
   sha256msg2 xmm4,xmm3
   sha256msg1 xmm6,xmm3
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   movdqa xmm7,xmm4
   sha256rnds2 xmm1,xmm2
   { do rounds 44-47 }
   movdqu xmm0,oword ptr[aK+$30]
   palignr xmm7,xmm3,$04
   paddd xmm0,xmm3
   paddd xmm5,xmm7
   sha256msg1 xmm3,xmm4
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   sha256msg2 xmm5,xmm4
   sha256rnds2 xmm1,xmm2
   { do rounds 48-51 }
   movdqu xmm0,oword ptr[aK+$40]
   paddd xmm0,xmm4
   movdqa xmm7,xmm5
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   sha256rnds2 xmm1,xmm2
   palignr xmm7,xmm4,$04
   { do rounds 52-55 }
   movdqu xmm0,oword ptr[aK+$50]
   paddd xmm6,xmm7
   sha256msg2 xmm6,xmm5
   paddd xmm0,xmm5
   sha256rnds2 xmm2,xmm1
   movdqa xmm7,xmm6
   pshufd xmm0,xmm0,$0E
   sha256rnds2 xmm1,xmm2
   palignr xmm7,xmm5,$04
   { do rounds 56-59 }
   movdqu xmm0,oword ptr[aK+$60]
   paddd xmm3,xmm7
   paddd xmm0,xmm6
   sha256msg2 xmm3,xmm6
   movdqu xmm7,oword ptr[aK+$70]
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm0,$0E
   paddd xmm3,xmm7
   sha256rnds2 xmm1,xmm2
   { do rounds 60-63 }
   movdqa xmm0,xmm3
   sha256rnds2 xmm2,xmm1
   pshufd xmm0,xmm3,$0E
   sha256rnds2 xmm1,xmm2

   { add saved state to current }
   {$ifdef x86_64}
   paddd xmm2,xmm12
   paddd xmm1,xmm11
   {$else}
   movdqu xmm7,oword ptr[ABEF_SAVE]
   movdqu xmm6,oword ptr[CDGH_SAVE]
   paddd xmm2,xmm7
   paddd xmm1,xmm6
   {$endif}

   { this is end end, save state }
   pshufd xmm1,xmm1,$1B
   pshufd xmm2,xmm2,$B1
   movdqa xmm0,xmm1
   pblendw xmm0,xmm2,$F0
   palignr xmm2,xmm1,$08
   movdqu oword ptr[Ctx],xmm0
   movdqu oword ptr[Ctx+$10],xmm2
{$if defined(x86_64) and defined(windows)}
   movdqu xmm6,sav6
   movdqu xmm7,sav7
   movdqu xmm11,sav11
   movdqu xmm12,sav12
{$endif}
end;

procedure sha256Compress_Dispatch(var Context:TContextBuffer; var HashBuffer:THashBuffer; K:pointer; Mask:pointer); forward;

var
  sha256Compress_Impl: procedure (var Context:TContextBuffer; var HashBuffer:THashBuffer; K:pointer; Mask:pointer) = @sha256Compress_Dispatch;

procedure sha256Compress_Dispatch(var Context:TContextBuffer; var HashBuffer:THashBuffer; K:pointer; Mask:pointer);
begin
  if SHASupport then
    sha256Compress_Impl:=@sha256x86AsmCompress
  else
    sha256Compress_Impl:=@sha256PascalCompress; {fallback to pascal implementation if CPU does not support SHA instruction set}
  sha256Compress_Impl(Context,HashBuffer,K,Mask);
end;

procedure sha256AsmCompress(var Context:TContextBuffer; var HashBuffer:THashBuffer; K:pointer; Mask:pointer); inline;
begin
  sha256Compress_Impl(Context,HashBuffer,K,Mask);
end;

{$pop}