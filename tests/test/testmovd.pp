{ %CPU=i386 }
{ %maxversion=1.0.99 }

{ This file tests the movd instruction has this
  instruction does convert 32 bit into 64 bit
  which is not handled by the normal assembler instruction
  suffixes Pierre Muller }

uses
  mmx;

{$asmmode intel}


procedure test_intel; { do not run }
begin
  asm
    SUB  ESP,4
    MOVD [ESP],MM7
    MOVD MM0,DWORD PTR [ESP]
    MOVD MM1,[ESP]
    MOVD DWORD PTR [ESP],MM3
    ADD  ESP,4
  end;
end;

procedure test_cvtsi2ss_intel;
begin
  asm
    cvtsi2ss xmm2,DWORD PTR [esp]
    cvtsi2ss xmm2, [esp]
  end;
end;

{$asmmode att}

procedure test_att; { do not run }
begin
  asm
    subl  $4,%esp
    movd  (%esp),%mm2
    movd  %mm6,(%esp)
    addl  $4,%esp
  end;
end;

procedure test_cvtsi2ss_att;
begin
  asm
    cvtsi2ss (%esp),%xmm2
  end;
end;

begin
   if is_mmx_cpu then
     begin
       emms;
       test_att;
       test_intel;
     end;
end.
