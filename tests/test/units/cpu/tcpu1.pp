{ %cpu=x86_64,i386}

uses
  cpu;

begin
  write('AVX support: ');
  if AVXSupport then
    begin
      writeln('yes');
      asm
        vpxor %xmm0,%xmm0,%xmm0
      end;
    end
  else
    writeln('no');
  write('AVX2 support: ');
  if AVX2Support then
    begin
      writeln('yes');
      asm
        vpxor %ymm0,%ymm0,%ymm0
      end;
    end
  else
    writeln('no');
  write('AVX512F support: ');
  if AVX512FSupport then
    begin
      writeln('yes');
      asm
        vpxor %ymm0,%ymm0,%ymm0
        vaddpd %zmm0,%zmm0,%zmm0
      end;
    end
  else
    writeln('no');
  write('AVX512VNNI support: ');
  if AVX512VNNISupport then
    begin
      writeln('yes');
      asm
       vpxor %ymm0,%ymm0,%ymm0
       vpdpbusd %zmm0,%zmm0,%zmm0
      end;
    end
  else
    writeln('no');
  write('AVX512BITALG support: ');
  if AVX512BITALGSupport then
    begin
      writeln('yes');
      asm
        vpxor %ymm0,%ymm0,%ymm0
        vpopcntb %zmm0,%zmm0
      end;
    end
  else
    writeln('no');
  write('AVX512VBMI support: ');
  if AVX512VBMISupport then
    begin
      writeln('yes');
      asm
       vpxor %ymm0,%ymm0,%ymm0
       vpermi2b %zmm0,%zmm0,%zmm0
      end;
    end
  else
    writeln('no');
  write('AVX512VBMI2 support: ');
  if AVX512VBMI2Support then
    begin
      writeln('yes');
      asm
        vpxor %ymm0,%ymm0,%ymm0
        vpexpandw %zmm0,%zmm0
      end;
    end
  else
    writeln('no');
  write('FMA support: ');
  if FMASupport then
    begin
      writeln('yes');
      asm
        pxor %xmm0,%xmm0
        vfmadd132SS %xmm0,%xmm0,%xmm0
      end;
    end
  else
    writeln('no');
  write('SHA support: ');
  if SHASupport then
    begin
      writeln('yes');
      asm
        sha256rnds2 %xmm0,%xmm0
      end;
    end
  else
    writeln('no');
  write('LZCNT support: ');
  if LZCNTSupport then
    begin
      writeln('yes');
      asm
        lzcnt %eax,%eax
      end;
    end
  else
    writeln('no');
  write('ADX support: ');
  if ADXSupport then
    begin
      writeln('yes');
      asm
        adcx %eax,%eax
      end;
    end
  else
    writeln('no');
  write('RDSEED support: ');
  if RDSEEDSupport then
    begin
      writeln('yes');
      asm
        rdseed %eax
      end;
    end
  else
    writeln('no');
end.

