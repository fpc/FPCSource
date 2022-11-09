{ %cpu=x86_64,i386}

uses
  cpu;

begin
  write('CMOV support: ');
  if CMOVSupport then
    begin
      writeln('yes');
      asm
        cmov %eax,%eax
        fldz
        fldz
        fcmovb %st(1)
        fstpl %st(0)
        fstpl %st(0)
      end;
    end
  else
    writeln('no');

  write('SSE3 support: ');
  if SSE3Support then
    begin
      writeln('yes');
      asm
        pxor %xmm0,%xmm0
        haddpd %xmm0,%xmm0
      end;
    end
  else
    writeln('no');

  write('SSSE3 support: ');
  if SSSE3Support then
    begin
      writeln('yes');
      asm
        pxor %xmm0,%xmm0
        phaddsw %xmm0,%xmm0
      end;
    end
  else
    writeln('no');

  write('AES support: ');
  if AESSupport then
    begin
      writeln('yes');
      asm
        pxor %xmm0,%xmm0
        aesenc %xmm0,%xmm0
      end;
    end
  else
    writeln('no');

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
  write('VAES support: ');
  if VAESSupport then
    begin
      writeln('yes');
      asm
        vpxor %ymm0,%ymm0,%ymm0
        vaesenc %ymm0,%ymm0,%ymm0
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

