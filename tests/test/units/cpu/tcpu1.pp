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
  write('FMA support: ');
  if FMASupport then
    begin
      writeln('yes');
      asm
        vpxor %ymm0,%ymm0,%ymm0
// no compiler support yet for fma
//        vfmadd132SS %ymm0,%ymm0,%ymm0
      end;
    end
  else
    writeln('no');
end.

