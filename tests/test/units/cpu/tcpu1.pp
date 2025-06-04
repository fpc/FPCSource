{ %cpu=x86_64,i386}

uses
  cpu;

procedure test_cpu;
{$ifndef cpui386}
var
  dummy16b : array[0..15] of byte;
  val_rax, val_rdx : qword;
  i : byte;
  dummy_matched : boolean;
{$endif ndef cpui386}
begin
  write('CMOV support: ');
  if CMOVSupport then
    begin
      writeln('yes');
      asm
        cmove %eax,%eax
        fldz
        fldz
        fcmove %st(1),%st(0)
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
  write('SHA512 support: ');
  if SHA512Support then
    begin
      writeln('yes');
      asm
//        vsha512msg2 %ymm0,%ymm0
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

{$ifndef cpui386}
  { makes no sense on i386, the instruction is not available in 32 bit mode }
  write('CMPXCHG16B support: ');
  if CMPXCHG16BSupport then
    begin
      writeln('yes');
      val_rax:=qword($123456789ABCDEF0);
      val_rdx:=qword($9ABCDEF012345678);
      pqword(@Dummy16b)^:=val_rax;
      pqword(@Dummy16b[8])^:=val_rdx;
      write('Dummy16b  values: ');
      for i:=0 to 15 do
	write(hexstr(Dummy16b[i],2));
      writeln;
      asm
        movq val_rax,%rax
        movq val_rdx,%rdx
	xor %rcx,%rcx
        add $512,%rcx
	xor %rbx,%rbx
        add $2048,%rbx
        cmpxchg16b Dummy16b
        jz .Lchanged
        mov %rax,val_rax
        mov %rdx,val_rdx
        mov $0,dummy_matched
        jmp .Lend
        .Lchanged:
        mov $1,dummy_matched
        .Lend:
      end;
      writeln('Dummy16b was modified: ',dummy_matched);
      write('Dummy16b  values: ');
      for i:=0 to 15 do
	write(hexstr(Dummy16b[i],2));
      writeln;
      if not dummy_matched then
        halt(1);
    end
  else
    writeln('no');
{$endif cpui386}
end;

begin
  test_cpu;
end.

