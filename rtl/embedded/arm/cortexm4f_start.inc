var
 _stack_top: record end; external name '_stack_top';
 _data: record end; external name '_data';
 _edata: record end; external name '_edata';
 _etext: record end; external name '_etext';
 _bss_start: record end; external name '_bss_start';
 _bss_end: record end; external name '_bss_end';

procedure Pascalmain; external name 'PASCALMAIN';

procedure HaltProc; assembler; nostackframe; public name'_haltproc';
asm
.Lloop:
   b .Lloop
end;

procedure Startup; assembler; nostackframe; [public, alias: '_START'];
asm
  ldr r1,.L_etext
  ldr r2,.L_data
  ldr r3,.L_edata
.Lcopyloop:
  cmp r2,r3
  ittt ls
  ldrls r0,[r1],#4
  strls r0,[r2],#4
  bls .Lcopyloop

  // clear onboard ram
  ldr r1,.L_bss_start
  ldr r2,.L_bss_end
  mov r0,#0
.Lzeroloop:
  cmp r1,r2
  itt ls
  strls r0,[r1],#4
  bls .Lzeroloop

  bl PASCALMAIN
  b HaltProc

.L_bss_start:
  .long _bss_start
.L_bss_end:
  .long _bss_end
.L_etext:
  .long _etext
.L_data:
  .long _data
.L_edata:
  .long _edata
end;