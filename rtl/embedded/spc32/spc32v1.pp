unit spc32v1;

{$goto on}
{$macro on}

interface

implementation

var
 _stack_top: record end; external name '_stack_top';
 _data: record end; external name '_data';
 _edata: record end; external name '_edata';
 _text_start: record end; external name '_text_start';
 _etext: record end; external name '_etext';
 _bss_start: record end; external name '_bss_start';
 _bss_end: record end; external name '_bss_end';

procedure Pascalmain; external name 'PASCALMAIN';

procedure HaltProc; assembler; nostackframe; public name'_haltproc';
asm
.Lloop:
   gs 0
   jmp .Lloop
end;

procedure Startup; assembler; nostackframe;
label _start;
asm
  .init
  .globl _start
_start:
  ld 0x1000
  //ldu hi16(0x1000)
  st r6
  
  nul
  ldu hi16(Pascalmain)
  jmp lo16(Pascalmain)
  .text
end;

end.
