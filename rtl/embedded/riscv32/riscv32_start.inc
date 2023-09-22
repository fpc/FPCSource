var
 _stack_top: record end; external name '_stack_top';
 _data: record end; external name '_data';
 _edata: record end; external name '_edata';
 _text_start: record end; external name '_text_start';
 _etext: record end; external name '_etext';
 _bss_start: record end; external name '_bss_start';
 _bss_end: record end; external name '_bss_end';

procedure Pascalmain; external name 'PASCALMAIN';

procedure _haltproc; external Name '_haltproc';
procedure HaltProc; noreturn;
begin
  while true do;
end;

procedure InitMemAndStart; noreturn;
var
  pdest, psrc, pend: PLongWord;
begin
  pdest:=@_data;
  psrc:=@_etext;
  pend:=@_bss_start;

  while pdest<pend do
    begin
      pdest^:=psrc^;
      inc(pdest);
      inc(psrc);
    end;

  pend:=@_bss_end;
  while pdest<pend do
    begin
      pdest^:=0;
      inc(pdest);
    end;

  PASCALMAIN;
  HaltProc;
end;


procedure LowlevelStartup; assembler; nostackframe; [public, alias: '_START'];
asm
  .weak ResetISR
  .set ResetISR, InitMemAndStart;
  .weak _haltproc
  .set _haltproc, HaltProc

  { Initialize global Pointer }
  .option push
  .option norelax

  lui   gp, %hi(_bss_start+0x800)
  addi  gp, gp, %lo(_bss_start+0x800)
  .option pop

  { Initialize Stack Pointer }
.L1:
  auipc sp, %pcrel_hi(_stack_top)
  addi  sp, sp, %pcrel_lo(.L1)

  { Initialise FP to zero }
  addi  fp, x0, 0

  jal x0, HandleArchSpecificReset
end;
