  uses
    intrinsics;

    var
      _data: record end; external name '__data_start';
      _edata: record end; external name '__data_end';
      _etext: record end; external name '_etext';
      _bss_start: record end; external name '__bss_start';
      _bss_end: record end; external name '__bss_end';
      _stack_top: record end; external name '_stack_top';
      __dtors_end: record end; external name '__dtors_end';

    procedure PASCALMAIN; external name 'PASCALMAIN';

    procedure _FPC_haltproc; public name '_haltproc';noreturn;
      begin
        avr_cli;
        while true do
          ;
      end;

    procedure Default_IRQ_handler; public name '_Default_IRQ_handler';noreturn;
      begin
        while true do
          ;
      end;

    procedure _FPC_init_zeroreg_SP; public name '_init_zeroreg_SP'; section '.init2';assembler;nostackframe;noreturn;
      asm
    {$ifdef CPUAVR_16_REGS}
        clr r17
    {$else CPUAVR_16_REGS}
        clr r1
    {$endif CPUAVR_16_REGS}

    // AVR1 has a hardware stack, no configuration possible
    {$ifndef CPUAVR1}
        // load stack pointer
        ldi r30,lo8(_stack_top)
        out 0x3d,r30
      {$if declared(SPH) or declared(CPU)}
        ldi r30,hi8(_stack_top)
        out 0x3e,r30
      {$endif declared}
    {$endif CPUAVR1}
      end;

    {$ifndef CPUAVR1}
    procedure _FPC_copy_data; section '.init4';assembler;nostackframe;noreturn;
      asm
        // Initialize .data section
        ldi XL,lo8(_data)
        ldi XH,hi8(_data)
        ldi YH,hi8(_edata)
        ldi ZL,lo8(_etext)
    {$ifdef CPUAVR_16_REGS}
        ldi ZH,hi8(_etext)+(0x40) // program memory mapped to $4000 in data space
    {$else CPUAVR_16_REGS}
        ldi ZH,hi8(_etext)
    {$endif CPUAVR_16_REGS}

        rjmp .LCopyDataLoopEntry
      .LCopyDataLoop:
    {$ifdef CPUAVR_16_REGS}
        ld r16, Z+
    {$else CPUAVR_16_REGS}
        lpm r16, Z+
    {$endif CPUAVR_16_REGS}
        st X+, r16

      .LCopyDataLoopEntry:
        cpi XL, lo8(_edata)
        cpc XH, YH
        brne .LCopyDataLoop

        // Zero .bss section
        ldi XL,lo8(_bss_start)
        ldi XH,hi8(_bss_start)
        ldi YH,hi8(_bss_end)

    {$ifdef RELBRANCHES}
        rjmp .LZeroBssLoopEntry
    {$else RELBRANCHES}
        jmp .LZeroBssLoopEntry
    {$endif RELBRANCHES}
      .LZeroBssLoop:
    {$ifdef CPUAVR_16_REGS}
        st X+, r17
    {$else CPUAVR_16_REGS}
        st X+, r1
    {$endif CPUAVR_16_REGS}

      .LZeroBssLoopEntry:
        cpi XL, lo8(_bss_end)
        cpc XH, YH
        brne .LZeroBssLoop
      end;
    {$endif CPUAVR1}

    procedure _FPC_jmp_main; section '.init9';assembler;nostackframe;noreturn;
      asm
    {$ifdef RELBRANCHES}
        rjmp PASCALMAIN
    {$else RELBRANCHES}
        jmp PASCALMAIN
    {$endif RELBRANCHES}
      end;

