{******************************************************************************
Register definitions and startup code for ATMEL ATmega128


******************************************************************************}
unit atmega128;

{$goto on}

  interface

  implementation

    procedure PASCALMAIN; external name 'PASCALMAIN';

    procedure _FPC_haltproc; assembler; nostackframe; public name '_haltproc';
      asm
        cli
      .Lhalt:
        xjmp .Lhalt
      end;

    var
      _data: record end; external name '_data';
      _edata: record end; external name '_edata';
      _etext: record end; external name '_etext';
      _bss_start: record end; external name '_bss_start';
      _bss_end: record end; external name '_bss_end';
      _stack_top: record end; external name '_stack_top';

    procedure _FPC_start; assembler; nostackframe;
      label
        _start;
      asm
        // code derived from phillips appnote 10254
        .init
        .align 16
        .globl _start
        b   _start
        b   .LUndefined_Addr  // Undefined Instruction vector
        b   .LSWI_Addr        // Software Interrupt vector
        b   .LPrefetch_Addr   // Prefetch abort vector
        b   .LAbort_Addr      // Data abort vector
        nop                   // reserved
        b   .LIRQ_Addr        // Interrupt Request (IRQ) vector
        b   .LFIQ_Addr        // Fast interrupt request (FIQ) vector

    .LUndefined_Addr:
        ldr r0,.L1
        ldr pc,[r0]
    .LSWI_Addr:
        ldr r0,.L2
        ldr pc,[r0]
    .LPrefetch_Addr:
        ldr r0,.L3
        ldr pc,[r0]
    .LAbort_Addr:
        ldr r0,.L4
        ldr pc,[r0]
    .LIRQ_Addr:
        ldr r0,.L5
        ldr pc,[r0]
    .LFIQ_Addr:
        ldr r0,.L5
        ldr pc,[r0]

    .L1:
        .long     Undefined_Handler
    .L2:
        .long     SWI_Handler
    .L3:
        .long     Prefetch_Handler
    .L4:
        .long     Abort_Handler
    .L5:
        .long     IRQ_Handler
    .L6:
        .long     FIQ_Handler

        {
          all ATMEL MCUs use the same startup code, the details are
          governed by defines
        }
        {$i start.inc}
      end;

end.
