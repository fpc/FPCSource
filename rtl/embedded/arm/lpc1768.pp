
unit lpc1768;

{$goto on}
{$define lpc1768}

interface

var
    STCTRL   : DWord absolute $E000E010;
    STRELOAD : DWord absolute $E000E014;
    STCURR   : DWord absolute $E000E018;

    FIO1DIR2 : Byte  absolute $2009C022;
    FIO1SET2 : Byte  absolute $2009C03A;
    FIO1CLR2 : Byte  absolute $2009C03E;

    SCS      : DWord absolute $400FC1A0;
    CLKSRCSEL: DWord absolute $400FC10C;
    PLL0FEED : DWord absolute $400FC08C;
    PLL0CON  : DWord absolute $400FC080;
    PLL0CFG  : DWord absolute $400FC084;
    PLL0STAT : DWord absolute $400FC088;
    CCLKCFG  : DWord absolute $400FC104;

implementation

var
    _data: record end; external name '_data';
    _edata: record end; external name '_edata';
    _etext: record end; external name '_etext';
    _bss_start: record end; external name '_bss_start';
    _bss_end: record end; external name '_bss_end';
    _stack_top: record end; external name '_stack_top';

procedure PASCALMAIN; external name 'PASCALMAIN';

procedure _FPC_haltproc; assembler; nostackframe; public name '_haltproc';
asm
.Lhalt:
    b .Lhalt
end;

procedure _FPC_start; assembler; nostackframe;
label _start;
asm
    .init
    .balign 16

    .long _stack_top            // stack top address
    .long _start+1              // 1 Reset
    .long .LDefaultHandler+1    // 2 NMI
    .long .LDefaultHandler+1    // 3 HardFault
    .long .LDefaultHandler+1    // 4 MemManage
    .long .LDefaultHandler+1    // 5 BusFault
    .long .LDefaultHandler+1    // 6 UsageFault
    .long .LDefaultHandler+1    // 7 RESERVED
    .long .LDefaultHandler+1    // 8 RESERVED
    .long .LDefaultHandler+1    // 9 RESERVED
    .long .LDefaultHandler+1    // 10 RESERVED
    .long .LDefaultHandler+1    // 11 SVCall
    .long .LDefaultHandler+1    // 12 Debug Monitor
    .long .LDefaultHandler+1    // 13 RESERVED
    .long .LDefaultHandler+1    // 14 PendSV
    .long .LDefaultHandler+1    // 15 SysTick
    .long .LDefaultHandler+1    // 16 External Interrupt(0)
    .long .LDefaultHandler+1    // 17 External Interrupt(1)
    .long .LDefaultHandler+1    // 18 External Interrupt(2)
    .long .LDefaultHandler+1    // 19 ...
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1

    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1

    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1

    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1
    .long .LDefaultHandler+1

    .globl _start
    .text
_start:

    // Copy initialized data to ram
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

    b PASCALMAIN
    b _FPC_haltproc

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
.LDefaultHandlerAddr:
    .long .LDefaultHandler
    // default irq handler just returns
.LDefaultHandler:
    mov pc,r14
end;

end.


