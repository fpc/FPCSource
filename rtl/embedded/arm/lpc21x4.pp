{$goto on}
{******************************************************************************
lpc2114.h - Register defs for Philips LPC2114, LPC2124


THE SOFTWARE IS DELIVERED "AS IS" WITHOUT WARRANTY OR CONDITION OF ANY KIND,
EITHER EXPRESS, IMPLIED OR STATUTORY. THIS INCLUDES WITHOUT LIMITATION ANY
WARRANTY OR CONDITION WITH RESPECT TO MERCHANTABILITY OR FITNESS FOR ANY
PARTICULAR PURPOSE, OR AGAINST THE INFRINGEMENTS OF INTELLECTUAL PROPERTY RIGHTS
OF OTHERS.

This file may be freely used for commercial and non-commercial applications,
including being redistributed with any tools.

If you find a problem with the file, please report it so that it can be fixed.

Created by Sten Larsson (sten_larsson at yahoo com)

Free Pascal conversion by the Free Pascal development team
******************************************************************************}

unit lpc21x4;

  interface

    {##############################################################################
     ## MISC
     ##############################################################################}

    { Constants for data to put in IRQ/FIQ Exception Vectors }
    const
      VECTDATA_IRQ  = dword($E51FFFF0);  {   LDR PC,[PC,#-0xFF0] }
      // VECTDATA_FIQ  {   __TODO }

    type
      TBitvector32 = bitpacked array[0..31] of 0..1;


    {##############################################################################
     ## VECTORED INTERRUPT CONTROLLER
     ##############################################################################}

    var
      VICIRQStatus      : DWord absolute dword($FFFFF000);
      VICFIQStatus      : DWord absolute $FFFFF004;
      VICRawIntr        : DWord absolute $FFFFF008;
      VICIntSelect      : DWord absolute $FFFFF00C;
      VICIntEnable      : DWord absolute $FFFFF010;
      VICIntEnClear     : DWord absolute $FFFFF014;
      VICSoftInt        : DWord absolute $FFFFF018;
      VICSoftIntClear   : DWord absolute $FFFFF01C;
      VICProtection     : DWord absolute $FFFFF020;
      VICVectAddr       : DWord absolute $FFFFF030;
      VICDefVectAddr    : DWord absolute $FFFFF034;

      VICVectAddr0  : DWord absolute $FFFFF100;
      VICVectAddr1  : DWord absolute $FFFFF104;
      VICVectAddr2  : DWord absolute $FFFFF108;
      VICVectAddr3  : DWord absolute $FFFFF10C;
      VICVectAddr4  : DWord absolute $FFFFF110;
      VICVectAddr5  : DWord absolute $FFFFF114;
      VICVectAddr6  : DWord absolute $FFFFF118;
      VICVectAddr7  : DWord absolute $FFFFF11C;
      VICVectAddr8  : DWord absolute $FFFFF120;
      VICVectAddr9  : DWord absolute $FFFFF124;
      VICVectAddr10 : DWord absolute $FFFFF128;
      VICVectAddr11 : DWord absolute $FFFFF12C;
      VICVectAddr12 : DWord absolute $FFFFF130;
      VICVectAddr13 : DWord absolute $FFFFF134;
      VICVectAddr14 : DWord absolute $FFFFF138;
      VICVectAddr15 : DWord absolute $FFFFF13C;

      VICVectCntl0  : DWord absolute $FFFFF200;
      VICVectCntl1  : DWord absolute $FFFFF204;
      VICVectCntl2  : DWord absolute $FFFFF208;
      VICVectCntl3  : DWord absolute $FFFFF20C;
      VICVectCntl4  : DWord absolute $FFFFF210;
      VICVectCntl5  : DWord absolute $FFFFF214;
      VICVectCntl6  : DWord absolute $FFFFF218;
      VICVectCntl7  : DWord absolute $FFFFF21C;
      VICVectCntl8  : DWord absolute $FFFFF220;
      VICVectCntl9  : DWord absolute $FFFFF224;
      VICVectCntl10 : DWord absolute $FFFFF228;
      VICVectCntl11 : DWord absolute $FFFFF22C;
      VICVectCntl12 : DWord absolute $FFFFF230;
      VICVectCntl13 : DWord absolute $FFFFF234;
      VICVectCntl14 : DWord absolute $FFFFF238;
      VICVectCntl15 : DWord absolute $FFFFF23C;

      VICITCR       : DWord absolute $FFFFF300;
      VICITIP1      : DWord absolute $FFFFF304;
      VICITIP2      : DWord absolute $FFFFF308;
      VICITOP1      : DWord absolute $FFFFF30C;
      VICITOP2      : DWord absolute $FFFFF310;
      VICPeriphID0  : DWord absolute $FFFFFFE0;
      VICPeriphID1  : DWord absolute $FFFFFFE4;
      VICPeriphID2  : DWord absolute $FFFFFFE8;
      VICPeriphID3  : DWord absolute $FFFFFFEC;

      VICIntEnClr   : DWord absolute $FFFFF014;
      VICSoftIntClr : DWord absolute $FFFFF01C;


    {##############################################################################
     ## PCB - Pin Connect Block
     ##############################################################################}

      PCB_PINSEL0       : DWord absolute $E002C000;
      PCB_PINSEL1       : DWord absolute $E002C004;
      PCB_PINSEL2       : DWord absolute $E002C014;


    {##############################################################################
     ## GPIO - General Purpose I/O
     ##############################################################################}

      GPIO0_IOPIN       : DWord absolute $E0028000;
      GPIO0_IOSET       : DWord absolute $E0028004;
      GPIO0_IODIR       : DWord absolute $E0028008;
      GPIO0_IOCLR       : DWord absolute $E002800C;

      GPIO1_IOPIN       : DWord absolute $E0028010;
      GPIO1_IOSET       : DWord absolute $E0028014;
      GPIO1_IODIR       : DWord absolute $E0028018;
      GPIO1_IOCLR       : DWord absolute $E002801C;


    {##############################################################################
     ## UART0 / UART1
     ##############################################################################}

{ ---- UART 0 --------------------------------------------- }
      UART0_RBR     : DWord absolute $E000C000;
      UART0_THR     : DWord absolute $E000C000;
      UART0_IER     : DWord absolute $E000C004;
      UART0_IIR     : DWord absolute $E000C008;
      UART0_FCR     : DWord absolute $E000C008;
      UART0_LCR     : DWord absolute $E000C00C;
      UART0_LSR     : DWord absolute $E000C014;
      UART0_SCR     : DWord absolute $E000C01C;
      UART0_DLL     : DWord absolute $E000C000;
      UART0_DLM     : DWord absolute $E000C004;

{ ---- UART 1 --------------------------------------------- }
      UART1_RBR     : DWord absolute $E0010000;
      UART1_THR     : DWord absolute $E0010000;
      UART1_IER     : DWord absolute $E0010004;
      UART1_IIR     : DWord absolute $E0010008;
      UART1_FCR     : DWord absolute $E0010008;
      UART1_LCR     : DWord absolute $E001000C;
      UART1_LSR     : DWord absolute $E0010014;
      UART1_SCR     : DWord absolute $E001001C;
      UART1_DLL     : DWord absolute $E0010000;
      UART1_DLM     : DWord absolute $E0010004;
      UART1_MCR     : DWord absolute $E0010010;
      UART1_MSR     : DWord absolute $E0010018;


    {##############################################################################
     ## I2C
     ##############################################################################}

      I2C_I2CONSET  : DWord absolute $E001C000;
      I2C_I2STAT    : DWord absolute $E001C004;
      I2C_I2DAT     : DWord absolute $E001C008;
      I2C_I2ADR     : DWord absolute $E001C00C;
      I2C_I2SCLH    : DWord absolute $E001C010;
      I2C_I2SCLL    : DWord absolute $E001C014;
      I2C_I2CONCLR  : DWord absolute $E001C018;


    {##############################################################################
     ## SPI - Serial Peripheral Interface
     ##############################################################################}

      SPI_SPCR      : DWord absolute $E0020000;  { SPI = SPI0 }
      SPI_SPSR      : DWord absolute $E0020004;
      SPI_SPDR      : DWord absolute $E0020008;
      SPI_SPCCR     : DWord absolute $E002000C;
      SPI_SPTCR     : DWord absolute $E0020010;
      SPI_SPTSR     : DWord absolute $E0020014;
      SPI_SPTOR     : DWord absolute $E0020018;
      SPI_SPINT     : DWord absolute $E002001C;

      SPI0_SPCR     : DWord absolute $E0020000;  { SPI = SPI0 }
      SPI0_SPSR     : DWord absolute $E0020004;
      SPI0_SPDR     : DWord absolute $E0020008;
      SPI0_SPCCR        : DWord absolute $E002000C;
      SPI0_SPTCR        : DWord absolute $E0020010;
      SPI0_SPTSR        : DWord absolute $E0020014;
      SPI0_SPTOR        : DWord absolute $E0020018;
      SPI0_SPINT        : DWord absolute $E002001C;

      SPI1_SPCR     : DWord absolute $E0030000;
      SPI1_SPSR     : DWord absolute $E0030004;
      SPI1_SPDR     : DWord absolute $E0030008;
      SPI1_SPCCR        : DWord absolute $E003000C;
      SPI1_SPTCR        : DWord absolute $E0030010;
      SPI1_SPTSR        : DWord absolute $E0030014;
      SPI1_SPTOR        : DWord absolute $E0030018;
      SPI1_SPINT        : DWord absolute $E003001C;


    {##############################################################################
     ## Timer 0 and Timer 1
     ##############################################################################}

    { ---- Timer 0 -------------------------------------------- }
      T0_IR         : DWord absolute $E0004000;
      T0_TCR        : DWord absolute $E0004004;
      T0_TC         : DWord absolute $E0004008;
      T0_PR         : DWord absolute $E000400C;
      T0_PC         : DWord absolute $E0004010;
      T0_MCR            : DWord absolute $E0004014;
      T0_MR0            : DWord absolute $E0004018;
      T0_MR1            : DWord absolute $E000401C;
      T0_MR2            : DWord absolute $E0004020;
      T0_MR3            : DWord absolute $E0004024;
      T0_CCR            : DWord absolute $E0004028;
      T0_CR0            : DWord absolute $E000402C;
      T0_CR1            : DWord absolute $E0004030;
      T0_CR2            : DWord absolute $E0004034;
      T0_CR3            : DWord absolute $E0004038;
      T0_EMR            : DWord absolute $E000403C;

    { ---- Timer 1 -------------------------------------------- }
      T1_IR         : DWord absolute $E0008000;
      T1_TCR        : DWord absolute $E0008004;
      T1_TC         : DWord absolute $E0008008;
      T1_PR         : DWord absolute $E000800C;
      T1_PC         : DWord absolute $E0008010;
      T1_MCR            : DWord absolute $E0008014;
      T1_MR0            : DWord absolute $E0008018;
      T1_MR1            : DWord absolute $E000801C;
      T1_MR2            : DWord absolute $E0008020;
      T1_MR3            : DWord absolute $E0008024;
      T1_CCR            : DWord absolute $E0008028;
      T1_CR0            : DWord absolute $E000802C;
      T1_CR1            : DWord absolute $E0008030;
      T1_CR2            : DWord absolute $E0008034;
      T1_CR3            : DWord absolute $E0008038;
      T1_EMR            : DWord absolute $E000803C;


    {##############################################################################
     ## PWM
     ##############################################################################}

      PWM_IR            : DWord absolute $E0014000;
      PWM_TCR           : DWord absolute $E0014004;
      PWM_TC            : DWord absolute $E0014008;
      PWM_PR            : DWord absolute $E001400C;
      PWM_PC            : DWord absolute $E0014010;
      PWM_MCR           : DWord absolute $E0014014;
      PWM_MR0           : DWord absolute $E0014018;
      PWM_MR1           : DWord absolute $E001401C;
      PWM_MR2           : DWord absolute $E0014020;
      PWM_MR3           : DWord absolute $E0014024;
      PWM_MR4           : DWord absolute $E0014040;
      PWM_MR5           : DWord absolute $E0014044;
      PWM_MR6           : DWord absolute $E0014048;
      PWM_EMR           : DWord absolute $E001403C;
      PWM_PCR           : DWord absolute $E001404C;
      PWM_LER           : DWord absolute $E0014050;
      PWM_CCR           : DWord absolute $E0014028;
      PWM_CR0           : DWord absolute $E001402C;
      PWM_CR1           : DWord absolute $E0014030;
      PWM_CR2           : DWord absolute $E0014034;
      PWM_CR3           : DWord absolute $E0014038;

    {##############################################################################
     ## RTC
     ##############################################################################}

    { ---- RTC: Miscellaneous Register Group ------------------ }
      RTC_ILR           : DWord absolute $E0024000;
      RTC_CTC           : DWord absolute $E0024004;
      RTC_CCR           : DWord absolute $E0024008;
      RTC_CIIR          : DWord absolute $E002400C;
      RTC_AMR           : DWord absolute $E0024010;
      RTC_CTIME0        : DWord absolute $E0024014;
      RTC_CTIME1        : DWord absolute $E0024018;
      RTC_CTIME2        : DWord absolute $E002401C;

    { ---- RTC: Timer Control Group --------------------------- }
      RTC_SEC           : DWord absolute $E0024020;
      RTC_MIN           : DWord absolute $E0024024;
      RTC_HOUR          : DWord absolute $E0024028;
      RTC_DOM           : DWord absolute $E002402C;
      RTC_DOW           : DWord absolute $E0024030;
      RTC_DOY           : DWord absolute $E0024034;
      RTC_MONTH         : DWord absolute $E0024038;
      RTC_YEAR          : DWord absolute $E002403C;

    { ---- RTC: Alarm Control Group --------------------------- }
      RTC_ALSEC     : DWord absolute $E0024060;
      RTC_ALMIN     : DWord absolute $E0024064;
      RTC_ALHOUR    : DWord absolute $E0024068;
      RTC_ALDOM     : DWord absolute $E002406C;
      RTC_ALDOW     : DWord absolute $E0024070;
      RTC_ALDOY     : DWord absolute $E0024074;
      RTC_ALMON     : DWord absolute $E0024078;
      RTC_ALYEAR    : DWord absolute $E002407C;

    { ---- RTC: Reference Clock Divider Group ----------------- }
      RTC_PREINT        : DWord absolute $E0024080;
      RTC_PREFRAC       : DWord absolute $E0024084;


    {##############################################################################
     ## AE - AD Converter
     ##############################################################################}

      AD_ADCR          : DWord absolute $E0034000;
      AD_ADDR          : DWord absolute $E0034004;


    {##############################################################################
     ## WD - Watchdog
     ##############################################################################}

      WD_WDMOD      : DWord absolute $E0000000;
      WD_WDTC       : DWord absolute $E0000004;
      WD_WDFEED     : DWord absolute $E0000008;
      WD_WDTV       : DWord absolute $E000000C;


    {##############################################################################
     ## SCB - System Control Block
     ##############################################################################}

      SCB_EXTINT        : DWord absolute $E01FC140;
      SCB_EXTWAKE       : DWord absolute $E01FC144;
      SCB_EXTMODE       : DWord absolute $E01FC148;
      SCB_EXTPOLAR      : DWord absolute $E01FC14C;
      SCB_MEMMAP        : DWord absolute $E01FC040;
      SCB_PLLCON        : DWord absolute $E01FC080;
      SCB_PLLCFG        : DWord absolute $E01FC084;
      SCB_PLLSTAT       : DWord absolute $E01FC088;
      SCB_PLLFEED       : DWord absolute $E01FC08C;
      SCB_PCON          : DWord absolute $E01FC0C0;
      SCB_PCONP         : DWord absolute $E01FC0C4;
      SCB_VPBDIV        : DWord absolute $E01FC100;


    {##############################################################################
     ## MAM - Memory Accelerator Module
     ##############################################################################}

      MAM_MAMCR         : DWord absolute $E01FC000;
      MAM_MAMTIM        : DWord absolute $E01FC004;
      MAM_MAMMAP        : DWord absolute $E01FC040;

    var
      Undefined_Handler,
      SWI_Handler,
      Prefetch_Handler,
      Abort_Handler,
      FIQ_Handler : pointer;

    type
      tm = 1..32;
      tp = 1..8;

    procedure InitPLL(m : tm;p : tp);
    procedure PLLFeed;
    function GetProcessorClock(CrystalFrequency : DWord) : DWord;

  implementation

    procedure PLLFeed;
      begin
        SCB_PLLFEED:=$aa;
        SCB_PLLFEED:=$55;
      end;


    function GetProcessorClock(CrystalFrequency : DWord) : DWord;
      begin
        if (TBitvector32(SCB_PLLSTAT)[8] and 1)<>0 then
          GetProcessorClock:=((SCB_PLLSTAT and $f)+1)*CrystalFrequency
        else
          GetProcessorClock:=CrystalFrequency;
      end;


    procedure InitPLL(m : tm;p : tp);
      begin
        case p of
          1: p:=0;
          2..3: p:=1;
          4..7: p:=2;
          8: p:=3;
        end;
        { set p and m }
        SCB_PLLCFG:=(m-1) or (p shl 5);

        { write changes }
        PLLFeed;

        { start PLL }
        TBitvector32(SCB_PLLCON)[0]:=1;

        { write changes }
        PLLFeed;

        { wait for pll sync }
        while TBitvector32(SCB_PLLSTAT)[10]=0 do
          ;

        { connect PLL }
        TBitvector32(SCB_PLLCON)[1]:=1;

        { write changes }
        PLLFeed;
      end;


    procedure PASCALMAIN; external name 'PASCALMAIN';

    procedure _FPC_haltproc; assembler; nostackframe; public name '_haltproc';
      asm
      .Lhalt:
        b .Lhalt
      end;


    procedure _FPC_start; assembler; nostackframe;
      label
        _start;
      asm
        // code derived from phillips appnote 10254
        .init
        .align 16
        .globl _start
        b   _start
        ldr pc, .L1
        ldr pc, .L2
        ldr pc, .L3
        ldr pc, .L4

        // signature
        nop
        ldr pc, [pc, #-0xFF0] // load irq vector from vic
        ldr pc, .L5
(*
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
    .LFIQ_Addr:
        ldr r0,.L5
        ldr pc,[r0]
*)
    .L1:
        .long     Undefined_Handler
    .L2:
        .long     SWI_Handler
    .L3:
        .long     Prefetch_Handler
    .L4:
        .long     Abort_Handler
    .L5:
        .long     FIQ_Handler

    _start:
        (*
          Set SP for Supervisor mode. Depending upon
          the stack the application needs this value
          needs to be set.
          stack is already set by bootloader
          but if this point is entered by any
          other means than reset, the stack pointer
          needs to be set explicity
        *)
        // LDR SP,=0x40001000

        (*
          Setting up SP for IRQ and FIQ mode.
          Change mode before setting each one
          move back again to Supervisor mode
          Each interrupt has its own link
          register, stack pointer and program
          counter The stack pointers must be
          initialized for interrupts to be
          used later.
        *)

        (*
          setup for fiq and irq interrupt stacks to run
          below current stack by 1000.
        *)
        mov r0, sp         // copy current stack pointer
        sub r0, r0, #1000  // make irq stack pointer
        sub r1, r0, #1000  // make fiq stack pointer
        msr cpsr_c, #0x12  // switch to irq mode
        mov sp, r0         // set irq stack pointer
        msr cpsr_c, #0x11  // fiq mode
        mov sp, r1         // set fiq stack pointer
        msr cpsr_c, #0x13  // supervisor mode F,I enabled

        ldr r1,.LDefaultHandlerAddr
        ldr r0,.L1
        str r1,[r0]
        ldr r0,.L2
        str r1,[r0]
        ldr r0,.L3
        str r1,[r0]
        ldr r0,.L4
        str r1,[r0]
        ldr r0,.L5
        str r1,[r0]

        // clear onboard ram
        mov r1,#0x1000
        ldr r2,.LRAMStart
        mov r0,#0
.Lzeroloop:
        str r0,[r2]
        subs r1,r1,#1
        add r2,r2,#4
        bne .Lzeroloop

        bl PASCALMAIN
        bl _FPC_haltproc
.LRAMStart:
        .long 0x40000000
.LDefaultHandlerAddr:
        .long .LDefaultHandler
        // default irq handler just returns
.LDefaultHandler:
        mov pc,r14
        .text
      end;

end.
