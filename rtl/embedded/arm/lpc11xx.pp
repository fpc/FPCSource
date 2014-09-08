unit lpc11xx;
{$goto on}
{$define lpc11xx}

interface
{$PACKRECORDS 2}

const
  //------------------------- Cortex-M0 Processor Exceptions Numbers -------------------------
  Reset_IRQn            = -15; // 1 Reset Vector, invoked on Power up and warm reset
  NonMaskableInt_IRQn   = -14; // 2 Non Maskable Interrupt
  HardFault_IRQn        = -13; // 3 Cortex-M0 Hard Fault Interrupt
  SVCall_IRQn           = -5;  // 11 Cortex-M0 SV Call Interrupt
  PendSV_IRQn           = -2;  // 14 Cortex-M0 Pend SV Interrupt
  SysTick_IRQn          = -1;  // 15 Cortex-M0 System Tick Interrupt

  //------------------------- LPC11Cxx or LPC11xx Specific Interrupt Numbers -------------------------
  WAKEUP0_IRQn          = 0;   // All I/O pins can be used as wakeup source.
  WAKEUP1_IRQn          = 1;   // There are 13 pins in total for LPC11xx
  WAKEUP2_IRQn          = 2;
  WAKEUP3_IRQn          = 3;
  WAKEUP4_IRQn          = 4;
  WAKEUP5_IRQn          = 5;
  WAKEUP6_IRQn          = 6;
  WAKEUP7_IRQn          = 7;
  WAKEUP8_IRQn          = 8;
  WAKEUP9_IRQn          = 9;
  WAKEUP10_IRQn         = 10;
  WAKEUP11_IRQn         = 11;
  WAKEUP12_IRQn         = 12;
  CAN_IRQn              = 13;  // CAN Interrupt
  SSP1_IRQn             = 14;  // SSP1 Interrupt
  I2C_IRQn              = 15;  // I2C Interrupt
  TIMER_16_0_IRQn       = 16;  // 16-bit Timer0 Interrupt
  TIMER_16_1_IRQn       = 17;  // 16-bit Timer1 Interrupt
  TIMER_32_0_IRQn       = 18;  // 32-bit Timer0 Interrupt
  TIMER_32_1_IRQn       = 19;  // 32-bit Timer1 Interrupt
  SSP0_IRQn             = 20;  // SSP0 Interrupt
  UART_IRQn             = 21;  // UART Interrupt
  ADC_IRQn              = 24;  // A/D Converter Interrupt
  WDT_IRQn              = 25;  // Watchdog timer Interrupt
  BOD_IRQn              = 26;  // Brown Out Detect(BOD) Interrupt
  FMC_IRQn              = 27;  // Flash Memory Controller Interrupt
  EINT3_IRQn            = 28;  // External Interrupt 3 Interrupt
  EINT2_IRQn            = 29;  // External Interrupt 2 Interrupt
  EIN1_IRQn             = 30;  // External Interrupt 1 Interrupt
  EINT0_IRQn            = 31;  // External Interrupt 0 Interrupt

type
  { ------------- System Control (SYSCON) ------------- }
  TSYSCON_Registers = record
    SYSMEMREMAP  : longword;
    PRESETCTRL   : longword;
    SYSPLLCTRL   : longword;
    SYSPLLSTAT   : longword;
    RESERVED0    : array [0 .. 3] of longword;
    SYSOSCCTRL   : longword;
    WDTOSCCTRL   : longword;
    IRCCTRL      : longword;
    RESERVED1    : longword;
    SYSRSTSTAT   : longword;
    RESERVED2    : array [0 .. 2] of longword;
    SYSPLLCLKSEL : longword;
    SYSPLLCLKUEN : longword;
    RESERVED3    : array [0 .. 9] of longword;
    MAINCLKSEL   : longword;
    MAINCLKUEN   : longword;
    SYSAHBCLKDIV : longword;
    RESERVED4    : longword;
    SYSAHBCLKCTRL: longword;
    RESERVED5    : array [0 .. 3] of longword;
    SSP0CLKDIV   : longword;
    UARTCLKDIV   : longword;
    SSP1CLKDIV   : longword;
    RESERVED6    : array [0 .. 11] of longword;
    WDTCLKSEL    : longword;
    WDTCLKUEN    : longword;
    WDTCLKDIV    : longword;
    RESERVED8    : longword;
    CLKOUTCLKSEL : longword;
    CLKOUTUEN    : longword;
    CLKOUTDIV    : longword;
    RESERVED9    : array [0 .. 4] of longword;
    PIOPORCAP0   : longword;
    PIOPORCAP1   : longword;
    RESERVED10   : array [0 .. 17] of longword;
    BODCTRL      : longword;
    SYSTCKCAL    : longword;
    RESERVED13   : array [0 .. 6] of longword;
    NMISRC       : longword;
    RESERVED14   : array [0 .. 33] of longword;
    STARTAPRP0   : longword;
    STARTERP0    : longword;
    STARTRSRP0CLR: longword;
    STARTSRP0    : longword;
    STARTAPRP1   : longword;
    STARTERP1    : longword;
    STARTRSRP1CLR: longword;
    STARTSRP1    : longword;
    RESERVED17   : array [0 .. 3] of longword;
    PDSLEEPCFG   : longword;
    PDAWAKECFG   : longword;
    PDRUNCFG     : longword;
    RESERVED15   : array [0 .. 109] of longword;
    DEVICE_ID    : longword;
  end;

  { ------------- Pin Connect Block (IOCON) ------------- }
  TIOCON_Registers = record
    PIO2_6       : longword;
    RESERVED0    : longword;
    PIO2_0       : longword;
    RESET_PIO0_0 : longword;
    PIO0_1       : longword;
    PIO1_8       : longword;
    RESERVED1    : longword;
    PIO0_2       : longword;
    PIO2_7       : longword;
    PIO2_8       : longword;
    PIO2_1       : longword;
    PIO0_3       : longword;
    PIO0_4       : longword;
    PIO0_5       : longword;
    PIO1_9       : longword;
    PIO3_4       : longword;
    PIO2_4       : longword;
    PIO2_5       : longword;
    PIO3_5       : longword;
    PIO0_6       : longword;
    PIO0_7       : longword;
    PIO2_9       : longword;
    PIO2_10      : longword;
    PIO2_2       : longword;
    PIO0_8       : longword;
    PIO0_9       : longword;
    SWCLK_PIO0_10: longword;
    PIO1_10      : longword;
    PIO2_11      : longword;
    R_PIO0_11    : longword;
    R_PIO1_0     : longword;
    R_PIO1_1     : longword;
    R_PIO1_2     : longword;
    PIO3_0       : longword;
    PIO3_1       : longword;
    PIO2_3       : longword;
    SWDIO_PIO1_3 : longword;
    PIO1_4       : longword;
    PIO1_11      : longword;
    PIO3_2       : longword;
    PIO1_5       : longword;
    PIO1_6       : longword;
    PIO1_7       : longword;
    PIO3_3       : longword;
    SCK_LOC      : longword;
    DSR_LOC      : longword;
    DCD_LOC      : longword;
    RI_LOC       : longword;
  end;

  { ------------- Power Management Unit (PMU) ------------- }
  TPMU_Registers = record
    PCON  : longword;
    GPREG0: longword;
    GPREG1: longword;
    GPREG2: longword;
    GPREG3: longword;
    GPREG4: longword;
  end;

  { -------------  Flash Controller (FLASHCTRL) ------------- }
  TFLASHCTRL_Registers = record
    RESERVED0: array [0 .. 3] of longword;
    FLASHCFG : longword;
    RESERVED1: array [0 .. 2] of longword;
    FMSSTART : longword;
    FMSSTOP  : longword;
    RESERVED2: longword;
    FMSW0    : longword;
    FMSW1    : longword;
    FMSW2    : longword;
    FMSW3    : longword;
    RESERVED3: array [0 .. 1000] of longword;
    FMSTAT   : longword;
    RESERVED4: longword;
    FMSTATCLR: longword;
  end;

  { ------------- General Purpose Input/Output (GPIO) ------------- }
  TGPIO_Registers = record
    MASKED_ACCESS: array [0 .. 4095] of longword;
    RESERVED1    : array [0 .. 4095] of longword;
    DIR          : longword;
    IS           : longword;
    IBE          : longword;
    IEV          : longword;
    IE           : longword;
    RIS          : longword;
    MIS          : longword;
    IC           : longword;
  end;

  { ------------- Timer (TMR) ------------- }
  TTMR_Registers = record
    IR       : longword;
    TCR      : longword;
    TC       : longword;
    PR       : longword;
    PC       : longword;
    MCR      : longword;
    MR0      : longword;
    MR1      : longword;
    MR2      : longword;
    MR3      : longword;
    CCR      : longword;
    CR0      : longword;
    CR1      : longword;
    RESERVED1: array [0 .. 1] of longword;
    EMR      : longword;
    RESERVED2: array [0 .. 11] of longword;
    CTCR     : longword;
    PWMC     : longword;
  end;

  { ------------- Universal Asynchronous Receiver Transmitter (UART) ------------- }
  TUART_Registers = record
    RBR_THR_DLL: longword;
    DLM_IER    : longword;
    IIR_FCR    : longword;
    LCR        : longword;
    MCR        : longword;
    LSR        : longword;
    MSR        : longword;
    SCR        : longword;
    ACR        : longword;
    RESERVED0  : longword;
    FDR        : longword;
    RESERVED1  : longword;
    TER        : longword;
    RESERVED2  : array [0 .. 5] of longword;
    RS485CTRL  : longword;
    ADRMATCH   : longword;
    RS485DLY   : longword;
    FIFOLVL    : longword;
  end;

  { ------------- Synchronous Serial Communication (SSP) ------------- }
  TSSP_Registers = record
    CR0 : longword;
    CR1 : longword;
    DR  : longword;
    SR  : longword;
    CPSR: longword;
    IMSC: longword;
    RIS : longword;
    MIS : longword;
    ICR : longword;
  end;

  { ------------- Inter-Integrated Circuit (I2C) ------------- }
  TI2C_Registers = record
    CONSET     : longword;
    STAT       : longword;
    DAT        : longword;
    ADR0       : longword;
    SCLH       : longword;
    SCLL       : longword;
    CONCLR     : longword;
    MMCTRL     : longword;
    ADR1       : longword;
    ADR2       : longword;
    ADR3       : longword;
    DATA_BUFFER: longword;
    MASK0      : longword;
    MASK1      : longword;
    MASK2      : longword;
    MASK3      : longword;
  end;

  { ------------- Watchdog Timer (WDT) ------------- }
  TWDT_Registers = record
    _MOD     : longword;
    TC       : longword;
    FEED     : longword;
    TV       : longword;
    RESERVED0: longword;
    WARNINT  : longword;
    WINDOW   : longword;
  end;

  { ------------- Analog-to-Digital Converter (ADC) ------------- }
  TADC_Registers = record
    CR       : longword;
    GDR      : longword;
    RESERVED0: longword;
    INTEN    : longword;
    DR       : array [0 .. 7] of longword;
    STAT     : longword;
  end;

  { ------------- CAN Controller (CAN) ------------- }
  TCAN_Registers = record
    CNTL      : longword;
    STAT      : longword;
    EC        : longword;
    BT        : longword;
    INT       : longword;
    TEST      : longword;
    BRPE      : longword;
    RESERVED0 : longword;
    IF1_CMDREQ: longword;
    IF1_CMDMSK: longword;
    IF1_MSK1  : longword;
    IF1_MSK2  : longword;
    IF1_ARB1  : longword;
    IF1_ARB2  : longword;
    IF1_MCTRL : longword;
    IF1_DA1   : longword;
    IF1_DA2   : longword;
    IF1_DB1   : longword;
    IF1_DB2   : longword;
    RESERVED1 : array [0 .. 12] of longword;
    IF2_CMDREQ: longword;
    IF2_CMDMSK: longword;
    IF2_MSK1  : longword;
    IF2_MSK2  : longword;
    IF2_ARB1  : longword;
    IF2_ARB2  : longword;
    IF2_MCTRL : longword;
    IF2_DA1   : longword;
    IF2_DA2   : longword;
    IF2_DB1   : longword;
    IF2_DB2   : longword;
    RESERVED2 : array [0 .. 20] of longword;
    TXREQ1    : longword;
    TXREQ2    : longword;
    RESERVED3 : array [0 .. 5] of longword;
    ND1       : longword;
    ND2       : longword;
    RESERVED4 : array [0 .. 5] of longword;
    IR1       : longword;
    IR2       : longword;
    RESERVED5 : array [0 .. 5] of longword;
    MSGV1     : longword;
    MSGV2     : longword;
    RESERVED6 : array [0 .. 5] of longword;
    CLKDIV    : longword;
  end;

// ****************************************************************************
// Peripheral memory map
// ****************************************************************************

const
  { Base addresses }
  LPC_FLASH_BASE = ($00000000);
  LPC_RAM_BASE   = ($10000000);
  LPC_APB0_BASE  = ($40000000);
  LPC_AHB_BASE   = ($50000000);

  /// APB peripherals
  LPC_I2C_BASE       = (LPC_APB0_BASE + $00000);
  LPC_WDT_BASE       = (LPC_APB0_BASE + $04000);
  LPC_UART_BASE      = (LPC_APB0_BASE + $08000);
  LPC_CT16B0_BASE    = (LPC_APB0_BASE + $0C000);
  LPC_CT16B1_BASE    = (LPC_APB0_BASE + $10000);
  LPC_CT32B0_BASE    = (LPC_APB0_BASE + $14000);
  LPC_CT32B1_BASE    = (LPC_APB0_BASE + $18000);
  LPC_ADC_BASE       = (LPC_APB0_BASE + $1C000);
  LPC_PMU_BASE       = (LPC_APB0_BASE + $38000);
  LPC_FLASHCTRL_BASE = (LPC_APB0_BASE + $3C000);
  LPC_SSP0_BASE      = (LPC_APB0_BASE + $40000);
  LPC_IOCON_BASE     = (LPC_APB0_BASE + $44000);
  LPC_SYSCON_BASE    = (LPC_APB0_BASE + $48000);
  LPC_CAN_BASE       = (LPC_APB0_BASE + $50000);
  LPC_SSP1_BASE      = (LPC_APB0_BASE + $58000);

  // AHB peripherals
  LPC_GPIO0_BASE = (LPC_AHB_BASE + $00000);
  LPC_GPIO1_BASE = (LPC_AHB_BASE + $10000);
  LPC_GPIO2_BASE = (LPC_AHB_BASE + $20000);
  LPC_GPIO3_BASE = (LPC_AHB_BASE + $30000);

// ****************************************************************************
// Peripheral declaration
// ****************************************************************************
{$ALIGN 2}

var
  LPC_I2C      : TI2C_Registers    absolute(LPC_I2C_BASE);
  LPC_WDT      : TWDT_Registers    absolute(LPC_WDT_BASE);
  LPC_UART     : TUART_Registers   absolute(LPC_UART_BASE);
  LPC_TMR16B0  : TTMR_Registers    absolute(LPC_CT16B0_BASE);
  LPC_TMR16B1  : TTMR_Registers    absolute(LPC_CT16B1_BASE);
  LPC_TMR32B0  : TTMR_Registers    absolute(LPC_CT32B0_BASE);
  LPC_TMR32B1  : TTMR_Registers    absolute(LPC_CT32B1_BASE);
  LPC_ADC      : TADC_Registers    absolute(LPC_ADC_BASE);
  LPC_PMU      : TPMU_Registers    absolute(LPC_PMU_BASE);
  LPC_FLASHCTRL: TPMU_Registers    absolute(LPC_PMU_BASE);
  LPC_SSP0     : TSSP_Registers    absolute(LPC_SSP0_BASE);
  LPC_SSP1     : TSSP_Registers    absolute(LPC_SSP1_BASE);
  LPC_CAN      : TCAN_Registers    absolute(LPC_CAN_BASE);
  LPC_IOCON    : TIOCON_Registers  absolute(LPC_IOCON_BASE);
  LPC_SYSCON   : TSYSCON_Registers absolute(LPC_SYSCON_BASE);

  LPC_GPIO0    : TGPIO_Registers   absolute(LPC_GPIO0_BASE);
  LPC_GPIO1    : TGPIO_Registers   absolute(LPC_GPIO1_BASE);
  LPC_GPIO2    : TGPIO_Registers   absolute(LPC_GPIO2_BASE);
  LPC_GPIO3    : TGPIO_Registers   absolute(LPC_GPIO3_BASE);

implementation

procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
procedure Hardfault_interrupt;      external name 'Hardfault_interrupt';
procedure Startup_Checksum;         external name 'Startup_Checksum';
procedure SVCall_interrupt;         external name 'SVCall_interrupt';
procedure PendSV_interrupt;         external name 'PendSV_interrupt';
procedure SysTick_interrupt;        external name 'SysTick_interrupt';

procedure WAKEUP0_Interrupt;   external name 'WAKEUP0_Interrupt';
procedure WAKEUP1_Interrupt;   external name 'WAKEUP1_Interrupt';
procedure WAKEUP2_Interrupt;   external name 'WAKEUP2_Interrupt';
procedure WAKEUP3_Interrupt;   external name 'WAKEUP3_Interrupt';
procedure WAKEUP4_Interrupt;   external name 'WAKEUP4_Interrupt';
procedure WAKEUP5_Interrupt;   external name 'WAKEUP5_Interrupt';
procedure WAKEUP6_Interrupt;   external name 'WAKEUP6_Interrupt';
procedure WAKEUP7_Interrupt;   external name 'WAKEUP7_Interrupt';
procedure WAKEUP8_Interrupt;   external name 'WAKEUP8_Interrupt';
procedure WAKEUP9_Interrupt;   external name 'WAKEUP9_Interrupt';
procedure WAKEUP10_Interrupt;  external name 'WAKEUP10_Interrupt';
procedure WAKEUP11_Interrupt;  external name 'WAKEUP11_Interrupt';
procedure WAKEUP12_Interrupt;  external name 'WAKEUP12_Interrupt';
procedure CAN_Interrupt;       external name 'CAN_Interrupt';
procedure SSP1_Interrupt;      external name 'SSP1_Interrupt';
procedure I2C_Interrupt;       external name 'I2C_Interrupt';
procedure TIMER16_0_Interrupt; external name 'TIMER16_0_Interrupt';
procedure TIMER16_1_Interrupt; external name 'TIMER16_1_Interrupt';
procedure TIMER32_0_Interrupt; external name 'TIMER32_0_Interrupt';
procedure TIMER32_1_Interrupt; external name 'TIMER32_1_Interrupt';
procedure SSP0_Interrupt;      external name 'SSP0_Interrupt';
procedure UART_Interrupt;      external name 'UART_Interrupt';
procedure ADC_Interrupt;       external name 'ADC_Interrupt';
procedure WDT_Interrupt;       external name 'WDT_Interrupt';
procedure BOD_Interrupt;       external name 'BOD_Interrupt';
procedure FMC_Interrupt;       external name 'FMC_Interrupt';
procedure EINT3_Interrupt;     external name 'EINT3_Interrupt';
procedure EINT2_Interrupt;     external name 'EINT2_Interrupt';
procedure EINT1_Interrupt;     external name 'EINT1_Interrupt';
procedure EINT0_Interrupt;     external name 'EINT0_Interrupt';

{$I cortexm0_start.inc}

procedure Vectors; assembler;
nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
  interrupt_vectors:
  .long _stack_top            // stack top address
  .long Startup
  .long NonMaskableInt_interrupt
  .long Hardfault_interrupt
  .long 0
  .long 0
  .long 0
  .long Startup_Checksum
  .long 0
  .long 0
  .long 0
  .long SVCall_interrupt
  .long 0
  .long 0
  .long PendSV_interrupt
  .long SysTick_interrupt

  .long WAKEUP0_Interrupt
  .long WAKEUP1_Interrupt
  .long WAKEUP2_Interrupt
  .long WAKEUP3_Interrupt
  .long WAKEUP4_Interrupt
  .long WAKEUP5_Interrupt
  .long WAKEUP6_Interrupt
  .long WAKEUP7_Interrupt
  .long WAKEUP8_Interrupt
  .long WAKEUP9_Interrupt
  .long WAKEUP10_Interrupt
  .long WAKEUP11_Interrupt
  .long WAKEUP12_Interrupt
  .long CAN_Interrupt
  .long SSP1_Interrupt
  .long I2C_Interrupt
  .long TIMER16_0_Interrupt
  .long TIMER16_1_Interrupt
  .long TIMER32_0_Interrupt
  .long TIMER32_1_Interrupt
  .long SSP0_Interrupt
  .long UART_Interrupt
  .long 0
  .long 0
  .long ADC_Interrupt
  .long WDT_Interrupt
  .long BOD_Interrupt
  .long FMC_Interrupt
  .long EINT3_Interrupt
  .long EINT2_Interrupt
  .long EINT1_Interrupt
  .long EINT0_Interrupt

  .weak NonMaskableInt_interrupt
  .weak Hardfault_interrupt
  .weak Startup_Checksum
  .weak SVCall_interrupt
  .weak PendSV_interrupt
  .weak SysTick_interrupt
  .weak WAKEUP0_Interrupt
  .weak WAKEUP1_Interrupt
  .weak WAKEUP2_Interrupt
  .weak WAKEUP3_Interrupt
  .weak WAKEUP4_Interrupt
  .weak WAKEUP5_Interrupt
  .weak WAKEUP6_Interrupt
  .weak WAKEUP7_Interrupt
  .weak WAKEUP8_Interrupt
  .weak WAKEUP9_Interrupt
  .weak WAKEUP10_Interrupt
  .weak WAKEUP11_Interrupt
  .weak WAKEUP12_Interrupt
  .weak CAN_Interrupt
  .weak SSP1_Interrupt
  .weak I2C_Interrupt
  .weak TIMER16_0_Interrupt
  .weak TIMER16_1_Interrupt
  .weak TIMER32_0_Interrupt
  .weak TIMER32_1_Interrupt
  .weak SSP0_Interrupt
  .weak UART_Interrupt
  .weak ADC_Interrupt
  .weak WDT_Interrupt
  .weak BOD_Interrupt
  .weak FMC_Interrupt
  .weak EINT3_Interrupt
  .weak EINT2_Interrupt
  .weak EINT1_Interrupt
  .weak EINT0_Interrupt

  .set NonMaskableInt_interrupt, Startup
  .set Hardfault_interrupt     , Startup
  .set SVCall_interrupt        , Startup
  .set PendSV_interrupt        , Startup
  .set SysTick_interrupt       , Startup

  .set WAKEUP0_Interrupt  , Startup
  .set WAKEUP1_Interrupt  , Startup
  .set WAKEUP2_Interrupt  , Startup
  .set WAKEUP3_Interrupt  , Startup
  .set WAKEUP4_Interrupt  , Startup
  .set WAKEUP5_Interrupt  , Startup
  .set WAKEUP6_Interrupt  , Startup
  .set WAKEUP7_Interrupt  , Startup
  .set WAKEUP8_Interrupt  , Startup
  .set WAKEUP9_Interrupt  , Startup
  .set WAKEUP10_Interrupt , Startup
  .set WAKEUP11_Interrupt , Startup
  .set WAKEUP12_Interrupt , Startup
  .set CAN_Interrupt      , Startup
  .set SSP1_Interrupt     , Startup
  .set I2C_Interrupt      , Startup
  .set TIMER16_0_Interrupt, Startup
  .set TIMER16_1_Interrupt, Startup
  .set TIMER32_0_Interrupt, Startup
  .set TIMER32_1_Interrupt, Startup
  .set SSP0_Interrupt     , Startup
  .set UART_Interrupt     , Startup
  .set ADC_Interrupt      , Startup
  .set WDT_Interrupt      , Startup
  .set BOD_Interrupt      , Startup
  .set FMC_Interrupt      , Startup
  .set EINT3_Interrupt    , Startup
  .set EINT2_Interrupt    , Startup
  .set EINT1_Interrupt    , Startup
  .set EINT0_Interrupt    , Startup
  .text
end;

end.
