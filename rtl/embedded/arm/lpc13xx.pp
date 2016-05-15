unit lpc13xx;
{$goto on}
{$define lpc13xx}

interface
{$PACKRECORDS 2}

const
  //------------------------- Cortex-M3 Processor Exceptions Numbers -------------------------
  Reset_IRQn            = -15; // 1 Reset Vector, invoked on Power up and warm reset
  NonMaskableInt_IRQn   = -14; // 2 Non Maskable Interrupt
  MemoryManagement_IRQn = -12; // 4 Cortex-M3 Memory Management Interrupt
  BusFault_IRQn         = -11; // 5 Cortex-M3 Bus Fault Interrupt
  UsageFault_IRQn       = -10; // 6 Cortex-M3 Usage Fault Interrupt
  SVCall_IRQn           = -5;  // 11 Cortex-M3 SV Call Interrupt
  DebugMonitor_IRQn     = -4;  // 12 Cortex-M3 Debug Monitor Interrupt
  PendSV_IRQn           = -2;  // 14 Cortex-M3 Pend SV Interrupt
  SysTick_IRQn          = -1;  // 15 Cortex-M3 System Tick Interrupt

  //------------------------- LPC13xx Specific Interrupt Numbers -------------------------
  WAKEUP0_IRQn          = 0;   // All I/O pins can be used as wakeup source.
  WAKEUP1_IRQn          = 1;   // There are 40 pins in total for LPC17xx
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
  WAKEUP13_IRQn         = 13;
  WAKEUP14_IRQn         = 14;
  WAKEUP15_IRQn         = 15;
  WAKEUP16_IRQn         = 16;
  WAKEUP17_IRQn         = 17;
  WAKEUP18_IRQn         = 18;
  WAKEUP19_IRQn         = 19;
  WAKEUP20_IRQn         = 20;
  WAKEUP21_IRQn         = 21;
  WAKEUP22_IRQn         = 22;
  WAKEUP23_IRQn         = 23;
  WAKEUP24_IRQn         = 24;
  WAKEUP25_IRQn         = 25;
  WAKEUP26_IRQn         = 26;
  WAKEUP27_IRQn         = 27;
  WAKEUP28_IRQn         = 28;
  WAKEUP29_IRQn         = 29;
  WAKEUP30_IRQn         = 30;
  WAKEUP31_IRQn         = 31;
  WAKEUP32_IRQn         = 32;
  WAKEUP33_IRQn         = 33;
  WAKEUP34_IRQn         = 34;
  WAKEUP35_IRQn         = 35;
  WAKEUP36_IRQn         = 36;
  WAKEUP37_IRQn         = 37;
  WAKEUP38_IRQn         = 38;
  WAKEUP39_IRQn         = 39;
  I2C_IRQn              = 40;  // I2C Interrupt
  TIMER_16_0_IRQn       = 41;  // 16-bit Timer0 Interrupt
  TIMER_16_1_IRQn       = 42;  // 16-bit Timer1 Interrupt
  TIMER_32_0_IRQn       = 43;  // 32-bit Timer0 Interrupt
  TIMER_32_1_IRQn       = 44;  // 32-bit Timer1 Interrupt
  SSP_IRQn              = 45;  // SSP0 Interrupt  - original name
  SSP0_IRQn             = 45;  // SSP0 Interrupt  - new name
  UART_IRQn             = 46;  // UART Interrupt
  USB_IRQn              = 47;  // USB Regular Interrupt
  USB_FIQn              = 48;  // USB Fast Interrupt
  ADC_IRQn              = 49;  // A/D Converter Interrupt
  WDT_IRQn              = 50;  // Watchdog timer Interrupt
  BOD_IRQn              = 51;  // Brown Out Detect(BOD) Interrupt
  RESERVED_IRQn         = 52;  // Reserved Interrupt
  EINT3_IRQn            = 53;  // External Interrupt 3 Interrupt
  EINT2_IRQn            = 54;  // External Interrupt 2 Interrupt
  EINT1_IRQn            = 55;  // External Interrupt 1 Interrupt
  EINT0_IRQn            = 56;  // External Interrupt 0 Interrupt
  SSP1_IRQn             = 57;  // SSP1 Interrupt

  { ------------- System Control (SYSCON) ------------- }

type
  TSYSCON_Registers = record
    SYSMEMREMAP  : longword;
    PRESETCTRL   : longword;
    SYSPLLCTRL   : longword;
    SYSPLLSTAT   : longword;
    USBPLLCTRL   : longword;
    USBPLLSTAT   : longword;
    RESERVED0    : array [0 .. 1] of longword;
    SYSOSCCTRL   : longword;
    WDTOSCCTRL   : longword;
    IRCCTRL      : longword;
    RESERVED1    : array [0 .. 0] of longword;
    SYSRESSTAT   : longword;
    RESERVED2    : array [0 .. 2] of longword;
    SYSPLLCLKSEL : longword;
    SYSPLLCLKUEN : longword;
    USBPLLCLKSEL : longword;
    USBPLLCLKUEN : longword;
    RESERVED3    : array [0 .. 7] of longword;
    MAINCLKSEL   : longword;
    MAINCLKUEN   : longword;
    SYSAHBCLKDIV : longword;
    RESERVED4    : array [0 .. 0] of longword;
    SYSAHBCLKCTRL: longword;
    RESERVED5    : array [0 .. 3] of longword;
    SSP0CLKDIV   : longword;
    UARTCLKDIV   : longword;
    SSP1CLKDIV   : longword;
    RESERVED6    : array [0 .. 2] of longword;
    TRACECLKDIV  : longword;
    SYSTICKCLKDIV: longword;
    RESERVED7    : array [0 .. 2] of longword;
    USBCLKSEL    : longword;
    USBCLKUEN    : longword;
    USBCLKDIV    : longword;
    RESERVED8    : array [0 .. 0] of longword;
    WDTCLKSEL    : longword;
    WDTCLKUEN    : longword;
    WDTCLKDIV    : longword;
    RESERVED9    : array [0 .. 0] of longword;
    CLKOUTCLKSEL : longword;
    CLKOUTUEN    : longword;
    CLKOUTDIV    : longword;
    RESERVED10   : array [0 .. 4] of longword;
    PIOPORCAP0   : longword;
    PIOPORCAP1   : longword;
    RESERVED11   : array [0 .. 17] of longword;
    BODCTRL      : longword;
    RESERVED12   : array [0 .. 0] of longword;
    SYSTCKCAL    : longword;
    RESERVED13   : array [0 .. 40] of longword;
    STARTAPRP0   : longword;
    STARTERP0    : longword;
    STARTRSRP0CLR: longword;
    STARTSRP0    : longword;
    STARTAPRP1   : longword;
    STARTERP1    : longword;
    STARTRSRP1CLR: longword;
    STARTSRP1    : longword;
    RESERVED14   : array [0 .. 3] of longword;
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
    RESERVED1: array [0 .. 2] of longword;
    EMR      : longword;
    RESERVED2: array [0 .. 11] of longword;
    CTCR     : longword;
    PWMC     : longword;
  end;

  { ------------- Universal Asynchronous Receiver Transmitter (UART) ------------- }
  TUART_Registers = record
    DLL      : longword;
    DLM      : longword;
    FCR      : longword;
    LCR      : longword;
    MCR      : longword;
    LSR      : longword;
    MSR      : longword;
    SCR      : longword;
    ACR      : longword;
    ICR      : longword;
    FDR      : longword;
    RESERVED0: longword;
    TER      : longword;
    RESERVED1: array [0 .. 5] of longword;
    RS485CTRL: longword;
    ADRMATCH : longword;
    RS485DLY : longword;
    FIFOLVL  : longword;
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

  { ------------- Universal Serial Bus (USB) ------------- }
  TUSB_Registers = record
    DevIntSt : longword;
    DevIntEn : longword;
    DevIntClr: longword;
    DevIntSet: longword;
    CmdCode  : longword;
    CmdData  : longword;
    RxData   : longword;
    TxData   : longword;
    RxPLen   : longword;
    TxPLen   : longword;
    Ctrl     : longword;
    DevFIQSel: longword;
  end;

{ **************************************************************************** }
{ Peripheral memory map }
{ **************************************************************************** }

const
  LPC_FLASH_BASE = $00000000;
  LPC_RAM_BASE   = $10000000;
  LPC_APB0_BASE  = $40000000;
  LPC_AHB_BASE   = $50000000;

  { APB0 peripherals }
  LPC_I2C_BASE    = LPC_APB0_BASE + $00000;
  LPC_WDT_BASE    = LPC_APB0_BASE + $04000;
  LPC_UART_BASE   = LPC_APB0_BASE + $08000;
  LPC_CT16B0_BASE = LPC_APB0_BASE + $0C000;
  LPC_CT16B1_BASE = LPC_APB0_BASE + $10000;
  LPC_CT32B0_BASE = LPC_APB0_BASE + $14000;
  LPC_CT32B1_BASE = LPC_APB0_BASE + $18000;
  LPC_ADC_BASE    = LPC_APB0_BASE + $1C000;
  LPC_USB_BASE    = LPC_APB0_BASE + $20000;
  LPC_PMU_BASE    = LPC_APB0_BASE + $38000;
  LPC_SSP0_BASE   = LPC_APB0_BASE + $40000;
  LPC_IOCON_BASE  = LPC_APB0_BASE + $44000;
  LPC_SYSCON_BASE = LPC_APB0_BASE + $48000;
  LPC_SSP1_BASE   = LPC_APB0_BASE + $58000;

  { AHB peripherals }
  LPC_GPIO0_BASE = LPC_AHB_BASE + $00000;
  LPC_GPIO1_BASE = LPC_AHB_BASE + $10000;
  LPC_GPIO2_BASE = LPC_AHB_BASE + $20000;
  LPC_GPIO3_BASE = LPC_AHB_BASE + $30000;

{ **************************************************************************** }
{ Peripheral declaration }
{ **************************************************************************** }

var
  LPC_I2C      : TI2C_Registers    absolute(LPC_I2C_BASE);
  LPC_WDT      : TWDT_Registers    absolute(LPC_WDT_BASE);
  LPC_UART     : TUART_Registers   absolute(LPC_UART_BASE);
  LPC_CT16B0   : TTMR_Registers     absolute(LPC_CT16B0_BASE);
  LPC_CT16B1   : TTMR_Registers     absolute(LPC_CT16B1_BASE);
  LPC_CT32B0   : TTMR_Registers     absolute(LPC_CT32B0_BASE);
  LPC_CT23B1   : TTMR_Registers     absolute(LPC_CT32B1_BASE);
  LPC_ADC      : TADC_Registers    absolute(LPC_ADC_BASE);
  LPC_USB      : TUSB_Registers    absolute(LPC_USB_BASE);
  LPC_PMU      : TPMU_Registers    absolute(LPC_PMU_BASE);
  LPC_SSP0     : TSSP_Registers    absolute(LPC_SSP0_BASE);
  LPC_IOCON    : TIOCON_Registers  absolute(LPC_IOCON_BASE);
  LPC_SYSCON   : TSYSCON_Registers absolute(LPC_SYSCON_BASE);
  LPC_SSP1     : TSSP_Registers    absolute(LPC_SSP0_BASE);

  LPC_GPIO0    : TGPIO_Registers      absolute(LPC_GPIO0_BASE);
  LPC_GPIO1    : TGPIO_Registers      absolute(LPC_GPIO1_BASE);
  LPC_GPIO2    : TGPIO_Registers      absolute(LPC_GPIO2_BASE);
  LPC_GPIO3    : TGPIO_Registers      absolute(LPC_GPIO3_BASE);

implementation

procedure NonMaskableInt_interrupt;   external name 'NonMaskableInt_interrupt';
procedure MemoryManagement_interrupt; external name 'MemoryManagement_interrupt';
procedure BusFault_interrupt;         external name 'BusFault_interrupt';
procedure UsageFault_interrupt;       external name 'UsageFault_interrupt';
procedure Startup_Checksum;           external name 'Startup_Checksum';
procedure SVCall_interrupt;           external name 'SVCall_interrupt';
procedure DebugMonitor_interrupt;     external name 'DebugMonitor_interrupt';
procedure PendSV_interrupt;           external name 'PendSV_interrupt';
procedure SysTick_interrupt;          external name 'SysTick_interrupt';

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
procedure WAKEUP13_Interrupt;  external name 'WAKEUP13_Interrupt';
procedure WAKEUP14_Interrupt;  external name 'WAKEUP14_Interrupt';
procedure WAKEUP15_Interrupt;  external name 'WAKEUP15_Interrupt';
procedure WAKEUP16_Interrupt;  external name 'WAKEUP16_Interrupt';
procedure WAKEUP17_Interrupt;  external name 'WAKEUP17_Interrupt';
procedure WAKEUP18_Interrupt;  external name 'WAKEUP18_Interrupt';
procedure WAKEUP19_Interrupt;  external name 'WAKEUP19_Interrupt';
procedure WAKEUP20_Interrupt;  external name 'WAKEUP20_Interrupt';
procedure WAKEUP21_Interrupt;  external name 'WAKEUP21_Interrupt';
procedure WAKEUP22_Interrupt;  external name 'WAKEUP22_Interrupt';
procedure WAKEUP23_Interrupt;  external name 'WAKEUP23_Interrupt';
procedure WAKEUP24_Interrupt;  external name 'WAKEUP24_Interrupt';
procedure WAKEUP25_Interrupt;  external name 'WAKEUP25_Interrupt';
procedure WAKEUP26_Interrupt;  external name 'WAKEUP26_Interrupt';
procedure WAKEUP27_Interrupt;  external name 'WAKEUP27_Interrupt';
procedure WAKEUP28_Interrupt;  external name 'WAKEUP28_Interrupt';
procedure WAKEUP29_Interrupt;  external name 'WAKEUP29_Interrupt';
procedure WAKEUP30_Interrupt;  external name 'WAKEUP30_Interrupt';
procedure WAKEUP31_Interrupt;  external name 'WAKEUP31_Interrupt';
procedure WAKEUP32_Interrupt;  external name 'WAKEUP32_Interrupt';
procedure WAKEUP33_Interrupt;  external name 'WAKEUP33_Interrupt';
procedure WAKEUP34_Interrupt;  external name 'WAKEUP34_Interrupt';
procedure WAKEUP35_Interrupt;  external name 'WAKEUP35_Interrupt';
procedure WAKEUP36_Interrupt;  external name 'WAKEUP36_Interrupt';
procedure WAKEUP37_Interrupt;  external name 'WAKEUP37_Interrupt';
procedure WAKEUP38_Interrupt;  external name 'WAKEUP38_Interrupt';
procedure WAKEUP39_Interrupt;  external name 'WAKEUP39_Interrupt';
procedure I2C_Interrupt;       external name 'I2C_Interrupt';
procedure TIMER16_0_Interrupt; external name 'TIMER16_0_Interrupt';
procedure TIMER16_1_Interrupt; external name 'TIMER16_1_Interrupt';
procedure TIMER32_0_Interrupt; external name 'TIMER32_0_Interrupt';
procedure TIMER32_1_Interrupt; external name 'TIMER32_1_Interrupt';
procedure SSP0_Interrupt;      external name 'SSP0_Interrupt';
procedure UART_Interrupt;      external name 'UART_Interrupt';
procedure USB_Interrupt;       external name 'USB_Interrupt';
procedure USB_F_Interrupt;     external name 'USB_F_Interrupt';
procedure ADC_Interrupt;       external name 'ADC_Interrupt';
procedure WDT_Interrupt;       external name 'WDT_Interrupt';
procedure BOD_Interrupt;       external name 'BOD_Interrupt';
procedure EINT3_Interrupt;     external name 'EINT3_Interrupt';
procedure EINT2_Interrupt;     external name 'EINT2_Interrupt';
procedure EINT1_Interrupt;     external name 'EINT1_Interrupt';
procedure EINT0_Interrupt;     external name 'EINT0_Interrupt';
procedure SSP1_Interrupt;      external name 'SSP1_Interrupt';

{$I cortexm3_start.inc}

procedure Vectors; assembler;
nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
  interrupt_vectors:
  .long _stack_top
  .long Startup
  .long NonMaskableInt_interrupt
  .long 0
  .long MemoryManagement_interrupt
  .long BusFault_interrupt
  .long UsageFault_interrupt
  .long Startup_Checksum
  .long 0
  .long 0
  .long 0
  .long SVCall_interrupt
  .long DebugMonitor_interrupt
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
  .long WAKEUP13_Interrupt
  .long WAKEUP14_Interrupt
  .long WAKEUP15_Interrupt
  .long WAKEUP16_Interrupt
  .long WAKEUP17_Interrupt
  .long WAKEUP18_Interrupt
  .long WAKEUP19_Interrupt
  .long WAKEUP20_Interrupt
  .long WAKEUP21_Interrupt
  .long WAKEUP22_Interrupt
  .long WAKEUP23_Interrupt
  .long WAKEUP24_Interrupt
  .long WAKEUP25_Interrupt
  .long WAKEUP26_Interrupt
  .long WAKEUP27_Interrupt
  .long WAKEUP28_Interrupt
  .long WAKEUP29_Interrupt
  .long WAKEUP30_Interrupt
  .long WAKEUP31_Interrupt
  .long WAKEUP32_Interrupt
  .long WAKEUP33_Interrupt
  .long WAKEUP34_Interrupt
  .long WAKEUP35_Interrupt
  .long WAKEUP36_Interrupt
  .long WAKEUP37_Interrupt
  .long WAKEUP38_Interrupt
  .long WAKEUP39_Interrupt
  .long I2C_Interrupt
  .long TIMER16_0_Interrupt
  .long TIMER16_1_Interrupt
  .long TIMER32_0_Interrupt
  .long TIMER32_1_Interrupt
  .long SSP0_Interrupt
  .long UART_Interrupt
  .long USB_Interrupt
  .long USB_F_Interrupt
  .long ADC_Interrupt
  .long WDT_Interrupt
  .long BOD_Interrupt
  .long 0
  .long EINT3_Interrupt
  .long EINT2_Interrupt
  .long EINT1_Interrupt
  .long EINT0_Interrupt
  .long SSP1_Interrupt

  .weak NonMaskableInt_interrupt
  .weak MemoryManagement_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak Startup_Checksum
  .weak SVCall_interrupt
  .weak DebugMonitor_interrupt
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
  .weak WAKEUP13_Interrupt
  .weak WAKEUP14_Interrupt
  .weak WAKEUP15_Interrupt
  .weak WAKEUP16_Interrupt
  .weak WAKEUP17_Interrupt
  .weak WAKEUP18_Interrupt
  .weak WAKEUP19_Interrupt
  .weak WAKEUP20_Interrupt
  .weak WAKEUP21_Interrupt
  .weak WAKEUP22_Interrupt
  .weak WAKEUP23_Interrupt
  .weak WAKEUP24_Interrupt
  .weak WAKEUP25_Interrupt
  .weak WAKEUP26_Interrupt
  .weak WAKEUP27_Interrupt
  .weak WAKEUP28_Interrupt
  .weak WAKEUP29_Interrupt
  .weak WAKEUP30_Interrupt
  .weak WAKEUP31_Interrupt
  .weak WAKEUP32_Interrupt
  .weak WAKEUP33_Interrupt
  .weak WAKEUP34_Interrupt
  .weak WAKEUP35_Interrupt
  .weak WAKEUP36_Interrupt
  .weak WAKEUP37_Interrupt
  .weak WAKEUP38_Interrupt
  .weak WAKEUP39_Interrupt
  .weak I2C_Interrupt
  .weak TIMER16_0_Interrupt
  .weak TIMER16_1_Interrupt
  .weak TIMER32_0_Interrupt
  .weak TIMER32_1_Interrupt
  .weak SSP0_Interrupt
  .weak UART_Interrupt
  .weak USB_Interrupt
  .weak USB_F_Interrupt
  .weak ADC_Interrupt
  .weak WDT_Interrupt
  .weak BOD_Interrupt
  .weak EINT3_Interrupt
  .weak EINT2_Interrupt
  .weak EINT1_Interrupt
  .weak EINT0_Interrupt
  .weak SSP1_Interrupt

  .set NonMaskableInt_interrupt  , Startup
  .set MemoryManagement_interrupt, Startup
  .set BusFault_interrupt        , Startup
  .set UsageFault_interrupt      , Startup
  .set SVCall_interrupt          , Startup
  .set DebugMonitor_interrupt    , Startup
  .set PendSV_interrupt          , Startup
  .set SysTick_interrupt         , Startup

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
  .set WAKEUP13_Interrupt , Startup
  .set WAKEUP14_Interrupt , Startup
  .set WAKEUP15_Interrupt , Startup
  .set WAKEUP16_Interrupt , Startup
  .set WAKEUP17_Interrupt , Startup
  .set WAKEUP18_Interrupt , Startup
  .set WAKEUP19_Interrupt , Startup
  .set WAKEUP20_Interrupt , Startup
  .set WAKEUP21_Interrupt , Startup
  .set WAKEUP22_Interrupt , Startup
  .set WAKEUP23_Interrupt , Startup
  .set WAKEUP24_Interrupt , Startup
  .set WAKEUP25_Interrupt , Startup
  .set WAKEUP26_Interrupt , Startup
  .set WAKEUP27_Interrupt , Startup
  .set WAKEUP28_Interrupt , Startup
  .set WAKEUP29_Interrupt , Startup
  .set WAKEUP30_Interrupt , Startup
  .set WAKEUP31_Interrupt , Startup
  .set WAKEUP32_Interrupt , Startup
  .set WAKEUP33_Interrupt , Startup
  .set WAKEUP34_Interrupt , Startup
  .set WAKEUP35_Interrupt , Startup
  .set WAKEUP36_Interrupt , Startup
  .set WAKEUP37_Interrupt , Startup
  .set WAKEUP38_Interrupt , Startup
  .set WAKEUP39_Interrupt , Startup
  .set I2C_Interrupt      , Startup
  .set TIMER16_0_Interrupt, Startup
  .set TIMER16_1_Interrupt, Startup
  .set TIMER32_0_Interrupt, Startup
  .set TIMER32_1_Interrupt, Startup
  .set SSP0_Interrupt     , Startup
  .set UART_Interrupt     , Startup
  .set USB_Interrupt      , Startup
  .set USB_F_Interrupt    , Startup
  .set ADC_Interrupt      , Startup
  .set WDT_Interrupt      , Startup
  .set BOD_Interrupt      , Startup
  .set EINT3_Interrupt    , Startup
  .set EINT2_Interrupt    , Startup
  .set EINT1_Interrupt    , Startup
  .set EINT0_Interrupt    , Startup
  .set SSP1_Interrupt     , Startup
  .text
end;


end.
