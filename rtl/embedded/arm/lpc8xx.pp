unit lpc8xx;
{$goto on}
{$define lpc8xx}

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

  //------------------------- LPC8xx Specific Interrupt Numbers -------------------------
  SPI0_IRQn             = 0;   // SPI0
  SPI1_IRQn             = 1;   // SPI1
  UART0_IRQn            = 3;   // USART0
  UART1_IRQn            = 4;   // USART1
  UART2_IRQn            = 5;   // USART2
  I2C_IRQn              = 8;   // I2C
  SCT_IRQn              = 9;   // SCT
  MRT_IRQn              = 10;  // MRT
  CMP_IRQn              = 11;  // CMP
  WDT_IRQn              = 12;  // WDT
  BOD_IRQn              = 13;  // BOD
  WKT_IRQn              = 15;  // WKT Interrupt
  PININT0_IRQn          = 24;  // External Interrupt 0
  PININT1_IRQn          = 25;  // External Interrupt 1
  PININT2_IRQn          = 26;  // External Interrupt 2
  PININT3_IRQn          = 27;  // External Interrupt 3
  PININT4_IRQn          = 28;  // External Interrupt 4
  PININT5_IRQn          = 29;  // External Interrupt 5
  PININT6_IRQn          = 30;  // External Interrupt 6
  PININT7_IRQn          = 31;  // External Interrupt 7

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
    RESERVED1    : array [0 .. 1] of longword;
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
    UARTCLKDIV   : longword;
    RESERVED6    : array [0 .. 17] of longword;
    CLKOUTSEL    : longword;
    CLKOUTUEN    : longword;
    CLKOUTDIV    : longword;
    RESERVED7    : longword;
    UARTFRGDIV   : longword;
    UARTFRGMULT  : longword;
    RESERVED8    : longword;
    EXTTRACECMD  : longword;
    PIOPORCAP0   : longword;
    RESERVED9    : array [0 .. 11] of longword;
    IOCONCLKDIV  : array [0 .. 6] of longword;
    BODCTRL      : longword;
    SYSTCKCAL    : longword;
    RESERVED10   : array [0 .. 5] of longword;
    IRQLATENCY   : longword;
    NMISRC       : longword;
    PINTSEL      : array [0 .. 7] of longword;
    RESERVED11   : array [0 .. 26] of longword;
    STARTERP0    : longword;
    RESERVED12   : array [0 .. 2] of longword;
    STARTERP1    : longword;
    RESERVED13   : array [0 .. 5] of longword;
    PDSLEEPCFG   : longword;
    PDAWAKECFG   : longword;
    PDRUNCFG     : longword;
    RESERVED14   : array [0 .. 109] of longword;
    DEVICE_ID    : longword;
  end;

  { ------------- Pin Connect Block (IOCON) ------------- }
  TIOCON_Registers = record
    PIO0_17  : longword;
    PIO0_13  : longword;
    PIO0_12  : longword;
    PIO0_5   : longword;
    PIO0_4   : longword;
    PIO0_3   : longword;
    PIO0_2   : longword;
    PIO0_11  : longword;
    PIO0_10  : longword;
    PIO0_16  : longword;
    PIO0_15  : longword;
    PIO0_1   : longword;
    RESERVED0: longword;
    PIO0_9   : longword;
    PIO0_8   : longword;
    PIO0_7   : longword;
    PIO0_6   : longword;
    PIO0_0   : longword;
    PIO0_14  : longword;
  end;

  { ------------- Flash Controller (FLASHCTRL) ------------- }
  TFLASHCTRL_Registers = record
    RESERVED0: array [0 .. 3] of longword;
    FLASHCFG : longword;
    RESERVED1: array [0 .. 2] of longword;
    FMSSTART : longword;
    FMSSTOP  : longword;
    RESERVED2: longword;
    FMSW0    : longword;
  end;

  { ------------- Power Management Unit (PMU) ------------- }
  TPMU_Registers = record
    PCON   : longword;
    GPREG0 : longword;
    GPREG1 : longword;
    GPREG2 : longword;
    GPREG3 : longword;
    DPDCTRL: longword;
  end;

  { ------------- Switch Matrix Register (SWM) ------------- }
  TSWM_Registers = record
    PINASSIGN : array [0 .. 8] of longword;
    RESERVED0 : array [0 .. 102] of longword;
    PINENABLE0: longword;
  end;

  { ------------- General Purpose Input/Output (GPIO) ------------- }
  TGPIOPORT_Registers = record
    B0       : array [0 .. 17] of byte;
    RESERVED0: array [0 .. 2038] of word;
    W0       : array [0 .. 17] of longword;
    RESERVED1: array [0 .. 1005] of longword;
    DIR0     : longword;
    RESERVED2: array [0 .. 30] of longword;
    MASK0    : longword;
    RESERVED3: array [0 .. 30] of longword;
    PIN0     : longword;
    RESERVED4: array [0 .. 30] of longword;
    MPIN0    : longword;
    RESERVED5: array [0 .. 30] of longword;
    SET0     : longword;
    RESERVED6: array [0 .. 30] of longword;
    CLR0     : longword;
    RESERVED7: array [0 .. 30] of longword;
    NOT0     : longword;
  end;

  { -------------  Pin interrupts/pattern match engine (PIN_INT) ------------- }
  TPININT_Registers = record
    ISEL  : longword;
    IENR  : longword;
    SIENR : longword;
    CIENR : longword;
    IENF  : longword;
    SIENF : longword;
    CIENF : longword;
    RISE  : longword;
    FALL  : longword;
    IST   : longword;
    PMCTRL: longword;
    PMSRC : longword;
    PMCFG : longword;
  end;

  { -------------  CRC Engine (CRC) -------------  }
  TCRC_Registers = record
    MODE: longword;
    SEED: longword;
    SUM : longword;
  end;

  { -------------  Comparator (CMP) -------------  }
  TCMP_Registers = record
    CTRL: longword;
    LAD : longword;
  end;

  { -------------  Wakeup Timer (WKT) -------------  }
  TWKT_Registers = record
    CTRL     : longword;
    RESERVED0: array [0 .. 1] of longword;
    COUNT    : longword;
  end;

  { -------------  Multi-Rate Timer(MRT) -------------  }
  TMRTChannel = record
    INTVAL: longword;
    TIMER : longword;
    CTRL  : longword;
    STAT  : longword;
  end;

  TMRT_Registers = record
    CHANNEL  : array [0 .. 3] of TMRTChannel;
    RESERVED0: array [0 .. 0] of longword;
    IDLE_CH  : longword;
    IRQ_FLAG : longword;
  end;

  { ------------- Universal Asynchronous Receiver Transmitter (USART) -------------  }
  TUSART_Registers = record
    CFG        : longword;
    CTRL       : longword;
    STAT       : longword;
    INTENSET   : longword;
    INTENCLR   : longword;
    RXDATA     : longword;
    RXDATA_STAT: longword;
    TXDATA     : longword;
    BRG        : longword;
    INTSTAT    : longword;
  end;

  { ------------- Synchronous Serial Interface Controller (SPI) -------------  }
  TSPI_Registers = record
    CFG     : longword;
    DLY     : longword;
    STAT    : longword;
    INTENSET: longword;
    INTENCLR: longword;
    RXDAT   : longword;
    TXDATCTL: longword;
    TXDAT   : longword;
    TXCTRL  : longword;
    _DIV    : longword;
    INTSTAT : longword;
  end;

  { ------------- Inter-Integrated Circuit (I2C) ------------- }
  TI2C_Registers = record
    CFG      : longword;
    STAT     : longword;
    INTENSET : longword;
    INTENCLR : longword;
    TIMEOUT  : longword;
    _DIV     : longword;
    INTSTAT  : longword;
    RESERVED0: longword;
    MSTCTL   : longword;
    MSTTIME  : longword;
    MSTDAT   : longword;
    RESERVED1: array [0 .. 4] of longword;
    SLVCTL   : longword;
    SLVDAT   : longword;
    SLVADR0  : longword;
    SLVADR1  : longword;
    SLVADR2  : longword;
    SLVADR3  : longword;
    SLVQUAL0 : longword;
    RESERVED2: array [0 .. 8] of longword;
    MONRXDAT : longword;
  end;

  { ------------- State Configurable Timer (SCT) ------------- }
const
  CONFIG_SCT_nEV = 6;
  { Number of match/compare registers }
  CONFIG_SCT_nRG = 5;
  { Number of outputs }
  CONFIG_SCT_nOU = 4;

type
  TSCTState = record
    STATE : longword;
    CTRL  : longword;
  end;
  TSCTSet = record
    _SET : longword;
    CLR  : longword;
  end;

type
  TSCT_Registers = record
    CONFIG          : longword;
    CTRL            : longword;
    LIMIT           : longword;
    HALT            : longword;
    STOP            : longword;
    START           : longword;
    RESERVED1       : array [0 .. 9] of longword;
    COUNT           : longword;
    STATE           : longword;
    INPUT           : longword;
    REGMODE         : longword;
    OUTPUT          : longword;
    OUTPUTDIRCTRL   : longword;
    RES             : longword;
    RESERVED2       : array [0 .. 36] of longword;
    EVEN            : longword;
    EVFLAG          : longword;
    CONEN           : longword;
    CONFLAG         : longword;
    MATCH_CAP       : array [0 .. (CONFIG_SCT_nRG) - 1] of longword;
    RESERVED3       : array [0 .. (32 - CONFIG_SCT_nRG) - 1] of longword;
    MATCHREL_CAPCTRL: array [0 .. (CONFIG_SCT_nRG) - 1] of longword;
    RESERVED6       : array [0 .. (32 - CONFIG_SCT_nRG) - 1] of longword;
    EVENT           : array [0 .. (CONFIG_SCT_nEV) - 1] of TSCTState;
    RESERVED9       : array [0 .. (128 - (2 * CONFIG_SCT_nEV)) - 1] of longword;
    _OUT            : array [0 .. (CONFIG_SCT_nOU) - 1] of TSCTSet;
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

{ **************************************************************************** }
{ Peripheral memory map }
{ **************************************************************************** }

const
  { Base addresses }
  LPC_FLASH_BASE = $00000000;
  LPC_RAM_BASE   = $10000000;
  LPC_ROM_BASE   = $1FFF0000;
  LPC_APB0_BASE  = $40000000;
  LPC_AHB_BASE   = $50000000;
  { APB0 peripherals }
  LPC_WDT_BASE       = LPC_APB0_BASE + $00000;
  LPC_MRT_BASE       = LPC_APB0_BASE + $04000;
  LPC_WKT_BASE       = LPC_APB0_BASE + $08000;
  LPC_SWM_BASE       = LPC_APB0_BASE + $0C000;
  LPC_PMU_BASE       = LPC_APB0_BASE + $20000;
  LPC_CMP_BASE       = LPC_APB0_BASE + $24000;
  LPC_FLASHCTRL_BASE = LPC_APB0_BASE + $40000;
  LPC_IOCON_BASE     = LPC_APB0_BASE + $44000;
  LPC_SYSCON_BASE    = LPC_APB0_BASE + $48000;
  LPC_I2C_BASE       = LPC_APB0_BASE + $50000;
  LPC_SPI0_BASE      = LPC_APB0_BASE + $58000;
  LPC_SPI1_BASE      = LPC_APB0_BASE + $5C000;
  LPC_USART0_BASE    = LPC_APB0_BASE + $64000;
  LPC_USART1_BASE    = LPC_APB0_BASE + $68000;
  LPC_USART2_BASE    = LPC_APB0_BASE + $6C000;
  { AHB peripherals }
  LPC_CRC_BASE       = LPC_AHB_BASE + $00000;
  LPC_SCT_BASE       = LPC_AHB_BASE + $04000;
  LPC_GPIO_PORT_BASE = $A0000000;
  LPC_PIN_INT_BASE   = LPC_GPIO_PORT_BASE + $4000;

// ****************************************************************************
// Peripheral declaration
// ****************************************************************************
{$ALIGN 2}

var
  WDT      : TWDT_Registers       absolute LPC_WDT_BASE;
  MRT      : TMRT_Registers       absolute LPC_MRT_BASE;
  WKT      : TWKT_Registers       absolute LPC_WKT_BASE;
  SWM      : TSWM_Registers       absolute LPC_SWM_BASE;
  PMU      : TPMU_Registers       absolute LPC_PMU_BASE;
  CMP      : TCMP_Registers       absolute LPC_CMP_BASE;
  FLASHCTRL: TFLASHCTRL_Registers absolute LPC_FLASHCTRL_BASE;
  IOCON    : TIOCON_Registers     absolute LPC_IOCON_BASE;
  SYSCON   : TSysCon_Registers    absolute LPC_SYSCON_BASE;
  I2C      : TI2C_Registers       absolute LPC_I2C_BASE;
  SPI0     : TSPI_Registers       absolute LPC_SPI0_BASE;
  SPI1     : TSPI_Registers       absolute LPC_SPI1_BASE;
  USART0   : TUSART_Registers     absolute LPC_USART0_BASE;
  USART1   : TUSART_Registers     absolute LPC_USART0_BASE;
  USART2   : TUSART_Registers     absolute LPC_USART0_BASE;
  CRC      : TCRC_Registers       absolute LPC_CRC_BASE;
  SCT      : TSCT_Registers       absolute LPC_SCT_BASE;
  GPIO_PORT: TGPIOPort_Registers  absolute LPC_GPIO_PORT_BASE;
  PIN_INT  : TPININT_Registers    absolute LPC_PIN_INT_BASE;

implementation

procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
procedure Hardfault_interrupt;      external name 'Hardfault_interrupt';
procedure Startup_Checksum;         external name 'Startup_Checksum';
procedure SVCall_interrupt;         external name 'SVCall_interrupt';
procedure PendSV_interrupt;         external name 'PendSV_interrupt';
procedure SysTick_interrupt;        external name 'SysTick_interrupt';

procedure SPI0_Interrupt;   external name 'SPI0_Interrupt';
procedure SPI1_Interrupt;   external name 'SPI1_Interrupt';
procedure UART0_Interrupt;  external name 'UART0_Interrupt';
procedure UART1_Interrupt;  external name 'UART1_Interrupt';
procedure UART2_Interrupt;  external name 'UART2_Interrupt';
procedure I2C_Interrupt;    external name 'I2C_Interrupt';
procedure SCT_Interrupt;    external name 'SCT_Interrupt';
procedure MRT_Interrupt;    external name 'MRT_Interrupt';
procedure CMP_Interrupt;    external name 'CMP_Interrupt';
procedure WDT_Interrupt;    external name 'WDT_Interrupt';
procedure BOD_Interrupt;    external name 'BOD_Interrupt';
procedure WKT_Interrupt;    external name 'WKT_Interrupt';
procedure PINIT0_Interrupt; external name 'PINIT0_Interrupt';
procedure PINIT1_Interrupt; external name 'PINIT1_Interrupt';
procedure PINIT2_Interrupt; external name 'PINIT2_Interrupt';
procedure PINIT3_Interrupt; external name 'PINIT3_Interrupt';
procedure PINIT4_Interrupt; external name 'PINIT4_Interrupt';
procedure PINIT5_Interrupt; external name 'PINIT5_Interrupt';
procedure PINIT6_Interrupt; external name 'PINIT6_Interrupt';
procedure PINIT7_Interrupt; external name 'PINIT7_Interrupt';

{$I cortexm0_start.inc}

procedure Vectors; assembler;
nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
  interrupt_vectors:
  .long _stack_top
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

  .long SPI0_Interrupt
  .long SPI1_Interrupt
  .long 0
  .long UART0_Interrupt
  .long UART1_Interrupt
  .long UART2_Interrupt
  .long 0
  .long 0
  .long I2C_Interrupt
  .long SCT_Interrupt
  .long MRT_Interrupt
  .long CMP_Interrupt
  .long WDT_Interrupt
  .long BOD_Interrupt
  .long 0
  .long WKT_Interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long 0
  .long PINIT0_Interrupt
  .long PINIT1_Interrupt
  .long PINIT2_Interrupt
  .long PINIT3_Interrupt
  .long PINIT4_Interrupt
  .long PINIT5_Interrupt
  .long PINIT6_Interrupt
  .long PINIT7_Interrupt

  .weak NonMaskableInt_interrupt
  .weak Hardfault_interrupt
  .weak Startup_Checksum
  .weak SVCall_interrupt
  .weak PendSV_interrupt
  .weak SysTick_interrupt

  .weak SPI0_Interrupt
  .weak SPI1_Interrupt
  .weak UART0_Interrupt
  .weak UART1_Interrupt
  .weak UART2_Interrupt
  .weak I2C_Interrupt
  .weak SCT_Interrupt
  .weak MRT_Interrupt
  .weak CMP_Interrupt
  .weak WDT_Interrupt
  .weak BOD_Interrupt
  .weak WKT_Interrupt
  .weak PINIT0_Interrupt
  .weak PINIT1_Interrupt
  .weak PINIT2_Interrupt
  .weak PINIT3_Interrupt
  .weak PINIT4_Interrupt
  .weak PINIT5_Interrupt
  .weak PINIT6_Interrupt
  .weak PINIT7_Interrupt

  .set NonMaskableInt_interrupt, Startup
  .set Hardfault_interrupt     , Startup
  .set SVCall_interrupt        , Startup
  .set PendSV_interrupt        , Startup
  .set SysTick_interrupt       , Startup

  .set SPI0_Interrupt  , Startup
  .set SPI1_Interrupt  , Startup
  .set UART0_Interrupt , Startup
  .set UART1_Interrupt , Startup
  .set UART2_Interrupt , Startup
  .set I2C_Interrupt   , Startup
  .set SCT_Interrupt   , Startup
  .set MRT_Interrupt   , Startup
  .set CMP_Interrupt   , Startup
  .set WDT_Interrupt   , Startup
  .set BOD_Interrupt   , Startup
  .set WKT_Interrupt   , Startup
  .set PINIT0_Interrupt, Startup
  .set PINIT1_Interrupt, Startup
  .set PINIT2_Interrupt, Startup
  .set PINIT3_Interrupt, Startup
  .set PINIT4_Interrupt, Startup
  .set PINIT5_Interrupt, Startup
  .set PINIT6_Interrupt, Startup
  .set PINIT7_Interrupt, Startup

  .text
end;

end.
