unit lpc122x;
{$goto on}
{$define lpc122x}

interface
{$PACKRECORDS 2}

const
  //-------------------------  Cortex-M0 Processor Exceptions Numbers  -----------------------------
  Reset_IRQn            = -15; // 1  Reset Vector, invoked on Power up and warm reset */
  NonMaskableInt_IRQn   = -14; // 2  Non maskable Interrupt, cannot be stopped or preempted */
  HardFault_IRQn        = -13; // 3  Hard Fault, all classes of Fault */
  SVCall_IRQn           = -5;  // 11  System Service Call via SVC instruction */
  DebugMonitor_IRQn     = -4;  // 12  Debug Monitor                    */
  PendSV_IRQn           = -2;  // 14  Pendable request for system service */
  SysTick_IRQn          = -1;  // 15  System Tick Timer                */

  //---------------------------  LPC122x Specific Interrupt Numbers  -------------------------------
  WAKEUP0_IRQn          = 0;   // PIO0_0 to PIO0_11  Wakeup */
  WAKEUP1_IRQn          = 1;
  WAKEUP2_IRQn          = 2;
  WAKEUP3_IRQn          = 3;
  WAKEUP4_IRQn          = 4;
  WAKEUP5_IRQn          = 5;
  WAKEUP6_IRQn          = 6;
  WAKEUP7_IRQn          = 7;
  WAKEUP8_IRQn          = 8;
  WAKEUP9_IRQn          = 9;
  WAKEUP10_IRQn         = 10;
  WAKEUP11_IRQn         = 11;  // PIO0_0 to PIO0_11  Wakeup */
  I2C_IRQn              = 12;  // I2C Interrupt      */
  TIMER_16_0_IRQn       = 13;  // 16-bit Timer0 Interrupt    */
  TIMER_16_1_IRQn       = 14;  // 16-bit Timer1 Interrupt    */
  TIMER_32_0_IRQn       = 15;  // 32-bit Timer0 Interrupt    */
  TIMER_32_1_IRQn       = 16;  // 32-bit Timer1 Interrupt    */
  SSP_IRQn              = 17;  // SSP Interrupt      */
  UART0_IRQn            = 18;  // UART0 Interrupt    */
  UART1_IRQn            = 19;  // UART1 Interrupt    */
  CMP_IRQn              = 20;  // Comparator Interrupt       */
  ADC_IRQn              = 21;  // A/D Converter Interrupt    */
  WDT_IRQn              = 22;  // Watchdog timer Interrupt   */
  BOD_IRQn              = 23;  // Brown Out Detect(BOD) Interrupt    */
  EINT0_IRQn            = 25;  // External Interrupt 0 Interrupt     */
  EINT1_IRQn            = 26;  // External Interrupt 1 Interrupt     */
  EINT2_IRQn            = 27;  // External Interrupt 2 Interrupt     */
  DMA_IRQn              = 29;  // DMA Interrupt      */
  RTC_IRQn              = 30;  // RTC Interrupt      */

type
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
    _MOD   : longword;
    TC     : longword;
    FEED   : longword;
    TV     : longword;
    CLKSEL : longword;
    WARNINT: longword;
    WINDOW : longword;
  end;

  { ------------- Universal Asynchronous Receiver Transmitter 0 (UART0) ------------- }
  TUART0_Registers = record
    DLL_THR_RBR  : longword;
    IER_DLM      : longword;
    FCR_IIR      : longword;
    LCR          : longword;
    MCR          : longword;
    LSR          : longword;
    MSR          : longword;
    SCR          : longword;
    ACR          : longword;
    RESERVED0    : longword;
    FDR          : longword;
    RESERVED1    : longword;
    TER          : longword;
    RESERVED2    : array [0 .. 5] of longword;
    RS485CTRL    : longword;
    RS485ADRMATCH: longword;
    RS485DLY     : longword;
    FIFOLVL      : longword;
  end;

  { ------------- Universal Asynchronous Receiver Transmitter 1 (UART1) ------------- }
  TUART1_Registers = record
    DLL_THR_RBR: longword;
    IER_DLM    : longword;
    FCR_IIR    : longword;
    LCR        : longword;
    RESERVED0  : longword;
    LSR        : longword;
    RESERVED1  : longword;
    SCR        : longword;
    ACR        : longword;
    ICR        : longword;
    FDR        : longword;
    RESERVED2  : longword;
    TER        : longword;
    RESERVED3  : array [0 .. 8] of longword;
    FIFOLVL    : longword;
  end;

  { ------------- Timer (TCTxxBx) ------------- }
  TCTxxBx_Registers = record
    IR       : longword;
    TCR      : longword;
    TC       : longword;
    PR       : longword;
    PC       : longword;
    MR0      : longword;
    MR1      : longword;
    MR2      : longword;
    MR3      : longword;
    CCR      : longword;
    CR0      : longword;
    CR1      : longword;
    CR2      : longword;
    CR3      : longword;
    EMR      : longword;
    RESERVED0: array [0 .. 11] of longword;
    CTCR     : longword;
    PWMC     : longword;
  end;

  { ------------- Analog-to-Digital Converter (ADC) ------------- }
  TADC_Registers = record
    CR       : longword;
    GDR      : longword;
    RESERVED0: longword;
    INTEN    : longword;
    STAT     : longword;
    TRM      : longword;
  end;

  { ------------- Power Management Unit (PMU) ------------- }
  TPMU_Registers = record
    PCON  : longword;
    GPREG0: longword;
    GPREG1: longword;
    GPREG2: longword;
    GPREG3: longword;
    SYSCFG: longword;
  end;

  { ------------- Synchronous Serial Communication (SSP) ------------- }
  TSSP_Registers = record
    CR0  : longword;
    CR1  : longword;
    DR   : longword;
    SR   : longword;
    CPSR : longword;
    IMSC : longword;
    RIS  : longword;
    MIS  : longword;
    ICR  : longword;
    DMACR: longword;
  end;

  { ------------- Pin Connect Block (IOCON) ------------- }
  TIOCON_Registers = record
    RESERVED0    : array [0 .. 1] of longword;
    PIO0_19      : longword;
    PIO0_20      : longword;
    PIO0_21      : longword;
    PIO0_22      : longword;
    PIO0_23      : longword;
    PIO0_24      : longword;
    SWDIO_PIO0_25: longword;
    SWCLK_PIO0_26: longword;
    PIO0_27      : longword;
    PIO2_12      : longword;
    PIO2_13      : longword;
    PIO2_14      : longword;
    PIO2_15      : longword;
    PIO0_28      : longword;
    PIO0_29      : longword;
    PIO0_0       : longword;
    PIO0_1       : longword;
    PIO0_2       : longword;
    RESERVED1    : longword;
    PIO0_3       : longword;
    PIO0_4       : longword;
    PIO0_5       : longword;
    PIO0_6       : longword;
    PIO0_7       : longword;
    PIO0_8       : longword;
    PIO0_9       : longword;
    PIO2_0       : longword;
    PIO2_1       : longword;
    PIO2_2       : longword;
    PIO2_3       : longword;
    PIO2_4       : longword;
    PIO2_5       : longword;
    PIO2_6       : longword;
    PIO2_7       : longword;
    PIO0_10      : longword;
    PIO0_11      : longword;
    PIO0_12      : longword;
    RESET_PIO0_13: longword;
    PIO0_14      : longword;
    PIO0_15      : longword;
    PIO0_16      : longword;
    PIO0_17      : longword;
    PIO0_18      : longword;
    R_PIO0_30    : longword;
    R_PIO0_31    : longword;
    R_PIO1_0     : longword;
    R_PIO1_1     : longword;
    PIO1_2       : longword;
    PIO1_3       : longword;
    PIO1_4       : longword;
    PIO1_5       : longword;
    PIO1_6       : longword;
    RESERVED2    : array [0 .. 1] of longword;
    PIO2_8       : longword;
    PIO2_9       : longword;
    PIO2_10      : longword;
    PIO2_11      : longword;
  end;

  { ------------- System Control (SYSCON) ------------- }
  TSYSCON_Registers = record
    SYSMEMREMAP    : longword;
    PRESETCTRL     : longword;
    SYSPLLCTRL     : longword;
    SYSPLLSTAT     : longword;
    RESERVED0      : array [0 .. 3] of longword;
    SYSOSCCTRL     : longword;
    WDTOSCCTRL     : longword;
    IRCCTRL        : longword;
    RESERVED1      : longword;
    SYSRESSTAT     : longword;
    RESERVED2      : array [0 .. 2] of longword;
    SYSPLLCLKSEL   : longword;
    SYSPLLCLKUEN   : longword;
    RESERVED3      : array [0 .. 9] of longword;
    MAINCLKSEL     : longword;
    MAINCLKUEN     : longword;
    SYSAHBCLKDIV   : longword;
    RESERVED4      : longword;
    SYSAHBCLKCTRL  : longword;
    RESERVED5      : array [0 .. 3] of longword;
    SSPCLKDIV      : longword;
    UART0CLKDIV    : longword;
    UART1CLKDIV    : longword;
    RTCCLKDIV      : longword;
    RESERVED6      : array [0 .. 14] of longword;
    CLKOUTCLKSEL   : longword;
    CLKOUTUEN      : longword;
    CLKOUTDIV      : longword;
    RESERVED7      : array [0 .. 4] of longword;
    PIOPORCAP0     : longword;
    PIOPORCAP1     : longword;
    RESERVED8      : array [0 .. 10] of longword;
    IOCONFIGCLKDIV6: longword;
    IOCONFIGCLKDIV5: longword;
    IOCONFIGCLKDIV4: longword;
    IOCONFIGCLKDIV3: longword;
    IOCONFIGCLKDIV2: longword;
    IOCONFIGCLKDIV1: longword;
    IOCONFIGCLKDIV0: longword;
    BODCTRL        : longword;
    SYSTCKCAL      : longword;
    AHBPRIO        : longword;
    RESERVED9      : array [0 .. 4] of longword;
    IRQLATENCY     : longword;
    INTNMI         : longword;
    RESERVED10     : array [0 .. 33] of longword;
    STARTAPRP0     : longword;
    STARTERP0      : longword;
    STARTRSRP0CLR  : longword;
    STARTSRP0      : longword;
    STARTAPRP1     : longword;
    STARTERP1      : longword;
    STARTRSRP1CLR  : longword;
    STARTSRP1      : longword;
    RESERVED11     : array [0 .. 3] of longword;
    PDSLEEPCFG     : longword;
    PDAWAKECFG     : longword;
    PDRUNCFG       : longword;
    RESERVED12     : array [0 .. 109] of longword;
    DEVICE_ID      : longword;
  end;

  { ------------- Micro DMA Controller (MICRO_DMA) ------------- }
  TMICRODMA_Registers = record
    DMA_STATUS          : longword;
    DMA_CFG             : longword;
    CTRL_BASE_PTR       : longword;
    ATL_CTRL_BASE_PTR   : longword;
    DMA_WAITONREQ_STATUS: longword;
    CHNL_SW_REQUEST     : longword;
    CHNL_USEBURST_SET   : longword;
    CHNL_USEBURST_CLR   : longword;
    CHNL_REQ_MASK_SET   : longword;
    CHNL_REQ_MASK_CLR   : longword;
    CHNL_ENABLE_SET     : longword;
    CHNL_ENABLE_CLR     : longword;
    CHNL_PRI_ALT_SET    : longword;
    CHNL_PRI_ALT_CLR    : longword;
    CHNL_PRIORITY_SET   : longword;
    CHNL_PRIORITY_CLR   : longword;
    RESERVED0           : array [0 .. 2] of longword;
    ERR_CLR             : longword;
    RESERVED1           : array [0 .. 11] of longword;
    CHNL_IRQ_STATUS     : longword;
    IRQ_ERR_ENABLE      : longword;
    CHNL_IRQ_ENABLE     : longword;
  end;

  { ------------- Real Time Clock (RTC) ------------- }
  TRTC_Registers = record
    DR  : longword;
    MR  : longword;
    LR  : longword;
    CR  : longword;
    ICSC: longword;
    RIS : longword;
    MIS : longword;
    ICR : longword;
  end;

  { ------------- Analog Comparator (ACOMP) ------------- }
  TACOMP_Registers = record
    CMP : longword;
    VLAD: longword;
  end;

  { ------------- General Purpose Input/Output (GPIO) ------------- }
  TGPIO_Registers = record
    MASK     : longword;
    PIN      : longword;
    _OUT     : longword;
    _SET     : longword;
    CLR      : longword;
    _NOT     : longword;
    RESERVED0: array [0 .. 1] of longword;
    DIR      : longword;
    _IS      : longword;
    IBE      : longword;
    IEV      : longword;
    IE       : longword;
    RIS      : longword;
    MIS      : longword;
    IC       : longword;
  end;

  TFLASHCTRL_Registers = record
    RESERVED0: array [0 .. 9] of longword;
    FLASHCFG : longword;
  end;

  { ------------- CRC Engine(CRC) ------------- }
  TCRC_Registers = record
    MODE: longword;
    SEED: longword;
    SUM : longword;
  end;

// *****************************************************************************/
// Peripheral memory map
// *****************************************************************************/

const
  /// Base addresses
  LPC_FLASH_BASE = ($00000000);
  LPC_RAM_BASE   = ($10000000);
  LPC_APB_BASE   = ($40000000);
  LPC_AHB_BASE   = ($50000000);
  LPC_CM3_BASE   = ($E0000000);

  /// APB peripherals
  LPC_I2C_BASE       = (LPC_APB_BASE + $00000);
  LPC_WDT_BASE       = (LPC_APB_BASE + $04000);
  LPC_UART0_BASE     = (LPC_APB_BASE + $08000);
  LPC_UART1_BASE     = (LPC_APB_BASE + $0C000);
  LPC_CT16B0_BASE    = (LPC_APB_BASE + $10000);
  LPC_CT16B1_BASE    = (LPC_APB_BASE + $14000);
  LPC_CT32B0_BASE    = (LPC_APB_BASE + $18000);
  LPC_CT32B1_BASE    = (LPC_APB_BASE + $1C000);
  LPC_ADC_BASE       = (LPC_APB_BASE + $20000);
  LPC_PMU_BASE       = (LPC_APB_BASE + $38000);
  LPC_SSP_BASE       = (LPC_APB_BASE + $40000);
  LPC_IOCON_BASE     = (LPC_APB_BASE + $44000);
  LPC_SYSCON_BASE    = (LPC_APB_BASE + $48000);
  LPC_MICRO_DMA_BASE = (LPC_APB_BASE + $4C000);
  LPC_RTC_BASE       = (LPC_APB_BASE + $50000);
  LPC_ACOMP_BASE     = (LPC_APB_BASE + $54000);

  /// AHB peripherals
  LPC_GPIO0_BASE     = (LPC_AHB_BASE + $00000);
  LPC_GPIO1_BASE     = (LPC_AHB_BASE + $10000);
  LPC_GPIO2_BASE     = (LPC_AHB_BASE + $20000);
  LPC_FLASHCTRL_BASE = (LPC_AHB_BASE + $60000);
  LPC_CRC_BASE       = (LPC_AHB_BASE + $70000);

// *****************************************************************************
// Peripheral declaration
// *****************************************************************************

{$ALIGN 2}

var
  LPC_I2C      : TI2C_Registers      absolute(LPC_I2C_BASE);
  LPC_WDT      : TWDT_Registers      absolute(LPC_WDT_BASE);
  LPC_UART0    : TUART0_Registers    absolute(LPC_UART0_BASE);
  LPC_UART1    : TUART1_Registers    absolute(LPC_UART1_BASE);
  LPC_CT16B0   : TCTxxBx_Registers   absolute(LPC_CT16B0_BASE);
  LPC_CT16B1   : TCTxxBx_Registers   absolute(LPC_CT16B1_BASE);
  LPC_CT32B0   : TCTxxBx_Registers   absolute(LPC_CT32B0_BASE);
  LPC_CT23B1   : TCTxxBx_Registers   absolute(LPC_CT32B1_BASE);
  LPC_ADC      : TADC_Registers      absolute(LPC_ADC_BASE);
  LPC_PMU      : TPMU_Registers      absolute(LPC_PMU_BASE);
  LPC_SSP      : TSSP_Registers      absolute(LPC_SSP_BASE);
  LPC_IOCON    : TIOCON_Registers    absolute(LPC_IOCON_BASE);
  LPC_SYSCON   : TSYSCON_Registers   absolute(LPC_SYSCON_BASE);
  LPC_MICRO_DMA: TMICRODMA_Registers absolute(LPC_MICRO_DMA_BASE);
  LPC_RTC      : TRTC_Registers      absolute(LPC_RTC_BASE);
  LPC_ACOMP    : TACOMP_Registers    absolute(LPC_ACOMP_BASE);

  LPC_GPIO0    : TGPIO_Registers      absolute(LPC_GPIO0_BASE);
  LPC_GPIO1    : TGPIO_Registers      absolute(LPC_GPIO1_BASE);
  LPC_GPIO2    : TGPIO_Registers      absolute(LPC_GPIO2_BASE);
  LPC_FLASHCTRL: TFLASHCTRL_Registers absolute(LPC_FLASHCTRL_BASE);
  LPC_CRC      : TCRC_Registers       absolute(LPC_CRC_BASE);

implementation

procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
procedure Hardfault_interrupt;      external name 'Hardfault_interrupt';
procedure Startup_Checksum;         external name 'Startup_Checksum';
procedure SVCall_interrupt;         external name 'SVCall_interrupt';
procedure DebugMonitor_interrupt;   external name 'DebugMonitor_interrupt';
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
procedure I2C_Interrupt;       external name 'I2C_Interrupt';
procedure TIMER16_0_Interrupt; external name 'TIMER16_0_Interrupt';
procedure TIMER16_1_Interrupt; external name 'TIMER16_1_Interrupt';
procedure TIMER32_0_Interrupt; external name 'TIMER32_0_Interrupt';
procedure TIMER32_1_Interrupt; external name 'TIMER32_1_Interrupt';
procedure SSP_Interrupt;       external name 'SSP_Interrupt';
procedure UART0_Interrupt;     external name 'UART0_Interrupt';
procedure UART1_Interrupt;     external name 'UART1_Interrupt';
procedure CMP_Interrupt;       external name 'CMP_Interrupt';
procedure ADC_Interrupt;       external name 'ADC_Interrupt';
procedure WDT_Interrupt;       external name 'WDT_Interrupt';
procedure BOD_Interrupt;       external name 'BOD_Interrupt';
procedure EINT0_Interrupt;     external name 'EINT0_Interrupt';
procedure EINT1_Interrupt;     external name 'EINT1_Interrupt';
procedure EINT2_Interrupt;     external name 'EINT2_Interrupt';
procedure DMA_Interrupt;       external name 'DMA_Interrupt';
procedure RTC_Interrupt;       external name 'RTC_Interrupt';

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
  .long I2C_Interrupt
  .long TIMER16_0_Interrupt
  .long TIMER16_1_Interrupt
  .long TIMER32_0_Interrupt
  .long TIMER32_1_Interrupt
  .long SSP_Interrupt
  .long UART0_Interrupt
  .long UART1_Interrupt
  .long CMP_interrupt
  .long ADC_Interrupt
  .long WDT_Interrupt
  .long BOD_Interrupt
  .long 0
  .long EINT0_Interrupt
  .long EINT1_Interrupt
  .long EINT2_Interrupt
  .long 0
  .long DMA_Interrupt
  .long RTC_Interrupt
  .long 0

  .weak NonMaskableInt_interrupt
  .weak Hardfault_interrupt
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
  .weak I2C_Interrupt
  .weak TIMER16_0_Interrupt
  .weak TIMER16_1_Interrupt
  .weak TIMER32_0_Interrupt
  .weak TIMER32_1_Interrupt
  .weak SSP_Interrupt
  .weak UART0_Interrupt
  .weak UART1_Interrupt
  .weak CMP_interrupt
  .weak ADC_Interrupt
  .weak WDT_Interrupt
  .weak BOD_Interrupt
  .weak EINT0_Interrupt
  .weak EINT1_Interrupt
  .weak EINT2_Interrupt
  .weak DMA_Interrupt
  .weak RTC_Interrupt

  .set NonMaskableInt_interrupt, Startup
  .set Hardfault_interrupt     , Startup
  .set SVCall_interrupt        , Startup
  .set DebugMonitor_interrupt  , Startup
  .set PendSV_interrupt        , Startup

  .set SysTick_interrupt  , Startup
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
  .set I2C_Interrupt      , Startup
  .set TIMER16_0_Interrupt, Startup
  .set TIMER16_1_Interrupt, Startup
  .set TIMER32_0_Interrupt, Startup
  .set TIMER32_1_Interrupt, Startup
  .set SSP_Interrupt      , Startup
  .set UART0_Interrupt    , Startup
  .set UART1_Interrupt    , Startup
  .set CMP_Interrupt      , Startup
  .set ADC_Interrupt      , Startup
  .set WDT_Interrupt      , Startup
  .set BOD_Interrupt      , Startup
  .set EINT0_Interrupt    , Startup
  .set EINT1_Interrupt    , Startup
  .set EINT2_Interrupt    , Startup
  .set DMA_Interrupt      , Startup
  .set RTC_Interrupt      , Startup
  .text
end;

end.
