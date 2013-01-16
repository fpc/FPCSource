
unit lpc1768;

{$goto on}
{$define lpc1768}

interface

{$PACKRECORDS 2}

//
//    STCTRL   : DWord absolute $E000E010;
//    STRELOAD : DWord absolute $E000E014;
//    STCURR   : DWord absolute $E000E018;
//
//    FIO1DIR2 : Byte  absolute $2009C022;
//    FIO1SET2 : Byte  absolute $2009C03A;
//    FIO1CLR2 : Byte  absolute $2009C03E;
//
//    SCS      : DWord absolute $400FC1A0;
//    CLKSRCSEL: DWord absolute $400FC10C;
//    PLL0FEED : DWord absolute $400FC08C;
//    PLL0CON  : DWord absolute $400FC080;
//    PLL0CFG  : DWord absolute $400FC084;
//    PLL0STAT : DWord absolute $400FC088;
//    CCLKCFG  : DWord absolute $400FC104;
//

Const
  NonMaskableInt_IRQn        = -14; //  2 Non Maskable // interrupt
  HardFault_IRQn          	 = -13;  //  4 Cortex-M3 Memory Management // interrupt
  MemoryManagement_IRQn      = -12; //  4 Cortex-M3 Memory Management // interrupt
  BusFault_IRQn              = -11; //  5 Cortex-M3 Bus Fault // interrupt
  UsageFault_IRQn            = -10; //  6 Cortex-M3 Usage Fault // interrupt
  SVCall_IRQn                = -5;   // 11 Cortex-M3 SV Call // interrupt
  DebugMonitor_IRQn          = -4;   // 12 Cortex-M3 Debug Monitor // interrupt
  PendSV_IRQn                = -2;   // 14 Cortex-M3 Pend SV // interrupt
  SysTick_IRQn               = -1;  // 15 Cortex-M3 System Tick // interrupt
  WWDG_IRQn                  = 0;   //  Window WatchDog // interrupt
  PVD_IRQn                   = 1;    //  PVD through EXTI Line detection // interrupt
  TAMPER_IRQn                = 2;   //  Tamper // interrupt
  RTC_IRQn                   = 3;    //  RTC global // interrupt
  FLASH_IRQn                 = 4;    //  FLASH global // interrupt
  RCC_IRQn                   = 5;    //  RCC global // interrupt
  EXTI0_IRQn                 = 6;    //  EXTI Line0 // interrupt
  EXTI1_IRQn                 = 7;    //  EXTI Line1 // interrupt
  EXTI2_IRQn                 = 8;    //  EXTI Line2 // interrupt
  EXTI3_IRQn                 = 9;    //  EXTI Line3 // interrupt
  EXTI4_IRQn                 = 10;  // EXTI Line4 // interrupt
  DMA1_Channel1_IRQn         = 11;  // DMA1 Channel 1 global // interrupt
  DMA1_Channel2_IRQn         = 12;  // DMA1 Channel 2 global // interrupt
  DMA1_Channel3_IRQn         = 13;  // DMA1 Channel 3 global // interrupt
  DMA1_Channel4_IRQn         = 14;  // DMA1 Channel 4 global // interrupt
  DMA1_Channel5_IRQn         = 15;  // DMA1 Channel 5 global // interrupt
  DMA1_Channel6_IRQn         = 16;  // DMA1 Channel 6 global // interrupt
  DMA1_Channel7_IRQn         = 17;  // DMA1 Channel 7 global // interrupt
  ADC1_2_IRQn                = 18;   // ADC1 et ADC2 global // interrupt
  USB_HP_CAN1_TX_IRQn        = 19;   // USB High Priority or CAN1 TX Interrupts
  USB_LP_CAN1_RX0_IRQn       = 20;  // USB Low Priority or CAN1 RX0 Interrupts
  CAN1_RX1_IRQn              = 21;   // CAN1 RX1 // interrupt
  CAN1_SCE_IRQn              = 22;   // CAN1 SCE // interrupt
  EXTI9_5_IRQn               = 23;  // External Line[9:5] Interrupts
  TIM1_BRK_IRQn              = 24;   // TIM1 Break // interrupt
  TIM1_UP_IRQn               = 25;  // TIM1 Update // interrupt
  TIM1_TRG_COM_IRQn          = 26;   // TIM1 Trigger and Commutation // interrupt
  TIM1_CC_IRQn               = 27;  // TIM1 Capture Compare // interrupt
  TIM2_IRQn                  = 28;   // TIM2 global // interrupt
  TIM3_IRQn                  = 29;   // TIM3 global // interrupt
  TIM4_IRQn                  = 30;   // TIM4 global // interrupt
  I2C1_EV_IRQn               = 31;  // I2C1 Event // interrupt
  I2C1_ER_IRQn               = 32;  // I2C1 Error // interrupt
  I2C2_EV_IRQn               = 33;  // I2C2 Event // interrupt
  I2C2_ER_IRQn               = 34;  // I2C2 Error // interrupt
  SPI1_IRQn                  = 35;   // SPI1 global // interrupt
  SPI2_IRQn                  = 36;   // SPI2 global // interrupt
  USART1_IRQn                = 37;   // USART1 global // interrupt
  USART2_IRQn                = 38;   // USART2 global // interrupt
  USART3_IRQn                = 39;   // USART3 global // interrupt
  EXTI15_10_IRQn             = 40;  // External Line[15:10] Interrupts
  RTCAlarm_IRQn              = 41;   // RTC Alarm through EXTI Line // interrupt
  USBWakeUp_IRQn             = 42;  // USB WakeUp from suspend through EXTI Line // interrupt
  TIM8_BRK_IRQn              = 43;   // TIM8 Break // interrupt
  TIM8_UP_IRQn               = 44;  // TIM8 Update // interrupt
  TIM8_TRG_COM_IRQn          = 45;   // TIM8 Trigger and Commutation // interrupt
  TIM8_CC_IRQn               = 46;  // TIM8 Capture Compare // interrupt
  ADC3_IRQn                  = 47;   // ADC3 global // interrupt
  FSMC_IRQn                  = 48;   // FSMC global // interrupt
  SDIO_IRQn                  = 49;   // SDIO global // interrupt
  TIM5_IRQn                  = 50;   // TIM5 global // interrupt
  SPI3_IRQn                  = 51;   // SPI3 global // interrupt
  UART4_IRQn                 = 52;  // UART4 global // interrupt
  UART5_IRQn                 = 53;  // UART5 global // interrupt
  TIM6_IRQn                  = 54;   // TIM6 global // interrupt
  TIM7_IRQn                  = 55;   // TIM7 global // interrupt
  DMA2_Channel1_IRQn         = 56;  // DMA2 Channel 1 global // interrupt
  DMA2_Channel2_IRQn         = 57;  // DMA2 Channel 2 global // interrupt
  DMA2_Channel3_IRQn         = 58;  // DMA2 Channel 3 global // interrupt
  DMA2_Channel4_5_IRQn       = 59;  // DMA2 Channel 4 and Channel 5 global // interrupt

Type

//*------------- System Control (SC) ------------------------------------------*/

TSCRegisters = Record
  FLASHCFG		: DWord;               // Flash Accelerator Module           */
  RESERVED0		: Array [1..31] Of DWord;
  PLL0CON		: DWord;                // Clocking and Power Control         */
  PLL0CFG		: DWord;
  PLL0STAT		: DWord;
  PLL0FEED		: DWord;
  RESERVED1		: Array [1..4] Of DWord;
  PLL1CON		: DWord;
  PLL1CFG		: DWord;
  PLL1STAT		: DWord;
  PLL1FEED		: DWord;
  RESERVED2		: Array [1..4] Of DWord;
  PCON			: DWord;
  PCONP			: DWord;
  RESERVED3		: Array [1..15] Of DWord;
  CCLKCFG		: DWord;
  USBCLKCFG		: DWord;
  CLKSRCSEL		: DWord;
  RESERVED4		: Array [1..12] Of DWord;
  EXTINT		: DWord;                 // External Interrupts                */
  RESERVED5		: DWord;
  EXTMODE		: DWord;
  EXTPOLAR		: DWord;
  RESERVED		: Array [1..12] Of DWord;
  RSID			: DWord;                   // Reset                              */
  RESERVED7		: Array [1..7] Of DWord;
  SCS			: DWord;                    // Syscon Miscellaneous Registers     */
  IRCTRIM		: DWord;                // Clock Dividers                     */
  PCLKSEL0		: DWord;
  PCLKSEL1		: DWord;
  RESERVED8		: Array [1..4] Of DWord;
  USBIntSt		: DWord;               // USB Device/OTG Interrupt Register  */
  DMAREQSEL		: DWord;
  CLKOUTCFG		: DWord;              // Clock Output Configuration         */
End;

//*------------- Pin Connect Block (PINCON) -----------------------------------*/

TPINCONRegisters = Record
   PINSEL0		: DWord;
   PINSEL1		: DWord;
   PINSEL2		: DWord;
   PINSEL3		: DWord;
   PINSEL4		: DWord;
   PINSEL5		: DWord;
   PINSEL6		: DWord;
   PINSEL7		: DWord;
   PINSEL8		: DWord;
   PINSEL9		: DWord;
   PINSEL10		: DWord;
   RESERVED0	: Array [1..5] Of DWord;
   PINMODE0		: DWord;
   PINMODE1		: DWord;
   PINMODE2		: DWord;
   PINMODE3		: DWord;
   PINMODE4		: DWord;
   PINMODE5		: DWord;
   PINMODE6		: DWord;
   PINMODE7		: DWord;
   PINMODE8		: DWord;
   PINMODE9		: DWord;
   PINMODE_OD0	: DWord;
   PINMODE_OD1	: DWord;
   PINMODE_OD2	: DWord;
   PINMODE_OD3	: DWord;
   PINMODE_OD4	: DWord;
   I2CPADCFG	: DWord;
End;

//------------- General Purpose Input/Output (GPIO) --------------------------*/

{
TGPIORegisters = Record
  Case Byte Of
    0: (FIODIR: DWord);
    1: (FIORIRL, FIODIRH: Word);
    2: (FIODIR0, FIODIR1, FIODIR2, FIODIR3: Byte);
  End;
  RESERVED0: Array [1..3] Of DWord;;
  Case Byte Of
    0: (FIOMASK: DWord);
    1: (FIOMASKL, FIOMASKH: Word);
    2: (FIOMASK0, FIOMASK1, FIOMASK2, FIOMASK3: Byte);
  End;
  Case Byte Of
    0: (FIOPIN: DWord);
    1: (FIOPINL, FIOPINH: Word);
    2: (FIOPIN0, FIOPIN1, FIOPIN2, FIOPIN3: Byte);
  End;
  Case Byte Of
    0: (FIOSET: DWord);
    1: (FIOSETL, FIOSETH: Word);
    2: (FIOSET0, FIOSET1, FIOSET2, FIOSET3: Byte);
  End;
  Case Byte Of
    0: (FIOCLR: DWord);
    1: (FIOCLRL, FIOSETH: Word);
    2: (FIOCLR0, FIOCLR1, FIOCLR2, FIOCLR3: Byte);
  End;
End;
}
  TGPIORegisters = Record
    FIODIR: DWord;
    RESERVED0: Array [1..3] Of DWord;
    FIOMASK: DWord;
    FIOPIN: DWord;
    FIOSET: DWord;
    FIOCLR: DWord;
  End;

TGPIOINTRegisters = Record
  IntStatus: DWord;
  IO0IntStatR: DWord;
  IO0IntStatF: DWord;
  IO0IntClr: DWord;
  IO0IntEnR: DWord;
  IO0IntEnF: DWord;
  RESERVED0: Array [1..2] Of DWord;
  IO2IntStatR: DWord;
  IO2IntStatF: DWord;
  IO2IntClr: DWord;
  IO2IntEnR: DWord;
  IO2IntEnF: DWord;
End;

//*------------- Timer (TIM) --------------------------------------------------*/
TTIMRegisters = Record
  IR: DWord;
  TCR: DWord;
  TC: DWord;
  PR: DWord;
  PC: DWord;
  MCR: DWord;
  MR0: DWord;
  MR1: DWord;
  MR2: DWord;
  MR3: DWord;
  CCR: DWord;
  CR0: DWord;
  CR1: DWord;
  RESERVED0: Array [1..2] Of DWord;
  EMR: DWord;
  RESERVED1: Array [1..12] Of DWord;
  CTCR: DWord;
End;

//*------------- Pulse-Width Modulation (PWM) ---------------------------------*/
TPWMRegisters = Record
  IR: DWord;
  TCR: DWord;
  TC: DWord;
  PR: DWord;
  PC: DWord;
  MCR: DWord;
  MR0: DWord;
  MR1: DWord;
  MR2: DWord;
  MR3: DWord;
  CCR: DWord;
  CR0: DWord;
  CR1: DWord;
  CR2: DWord;
  CR3: DWord;
  RESERVED0: DWord;
  MR4: DWord;
  MR5: DWord;
  MR6: DWord;
  PCR: DWord;
  LER: DWord;
  RESERVED1: Array [1..7] Of DWord;
  CTCR: DWord;
End;

//*------------- Universal Asynchronous Receiver Transmitter (UART) -----------*/
{
TUARTRegisters = Record
  Case Byte Of
    0: (RBR: Byte);
    1: (THR: Byte);
    2: (DLL: Byte);
    3: (RESERVED: DWord);
  End;
  Case Byte Of
    0: (DLM: Byte);
    1: (IER: DWord);
  End;
  Case Byte Of
    0: (IIR: DWord);
    1: (FCR: Byte);
  End;
  LCR: Byte;
  RESERVED1: Array [1..7] Of Byte;
  LSR: Byte;
  RESERVED2: Array [1..7] Of Byte;
  SCR: Byte;
  RESERVED3: Array [1..3] Of Byte;
  ACR: DWord;
  ICR: Byte;
  RESERVED4: Array [1..3] Of Byte;
  FDR: Byte;
  RESERVED5: Array [1..7] Of Byte;
  TER: Byte;
  RESERVED6: Array [1..39] Of Byte;
  FIFOLVL: Byte;
End;

TUART0Registers = Record
  Case Byte Of
    0: (RBR: Byte);
    1: (THR: Byte);
    2: (DLL: Byte);
    3: (RESERVED: DWord);
  End;
  Case Byte Of
    0: (DLM: Byte);
    1: (IER: DWord);
  End;
  Case Byte Of
    0: (IIR: DWord);
    1: (FCR: Byte);
  End;
  LCR: Byte;
  RESERVED1: Array [1..7] Of Byte;
  LSR: Byte;
  RESERVED2: Array [1..7] Of Byte;
  SCR: Byte;
  RESERVED3: Array [1..3] Of Byte;
  ACR: DWord;
  ICR: Byte;
  RESERVED4: Array [1..3] Of Byte;
  FDR: Byte;
  RESERVED5: Array [1..7] Of Byte;
  TER: Byte;
  RESERVED6: Array [1..39] Of Byte;
  FIFOLVL: Byte;
End;


TUART1Registers = Record
  Case Byte Of
    0: (RBR: Byte);
    1: (THR: Byte);
    2: (DLL: Byte);
    3: (RESERVED: DWord);
  End;
  Case Byte Of
    0: (DLM: Byte);
    1: (IER: DWord);
  End;
  Case Byte Of
    0: (IIR: DWord);
    1: (FCR: Byte);
  End;
  LCR: Byte;
  RESERVED1: Array [1..3] Of Byte;
  MCR: Byte;
  RESERVED2: Array [1..3] Of Byte;
  LSR: Byte;
  RESERVED3: Array [1..3] Of Byte;
  MSR: Byte;
  RESERVED4: Array [1..3] Of Byte;
  SCR: Byte;
  RESERVED5: Array [1..3] Of Byte;
  ACR: DWord;
  RESERVED6: DWord;
  FDR: DWord;
  RESERVED7: DWord;
  TER: Byte;
  RESERVED8: Array [1..27] Of Byte;
  RS485CTRL: Byte;
  RESERVED9: Array [1..3] Of Byte;
  ADRMATCH: Byte;
  RESERVED10: Array [1..3] Of Byte;
  RS485DLY: Byte;
  RESERVED11: Array [1..3] Of Byte;
  FIFOLVL: Byte
End;
}
//*------------- Serial Peripheral Interface (SPI) ----------------------------*/

TSPIRegisters = Record
  SPCR			: DWord;
  SPSR			: DWord;
  SPDR			: DWord;
  RESERVED0		: Array [1..3] Of DWord;
  SPINT			: DWord;
End;

//*------------- Synchronous Serial Communication (SSP) -----------------------*/

TSSPRegisters = Record
  CR0,
  CR1,
  DR,
  SR,
  CPSR,
  IMSC,
  RIS,
  MIS,
  ICR,
  DMACR			: DWord;
End;

//*------------- Inter-Integrated Circuit (I2C) -------------------------------*/

TI2CRegisters = Record
  I2CONSET		: DWord;
  I2STAT		: DWord;
  I2DAT			: DWord;
  I2ADR0		: DWord;
  I2SCLH		: DWord;
  I2SCLL		: DWord;
  I2CONCLR		: DWord;
  MMCTRL		: DWord;
  I2ADR1		: DWord;
  I2ADR2		: DWord;
  I2ADR3		: DWord;
  I2DATA_BUFFER	: DWord;
  I2MASK0		: DWord;
  I2MASK1		: DWord;
  I2MASK2		: DWord;
  I2MASK3		: DWord;
End;

//*------------- Inter IC Sound (I2S) -----------------------------------------*/

TI2SRegisters = Record
  I2SDAO		: DWord;
  I2SDAI		: DWord;
  I2STXFIFO		: DWord;
  I2SRXFIFO		: DWord;
  I2SSTATE		: DWord;
  I2SDMA1		: DWord;
  I2SDMA2		: DWord;
  I2SIRQ		: DWord;
  I2STXRATE		: DWord;
  I2SRXRATE		: DWord;
  I2STXBITRATE	: DWord;
  I2SRXBITRATE	: DWord;
  I2STXMODE		: DWord;
  I2SRXMODE		: DWord;
End;

//*------------- Repetitive Interrupt Timer (RIT) -----------------------------*/

TRITRegisters = Record
  RICOMPVAL		: DWord;
  RIMASK			: DWord;
  RICTRL			: Byte;
  RESERVED0		: Array [1..3] Of Byte;
  RICOUNTER		: DWord;
End;

//*------------- Real-Time Clock (RTC) ----------------------------------------*/
TRTCRegisters = Record
   ILR			: Byte;
   RESERVED0		: Array [1..7] Of Byte;
   CCR			: Byte;
   RESERVED1		: Array [1..3] Of Byte;
   CIIR			: Byte;
   RESERVED2		: Array [1..3] Of Byte;
   AMR			: Byte;
   RESERVED3		: Array [1..3] Of Byte;
   CTIME0		: DWord;
   CTIME1		: DWord;
   CTIME2		: DWord;
   SEC			: Byte;
   RESERVED4		: Array [1..3] Of Byte;
   MIN			: Byte;
   RESERVED5		: Array [1..3] Of Byte;
   HOUR			: Byte;
   RESERVED6		: Array [1..3] Of Byte;
   DOM			: Byte;
   RESERVED7		: Array [1..3] Of Byte;
   DOW			: Byte;
   RESERVED8		: Array [1..3] Of Byte;
   DOY			: Word;
   RESERVED9		: Word;
   MONTH			: Byte;
   RESERVED10		: Array [1..3] Of Byte;
   YEAR			: Word;
   RESERVED11		: Word;
   CALIBRATION	: DWord;
   GPREG0		: DWord;
   GPREG1		: DWord;
   GPREG2		: DWord;
   GPREG3		: DWord;
   GPREG4		: DWord;
   RTC_AUXEN		: Byte;
   RESERVED12		: Array [1..3] Of Byte;
   RTC_AUX		: Byte;
   RESERVED13		: Array [1..3] Of Byte;
   ALSEC			: Byte;
   RESERVED14		: Array [1..3] Of Byte;
   ALMIN			: Byte;
   RESERVED15		: Array [1..3] Of Byte;
   ALHOUR		: Byte;
   RESERVED16		: Array [1..3] Of Byte;
   ALDOM			: Byte;
   RESERVED17		: Array [1..3] Of Byte;
   ALDOW			: Byte;
   RESERVED18		: Array [1..3] Of Byte;
   ALDOY			: Word;
   RESERVED19		: Word;
   ALMON			: Byte;
   RESERVED20		: Array [1..3] Of Byte;
   ALYEAR		: Word;
   RESERVED21		: Word;
End;

//*------------- Watchdog Timer (WDT) -----------------------------------------*/

TWDTRegisters = Record
  WDMOD			: Byte;
  RESERVED0		: Array [1..3] Of Byte;
  WDTC			: DWord;
  WDFEED			: Byte;
  RESERVED1		: Array [1..3] Of Byte;
  WDTV			: DWord;
  WDCLKSEL		: DWord;
End;

//*------------- Analog-to-Digital Converter (ADC) ----------------------------*/
TADCRegisters = Record
  ADCR			: DWord;
  ADGDR			: DWord;
  RESERVED0		: DWord;
  ADINTEN		: DWord;
  ADDR0			: DWord;
  ADDR1			: DWord;
  ADDR2			: DWord;
  ADDR3			: DWord;
  ADDR4			: DWord;
  ADDR5			: DWord;
  ADDR6			: DWord;
  ADDR7			: DWord;
  ADSTAT			: DWord;
  ADTRM			: DWord;
End;

//*------------- Digital-to-Analog Converter (DAC) ----------------------------*/
TDACRegisters = Record
  DACR			: DWord;
  DACCTRL		: DWord;
  DACCNTVAL		: Word;
End;

//*------------- Motor Control Pulse-Width Modulation (MCPWM) -----------------*/
TMCPWMRegisters = Record
  MCCON		: DWord;
  MCCON_SET	: DWord;
  MCCON_CLR	: DWord;
  MCCAPCON		: DWord;
  MCCAPCON_SET	: DWord;
  MCCAPCON_CLR	: DWord;
  MCTIM0		: DWord;
  MCTIM1		: DWord;
  MCTIM2		: DWord;
  MCPER0		: DWord;
  MCPER1		: DWord;
  MCPER2		: DWord;
  MCPW0		: DWord;
  MCPW1		: DWord;
  MCPW2		: DWord;
  MCDEADTIME	: DWord;
  MCCCP		: DWord;
  MCCR0		: DWord;
  MCCR1		: DWord;
  MCCR2		: DWord;
  MCINTEN		: DWord;
  MCINTEN_SET	: DWord;
  MCINTEN_CLR	: DWord;
  MCCNTCON		: DWord;
  MCCNTCON_SET	: DWord;
  MCCNTCON_CLR	: DWord;
  MCINTFLAG	: DWord;
  MCINTFLAG_SET: DWord;
  MCINTFLAG_CLR: DWord;
  MCCAP_CLR	: DWord;
End;
//*------------- Quadrature Encoder Interface (QEI) ---------------------------*/
TQEIRegisters = Record
  QEICON: DWord;
  QEISTAT: DWord;
  QEICONF: DWord;
  QEIPOS: DWord;
  QEIMAXPOS: DWord;
  CMPOS0: DWord;
  CMPOS1: DWord;
  CMPOS2: DWord;
  INXCNT: DWord;
  INXCMP: DWord;
  QEILOAD: DWord;
  QEITIME: DWord;
  QEIVEL: DWord;
  QEICAP: DWord;
  VELCOMP: DWord;
  FILTER: DWord;
  RESERVED0: Array [1..998] Of DWord;
  QEIIEC: DWord;
  QEIIES: DWord;
  QEIINTSTAT: DWord;
  QEIIE: DWord;
  QEICLR: DWord;
  QEISET: DWord;
End;

//*------------- Controller Area Network (CAN) --------------------------------*/
TCANAF_RAMRegisters = Record
  MASK: Array [1..512] Of DWord;              //* ID Masks                           */
End;

TCANAF = Record                          //* Acceptance Filter Registers        */
  AFMR: DWord;
  SFF_sa: DWord;
  SFF_GRP_sa: DWord;
  EFF_sa: DWord;
  EFF_GRP_sa: DWord;
  ENDofTable: DWord;
  LUTerrAd: DWord;
  LUTerr: DWord;
  FCANIE: DWord;
  FCANIC0: DWord;
  FCANIC1: DWord;
End;

TCANCRRegisters = Record                     //* Central Registers                  */
     CANTxSR: DWord;
     CANRxSR: DWord;
     CANMSR: DWord;
End;

TCANRegisters = Record                     //* Controller Registers               */
    _MOD: DWord;
     CMR: DWord;
    GSR: DWord;
     ICR: DWord;
    IER: DWord;
    BTR: DWord;
    EWL: DWord;
     SR: DWord;
    RFS: DWord;
    RID: DWord;
    RDA: DWord;
    RDB: DWord;
    TFI1: DWord;
    TID1: DWord;
    TDA1: DWord;
    TDB1: DWord;
    TFI2: DWord;
    TID2: DWord;
    TDA2: DWord;
    TDB2: DWord;
    TFI3: DWord;
    TID3: DWord;
    TDA3: DWord;
    TDB3: DWord;
End;

//*------------- General Purpose Direct Memory Access (GPDMA) -----------------*/

TGPDMARegisters = Record              //* Common Registers                   */
     DMACIntStat: DWord;
     DMACIntTCStat: DWord;
     DMACIntTCClear: DWord;
     DMACIntErrStat: DWord;
     DMACIntErrClr: DWord;
     DMACRawIntTCStat: DWord;
     DMACRawIntErrStat: DWord;
     DMACEnbldChns: DWord;
    DMACSoftBReq: DWord;
    DMACSoftSReq: DWord;
    DMACSoftLBReq: DWord;
    DMACSoftLSReq: DWord;
    DMACConfig: DWord;
    DMACSync: DWord;
End;

TGPDMACHRegisters = Record              //* Channel Registers                  */
  DMACCSrcAddr	: DWord;
  DMACCDestAddr	: DWord;
  DMACCLLI		: DWord;
  DMACCControl	: DWord;
  DMACCConfig		: DWord;
End;

//*------------- Universal Serial Bus (USB) -----------------------------------*/
TUSBRegisters = Record
  HcRevision: DWord;             //* USB Host Registers                 */
  HcControl: DWord;
    HcCommandStatus: DWord;
    HcInterruptStatus: DWord;
    HcInterruptEnable: DWord;
    HcInterruptDisable: DWord;
    HcHCCA: DWord;
     HcPeriodCurrentED: DWord;
    HcControlHeadED: DWord;
    HcControlCurrentED: DWord;
    HcBulkHeadED: DWord;
    HcBulkCurrentED: DWord;
     HcDoneHead: DWord;
    HcFmInterval: DWord;
     HcFmRemaining: DWord;
     HcFmNumber: DWord;
    HcPeriodicStart: DWord;
    HcLSTreshold: DWord;
    HcRhDescriptorA: DWord;
    HcRhDescriptorB: DWord;
    HcRhStatus: DWord;
    HcRhPortStatus1: DWord;
    HcRhPortStatus2: DWord;
    RESERVED0: Array [1..40] Of DWord;
     Module_ID: DWord;

     OTGIntSt: DWord;               //* USB On-The-Go Registers            */
    OTGIntEn: DWord;
     OTGIntSet: DWord;
     OTGIntClr: DWord;
    OTGStCtrl: DWord;
    OTGTmr: DWord;
        RESERVED1: Array [1..58] Of DWord;

     USBDevIntSt: DWord;            // USB Device Interrupt Registers     */
    USBDevIntEn: DWord;
     USBDevIntClr: DWord;
     USBDevIntSet: DWord;

     USBCmdCode: DWord;             // USB Device SIE Command Registers   */
     USBCmdData: DWord;

     USBRxData: DWord;              // USB Device Transfer Registers      */
     USBTxData: DWord;
     USBRxPLen: DWord;
     USBTxPLen: DWord;
    USBCtrl: DWord;
     USBDevIntPri: DWord;

     USBEpIntSt: DWord;             // USB Device Endpoint Interrupt Regs */
    USBEpIntEn: DWord;
     USBEpIntClr: DWord;
     USBEpIntSet: DWord;
     USBEpIntPri: DWord;

    USBReEp: DWord;                // USB Device Endpoint Realization Reg*/
     USBEpInd: DWord;
    USBMaxPSize: DWord;

     USBDMARSt: DWord;              // USB Device DMA Registers           */
     USBDMARClr: DWord;
     USBDMARSet: DWord;
        RESERVED2:Array [1..9] Of DWord;
    USBUDCAH: DWord;
     USBEpDMASt: DWord;
     USBEpDMAEn: DWord;
     USBEpDMADis: DWord;
     USBDMAIntSt: DWord;
    USBDMAIntEn: DWord;
        RESERVED3:Array [1..2] Of DWord;
     USBEoTIntSt: DWord;
     USBEoTIntClr: DWord;
     USBEoTIntSet: DWord;
     USBNDDRIntSt: DWord;
     USBNDDRIntClr: DWord;
     USBNDDRIntSet: DWord;
     USBSysErrIntSt: DWord;
     USBSysErrIntClr: DWord;
     USBSysErrIntSet: DWord;
        RESERVED4: Array [1..15] Of DWord;

     I2C_RX: DWord;                 // USB OTG I2C Registers              */
     I2C_WO: DWord;
     I2C_STS: DWord;
    I2C_CTL: DWord;
    I2C_CLKHI: DWord;
     I2C_CLKLO: DWord;
        RESERVED5:Array [1..823] Of DWord;
    USBClkCtrl: Byte;		// USB Clock Control Registers        */
  End;
///------------- Ethernet Media Access Controller (EMAC) ----------------------*/
TEMACRegisters = Record
    MAC1: DWord;                   // MAC Registers                      */
    MAC2: DWord;
    IPGT: DWord;
    IPGR: DWord;
    CLRT: DWord;
    MAXF: DWord;
    SUPP: DWord;
    TEST: DWord;
    MCFG: DWord;
    MCMD: DWord;
    MADR: DWord;
     MWTD: DWord;
     MRDD: DWord;
     MIND: DWord;
        RESERVED0:Array [1..2] Of DWord;
    SA0: DWord;
    SA1: DWord;
    SA2: DWord;
        RESERVED1:Array [1..45] Of DWord;
    Command: DWord;                // Control Registers                  */
     Status: DWord;
    RxDescriptor: DWord;
    RxStatus: DWord;
    RxDescriptorNumber: DWord;
     RxProduceIndex: DWord;
    RxConsumeIndex: DWord;
    TxDescriptor: DWord;
    TxStatus: DWord;
    TxDescriptorNumber: DWord;
    TxProduceIndex: DWord;
     TxConsumeIndex: DWord;
        RESERVED2:Array [1..10] Of DWord;
     TSV0: DWord;
     TSV1: DWord;
     RSV: DWord;
        RESERVED3: Array [1..3] Of DWord;
    FlowControlCounter: DWord;
     FlowControlStatus: DWord;
        RESERVED4: Array [1..34] Of DWord;
    RxFilterCtrl: DWord;           // Rx Filter Registers                */
    RxFilterWoLStatus: DWord;
    RxFilterWoLClear: DWord;
        RESERVED5: DWord;
    HashFilterL: DWord;
    HashFilterH: DWord;
        RESERVED6:Array [1..882] Of DWord;
     IntStatus: DWord;              // Module Control Registers           */
    IntEnable: DWord;
     IntClear: DWord;
     IntSet: DWord;
        RESERVED7: DWord;
    PowerDown: DWord;
        RESERVED8: DWord;
    Module_ID: DWord;
End;

 TNVICRegisters = packed record
  ISER: array[0..7] of longword;
   reserved0: array[0..23] of longword;
  ICER: array[0..7] of longword;
   reserved1: array[0..23] of longword;
  ISPR: array[0..7] of longword;
   reserved2: array[0..23] of longword;
  ICPR: array[0..7] of longword;
   reserved3: array[0..23] of longword;
  IABR: array[0..7] of longword;
   reserved4: array[0..55] of longword;
  IP: array[0..239] of longword;
   reserved5: array[0..643] of longword;
  STIR: longword;
 End;

 TSCBRegisters = packed record
  CPUID,                            {!< CPU ID Base Register                                     }
  ICSR,                            {!< Interrupt Control State Register                        }
  VTOR,                            {!< Vector Table Offset Register                            }
  AIRCR,                            {!< Application Interrupt / Reset Control Register           }
  SCR,                              {!< System Control Register                                 }
  CCR: longword;                    {!< Configuration Control Register                           }
  SHP: array[0..11] of byte;        {!< System Handlers Priority Registers (4-7, 8-11, 12-15)   }
  SHCSR,                            {!< System Handler Control and State Register               }
  CFSR,                            {!< Configurable Fault Status Register                      }
  HFSR,                            {!< Hard Fault Status Register                              }
  DFSR,                            {!< Debug Fault Status Register                              }
  MMFAR,                            {!< Mem Manage Address Register                             }
  BFAR,                            {!< Bus Fault Address Register                              }
  AFSR: longword;                  {!< Auxiliary Fault Status Register                          }
  PFR: array[0..1] of longword;    {!< Processor Feature Register                              }
  DFR,                              {!< Debug Feature Register                                   }
  ADR: longword;                    {!< Auxiliary Feature Register                               }
  MMFR: array[0..3] of longword;    {!< Memory Model Feature Register                           }
  ISAR: array[0..4] of longword;    {!< ISA Feature Register                                     }
 end;

TSysTickRegisters = Packed Record
  CTRL,
  RELOAD,
  VAL,
  CALIB: LongWord;
End;

// Based on CORE_CM3.H

///*****************************************************************************/
///                         Peripheral memory map                              */
///*****************************************************************************/

Const
 LPC_SCS_BASE        		= $E000E000;
 LPC_SCB_BASE          = (LPC_SCS_BASE + $0D00);      // System Control Block Base Address

/// Base addresses                                                             */

 LPC_FLASH_BASE        = ($00000000);
 LPC_RAM_BASE          = ($10000000);
 LPC_GPIO_BASE         = ($2009C000);
 LPC_APB0_BASE         = ($40000000);
 LPC_APB1_BASE         = ($40080000);
 LPC_AHB_BASE          = ($50000000);
 LPC_CM3_BASE          = ($E0000000);

/// APB0 peripherals                                                           */
 LPC_WDT_BASE          = (LPC_APB0_BASE + $00000);
 LPC_TIM0_BASE         = (LPC_APB0_BASE + $04000);
 LPC_TIM1_BASE         = (LPC_APB0_BASE + $08000);
 LPC_UART0_BASE        = (LPC_APB0_BASE + $0C000);
 LPC_UART1_BASE        = (LPC_APB0_BASE + $10000);
 LPC_PWM1_BASE         = (LPC_APB0_BASE + $18000);
 LPC_I2C0_BASE         = (LPC_APB0_BASE + $1C000);
 LPC_SPI_BASE          = (LPC_APB0_BASE + $20000);
 LPC_RTC_BASE          = (LPC_APB0_BASE + $24000);
 LPC_GPIOINT_BASE      = (LPC_APB0_BASE + $28080);
 LPC_PINCON_BASE       = (LPC_APB0_BASE + $2C000);
 LPC_SSP1_BASE         = (LPC_APB0_BASE + $30000);
 LPC_ADC_BASE          = (LPC_APB0_BASE + $34000);
 LPC_CANAF_RAM_BASE    = (LPC_APB0_BASE + $38000);
 LPC_CANAF_BASE        = (LPC_APB0_BASE + $3C000);
 LPC_CANCR_BASE        = (LPC_APB0_BASE + $40000);
 LPC_CAN1_BASE         = (LPC_APB0_BASE + $44000);
 LPC_CAN2_BASE         = (LPC_APB0_BASE + $48000);
 LPC_I2C1_BASE         = (LPC_APB0_BASE + $5C000);

/// APB1 peripherals                                                           */
 LPC_SSP0_BASE         = (LPC_APB1_BASE + $08000);
 LPC_DAC_BASE          = (LPC_APB1_BASE + $0C000);
 LPC_TIM2_BASE         = (LPC_APB1_BASE + $10000);
 LPC_TIM3_BASE         = (LPC_APB1_BASE + $14000);
 LPC_UART2_BASE        = (LPC_APB1_BASE + $18000);
 LPC_UART3_BASE        = (LPC_APB1_BASE + $1C000);
 LPC_I2C2_BASE         = (LPC_APB1_BASE + $20000);
 LPC_I2S_BASE          = (LPC_APB1_BASE + $28000);
 LPC_RIT_BASE          = (LPC_APB1_BASE + $30000);
 LPC_MCPWM_BASE        = (LPC_APB1_BASE + $38000);
 LPC_QEI_BASE          = (LPC_APB1_BASE + $3C000);
 LPC_SC_BASE           = (LPC_APB1_BASE + $7C000);

/// AHB peripherals                                                            */
 LPC_EMAC_BASE         = (LPC_AHB_BASE  + $00000);
 LPC_GPDMA_BASE        = (LPC_AHB_BASE  + $04000);
 LPC_GPDMACH0_BASE     = (LPC_AHB_BASE  + $04100);
 LPC_GPDMACH1_BASE     = (LPC_AHB_BASE  + $04120);
 LPC_GPDMACH2_BASE     = (LPC_AHB_BASE  + $04140);
 LPC_GPDMACH3_BASE     = (LPC_AHB_BASE  + $04160);
 LPC_GPDMACH4_BASE     = (LPC_AHB_BASE  + $04180);
 LPC_GPDMACH5_BASE     = (LPC_AHB_BASE  + $041A0);
 LPC_GPDMACH6_BASE     = (LPC_AHB_BASE  + $041C0);
 LPC_GPDMACH7_BASE     = (LPC_AHB_BASE  + $041E0);
 LPC_USB_BASE          = (LPC_AHB_BASE  + $0C000);

/// GPIOs                                                                      */
 LPC_GPIO0_BASE        = (LPC_GPIO_BASE + $00000);
 LPC_GPIO1_BASE        = (LPC_GPIO_BASE + $00020);
 LPC_GPIO2_BASE        = (LPC_GPIO_BASE + $00040);
 LPC_GPIO3_BASE        = (LPC_GPIO_BASE + $00060);
 LPC_GPIO4_BASE        = (LPC_GPIO_BASE + $00080);

///*****************************************************************************/
///                         Peripheral declaration                             */
///*****************************************************************************/

{$ALIGN 2}

Var
 LPC_SC                : TSCRegisters  Absolute (LPC_SC_BASE);
  LPC_SCB    : TSCBRegisters Absolute (LPC_SCB_BASE);
 
 LPC_GPIO0             : TGPIORegisters Absolute (LPC_GPIO0_BASE);
 LPC_GPIO1             : TGPIORegisters Absolute (LPC_GPIO1_BASE);
 LPC_GPIO2             : TGPIORegisters Absolute (LPC_GPIO2_BASE);
 LPC_GPIO3             : TGPIORegisters Absolute (LPC_GPIO3_BASE);
 LPC_GPIO4             : TGPIORegisters Absolute (LPC_GPIO4_BASE);
 LPC_WDT               : TWDTRegisters Absolute  (LPC_WDT_BASE);
 LPC_TIM0              : TTIMRegisters Absolute (LPC_TIM0_BASE);
 LPC_TIM1              : TTIMRegisters Absolute (LPC_TIM1_BASE);
 LPC_TIM2              : TTIMRegisters Absolute (LPC_TIM2_BASE);
 LPC_TIM3              : TTIMRegisters Absolute (LPC_TIM3_BASE);
 LPC_RIT               : TRITRegisters Absolute (LPC_RIT_BASE);
{
 LPC_UART0             : TUART0Registers Absolute (LPC_UART0_BASE);
 LPC_UART1             : TUART1Registers Absolute (LPC_UART1_BASE);
 LPC_UART2             : TUARTRegisters Absolute (LPC_UART2_BASE);
 LPC_UART3             : TUARTRegisters Absolute (LPC_UART3_BASE);
}
 LPC_SYSTICK		   : TSysTickRegisters  Absolute (LPC_SCS_BASE+$0010);
 LPC_NVIC: TNVICRegisters Absolute (LPC_SCS_BASE+$0100);

 LPC_PWM1              : TPWMRegisters Absolute (LPC_PWM1_BASE);
 LPC_I2C0              : TI2CRegisters Absolute (LPC_I2C0_BASE);
 LPC_I2C1              : TI2CRegisters Absolute (LPC_I2C1_BASE);
 LPC_I2C2              : TI2CRegisters Absolute (LPC_I2C2_BASE);
 LPC_I2S               : TI2SRegisters Absolute (LPC_I2S_BASE);
 LPC_SPI               : TSPIRegisters Absolute (LPC_SPI_BASE);
 LPC_RTC               : TRTCRegisters Absolute (LPC_RTC_BASE);
 LPC_GPIOINT           : TGPIOINTRegisters Absolute (LPC_GPIOINT_BASE);
 LPC_PINCON            : TPINCONRegisters Absolute (LPC_PINCON_BASE);
 LPC_SSP0              : TSSPRegisters Absolute (LPC_SSP0_BASE);
 LPC_SSP1              : TSSPRegisters Absolute (LPC_SSP1_BASE);
 LPC_ADC               : TADCRegisters Absolute  (LPC_ADC_BASE);
 LPC_DAC               : TDACRegisters Absolute  (LPC_DAC_BASE);
{
 LPC_CANAF_RAM         : TCANAF_RAMRegisters Absolute  (LPC_CANAF_RAM_BASE);
 LPC_CANAF             : TCANAFRegisters Absolute  (LPC_CANAF_BASE);
 LPC_CANCR             : TCANCR_RAMRegisters Absolute  (LPC_CANCR_BASE);
}
 LPC_CAN1              : TCANRegisters Absolute  (LPC_CAN1_BASE);
 LPC_CAN2              : TCANRegisters Absolute  (LPC_CAN2_BASE);
 LPC_MCPWM             : TMCPWMRegisters Absolute  (LPC_MCPWM_BASE);
 LPC_QEI               : TQEIRegisters Absolute  (LPC_QEI_BASE);
 LPC_EMAC              : TEMACRegisters Absolute  (LPC_EMAC_BASE);
 LPC_GPDMA             : TGPDMARegisters Absolute  (LPC_GPDMA_BASE);
 LPC_GPDMACH0          : TGPDMACHRegisters Absolute  (LPC_GPDMACH0_BASE);
 LPC_GPDMACH1          : TGPDMACHRegisters Absolute  (LPC_GPDMACH1_BASE);
 LPC_GPDMACH2          : TGPDMACHRegisters Absolute  (LPC_GPDMACH2_BASE);
 LPC_GPDMACH3          : TGPDMACHRegisters Absolute  (LPC_GPDMACH3_BASE);
 LPC_GPDMACH4          : TGPDMACHRegisters Absolute  (LPC_GPDMACH4_BASE);
 LPC_GPDMACH5          : TGPDMACHRegisters Absolute  (LPC_GPDMACH5_BASE);
 LPC_GPDMACH6          : TGPDMACHRegisters Absolute  (LPC_GPDMACH6_BASE);
 LPC_GPDMACH7          : TGPDMACHRegisters Absolute  (LPC_GPDMACH7_BASE);
 LPC_USB               : TUSBRegisters Absolute  (LPC_USB_BASE);

implementation

procedure NMI_interrupt; external name 'NMI_interrupt';
procedure Hardfault_interrupt; external name 'Hardfault_interrupt';
procedure MemManage_interrupt; external name 'MemManage_interrupt';
procedure BusFault_interrupt; external name 'BusFault_interrupt';
procedure UsageFault_interrupt; external name 'UsageFault_interrupt';
procedure SWI_interrupt; external name 'SWI_interrupt';
procedure DebugMonitor_interrupt; external name 'DebugMonitor_interrupt';
procedure PendingSV_interrupt; external name 'PendingSV_interrupt';
procedure SysTick_interrupt; external name 'SysTick_interrupt';
procedure Watchdog_Interrupt; external name 'Watchdog_Interrupt';
procedure Timer0_Interrupt; external name 'Timer0_Interrupt';
procedure Timer1_Interrupt; external name 'Timer1_Interrupt';
procedure Timer2_Interrupt; external name 'Timer2_Interrupt';
procedure Timer3_Interrupt; external name 'Timer3_Interrupt';
procedure UART0_Interrupt; external name 'UART0_Interrupt';
procedure UART1_Interrupt; external name 'UART1_Interrupt';
procedure UART2_Interrupt; external name 'UART2_Interrupt';
procedure UART3_Interrupt; external name 'UART3_Interrupt';
procedure PWM1_Interrupt; external name 'PWM1_Interrupt';
procedure I2C0_Interrupt; external name 'I2C0_Interrupt';
procedure I2C1_Interrupt; external name 'I2C1_Interrupt';
procedure I2C2_Interrupt; external name 'I2C2_Interrupt';
procedure SPI_Interrupt; external name 'SPI_Interrupt';
procedure SSP0_Interrupt; external name 'SSP0_Interrupt';
procedure SSP1_Interrupt; external name 'SSP1_Interrupt';
procedure PLL0_Interrupt; external name 'PLL0_Interrupt';
procedure RTC_Interrupt; external name 'RTC_Interrupt';
procedure EINT0_Interrupt; external name 'EINT0_Interrupt';
procedure EINT1_Interrupt; external name 'EINT1_Interrupt';
procedure EINT2_Interrupt; external name 'EINT2_Interrupt';
procedure EINT3_Interrupt; external name 'EINT3_Interrupt';
procedure ADC_Interrupt; external name 'ADC_Interrupt';
procedure BOD_Interrupt; external name 'BOD_Interrupt';
procedure USB_Interrupt; external name 'USB_Interrupt';
procedure CAN_Interrupt; external name 'CAN_Interrupt';
procedure HPDMA_Interrupt; external name 'HPDMA_Interrupt';
procedure I2C_Interrupt; external name 'I2C_Interrupt';
procedure Ethernet_Interrupt; external name 'Ethernet_Interrupt';
procedure RITINT_Interrupt; external name 'RITINT_Interrupt';
procedure MotorControlPWM_Interrupt; external name 'MotorControlPWM_Interrupt';
procedure QuadratureEncoder_Interrupt; external name 'QuadratureEncoder_Interrupt';
procedure PLL1_Interrupt; external name 'PLL1_Interrupt';
procedure USBActivity_Interrupt; external name 'USBActivity_Interrupt';
procedure CanActivity_Interrupt; external name 'CanActivity_Interrupt';

{$i cortexm3_start.inc}

procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
interrupt_vectors:
  .long _stack_top            // stack top address
  .long Startup
  .long NMI_interrupt
  .long Hardfault_interrupt
  .long MemManage_interrupt
  .long BusFault_interrupt
  .long UsageFault_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long SWI_interrupt
  .long DebugMonitor_interrupt
  .long 0
  .long PendingSV_interrupt
  .long SysTick_interrupt
  
  .long Watchdog_Interrupt
  .long Timer0_Interrupt
  .long Timer1_Interrupt
  .long Timer2_Interrupt
  .long Timer3_Interrupt
  .long UART0_Interrupt
  .long UART1_Interrupt
  .long UART2_Interrupt
  .long UART3_Interrupt
  .long PWM1_Interrupt
  .long I2C0_Interrupt
  .long I2C1_Interrupt
  .long I2C2_Interrupt
  .long SPI_Interrupt
  .long SSP0_Interrupt
  .long SSP1_Interrupt
  .long PLL0_Interrupt
  .long RTC_Interrupt
  .long EINT0_Interrupt
  .long EINT1_Interrupt
  .long EINT2_Interrupt
  .long EINT3_Interrupt
  .long ADC_Interrupt
  .long BOD_Interrupt
  .long USB_Interrupt
  .long CAN_Interrupt
  .long HPDMA_Interrupt
  .long I2C_Interrupt
  .long Ethernet_Interrupt
  .long RITINT_Interrupt
  .long MotorControlPWM_Interrupt
  .long QuadratureEncoder_Interrupt
  .long PLL1_Interrupt
  .long USBActivity_Interrupt
  .long CanActivity_Interrupt
  
  .weak NMI_interrupt
  .weak Hardfault_interrupt
  .weak MemManage_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak SWI_interrupt
  .weak DebugMonitor_interrupt
  .weak PendingSV_interrupt
  .weak SysTick_interrupt
  .weak Watchdog_Interrupt
  .weak Timer0_Interrupt
  .weak Timer1_Interrupt
  .weak Timer2_Interrupt
  .weak Timer3_Interrupt
  .weak UART0_Interrupt
  .weak UART1_Interrupt
  .weak UART2_Interrupt
  .weak UART3_Interrupt
  .weak PWM1_Interrupt
  .weak I2C0_Interrupt
  .weak I2C1_Interrupt
  .weak I2C2_Interrupt
  .weak SPI_Interrupt
  .weak SSP0_Interrupt
  .weak SSP1_Interrupt
  .weak PLL0_Interrupt
  .weak RTC_Interrupt
  .weak EINT0_Interrupt
  .weak EINT1_Interrupt
  .weak EINT2_Interrupt
  .weak EINT3_Interrupt
  .weak ADC_Interrupt
  .weak BOD_Interrupt
  .weak USB_Interrupt
  .weak CAN_Interrupt
  .weak HPDMA_Interrupt
  .weak I2C_Interrupt
  .weak Ethernet_Interrupt
  .weak RITINT_Interrupt
  .weak MotorControlPWM_Interrupt
  .weak QuadratureEncoder_Interrupt
  .weak PLL1_Interrupt
  .weak USBActivity_Interrupt
  .weak CanActivity_Interrupt
  
    .set NMI_interrupt, Startup
  .set Hardfault_interrupt, Startup
  .set MemManage_interrupt, Startup
  .set BusFault_interrupt, Startup
  .set UsageFault_interrupt, Startup
  .set SWI_interrupt, Startup
  .set DebugMonitor_interrupt, Startup
  .set PendingSV_interrupt, Startup
  .set SysTick_interrupt, Startup
  .set Watchdog_Interrupt, Startup
  .set Timer0_Interrupt, Startup
  .set Timer1_Interrupt, Startup
  .set Timer2_Interrupt, Startup
  .set Timer3_Interrupt, Startup
  .set UART0_Interrupt, Startup
  .set UART1_Interrupt, Startup
  .set UART2_Interrupt, Startup
  .set UART3_Interrupt, Startup
  .set PWM1_Interrupt, Startup
  .set I2C0_Interrupt, Startup
  .set I2C1_Interrupt, Startup
  .set I2C2_Interrupt, Startup
  .set SPI_Interrupt, Startup
  .set SSP0_Interrupt, Startup
  .set SSP1_Interrupt, Startup
  .set PLL0_Interrupt, Startup
  .set RTC_Interrupt, Startup
  .set EINT0_Interrupt, Startup
  .set EINT1_Interrupt, Startup
  .set EINT2_Interrupt, Startup
  .set EINT3_Interrupt, Startup
  .set ADC_Interrupt, Startup
  .set BOD_Interrupt, Startup
  .set USB_Interrupt, Startup
  .set CAN_Interrupt, Startup
  .set HPDMA_Interrupt, Startup
  .set I2C_Interrupt, Startup
  .set Ethernet_Interrupt, Startup
  .set RITINT_Interrupt, Startup
  .set MotorControlPWM_Interrupt, Startup
  .set QuadratureEncoder_Interrupt, Startup
  .set PLL1_Interrupt, Startup
  .set USBActivity_Interrupt, Startup
  .set CanActivity_Interrupt, Startup
  
  .text
end;

end.


