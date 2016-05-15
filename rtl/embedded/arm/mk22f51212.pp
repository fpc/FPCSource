unit mk22f51212;
interface
{$PACKRECORDS 2}
{$GOTO ON}
{$MODESWITCH ADVANCEDRECORDS}
// ** ###################################################################
// **     Compilers:           ARM Compiler
// **                          Freescale C/C++ for Embedded ARM
// **                          GNU C Compiler
// **                          GNU C Compiler - CodeSourcery Sourcery G++
// **                          IAR ANSI C/C++ Compiler for ARM
// **
// **     Reference manual:    K22P121M120SF7RM, Rev.0.61, Jan 10, 2014
// **     Version:             rev. 2.3, 2014-01-13
// **
// **     Abstract:
// **         CMSIS Peripheral Access Layer for MK22F51212
// **
// **     Copyright: 1997 - 2014 Freescale, Inc. All Rights Reserved.
// **
// **     http:                 www.freescale.com
// **     mail:                 support@freescale.com
// **
// **     Revisions:
// **     - rev. 1.0 (2013-07-23)
// **         Initial version.
// **     - rev. 1.1 (2013-09-17)
// **         RM rev. 0.4 update.
// **     - rev. 2.0 (2013-10-29)
// **         Register accessor macros added to the memory map.
// **         Symbols for Processor Expert memory map compatibility added to the memory map.
// **         Startup file for gcc has been updated according to CMSIS 3.2.
// **         System initialization updated.
// **     - rev. 2.1 (2013-10-29)
// **         Definition of BITBAND macros updated to support peripherals with 32-bit acces disabled.
// **     - rev. 2.2 (2013-12-20)
// **         Update according to reference manual rev. 0.6,
// **     - rev. 2.3 (2014-01-13)
// **         Update according to reference manual rev. 0.61,
// **
// ** ###################################################################
// !
// * @file MK22F51212.h
// * @version 2.3
// * @date 2014-01-13
// CMSIS Peripheral Access Layer for MK22F51212
// *
// * CMSIS Peripheral Access Layer for MK22F51212
// ----------------------------------------------------------------------------
// -- MCU activation
// ----------------------------------------------------------------------------
// Prevention from multiple including the same memory map
// Check if another memory map has not been also included
// * Memory map major version (memory maps with equal major version number are
// * compatible)
// * Memory map minor version
// Macro to calculate address of an aliased word in the peripheral
// *        bitband area for a peripheral register and bit (bit band region 0x40000000 to
// *        0x400FFFFF).
// * @param Reg Register to access.
// * @param Bit Bit number to access.
// * @return  Address of the aliased word in the peripheral bitband area.
// Macro to access a single bit of a peripheral register (bit band region
// *        0x40000000 to 0x400FFFFF) using the bit-band alias region access. Can
// *        be used for peripherals with 32bit access allowed.
// * @param Reg Register to access.
// * @param Bit Bit number to access.
// * @return Value of the targeted bit in the bit band region.
// Macro to access a single bit of a peripheral register (bit band region
// *        0x40000000 to 0x400FFFFF) using the bit-band alias region access. Can
// *        be used for peripherals with 16bit access allowed.
// * @param Reg Register to access.
// * @param Bit Bit number to access.
// * @return Value of the targeted bit in the bit band region.
// Macro to access a single bit of a peripheral register (bit band region
// *        0x40000000 to 0x400FFFFF) using the bit-band alias region access. Can
// *        be used for peripherals with 8bit access allowed.
// * @param Reg Register to access.
// * @param Bit Bit number to access.
// * @return Value of the targeted bit in the bit band region.
// ----------------------------------------------------------------------------
// -- Interrupt vector numbers
// ----------------------------------------------------------------------------
// !
// * Interrupt Number Definitions

type
  TIRQn_Enum   = (
    NonMaskableInt_IRQn = -14,        // *< Non Maskable Interrupt
    HardFault_IRQn = -13,             // *< Cortex-M4 SV Hard Fault Interrupt
    MemoryManagement_IRQn = -12,      // *< Cortex-M4 Memory Management Interrupt
    BusFault_IRQn = -11,              // *< Cortex-M4 Bus Fault Interrupt
    UsageFault_IRQn = -10,            // *< Cortex-M4 Usage Fault Interrupt
    SVCall_IRQn = -5,                 // *< Cortex-M4 SV Call Interrupt
    DebugMonitor_IRQn = -4,           // *< Cortex-M4 Debug Monitor Interrupt
    PendSV_IRQn = -2,                 // *< Cortex-M4 Pend SV Interrupt
    SysTick_IRQn = -1,                // *< Cortex-M4 System Tick Interrupt
    DMA0_IRQn  = 0,                   // *< DMA Channel 0 Transfer Complete
    DMA1_IRQn  = 1,                   // *< DMA Channel 1 Transfer Complete
    DMA2_IRQn  = 2,                   // *< DMA Channel 2 Transfer Complete
    DMA3_IRQn  = 3,                   // *< DMA Channel 3 Transfer Complete
    DMA4_IRQn  = 4,                   // *< DMA Channel 4 Transfer Complete
    DMA5_IRQn  = 5,                   // *< DMA Channel 5 Transfer Complete
    DMA6_IRQn  = 6,                   // *< DMA Channel 6 Transfer Complete
    DMA7_IRQn  = 7,                   // *< DMA Channel 7 Transfer Complete
    DMA8_IRQn  = 8,                   // *< DMA Channel 8 Transfer Complete
    DMA9_IRQn  = 9,                   // *< DMA Channel 9 Transfer Complete
    DMA10_IRQn = 10,                  // *< DMA Channel 10 Transfer Complete
    DMA11_IRQn = 11,                  // *< DMA Channel 11 Transfer Complete
    DMA12_IRQn = 12,                  // *< DMA Channel 12 Transfer Complete
    DMA13_IRQn = 13,                  // *< DMA Channel 13 Transfer Complete
    DMA14_IRQn = 14,                  // *< DMA Channel 14 Transfer Complete
    DMA15_IRQn = 15,                  // *< DMA Channel 15 Transfer Complete
    DMA_Error_IRQn = 16,              // *< DMA Error Interrupt
    MCM_IRQn   = 17,                  // *< Normal Interrupt
    FTF_IRQn   = 18,                  // *< FTFA Command complete interrupt
    Read_Collision_IRQn = 19,         // *< Read Collision Interrupt
    LVD_LVW_IRQn = 20,                // *< Low Voltage Detect, Low Voltage Warning
    LLW_IRQn   = 21,                  // *< Low Leakage Wakeup
    Watchdog_IRQn = 22,               // *< WDOG Interrupt
    RNG_IRQn   = 23,                  // *< RNG Interrupt
    I2C0_IRQn  = 24,                  // *< I2C0 interrupt
    I2C1_IRQn  = 25,                  // *< I2C1 interrupt
    SPI0_IRQn  = 26,                  // *< SPI0 Interrupt
    SPI1_IRQn  = 27,                  // *< SPI1 Interrupt
    I2S0_Tx_IRQn = 28,                // *< I2S0 transmit interrupt
    I2S0_Rx_IRQn = 29,                // *< I2S0 receive interrupt
    LPUART0_IRQn = 30,                // *< LPUART0 status/error interrupt
    UART0_RX_TX_IRQn = 31,            // *< UART0 Receive/Transmit interrupt
    UART0_ERR_IRQn = 32,              // *< UART0 Error interrupt
    UART1_RX_TX_IRQn = 33,            // *< UART1 Receive/Transmit interrupt
    UART1_ERR_IRQn = 34,              // *< UART1 Error interrupt
    UART2_RX_TX_IRQn = 35,            // *< UART2 Receive/Transmit interrupt
    UART2_ERR_IRQn = 36,              // *< UART2 Error interrupt
    RESERVED53_IRQn = 37,             // *< Reserved interrupt 53
    RESERVED54_IRQn = 38,             // *< Reserved interrupt 54
    ADC0_IRQn  = 39,                  // *< ADC0 interrupt
    CMP0_IRQn  = 40,                  // *< CMP0 interrupt
    CMP1_IRQn  = 41,                  // *< CMP1 interrupt
    FTM0_IRQn  = 42,                  // *< FTM0 fault, overflow and channels interrupt
    FTM1_IRQn  = 43,                  // *< FTM1 fault, overflow and channels interrupt
    FTM2_IRQn  = 44,                  // *< FTM2 fault, overflow and channels interrupt
    RESERVED61_IRQn = 45,             // *< Reserved interrupt 61
    RTC_IRQn   = 46,                  // *< RTC interrupt
    RTC_Seconds_IRQn = 47,            // *< RTC seconds interrupt
    PIT0_IRQn  = 48,                  // *< PIT timer channel 0 interrupt
    PIT1_IRQn  = 49,                  // *< PIT timer channel 1 interrupt
    PIT2_IRQn  = 50,                  // *< PIT timer channel 2 interrupt
    PIT3_IRQn  = 51,                  // *< PIT timer channel 3 interrupt
    PDB0_IRQn  = 52,                  // *< PDB0 Interrupt
    USB0_IRQn  = 53,                  // *< USB0 interrupt
    RESERVED70_IRQn = 54,             // *< Reserved interrupt 70
    RESERVED71_IRQn = 55,             // *< Reserved interrupt 71
    DAC0_IRQn  = 56,                  // *< DAC0 interrupt
    MCG_IRQn   = 57,                  // *< MCG Interrupt
    LPTimer_IRQn = 58,                // *< LPTimer interrupt
    PORTA_IRQn = 59,                  // *< Port A interrupt
    PORTB_IRQn = 60,                  // *< Port B interrupt
    PORTC_IRQn = 61,                  // *< Port C interrupt
    PORTD_IRQn = 62,                  // *< Port D interrupt
    PORTE_IRQn = 63,                  // *< Port E interrupt
    SWI_IRQn   = 64,                  // *< Software interrupt
    RESERVED81_IRQn = 65,             // *< Reserved interrupt 81
    RESERVED82_IRQn = 66,             // *< Reserved interrupt 82
    RESERVED83_IRQn = 67,             // *< Reserved interrupt 83
    RESERVED84_IRQn = 68,             // *< Reserved interrupt 84
    RESERVED85_IRQn = 69,             // *< Reserved interrupt 85
    RESERVED86_IRQn = 70,             // *< Reserved interrupt 86
    FTM3_IRQn  = 71,                  // *< FTM3 fault, overflow and channels interrupt
    DAC1_IRQn  = 72,                  // *< DAC1 interrupt
    ADC1_IRQn  = 73,                  // *< ADC1 interrupt
    RESERVED90_IRQn = 74,             // *< Reserved Interrupt 90
    RESERVED91_IRQn = 75,             // *< Reserved Interrupt 91
    RESERVED92_IRQn = 76,             // *< Reserved Interrupt 92
    RESERVED93_IRQn = 77,             // *< Reserved Interrupt 93
    RESERVED94_IRQn = 78,             // *< Reserved Interrupt 94
    RESERVED95_IRQn = 79,             // *< Reserved Interrupt 95
    RESERVED96_IRQn = 80,             // *< Reserved Interrupt 96
    RESERVED97_IRQn = 81,             // *< Reserved Interrupt 97
    RESERVED98_IRQn = 82,             // *< Reserved Interrupt 98
    RESERVED99_IRQn = 83,             // *< Reserved Interrupt 99
    RESERVED100_IRQn = 84,            // *< Reserved Interrupt 100
    RESERVED101_IRQn = 85             // *< Reserved Interrupt 101
  );

  TADC_Registers = record
    SC1        : array[0..1] of longword; // *< ADC Status and Control Registers 1, array offset: 0x0, array step: 0x4
    CFG1       : longword;            // *< ADC Configuration Register 1, offset: 0x8
    CFG2       : longword;            // *< ADC Configuration Register 2, offset: 0xC
    R          : array[0..1] of longword; // *< ADC Data Result Register, array offset: 0x10, array step: 0x4
    CV1        : longword;            // *< Compare Value Registers, offset: 0x18
    CV2        : longword;            // *< Compare Value Registers, offset: 0x1C
    SC2        : longword;            // *< Status and Control Register 2, offset: 0x20
    SC3        : longword;            // *< Status and Control Register 3, offset: 0x24
    OFS        : longword;            // *< ADC Offset Correction Register, offset: 0x28
    PG         : longword;            // *< ADC Plus-Side Gain Register, offset: 0x2C
    MG         : longword;            // *< ADC Minus-Side Gain Register, offset: 0x30
    CLPD       : longword;            // *< ADC Plus-Side General Calibration Value Register, offset: 0x34
    CLPS       : longword;            // *< ADC Plus-Side General Calibration Value Register, offset: 0x38
    CLP4       : longword;            // *< ADC Plus-Side General Calibration Value Register, offset: 0x3C
    CLP3       : longword;            // *< ADC Plus-Side General Calibration Value Register, offset: 0x40
    CLP2       : longword;            // *< ADC Plus-Side General Calibration Value Register, offset: 0x44
    CLP1       : longword;            // *< ADC Plus-Side General Calibration Value Register, offset: 0x48
    CLP0       : longword;            // *< ADC Plus-Side General Calibration Value Register, offset: 0x4C
    RESERVED_0 : array[0..3] of byte;
    CLMD       : longword;            // *< ADC Minus-Side General Calibration Value Register, offset: 0x54
    CLMS       : longword;            // *< ADC Minus-Side General Calibration Value Register, offset: 0x58
    CLM4       : longword;            // *< ADC Minus-Side General Calibration Value Register, offset: 0x5C
    CLM3       : longword;            // *< ADC Minus-Side General Calibration Value Register, offset: 0x60
    CLM2       : longword;            // *< ADC Minus-Side General Calibration Value Register, offset: 0x64
    CLM1       : longword;            // *< ADC Minus-Side General Calibration Value Register, offset: 0x68
    CLM0       : longword;            // *< ADC Minus-Side General Calibration Value Register, offset: 0x6C
  end;

const
  ADC0_BASE    = $4003B000;

var
  ADC0         : TADC_Registers absolute ADC0_BASE;

const
  ADC1_BASE    = $40027000;

var
  ADC1         : TADC_Registers absolute ADC1_BASE;

type
  TCMP_Registers = record
    CR0        : byte;                // *< CMP Control Register 0, offset: 0x0
    CR1        : byte;                // *< CMP Control Register 1, offset: 0x1
    FPR        : byte;                // *< CMP Filter Period Register, offset: 0x2
    SCR        : byte;                // *< CMP Status and Control Register, offset: 0x3
    DACCR      : byte;                // *< DAC Control Register, offset: 0x4
    MUXCR      : byte;                // *< MUX Control Register, offset: 0x5
  end;

const
  CMP0_BASE    = $40073000;

var
  CMP0         : TCMP_Registers absolute CMP0_BASE;

const
  CMP1_BASE    = $40073008;

var
  CMP1         : TCMP_Registers absolute CMP1_BASE;

type
  TCRC_Registers = record
    DATA       : longword;  // *< CRC Data register, offset: 0x0
    GPOLY      : longword;  // *< CRC Polynomial register, offset: 0x4
    CTRL       : longword;  // *< CRC Control register, offset: 0x8
  end;

const
  CRC_BASE     = $40032000;

var
  CRC0         : TCRC_Registers absolute CRC_BASE;

type
  TDAC_DAT         = record
    DATL       : byte;                // *< DAC Data Low Register, array offset: 0x0, array step: 0x2
    DATH       : byte;                // *< DAC Data High Register, array offset: 0x1, array step: 0x2
  end;

  TDAC_Registers = record
    DAT        : array[0..15] of TDAC_DAT;
    SR         : byte;                // *< DAC Status Register, offset: 0x20
    C0         : byte;                // *< DAC Control Register, offset: 0x21
    C1         : byte;                // *< DAC Control Register 1, offset: 0x22
    C2         : byte;                // *< DAC Control Register 2, offset: 0x23
  end;

const
  DAC0_BASE    = $4003F000;

var
  DAC0         : TDAC_Registers absolute DAC0_BASE;

const
  DAC1_BASE    = $40028000;

var
  DAC1         : TDAC_Registers absolute DAC1_BASE;

type
  TDMA_TCD         = record
    SADDR      : longword;            // *< TCD Source Address, array offset: 0x1000, array step: 0x20
    SOFF       : word;                // *< TCD Signed Source Address Offset, array offset: 0x1004, array step: 0x20
    ATTR       : word;                // *< TCD Transfer Attributes, array offset: 0x1006, array step: 0x20
    NBYTES_MLNO: longword;            // *< TCD Minor Byte Count (Minor Loop Disabled), array offset: 0x1008, array step: 0x20
    SLAST      : longword;            // *< TCD Last Source Address Adjustment, array offset: 0x100C, array step: 0x20
    DADDR      : longword;            // *< TCD Destination Address, array offset: 0x1010, array step: 0x20
    DOFF       : word;                // *< TCD Signed Destination Address Offset, array offset: 0x1014, array step: 0x20
    CITER_ELINKNO : word;             // *< TCD Current Minor Loop Link, Major Loop Count (Channel Linking Disabled), array offset: 0x1016, array step: 0x20
    DLAST_SGA  : longword;            // *< TCD Last Destination Address Adjustment/Scatter Gather Address, array offset: 0x1018, array step: 0x20
    CSR        : word;                // *< TCD Control and Status, array offset: 0x101C, array step: 0x20
    BITER_ELINKNO : word;             // *< TCD Beginning Minor Loop Link, Major Loop Count (Channel Linking Disabled), array offset: 0x101E, array step: 0x20
  end;

  TDMA_Registers = record
    CR         : longword;            // *< Control Register, offset: 0x0
    ES         : longword;            // *< Error Status Register, offset: 0x4
    RESERVED_0 : array[0..3] of byte;
    ERQ        : longword;            // *< Enable Request Register, offset: 0xC
    RESERVED_1 : array[0..3] of byte;
    EEI        : longword;            // *< Enable Error Interrupt Register, offset: 0x14
    CEEI       : byte;                // *< Clear Enable Error Interrupt Register, offset: 0x18
    SEEI       : byte;                // *< Set Enable Error Interrupt Register, offset: 0x19
    CERQ       : byte;                // *< Clear Enable Request Register, offset: 0x1A
    SERQ       : byte;                // *< Set Enable Request Register, offset: 0x1B
    CDNE       : byte;                // *< Clear DONE Status Bit Register, offset: 0x1C
    SSRT       : byte;                // *< Set START Bit Register, offset: 0x1D
    CERR       : byte;                // *< Clear Error Register, offset: 0x1E
    CINT       : byte;                // *< Clear Interrupt Request Register, offset: 0x1F
    RESERVED_2 : array[0..3] of byte;
    INT        : longword;            // *< Interrupt Request Register, offset: 0x24
    RESERVED_3 : array[0..3] of byte;
    ERR        : longword;            // *< Error Register, offset: 0x2C
    RESERVED_4 : array[0..3] of byte;
    HRS        : longword;            // *< Hardware Request Status Register, offset: 0x34
    RESERVED_5 : array[0..11] of byte;
    EARS       : longword;            // *< Enable Asynchronous Request in Stop Register, offset: 0x44
    RESERVED_6 : array[0..183] of byte;
    DCHPRI3    : byte;                // *< Channel n Priority Register, offset: 0x100
    DCHPRI2    : byte;                // *< Channel n Priority Register, offset: 0x101
    DCHPRI1    : byte;                // *< Channel n Priority Register, offset: 0x102
    DCHPRI0    : byte;                // *< Channel n Priority Register, offset: 0x103
    DCHPRI7    : byte;                // *< Channel n Priority Register, offset: 0x104
    DCHPRI6    : byte;                // *< Channel n Priority Register, offset: 0x105
    DCHPRI5    : byte;                // *< Channel n Priority Register, offset: 0x106
    DCHPRI4    : byte;                // *< Channel n Priority Register, offset: 0x107
    DCHPRI11   : byte;                // *< Channel n Priority Register, offset: 0x108
    DCHPRI10   : byte;                // *< Channel n Priority Register, offset: 0x109
    DCHPRI9    : byte;                // *< Channel n Priority Register, offset: 0x10A
    DCHPRI8    : byte;                // *< Channel n Priority Register, offset: 0x10B
    DCHPRI15   : byte;                // *< Channel n Priority Register, offset: 0x10C
    DCHPRI14   : byte;                // *< Channel n Priority Register, offset: 0x10D
    DCHPRI13   : byte;                // *< Channel n Priority Register, offset: 0x10E
    DCHPRI12   : byte;                // *< Channel n Priority Register, offset: 0x10F
    RESERVED_7 : array[0..3823] of byte;
    TCD        : array[0..15] of TDMA_TCD;
  end;

const
  DMA_BASE     = $40008000;

var
  DMA0         : TDMA_Registers absolute DMA_BASE;

type
  TDMAMUX_Registers = record
    CHCFG      : array[0..15] of byte; // *< Channel Configuration register, array offset: 0x0, array step: 0x1
  end;

const
  DMAMUX_BASE  = $40021000;

var
  DMAMUX       : TDMAMUX_Registers absolute DMAMUX_BASE;

type
  TEWM_Registers = record
    CTRL       : byte;                // *< Control Register, offset: 0x0
    SERV       : byte;                // *< Service Register, offset: 0x1
    CMPL       : byte;                // *< Compare Low Register, offset: 0x2
    CMPH       : byte;                // *< Compare High Register, offset: 0x3
    RESERVED_0 : array[0..0] of byte;
    CLKPRESCALER : byte;              // *< Clock Prescaler Register, offset: 0x5
  end;

const
  EWM_BASE     = $40061000;

var
  EWM          : TEWM_Registers absolute EWM_BASE;

type
  TFB_CS       = record
    CSAR       : longword;            // *< Chip Select Address Register, array offset: 0x0, array step: 0xC
    CSMR       : longword;            // *< Chip Select Mask Register, array offset: 0x4, array step: 0xC
    CSCR       : longword;            // *< Chip Select Control Register, array offset: 0x8, array step: 0xC
  end;

  TFB_Registers = record
    CS         : array[0..5] of TFB_CS;
    RESERVED_0 : array[0..23] of byte;
    CSPMCR     : longword;            // *< Chip Select port Multiplexing Control Register, offset: 0x60
  end;

const
  FB_BASE      = $4000C000;

var
  FB           : TFB_Registers absolute FB_BASE;

type
  TFMC_SET     = record
    DATA_U     : longword;            // *< Cache Data Storage (upper word), array offset: 0x200, array step: index*0x40, index2*0x8
    DATA_L     : longword;            // *< Cache Data Storage (lower word), array offset: 0x204, array step: index*0x40, index2*0x8
  end;

  TFMC_Registers = record
    PFAPR      : longword;            // *< Flash Access Protection Register, offset: 0x0
    PFB0CR     : longword;            // *< Flash Bank 0 Control Register, offset: 0x4
    PFB1CR     : longword;            // *< Flash Bank 1 Control Register, offset: 0x8
    RESERVED_0 : array[0..243] of byte;
    TAGVDW0S   : array[0..7] of longword; // *< Cache Tag Storage, array offset: 0x100, array step: 0x4
    TAGVDW1S   : array[0..7] of longword; // *< Cache Tag Storage, array offset: 0x120, array step: 0x4
    TAGVDW2S   : array[0..7] of longword; // *< Cache Tag Storage, array offset: 0x140, array step: 0x4
    TAGVDW3S   : array[0..7] of longword; // *< Cache Tag Storage, array offset: 0x160, array step: 0x4
    RESERVED_1 : array[0..127] of byte;
    &SET       : array[0..3] of TFMC_SET;
  end;

const
  FMC_BASE     = $4001F000;

var
  FMC          : TFMC_Registers absolute FMC_BASE;

type
  TFTFA_Registers = record
    FSTAT      : byte;                // *< Flash Status Register, offset: 0x0
    FCNFG      : byte;                // *< Flash Configuration Register, offset: 0x1
    FSEC       : byte;                // *< Flash Security Register, offset: 0x2
    FOPT       : byte;                // *< Flash Option Register, offset: 0x3
    FCCOB3     : byte;                // *< Flash Common Command Object Registers, offset: 0x4
    FCCOB2     : byte;                // *< Flash Common Command Object Registers, offset: 0x5
    FCCOB1     : byte;                // *< Flash Common Command Object Registers, offset: 0x6
    FCCOB0     : byte;                // *< Flash Common Command Object Registers, offset: 0x7
    FCCOB7     : byte;                // *< Flash Common Command Object Registers, offset: 0x8
    FCCOB6     : byte;                // *< Flash Common Command Object Registers, offset: 0x9
    FCCOB5     : byte;                // *< Flash Common Command Object Registers, offset: 0xA
    FCCOB4     : byte;                // *< Flash Common Command Object Registers, offset: 0xB
    FCCOBB     : byte;                // *< Flash Common Command Object Registers, offset: 0xC
    FCCOBA     : byte;                // *< Flash Common Command Object Registers, offset: 0xD
    FCCOB9     : byte;                // *< Flash Common Command Object Registers, offset: 0xE
    FCCOB8     : byte;                // *< Flash Common Command Object Registers, offset: 0xF
    FPROT3     : byte;                // *< Program Flash Protection Registers, offset: 0x10
    FPROT2     : byte;                // *< Program Flash Protection Registers, offset: 0x11
    FPROT1     : byte;                // *< Program Flash Protection Registers, offset: 0x12
    FPROT0     : byte;                // *< Program Flash Protection Registers, offset: 0x13
    RESERVED_0 : array[0..3] of byte;
    XACCH3     : byte;                // *< Execute-only Access Registers, offset: 0x18
    XACCH2     : byte;                // *< Execute-only Access Registers, offset: 0x19
    XACCH1     : byte;                // *< Execute-only Access Registers, offset: 0x1A
    XACCH0     : byte;                // *< Execute-only Access Registers, offset: 0x1B
    XACCL3     : byte;                // *< Execute-only Access Registers, offset: 0x1C
    XACCL2     : byte;                // *< Execute-only Access Registers, offset: 0x1D
    XACCL1     : byte;                // *< Execute-only Access Registers, offset: 0x1E
    XACCL0     : byte;                // *< Execute-only Access Registers, offset: 0x1F
    SACCH3     : byte;                // *< Supervisor-only Access Registers, offset: 0x20
    SACCH2     : byte;                // *< Supervisor-only Access Registers, offset: 0x21
    SACCH1     : byte;                // *< Supervisor-only Access Registers, offset: 0x22
    SACCH0     : byte;                // *< Supervisor-only Access Registers, offset: 0x23
    SACCL3     : byte;                // *< Supervisor-only Access Registers, offset: 0x24
    SACCL2     : byte;                // *< Supervisor-only Access Registers, offset: 0x25
    SACCL1     : byte;                // *< Supervisor-only Access Registers, offset: 0x26
    SACCL0     : byte;                // *< Supervisor-only Access Registers, offset: 0x27
    FACSS      : byte;                // *< Flash Access Segment Size Register, offset: 0x28
    RESERVED_1 : array[0..1] of byte;
    FACSN      : byte;                // *< Flash Access Segment Number Register, offset: 0x2B
  end;

const
  FTFA_BASE    = $40020000;

var
  FTFA         : TFTFA_Registers absolute FTFA_BASE;

type
  TFMT_CONTROLS    = record
    CnSC       : longword;            // *< Channel (n) Status And Control, array offset: 0xC, array step: 0x8
    CnV        : longword;            // *< Channel (n) Value, array offset: 0x10, array step: 0x8
  end;

  TFTM_Registers = record
    SC         : longword;            // *< Status And Control, offset: 0x0
    CNT        : longword;            // *< Counter, offset: 0x4
    &MOD       : longword;            // *< Modulo, offset: 0x8
    CONTROLS   : array[0..7] of TFMT_CONTROLS;
    CNTIN      : longword;            // *< Counter Initial Value, offset: 0x4C
    STATUS     : longword;            // *< Capture And Compare Status, offset: 0x50
    MODE       : longword;            // *< Features Mode Selection, offset: 0x54
    SYNC       : longword;            // *< Synchronization, offset: 0x58
    OUTINIT    : longword;            // *< Initial State For Channels Output, offset: 0x5C
    OUTMASK    : longword;            // *< Output Mask, offset: 0x60
    COMBINE    : longword;            // *< Function For Linked Channels, offset: 0x64
    DEADTIME   : longword;            // *< Deadtime Insertion Control, offset: 0x68
    EXTTRIG    : longword;            // *< FTM External Trigger, offset: 0x6C
    POL        : longword;            // *< Channels Polarity, offset: 0x70
    FMS        : longword;            // *< Fault Mode Status, offset: 0x74
    FILTER     : longword;            // *< Input Capture Filter Control, offset: 0x78
    FLTCTRL    : longword;            // *< Fault Control, offset: 0x7C
    QDCTRL     : longword;            // *< Quadrature Decoder Control And Status, offset: 0x80
    CONF       : longword;            // *< Configuration, offset: 0x84
    FLTPOL     : longword;            // *< FTM Fault Input Polarity, offset: 0x88
    SYNCONF    : longword;            // *< Synchronization Configuration, offset: 0x8C
    INVCTRL    : longword;            // *< FTM Inverting Control, offset: 0x90
    SWOCTRL    : longword;            // *< FTM Software Output Control, offset: 0x94
    PWMLOAD    : longword;            // *< FTM PWM Load, offset: 0x98
  end;

const
  FTM0_BASE    = $40038000;

var
  FTM0         : TFTM_Registers absolute FTM0_BASE;

const
  FTM1_BASE    = $40039000;

var
  FTM1         : TFTM_Registers absolute FTM1_BASE;

const
  FTM2_BASE    = $4003A000;

var
  FTM2         : TFTM_Registers absolute FTM2_BASE;

const
  FTM3_BASE    = $40026000;

var
  FTM3         : TFTM_Registers absolute FTM3_BASE;

type
  TGPIO_Registers = record
    PDOR       : longword;            // *< Port Data Output Register, offset: 0x0
    PSOR       : longword;            // *< Port Set Output Register, offset: 0x4
    PCOR       : longword;            // *< Port Clear Output Register, offset: 0x8
    PTOR       : longword;            // *< Port Toggle Output Register, offset: 0xC
    PDIR       : longword;            // *< Port Data Input Register, offset: 0x10
    PDDR       : longword;            // *< Port Data Direction Register, offset: 0x14
  end;

const
  PTA_BASE     = $400FF000;

var
  PTA          : TGPIO_Registers absolute PTA_BASE;

const
  PTB_BASE     = $400FF040;

var
  PTB          : TGPIO_Registers absolute PTB_BASE;

const
  PTC_BASE     = $400FF080;

var
  PTC          : TGPIO_Registers absolute PTC_BASE;

const
  PTD_BASE     = $400FF0C0;

var
  PTD          : TGPIO_Registers absolute PTD_BASE;

const
  PTE_BASE     = $400FF100;

var
  PTE          : TGPIO_Registers absolute PTE_BASE;

type
  TI2C_Registers = record
    A1         : byte;                // *< I2C Address Register 1, offset: 0x0
    F          : byte;                // *< I2C Frequency Divider register, offset: 0x1
    C1         : byte;                // *< I2C Control Register 1, offset: 0x2
    S          : byte;                // *< I2C Status register, offset: 0x3
    D          : byte;                // *< I2C Data I/O register, offset: 0x4
    C2         : byte;                // *< I2C Control Register 2, offset: 0x5
    FLT        : byte;                // *< I2C Programmable Input Glitch Filter register, offset: 0x6
    RA         : byte;                // *< I2C Range Address register, offset: 0x7
    SMB        : byte;                // *< I2C SMBus Control and Status register, offset: 0x8
    A2         : byte;                // *< I2C Address Register 2, offset: 0x9
    SLTH       : byte;                // *< I2C SCL Low Timeout Register High, offset: 0xA
    SLTL       : byte;                // *< I2C SCL Low Timeout Register Low, offset: 0xB
  end;

const
  I2C0_BASE    = $40066000;

var
  I2C0         : TI2C_Registers absolute I2C0_BASE;

const
  I2C1_BASE    = $40067000;

var
  I2C1         : TI2C_Registers absolute I2C1_BASE;

type
  TI2S_Registers = record
    TCSR       : longword;            // *< SAI Transmit Control Register, offset: 0x0
    TCR1       : longword;            // *< SAI Transmit Configuration 1 Register, offset: 0x4
    TCR2       : longword;            // *< SAI Transmit Configuration 2 Register, offset: 0x8
    TCR3       : longword;            // *< SAI Transmit Configuration 3 Register, offset: 0xC
    TCR4       : longword;            // *< SAI Transmit Configuration 4 Register, offset: 0x10
    TCR5       : longword;            // *< SAI Transmit Configuration 5 Register, offset: 0x14
    RESERVED_0 : array[0..7] of byte;
    TDR        : longWord;            // *< SAI Transmit Data Register, array offset: 0x20, array step: 0x4
    RESERVED_1 : array[0..27] of byte;
    TFR        : longWord;            // *< SAI Transmit FIFO Register, array offset: 0x40, array step: 0x4
    RESERVED_2 : array[0..27] of byte;
    TMR        : longword;            // *< SAI Transmit Mask Register, offset: 0x60
    RESERVED_3 : array[0..27] of byte;
    RCSR       : longword;            // *< SAI Receive Control Register, offset: 0x80
    RCR1       : longword;            // *< SAI Receive Configuration 1 Register, offset: 0x84
    RCR2       : longword;            // *< SAI Receive Configuration 2 Register, offset: 0x88
    RCR3       : longword;            // *< SAI Receive Configuration 3 Register, offset: 0x8C
    RCR4       : longword;            // *< SAI Receive Configuration 4 Register, offset: 0x90
    RCR5       : longword;            // *< SAI Receive Configuration 5 Register, offset: 0x94
    RESERVED_4 : array[0..7] of byte;
    RDR        : longWord;            // *< SAI Receive Data Register, array offset: 0xA0, array step: 0x4
    RESERVED_5 : array[0..27] of byte;
    RFR        : longWord;            // *< SAI Receive FIFO Register, array offset: 0xC0, array step: 0x4
    RESERVED_6 : array[0..27] of byte;
    RMR        : longword;            // *< SAI Receive Mask Register, offset: 0xE0
    RESERVED_7 : array[0..27] of byte;
    MCR        : longword;            // *< SAI MCLK Control Register, offset: 0x100
    MDR        : longword;            // *< SAI MCLK Divide Register, offset: 0x104
  end;

const
  I2S0_BASE    = $4002F000;

var
  I2S0         : TI2S_Registers absolute I2S0_BASE;

type
  TLLWU_Registers = record
    PE1        : byte;                // *< LLWU Pin Enable 1 register, offset: 0x0
    PE2        : byte;                // *< LLWU Pin Enable 2 register, offset: 0x1
    PE3        : byte;                // *< LLWU Pin Enable 3 register, offset: 0x2
    PE4        : byte;                // *< LLWU Pin Enable 4 register, offset: 0x3
    ME         : byte;                // *< LLWU Module Enable register, offset: 0x4
    F1         : byte;                // *< LLWU Flag 1 register, offset: 0x5
    F2         : byte;                // *< LLWU Flag 2 register, offset: 0x6
    F3         : byte;                // *< LLWU Flag 3 register, offset: 0x7
    FILT1      : byte;                // *< LLWU Pin Filter 1 register, offset: 0x8
    FILT2      : byte;                // *< LLWU Pin Filter 2 register, offset: 0x9
  end;

const
  LLWU_BASE    = $4007C000;

var
  LLWU         : TLLWU_Registers absolute LLWU_BASE;

type
  TLPTMR_Registers = record
    CSR        : longword;            // *< Low Power Timer Control Status Register, offset: 0x0
    PSR        : longword;            // *< Low Power Timer Prescale Register, offset: 0x4
    CMR        : longword;            // *< Low Power Timer Compare Register, offset: 0x8
    CNR        : longword;            // *< Low Power Timer Counter Register, offset: 0xC
  end;

const
  LPTMR0_BASE  = $40040000;

var
  LPTMR0       : TLPTMR_Registers absolute LPTMR0_BASE;

type
  TLPUART_Registers = record
    BAUD       : longword;            // *< LPUART Baud Rate Register, offset: 0x0
    STAT       : longword;            // *< LPUART Status Register, offset: 0x4
    CTRL       : longword;            // *< LPUART Control Register, offset: 0x8
    DATA       : longword;            // *< LPUART Data Register, offset: 0xC
    MATCH      : longword;            // *< LPUART Match Address Register, offset: 0x10
    MODIR      : longword;            // *< LPUART Modem IrDA Register, offset: 0x14
  end;

const
  LPUART0_BASE = $4002A000;

var
  LPUART0      : TLPUART_Registers absolute LPUART0_BASE;

type
  TMCG_Registers = record
    C1         : byte;                // *< MCG Control 1 Register, offset: 0x0
    C2         : byte;                // *< MCG Control 2 Register, offset: 0x1
    C3         : byte;                // *< MCG Control 3 Register, offset: 0x2
    C4         : byte;                // *< MCG Control 4 Register, offset: 0x3
    C5         : byte;                // *< MCG Control 5 Register, offset: 0x4
    C6         : byte;                // *< MCG Control 6 Register, offset: 0x5
    S          : byte;                // *< MCG Status Register, offset: 0x6
    RESERVED_0 : array[0..0] of byte;
    SC         : byte;                // *< MCG Status and Control Register, offset: 0x8
    RESERVED_1 : array[0..0] of byte;
    ATCVH      : byte;                // *< MCG Auto Trim Compare Value High Register, offset: 0xA
    ATCVL      : byte;                // *< MCG Auto Trim Compare Value Low Register, offset: 0xB
    C7         : byte;                // *< MCG Control 7 Register, offset: 0xC
    C8         : byte;                // *< MCG Control 8 Register, offset: 0xD
  end;

const
  MCG_BASE     = $40064000;

var
  MCG          : TMCG_Registers absolute MCG_BASE;

type
  TMCM_Registers = record
    RESERVED_0 : array[0..7] of byte;
    PLASC      : word;                // *< Crossbar Switch (AXBS) Slave Configuration, offset: 0x8
    PLAMC      : word;                // *< Crossbar Switch (AXBS) Master Configuration, offset: 0xA
    PLACR      : longword;            // *< Crossbar Switch (AXBS) Control Register, offset: 0xC
    ISR        : longword;            // *< Interrupt Status and Control Register, offset: 0x10
    RESERVED_1 : array[0..43] of byte;
    CPO        : longword;            // *< Compute Operation Control Register, offset: 0x40
  end;

const
  MCM_BASE     = $E0080000;

var
  MCM          : TMCM_Registers absolute MCM_BASE;

type
  TNV_Registers = record
    BACKKEY3   : byte;                // *< Backdoor Comparison Key 3., offset: 0x0
    BACKKEY2   : byte;                // *< Backdoor Comparison Key 2., offset: 0x1
    BACKKEY1   : byte;                // *< Backdoor Comparison Key 1., offset: 0x2
    BACKKEY0   : byte;                // *< Backdoor Comparison Key 0., offset: 0x3
    BACKKEY7   : byte;                // *< Backdoor Comparison Key 7., offset: 0x4
    BACKKEY6   : byte;                // *< Backdoor Comparison Key 6., offset: 0x5
    BACKKEY5   : byte;                // *< Backdoor Comparison Key 5., offset: 0x6
    BACKKEY4   : byte;                // *< Backdoor Comparison Key 4., offset: 0x7
    FPROT3     : byte;                // *< Non-volatile P-Flash Protection 1 - Low Register, offset: 0x8
    FPROT2     : byte;                // *< Non-volatile P-Flash Protection 1 - High Register, offset: 0x9
    FPROT1     : byte;                // *< Non-volatile P-Flash Protection 0 - Low Register, offset: 0xA
    FPROT0     : byte;                // *< Non-volatile P-Flash Protection 0 - High Register, offset: 0xB
    FSEC       : byte;                // *< Non-volatile Flash Security Register, offset: 0xC
    FOPT       : byte;                // *< Non-volatile Flash Option Register, offset: 0xD
  end;

const
  FTFA_FlashConfig_BASE = $400;

var
  FTFA_FlashConfig : TNV_Registers absolute FTFA_FlashConfig_BASE;

type
  TOSC_Registers = record
    CR         : byte;                // *< OSC Control Register, offset: 0x0
    RESERVED_0 : array[0..0] of byte;
    &DIV       : byte;                // *< OSC_DIV, offset: 0x2
  end;

const
  OSC_BASE     = $40065000;

var
  OSC          : TOSC_Registers absolute OSC_BASE;

type
  TPDB_CH      = record
    C1         : longword;            // *< Channel n Control register 1, array offset: 0x10, array step: 0x28
    S          : longword;            // *< Channel n Status register, array offset: 0x14, array step: 0x28
    DLY        : array[0..1] of longword; // *< Channel n Delay 0 register..Channel n Delay 1 register, array offset: 0x18, array step: index*0x28, index2*0x4
    RESERVED_0 : array[0..23] of byte;
  end;
  TPDB_DAC     = record
    INTC       : longword;            // *< DAC Interval Trigger n Control register, array offset: 0x150, array step: 0x8
    INT        : longword;            // *< DAC Interval n register, array offset: 0x154, array step: 0x8
  end;

  TPDB_Registers = record
    SC         : longword;            // *< Status and Control register, offset: 0x0
    &MOD       : longword;            // *< Modulus register, offset: 0x4
    CNT        : longword;            // *< Counter register, offset: 0x8
    IDLY       : longword;            // *< Interrupt Delay register, offset: 0xC
    CH         : array[0..1] of TPDB_CH;
    RESERVED_0 : array[0..239] of byte;
    DAC        : array[0..1] of TPDB_DAC;
    RESERVED_1 : array[0..47] of byte;
    POEN       : longword;            // *< Pulse-Out n Enable register, offset: 0x190
    PODLY      : array[0..1] of longword; // *< Pulse-Out n Delay register, array offset: 0x194, array step: 0x4
  end;

const
  PDB0_BASE    = $40036000;

var
  PDB0         : TPDB_Registers absolute PDB0_BASE;

type
  TPIT_CHANNEL     = record
    LDVAL      : longword;            // *< Timer Load Value Register, array offset: 0x100, array step: 0x10
    CVAL       : longword;            // *< Current Timer Value Register, array offset: 0x104, array step: 0x10
    TCTRL      : longword;            // *< Timer Control Register, array offset: 0x108, array step: 0x10
    TFLG       : longword;            // *< Timer Flag Register, array offset: 0x10C, array step: 0x10
  end;

  TPIT_Registers = record
    MCR        : longword;            // *< PIT Module Control Register, offset: 0x0
    RESERVED_0 : array[0..251] of byte;
    CHANNEL    : array[0..3] of TPIT_CHANNEL;
  end;

const
  PIT_BASE     = $40037000;

var
  PIT          : TPIT_Registers absolute PIT_BASE;

type
  TPMC_Registers = record
    LVDSC1     : byte;                // *< Low Voltage Detect Status And Control 1 register, offset: 0x0
    LVDSC2     : byte;                // *< Low Voltage Detect Status And Control 2 register, offset: 0x1
    REGSC      : byte;                // *< Regulator Status And Control register, offset: 0x2
  end;

const
  PMC_BASE     = $4007D000;

var
  PMC          : TPMC_Registers absolute PMC_BASE;

type
  TPORT_Registers = record
    PCR        : array[0..31] of longword; // *< Pin Control Register n, array offset: 0x0, array step: 0x4
    GPCLR      : longword;            // *< Global Pin Control Low Register, offset: 0x80
    GPCHR      : longword;            // *< Global Pin Control High Register, offset: 0x84
    RESERVED_0 : array[0..23] of byte;
    ISFR       : longword;            // *< Interrupt Status Flag Register, offset: 0xA0
    RESERVED_1 : array[0..27] of byte;
    DFER       : longword;            // *< Digital Filter Enable Register, offset: 0xC0
    DFCR       : longword;            // *< Digital Filter Clock Register, offset: 0xC4
    DFWR       : longword;            // *< Digital Filter Width Register, offset: 0xC8
  end;

const
  PORTA_BASE   = $40049000;

var
  PORTA        : TPORT_Registers absolute PORTA_BASE;

const
  PORTB_BASE   = $4004A000;

var
  PORTB        : TPORT_Registers absolute PORTB_BASE;

const
  PORTC_BASE   = $4004B000;

var
  PORTC        : TPORT_Registers absolute PORTC_BASE;

const
  PORTD_BASE   = $4004C000;

var
  PORTD        : TPORT_Registers absolute PORTD_BASE;

const
  PORTE_BASE   = $4004D000;

var
  PORTE        : TPORT_Registers absolute PORTE_BASE;

type
  TRCM_Registers = record
    SRS0       : byte;                // *< System Reset Status Register 0, offset: 0x0
    SRS1       : byte;                // *< System Reset Status Register 1, offset: 0x1
    RESERVED_0 : array[0..1] of byte;
    RPFC       : byte;                // *< Reset Pin Filter Control register, offset: 0x4
    RPFW       : byte;                // *< Reset Pin Filter Width register, offset: 0x5
    RESERVED_1 : array[0..0] of byte;
    MR         : byte;                // *< Mode Register, offset: 0x7
    SSRS0      : byte;                // *< Sticky System Reset Status Register 0, offset: 0x8
    SSRS1      : byte;                // *< Sticky System Reset Status Register 1, offset: 0x9
  end;

const
  RCM_BASE     = $4007F000;

var
  RCM          : TRCM_Registers absolute RCM_BASE;

type
  TRFSYS_Registers = record
    REG        : array[0..7] of longword; // *< Register file register, array offset: 0x0, array step: 0x4
  end;

const
  RFSYS_BASE   = $40041000;

var
  RFSYS        : TRFSYS_Registers absolute RFSYS_BASE;

type
  TRFVBAT_Registers = record
    REG        : array[0..7] of longword; // *< VBAT register file register, array offset: 0x0, array step: 0x4
  end;

const
  RFVBAT_BASE  = $4003E000;

var
  RFVBAT       : TRFVBAT_Registers absolute RFVBAT_BASE;

type
  TRNG_Registers = record
    CR         : longword;            // *< RNGA Control Register, offset: 0x0
    SR         : longword;            // *< RNGA Status Register, offset: 0x4
    ER         : longword;            // *< RNGA Entropy Register, offset: 0x8
    &OR        : longword;            // *< RNGA Output Register, offset: 0xC
  end;

const
  RNG_BASE     = $40029000;

var
  RNG          : TRNG_Registers absolute RNG_BASE;

type
  TRTC_Registers = record
    TSR        : longword;            // *< RTC Time Seconds Register, offset: 0x0
    TPR        : longword;            // *< RTC Time Prescaler Register, offset: 0x4
    TAR        : longword;            // *< RTC Time Alarm Register, offset: 0x8
    TCR        : longword;            // *< RTC Time Compensation Register, offset: 0xC
    CR         : longword;            // *< RTC Control Register, offset: 0x10
    SR         : longword;            // *< RTC Status Register, offset: 0x14
    LR         : longword;            // *< RTC Lock Register, offset: 0x18
    IER        : longword;            // *< RTC Interrupt Enable Register, offset: 0x1C
    RESERVED_0 : array[0..2015] of byte;
    WAR        : longword;            // *< RTC Write Access Register, offset: 0x800
    RAR        : longword;            // *< RTC Read Access Register, offset: 0x804
  end;

const
  RTC_BASE     = $4003D000;

var
  RTC          : TRTC_Registers absolute RTC_BASE;

type
  TSIM_Registers = record
    SOPT1      : longword;            // *< System Options Register 1, offset: 0x0
    SOPT1CFG   : longword;            // *< SOPT1 Configuration Register, offset: 0x4
    RESERVED_0 : array[0..4091] of byte;
    SOPT2      : longword;            // *< System Options Register 2, offset: 0x1004
    RESERVED_1 : array[0..3] of byte;
    SOPT4      : longword;            // *< System Options Register 4, offset: 0x100C
    SOPT5      : longword;            // *< System Options Register 5, offset: 0x1010
    RESERVED_2 : array[0..3] of byte;
    SOPT7      : longword;            // *< System Options Register 7, offset: 0x1018
    SOPT8      : longword;            // *< System Options Register 8, offset: 0x101C
    RESERVED_3 : array[0..3] of byte;
    SDID       : longword;            // *< System Device Identification Register, offset: 0x1024
    RESERVED_4 : array[0..11] of byte;
    SCGC4      : longword;            // *< System Clock Gating Control Register 4, offset: 0x1034
    SCGC5      : longword;            // *< System Clock Gating Control Register 5, offset: 0x1038
    SCGC6      : longword;            // *< System Clock Gating Control Register 6, offset: 0x103C
    SCGC7      : longword;            // *< System Clock Gating Control Register 7, offset: 0x1040
    CLKDIV1    : longword;            // *< System Clock Divider Register 1, offset: 0x1044
    CLKDIV2    : longword;            // *< System Clock Divider Register 2, offset: 0x1048
    FCFG1      : longword;            // *< Flash Configuration Register 1, offset: 0x104C
    FCFG2      : longword;            // *< Flash Configuration Register 2, offset: 0x1050
    UIDH       : longword;            // *< Unique Identification Register High, offset: 0x1054
    UIDMH      : longword;            // *< Unique Identification Register Mid-High, offset: 0x1058
    UIDML      : longword;            // *< Unique Identification Register Mid Low, offset: 0x105C
    UIDL       : longword;            // *< Unique Identification Register Low, offset: 0x1060
  end;

const
  SIM_BASE     = $40047000;

var
  SIM          : TSIM_Registers absolute SIM_BASE;

type
  TSMC_Registers = record
    PMPROT     : byte;                // *< Power Mode Protection register, offset: 0x0
    PMCTRL     : byte;                // *< Power Mode Control register, offset: 0x1
    STOPCTRL   : byte;                // *< Stop Control Register, offset: 0x2
    PMSTAT     : byte;                // *< Power Mode Status register, offset: 0x3
  end;

const
  SMC_BASE     = $4007E000;

var
  SMC          : TSMC_Registers absolute SMC_BASE;

type
  TSPI_Registers = record
    MCR        : longword;            // *< Module Configuration Register, offset: 0x0
    RESERVED_0 : array[0..3] of byte;
    TCR        : longword;            // *< Transfer Count Register, offset: 0x8
    CTAR : array[0..1] of longword;   // *< Clock and Transfer Attributes Register (In Master Mode), array offset: 0xC, array step: 0x4
    RESERVED_1 : array[0..23] of byte;
    SR         : longword;            // *< Status Register, offset: 0x2C
    RSER       : longword;            // *< DMA/Interrupt Request Select and Enable Register, offset: 0x30
    PUSHR      : longword;            // *< PUSH TX FIFO Register In Master Mode, offset: 0x34
    POPR       : longword;            // *< POP RX FIFO Register, offset: 0x38
    TXFR0      : longword;            // *< Transmit FIFO Registers, offset: 0x3C
    TXFR1      : longword;            // *< Transmit FIFO Registers, offset: 0x40
    TXFR2      : longword;            // *< Transmit FIFO Registers, offset: 0x44
    TXFR3      : longword;            // *< Transmit FIFO Registers, offset: 0x48
    RESERVED_2 : array[0..47] of byte;
    RXFR0      : longword;            // *< Receive FIFO Registers, offset: 0x7C
    RXFR1      : longword;            // *< Receive FIFO Registers, offset: 0x80
    RXFR2      : longword;            // *< Receive FIFO Registers, offset: 0x84
    RXFR3      : longword;            // *< Receive FIFO Registers, offset: 0x88
  end;

const
  SPI0_BASE    = $4002C000;

var
  SPI0         : TSPI_Registers absolute SPI0_BASE;

const
  SPI1_BASE    = $4002D000;

var
  SPI1         : TSPI_Registers absolute SPI1_BASE;

type
  TUART_Registers = record
    BDH        : byte;                // *< UART Baud Rate Registers: High, offset: 0x0
    BDL        : byte;                // *< UART Baud Rate Registers: Low, offset: 0x1
    C1         : byte;                // *< UART Control Register 1, offset: 0x2
    C2         : byte;                // *< UART Control Register 2, offset: 0x3
    S1         : byte;                // *< UART Status Register 1, offset: 0x4
    S2         : byte;                // *< UART Status Register 2, offset: 0x5
    C3         : byte;                // *< UART Control Register 3, offset: 0x6
    D          : byte;                // *< UART Data Register, offset: 0x7
    MA1        : byte;                // *< UART Match Address Registers 1, offset: 0x8
    MA2        : byte;                // *< UART Match Address Registers 2, offset: 0x9
    C4         : byte;                // *< UART Control Register 4, offset: 0xA
    C5         : byte;                // *< UART Control Register 5, offset: 0xB
    ED         : byte;                // *< UART Extended Data Register, offset: 0xC
    MODEM      : byte;                // *< UART Modem Register, offset: 0xD
    IR         : byte;                // *< UART Infrared Register, offset: 0xE
    RESERVED_0 : array[0..0] of byte;
    PFIFO      : byte;                // *< UART FIFO Parameters, offset: 0x10
    CFIFO      : byte;                // *< UART FIFO Control Register, offset: 0x11
    SFIFO      : byte;                // *< UART FIFO Status Register, offset: 0x12
    TWFIFO     : byte;                // *< UART FIFO Transmit Watermark, offset: 0x13
    TCFIFO     : byte;                // *< UART FIFO Transmit Count, offset: 0x14
    RWFIFO     : byte;                // *< UART FIFO Receive Watermark, offset: 0x15
    RCFIFO     : byte;                // *< UART FIFO Receive Count, offset: 0x16
    RESERVED_1 : array[0..0] of byte;
    C7816      : byte;                // *< UART 7816 Control Register, offset: 0x18
    IE7816     : byte;                // *< UART 7816 Interrupt Enable Register, offset: 0x19
    IS7816     : byte;                // *< UART 7816 Interrupt Status Register, offset: 0x1A
    WP7816     : byte;                // *< UART 7816 Wait Parameter Register, offset: 0x1B
    WN7816     : byte;                // *< UART 7816 Wait N Register, offset: 0x1C
    WF7816     : byte;                // *< UART 7816 Wait FD Register, offset: 0x1D
    ET7816     : byte;                // *< UART 7816 Error Threshold Register, offset: 0x1E
    TL7816     : byte;                // *< UART 7816 Transmit Length Register, offset: 0x1F
    RESERVED_2 : array[0..25] of byte;
    AP7816A_T0 : byte;                // *< UART 7816 ATR Duration Timer Register A, offset: 0x3A
    AP7816B_T0 : byte;                // *< UART 7816 ATR Duration Timer Register B, offset: 0x3B
    WP7816A_T0 : byte;                // *< UART 7816 Wait Parameter Register A, offset: 0x3C
    WP7816B_T0 : byte;                // *< UART 7816 Wait Parameter Register B, offset: 0x3D
    WGP7816_T1 : byte;                // *< UART 7816 Wait and Guard Parameter Register, offset: 0x3E
    WP7816C_T1 : byte;                // *< UART 7816 Wait Parameter Register C, offset: 0x3F
  end;

const
  UART0_BASE   = $4006A000;

var
  UART0        : TUART_Registers absolute UART0_BASE;

const
  UART1_BASE   = $4006B000;

var
  UART1        : TUART_Registers absolute UART1_BASE;

const
  UART2_BASE   = $4006C000;

var
  UART2        : TUART_Registers absolute UART2_BASE;

type
  TUSB_ENDPOINT= record
    ENDPT      : byte;                // *< Endpoint Control register, array offset: 0xC0, array step: 0x4
    RESERVED_0 : array[0..2] of byte;
  end;

  TUSB_Registers = record
    PERID      : byte;                // *< Peripheral ID register, offset: 0x0
    RESERVED_0 : array[0..2] of byte;
    IDCOMP     : byte;                // *< Peripheral ID Complement register, offset: 0x4
    RESERVED_1 : array[0..2] of byte;
    REV        : byte;                // *< Peripheral Revision register, offset: 0x8
    RESERVED_2 : array[0..2] of byte;
    ADDINFO    : byte;                // *< Peripheral Additional Info register, offset: 0xC
    RESERVED_3 : array[0..2] of byte;
    OTGISTAT   : byte;                // *< OTG Interrupt Status register, offset: 0x10
    RESERVED_4 : array[0..2] of byte;
    OTGICR     : byte;                // *< OTG Interrupt Control register, offset: 0x14
    RESERVED_5 : array[0..2] of byte;
    OTGSTAT    : byte;                // *< OTG Status register, offset: 0x18
    RESERVED_6 : array[0..2] of byte;
    OTGCTL     : byte;                // *< OTG Control register, offset: 0x1C
    RESERVED_7 : array[0..98] of byte;
    ISTAT      : byte;                // *< Interrupt Status register, offset: 0x80
    RESERVED_8 : array[0..2] of byte;
    INTEN      : byte;                // *< Interrupt Enable register, offset: 0x84
    RESERVED_9 : array[0..2] of byte;
    ERRSTAT    : byte;                // *< Error Interrupt Status register, offset: 0x88
    RESERVED_10 : array[0..2] of byte;
    ERREN      : byte;                // *< Error Interrupt Enable register, offset: 0x8C
    RESERVED_11 : array[0..2] of byte;
    STAT       : byte;                // *< Status register, offset: 0x90
    RESERVED_12 : array[0..2] of byte;
    CTL        : byte;                // *< Control register, offset: 0x94
    RESERVED_13 : array[0..2] of byte;
    ADDR       : byte;                // *< Address register, offset: 0x98
    RESERVED_14 : array[0..2] of byte;
    BDTPAGE1   : byte;                // *< BDT Page register 1, offset: 0x9C
    RESERVED_15 : array[0..2] of byte;
    FRMNUML    : byte;                // *< Frame Number register Low, offset: 0xA0
    RESERVED_16 : array[0..2] of byte;
    FRMNUMH    : byte;                // *< Frame Number register High, offset: 0xA4
    RESERVED_17 : array[0..2] of byte;
    TOKEN      : byte;                // *< Token register, offset: 0xA8
    RESERVED_18 : array[0..2] of byte;
    SOFTHLD    : byte;                // *< SOF Threshold register, offset: 0xAC
    RESERVED_19 : array[0..2] of byte;
    BDTPAGE2   : byte;                // *< BDT Page Register 2, offset: 0xB0
    RESERVED_20 : array[0..2] of byte;
    BDTPAGE3   : byte;                // *< BDT Page Register 3, offset: 0xB4
    RESERVED_21 : array[0..10] of byte;
    ENDPOINT   : array[0..15] of TUSB_ENDPOINT;
    USBCTRL    : byte;                // *< USB Control register, offset: 0x100
    RESERVED_22 : array[0..2] of byte;
    OBSERVE    : byte;                // *< USB OTG Observe register, offset: 0x104
    RESERVED_23 : array[0..2] of byte;
    CONTROL    : byte;                // *< USB OTG Control register, offset: 0x108
    RESERVED_24 : array[0..2] of byte;
    USBTRC0    : byte;                // *< USB Transceiver Control register 0, offset: 0x10C
    RESERVED_25 : array[0..6] of byte;
    USBFRMADJUST : byte;              // *< Frame Adjust Register, offset: 0x114
    RESERVED_26 : array[0..42] of byte;
    CLK_RECOVER_CTRL : byte;          // *< USB Clock recovery control, offset: 0x140
    RESERVED_27 : array[0..2] of byte;
    CLK_RECOVER_IRC_EN : byte;        // *< IRC48M oscillator enable register, offset: 0x144
    RESERVED_28 : array[0..22] of byte;
    CLK_RECOVER_INT_STATUS : byte;    // *< Clock recovery separated interrupt status, offset: 0x15C
  end;

const
  USB0_BASE    = $40072000;

var
  USB0         : TUSB_Registers absolute USB0_BASE;

type
  TVREF_Registers = record
    TRM        : byte;                // *< VREF Trim Register, offset: 0x0
    SC         : byte;                // *< VREF Status and Control Register, offset: 0x1
  end;

const
  VREF_BASE    = $40074000;

var
  VREF         : TVREF_Registers absolute VREF_BASE;

type
  TWDOG_Registers = record
    STCTRLH    : word;                // *< Watchdog Status and Control Register High, offset: 0x0
    STCTRLL    : word;                // *< Watchdog Status and Control Register Low, offset: 0x2
    TOVALH     : word;                // *< Watchdog Time-out Value Register High, offset: 0x4
    TOVALL     : word;                // *< Watchdog Time-out Value Register Low, offset: 0x6
    WINH       : word;                // *< Watchdog Window Register High, offset: 0x8
    WINL       : word;                // *< Watchdog Window Register Low, offset: 0xA
    REFRESH    : word;                // *< Watchdog Refresh register, offset: 0xC
    UNLOCK     : word;                // *< Watchdog Unlock register, offset: 0xE
    TMROUTH    : word;                // *< Watchdog Timer Output Register High, offset: 0x10
    TMROUTL    : word;                // *< Watchdog Timer Output Register Low, offset: 0x12
    RSTCNT     : word;                // *< Watchdog Reset Count register, offset: 0x14
    PRESC      : word;                // *< Watchdog Prescaler register, offset: 0x16
  end;

const
  WDOG_BASE    = $40052000;

var
  WDOG         : TWDOG_Registers absolute WDOG_BASE;

implementation

procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
procedure HardFault_interrupt; external name 'HardFault_interrupt';
procedure MemoryManagement_interrupt; external name 'MemoryManagement_interrupt';
procedure BusFault_interrupt; external name 'BusFault_interrupt';
procedure UsageFault_interrupt; external name 'UsageFault_interrupt';
procedure SVCall_interrupt; external name 'SVCall_interrupt';
procedure DebugMonitor_interrupt; external name 'DebugMonitor_interrupt';
procedure PendSV_interrupt; external name 'PendSV_interrupt';
procedure SysTick_interrupt; external name 'SysTick_interrupt';
procedure DMA0_interrupt; external name 'DMA0_interrupt';
procedure DMA1_interrupt; external name 'DMA1_interrupt';
procedure DMA2_interrupt; external name 'DMA2_interrupt';
procedure DMA3_interrupt; external name 'DMA3_interrupt';
procedure DMA4_interrupt; external name 'DMA4_interrupt';
procedure DMA5_interrupt; external name 'DMA5_interrupt';
procedure DMA6_interrupt; external name 'DMA6_interrupt';
procedure DMA7_interrupt; external name 'DMA7_interrupt';
procedure DMA8_interrupt; external name 'DMA8_interrupt';
procedure DMA9_interrupt; external name 'DMA9_interrupt';
procedure DMA10_interrupt; external name 'DMA10_interrupt';
procedure DMA11_interrupt; external name 'DMA11_interrupt';
procedure DMA12_interrupt; external name 'DMA12_interrupt';
procedure DMA13_interrupt; external name 'DMA13_interrupt';
procedure DMA14_interrupt; external name 'DMA14_interrupt';
procedure DMA15_interrupt; external name 'DMA15_interrupt';
procedure DMA_Error_interrupt; external name 'DMA_Error_interrupt';
procedure MCM_interrupt; external name 'MCM_interrupt';
procedure FTF_interrupt; external name 'FTF_interrupt';
procedure Read_Collision_interrupt; external name 'Read_Collision_interrupt';
procedure LVD_LVW_interrupt; external name 'LVD_LVW_interrupt';
procedure LLW_interrupt; external name 'LLW_interrupt';
procedure Watchdog_interrupt; external name 'Watchdog_interrupt';
procedure RNG_interrupt; external name 'RNG_interrupt';
procedure I2C0_interrupt; external name 'I2C0_interrupt';
procedure I2C1_interrupt; external name 'I2C1_interrupt';
procedure SPI0_interrupt; external name 'SPI0_interrupt';
procedure SPI1_interrupt; external name 'SPI1_interrupt';
procedure I2S0_Tx_interrupt; external name 'I2S0_Tx_interrupt';
procedure I2S0_Rx_interrupt; external name 'I2S0_Rx_interrupt';
procedure LPUART0_interrupt; external name 'LPUART0_interrupt';
procedure UART0_RX_TX_interrupt; external name 'UART0_RX_TX_interrupt';
procedure UART0_ERR_interrupt; external name 'UART0_ERR_interrupt';
procedure UART1_RX_TX_interrupt; external name 'UART1_RX_TX_interrupt';
procedure UART1_ERR_interrupt; external name 'UART1_ERR_interrupt';
procedure UART2_RX_TX_interrupt; external name 'UART2_RX_TX_interrupt';
procedure UART2_ERR_interrupt; external name 'UART2_ERR_interrupt';
procedure RESERVED53_interrupt; external name 'RESERVED53_interrupt';
procedure RESERVED54_interrupt; external name 'RESERVED54_interrupt';
procedure ADC0_interrupt; external name 'ADC0_interrupt';
procedure CMP0_interrupt; external name 'CMP0_interrupt';
procedure CMP1_interrupt; external name 'CMP1_interrupt';
procedure FTM0_interrupt; external name 'FTM0_interrupt';
procedure FTM1_interrupt; external name 'FTM1_interrupt';
procedure FTM2_interrupt; external name 'FTM2_interrupt';
procedure RESERVED61_interrupt; external name 'RESERVED61_interrupt';
procedure RTC_interrupt; external name 'RTC_interrupt';
procedure RTC_Seconds_interrupt; external name 'RTC_Seconds_interrupt';
procedure PIT0_interrupt; external name 'PIT0_interrupt';
procedure PIT1_interrupt; external name 'PIT1_interrupt';
procedure PIT2_interrupt; external name 'PIT2_interrupt';
procedure PIT3_interrupt; external name 'PIT3_interrupt';
procedure PDB0_interrupt; external name 'PDB0_interrupt';
procedure USB0_interrupt; external name 'USB0_interrupt';
procedure RESERVED70_interrupt; external name 'RESERVED70_interrupt';
procedure RESERVED71_interrupt; external name 'RESERVED71_interrupt';
procedure DAC0_interrupt; external name 'DAC0_interrupt';
procedure MCG_interrupt; external name 'MCG_interrupt';
procedure LPTimer_interrupt; external name 'LPTimer_interrupt';
procedure PORTA_interrupt; external name 'PORTA_interrupt';
procedure PORTB_interrupt; external name 'PORTB_interrupt';
procedure PORTC_interrupt; external name 'PORTC_interrupt';
procedure PORTD_interrupt; external name 'PORTD_interrupt';
procedure PORTE_interrupt; external name 'PORTE_interrupt';
procedure SWI_interrupt; external name 'SWI_interrupt';
procedure RESERVED81_interrupt; external name 'RESERVED81_interrupt';
procedure RESERVED82_interrupt; external name 'RESERVED82_interrupt';
procedure RESERVED83_interrupt; external name 'RESERVED83_interrupt';
procedure RESERVED84_interrupt; external name 'RESERVED84_interrupt';
procedure RESERVED85_interrupt; external name 'RESERVED85_interrupt';
procedure RESERVED86_interrupt; external name 'RESERVED86_interrupt';
procedure FTM3_interrupt; external name 'FTM3_interrupt';
procedure DAC1_interrupt; external name 'DAC1_interrupt';
procedure ADC1_interrupt; external name 'ADC1_interrupt';
procedure RESERVED90_interrupt; external name 'RESERVED90_interrupt';
procedure RESERVED91_interrupt; external name 'RESERVED91_interrupt';
procedure RESERVED92_interrupt; external name 'RESERVED92_interrupt';
procedure RESERVED93_interrupt; external name 'RESERVED93_interrupt';
procedure RESERVED94_interrupt; external name 'RESERVED94_interrupt';
procedure RESERVED95_interrupt; external name 'RESERVED95_interrupt';
procedure RESERVED96_interrupt; external name 'RESERVED96_interrupt';
procedure RESERVED97_interrupt; external name 'RESERVED97_interrupt';
procedure RESERVED98_interrupt; external name 'RESERVED98_interrupt';
procedure RESERVED99_interrupt; external name 'RESERVED99_interrupt';
procedure RESERVED100_interrupt; external name 'RESERVED100_interrupt';
procedure RESERVED101_interrupt; external name 'RESERVED101_interrupt';

{$i cortexm4f_start.inc}

procedure FlashConfiguration; assembler; nostackframe;
label flash_conf;
asm
  .section ".flash_config.flash_conf"
flash_conf:
  .byte 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE, 0xFF, 0xFF, 0xFF

  .text
end;

procedure LowLevelStartup; assembler; nostackframe; [public, alias: '_LOWLEVELSTART'];
asm
  // Unlock watchdog
  ldr r0, .LWDOG_BASE
  movw        r1, #0xc520 //50464
  strh        r1, [r0, #0xE]
  movw        r1, #0xd928 //55592
  strh        r1, [r0, #0xE]
  nop
  nop
  // Disable watchdog for now
  movs r1, #0x1d2
  strh r1, [r0, #0]

  b Startup

.LWDOG_BASE:
  .long 0x40052000
end;

procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
  interrupt_vectors:
  .long _stack_top
  .long LowLevelStartup
  .long NonMaskableInt_interrupt
  .long HardFault_interrupt
  .long MemoryManagement_interrupt
  .long BusFault_interrupt
  .long UsageFault_interrupt
  .long 0
  .long 0
  .long 0
  .long 0
  .long SVCall_interrupt
  .long DebugMonitor_interrupt
  .long 0
  .long PendSV_interrupt
  .long SysTick_interrupt
  .long DMA0_interrupt
  .long DMA1_interrupt
  .long DMA2_interrupt
  .long DMA3_interrupt
  .long DMA4_interrupt
  .long DMA5_interrupt
  .long DMA6_interrupt
  .long DMA7_interrupt
  .long DMA8_interrupt
  .long DMA9_interrupt
  .long DMA10_interrupt
  .long DMA11_interrupt
  .long DMA12_interrupt
  .long DMA13_interrupt
  .long DMA14_interrupt
  .long DMA15_interrupt
  .long DMA_Error_interrupt
  .long MCM_interrupt
  .long FTF_interrupt
  .long Read_Collision_interrupt
  .long LVD_LVW_interrupt
  .long LLW_interrupt
  .long Watchdog_interrupt
  .long RNG_interrupt
  .long I2C0_interrupt
  .long I2C1_interrupt
  .long SPI0_interrupt
  .long SPI1_interrupt
  .long I2S0_Tx_interrupt
  .long I2S0_Rx_interrupt
  .long LPUART0_interrupt
  .long UART0_RX_TX_interrupt
  .long UART0_ERR_interrupt
  .long UART1_RX_TX_interrupt
  .long UART1_ERR_interrupt
  .long UART2_RX_TX_interrupt
  .long UART2_ERR_interrupt
  .long RESERVED53_interrupt
  .long RESERVED54_interrupt
  .long ADC0_interrupt
  .long CMP0_interrupt
  .long CMP1_interrupt
  .long FTM0_interrupt
  .long FTM1_interrupt
  .long FTM2_interrupt
  .long RESERVED61_interrupt
  .long RTC_interrupt
  .long RTC_Seconds_interrupt
  .long PIT0_interrupt
  .long PIT1_interrupt
  .long PIT2_interrupt
  .long PIT3_interrupt
  .long PDB0_interrupt
  .long USB0_interrupt
  .long RESERVED70_interrupt
  .long RESERVED71_interrupt
  .long DAC0_interrupt
  .long MCG_interrupt
  .long LPTimer_interrupt
  .long PORTA_interrupt
  .long PORTB_interrupt
  .long PORTC_interrupt
  .long PORTD_interrupt
  .long PORTE_interrupt
  .long SWI_interrupt
  .long RESERVED81_interrupt
  .long RESERVED82_interrupt
  .long RESERVED83_interrupt
  .long RESERVED84_interrupt
  .long RESERVED85_interrupt
  .long RESERVED86_interrupt
  .long FTM3_interrupt
  .long DAC1_interrupt
  .long ADC1_interrupt
  .long RESERVED90_interrupt
  .long RESERVED91_interrupt
  .long RESERVED92_interrupt
  .long RESERVED93_interrupt
  .long RESERVED94_interrupt
  .long RESERVED95_interrupt
  .long RESERVED96_interrupt
  .long RESERVED97_interrupt
  .long RESERVED98_interrupt
  .long RESERVED99_interrupt
  .long RESERVED100_interrupt
  .long RESERVED101_interrupt

  .weak NonMaskableInt_interrupt
  .weak HardFault_interrupt
  .weak MemoryManagement_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak SVCall_interrupt
  .weak DebugMonitor_interrupt
  .weak PendSV_interrupt
  .weak SysTick_interrupt
  .weak DMA0_interrupt
  .weak DMA1_interrupt
  .weak DMA2_interrupt
  .weak DMA3_interrupt
  .weak DMA4_interrupt
  .weak DMA5_interrupt
  .weak DMA6_interrupt
  .weak DMA7_interrupt
  .weak DMA8_interrupt
  .weak DMA9_interrupt
  .weak DMA10_interrupt
  .weak DMA11_interrupt
  .weak DMA12_interrupt
  .weak DMA13_interrupt
  .weak DMA14_interrupt
  .weak DMA15_interrupt
  .weak DMA_Error_interrupt
  .weak MCM_interrupt
  .weak FTF_interrupt
  .weak Read_Collision_interrupt
  .weak LVD_LVW_interrupt
  .weak LLW_interrupt
  .weak Watchdog_interrupt
  .weak RNG_interrupt
  .weak I2C0_interrupt
  .weak I2C1_interrupt
  .weak SPI0_interrupt
  .weak SPI1_interrupt
  .weak I2S0_Tx_interrupt
  .weak I2S0_Rx_interrupt
  .weak LPUART0_interrupt
  .weak UART0_RX_TX_interrupt
  .weak UART0_ERR_interrupt
  .weak UART1_RX_TX_interrupt
  .weak UART1_ERR_interrupt
  .weak UART2_RX_TX_interrupt
  .weak UART2_ERR_interrupt
  .weak RESERVED53_interrupt
  .weak RESERVED54_interrupt
  .weak ADC0_interrupt
  .weak CMP0_interrupt
  .weak CMP1_interrupt
  .weak FTM0_interrupt
  .weak FTM1_interrupt
  .weak FTM2_interrupt
  .weak RESERVED61_interrupt
  .weak RTC_interrupt
  .weak RTC_Seconds_interrupt
  .weak PIT0_interrupt
  .weak PIT1_interrupt
  .weak PIT2_interrupt
  .weak PIT3_interrupt
  .weak PDB0_interrupt
  .weak USB0_interrupt
  .weak RESERVED70_interrupt
  .weak RESERVED71_interrupt
  .weak DAC0_interrupt
  .weak MCG_interrupt
  .weak LPTimer_interrupt
  .weak PORTA_interrupt
  .weak PORTB_interrupt
  .weak PORTC_interrupt
  .weak PORTD_interrupt
  .weak PORTE_interrupt
  .weak SWI_interrupt
  .weak RESERVED81_interrupt
  .weak RESERVED82_interrupt
  .weak RESERVED83_interrupt
  .weak RESERVED84_interrupt
  .weak RESERVED85_interrupt
  .weak RESERVED86_interrupt
  .weak FTM3_interrupt
  .weak DAC1_interrupt
  .weak ADC1_interrupt
  .weak RESERVED90_interrupt
  .weak RESERVED91_interrupt
  .weak RESERVED92_interrupt
  .weak RESERVED93_interrupt
  .weak RESERVED94_interrupt
  .weak RESERVED95_interrupt
  .weak RESERVED96_interrupt
  .weak RESERVED97_interrupt
  .weak RESERVED98_interrupt
  .weak RESERVED99_interrupt
  .weak RESERVED100_interrupt
  .weak RESERVED101_interrupt
  .set NonMaskableInt_interrupt, HaltProc
  .set HardFault_interrupt, HaltProc
  .set MemoryManagement_interrupt, HaltProc
  .set BusFault_interrupt, HaltProc
  .set UsageFault_interrupt, HaltProc
  .set SVCall_interrupt, HaltProc
  .set DebugMonitor_interrupt, HaltProc
  .set PendSV_interrupt, HaltProc
  .set SysTick_interrupt, HaltProc
  .set DMA0_interrupt, HaltProc
  .set DMA1_interrupt, HaltProc
  .set DMA2_interrupt, HaltProc
  .set DMA3_interrupt, HaltProc
  .set DMA4_interrupt, HaltProc
  .set DMA5_interrupt, HaltProc
  .set DMA6_interrupt, HaltProc
  .set DMA7_interrupt, HaltProc
  .set DMA8_interrupt, HaltProc
  .set DMA9_interrupt, HaltProc
  .set DMA10_interrupt, HaltProc
  .set DMA11_interrupt, HaltProc
  .set DMA12_interrupt, HaltProc
  .set DMA13_interrupt, HaltProc
  .set DMA14_interrupt, HaltProc
  .set DMA15_interrupt, HaltProc
  .set DMA_Error_interrupt, HaltProc
  .set MCM_interrupt, HaltProc
  .set FTF_interrupt, HaltProc
  .set Read_Collision_interrupt, HaltProc
  .set LVD_LVW_interrupt, HaltProc
  .set LLW_interrupt, HaltProc
  .set Watchdog_interrupt, HaltProc
  .set RNG_interrupt, HaltProc
  .set I2C0_interrupt, HaltProc
  .set I2C1_interrupt, HaltProc
  .set SPI0_interrupt, HaltProc
  .set SPI1_interrupt, HaltProc
  .set I2S0_Tx_interrupt, HaltProc
  .set I2S0_Rx_interrupt, HaltProc
  .set LPUART0_interrupt, HaltProc
  .set UART0_RX_TX_interrupt, HaltProc
  .set UART0_ERR_interrupt, HaltProc
  .set UART1_RX_TX_interrupt, HaltProc
  .set UART1_ERR_interrupt, HaltProc
  .set UART2_RX_TX_interrupt, HaltProc
  .set UART2_ERR_interrupt, HaltProc
  .set RESERVED53_interrupt, HaltProc
  .set RESERVED54_interrupt, HaltProc
  .set ADC0_interrupt, HaltProc
  .set CMP0_interrupt, HaltProc
  .set CMP1_interrupt, HaltProc
  .set FTM0_interrupt, HaltProc
  .set FTM1_interrupt, HaltProc
  .set FTM2_interrupt, HaltProc
  .set RESERVED61_interrupt, HaltProc
  .set RTC_interrupt, HaltProc
  .set RTC_Seconds_interrupt, HaltProc
  .set PIT0_interrupt, HaltProc
  .set PIT1_interrupt, HaltProc
  .set PIT2_interrupt, HaltProc
  .set PIT3_interrupt, HaltProc
  .set PDB0_interrupt, HaltProc
  .set USB0_interrupt, HaltProc
  .set RESERVED70_interrupt, HaltProc
  .set RESERVED71_interrupt, HaltProc
  .set DAC0_interrupt, HaltProc
  .set MCG_interrupt, HaltProc
  .set LPTimer_interrupt, HaltProc
  .set PORTA_interrupt, HaltProc
  .set PORTB_interrupt, HaltProc
  .set PORTC_interrupt, HaltProc
  .set PORTD_interrupt, HaltProc
  .set PORTE_interrupt, HaltProc
  .set SWI_interrupt, HaltProc
  .set RESERVED81_interrupt, HaltProc
  .set RESERVED82_interrupt, HaltProc
  .set RESERVED83_interrupt, HaltProc
  .set RESERVED84_interrupt, HaltProc
  .set RESERVED85_interrupt, HaltProc
  .set RESERVED86_interrupt, HaltProc
  .set FTM3_interrupt, HaltProc
  .set DAC1_interrupt, HaltProc
  .set ADC1_interrupt, HaltProc
  .set RESERVED90_interrupt, HaltProc
  .set RESERVED91_interrupt, HaltProc
  .set RESERVED92_interrupt, HaltProc
  .set RESERVED93_interrupt, HaltProc
  .set RESERVED94_interrupt, HaltProc
  .set RESERVED95_interrupt, HaltProc
  .set RESERVED96_interrupt, HaltProc
  .set RESERVED97_interrupt, HaltProc
  .set RESERVED98_interrupt, HaltProc
  .set RESERVED99_interrupt, HaltProc
  .set RESERVED100_interrupt, HaltProc
  .set RESERVED101_interrupt, HaltProc
  .text
end;
end.
