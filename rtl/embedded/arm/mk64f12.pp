unit mk64f12;
interface
{$PACKRECORDS 2}
{$GOTO ON}
{$MODESWITCH ADVANCEDRECORDS}
// ** ###################################################################
// **     Processors:          MK64FN1M0VDC12
// **                          MK64FN1M0VLL12
// **                          MK64FN1M0VLQ12
// **                          MK64FN1M0VMD12
// **
// **     Compilers:           Keil ARM C/C++ Compiler
// **                          Freescale C/C++ for Embedded ARM
// **                          GNU C Compiler
// **                          GNU C Compiler - CodeSourcery Sourcery G++
// **                          IAR ANSI C/C++ Compiler for ARM
// **
// **     Reference manual:    K64P144M120SF5RM, Rev.2, January 2014
// **     Version:             rev. 2.8, 2015-02-19
// **     Build:               b150225
// **
// **     Abstract:
// **         CMSIS Peripheral Access Layer for MK64F12
// **
// **     Copyright (c) 1997 - 2015 Freescale Semiconductor, Inc.
// **     All rights reserved.
// **
// **     Redistribution and use in source and binary forms, with or without modification,
// **     are permitted provided that the following conditions are met:
// **
// **     o Redistributions of source code must retain the above copyright notice, this list
// **       of conditions and the following disclaimer.
// **
// **     o Redistributions in binary form must reproduce the above copyright notice, this
// **       list of conditions and the following disclaimer in the documentation and/or
// **       other materials provided with the distribution.
// **
// **     o Neither the name of Freescale Semiconductor, Inc. nor the names of its
// **       contributors may be used to endorse or promote products derived from this
// **       software without specific prior written permission.
// **
// **     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// **     ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// **     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// **     DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// **     ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// **     (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// **     LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
// **     ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// **     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// **     SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// **
// **     http:                 www.freescale.com
// **     mail:                 support@freescale.com
// **
// **     Revisions:
// **     - rev. 1.0 (2013-08-12)
// **         Initial version.
// **     - rev. 2.0 (2013-10-29)
// **         Register accessor macros added to the memory map.
// **         Symbols for Processor Expert memory map compatibility added to the memory map.
// **         Startup file for gcc has been updated according to CMSIS 3.2.
// **         System initialization updated.
// **         MCG - registers updated.
// **         PORTA, PORTB, PORTC, PORTE - registers for digital filter removed.
// **     - rev. 2.1 (2013-10-30)
// **         Definition of BITBAND macros updated to support peripherals with 32-bit acces disabled.
// **     - rev. 2.2 (2013-12-09)
// **         DMA - EARS register removed.
// **         AIPS0, AIPS1 - MPRA register updated.
// **     - rev. 2.3 (2014-01-24)
// **         Update according to reference manual rev. 2
// **         ENET, MCG, MCM, SIM, USB - registers updated
// **     - rev. 2.4 (2014-02-10)
// **         The declaration of clock configurations has been moved to separate header file system_MK64F12.h
// **         Update of SystemInit() and SystemCoreClockUpdate() functions.
// **     - rev. 2.5 (2014-02-10)
// **         The declaration of clock configurations has been moved to separate header file system_MK64F12.h
// **         Update of SystemInit() and SystemCoreClockUpdate() functions.
// **         Module access macro module_BASES replaced by module_BASE_PTRS.
// **     - rev. 2.6 (2014-08-28)
// **         Update of system files - default clock configuration changed.
// **         Update of startup files - possibility to override DefaultISR added.
// **     - rev. 2.7 (2014-10-14)
// **         Interrupt INT_LPTimer renamed to INT_LPTMR0, interrupt INT_Watchdog renamed to INT_WDOG_EWM.
// **     - rev. 2.8 (2015-02-19)
// **         Renamed interrupt vector LLW to LLWU.
// **
// ** ###################################################################
// !
// * @file MK64F12.h
// * @version 2.8
// * @date 2015-02-19
// CMSIS Peripheral Access Layer for MK64F12
// *
// * CMSIS Peripheral Access Layer for MK64F12
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
    FTFE_IRQn  = 18,                  // *< FTFE Command complete interrupt
    Read_Collision_IRQn = 19,         // *< Read Collision Interrupt
    LVD_LVW_IRQn = 20,                // *< Low Voltage Detect, Low Voltage Warning
    LLWU_IRQn  = 21,                  // *< Low Leakage Wakeup Unit
    WDOG_EWM_IRQn = 22,               // *< WDOG Interrupt
    RNG_IRQn   = 23,                  // *< RNG Interrupt
    I2C0_IRQn  = 24,                  // *< I2C0 interrupt
    I2C1_IRQn  = 25,                  // *< I2C1 interrupt
    SPI0_IRQn  = 26,                  // *< SPI0 Interrupt
    SPI1_IRQn  = 27,                  // *< SPI1 Interrupt
    I2S0_Tx_IRQn = 28,                // *< I2S0 transmit interrupt
    I2S0_Rx_IRQn = 29,                // *< I2S0 receive interrupt
    UART0_LON_IRQn = 30,              // *< UART0 LON interrupt
    UART0_RX_TX_IRQn = 31,            // *< UART0 Receive/Transmit interrupt
    UART0_ERR_IRQn = 32,              // *< UART0 Error interrupt
    UART1_RX_TX_IRQn = 33,            // *< UART1 Receive/Transmit interrupt
    UART1_ERR_IRQn = 34,              // *< UART1 Error interrupt
    UART2_RX_TX_IRQn = 35,            // *< UART2 Receive/Transmit interrupt
    UART2_ERR_IRQn = 36,              // *< UART2 Error interrupt
    UART3_RX_TX_IRQn = 37,            // *< UART3 Receive/Transmit interrupt
    UART3_ERR_IRQn = 38,              // *< UART3 Error interrupt
    ADC0_IRQn  = 39,                  // *< ADC0 interrupt
    CMP0_IRQn  = 40,                  // *< CMP0 interrupt
    CMP1_IRQn  = 41,                  // *< CMP1 interrupt
    FTM0_IRQn  = 42,                  // *< FTM0 fault, overflow and channels interrupt
    FTM1_IRQn  = 43,                  // *< FTM1 fault, overflow and channels interrupt
    FTM2_IRQn  = 44,                  // *< FTM2 fault, overflow and channels interrupt
    CMT_IRQn   = 45,                  // *< CMT interrupt
    RTC_IRQn   = 46,                  // *< RTC interrupt
    RTC_Seconds_IRQn = 47,            // *< RTC seconds interrupt
    PIT0_IRQn  = 48,                  // *< PIT timer channel 0 interrupt
    PIT1_IRQn  = 49,                  // *< PIT timer channel 1 interrupt
    PIT2_IRQn  = 50,                  // *< PIT timer channel 2 interrupt
    PIT3_IRQn  = 51,                  // *< PIT timer channel 3 interrupt
    PDB0_IRQn  = 52,                  // *< PDB0 Interrupt
    USB0_IRQn  = 53,                  // *< USB0 interrupt
    USBDCD_IRQn = 54,                 // *< USBDCD Interrupt
    RESERVED71_IRQn = 55,             // *< Reserved interrupt 71
    DAC0_IRQn  = 56,                  // *< DAC0 interrupt
    MCG_IRQn   = 57,                  // *< MCG Interrupt
    LPTMR0_IRQn = 58,                 // *< LPTimer interrupt
    PORTA_IRQn = 59,                  // *< Port A interrupt
    PORTB_IRQn = 60,                  // *< Port B interrupt
    PORTC_IRQn = 61,                  // *< Port C interrupt
    PORTD_IRQn = 62,                  // *< Port D interrupt
    PORTE_IRQn = 63,                  // *< Port E interrupt
    SWI_IRQn   = 64,                  // *< Software interrupt
    SPI2_IRQn  = 65,                  // *< SPI2 Interrupt
    UART4_RX_TX_IRQn = 66,            // *< UART4 Receive/Transmit interrupt
    UART4_ERR_IRQn = 67,              // *< UART4 Error interrupt
    UART5_RX_TX_IRQn = 68,            // *< UART5 Receive/Transmit interrupt
    UART5_ERR_IRQn = 69,              // *< UART5 Error interrupt
    CMP2_IRQn  = 70,                  // *< CMP2 interrupt
    FTM3_IRQn  = 71,                  // *< FTM3 fault, overflow and channels interrupt
    DAC1_IRQn  = 72,                  // *< DAC1 interrupt
    ADC1_IRQn  = 73,                  // *< ADC1 interrupt
    I2C2_IRQn  = 74,                  // *< I2C2 interrupt
    CAN0_ORed_Message_buffer_IRQn = 75, // *< CAN0 OR'd message buffers interrupt
    CAN0_Bus_Off_IRQn = 76,           // *< CAN0 bus off interrupt
    CAN0_Error_IRQn = 77,             // *< CAN0 error interrupt
    CAN0_Tx_Warning_IRQn = 78,        // *< CAN0 Tx warning interrupt
    CAN0_Rx_Warning_IRQn = 79,        // *< CAN0 Rx warning interrupt
    CAN0_Wake_Up_IRQn = 80,           // *< CAN0 wake up interrupt
    SDHC_IRQn  = 81,                  // *< SDHC interrupt
    ENET_1588_Timer_IRQn = 82,        // *< Ethernet MAC IEEE 1588 Timer Interrupt
    ENET_Transmit_IRQn = 83,          // *< Ethernet MAC Transmit Interrupt
    ENET_Receive_IRQn = 84,           // *< Ethernet MAC Receive Interrupt
    ENET_Error_IRQn = 85              // *< Ethernet MAC Error and miscelaneous Interrupt
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
  ADC1_BASE    = $400BB000;

var
  ADC1         : TADC_Registers absolute ADC1_BASE;

type
  TAIPS_Registers = record
    MPRA       : longword;            // *< Master Privilege Register A, offset: 0x0
    RESERVED_0 : array[0..27] of byte;
    PACRA      : longword;            // *< Peripheral Access Control Register, offset: 0x20
    PACRB      : longword;            // *< Peripheral Access Control Register, offset: 0x24
    PACRC      : longword;            // *< Peripheral Access Control Register, offset: 0x28
    PACRD      : longword;            // *< Peripheral Access Control Register, offset: 0x2C
    RESERVED_1 : array[0..15] of byte;
    PACRE      : longword;            // *< Peripheral Access Control Register, offset: 0x40
    PACRF      : longword;            // *< Peripheral Access Control Register, offset: 0x44
    PACRG      : longword;            // *< Peripheral Access Control Register, offset: 0x48
    PACRH      : longword;            // *< Peripheral Access Control Register, offset: 0x4C
    PACRI      : longword;            // *< Peripheral Access Control Register, offset: 0x50
    PACRJ      : longword;            // *< Peripheral Access Control Register, offset: 0x54
    PACRK      : longword;            // *< Peripheral Access Control Register, offset: 0x58
    PACRL      : longword;            // *< Peripheral Access Control Register, offset: 0x5C
    PACRM      : longword;            // *< Peripheral Access Control Register, offset: 0x60
    PACRN      : longword;            // *< Peripheral Access Control Register, offset: 0x64
    PACRO      : longword;            // *< Peripheral Access Control Register, offset: 0x68
    PACRP      : longword;            // *< Peripheral Access Control Register, offset: 0x6C
    RESERVED_2 : array[0..15] of byte;
    PACRU      : longword;            // *< Peripheral Access Control Register, offset: 0x80
  end;

const
  AIPS0_BASE   = $40000000;

var
  AIPS0        : TAIPS_Registers absolute AIPS0_BASE;

const
  AIPS1_BASE   = $40080000;

var
  AIPS1        : TAIPS_Registers absolute AIPS1_BASE;

type
  TAXBS_SLAVE  = record
    PRS        : longword;            // *< Priority Registers Slave, array offset: 0x0, array step: 0x100
    RESERVED_0 : array[0..11] of byte;
    CRS        : longword;            // *< Control Register, array offset: 0x10, array step: 0x100
    RESERVED_1 : array[0..235] of byte;
  end;

  TAXBS_Registers = record
    SLAVE      : array[0..4] of TAXBS_SLAVE;
    RESERVED_0 : array[0..767] of byte;
    MGPCR0     : longword;            // *< Master General Purpose Control Register, offset: 0x800
    RESERVED_1 : array[0..251] of byte;
    MGPCR1     : longword;            // *< Master General Purpose Control Register, offset: 0x900
    RESERVED_2 : array[0..251] of byte;
    MGPCR2     : longword;            // *< Master General Purpose Control Register, offset: 0xA00
    RESERVED_3 : array[0..251] of byte;
    MGPCR3     : longword;            // *< Master General Purpose Control Register, offset: 0xB00
    RESERVED_4 : array[0..251] of byte;
    MGPCR4     : longword;            // *< Master General Purpose Control Register, offset: 0xC00
    RESERVED_5 : array[0..251] of byte;
    MGPCR5     : longword;            // *< Master General Purpose Control Register, offset: 0xD00
  end;

const
  AXBS_BASE    = $40004000;

var
  AXBS         : TAXBS_Registers absolute AXBS_BASE;

type
  TCAN_MB      = record
    CS         : longword;            // *< Message Buffer 0 CS Register..Message Buffer 15 CS Register, array offset: 0x80, array step: 0x10
    ID         : longword;            // *< Message Buffer 0 ID Register..Message Buffer 15 ID Register, array offset: 0x84, array step: 0x10
    WORD0      : longword;            // *< Message Buffer 0 WORD0 Register..Message Buffer 15 WORD0 Register, array offset: 0x88, array step: 0x10
    WORD1      : longword;            // *< Message Buffer 0 WORD1 Register..Message Buffer 15 WORD1 Register, array offset: 0x8C, array step: 0x10
  end;

  TCAN_Registers = record
    MCR        : longword;            // *< Module Configuration Register, offset: 0x0
    CTRL1      : longword;            // *< Control 1 register, offset: 0x4
    TIMER      : longword;            // *< Free Running Timer, offset: 0x8
    RESERVED_0 : array[0..3] of byte;
    RXMGMASK   : longword;            // *< Rx Mailboxes Global Mask Register, offset: 0x10
    RX14MASK   : longword;            // *< Rx 14 Mask register, offset: 0x14
    RX15MASK   : longword;            // *< Rx 15 Mask register, offset: 0x18
    ECR        : longword;            // *< Error Counter, offset: 0x1C
    ESR1       : longword;            // *< Error and Status 1 register, offset: 0x20
    RESERVED_1 : array[0..3] of byte;
    IMASK1     : longword;            // *< Interrupt Masks 1 register, offset: 0x28
    RESERVED_2 : array[0..3] of byte;
    IFLAG1     : longword;            // *< Interrupt Flags 1 register, offset: 0x30
    CTRL2      : longword;            // *< Control 2 register, offset: 0x34
    ESR2       : longword;            // *< Error and Status 2 register, offset: 0x38
    RESERVED_3 : array[0..7] of byte;
    CRCR       : longword;            // *< CRC Register, offset: 0x44
    RXFGMASK   : longword;            // *< Rx FIFO Global Mask register, offset: 0x48
    RXFIR      : longword;            // *< Rx FIFO Information Register, offset: 0x4C
    RESERVED_4 : array[0..47] of byte;
    MB         : array[0..15] of TCAN_MB;
    RESERVED_5 : array[0..1791] of byte;
    RXIMR      : array[0..15] of longword; // *< Rx Individual Mask Registers, array offset: 0x880, array step: 0x4
  end;

const
  CAN0_BASE    = $40024000;

var
  CAN0         : TCAN_Registers absolute CAN0_BASE;

type
  TCAU_Registers = record
    DIRECT     : array[0..15] of longword; // *< Direct access register 0..Direct access register 15, array offset: 0x0, array step: 0x4
    RESERVED_0 : array[0..2047] of byte;
    LDR_CASR   : longword;            // *< Status register - Load Register command, offset: 0x840
    LDR_CAA    : longword;            // *< Accumulator register - Load Register command, offset: 0x844
    LDR_CA     : array[0..8] of longword; // *< General Purpose Register 0 - Load Register command..General Purpose Register 8 - Load Register command, array offset: 0x848, array step: 0x4
    RESERVED_1 : array[0..19] of byte;
    STR_CASR   : longword;            // *< Status register - Store Register command, offset: 0x880
    STR_CAA    : longword;            // *< Accumulator register - Store Register command, offset: 0x884
    STR_CA     : array[0..8] of longword; // *< General Purpose Register 0 - Store Register command..General Purpose Register 8 - Store Register command, array offset: 0x888, array step: 0x4
    RESERVED_2 : array[0..19] of byte;
    ADR_CASR   : longword;            // *< Status register - Add Register command, offset: 0x8C0
    ADR_CAA    : longword;            // *< Accumulator register - Add to register command, offset: 0x8C4
    ADR_CA     : array[0..8] of longword; // *< General Purpose Register 0 - Add to register command..General Purpose Register 8 - Add to register command, array offset: 0x8C8, array step: 0x4
    RESERVED_3 : array[0..19] of byte;
    RADR_CASR  : longword;            // *< Status register - Reverse and Add to Register command, offset: 0x900
    RADR_CAA   : longword;            // *< Accumulator register - Reverse and Add to Register command, offset: 0x904
    RADR_CA    : array[0..8] of longword; // *< General Purpose Register 0 - Reverse and Add to Register command..General Purpose Register 8 - Reverse and Add to Register command, array offset: 0x908, array step: 0x4
    RESERVED_4 : array[0..83] of byte;
    XOR_CASR   : longword;            // *< Status register - Exclusive Or command, offset: 0x980
    XOR_CAA    : longword;            // *< Accumulator register - Exclusive Or command, offset: 0x984
    XOR_CA     : array[0..8] of longword; // *< General Purpose Register 0 - Exclusive Or command..General Purpose Register 8 - Exclusive Or command, array offset: 0x988, array step: 0x4
    RESERVED_5 : array[0..19] of byte;
    ROTL_CASR  : longword;            // *< Status register - Rotate Left command, offset: 0x9C0
    ROTL_CAA   : longword;            // *< Accumulator register - Rotate Left command, offset: 0x9C4
    ROTL_CA    : array[0..8] of longword; // *< General Purpose Register 0 - Rotate Left command..General Purpose Register 8 - Rotate Left command, array offset: 0x9C8, array step: 0x4
    RESERVED_6 : array[0..275] of byte;
    AESC_CASR  : longword;            // *< Status register - AES Column Operation command, offset: 0xB00
    AESC_CAA   : longword;            // *< Accumulator register - AES Column Operation command, offset: 0xB04
    AESC_CA    : array[0..8] of longword; // *< General Purpose Register 0 - AES Column Operation command..General Purpose Register 8 - AES Column Operation command, array offset: 0xB08, array step: 0x4
    RESERVED_7 : array[0..19] of byte;
    AESIC_CASR : longword;            // *< Status register - AES Inverse Column Operation command, offset: 0xB40
    AESIC_CAA  : longword;            // *< Accumulator register - AES Inverse Column Operation command, offset: 0xB44
    AESIC_CA   : array[0..8] of longword; // *< General Purpose Register 0 - AES Inverse Column Operation command..General Purpose Register 8 - AES Inverse Column Operation command, array offset: 0xB48, array step: 0x4
  end;

const
  CAU_BASE     = $E0081000;

var
  CAU          : TCAU_Registers absolute CAU_BASE;

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

const
  CMP2_BASE    = $40073010;

var
  CMP2         : TCMP_Registers absolute CMP2_BASE;

type
  TCMT_Registers = record
    CGH1       : byte;                // *< CMT Carrier Generator High Data Register 1, offset: 0x0
    CGL1       : byte;                // *< CMT Carrier Generator Low Data Register 1, offset: 0x1
    CGH2       : byte;                // *< CMT Carrier Generator High Data Register 2, offset: 0x2
    CGL2       : byte;                // *< CMT Carrier Generator Low Data Register 2, offset: 0x3
    OC         : byte;                // *< CMT Output Control Register, offset: 0x4
    MSC        : byte;                // *< CMT Modulator Status and Control Register, offset: 0x5
    CMD1       : byte;                // *< CMT Modulator Data Register Mark High, offset: 0x6
    CMD2       : byte;                // *< CMT Modulator Data Register Mark Low, offset: 0x7
    CMD3       : byte;                // *< CMT Modulator Data Register Space High, offset: 0x8
    CMD4       : byte;                // *< CMT Modulator Data Register Space Low, offset: 0x9
    PPS        : byte;                // *< CMT Primary Prescaler Register, offset: 0xA
    DMA        : byte;                // *< CMT Direct Memory Access Register, offset: 0xB
  end;

const
  CMT_BASE     = $40062000;

var
  CMT          : TCMT_Registers absolute CMT_BASE;

type
  TCRC_Registers = record
    DATA       : longword;            // *< CRC Data register, offset: 0x0
    GPOLY      : longword;            // *< CRC Polynomial register, offset: 0x4
    CTRL       : longword;            // *< CRC Control register, offset: 0x8
  end;

const
  CRC_BASE     = $40032000;

var
  CRC0         : TCRC_Registers absolute CRC_BASE;

type
  TDAC_DAT     = record
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
  DAC0_BASE    = $400CC000;

var
  DAC0         : TDAC_Registers absolute DAC0_BASE;

const
  DAC1_BASE    = $400CD000;

var
  DAC1         : TDAC_Registers absolute DAC1_BASE;

type
  TDMA_TCD     = record
    SADDR      : longword;            // *< TCD Source Address, array offset: 0x1000, array step: 0x20
    SOFF       : word;                // *< TCD Signed Source Address Offset, array offset: 0x1004, array step: 0x20
    ATTR       : word;                // *< TCD Transfer Attributes, array offset: 0x1006, array step: 0x20
    NBYTES_MLNO : longword;           // *< TCD Minor Byte Count (Minor Loop Disabled), array offset: 0x1008, array step: 0x20
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
    RESERVED_5 : array[0..199] of byte;
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
    RESERVED_6 : array[0..3823] of byte;
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
  TENET_CHANNEL= record
    TCSR       : longword;            // *< Timer Control Status Register, array offset: 0x608, array step: 0x8
    TCCR       : longword;            // *< Timer Compare Capture Register, array offset: 0x60C, array step: 0x8
  end;

  TENET_Registers = record
    RESERVED_0 : array[0..3] of byte;
    EIR        : longword;            // *< Interrupt Event Register, offset: 0x4
    EIMR       : longword;            // *< Interrupt Mask Register, offset: 0x8
    RESERVED_1 : array[0..3] of byte;
    RDAR       : longword;            // *< Receive Descriptor Active Register, offset: 0x10
    TDAR       : longword;            // *< Transmit Descriptor Active Register, offset: 0x14
    RESERVED_2 : array[0..11] of byte;
    ECR        : longword;            // *< Ethernet Control Register, offset: 0x24
    RESERVED_3 : array[0..23] of byte;
    MMFR       : longword;            // *< MII Management Frame Register, offset: 0x40
    MSCR       : longword;            // *< MII Speed Control Register, offset: 0x44
    RESERVED_4 : array[0..27] of byte;
    MIBC       : longword;            // *< MIB Control Register, offset: 0x64
    RESERVED_5 : array[0..27] of byte;
    RCR        : longword;            // *< Receive Control Register, offset: 0x84
    RESERVED_6 : array[0..59] of byte;
    TCR        : longword;            // *< Transmit Control Register, offset: 0xC4
    RESERVED_7 : array[0..27] of byte;
    PALR       : longword;            // *< Physical Address Lower Register, offset: 0xE4
    PAUR       : longword;            // *< Physical Address Upper Register, offset: 0xE8
    OPD        : longword;            // *< Opcode/Pause Duration Register, offset: 0xEC
    RESERVED_8 : array[0..39] of byte;
    IAUR       : longword;            // *< Descriptor Individual Upper Address Register, offset: 0x118
    IALR       : longword;            // *< Descriptor Individual Lower Address Register, offset: 0x11C
    GAUR       : longword;            // *< Descriptor Group Upper Address Register, offset: 0x120
    GALR       : longword;            // *< Descriptor Group Lower Address Register, offset: 0x124
    RESERVED_9 : array[0..27] of byte;
    TFWR       : longword;            // *< Transmit FIFO Watermark Register, offset: 0x144
    RESERVED_10 : array[0..55] of byte;
    RDSR       : longword;            // *< Receive Descriptor Ring Start Register, offset: 0x180
    TDSR       : longword;            // *< Transmit Buffer Descriptor Ring Start Register, offset: 0x184
    MRBR       : longword;            // *< Maximum Receive Buffer Size Register, offset: 0x188
    RESERVED_11 : array[0..3] of byte;
    RSFL       : longword;            // *< Receive FIFO Section Full Threshold, offset: 0x190
    RSEM       : longword;            // *< Receive FIFO Section Empty Threshold, offset: 0x194
    RAEM       : longword;            // *< Receive FIFO Almost Empty Threshold, offset: 0x198
    RAFL       : longword;            // *< Receive FIFO Almost Full Threshold, offset: 0x19C
    TSEM       : longword;            // *< Transmit FIFO Section Empty Threshold, offset: 0x1A0
    TAEM       : longword;            // *< Transmit FIFO Almost Empty Threshold, offset: 0x1A4
    TAFL       : longword;            // *< Transmit FIFO Almost Full Threshold, offset: 0x1A8
    TIPG       : longword;            // *< Transmit Inter-Packet Gap, offset: 0x1AC
    FTRL       : longword;            // *< Frame Truncation Length, offset: 0x1B0
    RESERVED_12 : array[0..11] of byte;
    TACC       : longword;            // *< Transmit Accelerator Function Configuration, offset: 0x1C0
    RACC       : longword;            // *< Receive Accelerator Function Configuration, offset: 0x1C4
    RESERVED_13 : array[0..59] of byte;
    RMON_T_PACKETS : longword;        // *< Tx Packet Count Statistic Register, offset: 0x204
    RMON_T_BC_PKT : longword;         // *< Tx Broadcast Packets Statistic Register, offset: 0x208
    RMON_T_MC_PKT : longword;         // *< Tx Multicast Packets Statistic Register, offset: 0x20C
    RMON_T_CRC_ALIGN : longword;      // *< Tx Packets with CRC/Align Error Statistic Register, offset: 0x210
    RMON_T_UNDERSIZE : longword;      // *< Tx Packets Less Than Bytes and Good CRC Statistic Register, offset: 0x214
    RMON_T_OVERSIZE : longword;       // *< Tx Packets GT MAX_FL bytes and Good CRC Statistic Register, offset: 0x218
    RMON_T_FRAG : longword;           // *< Tx Packets Less Than 64 Bytes and Bad CRC Statistic Register, offset: 0x21C
    RMON_T_JAB : longword;            // *< Tx Packets Greater Than MAX_FL bytes and Bad CRC Statistic Register, offset: 0x220
    RMON_T_COL : longword;            // *< Tx Collision Count Statistic Register, offset: 0x224
    RMON_T_P64 : longword;            // *< Tx 64-Byte Packets Statistic Register, offset: 0x228
    RMON_T_P65TO127 : longword;       // *< Tx 65- to 127-byte Packets Statistic Register, offset: 0x22C
    RMON_T_P128TO255 : longword;      // *< Tx 128- to 255-byte Packets Statistic Register, offset: 0x230
    RMON_T_P256TO511 : longword;      // *< Tx 256- to 511-byte Packets Statistic Register, offset: 0x234
    RMON_T_P512TO1023 : longword;     // *< Tx 512- to 1023-byte Packets Statistic Register, offset: 0x238
    RMON_T_P1024TO2047 : longword;    // *< Tx 1024- to 2047-byte Packets Statistic Register, offset: 0x23C
    RMON_T_P_GTE2048 : longword;      // *< Tx Packets Greater Than 2048 Bytes Statistic Register, offset: 0x240
    RMON_T_OCTETS : longword;         // *< Tx Octets Statistic Register, offset: 0x244
    RESERVED_14 : array[0..3] of byte;
    IEEE_T_FRAME_OK : longword;       // *< Frames Transmitted OK Statistic Register, offset: 0x24C
    IEEE_T_1COL : longword;           // *< Frames Transmitted with Single Collision Statistic Register, offset: 0x250
    IEEE_T_MCOL : longword;           // *< Frames Transmitted with Multiple Collisions Statistic Register, offset: 0x254
    IEEE_T_DEF : longword;            // *< Frames Transmitted after Deferral Delay Statistic Register, offset: 0x258
    IEEE_T_LCOL : longword;           // *< Frames Transmitted with Late Collision Statistic Register, offset: 0x25C
    IEEE_T_EXCOL : longword;          // *< Frames Transmitted with Excessive Collisions Statistic Register, offset: 0x260
    IEEE_T_MACERR : longword;         // *< Frames Transmitted with Tx FIFO Underrun Statistic Register, offset: 0x264
    IEEE_T_CSERR : longword;          // *< Frames Transmitted with Carrier Sense Error Statistic Register, offset: 0x268
    RESERVED_15 : array[0..3] of byte;
    IEEE_T_FDXFC : longword;          // *< Flow Control Pause Frames Transmitted Statistic Register, offset: 0x270
    IEEE_T_OCTETS_OK : longword;      // *< Octet Count for Frames Transmitted w/o Error Statistic Register, offset: 0x274
    RESERVED_16 : array[0..11] of byte;
    RMON_R_PACKETS : longword;        // *< Rx Packet Count Statistic Register, offset: 0x284
    RMON_R_BC_PKT : longword;         // *< Rx Broadcast Packets Statistic Register, offset: 0x288
    RMON_R_MC_PKT : longword;         // *< Rx Multicast Packets Statistic Register, offset: 0x28C
    RMON_R_CRC_ALIGN : longword;      // *< Rx Packets with CRC/Align Error Statistic Register, offset: 0x290
    RMON_R_UNDERSIZE : longword;      // *< Rx Packets with Less Than 64 Bytes and Good CRC Statistic Register, offset: 0x294
    RMON_R_OVERSIZE : longword;       // *< Rx Packets Greater Than MAX_FL and Good CRC Statistic Register, offset: 0x298
    RMON_R_FRAG : longword;           // *< Rx Packets Less Than 64 Bytes and Bad CRC Statistic Register, offset: 0x29C
    RMON_R_JAB : longword;            // *< Rx Packets Greater Than MAX_FL Bytes and Bad CRC Statistic Register, offset: 0x2A0
    RESERVED_17 : array[0..3] of byte;
    RMON_R_P64 : longword;            // *< Rx 64-Byte Packets Statistic Register, offset: 0x2A8
    RMON_R_P65TO127 : longword;       // *< Rx 65- to 127-Byte Packets Statistic Register, offset: 0x2AC
    RMON_R_P128TO255 : longword;      // *< Rx 128- to 255-Byte Packets Statistic Register, offset: 0x2B0
    RMON_R_P256TO511 : longword;      // *< Rx 256- to 511-Byte Packets Statistic Register, offset: 0x2B4
    RMON_R_P512TO1023 : longword;     // *< Rx 512- to 1023-Byte Packets Statistic Register, offset: 0x2B8
    RMON_R_P1024TO2047 : longword;    // *< Rx 1024- to 2047-Byte Packets Statistic Register, offset: 0x2BC
    RMON_R_P_GTE2048 : longword;      // *< Rx Packets Greater than 2048 Bytes Statistic Register, offset: 0x2C0
    RMON_R_OCTETS : longword;         // *< Rx Octets Statistic Register, offset: 0x2C4
    IEEE_R_DROP : longword;           // *< Frames not Counted Correctly Statistic Register, offset: 0x2C8
    IEEE_R_FRAME_OK : longword;       // *< Frames Received OK Statistic Register, offset: 0x2CC
    IEEE_R_CRC : longword;            // *< Frames Received with CRC Error Statistic Register, offset: 0x2D0
    IEEE_R_ALIGN : longword;          // *< Frames Received with Alignment Error Statistic Register, offset: 0x2D4
    IEEE_R_MACERR : longword;         // *< Receive FIFO Overflow Count Statistic Register, offset: 0x2D8
    IEEE_R_FDXFC : longword;          // *< Flow Control Pause Frames Received Statistic Register, offset: 0x2DC
    IEEE_R_OCTETS_OK : longword;      // *< Octet Count for Frames Received without Error Statistic Register, offset: 0x2E0
    RESERVED_18 : array[0..283] of byte;
    ATCR       : longword;            // *< Adjustable Timer Control Register, offset: 0x400
    ATVR       : longword;            // *< Timer Value Register, offset: 0x404
    ATOFF      : longword;            // *< Timer Offset Register, offset: 0x408
    ATPER      : longword;            // *< Timer Period Register, offset: 0x40C
    ATCOR      : longword;            // *< Timer Correction Register, offset: 0x410
    ATINC      : longword;            // *< Time-Stamping Clock Period Register, offset: 0x414
    ATSTMP     : longword;            // *< Timestamp of Last Transmitted Frame, offset: 0x418
    RESERVED_19 : array[0..487] of byte;
    TGSR       : longword;            // *< Timer Global Status Register, offset: 0x604
    CHANNEL    : array[0..3] of TENET_CHANNEL;
  end;

const
  ENET_BASE    = $400C0000;

var
  ENET         : TENET_Registers absolute ENET_BASE;

type
  TEWM_Registers = record
    CTRL       : byte;                // *< Control Register, offset: 0x0
    SERV       : byte;                // *< Service Register, offset: 0x1
    CMPL       : byte;                // *< Compare Low Register, offset: 0x2
    CMPH       : byte;                // *< Compare High Register, offset: 0x3
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
    DATA_U     : longword;            // *< Cache Data Storage (upper word), array offset: 0x200, array step: index*0x20, index2*0x8
    DATA_L     : longword;            // *< Cache Data Storage (lower word), array offset: 0x204, array step: index*0x20, index2*0x8
  end;

  TFMC_Registers = record
    PFAPR      : longword;            // *< Flash Access Protection Register, offset: 0x0
    PFB0CR     : longword;            // *< Flash Bank 0 Control Register, offset: 0x4
    PFB1CR     : longword;            // *< Flash Bank 1 Control Register, offset: 0x8
    RESERVED_0 : array[0..243] of byte;
    TAGVDW0S   : array[0..3] of longword; // *< Cache Tag Storage, array offset: 0x100, array step: 0x4
    TAGVDW1S   : array[0..3] of longword; // *< Cache Tag Storage, array offset: 0x110, array step: 0x4
    TAGVDW2S   : array[0..3] of longword; // *< Cache Tag Storage, array offset: 0x120, array step: 0x4
    TAGVDW3S   : array[0..3] of longword; // *< Cache Tag Storage, array offset: 0x130, array step: 0x4
    RESERVED_1 : array[0..191] of byte;
    &SET       : array[0..3] of TFMC_SET;
  end;

const
  FMC_BASE     = $4001F000;

var
  FMC          : TFMC_Registers absolute FMC_BASE;

type
  TFTFE_Registers = record
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
    RESERVED_0 : array[0..1] of byte;
    FEPROT     : byte;                // *< EEPROM Protection Register, offset: 0x16
    FDPROT     : byte;                // *< Data Flash Protection Register, offset: 0x17
  end;

const
  FTFE_BASE    = $40020000;

var
  FTFE         : TFTFE_Registers absolute FTFE_BASE;

type
  TFTM_CONTROLS= record
    CnSC       : longword;            // *< Channel (n) Status And Control, array offset: 0xC, array step: 0x8
    CnV        : longword;            // *< Channel (n) Value, array offset: 0x10, array step: 0x8
  end;

  TFTM_Registers = record
    SC         : longword;            // *< Status And Control, offset: 0x0
    CNT        : longword;            // *< Counter, offset: 0x4
    &MOD       : longword;            // *< Modulo, offset: 0x8
    CONTROLS   : array[0..7] of TFTM_CONTROLS;
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
  FTM3_BASE    = $400B9000;

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

const
  I2C2_BASE    = $400E6000;

var
  I2C2         : TI2C_Registers absolute I2C2_BASE;

type
  TI2S_Registers = record
    TCSR       : longword;            // *< SAI Transmit Control Register, offset: 0x0
    TCR1       : longword;            // *< SAI Transmit Configuration 1 Register, offset: 0x4
    TCR2       : longword;            // *< SAI Transmit Configuration 2 Register, offset: 0x8
    TCR3       : longword;            // *< SAI Transmit Configuration 3 Register, offset: 0xC
    TCR4       : longword;            // *< SAI Transmit Configuration 4 Register, offset: 0x10
    TCR5       : longword;            // *< SAI Transmit Configuration 5 Register, offset: 0x14
    RESERVED_0 : array[0..7] of byte;
    TDR        : array[0..1] of longword; // *< SAI Transmit Data Register, array offset: 0x20, array step: 0x4
    RESERVED_1 : array[0..23] of byte;
    TFR        : array[0..1] of longword; // *< SAI Transmit FIFO Register, array offset: 0x40, array step: 0x4
    RESERVED_2 : array[0..23] of byte;
    TMR        : longword;            // *< SAI Transmit Mask Register, offset: 0x60
    RESERVED_3 : array[0..27] of byte;
    RCSR       : longword;            // *< SAI Receive Control Register, offset: 0x80
    RCR1       : longword;            // *< SAI Receive Configuration 1 Register, offset: 0x84
    RCR2       : longword;            // *< SAI Receive Configuration 2 Register, offset: 0x88
    RCR3       : longword;            // *< SAI Receive Configuration 3 Register, offset: 0x8C
    RCR4       : longword;            // *< SAI Receive Configuration 4 Register, offset: 0x90
    RCR5       : longword;            // *< SAI Receive Configuration 5 Register, offset: 0x94
    RESERVED_4 : array[0..7] of byte;
    RDR        : array[0..1] of longword; // *< SAI Receive Data Register, array offset: 0xA0, array step: 0x4
    RESERVED_5 : array[0..23] of byte;
    RFR        : array[0..1] of longword; // *< SAI Receive FIFO Register, array offset: 0xC0, array step: 0x4
    RESERVED_6 : array[0..23] of byte;
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
    RST        : byte;                // *< LLWU Reset Enable register, offset: 0xA
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
    CR         : longword;            // *< Control Register, offset: 0xC
    ISCR       : longword;            // *< Interrupt Status Register, offset: 0x10
    ETBCC      : longword;            // *< ETB Counter Control register, offset: 0x14
    ETBRL      : longword;            // *< ETB Reload register, offset: 0x18
    ETBCNT     : longword;            // *< ETB Counter Value register, offset: 0x1C
    RESERVED_1 : array[0..15] of byte;
    PID        : longword;            // *< Process ID register, offset: 0x30
  end;

const
  MCM_BASE     = $E0080000;

var
  MCM          : TMCM_Registers absolute MCM_BASE;

type
  TMPU_SP      = record
    EAR        : longword;            // *< Error Address Register, slave port n, array offset: 0x10, array step: 0x8
    EDR        : longword;            // *< Error Detail Register, slave port n, array offset: 0x14, array step: 0x8
  end;

  TMPU_Registers = record
    CESR       : longword;            // *< Control/Error Status Register, offset: 0x0
    RESERVED_0 : array[0..11] of byte;
    SP         : array[0..4] of TMPU_SP;
    RESERVED_1 : array[0..967] of byte;
    WORD       : array[0..11] of longword; // *< Region Descriptor n, Word 0..Region Descriptor n, Word 3, array offset: 0x400, array step: index*0x10, index2*0x4
    RESERVED_2 : array[0..831] of byte;
    RGDAAC     : array[0..11] of longword; // *< Region Descriptor Alternate Access Control n, array offset: 0x800, array step: 0x4
  end;

const
  MPU_BASE     = $4000D000;

var
  MPU          : TMPU_Registers absolute MPU_BASE;

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
    FEPROT     : byte;                // *< Non-volatile EERAM Protection Register, offset: 0xE
    FDPROT     : byte;                // *< Non-volatile D-Flash Protection Register, offset: 0xF
  end;

const
  FTFE_FlashConfig_BASE = $400;

var
  FTFE_FlashConfig : TNV_Registers absolute FTFE_FlashConfig_BASE;

type
  TOSC_Registers = record
    CR         : byte;                // *< OSC Control Register, offset: 0x0
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
    PODLY      : array[0..2] of longword; // *< Pulse-Out n Delay register, array offset: 0x194, array step: 0x4
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
  TSDHC_Registers = record
    DSADDR     : longword;            // *< DMA System Address register, offset: 0x0
    BLKATTR    : longword;            // *< Block Attributes register, offset: 0x4
    CMDARG     : longword;            // *< Command Argument register, offset: 0x8
    XFERTYP    : longword;            // *< Transfer Type register, offset: 0xC
    CMDRSP     : array[0..3] of longword; // *< Command Response 0..Command Response 3, array offset: 0x10, array step: 0x4
    DATPORT    : longword;            // *< Buffer Data Port register, offset: 0x20
    PRSSTAT    : longword;            // *< Present State register, offset: 0x24
    PROCTL     : longword;            // *< Protocol Control register, offset: 0x28
    SYSCTL     : longword;            // *< System Control register, offset: 0x2C
    IRQSTAT    : longword;            // *< Interrupt Status register, offset: 0x30
    IRQSTATEN  : longword;            // *< Interrupt Status Enable register, offset: 0x34
    IRQSIGEN   : longword;            // *< Interrupt Signal Enable register, offset: 0x38
    AC12ERR    : longword;            // *< Auto CMD12 Error Status Register, offset: 0x3C
    HTCAPBLT   : longword;            // *< Host Controller Capabilities, offset: 0x40
    WML        : longword;            // *< Watermark Level Register, offset: 0x44
    RESERVED_0 : array[0..7] of byte;
    FEVT       : longword;            // *< Force Event register, offset: 0x50
    ADMAES     : longword;            // *< ADMA Error Status register, offset: 0x54
    ADSADDR    : longword;            // *< ADMA System Addressregister, offset: 0x58
    RESERVED_1 : array[0..99] of byte;
    VENDOR     : longword;            // *< Vendor Specific register, offset: 0xC0
    MMCBOOT    : longword;            // *< MMC Boot register, offset: 0xC4
    RESERVED_2 : array[0..51] of byte;
    HOSTVER    : longword;            // *< Host Controller Version, offset: 0xFC
  end;

const
  SDHC_BASE    = $400B1000;

var
  SDHC         : TSDHC_Registers absolute SDHC_BASE;

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
    RESERVED_3 : array[0..7] of byte;
    SDID       : longword;            // *< System Device Identification Register, offset: 0x1024
    SCGC1      : longword;            // *< System Clock Gating Control Register 1, offset: 0x1028
    SCGC2      : longword;            // *< System Clock Gating Control Register 2, offset: 0x102C
    SCGC3      : longword;            // *< System Clock Gating Control Register 3, offset: 0x1030
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
    VLLSCTRL   : byte;                // *< VLLS Control register, offset: 0x2
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
    CTAR       : array[0..1] of longword; // *< Clock and Transfer Attributes Register (In Master Mode), array offset: 0xC, array step: 0x4
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

const
  SPI2_BASE    = $400AC000;

var
  SPI2         : TSPI_Registers absolute SPI2_BASE;

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
    WP7816T0   : byte;                // *< UART 7816 Wait Parameter Register, offset: 0x1B
    WN7816     : byte;                // *< UART 7816 Wait N Register, offset: 0x1C
    WF7816     : byte;                // *< UART 7816 Wait FD Register, offset: 0x1D
    ET7816     : byte;                // *< UART 7816 Error Threshold Register, offset: 0x1E
    TL7816     : byte;                // *< UART 7816 Transmit Length Register, offset: 0x1F
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

const
  UART3_BASE   = $4006D000;

var
  UART3        : TUART_Registers absolute UART3_BASE;

const
  UART4_BASE   = $400EA000;

var
  UART4        : TUART_Registers absolute UART4_BASE;

const
  UART5_BASE   = $400EB000;

var
  UART5        : TUART_Registers absolute UART5_BASE;

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
  TUSBDCD_Registers = record
    CONTROL    : longword;            // *< Control register, offset: 0x0
    CLOCK      : longword;            // *< Clock register, offset: 0x4
    STATUS     : longword;            // *< Status register, offset: 0x8
    RESERVED_0 : array[0..3] of byte;
    TIMER0     : longword;            // *< TIMER0 register, offset: 0x10
    TIMER1     : longword;            // *< TIMER1 register, offset: 0x14
    TIMER2_BC11: longword;            // *< TIMER2_BC11 register, offset: 0x18
  end;

const
  USBDCD_BASE  = $40035000;

var
  USBDCD       : TUSBDCD_Registers absolute USBDCD_BASE;

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
procedure FTFE_interrupt; external name 'FTFE_interrupt';
procedure Read_Collision_interrupt; external name 'Read_Collision_interrupt';
procedure LVD_LVW_interrupt; external name 'LVD_LVW_interrupt';
procedure LLWU_interrupt; external name 'LLWU_interrupt';
procedure WDOG_EWM_interrupt; external name 'WDOG_EWM_interrupt';
procedure RNG_interrupt; external name 'RNG_interrupt';
procedure I2C0_interrupt; external name 'I2C0_interrupt';
procedure I2C1_interrupt; external name 'I2C1_interrupt';
procedure SPI0_interrupt; external name 'SPI0_interrupt';
procedure SPI1_interrupt; external name 'SPI1_interrupt';
procedure I2S0_Tx_interrupt; external name 'I2S0_Tx_interrupt';
procedure I2S0_Rx_interrupt; external name 'I2S0_Rx_interrupt';
procedure UART0_LON_interrupt; external name 'UART0_LON_interrupt';
procedure UART0_RX_TX_interrupt; external name 'UART0_RX_TX_interrupt';
procedure UART0_ERR_interrupt; external name 'UART0_ERR_interrupt';
procedure UART1_RX_TX_interrupt; external name 'UART1_RX_TX_interrupt';
procedure UART1_ERR_interrupt; external name 'UART1_ERR_interrupt';
procedure UART2_RX_TX_interrupt; external name 'UART2_RX_TX_interrupt';
procedure UART2_ERR_interrupt; external name 'UART2_ERR_interrupt';
procedure UART3_RX_TX_interrupt; external name 'UART3_RX_TX_interrupt';
procedure UART3_ERR_interrupt; external name 'UART3_ERR_interrupt';
procedure ADC0_interrupt; external name 'ADC0_interrupt';
procedure CMP0_interrupt; external name 'CMP0_interrupt';
procedure CMP1_interrupt; external name 'CMP1_interrupt';
procedure FTM0_interrupt; external name 'FTM0_interrupt';
procedure FTM1_interrupt; external name 'FTM1_interrupt';
procedure FTM2_interrupt; external name 'FTM2_interrupt';
procedure CMT_interrupt; external name 'CMT_interrupt';
procedure RTC_interrupt; external name 'RTC_interrupt';
procedure RTC_Seconds_interrupt; external name 'RTC_Seconds_interrupt';
procedure PIT0_interrupt; external name 'PIT0_interrupt';
procedure PIT1_interrupt; external name 'PIT1_interrupt';
procedure PIT2_interrupt; external name 'PIT2_interrupt';
procedure PIT3_interrupt; external name 'PIT3_interrupt';
procedure PDB0_interrupt; external name 'PDB0_interrupt';
procedure USB0_interrupt; external name 'USB0_interrupt';
procedure USBDCD_interrupt; external name 'USBDCD_interrupt';
procedure RESERVED71_interrupt; external name 'RESERVED71_interrupt';
procedure DAC0_interrupt; external name 'DAC0_interrupt';
procedure MCG_interrupt; external name 'MCG_interrupt';
procedure LPTMR0_interrupt; external name 'LPTMR0_interrupt';
procedure PORTA_interrupt; external name 'PORTA_interrupt';
procedure PORTB_interrupt; external name 'PORTB_interrupt';
procedure PORTC_interrupt; external name 'PORTC_interrupt';
procedure PORTD_interrupt; external name 'PORTD_interrupt';
procedure PORTE_interrupt; external name 'PORTE_interrupt';
procedure SWI_interrupt; external name 'SWI_interrupt';
procedure SPI2_interrupt; external name 'SPI2_interrupt';
procedure UART4_RX_TX_interrupt; external name 'UART4_RX_TX_interrupt';
procedure UART4_ERR_interrupt; external name 'UART4_ERR_interrupt';
procedure UART5_RX_TX_interrupt; external name 'UART5_RX_TX_interrupt';
procedure UART5_ERR_interrupt; external name 'UART5_ERR_interrupt';
procedure CMP2_interrupt; external name 'CMP2_interrupt';
procedure FTM3_interrupt; external name 'FTM3_interrupt';
procedure DAC1_interrupt; external name 'DAC1_interrupt';
procedure ADC1_interrupt; external name 'ADC1_interrupt';
procedure I2C2_interrupt; external name 'I2C2_interrupt';
procedure CAN0_ORed_Message_buffer_interrupt; external name 'CAN0_ORed_Message_buffer_interrupt';
procedure CAN0_Bus_Off_interrupt; external name 'CAN0_Bus_Off_interrupt';
procedure CAN0_Error_interrupt; external name 'CAN0_Error_interrupt';
procedure CAN0_Tx_Warning_interrupt; external name 'CAN0_Tx_Warning_interrupt';
procedure CAN0_Rx_Warning_interrupt; external name 'CAN0_Rx_Warning_interrupt';
procedure CAN0_Wake_Up_interrupt; external name 'CAN0_Wake_Up_interrupt';
procedure SDHC_interrupt; external name 'SDHC_interrupt';
procedure ENET_1588_Timer_interrupt; external name 'ENET_1588_Timer_interrupt';
procedure ENET_Transmit_interrupt; external name 'ENET_Transmit_interrupt';
procedure ENET_Receive_interrupt; external name 'ENET_Receive_interrupt';
procedure ENET_Error_interrupt; external name 'ENET_Error_interrupt';

{$i cortexm4f_start.inc}
procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
  interrupt_vectors:
  .long _stack_top
  .long Startup                                  // int -15
  .long NonMaskableInt_interrupt                 // int -14
  .long HardFault_interrupt                      // int -13
  .long MemoryManagement_interrupt               // int -12
  .long BusFault_interrupt                       // int -11
  .long UsageFault_interrupt                     // int -10
  .long 0                                        // int -9
  .long 0                                        // int -8
  .long 0                                        // int -7
  .long 0                                        // int -6
  .long SVCall_interrupt                         // int -5
  .long DebugMonitor_interrupt                   // int -4
  .long 0                                        // int -3
  .long PendSV_interrupt                         // int -2
  .long SysTick_interrupt                        // int -1
  .long DMA0_interrupt                           // int 0
  .long DMA1_interrupt                           // int 1
  .long DMA2_interrupt                           // int 2
  .long DMA3_interrupt                           // int 3
  .long DMA4_interrupt                           // int 4
  .long DMA5_interrupt                           // int 5
  .long DMA6_interrupt                           // int 6
  .long DMA7_interrupt                           // int 7
  .long DMA8_interrupt                           // int 8
  .long DMA9_interrupt                           // int 9
  .long DMA10_interrupt                          // int 10
  .long DMA11_interrupt                          // int 11
  .long DMA12_interrupt                          // int 12
  .long DMA13_interrupt                          // int 13
  .long DMA14_interrupt                          // int 14
  .long DMA15_interrupt                          // int 15
  .long DMA_Error_interrupt                      // int 16
  .long MCM_interrupt                            // int 17
  .long FTFE_interrupt                           // int 18
  .long Read_Collision_interrupt                 // int 19
  .long LVD_LVW_interrupt                        // int 20
  .long LLWU_interrupt                           // int 21
  .long WDOG_EWM_interrupt                       // int 22
  .long RNG_interrupt                            // int 23
  .long I2C0_interrupt                           // int 24
  .long I2C1_interrupt                           // int 25
  .long SPI0_interrupt                           // int 26
  .long SPI1_interrupt                           // int 27
  .long I2S0_Tx_interrupt                        // int 28
  .long I2S0_Rx_interrupt                        // int 29
  .long UART0_LON_interrupt                      // int 30
  .long UART0_RX_TX_interrupt                    // int 31
  .long UART0_ERR_interrupt                      // int 32
  .long UART1_RX_TX_interrupt                    // int 33
  .long UART1_ERR_interrupt                      // int 34
  .long UART2_RX_TX_interrupt                    // int 35
  .long UART2_ERR_interrupt                      // int 36
  .long UART3_RX_TX_interrupt                    // int 37
  .long UART3_ERR_interrupt                      // int 38
  .long ADC0_interrupt                           // int 39
  .long CMP0_interrupt                           // int 40
  .long CMP1_interrupt                           // int 41
  .long FTM0_interrupt                           // int 42
  .long FTM1_interrupt                           // int 43
  .long FTM2_interrupt                           // int 44
  .long CMT_interrupt                            // int 45
  .long RTC_interrupt                            // int 46
  .long RTC_Seconds_interrupt                    // int 47
  .long PIT0_interrupt                           // int 48
  .long PIT1_interrupt                           // int 49
  .long PIT2_interrupt                           // int 50
  .long PIT3_interrupt                           // int 51
  .long PDB0_interrupt                           // int 52
  .long USB0_interrupt                           // int 53
  .long USBDCD_interrupt                         // int 54
  .long RESERVED71_interrupt                     // int 55
  .long DAC0_interrupt                           // int 56
  .long MCG_interrupt                            // int 57
  .long LPTMR0_interrupt                         // int 58
  .long PORTA_interrupt                          // int 59
  .long PORTB_interrupt                          // int 60
  .long PORTC_interrupt                          // int 61
  .long PORTD_interrupt                          // int 62
  .long PORTE_interrupt                          // int 63
  .long SWI_interrupt                            // int 64
  .long SPI2_interrupt                           // int 65
  .long UART4_RX_TX_interrupt                    // int 66
  .long UART4_ERR_interrupt                      // int 67
  .long UART5_RX_TX_interrupt                    // int 68
  .long UART5_ERR_interrupt                      // int 69
  .long CMP2_interrupt                           // int 70
  .long FTM3_interrupt                           // int 71
  .long DAC1_interrupt                           // int 72
  .long ADC1_interrupt                           // int 73
  .long I2C2_interrupt                           // int 74
  .long CAN0_ORed_Message_buffer_interrupt       // int 75
  .long CAN0_Bus_Off_interrupt                   // int 76
  .long CAN0_Error_interrupt                     // int 77
  .long CAN0_Tx_Warning_interrupt                // int 78
  .long CAN0_Rx_Warning_interrupt                // int 79
  .long CAN0_Wake_Up_interrupt                   // int 80
  .long SDHC_interrupt                           // int 81
  .long ENET_1588_Timer_interrupt                // int 82
  .long ENET_Transmit_interrupt                  // int 83
  .long ENET_Receive_interrupt                   // int 84
  .long ENET_Error_interrupt                     // int 85

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
  .weak FTFE_interrupt
  .weak Read_Collision_interrupt
  .weak LVD_LVW_interrupt
  .weak LLWU_interrupt
  .weak WDOG_EWM_interrupt
  .weak RNG_interrupt
  .weak I2C0_interrupt
  .weak I2C1_interrupt
  .weak SPI0_interrupt
  .weak SPI1_interrupt
  .weak I2S0_Tx_interrupt
  .weak I2S0_Rx_interrupt
  .weak UART0_LON_interrupt
  .weak UART0_RX_TX_interrupt
  .weak UART0_ERR_interrupt
  .weak UART1_RX_TX_interrupt
  .weak UART1_ERR_interrupt
  .weak UART2_RX_TX_interrupt
  .weak UART2_ERR_interrupt
  .weak UART3_RX_TX_interrupt
  .weak UART3_ERR_interrupt
  .weak ADC0_interrupt
  .weak CMP0_interrupt
  .weak CMP1_interrupt
  .weak FTM0_interrupt
  .weak FTM1_interrupt
  .weak FTM2_interrupt
  .weak CMT_interrupt
  .weak RTC_interrupt
  .weak RTC_Seconds_interrupt
  .weak PIT0_interrupt
  .weak PIT1_interrupt
  .weak PIT2_interrupt
  .weak PIT3_interrupt
  .weak PDB0_interrupt
  .weak USB0_interrupt
  .weak USBDCD_interrupt
  .weak RESERVED71_interrupt
  .weak DAC0_interrupt
  .weak MCG_interrupt
  .weak LPTMR0_interrupt
  .weak PORTA_interrupt
  .weak PORTB_interrupt
  .weak PORTC_interrupt
  .weak PORTD_interrupt
  .weak PORTE_interrupt
  .weak SWI_interrupt
  .weak SPI2_interrupt
  .weak UART4_RX_TX_interrupt
  .weak UART4_ERR_interrupt
  .weak UART5_RX_TX_interrupt
  .weak UART5_ERR_interrupt
  .weak CMP2_interrupt
  .weak FTM3_interrupt
  .weak DAC1_interrupt
  .weak ADC1_interrupt
  .weak I2C2_interrupt
  .weak CAN0_ORed_Message_buffer_interrupt
  .weak CAN0_Bus_Off_interrupt
  .weak CAN0_Error_interrupt
  .weak CAN0_Tx_Warning_interrupt
  .weak CAN0_Rx_Warning_interrupt
  .weak CAN0_Wake_Up_interrupt
  .weak SDHC_interrupt
  .weak ENET_1588_Timer_interrupt
  .weak ENET_Transmit_interrupt
  .weak ENET_Receive_interrupt
  .weak ENET_Error_interrupt
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
  .set FTFE_interrupt, HaltProc
  .set Read_Collision_interrupt, HaltProc
  .set LVD_LVW_interrupt, HaltProc
  .set LLWU_interrupt, HaltProc
  .set WDOG_EWM_interrupt, HaltProc
  .set RNG_interrupt, HaltProc
  .set I2C0_interrupt, HaltProc
  .set I2C1_interrupt, HaltProc
  .set SPI0_interrupt, HaltProc
  .set SPI1_interrupt, HaltProc
  .set I2S0_Tx_interrupt, HaltProc
  .set I2S0_Rx_interrupt, HaltProc
  .set UART0_LON_interrupt, HaltProc
  .set UART0_RX_TX_interrupt, HaltProc
  .set UART0_ERR_interrupt, HaltProc
  .set UART1_RX_TX_interrupt, HaltProc
  .set UART1_ERR_interrupt, HaltProc
  .set UART2_RX_TX_interrupt, HaltProc
  .set UART2_ERR_interrupt, HaltProc
  .set UART3_RX_TX_interrupt, HaltProc
  .set UART3_ERR_interrupt, HaltProc
  .set ADC0_interrupt, HaltProc
  .set CMP0_interrupt, HaltProc
  .set CMP1_interrupt, HaltProc
  .set FTM0_interrupt, HaltProc
  .set FTM1_interrupt, HaltProc
  .set FTM2_interrupt, HaltProc
  .set CMT_interrupt, HaltProc
  .set RTC_interrupt, HaltProc
  .set RTC_Seconds_interrupt, HaltProc
  .set PIT0_interrupt, HaltProc
  .set PIT1_interrupt, HaltProc
  .set PIT2_interrupt, HaltProc
  .set PIT3_interrupt, HaltProc
  .set PDB0_interrupt, HaltProc
  .set USB0_interrupt, HaltProc
  .set USBDCD_interrupt, HaltProc
  .set RESERVED71_interrupt, HaltProc
  .set DAC0_interrupt, HaltProc
  .set MCG_interrupt, HaltProc
  .set LPTMR0_interrupt, HaltProc
  .set PORTA_interrupt, HaltProc
  .set PORTB_interrupt, HaltProc
  .set PORTC_interrupt, HaltProc
  .set PORTD_interrupt, HaltProc
  .set PORTE_interrupt, HaltProc
  .set SWI_interrupt, HaltProc
  .set SPI2_interrupt, HaltProc
  .set UART4_RX_TX_interrupt, HaltProc
  .set UART4_ERR_interrupt, HaltProc
  .set UART5_RX_TX_interrupt, HaltProc
  .set UART5_ERR_interrupt, HaltProc
  .set CMP2_interrupt, HaltProc
  .set FTM3_interrupt, HaltProc
  .set DAC1_interrupt, HaltProc
  .set ADC1_interrupt, HaltProc
  .set I2C2_interrupt, HaltProc
  .set CAN0_ORed_Message_buffer_interrupt, HaltProc
  .set CAN0_Bus_Off_interrupt, HaltProc
  .set CAN0_Error_interrupt, HaltProc
  .set CAN0_Tx_Warning_interrupt, HaltProc
  .set CAN0_Rx_Warning_interrupt, HaltProc
  .set CAN0_Wake_Up_interrupt, HaltProc
  .set SDHC_interrupt, HaltProc
  .set ENET_1588_Timer_interrupt, HaltProc
  .set ENET_Transmit_interrupt, HaltProc
  .set ENET_Receive_interrupt, HaltProc
  .set ENET_Error_interrupt, HaltProc
  .text
end;
end.
