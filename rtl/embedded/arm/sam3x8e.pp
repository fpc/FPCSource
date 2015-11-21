unit sam3x8e;
// ---------------------------------------------------------------------------- //
//                  Atmel Microcontroller Software Support                      //
//                       SAM Software Package License                           //
// ---------------------------------------------------------------------------- //
// Copyright (c) %copyright_year%, Atmel Corporation                            //
//                                                                              //
// All rights reserved.                                                         //
//                                                                              //
// Redistribution and use in source and binary forms, with or without           //
// modification, are permitted provided that the following condition is met:    //
//                                                                              //
// - Redistributions of source code must retain the above copyright notice,     //
// this list of conditions and the disclaimer below.                            //
//                                                                              //
// Atmel's name may not be used to endorse or promote products derived from     //
// this software without specific prior written permission.                     //
//                                                                              //
// DISCLAIMER:  THIS SOFTWARE IS PROVIDED BY ATMEL "AS IS" AND ANY EXPRESS OR   //
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF //
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT ARE   //
// DISCLAIMED. IN NO EVENT SHALL ATMEL BE LIABLE FOR ANY DIRECT, INDIRECT,      //
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT //
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,  //
// OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF    //
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING         //
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, //
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                           //
// ---------------------------------------------------------------------------- //

interface
{$inline on}
{$goto on}
{$modeswitch advancedrecords}
{$PACKRECORDS 2}
type
  TIRQn_Enum = (
    NonMaskableInt_IRQn = -14,        // *<  2 Non Maskable Interrupt
    HardFault_IRQn = -13,
    MemoryManagement_IRQn = -12,      // *<  4 Cortex-M3 Memory Management Interrupt
    BusFault_IRQn = -11,              // *<  5 Cortex-M3 Bus Fault Interrupt
    UsageFault_IRQn = -10,            // *<  6 Cortex-M3 Usage Fault Interrupt
    SVCall_IRQn = -5,                 // *< 11 Cortex-M3 SV Call Interrupt
    DebugMonitor_IRQn = -4,           // *< 12 Cortex-M3 Debug Monitor Interrupt
    PendSV_IRQn = -2,                 // *< 14 Cortex-M3 Pend SV Interrupt
    SysTick_IRQn = -1,                // *< 15 Cortex-M3 System Tick Interrupt
    SUPC_IRQn  = 0,                   // *<  0 SAM3X8E Supply Controller (SUPC)
    RSTC_IRQn  = 1,                   // *<  1 SAM3X8E Reset Controller (RSTC)
    RTC_IRQn   = 2,                   // *<  2 SAM3X8E Real Time Clock (RTC)
    RTT_IRQn   = 3,                   // *<  3 SAM3X8E Real Time Timer (RTT)
    WDT_IRQn   = 4,                   // *<  4 SAM3X8E Watchdog Timer (WDT)
    PMC_IRQn   = 5,                   // *<  5 SAM3X8E Power Management Controller (PMC)
    EFC0_IRQn  = 6,                   // *<  6 SAM3X8E Enhanced Flash Controller 0 (EFC0)
    EFC1_IRQn  = 7,                   // *<  7 SAM3X8E Enhanced Flash Controller 1 (EFC1)
    UART_IRQn  = 8,                   // *<  8 SAM3X8E Universal Asynchronous Receiver Transceiver (UART)
    SMC_IRQn   = 9,                   // *<  9 SAM3X8E Static Memory Controller (SMC)
    PIOA_IRQn  = 11,                  // *< 11 SAM3X8E Parallel I/O Controller A, (PIOA)
    PIOB_IRQn  = 12,                  // *< 12 SAM3X8E Parallel I/O Controller B (PIOB)
    PIOC_IRQn  = 13,                  // *< 13 SAM3X8E Parallel I/O Controller C (PIOC)
    PIOD_IRQn  = 14,                  // *< 14 SAM3X8E Parallel I/O Controller D (PIOD)
    USART0_IRQn = 17,                 // *< 17 SAM3X8E USART 0 (USART0)
    USART1_IRQn = 18,                 // *< 18 SAM3X8E USART 1 (USART1)
    USART2_IRQn = 19,                 // *< 19 SAM3X8E USART 2 (USART2)
    USART3_IRQn = 20,                 // *< 20 SAM3X8E USART 3 (USART3)
    HSMCI_IRQn = 21,                  // *< 21 SAM3X8E Multimedia Card Interface (HSMCI)
    TWI0_IRQn  = 22,                  // *< 22 SAM3X8E Two-Wire Interface 0 (TWI0)
    TWI1_IRQn  = 23,                  // *< 23 SAM3X8E Two-Wire Interface 1 (TWI1)
    SPI0_IRQn  = 24,                  // *< 24 SAM3X8E Serial Peripheral Interface (SPI0)
    SSC_IRQn   = 26,                  // *< 26 SAM3X8E Synchronous Serial Controller (SSC)
    TC0_IRQn   = 27,                  // *< 27 SAM3X8E Timer Counter 0 (TC0)
    TC1_IRQn   = 28,                  // *< 28 SAM3X8E Timer Counter 1 (TC1)
    TC2_IRQn   = 29,                  // *< 29 SAM3X8E Timer Counter 2 (TC2)
    TC3_IRQn   = 30,                  // *< 30 SAM3X8E Timer Counter 3 (TC3)
    TC4_IRQn   = 31,                  // *< 31 SAM3X8E Timer Counter 4 (TC4)
    TC5_IRQn   = 32,                  // *< 32 SAM3X8E Timer Counter 5 (TC5)
    TC6_IRQn   = 33,                  // *< 33 SAM3X8E Timer Counter 6 (TC6)
    TC7_IRQn   = 34,                  // *< 34 SAM3X8E Timer Counter 7 (TC7)
    TC8_IRQn   = 35,                  // *< 35 SAM3X8E Timer Counter 8 (TC8)
    PWM_IRQn   = 36,                  // *< 36 SAM3X8E Pulse Width Modulation Controller (PWM)
    ADC_IRQn   = 37,                  // *< 37 SAM3X8E ADC Controller (ADC)
    DACC_IRQn  = 38,                  // *< 38 SAM3X8E DAC Controller (DACC)
    DMAC_IRQn  = 39,                  // *< 39 SAM3X8E DMA Controller (DMAC)
    UOTGHS_IRQn = 40,                 // *< 40 SAM3X8E USB OTG High Speed (UOTGHS)
    TRNG_IRQn  = 41,                  // *< 41 SAM3X8E True Random Number Generator (TRNG)
    EMAC_IRQn  = 42,                  // *< 42 SAM3X8E Ethernet MAC (EMAC)
    CAN0_IRQn  = 43,                  // *< 43 SAM3X8E CAN Controller 0 (CAN0)
    CAN1_IRQn  = 44                   // *< 44 SAM3X8E CAN Controller 1 (CAN1)
  );

  TADC_Registers = record
    CR         : longword;            // *< \brief (Adc Offset: 0x00) Control Register
    MR         : longword;            // *< \brief (Adc Offset: 0x04) Mode Register
    SEQR1      : longword;            // *< \brief (Adc Offset: 0x08) Channel Sequence Register 1
    SEQR2      : longword;            // *< \brief (Adc Offset: 0x0C) Channel Sequence Register 2
    CHER       : longword;            // *< \brief (Adc Offset: 0x10) Channel Enable Register
    CHDR       : longword;            // *< \brief (Adc Offset: 0x14) Channel Disable Register
    CHSR       : longword;            // *< \brief (Adc Offset: 0x18) Channel Status Register
    Reserved1  : array[0..0] of longword;
    LCDR       : longword;            // *< \brief (Adc Offset: 0x20) Last Converted Data Register
    IER        : longword;            // *< \brief (Adc Offset: 0x24) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Adc Offset: 0x28) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Adc Offset: 0x2C) Interrupt Mask Register
    ISR        : longword;            // *< \brief (Adc Offset: 0x30) Interrupt Status Register
    Reserved2  : array[0..1] of longword;
    OVER       : longword;            // *< \brief (Adc Offset: 0x3C) Overrun Status Register
    EMR        : longword;            // *< \brief (Adc Offset: 0x40) Extended Mode Register
    CWR        : longword;            // *< \brief (Adc Offset: 0x44) Compare Window Register
    CGR        : longword;            // *< \brief (Adc Offset: 0x48) Channel Gain Register
    COR        : longword;            // *< \brief (Adc Offset: 0x4C) Channel Offset Register
    CDR        : array[0..15] of longword; // *< \brief (Adc Offset: 0x50) Channel Data Register
    Reserved3  : array[0..0] of longword;
    ACR        : longword;            // *< \brief (Adc Offset: 0x94) Analog Control Register
    Reserved4  : array[0..18] of longword;
    WPMR       : longword;            // *< \brief (Adc Offset: 0xE4) Write Protect Mode Register
    WPSR       : longword;            // *< \brief (Adc Offset: 0xE8) Write Protect Status Register
    Reserved5  : array[0..4] of longword;
    RPR        : longword;            // *< \brief (Adc Offset: 0x100) Receive Pointer Register
    RCR        : longword;            // *< \brief (Adc Offset: 0x104) Receive Counter Register
    Reserved6  : array[0..1] of longword;
    RNPR       : longword;            // *< \brief (Adc Offset: 0x110) Receive Next Pointer Register
    RNCR       : longword;            // *< \brief (Adc Offset: 0x114) Receive Next Counter Register
    Reserved7  : array[0..1] of longword;
    PTCR       : longword;            // *< \brief (Adc Offset: 0x120) Transfer Control Register
    PTSR       : longword;            // *< \brief (Adc Offset: 0x124) Transfer Status Register
  end;

  TCANMB_Registers = record
    MMR        : longword;            // *< \brief (CanMb Offset: 0x0) Mailbox Mode Register
    MAM        : longword;            // *< \brief (CanMb Offset: 0x4) Mailbox Acceptance Mask Register
    MID        : longword;            // *< \brief (CanMb Offset: 0x8) Mailbox ID Register
    MFID       : longword;            // *< \brief (CanMb Offset: 0xC) Mailbox Family ID Register
    MSR        : longword;            // *< \brief (CanMb Offset: 0x10) Mailbox Status Register
    MDL        : longword;            // *< \brief (CanMb Offset: 0x14) Mailbox Data Low Register
    MDH        : longword;            // *< \brief (CanMb Offset: 0x18) Mailbox Data High Register
    MCR        : longword;            // *< \brief (CanMb Offset: 0x1C) Mailbox Control Register
  end;

  TCAN_Registers = record
    MR         : longword;            // *< \brief (Can Offset: 0x0000) Mode Register
    IER        : longword;            // *< \brief (Can Offset: 0x0004) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Can Offset: 0x0008) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Can Offset: 0x000C) Interrupt Mask Register
    SR         : longword;            // *< \brief (Can Offset: 0x0010) Status Register
    BR         : longword;            // *< \brief (Can Offset: 0x0014) Baudrate Register
    TIM        : longword;            // *< \brief (Can Offset: 0x0018) Timer Register
    TIMESTP    : longword;           // *< \brief (Can Offset: 0x001C) Timestamp Register
    ECR        : longword;            // *< \brief (Can Offset: 0x0020) Error Counter Register
    TCR        : longword;            // *< \brief (Can Offset: 0x0024) Transfer Command Register
    ACR        : longword;            // *< \brief (Can Offset: 0x0028) Abort Command Register
    Reserved1  : array[0..45] of longword;
    WPMR       : longword;            // *< \brief (Can Offset: 0x00E4) Write Protect Mode Register
    WPSR       : longword;            // *< \brief (Can Offset: 0x00E8) Write Protect Status Register
    Reserved2  : array[0..68] of longword;
    MB         : array[0..7] of TCanMb_Registers; // *< \brief (Can Offset: 0x200) MB = 0 .. 7
  end;

  TCHIPID_Registers = record
    CIDR       : longword;           // *< \brief (Chipid Offset: 0x0) Chip ID Register
    EXID       : longword;           // *< \brief (Chipid Offset: 0x4) Chip ID Extension Register
  end;

  TDACC_Registers = record
    CR         : longword;            // *< \brief (Dacc Offset: 0x00) Control Register
    MR         : longword;            // *< \brief (Dacc Offset: 0x04) Mode Register
    Reserved1  : array[0..1] of longword;
    CHER       : longword;            // *< \brief (Dacc Offset: 0x10) Channel Enable Register
    CHDR       : longword;            // *< \brief (Dacc Offset: 0x14) Channel Disable Register
    CHSR       : longword;            // *< \brief (Dacc Offset: 0x18) Channel Status Register
    Reserved2  : array[0..0] of longword;
    CDR        : longword;            // *< \brief (Dacc Offset: 0x20) Conversion Data Register
    IER        : longword;            // *< \brief (Dacc Offset: 0x24) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Dacc Offset: 0x28) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Dacc Offset: 0x2C) Interrupt Mask Register
    ISR        : longword;            // *< \brief (Dacc Offset: 0x30) Interrupt Status Register
    Reserved3  : array[0..23] of longword;
    ACR        : longword;            // *< \brief (Dacc Offset: 0x94) Analog Current Register
    Reserved4  : array[0..18] of longword;
    WPMR       : longword;            // *< \brief (Dacc Offset: 0xE4) Write Protect Mode register
    WPSR       : longword;            // *< \brief (Dacc Offset: 0xE8) Write Protect Status register
    Reserved5  : array[0..6] of longword;
    TPR        : longword;            // *< \brief (Dacc Offset: 0x108) Transmit Pointer Register
    TCR        : longword;            // *< \brief (Dacc Offset: 0x10C) Transmit Counter Register
    Reserved6  : array[0..1] of longword;
    TNPR       : longword;            // *< \brief (Dacc Offset: 0x118) Transmit Next Pointer Register
    TNCR       : longword;            // *< \brief (Dacc Offset: 0x11C) Transmit Next Counter Register
    PTCR       : longword;            // *< \brief (Dacc Offset: 0x120) Transfer Control Register
    PTSR       : longword;            // *< \brief (Dacc Offset: 0x124) Transfer Status Register
  end;

  TDMACCH_NUM_Registers = record
    DMAC_SADDR : longword;            // *< \brief (DmacCh_num Offset: 0x0) DMAC Channel Source Address Register
    DMAC_DADDR : longword;            // *< \brief (DmacCh_num Offset: 0x4) DMAC Channel Destination Address Register
    DMAC_DSCR  : longword;            // *< \brief (DmacCh_num Offset: 0x8) DMAC Channel Descriptor Address Register
    DMAC_CTRLA : longword;            // *< \brief (DmacCh_num Offset: 0xC) DMAC Channel Control A Register
    DMAC_CTRLB : longword;            // *< \brief (DmacCh_num Offset: 0x10) DMAC Channel Control B Register
    DMAC_CFG   : longword;            // *< \brief (DmacCh_num Offset: 0x14) DMAC Channel Configuration Register
    Reserved1  : array[0..3] of longword;
  end;

  TDMAC_Registers = record
    GCFG       : longword;            // *< \brief (Dmac Offset: 0x000) DMAC Global Configuration Register
    EN         : longword;            // *< \brief (Dmac Offset: 0x004) DMAC Enable Register
    SREQ       : longword;            // *< \brief (Dmac Offset: 0x008) DMAC Software Single Request Register
    CREQ       : longword;            // *< \brief (Dmac Offset: 0x00C) DMAC Software Chunk Transfer Request Register
    LAST       : longword;            // *< \brief (Dmac Offset: 0x010) DMAC Software Last Transfer Flag Register
    Reserved1  : array[0..0] of longword;
    EBCIER     : longword;           // *< \brief (Dmac Offset: 0x018) DMAC Error, Chained Buffer Transfer Completed Interrupt and Buffer Transfer Completed Interrupt Enable register.
    EBCIDR     : longword;           // *< \brief (Dmac Offset: 0x01C) DMAC Error, Chained Buffer Transfer Completed Interrupt and Buffer Transfer Completed Interrupt Disable register.
    EBCIMR     : longword;           // *< \brief (Dmac Offset: 0x020) DMAC Error, Chained Buffer Transfer Completed Interrupt and Buffer transfer completed Mask Register.
    EBCISR     : longword;           // *< \brief (Dmac Offset: 0x024) DMAC Error, Chained Buffer Transfer Completed Interrupt and Buffer transfer completed Status Register.
    CHER       : longword;            // *< \brief (Dmac Offset: 0x028) DMAC Channel Handler Enable Register
    CHDR       : longword;            // *< \brief (Dmac Offset: 0x02C) DMAC Channel Handler Disable Register
    CHSR       : longword;            // *< \brief (Dmac Offset: 0x030) DMAC Channel Handler Status Register
    Reserved2  : array[0..1] of longword;
    CH_NUM     : array[0..5] of TDmacCh_num_Registers; // *< \brief (Dmac Offset: 0x3C) ch_num = 0 .. 5
    Reserved3  : array[0..45] of longword;
    WPMR       : longword;            // *< \brief (Dmac Offset: 0x1E4) DMAC Write Protect Mode Register
    WPSR       : longword;            // *< \brief (Dmac Offset: 0x1E8) DMAC Write Protect Status Register
  end;

  TEFC_Registers = record
    FMR        : longword;            // *< \brief (Efc Offset: 0x00) EEFC Flash Mode Register
    FCR        : longword;            // *< \brief (Efc Offset: 0x04) EEFC Flash Command Register
    FSR        : longword;            // *< \brief (Efc Offset: 0x08) EEFC Flash Status Register
    FRR        : longword;            // *< \brief (Efc Offset: 0x0C) EEFC Flash Result Register
  end;

  TEMACSA_Registers = record
    SAxB       : longword;            // *< \brief (EmacSa Offset: 0x0) Specific Address 1 Bottom Register
    SAxT       : longword;            // *< \brief (EmacSa Offset: 0x4) Specific Address 1 Top Register
  end;

  TEMAC_Registers = record
    NCR        : longword;            // *< \brief (Emac Offset: 0x00) Network Control Register
    NCFGR      : longword;            // *< \brief (Emac Offset: 0x04) Network Configuration Register
    NSR        : longword;            // *< \brief (Emac Offset: 0x08) Network Status Register
    Reserved1  : array[0..1] of longword;
    TSR        : longword;            // *< \brief (Emac Offset: 0x14) Transmit Status Register
    RBQP       : longword;            // *< \brief (Emac Offset: 0x18) Receive Buffer Queue Pointer Register
    TBQP       : longword;            // *< \brief (Emac Offset: 0x1C) Transmit Buffer Queue Pointer Register
    RSR        : longword;            // *< \brief (Emac Offset: 0x20) Receive Status Register
    ISR        : longword;            // *< \brief (Emac Offset: 0x24) Interrupt Status Register
    IER        : longword;            // *< \brief (Emac Offset: 0x28) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Emac Offset: 0x2C) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Emac Offset: 0x30) Interrupt Mask Register
    MAN        : longword;            // *< \brief (Emac Offset: 0x34) Phy Maintenance Register
    PTR        : longword;            // *< \brief (Emac Offset: 0x38) Pause Time Register
    PFR        : longword;            // *< \brief (Emac Offset: 0x3C) Pause Frames Received Register
    FTO        : longword;            // *< \brief (Emac Offset: 0x40) Frames Transmitted Ok Register
    SCF        : longword;            // *< \brief (Emac Offset: 0x44) Single Collision Frames Register
    MCF        : longword;            // *< \brief (Emac Offset: 0x48) Multiple Collision Frames Register
    FRO        : longword;            // *< \brief (Emac Offset: 0x4C) Frames Received Ok Register
    FCSE       : longword;            // *< \brief (Emac Offset: 0x50) Frame Check Sequence Errors Register
    ALE        : longword;            // *< \brief (Emac Offset: 0x54) Alignment Errors Register
    DTF        : longword;            // *< \brief (Emac Offset: 0x58) Deferred Transmission Frames Register
    LCOL       : longword;            // *< \brief (Emac Offset: 0x5C) Late Collisions Register
    ECOL       : longword;            // *< \brief (Emac Offset: 0x60) Excessive Collisions Register
    TUND       : longword;            // *< \brief (Emac Offset: 0x64) Transmit Underrun Errors Register
    CSE        : longword;            // *< \brief (Emac Offset: 0x68) Carrier Sense Errors Register
    RRE        : longword;            // *< \brief (Emac Offset: 0x6C) Receive Resource Errors Register
    ROV        : longword;            // *< \brief (Emac Offset: 0x70) Receive Overrun Errors Register
    RSE        : longword;            // *< \brief (Emac Offset: 0x74) Receive Symbol Errors Register
    ELE        : longword;            // *< \brief (Emac Offset: 0x78) Excessive Length Errors Register
    RJA        : longword;            // *< \brief (Emac Offset: 0x7C) Receive Jabbers Register
    USF        : longword;            // *< \brief (Emac Offset: 0x80) Undersize Frames Register
    STE        : longword;            // *< \brief (Emac Offset: 0x84) SQE Test Errors Register
    RLE        : longword;            // *< \brief (Emac Offset: 0x88) Received Length Field Mismatch Register
    Reserved2  : array[0..0] of longword;
    HRB        : longword;            // *< \brief (Emac Offset: 0x90) Hash Register Bottom [31:0] Register
    HRT        : longword;            // *< \brief (Emac Offset: 0x94) Hash Register Top [63:32] Register
    SA         : array[0..3] of TEmacSa_Registers; // *< \brief (Emac Offset: 0x98) sa = 1 .. 4
    TID        : longword;            // *< \brief (Emac Offset: 0xB8) Type ID Checking Register
    Reserved3  : array[0..0] of longword;
    USRIO      : longword;            // *< \brief (Emac Offset: 0xC0) User Input/Output Register
  end;

  TGPBR_Registers = record
    SYS_GPBR   : array[0..7] of longword; // *< \brief (Gpbr Offset: 0x0) General Purpose Backup Register
  end;

  THSMCI_Registers = record
    CR         : longword;            // *< \brief (Hsmci Offset: 0x00) Control Register
    MR         : longword;            // *< \brief (Hsmci Offset: 0x04) Mode Register
    DTOR       : longword;            // *< \brief (Hsmci Offset: 0x08) Data Timeout Register
    SDCR       : longword;            // *< \brief (Hsmci Offset: 0x0C) SD/SDIO Card Register
    ARGR       : longword;            // *< \brief (Hsmci Offset: 0x10) Argument Register
    CMDR       : longword;            // *< \brief (Hsmci Offset: 0x14) Command Register
    BLKR       : longword;            // *< \brief (Hsmci Offset: 0x18) Block Register
    CSTOR      : longword;           // *< \brief (Hsmci Offset: 0x1C) Completion Signal Timeout Register
    RSPR       : array[0..3] of longword; // *< \brief (Hsmci Offset: 0x20) Response Register
    RDR        : longword;            // *< \brief (Hsmci Offset: 0x30) Receive Data Register
    TDR        : longword;            // *< \brief (Hsmci Offset: 0x34) Transmit Data Register
    Reserved1  : array[0..1] of longword;
    SR         : longword;            // *< \brief (Hsmci Offset: 0x40) Status Register
    IER        : longword;            // *< \brief (Hsmci Offset: 0x44) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Hsmci Offset: 0x48) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Hsmci Offset: 0x4C) Interrupt Mask Register
    DMA        : longword;            // *< \brief (Hsmci Offset: 0x50) DMA Configuration Register
    CFG        : longword;            // *< \brief (Hsmci Offset: 0x54) Configuration Register
    Reserved2  : array[0..34] of longword;
    WPMR       : longword;            // *< \brief (Hsmci Offset: 0xE4) Write Protection Mode Register
    WPSR       : longword;            // *< \brief (Hsmci Offset: 0xE8) Write Protection Status Register
    Reserved3  : array[0..68] of longword;
    FIFO       : array[0..255] of longword; // *< \brief (Hsmci Offset: 0x200) FIFO Memory Aperture0
  end;

  TMATRIX_Registers = record
    MCFG       : array[0..5] of longword; // *< \brief (Matrix Offset: 0x0000) Master Configuration Register
    Reserved1  : array[0..9] of longword;
    SCFG       : array[0..8] of longword; // *< \brief (Matrix Offset: 0x0040) Slave Configuration Register
    Reserved2  : array[0..6] of longword;
    PRAS0      : longword;          // *< \brief (Matrix Offset: 0x0080) Priority Register A for Slave 0
    Reserved3  : array[0..0] of longword;
    PRAS1      : longword;          // *< \brief (Matrix Offset: 0x0088) Priority Register A for Slave 1
    Reserved4  : array[0..0] of longword;
    PRAS2      : longword;          // *< \brief (Matrix Offset: 0x0090) Priority Register A for Slave 2
    Reserved5  : array[0..0] of longword;
    PRAS3      : longword;          // *< \brief (Matrix Offset: 0x0098) Priority Register A for Slave 3
    Reserved6  : array[0..0] of longword;
    PRAS4      : longword;          // *< \brief (Matrix Offset: 0x00A0) Priority Register A for Slave 4
    Reserved7  : array[0..0] of longword;
    PRAS5      : longword;          // *< \brief (Matrix Offset: 0x00A8) Priority Register A for Slave 5
    Reserved8  : array[0..0] of longword;
    PRAS6      : longword;          // *< \brief (Matrix Offset: 0x00B0) Priority Register A for Slave 6
    Reserved9  : array[0..0] of longword;
    PRAS7      : longword;          // *< \brief (Matrix Offset: 0x00B8) Priority Register A for Slave 7
    Reserved10 : array[0..0] of longword;
    PRAS8      : longword;          // *< \brief (Matrix Offset: 0x00C0) Priority Register A for Slave 8
    Reserved11 : array[0..0] of longword;
    Reserved12 : array[0..13] of longword;
    MRCR       : longword;           // *< \brief (Matrix Offset: 0x0100) Master Remap Control Register
    Reserved13 : array[0..3] of longword;
    CCFG_SYSIO : longword;            // *< \brief (Matrix Offset: 0x0114) System I/O Configuration register
    Reserved14 : array[0..50] of longword;
    WPMR       : longword;           // *< \brief (Matrix Offset: 0x1E4) Write Protect Mode Register
    WPSR       : longword;           // *< \brief (Matrix Offset: 0x1E8) Write Protect Status Register
  end;

  TPDC_Registers = record
    RPR        : longword;            // *< \brief (Pdc Offset: 0x0) Receive Pointer Register
    RCR        : longword;            // *< \brief (Pdc Offset: 0x4) Receive Counter Register
    TPR        : longword;            // *< \brief (Pdc Offset: 0x8) Transmit Pointer Register
    TCR        : longword;            // *< \brief (Pdc Offset: 0xC) Transmit Counter Register
    RNPR       : longword;           // *< \brief (Pdc Offset: 0x10) Receive Next Pointer Register
    RNCR       : longword;           // *< \brief (Pdc Offset: 0x14) Receive Next Counter Register
    TNPR       : longword;           // *< \brief (Pdc Offset: 0x18) Transmit Next Pointer Register
    TNCR       : longword;           // *< \brief (Pdc Offset: 0x1C) Transmit Next Counter Register
    PTCR       : longword;           // *< \brief (Pdc Offset: 0x20) Transfer Control Register
    PTSR       : longword;           // *< \brief (Pdc Offset: 0x24) Transfer Status Register
  end;

  TPIO_Registers = record
    PER        : longword;            // *< \brief (Pio Offset: 0x0000) PIO Enable Register
    PDR        : longword;            // *< \brief (Pio Offset: 0x0004) PIO Disable Register
    PSR        : longword;            // *< \brief (Pio Offset: 0x0008) PIO Status Register
    Reserved1  : array[0..0] of longword;
    OER        : longword;            // *< \brief (Pio Offset: 0x0010) Output Enable Register
    ODR        : longword;            // *< \brief (Pio Offset: 0x0014) Output Disable Register
    OSR        : longword;            // *< \brief (Pio Offset: 0x0018) Output Status Register
    Reserved2  : array[0..0] of longword;
    IFER       : longword;            // *< \brief (Pio Offset: 0x0020) Glitch Input Filter Enable Register
    IFDR       : longword;            // *< \brief (Pio Offset: 0x0024) Glitch Input Filter Disable Register
    IFSR       : longword;            // *< \brief (Pio Offset: 0x0028) Glitch Input Filter Status Register
    Reserved3  : array[0..0] of longword;
    SODR       : longword;            // *< \brief (Pio Offset: 0x0030) Set Output Data Register
    CODR       : longword;            // *< \brief (Pio Offset: 0x0034) Clear Output Data Register
    ODSR       : longword;            // *< \brief (Pio Offset: 0x0038) Output Data Status Register
    PDSR       : longword;            // *< \brief (Pio Offset: 0x003C) Pin Data Status Register
    IER        : longword;            // *< \brief (Pio Offset: 0x0040) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Pio Offset: 0x0044) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Pio Offset: 0x0048) Interrupt Mask Register
    ISR        : longword;            // *< \brief (Pio Offset: 0x004C) Interrupt Status Register
    MDER       : longword;            // *< \brief (Pio Offset: 0x0050) Multi-driver Enable Register
    MDDR       : longword;            // *< \brief (Pio Offset: 0x0054) Multi-driver Disable Register
    MDSR       : longword;            // *< \brief (Pio Offset: 0x0058) Multi-driver Status Register
    Reserved4  : array[0..0] of longword;
    PUDR       : longword;            // *< \brief (Pio Offset: 0x0060) Pull-up Disable Register
    PUER       : longword;            // *< \brief (Pio Offset: 0x0064) Pull-up Enable Register
    PUSR       : longword;            // *< \brief (Pio Offset: 0x0068) Pad Pull-up Status Register
    Reserved5  : array[0..0] of longword;
    ABSR       : longword;            // *< \brief (Pio Offset: 0x0070) Peripheral AB Select Register
    Reserved6  : array[0..2] of longword;
    SCIFSR     : longword;            // *< \brief (Pio Offset: 0x0080) System Clock Glitch Input Filter Select Register
    DIFSR      : longword;            // *< \brief (Pio Offset: 0x0084) Debouncing Input Filter Select Register
    IFDGSR     : longword;            // *< \brief (Pio Offset: 0x0088) Glitch or Debouncing Input Filter Clock Selection Status Register
    SCDR       : longword;            // *< \brief (Pio Offset: 0x008C) Slow Clock Divider Debouncing Register
    Reserved7  : array[0..3] of longword;
    OWER       : longword;            // *< \brief (Pio Offset: 0x00A0) Output Write Enable
    OWDR       : longword;            // *< \brief (Pio Offset: 0x00A4) Output Write Disable
    OWSR       : longword;            // *< \brief (Pio Offset: 0x00A8) Output Write Status Register
    Reserved8  : array[0..0] of longword;
    AIMER      : longword;            // *< \brief (Pio Offset: 0x00B0) Additional Interrupt Modes Enable Register
    AIMDR      : longword;            // *< \brief (Pio Offset: 0x00B4) Additional Interrupt Modes Disables Register
    AIMMR      : longword;            // *< \brief (Pio Offset: 0x00B8) Additional Interrupt Modes Mask Register
    Reserved9  : array[0..0] of longword;
    ESR        : longword;            // *< \brief (Pio Offset: 0x00C0) Edge Select Register
    LSR        : longword;            // *< \brief (Pio Offset: 0x00C4) Level Select Register
    ELSR       : longword;            // *< \brief (Pio Offset: 0x00C8) Edge/Level Status Register
    Reserved10 : array[0..0] of longword;
    FELLSR     : longword;            // *< \brief (Pio Offset: 0x00D0) Falling Edge/Low Level Select Register
    REHLSR     : longword;            // *< \brief (Pio Offset: 0x00D4) Rising Edge/ High Level Select Register
    FRLHSR     : longword;            // *< \brief (Pio Offset: 0x00D8) Fall/Rise - Low/High Status Register
    Reserved11 : array[0..0] of longword;
    LOCKSR     : longword;            // *< \brief (Pio Offset: 0x00E0) Lock Status
    WPMR       : longword;            // *< \brief (Pio Offset: 0x00E4) Write Protect Mode Register
    WPSR       : longword;            // *< \brief (Pio Offset: 0x00E8) Write Protect Status Register
  end;

  TPMC_Registers = record
    SCER       : longword;            // *< \brief (Pmc Offset: 0x0000) System Clock Enable Register
    SCDR       : longword;            // *< \brief (Pmc Offset: 0x0004) System Clock Disable Register
    SCSR       : longword;            // *< \brief (Pmc Offset: 0x0008) System Clock Status Register
    Reserved1  : array[0..0] of longword;
    PCER0      : longword;            // *< \brief (Pmc Offset: 0x0010) Peripheral Clock Enable Register 0
    PCDR0      : longword;            // *< \brief (Pmc Offset: 0x0014) Peripheral Clock Disable Register 0
    PCSR0      : longword;            // *< \brief (Pmc Offset: 0x0018) Peripheral Clock Status Register 0
    CKGR_UCKR  : longword;            // *< \brief (Pmc Offset: 0x001C) UTMI Clock Register
    CKGR_MOR   : longword;            // *< \brief (Pmc Offset: 0x0020) Main Oscillator Register
    CKGR_MCFR  : longword;            // *< \brief (Pmc Offset: 0x0024) Main Clock Frequency Register
    CKGR_PLLAR : longword;            // *< \brief (Pmc Offset: 0x0028) PLLA Register
    Reserved2  : array[0..0] of longword;
    MCKR       : longword;            // *< \brief (Pmc Offset: 0x0030) Master Clock Register
    Reserved3  : array[0..0] of longword;
    USB        : longword;            // *< \brief (Pmc Offset: 0x0038) USB Clock Register
    Reserved4  : array[0..0] of longword;
    PCK        : array[0..2] of longword; // *< \brief (Pmc Offset: 0x0040) Programmable Clock 0 Register
    Reserved5  : array[0..4] of longword;
    IER        : longword;            // *< \brief (Pmc Offset: 0x0060) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Pmc Offset: 0x0064) Interrupt Disable Register
    SR         : longword;            // *< \brief (Pmc Offset: 0x0068) Status Register
    IMR        : longword;            // *< \brief (Pmc Offset: 0x006C) Interrupt Mask Register
    FSMR       : longword;            // *< \brief (Pmc Offset: 0x0070) Fast Startup Mode Register
    FSPR       : longword;            // *< \brief (Pmc Offset: 0x0074) Fast Startup Polarity Register
    FOCR       : longword;            // *< \brief (Pmc Offset: 0x0078) Fault Output Clear Register
    Reserved6  : array[0..25] of longword;
    WPMR       : longword;            // *< \brief (Pmc Offset: 0x00E4) Write Protect Mode Register
    WPSR       : longword;            // *< \brief (Pmc Offset: 0x00E8) Write Protect Status Register
    Reserved7  : array[0..4] of longword;
    PCER1      : longword;            // *< \brief (Pmc Offset: 0x0100) Peripheral Clock Enable Register 1
    PCDR1      : longword;            // *< \brief (Pmc Offset: 0x0104) Peripheral Clock Disable Register 1
    PCSR1      : longword;            // *< \brief (Pmc Offset: 0x0108) Peripheral Clock Status Register 1
    PCR        : longword;            // *< \brief (Pmc Offset: 0x010C) Peripheral Control Register
  end;

  TPWMCH_NUM_Registers = record
    CMR        : longword;            // *< \brief (PwmCh_num Offset: 0x0) PWM Channel Mode Register
    CDTY       : longword;            // *< \brief (PwmCh_num Offset: 0x4) PWM Channel Duty Cycle Register
    CDTYUPD    : longword;           // *< \brief (PwmCh_num Offset: 0x8) PWM Channel Duty Cycle Update Register
    CPRD       : longword;            // *< \brief (PwmCh_num Offset: 0xC) PWM Channel Period Register
    CPRDUPD    : longword;           // *< \brief (PwmCh_num Offset: 0x10) PWM Channel Period Update Register
    CCNT       : longword;            // *< \brief (PwmCh_num Offset: 0x14) PWM Channel Counter Register
    DT         : longword;            // *< \brief (PwmCh_num Offset: 0x18) PWM Channel Dead Time Register
    DTUPD      : longword;            // *< \brief (PwmCh_num Offset: 0x1C) PWM Channel Dead Time Update Register
  end;

  TPWMCMP_Registers = record
    CMPV       : longword;            // *< \brief (PwmCmp Offset: 0x0) PWM Comparison 0 Value Register
    CMPVUPD    : longword;           // *< \brief (PwmCmp Offset: 0x4) PWM Comparison 0 Value Update Register
    CMPM       : longword;            // *< \brief (PwmCmp Offset: 0x8) PWM Comparison 0 Mode Register
    CMPMUPD    : longword;           // *< \brief (PwmCmp Offset: 0xC) PWM Comparison 0 Mode Update Register
  end;

  TPWM_Registers = record
    CLK        : longword;            // *< \brief (Pwm Offset: 0x00) PWM Clock Register
    ENA        : longword;            // *< \brief (Pwm Offset: 0x04) PWM Enable Register
    DIS        : longword;            // *< \brief (Pwm Offset: 0x08) PWM Disable Register
    SR         : longword;            // *< \brief (Pwm Offset: 0x0C) PWM Status Register
    IER1       : longword;            // *< \brief (Pwm Offset: 0x10) PWM Interrupt Enable Register 1
    IDR1       : longword;            // *< \brief (Pwm Offset: 0x14) PWM Interrupt Disable Register 1
    IMR1       : longword;            // *< \brief (Pwm Offset: 0x18) PWM Interrupt Mask Register 1
    ISR1       : longword;            // *< \brief (Pwm Offset: 0x1C) PWM Interrupt Status Register 1
    SCM        : longword;            // *< \brief (Pwm Offset: 0x20) PWM Sync Channels Mode Register
    Reserved1  : array[0..0] of longword;
    SCUC       : longword;            // *< \brief (Pwm Offset: 0x28) PWM Sync Channels Update Control Register
    SCUP       : longword;            // *< \brief (Pwm Offset: 0x2C) PWM Sync Channels Update Period Register
    SCUPUPD    : longword;           // *< \brief (Pwm Offset: 0x30) PWM Sync Channels Update Period Update Register
    IER2       : longword;            // *< \brief (Pwm Offset: 0x34) PWM Interrupt Enable Register 2
    IDR2       : longword;            // *< \brief (Pwm Offset: 0x38) PWM Interrupt Disable Register 2
    IMR2       : longword;            // *< \brief (Pwm Offset: 0x3C) PWM Interrupt Mask Register 2
    ISR2       : longword;            // *< \brief (Pwm Offset: 0x40) PWM Interrupt Status Register 2
    OOV        : longword;            // *< \brief (Pwm Offset: 0x44) PWM Output Override Value Register
    OS         : longword;            // *< \brief (Pwm Offset: 0x48) PWM Output Selection Register
    OSS        : longword;            // *< \brief (Pwm Offset: 0x4C) PWM Output Selection Set Register
    OSC        : longword;            // *< \brief (Pwm Offset: 0x50) PWM Output Selection Clear Register
    OSSUPD     : longword;            // *< \brief (Pwm Offset: 0x54) PWM Output Selection Set Update Register
    OSCUPD     : longword;            // *< \brief (Pwm Offset: 0x58) PWM Output Selection Clear Update Register
    FMR        : longword;            // *< \brief (Pwm Offset: 0x5C) PWM Fault Mode Register
    FSR        : longword;            // *< \brief (Pwm Offset: 0x60) PWM Fault Status Register
    FCR        : longword;            // *< \brief (Pwm Offset: 0x64) PWM Fault Clear Register
    FPV        : longword;            // *< \brief (Pwm Offset: 0x68) PWM Fault Protection Value Register
    FPE1       : longword;            // *< \brief (Pwm Offset: 0x6C) PWM Fault Protection Enable Register 1
    FPE2       : longword;            // *< \brief (Pwm Offset: 0x70) PWM Fault Protection Enable Register 2
    Reserved2  : array[0..1] of longword;
    ELMR       : array[0..1] of longword; // *< \brief (Pwm Offset: 0x7C) PWM Event Line 0 Mode Register
    Reserved3  : array[0..10] of longword;
    SMMR       : longword;            // *< \brief (Pwm Offset: 0xB0) PWM Stepper Motor Mode Register
    Reserved4  : array[0..11] of longword;
    WPCR       : longword;            // *< \brief (Pwm Offset: 0xE4) PWM Write Protect Control Register
    WPSR       : longword;            // *< \brief (Pwm Offset: 0xE8) PWM Write Protect Status Register
    Reserved5  : array[0..6] of longword;
    TPR        : longword;            // *< \brief (Pwm Offset: 0x108) Transmit Pointer Register
    TCR        : longword;            // *< \brief (Pwm Offset: 0x10C) Transmit Counter Register
    Reserved6  : array[0..1] of longword;
    TNPR       : longword;            // *< \brief (Pwm Offset: 0x118) Transmit Next Pointer Register
    TNCR       : longword;            // *< \brief (Pwm Offset: 0x11C) Transmit Next Counter Register
    PTCR       : longword;            // *< \brief (Pwm Offset: 0x120) Transfer Control Register
    PTSR       : longword;            // *< \brief (Pwm Offset: 0x124) Transfer Status Register
    Reserved7  : array[0..1] of longword;
    CMP        : array[0..7] of TPwmCmp_Registers; // *< \brief (Pwm Offset: 0x130) 0 .. 7
    Reserved8  : array[0..19] of longword;
    CH_NUM     : array[0..7] of TPwmCh_num_Registers; // *< \brief (Pwm Offset: 0x200) ch_num = 0 .. 7
  end;

  TRSTC_Registers = record
    CR         : longword;            // *< \brief (Rstc Offset: 0x00) Control Register
    SR         : longword;            // *< \brief (Rstc Offset: 0x04) Status Register
    MR         : longword;            // *< \brief (Rstc Offset: 0x08) Mode Register
  end;

  TRTC_Registers = record
    CR         : longword;            // *< \brief (Rtc Offset: 0x00) Control Register
    MR         : longword;            // *< \brief (Rtc Offset: 0x04) Mode Register
    TIMR       : longword;            // *< \brief (Rtc Offset: 0x08) Time Register
    CALR       : longword;            // *< \brief (Rtc Offset: 0x0C) Calendar Register
    TIMALR     : longword;            // *< \brief (Rtc Offset: 0x10) Time Alarm Register
    CALALR     : longword;            // *< \brief (Rtc Offset: 0x14) Calendar Alarm Register
    SR         : longword;            // *< \brief (Rtc Offset: 0x18) Status Register
    SCCR       : longword;            // *< \brief (Rtc Offset: 0x1C) Status Clear Command Register
    IER        : longword;            // *< \brief (Rtc Offset: 0x20) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Rtc Offset: 0x24) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Rtc Offset: 0x28) Interrupt Mask Register
    VER        : longword;            // *< \brief (Rtc Offset: 0x2C) Valid Entry Register
    Reserved1  : array[0..44] of longword;
    WPMR       : longword;            // *< \brief (Rtc Offset: 0xE4) Write Protect Mode Register
  end;

  TRTT_Registers = record
    MR         : longword;            // *< \brief (Rtt Offset: 0x00) Mode Register
    AR         : longword;            // *< \brief (Rtt Offset: 0x04) Alarm Register
    VR         : longword;            // *< \brief (Rtt Offset: 0x08) Value Register
    SR         : longword;            // *< \brief (Rtt Offset: 0x0C) Status Register
  end;

  TSMCCS_number_Registers = record
    SETUP      : longword;            // *< \brief (SmcCs_number Offset: 0x0) SMC Setup Register
    PULSE      : longword;            // *< \brief (SmcCs_number Offset: 0x4) SMC Pulse Register
    CYCLE      : longword;            // *< \brief (SmcCs_number Offset: 0x8) SMC Cycle Register
    TIMINGS    : longword;           // *< \brief (SmcCs_number Offset: 0xC) SMC Timings Register
    MODE       : longword;            // *< \brief (SmcCs_number Offset: 0x10) SMC Mode Register
  end;

  TSMC_Registers = record
    CFG        : longword;            // *< \brief (Smc Offset: 0x000) SMC NFC Configuration Register
    CTRL       : longword;            // *< \brief (Smc Offset: 0x004) SMC NFC Control Register
    SR         : longword;            // *< \brief (Smc Offset: 0x008) SMC NFC Status Register
    IER        : longword;            // *< \brief (Smc Offset: 0x00C) SMC NFC Interrupt Enable Register
    IDR        : longword;            // *< \brief (Smc Offset: 0x010) SMC NFC Interrupt Disable Register
    IMR        : longword;            // *< \brief (Smc Offset: 0x014) SMC NFC Interrupt Mask Register
    ADDR       : longword;            // *< \brief (Smc Offset: 0x018) SMC NFC Address Cycle Zero Register
    BANK       : longword;            // *< \brief (Smc Offset: 0x01C) SMC Bank Address Register
    ECC_CTRL   : longword;          // *< \brief (Smc Offset: 0x020) SMC ECC Control Register
    ECC_MD     : longword;            // *< \brief (Smc Offset: 0x024) SMC ECC Mode Register
    ECC_SR1    : longword;           // *< \brief (Smc Offset: 0x028) SMC ECC Status 1 Register
    ECC_PR0    : longword;           // *< \brief (Smc Offset: 0x02C) SMC ECC Parity 0 Register
    ECC_PR1    : longword;           // *< \brief (Smc Offset: 0x030) SMC ECC parity 1 Register
    ECC_SR2    : longword;           // *< \brief (Smc Offset: 0x034) SMC ECC status 2 Register
    ECC_PR2    : longword;           // *< \brief (Smc Offset: 0x038) SMC ECC parity 2 Register
    ECC_PR3    : longword;           // *< \brief (Smc Offset: 0x03C) SMC ECC parity 3 Register
    ECC_PR4    : longword;           // *< \brief (Smc Offset: 0x040) SMC ECC parity 4 Register
    ECC_PR5    : longword;           // *< \brief (Smc Offset: 0x044) SMC ECC parity 5 Register
    ECC_PR6    : longword;           // *< \brief (Smc Offset: 0x048) SMC ECC parity 6 Register
    ECC_PR7    : longword;           // *< \brief (Smc Offset: 0x04C) SMC ECC parity 7 Register
    ECC_PR8    : longword;           // *< \brief (Smc Offset: 0x050) SMC ECC parity 8 Register
    ECC_PR9    : longword;           // *< \brief (Smc Offset: 0x054) SMC ECC parity 9 Register
    ECC_PR10   : longword;          // *< \brief (Smc Offset: 0x058) SMC ECC parity 10 Register
    ECC_PR11   : longword;          // *< \brief (Smc Offset: 0x05C) SMC ECC parity 11 Register
    ECC_PR12   : longword;          // *< \brief (Smc Offset: 0x060) SMC ECC parity 12 Register
    ECC_PR13   : longword;          // *< \brief (Smc Offset: 0x064) SMC ECC parity 13 Register
    ECC_PR14   : longword;          // *< \brief (Smc Offset: 0x068) SMC ECC parity 14 Register
    ECC_PR15   : longword;          // *< \brief (Smc Offset: 0x06C) SMC ECC parity 15 Register
    CS_NUMBER  : array[0..7] of TSmcCs_number_Registers; // *< \brief (Smc Offset: 0x70) CS_number = 0 .. 7
    OCMS       : longword;            // *< \brief (Smc Offset: 0x110) SMC OCMS Register
    KEY1       : longword;            // *< \brief (Smc Offset: 0x114) SMC OCMS KEY1 Register
    KEY2       : longword;            // *< \brief (Smc Offset: 0x118) SMC OCMS KEY2 Register
    Reserved1  : array[0..49] of longword;
    WPCR       : longword;            // *< \brief (Smc Offset: 0x1E4) Write Protection Control Register
    WPSR       : longword;            // *< \brief (Smc Offset: 0x1E8) Write Protection Status Register
  end;

  TSPI_Registers = record
    CR         : longword;            // *< \brief (Spi Offset: 0x00) Control Register
    MR         : longword;            // *< \brief (Spi Offset: 0x04) Mode Register
    RDR        : longword;            // *< \brief (Spi Offset: 0x08) Receive Data Register
    TDR        : longword;            // *< \brief (Spi Offset: 0x0C) Transmit Data Register
    SR         : longword;            // *< \brief (Spi Offset: 0x10) Status Register
    IER        : longword;            // *< \brief (Spi Offset: 0x14) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Spi Offset: 0x18) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Spi Offset: 0x1C) Interrupt Mask Register
    Reserved1  : array[0..3] of longword;
    CSR        : array[0..3] of longword; // *< \brief (Spi Offset: 0x30) Chip Select Register
    Reserved2  : array[0..40] of longword;
    WPMR       : longword;            // *< \brief (Spi Offset: 0xE4) Write Protection Control Register
    WPSR       : longword;            // *< \brief (Spi Offset: 0xE8) Write Protection Status Register
  end;

  TSSC_Registers = record
    CR         : longword;            // *< \brief (Ssc Offset: 0x0) Control Register
    CMR        : longword;            // *< \brief (Ssc Offset: 0x4) Clock Mode Register
    Reserved1  : array[0..1] of longword;
    RCMR       : longword;            // *< \brief (Ssc Offset: 0x10) Receive Clock Mode Register
    RFMR       : longword;            // *< \brief (Ssc Offset: 0x14) Receive Frame Mode Register
    TCMR       : longword;            // *< \brief (Ssc Offset: 0x18) Transmit Clock Mode Register
    TFMR       : longword;            // *< \brief (Ssc Offset: 0x1C) Transmit Frame Mode Register
    RHR        : longword;            // *< \brief (Ssc Offset: 0x20) Receive Holding Register
    THR        : longword;            // *< \brief (Ssc Offset: 0x24) Transmit Holding Register
    Reserved2  : array[0..1] of longword;
    RSHR       : longword;            // *< \brief (Ssc Offset: 0x30) Receive Sync. Holding Register
    TSHR       : longword;            // *< \brief (Ssc Offset: 0x34) Transmit Sync. Holding Register
    RC0R       : longword;            // *< \brief (Ssc Offset: 0x38) Receive Compare 0 Register
    RC1R       : longword;            // *< \brief (Ssc Offset: 0x3C) Receive Compare 1 Register
    SR         : longword;            // *< \brief (Ssc Offset: 0x40) Status Register
    IER        : longword;            // *< \brief (Ssc Offset: 0x44) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Ssc Offset: 0x48) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Ssc Offset: 0x4C) Interrupt Mask Register
    Reserved3  : array[0..36] of longword;
    WPMR       : longword;            // *< \brief (Ssc Offset: 0xE4) Write Protect Mode Register
    WPSR       : longword;            // *< \brief (Ssc Offset: 0xE8) Write Protect Status Register
  end;

  TSUPC_Registers = record
    CR         : longword;            // *< \brief (Supc Offset: 0x00) Supply Controller Control Register
    SMMR       : longword;            // *< \brief (Supc Offset: 0x04) Supply Controller Supply Monitor Mode Register
    MR         : longword;            // *< \brief (Supc Offset: 0x08) Supply Controller Mode Register
    WUMR       : longword;            // *< \brief (Supc Offset: 0x0C) Supply Controller Wake Up Mode Register
    WUIR       : longword;            // *< \brief (Supc Offset: 0x10) Supply Controller Wake Up Inputs Register
    SR         : longword;            // *< \brief (Supc Offset: 0x14) Supply Controller Status Register
  end;

  TTCCHANNEL_Registers = record
    CCR        : longword;            // *< \brief (TcChannel Offset: 0x0) Channel Control Register
    CMR        : longword;            // *< \brief (TcChannel Offset: 0x4) Channel Mode Register
    SMMR       : longword;            // *< \brief (TcChannel Offset: 0x8) Stepper Motor Mode Register
    Reserved1  : array[0..0] of longword;
    CV         : longword;            // *< \brief (TcChannel Offset: 0x10) Counter Value
    RA         : longword;            // *< \brief (TcChannel Offset: 0x14) Register A
    RB         : longword;            // *< \brief (TcChannel Offset: 0x18) Register B
    RC         : longword;            // *< \brief (TcChannel Offset: 0x1C) Register C
    SR         : longword;            // *< \brief (TcChannel Offset: 0x20) Status Register
    IER        : longword;            // *< \brief (TcChannel Offset: 0x24) Interrupt Enable Register
    IDR        : longword;            // *< \brief (TcChannel Offset: 0x28) Interrupt Disable Register
    IMR        : longword;            // *< \brief (TcChannel Offset: 0x2C) Interrupt Mask Register
    Reserved2  : array[0..3] of longword;
  end;

  TTC_Registers = record
    CHANNEL    : array[0..2] of TTcChannel_Registers; // *< \brief (Tc Offset: 0x0) channel = 0 .. 2
    BCR        : longword;            // *< \brief (Tc Offset: 0xC0) Block Control Register
    BMR        : longword;            // *< \brief (Tc Offset: 0xC4) Block Mode Register
    QIER       : longword;            // *< \brief (Tc Offset: 0xC8) QDEC Interrupt Enable Register
    QIDR       : longword;            // *< \brief (Tc Offset: 0xCC) QDEC Interrupt Disable Register
    QIMR       : longword;            // *< \brief (Tc Offset: 0xD0) QDEC Interrupt Mask Register
    QISR       : longword;            // *< \brief (Tc Offset: 0xD4) QDEC Interrupt Status Register
    FMR        : longword;            // *< \brief (Tc Offset: 0xD8) Fault Mode Register
    Reserved1  : array[0..1] of longword;
    WPMR       : longword;            // *< \brief (Tc Offset: 0xE4) Write Protect Mode Register
  end;

  TTRNG_Registers = record
    CR         : longword;            // *< \brief (Trng Offset: 0x00) Control Register
    Reserved1  : array[0..2] of longword;
    IER        : longword;            // *< \brief (Trng Offset: 0x10) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Trng Offset: 0x14) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Trng Offset: 0x18) Interrupt Mask Register
    ISR        : longword;            // *< \brief (Trng Offset: 0x1C) Interrupt Status Register
    Reserved2  : array[0..11] of longword;
    ODATA      : longword;            // *< \brief (Trng Offset: 0x50) Output Data Register
  end;

  TTWI_Registers = record
    CR         : longword;            // *< \brief (Twi Offset: 0x00) Control Register
    MMR        : longword;            // *< \brief (Twi Offset: 0x04) Master Mode Register
    SMR        : longword;            // *< \brief (Twi Offset: 0x08) Slave Mode Register
    IADR       : longword;            // *< \brief (Twi Offset: 0x0C) Internal Address Register
    CWGR       : longword;            // *< \brief (Twi Offset: 0x10) Clock Waveform Generator Register
    Reserved1  : array[0..2] of longword;
    SR         : longword;            // *< \brief (Twi Offset: 0x20) Status Register
    IER        : longword;            // *< \brief (Twi Offset: 0x24) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Twi Offset: 0x28) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Twi Offset: 0x2C) Interrupt Mask Register
    RHR        : longword;            // *< \brief (Twi Offset: 0x30) Receive Holding Register
    THR        : longword;            // *< \brief (Twi Offset: 0x34) Transmit Holding Register
    Reserved2  : array[0..49] of longword;
    RPR        : longword;            // *< \brief (Twi Offset: 0x100) Receive Pointer Register
    RCR        : longword;            // *< \brief (Twi Offset: 0x104) Receive Counter Register
    TPR        : longword;            // *< \brief (Twi Offset: 0x108) Transmit Pointer Register
    TCR        : longword;            // *< \brief (Twi Offset: 0x10C) Transmit Counter Register
    RNPR       : longword;            // *< \brief (Twi Offset: 0x110) Receive Next Pointer Register
    RNCR       : longword;            // *< \brief (Twi Offset: 0x114) Receive Next Counter Register
    TNPR       : longword;            // *< \brief (Twi Offset: 0x118) Transmit Next Pointer Register
    TNCR       : longword;            // *< \brief (Twi Offset: 0x11C) Transmit Next Counter Register
    PTCR       : longword;            // *< \brief (Twi Offset: 0x120) Transfer Control Register
    PTSR       : longword;            // *< \brief (Twi Offset: 0x124) Transfer Status Register
  end;

  TUART_Registers = record
    CR         : longword;            // *< \brief (Uart Offset: 0x0000) Control Register
    MR         : longword;            // *< \brief (Uart Offset: 0x0004) Mode Register
    IER        : longword;            // *< \brief (Uart Offset: 0x0008) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Uart Offset: 0x000C) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Uart Offset: 0x0010) Interrupt Mask Register
    SR         : longword;            // *< \brief (Uart Offset: 0x0014) Status Register
    RHR        : longword;            // *< \brief (Uart Offset: 0x0018) Receive Holding Register
    THR        : longword;            // *< \brief (Uart Offset: 0x001C) Transmit Holding Register
    BRGR       : longword;            // *< \brief (Uart Offset: 0x0020) Baud Rate Generator Register
    Reserved1  : array[0..54] of longword;
    RPR        : longword;            // *< \brief (Uart Offset: 0x100) Receive Pointer Register
    RCR        : longword;            // *< \brief (Uart Offset: 0x104) Receive Counter Register
    TPR        : longword;            // *< \brief (Uart Offset: 0x108) Transmit Pointer Register
    TCR        : longword;            // *< \brief (Uart Offset: 0x10C) Transmit Counter Register
    RNPR       : longword;            // *< \brief (Uart Offset: 0x110) Receive Next Pointer Register
    RNCR       : longword;            // *< \brief (Uart Offset: 0x114) Receive Next Counter Register
    TNPR       : longword;            // *< \brief (Uart Offset: 0x118) Transmit Next Pointer Register
    TNCR       : longword;            // *< \brief (Uart Offset: 0x11C) Transmit Next Counter Register
    PTCR       : longword;            // *< \brief (Uart Offset: 0x120) Transfer Control Register
    PTSR       : longword;            // *< \brief (Uart Offset: 0x124) Transfer Status Register
  end;

  TUOTGHSDEVDMA_Registers = record
    DEVDMANXTDSC : longword;   // *< \brief (UotghsDevdma Offset: 0x0) Device DMA Channel Next Descriptor Address Register
    DEVDMAADDRESS : longword;  // *< \brief (UotghsDevdma Offset: 0x4) Device DMA Channel Address Register
    DEVDMACONTROL : longword;  // *< \brief (UotghsDevdma Offset: 0x8) Device DMA Channel Control Register
    DEVDMASTATUS : longword;   // *< \brief (UotghsDevdma Offset: 0xC) Device DMA Channel Status Register
  end;

  TUOTGHSHSTDMA_Registers = record
    HSTDMANXTDSC : longword;   // *< \brief (UotghsHstdma Offset: 0x0) Host DMA Channel Next Descriptor Address Register
    HSTDMAADDRESS : longword;  // *< \brief (UotghsHstdma Offset: 0x4) Host DMA Channel Address Register
    HSTDMACONTROL : longword;  // *< \brief (UotghsHstdma Offset: 0x8) Host DMA Channel Control Register
    HSTDMASTATUS : longword;   // *< \brief (UotghsHstdma Offset: 0xC) Host DMA Channel Status Register
  end;

  TUOTGHS_Registers = record
    DEVCTRL    : longword;        // *< \brief (Uotghs Offset: 0x0000) Device General Control Register
    DEVISR     : longword;         // *< \brief (Uotghs Offset: 0x0004) Device Global Interrupt Status Register
    DEVICR     : longword;         // *< \brief (Uotghs Offset: 0x0008) Device Global Interrupt Clear Register
    DEVIFR     : longword;         // *< \brief (Uotghs Offset: 0x000C) Device Global Interrupt Set Register
    DEVIMR     : longword;         // *< \brief (Uotghs Offset: 0x0010) Device Global Interrupt Mask Register
    DEVIDR     : longword;         // *< \brief (Uotghs Offset: 0x0014) Device Global Interrupt Disable Register
    DEVIER     : longword;         // *< \brief (Uotghs Offset: 0x0018) Device Global Interrupt Enable Register
    DEVEPT     : longword;         // *< \brief (Uotghs Offset: 0x001C) Device Endpoint Register
    DEVFNUM    : longword;        // *< \brief (Uotghs Offset: 0x0020) Device Frame Number Register
    Reserved1  : array[0..54] of longword;
    DEVEPTCFG  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x100) Device Endpoint Configuration Register (n = 0)
    Reserved2  : array[0..1] of longword;
    DEVEPTISR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x130) Device Endpoint Status Register (n = 0)
    Reserved3  : array[0..1] of longword;
    DEVEPTICR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x160) Device Endpoint Clear Register (n = 0)
    Reserved4  : array[0..1] of longword;
    DEVEPTIFR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x190) Device Endpoint Set Register (n = 0)
    Reserved5  : array[0..1] of longword;
    DEVEPTIMR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x1C0) Device Endpoint Mask Register (n = 0)
    Reserved6  : array[0..1] of longword;
    DEVEPTIER  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x1F0) Device Endpoint Enable Register (n = 0)
    Reserved7  : array[0..1] of longword;
    DEVEPTIDR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x220) Device Endpoint Disable Register (n = 0)
    Reserved8  : array[0..49] of longword;
    DEVDMA     : array[0..6] of TUotghsDevdma_Registers; // *< \brief (Uotghs Offset: 0x310) n = 1 .. 7
    Reserved9  : array[0..31] of longword;
    HSTCTRL    : longword;        // *< \brief (Uotghs Offset: 0x0400) Host General Control Register
    HSTISR     : longword;         // *< \brief (Uotghs Offset: 0x0404) Host Global Interrupt Status Register
    HSTICR     : longword;         // *< \brief (Uotghs Offset: 0x0408) Host Global Interrupt Clear Register
    HSTIFR     : longword;         // *< \brief (Uotghs Offset: 0x040C) Host Global Interrupt Set Register
    HSTIMR     : longword;         // *< \brief (Uotghs Offset: 0x0410) Host Global Interrupt Mask Register
    HSTIDR     : longword;         // *< \brief (Uotghs Offset: 0x0414) Host Global Interrupt Disable Register
    HSTIER     : longword;         // *< \brief (Uotghs Offset: 0x0418) Host Global Interrupt Enable Register
    HSTPIP     : longword;         // *< \brief (Uotghs Offset: 0x0041C) Host Pipe Register
    HSTFNUM    : longword;        // *< \brief (Uotghs Offset: 0x0420) Host Frame Number Register
    HSTADDR1   : longword;       // *< \brief (Uotghs Offset: 0x0424) Host Address 1 Register
    HSTADDR2   : longword;       // *< \brief (Uotghs Offset: 0x0428) Host Address 2 Register
    HSTADDR3   : longword;       // *< \brief (Uotghs Offset: 0x042C) Host Address 3 Register
    Reserved10 : array[0..51] of longword;
    HSTPIPCFG  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x500) Host Pipe Configuration Register (n = 0)
    Reserved11 : array[0..1] of longword;
    HSTPIPISR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x530) Host Pipe Status Register (n = 0)
    Reserved12 : array[0..1] of longword;
    HSTPIPICR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x560) Host Pipe Clear Register (n = 0)
    Reserved13 : array[0..1] of longword;
    HSTPIPIFR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x590) Host Pipe Set Register (n = 0)
    Reserved14 : array[0..1] of longword;
    HSTPIPIMR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x5C0) Host Pipe Mask Register (n = 0)
    Reserved15 : array[0..1] of longword;
    HSTPIPIER  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x5F0) Host Pipe Enable Register (n = 0)
    Reserved16 : array[0..1] of longword;
    HSTPIPIDR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x620) Host Pipe Disable Register (n = 0)
    Reserved17 : array[0..1] of longword;
    HSTPIPINRQ : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x650) Host Pipe IN Request Register (n = 0)
    Reserved18 : array[0..1] of longword;
    HSTPIPERR  : array[0..9] of longword; // *< \brief (Uotghs Offset: 0x680) Host Pipe Error Register (n = 0)
    Reserved19 : array[0..25] of longword;
    HSTDMA     : array[0..6] of TUotghsHstdma_Registers; // *< \brief (Uotghs Offset: 0x710) n = 1 .. 7
    Reserved20 : array[0..31] of longword;
    CTRL       : longword;           // *< \brief (Uotghs Offset: 0x0800) General Control Register
    SR         : longword;            // *< \brief (Uotghs Offset: 0x0804) General Status Register
    SCR        : longword;            // *< \brief (Uotghs Offset: 0x0808) General Status Clear Register
    SFR        : longword;            // *< \brief (Uotghs Offset: 0x080C) General Status Set Register
    Reserved21 : array[0..6] of longword;
    FSM        : longword;            // *< \brief (Uotghs Offset: 0x082C) General Finite State Machine Register
  end;

  TUSART_Registers = record
    CR         : longword;            // *< \brief (Usart Offset: 0x0000) Control Register
    MR         : longword;            // *< \brief (Usart Offset: 0x0004) Mode Register
    IER        : longword;            // *< \brief (Usart Offset: 0x0008) Interrupt Enable Register
    IDR        : longword;            // *< \brief (Usart Offset: 0x000C) Interrupt Disable Register
    IMR        : longword;            // *< \brief (Usart Offset: 0x0010) Interrupt Mask Register
    CSR        : longword;            // *< \brief (Usart Offset: 0x0014) Channel Status Register
    RHR        : longword;            // *< \brief (Usart Offset: 0x0018) Receiver Holding Register
    THR        : longword;            // *< \brief (Usart Offset: 0x001C) Transmitter Holding Register
    BRGR       : longword;            // *< \brief (Usart Offset: 0x0020) Baud Rate Generator Register
    RTOR       : longword;            // *< \brief (Usart Offset: 0x0024) Receiver Time-out Register
    TTGR       : longword;            // *< \brief (Usart Offset: 0x0028) Transmitter Timeguard Register
    Reserved1  : array[0..4] of longword;
    FIDI       : longword;            // *< \brief (Usart Offset: 0x0040) FI DI Ratio Register
    NER        : longword;            // *< \brief (Usart Offset: 0x0044) Number of Errors Register
    Reserved2  : array[0..0] of longword;
    &IF         : longword;            // *< \brief (Usart Offset: 0x004C) IrDA Filter Register
    MAN        : longword;            // *< \brief (Usart Offset: 0x0050) Manchester Encoder Decoder Register
    LINMR      : longword;            // *< \brief (Usart Offset: 0x0054) LIN Mode Register
    LINIR      : longword;            // *< \brief (Usart Offset: 0x0058) LIN Identifier Register
    Reserved3  : array[0..33] of longword;
    WPMR       : longword;            // *< \brief (Usart Offset: 0xE4) Write Protect Mode Register
    WPSR       : longword;            // *< \brief (Usart Offset: 0xE8) Write Protect Status Register
    Reserved4  : array[0..4] of longword;
    RPR        : longword;            // *< \brief (Usart Offset: 0x100) Receive Pointer Register
    RCR        : longword;            // *< \brief (Usart Offset: 0x104) Receive Counter Register
    TPR        : longword;            // *< \brief (Usart Offset: 0x108) Transmit Pointer Register
    TCR        : longword;            // *< \brief (Usart Offset: 0x10C) Transmit Counter Register
    RNPR       : longword;            // *< \brief (Usart Offset: 0x110) Receive Next Pointer Register
    RNCR       : longword;            // *< \brief (Usart Offset: 0x114) Receive Next Counter Register
    TNPR       : longword;            // *< \brief (Usart Offset: 0x118) Transmit Next Pointer Register
    TNCR       : longword;            // *< \brief (Usart Offset: 0x11C) Transmit Next Counter Register
    PTCR       : longword;            // *< \brief (Usart Offset: 0x120) Transfer Control Register
    PTSR       : longword;            // *< \brief (Usart Offset: 0x124) Transfer Status Register
  end;

  TWDT_Registers = record
    CR         : longword;            // *< \brief (Wdt Offset: 0x00) Control Register
    MR         : longword;            // *< \brief (Wdt Offset: 0x04) Mode Register
    SR         : longword;            // *< \brief (Wdt Offset: 0x08) Status Register
  end;

var
  HSMCI        : THsmci_Registers absolute $40000000; // *< \brief (HSMCI     ) Base Address
  SSC          : TSsc_Registers absolute $40004000; // *< \brief (SSC       ) Base Address
  SPI0         : TSpi_Registers absolute $40008000; // *< \brief (SPI0      ) Base Address
  TC0          : TTc_Registers absolute $40080000; // *< \brief (TC0       ) Base Address
  TC1          : TTc_Registers absolute $40084000; // *< \brief (TC1       ) Base Address
  TC2          : TTc_Registers absolute $40088000; // *< \brief (TC2       ) Base Address
  TWI0         : TTwi_Registers absolute $4008C000; // *< \brief (TWI0      ) Base Address
  PDC_TWI0     : TPdc_Registers absolute $4008C100; // *< \brief (PDC_TWI0  ) Base Address
  TWI1         : TTwi_Registers absolute $40090000; // *< \brief (TWI1      ) Base Address
  PDC_TWI1     : TPdc_Registers absolute $40090100; // *< \brief (PDC_TWI1  ) Base Address
  PWM          : TPwm_Registers absolute $40094000; // *< \brief (PWM       ) Base Address
  PDC_PWM      : TPdc_Registers absolute $40094100; // *< \brief (PDC_PWM   ) Base Address
  USART0       : TUsart_Registers absolute $40098000; // *< \brief (USART0    ) Base Address
  PDC_USART0   : TPdc_Registers absolute $40098100; // *< \brief (PDC_USART0) Base Address
  USART1       : TUsart_Registers absolute $4009C000; // *< \brief (USART1    ) Base Address
  PDC_USART1   : TPdc_Registers absolute $4009C100; // *< \brief (PDC_USART1) Base Address
  USART2       : TUsart_Registers absolute $400A0000; // *< \brief (USART2    ) Base Address
  PDC_USART2   : TPdc_Registers absolute $400A0100; // *< \brief (PDC_USART2) Base Address
  USART3       : TUsart_Registers absolute $400A4000; // *< \brief (USART3    ) Base Address
  PDC_USART3   : TPdc_Registers absolute $400A4100; // *< \brief (PDC_USART3) Base Address
  UOTGHS       : TUotghs_Registers absolute $400AC000; // *< \brief (UOTGHS    ) Base Address
  EMAC         : TEmac_Registers absolute $400B0000; // *< \brief (EMAC      ) Base Address
  CAN0         : TCan_Registers absolute $400B4000; // *< \brief (CAN0      ) Base Address
  CAN1         : TCan_Registers absolute $400B8000; // *< \brief (CAN1      ) Base Address
  TRNG         : TTrng_Registers absolute $400BC000; // *< \brief (TRNG      ) Base Address
  ADC          : TAdc_Registers absolute $400C0000; // *< \brief (ADC       ) Base Address
  PDC_ADC      : TPdc_Registers absolute $400C0100; // *< \brief (PDC_ADC   ) Base Address
  DMAC         : TDmac_Registers absolute $400C4000; // *< \brief (DMAC      ) Base Address
  DACC         : TDacc_Registers absolute $400C8000; // *< \brief (DACC      ) Base Address
  PDC_DACC     : TPdc_Registers absolute $400C8100; // *< \brief (PDC_DACC  ) Base Address
  SMC          : TSmc_Registers absolute $400E0000; // *< \brief (SMC       ) Base Address
  MATRIX       : TMatrix_Registers absolute $400E0400; // *< \brief (MATRIX    ) Base Address
  PMC          : TPmc_Registers absolute $400E0600; // *< \brief (PMC       ) Base Address
  UART         : TUart_Registers absolute $400E0800; // *< \brief (UART      ) Base Address
  PDC_UART     : TPdc_Registers absolute $400E0900; // *< \brief (PDC_UART  ) Base Address
  CHIPID       : TChipid_Registers absolute $400E0940; // *< \brief (CHIPID    ) Base Address
  EFC0         : TEfc_Registers absolute $400E0A00; // *< \brief (EFC0      ) Base Address
  EFC1         : TEfc_Registers absolute $400E0C00; // *< \brief (EFC1      ) Base Address
  PIOA         : TPio_Registers absolute $400E0E00; // *< \brief (PIOA      ) Base Address
  PIOB         : TPio_Registers absolute $400E1000; // *< \brief (PIOB      ) Base Address
  PIOC         : TPio_Registers absolute $400E1200; // *< \brief (PIOC      ) Base Address
  PIOD         : TPio_Registers absolute $400E1400; // *< \brief (PIOD      ) Base Address
  RSTC         : TRstc_Registers absolute $400E1A00; // *< \brief (RSTC      ) Base Address
  SUPC         : TSupc_Registers absolute $400E1A10; // *< \brief (SUPC      ) Base Address
  RTT          : TRtt_Registers absolute $400E1A30; // *< \brief (RTT       ) Base Address
  WDT          : TWdt_Registers absolute $400E1A50; // *< \brief (WDT       ) Base Address
  RTC          : TRtc_Registers absolute $400E1A60; // *< \brief (RTC       ) Base Address
  GPBR         : TGpbr_Registers absolute $400E1A90; // *< \brief (GPBR      ) Base Address

implementation

  procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
  procedure MemoryManagement_interrupt; external name 'MemoryManagement_interrupt';
  procedure BusFault_interrupt; external name 'BusFault_interrupt';
  procedure UsageFault_interrupt; external name 'UsageFault_interrupt';
  procedure SVCall_interrupt; external name 'SVCall_interrupt';
  procedure DebugMonitor_interrupt; external name 'DebugMonitor_interrupt';
  procedure PendSV_interrupt; external name 'PendSV_interrupt';
  procedure SysTick_interrupt; external name 'SysTick_interrupt';
  procedure SUPC_interrupt; external name 'SUPC_interrupt';
  procedure RSTC_interrupt; external name 'RSTC_interrupt';
  procedure RTC_interrupt; external name 'RTC_interrupt';
  procedure RTT_interrupt; external name 'RTT_interrupt';
  procedure WDT_interrupt; external name 'WDT_interrupt';
  procedure PMC_interrupt; external name 'PMC_interrupt';
  procedure EFC0_interrupt; external name 'EFC0_interrupt';
  procedure EFC1_interrupt; external name 'EFC1_interrupt';
  procedure UART_interrupt; external name 'UART_interrupt';
  procedure SMC_interrupt; external name 'SMC_interrupt';
  procedure PIOA_interrupt; external name 'PIOA_interrupt';
  procedure PIOB_interrupt; external name 'PIOB_interrupt';
  procedure PIOC_interrupt; external name 'PIOC_interrupt';
  procedure PIOD_interrupt; external name 'PIOD_interrupt';
  procedure USART0_interrupt; external name 'USART0_interrupt';
  procedure USART1_interrupt; external name 'USART1_interrupt';
  procedure USART2_interrupt; external name 'USART2_interrupt';
  procedure USART3_interrupt; external name 'USART3_interrupt';
  procedure HSMCI_interrupt; external name 'HSMCI_interrupt';
  procedure TWI0_interrupt; external name 'TWI0_interrupt';
  procedure TWI1_interrupt; external name 'TWI1_interrupt';
  procedure SPI0_interrupt; external name 'SPI0_interrupt';
  procedure SSC_interrupt; external name 'SSC_interrupt';
  procedure TC0_interrupt; external name 'TC0_interrupt';
  procedure TC1_interrupt; external name 'TC1_interrupt';
  procedure TC2_interrupt; external name 'TC2_interrupt';
  procedure TC3_interrupt; external name 'TC3_interrupt';
  procedure TC4_interrupt; external name 'TC4_interrupt';
  procedure TC5_interrupt; external name 'TC5_interrupt';
  procedure TC6_interrupt; external name 'TC6_interrupt';
  procedure TC7_interrupt; external name 'TC7_interrupt';
  procedure TC8_interrupt; external name 'TC8_interrupt';
  procedure PWM_interrupt; external name 'PWM_interrupt';
  procedure ADC_interrupt; external name 'ADC_interrupt';
  procedure DACC_interrupt; external name 'DACC_interrupt';
  procedure DMAC_interrupt; external name 'DMAC_interrupt';
  procedure UOTGHS_interrupt; external name 'UOTGHS_interrupt';
  procedure TRNG_interrupt; external name 'TRNG_interrupt';
  procedure EMAC_interrupt; external name 'EMAC_interrupt';
  procedure CAN0_interrupt; external name 'CAN0_interrupt';
  procedure CAN1_interrupt; external name 'CAN1_interrupt';
  procedure PERIPH_COUNT_interrupt; external name 'PERIPH_COUNT_interrupt';

  {$i cortexm3_start.inc}

  procedure Vectors; assembler; nostackframe;
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
    .long 0
    .long 0
    .long 0
    .long 0
    .long SVCall_interrupt
    .long DebugMonitor_interrupt
    .long 0
    .long PendSV_interrupt
    .long SysTick_interrupt
    .long SUPC_interrupt
    .long RSTC_interrupt
    .long RTC_interrupt
    .long RTT_interrupt
    .long WDT_interrupt
    .long PMC_interrupt
    .long EFC0_interrupt
    .long EFC1_interrupt
    .long UART_interrupt
    .long SMC_interrupt
    .long 0
    .long PIOA_interrupt
    .long PIOB_interrupt
    .long PIOC_interrupt
    .long PIOD_interrupt
    .long 0
    .long 0
    .long USART0_interrupt
    .long USART1_interrupt
    .long USART2_interrupt
    .long USART3_interrupt
    .long HSMCI_interrupt
    .long TWI0_interrupt
    .long TWI1_interrupt
    .long SPI0_interrupt
    .long 0
    .long SSC_interrupt
    .long TC0_interrupt
    .long TC1_interrupt
    .long TC2_interrupt
    .long TC3_interrupt
    .long TC4_interrupt
    .long TC5_interrupt
    .long TC6_interrupt
    .long TC7_interrupt
    .long TC8_interrupt
    .long PWM_interrupt
    .long ADC_interrupt
    .long DACC_interrupt
    .long DMAC_interrupt
    .long UOTGHS_interrupt
    .long TRNG_interrupt
    .long EMAC_interrupt
    .long CAN0_interrupt
    .long CAN1_interrupt
    .long PERIPH_COUNT_interrupt

    .weak NonMaskableInt_interrupt
    .weak MemoryManagement_interrupt
    .weak BusFault_interrupt
    .weak UsageFault_interrupt
    .weak SVCall_interrupt
    .weak DebugMonitor_interrupt
    .weak PendSV_interrupt
    .weak SysTick_interrupt
    .weak SUPC_interrupt
    .weak RSTC_interrupt
    .weak RTC_interrupt
    .weak RTT_interrupt
    .weak WDT_interrupt
    .weak PMC_interrupt
    .weak EFC0_interrupt
    .weak EFC1_interrupt
    .weak UART_interrupt
    .weak SMC_interrupt
    .weak PIOA_interrupt
    .weak PIOB_interrupt
    .weak PIOC_interrupt
    .weak PIOD_interrupt
    .weak USART0_interrupt
    .weak USART1_interrupt
    .weak USART2_interrupt
    .weak USART3_interrupt
    .weak HSMCI_interrupt
    .weak TWI0_interrupt
    .weak TWI1_interrupt
    .weak SPI0_interrupt
    .weak SSC_interrupt
    .weak TC0_interrupt
    .weak TC1_interrupt
    .weak TC2_interrupt
    .weak TC3_interrupt
    .weak TC4_interrupt
    .weak TC5_interrupt
    .weak TC6_interrupt
    .weak TC7_interrupt
    .weak TC8_interrupt
    .weak PWM_interrupt
    .weak ADC_interrupt
    .weak DACC_interrupt
    .weak DMAC_interrupt
    .weak UOTGHS_interrupt
    .weak TRNG_interrupt
    .weak EMAC_interrupt
    .weak CAN0_interrupt
    .weak CAN1_interrupt
    .weak PERIPH_COUNT_interrupt

    .set NonMaskableInt_interrupt, HaltProc
    .set MemoryManagement_interrupt, HaltProc
    .set BusFault_interrupt, HaltProc
    .set UsageFault_interrupt, HaltProc
    .set SVCall_interrupt, HaltProc
    .set DebugMonitor_interrupt, HaltProc
    .set PendSV_interrupt, HaltProc
    .set SysTick_interrupt, HaltProc
    .set SUPC_interrupt, HaltProc
    .set RSTC_interrupt, HaltProc
    .set RTC_interrupt, HaltProc
    .set RTT_interrupt, HaltProc
    .set WDT_interrupt, HaltProc
    .set PMC_interrupt, HaltProc
    .set EFC0_interrupt, HaltProc
    .set EFC1_interrupt, HaltProc
    .set UART_interrupt, HaltProc
    .set SMC_interrupt, HaltProc
    .set PIOA_interrupt, HaltProc
    .set PIOB_interrupt, HaltProc
    .set PIOC_interrupt, HaltProc
    .set PIOD_interrupt, HaltProc
    .set USART0_interrupt, HaltProc
    .set USART1_interrupt, HaltProc
    .set USART2_interrupt, HaltProc
    .set USART3_interrupt, HaltProc
    .set HSMCI_interrupt, HaltProc
    .set TWI0_interrupt, HaltProc
    .set TWI1_interrupt, HaltProc
    .set SPI0_interrupt, HaltProc
    .set SSC_interrupt, HaltProc
    .set TC0_interrupt, HaltProc
    .set TC1_interrupt, HaltProc
    .set TC2_interrupt, HaltProc
    .set TC3_interrupt, HaltProc
    .set TC4_interrupt, HaltProc
    .set TC5_interrupt, HaltProc
    .set TC6_interrupt, HaltProc
    .set TC7_interrupt, HaltProc
    .set TC8_interrupt, HaltProc
    .set PWM_interrupt, HaltProc
    .set ADC_interrupt, HaltProc
    .set DACC_interrupt, HaltProc
    .set DMAC_interrupt, HaltProc
    .set UOTGHS_interrupt, HaltProc
    .set TRNG_interrupt, HaltProc
    .set EMAC_interrupt, HaltProc
    .set CAN0_interrupt, HaltProc
    .set CAN1_interrupt, HaltProc
    .set PERIPH_COUNT_interrupt, HaltProc
    .text
  end;
end.
