unit nrf52;
interface
{$PACKRECORDS 2}
{$GOTO ON}
{$MODESWITCH ADVANCEDRECORDS}
// *****************************************************************************************************
// * @file     nrf52.h
//    CMSIS Cortex-M4 Peripheral Access Layer Header File for
// *           nrf52 from Nordic Semiconductor.
// *
// * @version  V1
// * @date     18. November 2016
// *
// * @note     Generated with SVDConv V2.81d
// *           from CMSIS SVD File 'nrf52.svd' Version 1,
// *
// * @par      Copyright (c) 2016, Nordic Semiconductor ASA
// *           All rights reserved.
// *
// *           Redistribution and use in source and binary forms, with or without
// *           modification, are permitted provided that the following conditions are met:
// *
// *           * Redistributions of source code must retain the above copyright notice, this
// *           list of conditions and the following disclaimer.
// *
// *           * Redistributions in binary form must reproduce the above copyright notice,
// *           this list of conditions and the following disclaimer in the documentation
// *           and/or other materials provided with the distribution.
// *
// *           * Neither the name of Nordic Semiconductor ASA nor the names of its
// *           contributors may be used to endorse or promote products derived from
// *           this software without specific prior written permission.
// *
// *           THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// *           AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// *           IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// *           DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// *           FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// *           DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// *           SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// *           CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// *           OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// *           OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// *
// *
// ******************************************************************************************************
// -------------------------  Interrupt Number Definition  ------------------------

type
  TIRQn_Enum   = (
    Reset_IRQn = -15,                 //   1  Reset Vector, invoked on Power up and warm reset
    NonMaskableInt_IRQn = -14,        //   2  Non maskable Interrupt, cannot be stopped or preempted
    HardFault_IRQn = -13,             //   3  Hard Fault, all classes of Fault
    MemoryManagement_IRQn = -12,      //   4  Memory Management, MPU mismatch, including Access Violation
    BusFault_IRQn = -11,              //   5  Bus Fault, Pre-Fetch-, Memory Access Fault, other address/memory
    UsageFault_IRQn = -10,            //   6  Usage Fault, i.e. Undef Instruction, Illegal State Transition
    SVCall_IRQn = -5,                 //  11  System Service Call via SVC instruction
    DebugMonitor_IRQn = -4,           //  12  Debug Monitor
    PendSV_IRQn = -2,                 //  14  Pendable request for system service
    SysTick_IRQn = -1,                //  15  System Tick Timer
    POWER_CLOCK_IRQn = 0,             //   0  POWER_CLOCK
    RADIO_IRQn = 1,                   //   1  RADIO
    UARTE0_UART0_IRQn = 2,            //   2  UARTE0_UART0
SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0_IRQn=   3, //   3  SPIM0_SPIS0_TWIM0_TWIS0_SPI0_TWI0
SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1_IRQn=   4, //   4  SPIM1_SPIS1_TWIM1_TWIS1_SPI1_TWI1
    NFCT_IRQn  = 5,                   //   5  NFCT
    GPIOTE_IRQn = 6,                  //   6  GPIOTE
    SAADC_IRQn = 7,                   //   7  SAADC
    TIMER0_IRQn = 8,                  //   8  TIMER0
    TIMER1_IRQn = 9,                  //   9  TIMER1
    TIMER2_IRQn = 10,                 //  10  TIMER2
    RTC0_IRQn  = 11,                  //  11  RTC0
    TEMP_IRQn  = 12,                  //  12  TEMP
    RNG_IRQn   = 13,                  //  13  RNG
    ECB_IRQn   = 14,                  //  14  ECB
    CCM_AAR_IRQn = 15,                //  15  CCM_AAR
    WDT_IRQn   = 16,                  //  16  WDT
    RTC1_IRQn  = 17,                  //  17  RTC1
    QDEC_IRQn  = 18,                  //  18  QDEC
    COMP_LPCOMP_IRQn = 19,            //  19  COMP_LPCOMP
    SWI0_EGU0_IRQn = 20,              //  20  SWI0_EGU0
    SWI1_EGU1_IRQn = 21,              //  21  SWI1_EGU1
    SWI2_EGU2_IRQn = 22,              //  22  SWI2_EGU2
    SWI3_EGU3_IRQn = 23,              //  23  SWI3_EGU3
    SWI4_EGU4_IRQn = 24,              //  24  SWI4_EGU4
    SWI5_EGU5_IRQn = 25,              //  25  SWI5_EGU5
    TIMER3_IRQn = 26,                 //  26  TIMER3
    TIMER4_IRQn = 27,                 //  27  TIMER4
    PWM0_IRQn  = 28,                  //  28  PWM0
    PDM_IRQn   = 29,                  //  29  PDM
    MWU_IRQn   = 32,                  //  32  MWU
    PWM1_IRQn  = 33,                  //  33  PWM1
    PWM2_IRQn  = 34,                  //  34  PWM2
    SPIM2_SPIS2_SPI2_IRQn = 35,       //  35  SPIM2_SPIS2_SPI2
    RTC2_IRQn  = 36,                  //  36  RTC2
    I2S_IRQn   = 37,                  //  37  I2S
    FPU_IRQn   = 38                   //  38  FPU
  );

  TFICR_INFO_Registers = record
    PART       : longword;            // Part code
    VARIANT    : longword;            // Part Variant, Hardware version and Production configuration
    PACKAGE    : longword;            // Package option
    RAM        : longword;            // RAM variant
    FLASH      : longword;            // Flash variant
    UNUSED0    : array[0..2] of longword; // Description collection[0]: Unspecified
  end;

  TFICR_TEMP_Registers = record
    A0         : longword;            // Slope definition A0.
    A1         : longword;            // Slope definition A1.
    A2         : longword;            // Slope definition A2.
    A3         : longword;            // Slope definition A3.
    A4         : longword;            // Slope definition A4.
    A5         : longword;            // Slope definition A5.
    B0         : longword;            // y-intercept B0.
    B1         : longword;            // y-intercept B1.
    B2         : longword;            // y-intercept B2.
    B3         : longword;            // y-intercept B3.
    B4         : longword;            // y-intercept B4.
    B5         : longword;            // y-intercept B5.
    T0         : longword;            // Segment end T0.
    T1         : longword;            // Segment end T1.
    T2         : longword;            // Segment end T2.
    T3         : longword;            // Segment end T3.
    T4         : longword;            // Segment end T4.
  end;

  TFICR_NFC_Registers = record
    TAGHEADER0 : longword;            // Default header for NFC Tag. Software can read these values to
    TAGHEADER1 : longword;            // Default header for NFC Tag. Software can read these values to
    TAGHEADER2 : longword;            // Default header for NFC Tag. Software can read these values to
    TAGHEADER3 : longword;            // Default header for NFC Tag. Software can read these values to
  end;

  TPOWER_RAM_Registers = record
    POWER      : longword;            // Description cluster[0]: RAM0 power control register
    POWERSET   : longword;            // Description cluster[0]: RAM0 power control set register
    POWERCLR   : longword;            // Description cluster[0]: RAM0 power control clear register
    RESERVED0  : longword;
  end;

  TUARTE_PSEL_Registers = record
    RTS        : longword;            // Pin select for RTS signal
    TXD        : longword;            // Pin select for TXD signal
    CTS        : longword;            // Pin select for CTS signal
    RXD        : longword;            // Pin select for RXD signal
  end;

  TUARTE_RXD_Registers = record
    PTR        : longword;            // Data pointer
    MAXCNT     : longword;            // Maximum number of bytes in receive buffer
    AMOUNT     : longword;            // Number of bytes transferred in the last transaction
  end;

  TUARTE_TXD_Registers = record
    PTR        : longword;            // Data pointer
    MAXCNT     : longword;            // Maximum number of bytes in transmit buffer
    AMOUNT     : longword;            // Number of bytes transferred in the last transaction
  end;

  TSPIM_PSEL_Registers = record
    SCK        : longword;            // Pin select for SCK
    MOSI       : longword;            // Pin select for MOSI signal
    MISO       : longword;            // Pin select for MISO signal
  end;

  TSPIM_RXD_Registers = record
    PTR        : longword;            // Data pointer
    MAXCNT     : longword;            // Maximum number of bytes in receive buffer
    AMOUNT     : longword;            // Number of bytes transferred in the last transaction
    LIST       : longword;            // EasyDMA list type
  end;

  TSPIM_TXD_Registers = record
    PTR        : longword;            // Data pointer
    MAXCNT     : longword;            // Maximum number of bytes in transmit buffer
    AMOUNT     : longword;            // Number of bytes transferred in the last transaction
    LIST       : longword;            // EasyDMA list type
  end;

  TSPIS_PSEL_Registers = record
    SCK        : longword;            // Pin select for SCK
    MISO       : longword;            // Pin select for MISO signal
    MOSI       : longword;            // Pin select for MOSI signal
    CSN        : longword;            // Pin select for CSN signal
  end;

  TSPIS_RXD_Registers = record
    PTR        : longword;            // RXD data pointer
    MAXCNT     : longword;            // Maximum number of bytes in receive buffer
    AMOUNT     : longword;            // Number of bytes received in last granted transaction
  end;

  TSPIS_TXD_Registers = record
    PTR        : longword;            // TXD data pointer
    MAXCNT     : longword;            // Maximum number of bytes in transmit buffer
    AMOUNT     : longword;            // Number of bytes transmitted in last granted transaction
  end;

  TTWIM_PSEL_Registers = record
    SCL        : longword;            // Pin select for SCL signal
    SDA        : longword;            // Pin select for SDA signal
  end;

  TTWIM_RXD_Registers = record
    PTR        : longword;            // Data pointer
    MAXCNT     : longword;            // Maximum number of bytes in receive buffer
    AMOUNT     : longword;            // Number of bytes transferred in the last transaction
    LIST       : longword;            // EasyDMA list type
  end;

  TTWIM_TXD_Registers = record
    PTR        : longword;            // Data pointer
    MAXCNT     : longword;            // Maximum number of bytes in transmit buffer
    AMOUNT     : longword;            // Number of bytes transferred in the last transaction
    LIST       : longword;            // EasyDMA list type
  end;

  TTWIS_PSEL_Registers = record
    SCL        : longword;            // Pin select for SCL signal
    SDA        : longword;            // Pin select for SDA signal
  end;

  TTWIS_RXD_Registers = record
    PTR        : longword;            // RXD Data pointer
    MAXCNT     : longword;            // Maximum number of bytes in RXD buffer
    AMOUNT     : longword;            // Number of bytes transferred in the last RXD transaction
  end;

  TTWIS_TXD_Registers = record
    PTR        : longword;            // TXD Data pointer
    MAXCNT     : longword;            // Maximum number of bytes in TXD buffer
    AMOUNT     : longword;            // Number of bytes transferred in the last TXD transaction
  end;

  TSPI_PSEL_Registers = record
    SCK        : longword;            // Pin select for SCK
    MOSI       : longword;            // Pin select for MOSI
    MISO       : longword;            // Pin select for MISO
  end;

  TNFCT_FRAMESTATUS_Registers = record
    RX         : longword;            // Result of last incoming frames
  end;

  TNFCT_TXD_Registers = record
    FRAMECONFIG : longword;           // Configuration of outgoing frames
    AMOUNT     : longword;            // Size of outgoing frame
  end;

  TNFCT_RXD_Registers = record
    FRAMECONFIG : longword;           // Configuration of incoming frames
    AMOUNT     : longword;            // Size of last incoming frame
  end;

  TSAADC_EVENTS_CH_Registers = record
    LIMITH     : longword;            // Description cluster[0]: Last results is equal or above CH[0].LIMIT.HIGH
    LIMITL     : longword;            // Description cluster[0]: Last results is equal or below CH[0].LIMIT.LOW
  end;

  TSAADC_CH_Registers = record
    PSELP      : longword;            // Description cluster[0]: Input positive pin selection for CH[0]
    PSELN      : longword;            // Description cluster[0]: Input negative pin selection for CH[0]
    CONFIG     : longword;            // Description cluster[0]: Input configuration for CH[0]
    LIMIT      : longword;            // Description cluster[0]: High/low limits for event monitoring
  end;

  TSAADC_RESULT_Registers = record
    PTR        : longword;            // Data pointer
    MAXCNT     : longword;            // Maximum number of buffer words to transfer
    AMOUNT     : longword;            // Number of buffer words transferred since last START
  end;

  TQDEC_PSEL_Registers = record
    LED        : longword;            // Pin select for LED signal
    A          : longword;            // Pin select for A signal
    B          : longword;            // Pin select for B signal
  end;

  TPWM_SEQ_Registers = record
    PTR        : longword;            // Description cluster[0]: Beginning address in Data RAM of this
    CNT        : longword;            // Description cluster[0]: Amount of values (duty cycles) in this
    REFRESH    : longword;            // Description cluster[0]: Amount of additional PWM periods between
    ENDDELAY   : longword;            // Description cluster[0]: Time added after the sequence
    RESERVED1  : array[0..3] of longword;
  end;

  TPWM_PSEL_Registers = record
    OUT        : array[0..3] of longword; // Description collection[0]: Output pin select for PWM channel
  end;

  TPDM_PSEL_Registers = record
    CLK        : longword;            // Pin number configuration for PDM CLK signal
    DIN        : longword;            // Pin number configuration for PDM DIN signal
  end;

  TPDM_SAMPLE_Registers = record
    PTR        : longword;            // RAM address pointer to write samples to with EasyDMA
    MAXCNT     : longword;            // Number of samples to allocate memory for in EasyDMA mode
  end;

  TPPI_TASKS_CHG_Registers = record
    EN         : longword;            // Description cluster[0]: Enable channel group 0
    DIS        : longword;            // Description cluster[0]: Disable channel group 0
  end;

  TPPI_CH_Registers = record
    EEP        : longword;            // Description cluster[0]: Channel 0 event end-point
    TEP        : longword;            // Description cluster[0]: Channel 0 task end-point
  end;

  TPPI_FORK_Registers = record
    TEP        : longword;            // Description cluster[0]: Channel 0 task end-point
  end;

  TMWU_EVENTS_REGION_Registers = record
    WA         : longword;            // Description cluster[0]: Write access to region 0 detected
    RA         : longword;            // Description cluster[0]: Read access to region 0 detected
  end;

  TMWU_EVENTS_PREGION_Registers = record
    WA         : longword;            // Description cluster[0]: Write access to peripheral region 0
    RA         : longword;            // Description cluster[0]: Read access to peripheral region 0 detected
  end;

  TMWU_PERREGION_Registers = record
    SUBSTATWA  : longword;            // Description cluster[0]: Source of event/interrupt in region
    SUBSTATRA  : longword;            // Description cluster[0]: Source of event/interrupt in region
  end;

  TMWU_REGION_Registers = record
    START      : longword;            // Description cluster[0]: Start address for region 0
    &END       : longword;            // Description cluster[0]: End address of region 0
    RESERVED2  : array[0..1] of longword;
  end;

  TMWU_PREGION_Registers = record
    START      : longword;            // Description cluster[0]: Reserved for future use
    &END       : longword;            // Description cluster[0]: Reserved for future use
    SUBS       : longword;            // Description cluster[0]: Subregions of region 0
    RESERVED3  : longword;
  end;

  TI2S_CONFIG_Registers = record
    MODE       : longword;            // I2S mode.
    RXEN       : longword;            // Reception (RX) enable.
    TXEN       : longword;            // Transmission (TX) enable.
    MCKEN      : longword;            // Master clock generator enable.
    MCKFREQ    : longword;            // Master clock generator frequency.
    RATIO      : longword;            // MCK / LRCK ratio.
    SWIDTH     : longword;            // Sample width.
    ALIGN      : longword;            // Alignment of sample within a frame.
    FORMAT     : longword;            // Frame format.
    CHANNELS   : longword;            // Enable channels.
  end;

  TI2S_RXD_Registers = record
    PTR        : longword;            // Receive buffer RAM start address.
  end;

  TI2S_TXD_Registers = record
    PTR        : longword;            // Transmit buffer RAM start address.
  end;

  TI2S_RXTXD_Registers = record
    MAXCNT     : longword;            // Size of RXD and TXD buffers.
  end;

  TI2S_PSEL_Registers = record
    MCK        : longword;            // Pin select for MCK signal.
    SCK        : longword;            // Pin select for SCK signal.
    LRCK       : longword;            // Pin select for LRCK signal.
    SDIN       : longword;            // Pin select for SDIN signal.
    SDOUT      : longword;            // Pin select for SDOUT signal.
  end;

  TFICR_Registers = record            // FICR Structure
    RESERVED0  : array[0..3] of longword;
    CODEPAGESIZE : longword;          // Code memory page size
    CODESIZE   : longword;            // Code memory size
    RESERVED1  : array[0..17] of longword;
    DEVICEID   : array[0..1] of longword; // Description collection[0]: Device identifier
    RESERVED2  : array[0..5] of longword;
    ER         : array[0..3] of longword; // Description collection[0]: Encryption Root, word 0
    IR         : array[0..3] of longword; // Description collection[0]: Identity Root, word 0
    DEVICEADDRTYPE : longword;        // Device address type
    DEVICEADDR : array[0..1] of longword; // Description collection[0]: Device address 0
    RESERVED3  : array[0..20] of longword;
    INFO       : TFICR_INFO_Registers; // Device info
    RESERVED4  : array[0..184] of longword;
    TEMP       : TFICR_TEMP_Registers; // Registers storing factory TEMP module linearization coefficients
    RESERVED5  : array[0..1] of longword;
    NFC        : TFICR_NFC_Registers; // Unspecified
  end;

  TUICR_Registers = record            // UICR Structure
    UNUSED0    : longword;            // Unspecified
    UNUSED1    : longword;            // Unspecified
    UNUSED2    : longword;            // Unspecified
    RESERVED0  : longword;
    UNUSED3    : longword;            // Unspecified
    NRFFW      : array[0..14] of longword; // Description collection[0]: Reserved for Nordic firmware design
    NRFHW      : array[0..11] of longword; // Description collection[0]: Reserved for Nordic hardware design
    CUSTOMER   : array[0..31] of longword; // Description collection[0]: Reserved for customer
    RESERVED1  : array[0..63] of longword;
    PSELRESET  : array[0..1] of longword; // Description collection[0]: Mapping of the nRESET function (see
    APPROTECT  : longword;            // Access Port protection
    NFCPINS    : longword;            // Setting of pins dedicated to NFC functionality: NFC antenna
  end;

  TBPROT_Registers = record           // BPROT Structure
    RESERVED0  : array[0..383] of longword;
    CONFIG0    : longword;            // Block protect configuration register 0
    CONFIG1    : longword;            // Block protect configuration register 1
    DISABLEINDEBUG : longword;        // Disable protection mechanism in debug interface mode
    UNUSED0    : longword;            // Unspecified
    CONFIG2    : longword;            // Block protect configuration register 2
    CONFIG3    : longword;            // Block protect configuration register 3
  end;

  TPOWER_Registers = record           // POWER Structure
    RESERVED0  : array[0..29] of longword;
    TASKS_CONSTLAT : longword;        // Enable constant latency mode
    TASKS_LOWPWR : longword;          // Enable low power mode (variable latency)
    RESERVED1  : array[0..33] of longword;
    EVENTS_POFWARN : longword;        // Power failure warning
    RESERVED2  : array[0..1] of longword;
    EVENTS_SLEEPENTER : longword;     // CPU entered WFI/WFE sleep
    EVENTS_SLEEPEXIT : longword;      // CPU exited WFI/WFE sleep
    RESERVED3  : array[0..121] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED4  : array[0..60] of longword;
    RESETREAS  : longword;            // Reset reason
    RESERVED5  : array[0..8] of longword;
    RAMSTATUS  : longword;            // Deprecated register - RAM status register
    RESERVED6  : array[0..52] of longword;
    SYSTEMOFF  : longword;            // System OFF register
    RESERVED7  : array[0..2] of longword;
    POFCON     : longword;            // Power failure comparator configuration
    RESERVED8  : array[0..1] of longword;
    GPREGRET   : longword;            // General purpose retention register
    GPREGRET2  : longword;            // General purpose retention register
    RAMON      : longword;            // Deprecated register - RAM on/off register (this register is
    RESERVED9  : array[0..10] of longword;
    RAMONB     : longword;            // Deprecated register - RAM on/off register (this register is
    RESERVED10 : array[0..7] of longword;
    DCDCEN     : longword;            // DC/DC enable register
    RESERVED11 : array[0..224] of longword;
    RAM        : array[0..7] of TPOWER_RAM_Registers; // Unspecified
  end;

  TCLOCK_Registers = record           // CLOCK Structure
    TASKS_HFCLKSTART : longword;      // Start HFCLK crystal oscillator
    TASKS_HFCLKSTOP : longword;       // Stop HFCLK crystal oscillator
    TASKS_LFCLKSTART : longword;      // Start LFCLK source
    TASKS_LFCLKSTOP : longword;       // Stop LFCLK source
    TASKS_CAL  : longword;            // Start calibration of LFRC oscillator
    TASKS_CTSTART : longword;         // Start calibration timer
    TASKS_CTSTOP : longword;          // Stop calibration timer
    RESERVED0  : array[0..56] of longword;
    EVENTS_HFCLKSTARTED : longword;   // HFCLK oscillator started
    EVENTS_LFCLKSTARTED : longword;   // LFCLK started
    RESERVED1  : longword;
    EVENTS_DONE : longword;           // Calibration of LFCLK RC oscillator complete event
    EVENTS_CTTO : longword;           // Calibration timer timeout
    RESERVED2  : array[0..123] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..62] of longword;
    HFCLKRUN   : longword;            // Status indicating that HFCLKSTART task has been triggered
    HFCLKSTAT  : longword;            // HFCLK status
    RESERVED4  : longword;
    LFCLKRUN   : longword;            // Status indicating that LFCLKSTART task has been triggered
    LFCLKSTAT  : longword;            // LFCLK status
    LFCLKSRCCOPY : longword;          // Copy of LFCLKSRC register, set when LFCLKSTART task was triggered
    RESERVED5  : array[0..61] of longword;
    LFCLKSRC   : longword;            // Clock source for the LFCLK
    RESERVED6  : array[0..6] of longword;
    CTIV       : longword;            // Calibration timer interval
    RESERVED7  : array[0..7] of longword;
    TRACECONFIG : longword;           // Clocking options for the Trace Port debug interface
  end;

  TRADIO_Registers = record           // RADIO Structure
    TASKS_TXEN : longword;            // Enable RADIO in TX mode
    TASKS_RXEN : longword;            // Enable RADIO in RX mode
    TASKS_START : longword;           // Start RADIO
    TASKS_STOP : longword;            // Stop RADIO
    TASKS_DISABLE : longword;         // Disable RADIO
    TASKS_RSSISTART : longword;       // Start the RSSI and take one single sample of the receive signal
    TASKS_RSSISTOP : longword;        // Stop the RSSI measurement
    TASKS_BCSTART : longword;         // Start the bit counter
    TASKS_BCSTOP : longword;          // Stop the bit counter
    RESERVED0  : array[0..54] of longword;
    EVENTS_READY : longword;          // RADIO has ramped up and is ready to be started
    EVENTS_ADDRESS : longword;        // Address sent or received
    EVENTS_PAYLOAD : longword;        // Packet payload sent or received
    EVENTS_END : longword;            // Packet sent or received
    EVENTS_DISABLED : longword;       // RADIO has been disabled
    EVENTS_DEVMATCH : longword;       // A device address match occurred on the last received packet
    EVENTS_DEVMISS : longword;        // No device address match occurred on the last received packet
    EVENTS_RSSIEND : longword;        // Sampling of receive signal strength complete.
    RESERVED1  : array[0..1] of longword;
    EVENTS_BCMATCH : longword;        // Bit counter reached bit count value.
    RESERVED2  : longword;
    EVENTS_CRCOK : longword;          // Packet received with CRC ok
    EVENTS_CRCERROR : longword;       // Packet received with CRC error
    RESERVED3  : array[0..49] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED4  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED5  : array[0..60] of longword;
    CRCSTATUS  : longword;            // CRC status
    RESERVED6  : longword;
    RXMATCH    : longword;            // Received address
    RXCRC      : longword;            // CRC field of previously received packet
    DAI        : longword;            // Device address match index
    RESERVED7  : array[0..59] of longword;
    PACKETPTR  : longword;            // Packet pointer
    FREQUENCY  : longword;            // Frequency
    TXPOWER    : longword;            // Output power
    MODE       : longword;            // Data rate and modulation
    PCNF0      : longword;            // Packet configuration register 0
    PCNF1      : longword;            // Packet configuration register 1
    BASE0      : longword;            // Base address 0
    BASE1      : longword;            // Base address 1
    PREFIX0    : longword;            // Prefixes bytes for logical addresses 0-3
    PREFIX1    : longword;            // Prefixes bytes for logical addresses 4-7
    TXADDRESS  : longword;            // Transmit address select
    RXADDRESSES : longword;           // Receive address select
    CRCCNF     : longword;            // CRC configuration
    CRCPOLY    : longword;            // CRC polynomial
    CRCINIT    : longword;            // CRC initial value
    UNUSED0    : longword;            // Unspecified
    TIFS       : longword;            // Inter Frame Spacing in us
    RSSISAMPLE : longword;            // RSSI sample
    RESERVED8  : longword;
    STATE      : longword;            // Current radio state
    DATAWHITEIV : longword;           // Data whitening initial value
    RESERVED9  : array[0..1] of longword;
    BCC        : longword;            // Bit counter compare
    RESERVED10 : array[0..38] of longword;
    DAB        : array[0..7] of longword; // Description collection[0]: Device address base segment 0
    DAP        : array[0..7] of longword; // Description collection[0]: Device address prefix 0
    DACNF      : longword;            // Device address match configuration
    RESERVED11 : array[0..2] of longword;
    MODECNF0   : longword;            // Radio mode configuration register 0
    RESERVED12 : array[0..617] of longword;
    POWER      : longword;            // Peripheral power control
  end;

  TUARTE_Registers = record           // UARTE Structure
    TASKS_STARTRX : longword;         // Start UART receiver
    TASKS_STOPRX : longword;          // Stop UART receiver
    TASKS_STARTTX : longword;         // Start UART transmitter
    TASKS_STOPTX : longword;          // Stop UART transmitter
    RESERVED0  : array[0..6] of longword;
    TASKS_FLUSHRX : longword;         // Flush RX FIFO into RX buffer
    RESERVED1  : array[0..51] of longword;
    EVENTS_CTS : longword;            // CTS is activated (set low). Clear To Send.
    EVENTS_NCTS : longword;           // CTS is deactivated (set high). Not Clear To Send.
    EVENTS_RXDRDY : longword;         // Data received in RXD (but potentially not yet transferred to
    RESERVED2  : longword;
    EVENTS_ENDRX : longword;          // Receive buffer is filled up
    RESERVED3  : array[0..1] of longword;
    EVENTS_TXDRDY : longword;         // Data sent from TXD
    EVENTS_ENDTX : longword;          // Last TX byte transmitted
    EVENTS_ERROR : longword;          // Error detected
    RESERVED4  : array[0..6] of longword;
    EVENTS_RXTO : longword;           // Receiver timeout
    RESERVED5  : longword;
    EVENTS_RXSTARTED : longword;      // UART receiver has started
    EVENTS_TXSTARTED : longword;      // UART transmitter has started
    RESERVED6  : longword;
    EVENTS_TXSTOPPED : longword;      // Transmitter stopped
    RESERVED7  : array[0..40] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED8  : array[0..62] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED9  : array[0..92] of longword;
    ERRORSRC   : longword;            // Error source
    RESERVED10 : array[0..30] of longword;
    ENABLE     : longword;            // Enable UART
    RESERVED11 : longword;
    PSEL       : TUARTE_PSEL_Registers; // Unspecified
    RESERVED12 : array[0..2] of longword;
    BAUDRATE   : longword;            // Baud rate. Accuracy depends on the HFCLK source selected.
    RESERVED13 : array[0..2] of longword;
    RXD        : TUARTE_RXD_Registers; // RXD EasyDMA channel
    RESERVED14 : longword;
    TXD        : TUARTE_TXD_Registers; // TXD EasyDMA channel
    RESERVED15 : array[0..6] of longword;
    CONFIG     : longword;            // Configuration of parity and hardware flow control
  end;

  TUART_Registers = record            // UART Structure
    TASKS_STARTRX : longword;         // Start UART receiver
    TASKS_STOPRX : longword;          // Stop UART receiver
    TASKS_STARTTX : longword;         // Start UART transmitter
    TASKS_STOPTX : longword;          // Stop UART transmitter
    RESERVED0  : array[0..2] of longword;
    TASKS_SUSPEND : longword;         // Suspend UART
    RESERVED1  : array[0..55] of longword;
    EVENTS_CTS : longword;            // CTS is activated (set low). Clear To Send.
    EVENTS_NCTS : longword;           // CTS is deactivated (set high). Not Clear To Send.
    EVENTS_RXDRDY : longword;         // Data received in RXD
    RESERVED2  : array[0..3] of longword;
    EVENTS_TXDRDY : longword;         // Data sent from TXD
    RESERVED3  : longword;
    EVENTS_ERROR : longword;          // Error detected
    RESERVED4  : array[0..6] of longword;
    EVENTS_RXTO : longword;           // Receiver timeout
    RESERVED5  : array[0..45] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED6  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED7  : array[0..92] of longword;
    ERRORSRC   : longword;            // Error source
    RESERVED8  : array[0..30] of longword;
    ENABLE     : longword;            // Enable UART
    RESERVED9  : longword;
    PSELRTS    : longword;            // Pin select for RTS
    PSELTXD    : longword;            // Pin select for TXD
    PSELCTS    : longword;            // Pin select for CTS
    PSELRXD    : longword;            // Pin select for RXD
    RXD        : longword;            // RXD register
    TXD        : longword;            // TXD register
    RESERVED10 : longword;
    BAUDRATE   : longword;            // Baud rate
    RESERVED11 : array[0..16] of longword;
    CONFIG     : longword;            // Configuration of parity and hardware flow control
  end;

  TSPIM_Registers = record            // SPIM Structure
    RESERVED0  : array[0..3] of longword;
    TASKS_START : longword;           // Start SPI transaction
    TASKS_STOP : longword;            // Stop SPI transaction
    RESERVED1  : longword;
    TASKS_SUSPEND : longword;         // Suspend SPI transaction
    TASKS_RESUME : longword;          // Resume SPI transaction
    RESERVED2  : array[0..55] of longword;
    EVENTS_STOPPED : longword;        // SPI transaction has stopped
    RESERVED3  : array[0..1] of longword;
    EVENTS_ENDRX : longword;          // End of RXD buffer reached
    RESERVED4  : longword;
    EVENTS_END : longword;            // End of RXD buffer and TXD buffer reached
    RESERVED5  : longword;
    EVENTS_ENDTX : longword;          // End of TXD buffer reached
    RESERVED6  : array[0..9] of longword;
    EVENTS_STARTED : longword;        // Transaction started
    RESERVED7  : array[0..43] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED8  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED9  : array[0..124] of longword;
    ENABLE     : longword;            // Enable SPIM
    RESERVED10 : longword;
    PSEL       : TSPIM_PSEL_Registers; // Unspecified
    RESERVED11 : array[0..3] of longword;
    FREQUENCY  : longword;            // SPI frequency
    RESERVED12 : array[0..2] of longword;
    RXD        : TSPIM_RXD_Registers; // RXD EasyDMA channel
    TXD        : TSPIM_TXD_Registers; // TXD EasyDMA channel
    CONFIG     : longword;            // Configuration register
    RESERVED13 : array[0..25] of longword;
    ORC        : longword;            // Over-read character. Character clocked out in case and over-read
  end;

  TSPIS_Registers = record            // SPIS Structure
    RESERVED0  : array[0..8] of longword;
    TASKS_ACQUIRE : longword;         // Acquire SPI semaphore
    TASKS_RELEASE : longword;         // Release SPI semaphore, enabling the SPI slave to acquire it
    RESERVED1  : array[0..53] of longword;
    EVENTS_END : longword;            // Granted transaction completed
    RESERVED2  : array[0..1] of longword;
    EVENTS_ENDRX : longword;          // End of RXD buffer reached
    RESERVED3  : array[0..4] of longword;
    EVENTS_ACQUIRED : longword;       // Semaphore acquired
    RESERVED4  : array[0..52] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED5  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED6  : array[0..60] of longword;
    SEMSTAT    : longword;            // Semaphore status register
    RESERVED7  : array[0..14] of longword;
    STATUS     : longword;            // Status from last transaction
    RESERVED8  : array[0..46] of longword;
    ENABLE     : longword;            // Enable SPI slave
    RESERVED9  : longword;
    PSEL       : TSPIS_PSEL_Registers; // Unspecified
    RESERVED10 : array[0..6] of longword;
    RXD        : TSPIS_RXD_Registers; // Unspecified
    RESERVED11 : longword;
    TXD        : TSPIS_TXD_Registers; // Unspecified
    RESERVED12 : longword;
    CONFIG     : longword;            // Configuration register
    RESERVED13 : longword;
    DEF        : longword;            // Default character. Character clocked out in case of an ignored
    RESERVED14 : array[0..23] of longword;
    ORC        : longword;            // Over-read character
  end;

  TTWIM_Registers = record            // TWIM Structure
    TASKS_STARTRX : longword;         // Start TWI receive sequence
    RESERVED0  : longword;
    TASKS_STARTTX : longword;         // Start TWI transmit sequence
    RESERVED1  : array[0..1] of longword;
    TASKS_STOP : longword;            // Stop TWI transaction. Must be issued while the TWI master is
    RESERVED2  : longword;
    TASKS_SUSPEND : longword;         // Suspend TWI transaction
    TASKS_RESUME : longword;          // Resume TWI transaction
    RESERVED3  : array[0..55] of longword;
    EVENTS_STOPPED : longword;        // TWI stopped
    RESERVED4  : array[0..6] of longword;
    EVENTS_ERROR : longword;          // TWI error
    RESERVED5  : array[0..7] of longword;
    EVENTS_SUSPENDED : longword;      // Last byte has been sent out after the SUSPEND task has been
    EVENTS_RXSTARTED : longword;      // Receive sequence started
    EVENTS_TXSTARTED : longword;      // Transmit sequence started
    RESERVED6  : array[0..1] of longword;
    EVENTS_LASTRX : longword;         // Byte boundary, starting to receive the last byte
    EVENTS_LASTTX : longword;         // Byte boundary, starting to transmit the last byte
    RESERVED7  : array[0..38] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED8  : array[0..62] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED9  : array[0..109] of longword;
    ERRORSRC   : longword;            // Error source
    RESERVED10 : array[0..13] of longword;
    ENABLE     : longword;            // Enable TWIM
    RESERVED11 : longword;
    PSEL       : TTWIM_PSEL_Registers; // Unspecified
    RESERVED12 : array[0..4] of longword;
    FREQUENCY  : longword;            // TWI frequency
    RESERVED13 : array[0..2] of longword;
    RXD        : TTWIM_RXD_Registers; // RXD EasyDMA channel
    TXD        : TTWIM_TXD_Registers; // TXD EasyDMA channel
    RESERVED14 : array[0..12] of longword;
    ADDRESS    : longword;            // Address used in the TWI transfer
  end;

  TTWIS_Registers = record            // TWIS Structure
    RESERVED0  : array[0..4] of longword;
    TASKS_STOP : longword;            // Stop TWI transaction
    RESERVED1  : longword;
    TASKS_SUSPEND : longword;         // Suspend TWI transaction
    TASKS_RESUME : longword;          // Resume TWI transaction
    RESERVED2  : array[0..2] of longword;
    TASKS_PREPARERX : longword;       // Prepare the TWI slave to respond to a write command
    TASKS_PREPARETX : longword;       // Prepare the TWI slave to respond to a read command
    RESERVED3  : array[0..50] of longword;
    EVENTS_STOPPED : longword;        // TWI stopped
    RESERVED4  : array[0..6] of longword;
    EVENTS_ERROR : longword;          // TWI error
    RESERVED5  : array[0..8] of longword;
    EVENTS_RXSTARTED : longword;      // Receive sequence started
    EVENTS_TXSTARTED : longword;      // Transmit sequence started
    RESERVED6  : array[0..3] of longword;
    EVENTS_WRITE : longword;          // Write command received
    EVENTS_READ : longword;           // Read command received
    RESERVED7  : array[0..36] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED8  : array[0..62] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED9  : array[0..112] of longword;
    ERRORSRC   : longword;            // Error source
    MATCH      : longword;            // Status register indicating which address had a match
    RESERVED10 : array[0..9] of longword;
    ENABLE     : longword;            // Enable TWIS
    RESERVED11 : longword;
    PSEL       : TTWIS_PSEL_Registers; // Unspecified
    RESERVED12 : array[0..8] of longword;
    RXD        : TTWIS_RXD_Registers; // RXD EasyDMA channel
    RESERVED13 : longword;
    TXD        : TTWIS_TXD_Registers; // TXD EasyDMA channel
    RESERVED14 : array[0..13] of longword;
    ADDRESS    : array[0..1] of longword; // Description collection[0]: TWI slave address 0
    RESERVED15 : longword;
    CONFIG     : longword;            // Configuration register for the address match mechanism
    RESERVED16 : array[0..9] of longword;
    ORC        : longword;            // Over-read character. Character sent out in case of an over-read
  end;

  TSPI_Registers = record             // SPI Structure
    RESERVED0  : array[0..65] of longword;
    EVENTS_READY : longword;          // TXD byte sent and RXD byte received
    RESERVED1  : array[0..125] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED2  : array[0..124] of longword;
    ENABLE     : longword;            // Enable SPI
    RESERVED3  : longword;
    PSEL       : TSPI_PSEL_Registers; // Unspecified
    RESERVED4  : longword;
    RXD        : longword;            // RXD register
    TXD        : longword;            // TXD register
    RESERVED5  : longword;
    FREQUENCY  : longword;            // SPI frequency
    RESERVED6  : array[0..10] of longword;
    CONFIG     : longword;            // Configuration register
  end;

  TTWI_Registers = record             // TWI Structure
    TASKS_STARTRX : longword;         // Start TWI receive sequence
    RESERVED0  : longword;
    TASKS_STARTTX : longword;         // Start TWI transmit sequence
    RESERVED1  : array[0..1] of longword;
    TASKS_STOP : longword;            // Stop TWI transaction
    RESERVED2  : longword;
    TASKS_SUSPEND : longword;         // Suspend TWI transaction
    TASKS_RESUME : longword;          // Resume TWI transaction
    RESERVED3  : array[0..55] of longword;
    EVENTS_STOPPED : longword;        // TWI stopped
    EVENTS_RXDREADY : longword;       // TWI RXD byte received
    RESERVED4  : array[0..3] of longword;
    EVENTS_TXDSENT : longword;        // TWI TXD byte sent
    RESERVED5  : longword;
    EVENTS_ERROR : longword;          // TWI error
    RESERVED6  : array[0..3] of longword;
    EVENTS_BB  : longword;            // TWI byte boundary, generated before each byte that is sent or
    RESERVED7  : array[0..2] of longword;
    EVENTS_SUSPENDED : longword;      // TWI entered the suspended state
    RESERVED8  : array[0..44] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED9  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED10 : array[0..109] of longword;
    ERRORSRC   : longword;            // Error source
    RESERVED11 : array[0..13] of longword;
    ENABLE     : longword;            // Enable TWI
    RESERVED12 : longword;
    PSELSCL    : longword;            // Pin select for SCL
    PSELSDA    : longword;            // Pin select for SDA
    RESERVED13 : array[0..1] of longword;
    RXD        : longword;            // RXD register
    TXD        : longword;            // TXD register
    RESERVED14 : longword;
    FREQUENCY  : longword;            // TWI frequency
    RESERVED15 : array[0..23] of longword;
    ADDRESS    : longword;            // Address used in the TWI transfer
  end;

  TNFCT_Registers = record            // NFCT Structure
    TASKS_ACTIVATE : longword;        // Activate NFC peripheral for incoming and outgoing frames, change
    TASKS_DISABLE : longword;         // Disable NFC peripheral
    TASKS_SENSE : longword;           // Enable NFC sense field mode, change state to sense mode
    TASKS_STARTTX : longword;         // Start transmission of a outgoing frame, change state to transmit
    RESERVED0  : array[0..2] of longword;
    TASKS_ENABLERXDATA : longword;    // Initializes the EasyDMA for receive.
    RESERVED1  : longword;
    TASKS_GOIDLE : longword;          // Force state machine to IDLE state
    TASKS_GOSLEEP : longword;         // Force state machine to SLEEP_A state
    RESERVED2  : array[0..52] of longword;
    EVENTS_READY : longword;          // The NFC peripheral is ready to receive and send frames
    EVENTS_FIELDDETECTED : longword;  // Remote NFC field detected
    EVENTS_FIELDLOST : longword;      // Remote NFC field lost
    EVENTS_TXFRAMESTART : longword;   // Marks the start of the first symbol of a transmitted frame
    EVENTS_TXFRAMEEND : longword;     // Marks the end of the last transmitted on-air symbol of a frame
    EVENTS_RXFRAMESTART : longword;   // Marks the end of the first symbol of a received frame
    EVENTS_RXFRAMEEND : longword;     // Received data have been checked (CRC, parity) and transferred
    EVENTS_ERROR : longword;          // NFC error reported. The ERRORSTATUS register contains details
    RESERVED3  : array[0..1] of longword;
    EVENTS_RXERROR : longword;        // NFC RX frame error reported. The FRAMESTATUS.RX register contains
    EVENTS_ENDRX : longword;          // RX buffer (as defined by PACKETPTR and MAXLEN) in Data RAM full.
    EVENTS_ENDTX : longword;          // Transmission of data in RAM has ended, and EasyDMA has ended
    RESERVED4  : longword;
    EVENTS_AUTOCOLRESSTARTED : longword; // Auto collision resolution process has started
    RESERVED5  : array[0..2] of longword;
    EVENTS_COLLISION : longword;      // NFC Auto collision resolution error reported.
    EVENTS_SELECTED : longword;       // NFC Auto collision resolution successfully completed
    EVENTS_STARTED : longword;        // EasyDMA is ready to receive or send frames.
    RESERVED6  : array[0..42] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED7  : array[0..62] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED8  : array[0..61] of longword;
    ERRORSTATUS : longword;           // NFC Error Status register
    RESERVED9  : longword;
    FRAMESTATUS : TNFCT_FRAMESTATUS_Registers; // Unspecified
    RESERVED10 : array[0..7] of longword;
    CURRENTLOADCTRL : longword;       // Current value driven to the NFC Load Control
    RESERVED11 : array[0..1] of longword;
    FIELDPRESENT : longword;          // Indicates the presence or not of a valid field
    RESERVED12 : array[0..48] of longword;
    FRAMEDELAYMIN : longword;         // Minimum frame delay
    FRAMEDELAYMAX : longword;         // Maximum frame delay
    FRAMEDELAYMODE : longword;        // Configuration register for the Frame Delay Timer
    PACKETPTR  : longword;            // Packet pointer for TXD and RXD data storage in Data RAM
    MAXLEN     : longword;            // Size of allocated for TXD and RXD data storage buffer in Data
    TXD        : TNFCT_TXD_Registers; // Unspecified
    RXD        : TNFCT_RXD_Registers; // Unspecified
    RESERVED13 : array[0..25] of longword;
    NFCID1_LAST : longword;           // Last NFCID1 part (4, 7 or 10 bytes ID)
    NFCID1_2ND_LAST : longword;       // Second last NFCID1 part (7 or 10 bytes ID)
    NFCID1_3RD_LAST : longword;       // Third last NFCID1 part (10 bytes ID)
    RESERVED14 : longword;
    SENSRES    : longword;            // NFC-A SENS_RES auto-response settings
    SELRES     : longword;            // NFC-A SEL_RES auto-response settings
  end;

  TGPIOTE_Registers = record          // GPIOTE Structure
    TASKS_OUT  : array[0..7] of longword; // Description collection[0]: Task for writing to pin specified
    RESERVED0  : array[0..3] of longword;
    TASKS_SET  : array[0..7] of longword; // Description collection[0]: Task for writing to pin specified
    RESERVED1  : array[0..3] of longword;
    TASKS_CLR  : array[0..7] of longword; // Description collection[0]: Task for writing to pin specified
    RESERVED2  : array[0..31] of longword;
    EVENTS_IN  : array[0..7] of longword; // Description collection[0]: Event generated from pin specified
    RESERVED3  : array[0..22] of longword;
    EVENTS_PORT : longword;           // Event generated from multiple input GPIO pins with SENSE mechanism
    RESERVED4  : array[0..96] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED5  : array[0..128] of longword;
    CONFIG     : array[0..7] of longword; // Description collection[0]: Configuration for OUT[n], SET[n]
  end;

  TSAADC_Registers = record           // SAADC Structure
    TASKS_START : longword;           // Start the ADC and prepare the result buffer in RAM
    TASKS_SAMPLE : longword;          // Take one ADC sample, if scan is enabled all channels are sampled
    TASKS_STOP : longword;            // Stop the ADC and terminate any on-going conversion
    TASKS_CALIBRATEOFFSET : longword; // Starts offset auto-calibration
    RESERVED0  : array[0..59] of longword;
    EVENTS_STARTED : longword;        // The ADC has started
    EVENTS_END : longword;            // The ADC has filled up the Result buffer
    EVENTS_DONE : longword;           // A conversion task has been completed. Depending on the mode,
    EVENTS_RESULTDONE : longword;     // A result is ready to get transferred to RAM.
    EVENTS_CALIBRATEDONE : longword;  // Calibration is complete
    EVENTS_STOPPED : longword;        // The ADC has stopped
    EVENTS_CH  : array[0..7] of TSAADC_EVENTS_CH_Registers; // Unspecified
    RESERVED1  : array[0..105] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED2  : array[0..60] of longword;
    STATUS     : longword;            // Status
    RESERVED3  : array[0..62] of longword;
    ENABLE     : longword;            // Enable or disable ADC
    RESERVED4  : array[0..2] of longword;
    CH         : array[0..7] of TSAADC_CH_Registers; // Unspecified
    RESERVED5  : array[0..23] of longword;
    RESOLUTION : longword;            // Resolution configuration
    OVERSAMPLE : longword;            // Oversampling configuration. OVERSAMPLE should not be combined
    SAMPLERATE : longword;            // Controls normal or continuous sample rate
    RESERVED6  : array[0..11] of longword;
    RESULT     : TSAADC_RESULT_Registers; // RESULT EasyDMA channel
  end;

  TTIMER_Registers = record           // TIMER Structure
    TASKS_START : longword;           // Start Timer
    TASKS_STOP : longword;            // Stop Timer
    TASKS_COUNT : longword;           // Increment Timer (Counter mode only)
    TASKS_CLEAR : longword;           // Clear time
    TASKS_SHUTDOWN : longword;        // Deprecated register - Shut down timer
    RESERVED0  : array[0..10] of longword;
    TASKS_CAPTURE : array[0..5] of longword; // Description collection[0]: Capture Timer value to CC[0] register
    RESERVED1  : array[0..57] of longword;
    EVENTS_COMPARE : array[0..5] of longword; // Description collection[0]: Compare event on CC[0] match
    RESERVED2  : array[0..41] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED3  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED4  : array[0..125] of longword;
    MODE       : longword;            // Timer mode selection
    BITMODE    : longword;            // Configure the number of bits used by the TIMER
    RESERVED5  : longword;
    PRESCALER  : longword;            // Timer prescaler register
    RESERVED6  : array[0..10] of longword;
    CC         : array[0..5] of longword; // Description collection[0]: Capture/Compare register 0
  end;

  TRTC_Registers = record             // RTC Structure
    TASKS_START : longword;           // Start RTC COUNTER
    TASKS_STOP : longword;            // Stop RTC COUNTER
    TASKS_CLEAR : longword;           // Clear RTC COUNTER
    TASKS_TRIGOVRFLW : longword;      // Set COUNTER to 0xFFFFF0
    RESERVED0  : array[0..59] of longword;
    EVENTS_TICK : longword;           // Event on COUNTER increment
    EVENTS_OVRFLW : longword;         // Event on COUNTER overflow
    RESERVED1  : array[0..13] of longword;
    EVENTS_COMPARE : array[0..3] of longword; // Description collection[0]: Compare event on CC[0] match
    RESERVED2  : array[0..108] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..12] of longword;
    EVTEN      : longword;            // Enable or disable event routing
    EVTENSET   : longword;            // Enable event routing
    EVTENCLR   : longword;            // Disable event routing
    RESERVED4  : array[0..109] of longword;
    COUNTER    : longword;            // Current COUNTER value
    PRESCALER  : longword;            // 12 bit prescaler for COUNTER frequency (32768/(PRESCALER+1)).Must
    RESERVED5  : array[0..12] of longword;
    CC         : array[0..3] of longword; // Description collection[0]: Compare register 0
  end;

  TTEMP_Registers = record            // TEMP Structure
    TASKS_START : longword;           // Start temperature measurement
    TASKS_STOP : longword;            // Stop temperature measurement
    RESERVED0  : array[0..61] of longword;
    EVENTS_DATARDY : longword;        // Temperature measurement complete, data ready
    RESERVED1  : array[0..127] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED2  : array[0..126] of longword;
    TEMP       : longint;             // Temperature in degC (0.25deg steps)
    RESERVED3  : array[0..4] of longword;
    A0         : longword;            // Slope of 1st piece wise linear function
    A1         : longword;            // Slope of 2nd piece wise linear function
    A2         : longword;            // Slope of 3rd piece wise linear function
    A3         : longword;            // Slope of 4th piece wise linear function
    A4         : longword;            // Slope of 5th piece wise linear function
    A5         : longword;            // Slope of 6th piece wise linear function
    RESERVED4  : array[0..1] of longword;
    B0         : longword;            // y-intercept of 1st piece wise linear function
    B1         : longword;            // y-intercept of 2nd piece wise linear function
    B2         : longword;            // y-intercept of 3rd piece wise linear function
    B3         : longword;            // y-intercept of 4th piece wise linear function
    B4         : longword;            // y-intercept of 5th piece wise linear function
    B5         : longword;            // y-intercept of 6th piece wise linear function
    RESERVED5  : array[0..1] of longword;
    T0         : longword;            // End point of 1st piece wise linear function
    T1         : longword;            // End point of 2nd piece wise linear function
    T2         : longword;            // End point of 3rd piece wise linear function
    T3         : longword;            // End point of 4th piece wise linear function
    T4         : longword;            // End point of 5th piece wise linear function
  end;

  TRNG_Registers = record             // RNG Structure
    TASKS_START : longword;           // Task starting the random number generator
    TASKS_STOP : longword;            // Task stopping the random number generator
    RESERVED0  : array[0..61] of longword;
    EVENTS_VALRDY : longword;         // Event being generated for every new random number written to
    RESERVED1  : array[0..62] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED2  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..125] of longword;
    CONFIG     : longword;            // Configuration register
    VALUE      : longword;            // Output random number
  end;

  TECB_Registers = record             // ECB Structure
    TASKS_STARTECB : longword;        // Start ECB block encrypt
    TASKS_STOPECB : longword;         // Abort a possible executing ECB operation
    RESERVED0  : array[0..61] of longword;
    EVENTS_ENDECB : longword;         // ECB block encrypt complete
    EVENTS_ERRORECB : longword;       // ECB block encrypt aborted because of a STOPECB task or due to
    RESERVED1  : array[0..126] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED2  : array[0..125] of longword;
    ECBDATAPTR : longword;            // ECB block encrypt memory pointers
  end;

  TCCM_Registers = record             // CCM Structure
    TASKS_KSGEN : longword;           // Start generation of key-stream. This operation will stop by
    TASKS_CRYPT : longword;           // Start encryption/decryption. This operation will stop by itself
    TASKS_STOP : longword;            // Stop encryption/decryption
    RESERVED0  : array[0..60] of longword;
    EVENTS_ENDKSGEN : longword;       // Key-stream generation complete
    EVENTS_ENDCRYPT : longword;       // Encrypt/decrypt complete
    EVENTS_ERROR : longword;          // CCM error event
    RESERVED1  : array[0..60] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED2  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..60] of longword;
    MICSTATUS  : longword;            // MIC check result
    RESERVED4  : array[0..62] of longword;
    ENABLE     : longword;            // Enable
    MODE       : longword;            // Operation mode
    CNFPTR     : longword;            // Pointer to data structure holding AES key and NONCE vector
    INPTR      : longword;            // Input pointer
    OUTPTR     : longword;            // Output pointer
    SCRATCHPTR : longword;            // Pointer to data area used for temporary storage
  end;

  TAAR_Registers = record             // AAR Structure
    TASKS_START : longword;           // Start resolving addresses based on IRKs specified in the IRK
    RESERVED0  : longword;
    TASKS_STOP : longword;            // Stop resolving addresses
    RESERVED1  : array[0..60] of longword;
    EVENTS_END : longword;            // Address resolution procedure complete
    EVENTS_RESOLVED : longword;       // Address resolved
    EVENTS_NOTRESOLVED : longword;    // Address not resolved
    RESERVED2  : array[0..125] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..60] of longword;
    STATUS     : longword;            // Resolution status
    RESERVED4  : array[0..62] of longword;
    ENABLE     : longword;            // Enable AAR
    NIRK       : longword;            // Number of IRKs
    IRKPTR     : longword;            // Pointer to IRK data structure
    RESERVED5  : longword;
    ADDRPTR    : longword;            // Pointer to the resolvable address
    SCRATCHPTR : longword;            // Pointer to data area used for temporary storage
  end;

  TWDT_Registers = record             // WDT Structure
    TASKS_START : longword;           // Start the watchdog
    RESERVED0  : array[0..62] of longword;
    EVENTS_TIMEOUT : longword;        // Watchdog timeout
    RESERVED1  : array[0..127] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED2  : array[0..60] of longword;
    RUNSTATUS  : longword;            // Run status
    REQSTATUS  : longword;            // Request status
    RESERVED3  : array[0..62] of longword;
    CRV        : longword;            // Counter reload value
    RREN       : longword;            // Enable register for reload request registers
    CONFIG     : longword;            // Configuration register
    RESERVED4  : array[0..59] of longword;
    RR         : array[0..7] of longword; // Description collection[0]: Reload request 0
  end;

  TQDEC_Registers = record            // QDEC Structure
    TASKS_START : longword;           // Task starting the quadrature decoder
    TASKS_STOP : longword;            // Task stopping the quadrature decoder
    TASKS_READCLRACC : longword;      // Read and clear ACC and ACCDBL
    TASKS_RDCLRACC : longword;        // Read and clear ACC
    TASKS_RDCLRDBL : longword;        // Read and clear ACCDBL
    RESERVED0  : array[0..58] of longword;
    EVENTS_SAMPLERDY : longword;      // Event being generated for every new sample value written to
    EVENTS_REPORTRDY : longword;      // Non-null report ready
    EVENTS_ACCOF : longword;          // ACC or ACCDBL register overflow
    EVENTS_DBLRDY : longword;         // Double displacement(s) detected
    EVENTS_STOPPED : longword;        // QDEC has been stopped
    RESERVED1  : array[0..58] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED2  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..124] of longword;
    ENABLE     : longword;            // Enable the quadrature decoder
    LEDPOL     : longword;            // LED output pin polarity
    SAMPLEPER  : longword;            // Sample period
    SAMPLE     : longint;             // Motion sample value
    REPORTPER  : longword;            // Number of samples to be taken before REPORTRDY and DBLRDY events
    ACC        : longint;             // Register accumulating the valid transitions
    ACCREAD    : longint;             // Snapshot of the ACC register, updated by the READCLRACC or RDCLRACC
    PSEL       : TQDEC_PSEL_Registers; // Unspecified
    DBFEN      : longword;            // Enable input debounce filters
    RESERVED4  : array[0..4] of longword;
    LEDPRE     : longword;            // Time period the LED is switched ON prior to sampling
    ACCDBL     : longword;            // Register accumulating the number of detected double transitions
    ACCDBLREAD : longword;            // Snapshot of the ACCDBL, updated by the READCLRACC or RDCLRDBL
  end;

  TCOMP_Registers = record            // COMP Structure
    TASKS_START : longword;           // Start comparator
    TASKS_STOP : longword;            // Stop comparator
    TASKS_SAMPLE : longword;          // Sample comparator value
    RESERVED0  : array[0..60] of longword;
    EVENTS_READY : longword;          // COMP is ready and output is valid
    EVENTS_DOWN : longword;           // Downward crossing
    EVENTS_UP  : longword;            // Upward crossing
    EVENTS_CROSS : longword;          // Downward or upward crossing
    RESERVED1  : array[0..59] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED2  : array[0..62] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..60] of longword;
    RESULT     : longword;            // Compare result
    RESERVED4  : array[0..62] of longword;
    ENABLE     : longword;            // COMP enable
    PSEL       : longword;            // Pin select
    REFSEL     : longword;            // Reference source select
    EXTREFSEL  : longword;            // External reference select
    RESERVED5  : array[0..7] of longword;
    TH         : longword;            // Threshold configuration for hysteresis unit
    MODE       : longword;            // Mode configuration
    HYST       : longword;            // Comparator hysteresis enable
    ISOURCE    : longword;            // Current source select on analog input
  end;

  TLPCOMP_Registers = record          // LPCOMP Structure
    TASKS_START : longword;           // Start comparator
    TASKS_STOP : longword;            // Stop comparator
    TASKS_SAMPLE : longword;          // Sample comparator value
    RESERVED0  : array[0..60] of longword;
    EVENTS_READY : longword;          // LPCOMP is ready and output is valid
    EVENTS_DOWN : longword;           // Downward crossing
    EVENTS_UP  : longword;            // Upward crossing
    EVENTS_CROSS : longword;          // Downward or upward crossing
    RESERVED1  : array[0..59] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED2  : array[0..63] of longword;
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..60] of longword;
    RESULT     : longword;            // Compare result
    RESERVED4  : array[0..62] of longword;
    ENABLE     : longword;            // Enable LPCOMP
    PSEL       : longword;            // Input pin select
    REFSEL     : longword;            // Reference select
    EXTREFSEL  : longword;            // External reference select
    RESERVED5  : array[0..3] of longword;
    ANADETECT  : longword;            // Analog detect configuration
    RESERVED6  : array[0..4] of longword;
    HYST       : longword;            // Comparator hysteresis enable
  end;

  TSWI_Registers = record             // SWI Structure
    UNUSED     : longword;            // Unused.
  end;

  TEGU_Registers = record             // EGU Structure
    TASKS_TRIGGER : array[0..15] of longword; // Description collection[0]: Trigger 0 for triggering the corresponding
    RESERVED0  : array[0..47] of longword;
    EVENTS_TRIGGERED : array[0..15] of longword; // Description collection[0]: Event number 0 generated by triggering
    RESERVED1  : array[0..111] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
  end;

  TPWM_Registers = record             // PWM Structure
    RESERVED0  : longword;
    TASKS_STOP : longword;            // Stops PWM pulse generation on all channels at the end of current
    TASKS_SEQSTART : array[0..1] of longword; // Description collection[0]: Loads the first PWM value on all
    TASKS_NEXTSTEP : longword;        // Steps by one value in the current sequence on all enabled channels
    RESERVED1  : array[0..59] of longword;
    EVENTS_STOPPED : longword;        // Response to STOP task, emitted when PWM pulses are no longer
    EVENTS_SEQSTARTED : array[0..1] of longword; // Description collection[0]: First PWM period started on sequence
    EVENTS_SEQEND : array[0..1] of longword; // Description collection[0]: Emitted at end of every sequence
    EVENTS_PWMPERIODEND : longword;   // Emitted at the end of each PWM period
    EVENTS_LOOPSDONE : longword;      // Concatenated sequences have been played the amount of times
    RESERVED2  : array[0..55] of longword;
    SHORTS     : longword;            // Shortcut register
    RESERVED3  : array[0..62] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED4  : array[0..124] of longword;
    ENABLE     : longword;            // PWM module enable register
    MODE       : longword;            // Selects operating mode of the wave counter
    COUNTERTOP : longword;            // Value up to which the pulse generator counter counts
    PRESCALER  : longword;            // Configuration for PWM_CLK
    DECODER    : longword;            // Configuration of the decoder
    LOOP       : longword;            // Amount of playback of a loop
    RESERVED5  : array[0..1] of longword;
    SEQ        : array[0..1] of TPWM_SEQ_Registers; // Unspecified
    PSEL       : TPWM_PSEL_Registers; // Unspecified
  end;

  TPDM_Registers = record             // PDM Structure
    TASKS_START : longword;           // Starts continuous PDM transfer
    TASKS_STOP : longword;            // Stops PDM transfer
    RESERVED0  : array[0..61] of longword;
    EVENTS_STARTED : longword;        // PDM transfer has started
    EVENTS_STOPPED : longword;        // PDM transfer has finished
    EVENTS_END : longword;            // The PDM has written the last sample specified by SAMPLE.MAXCNT
    RESERVED1  : array[0..124] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED2  : array[0..124] of longword;
    ENABLE     : longword;            // PDM module enable register
    PDMCLKCTRL : longword;            // PDM clock generator control
    MODE       : longword;            // Defines the routing of the connected PDM microphones' signals
    RESERVED3  : array[0..2] of longword;
    GAINL      : longword;            // Left output gain adjustment
    GAINR      : longword;            // Right output gain adjustment
    RESERVED4  : array[0..7] of longword;
    PSEL       : TPDM_PSEL_Registers; // Unspecified
    RESERVED5  : array[0..5] of longword;
    SAMPLE     : TPDM_SAMPLE_Registers; // Unspecified
  end;

  TNVMC_Registers = record            // NVMC Structure
    RESERVED0  : array[0..255] of longword;
    READY      : longword;            // Ready flag
    RESERVED1  : array[0..63] of longword;
    CONFIG     : longword;            // Configuration register
    ERASEPAGE  : longword;            // Register for erasing a page in Code area
    ERASEALL   : longword;            // Register for erasing all non-volatile user memory
    ERASEPCR0  : longword;            // Deprecated register - Register for erasing a page in Code area.
    ERASEUICR  : longword;            // Register for erasing User Information Configuration Registers
    RESERVED2  : array[0..9] of longword;
    ICACHECNF  : longword;            // I-Code cache configuration register.
    RESERVED3  : longword;
    IHIT       : longword;            // I-Code cache hit counter.
    IMISS      : longword;            // I-Code cache miss counter.
  end;

  TPPI_Registers = record             // PPI Structure
    TASKS_CHG  : array[0..5] of TPPI_TASKS_CHG_Registers; // Channel group tasks
    RESERVED0  : array[0..307] of longword;
    CHEN       : longword;            // Channel enable register
    CHENSET    : longword;            // Channel enable set register
    CHENCLR    : longword;            // Channel enable clear register
    RESERVED1  : longword;
    CH         : array[0..19] of TPPI_CH_Registers; // PPI Channel
    RESERVED2  : array[0..147] of longword;
    CHG        : array[0..5] of longword; // Description collection[0]: Channel group 0
    RESERVED3  : array[0..61] of longword;
    FORK       : array[0..31] of TPPI_FORK_Registers; // Fork
  end;

  TMWU_Registers = record             // MWU Structure
    RESERVED0  : array[0..63] of longword;
    EVENTS_REGION : array[0..3] of TMWU_EVENTS_REGION_Registers; // Unspecified
    RESERVED1  : array[0..15] of longword;
    EVENTS_PREGION : array[0..1] of TMWU_EVENTS_PREGION_Registers; // Unspecified
    RESERVED2  : array[0..99] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..4] of longword;
    NMIEN      : longword;            // Enable or disable non-maskable interrupt
    NMIENSET   : longword;            // Enable non-maskable interrupt
    NMIENCLR   : longword;            // Disable non-maskable interrupt
    RESERVED4  : array[0..52] of longword;
    PERREGION  : array[0..1] of TMWU_PERREGION_Registers; // Unspecified
    RESERVED5  : array[0..63] of longword;
    REGIONEN   : longword;            // Enable/disable regions watch
    REGIONENSET : longword;           // Enable regions watch
    REGIONENCLR : longword;           // Disable regions watch
    RESERVED6  : array[0..56] of longword;
    REGION     : array[0..3] of TMWU_REGION_Registers; // Unspecified
    RESERVED7  : array[0..31] of longword;
    PREGION    : array[0..1] of TMWU_PREGION_Registers; // Unspecified
  end;

  TI2S_Registers = record             // I2S Structure
    TASKS_START : longword;           // Starts continuous I2S transfer. Also starts MCK generator when
    TASKS_STOP : longword;            // Stops I2S transfer. Also stops MCK generator. Triggering this
    RESERVED0  : array[0..62] of longword;
    EVENTS_RXPTRUPD : longword;       // The RXD.PTR register has been copied to internal double-buffers.
    EVENTS_STOPPED : longword;        // I2S transfer stopped.
    RESERVED1  : array[0..1] of longword;
    EVENTS_TXPTRUPD : longword;       // The TDX.PTR register has been copied to internal double-buffers.
    RESERVED2  : array[0..121] of longword;
    INTEN      : longword;            // Enable or disable interrupt
    INTENSET   : longword;            // Enable interrupt
    INTENCLR   : longword;            // Disable interrupt
    RESERVED3  : array[0..124] of longword;
    ENABLE     : longword;            // Enable I2S module.
    CONFIG     : TI2S_CONFIG_Registers; // Unspecified
    RESERVED4  : array[0..2] of longword;
    RXD        : TI2S_RXD_Registers;  // Unspecified
    RESERVED5  : longword;
    TXD        : TI2S_TXD_Registers;  // Unspecified
    RESERVED6  : array[0..2] of longword;
    RXTXD      : TI2S_RXTXD_Registers; // Unspecified
    RESERVED7  : array[0..2] of longword;
    PSEL       : TI2S_PSEL_Registers; // Unspecified
  end;

  TFPU_Registers = record             // FPU Structure
    UNUSED     : longword;            // Unused.
  end;

  TGPIO_Registers = record            // GPIO Structure
    RESERVED0  : array[0..320] of longword;
    OUT        : longword;            // Write GPIO port
    OUTSET     : longword;            // Set individual bits in GPIO port
    OUTCLR     : longword;            // Clear individual bits in GPIO port
    &IN        : longword;            // Read GPIO port
    DIR        : longword;            // Direction of GPIO pins
    DIRSET     : longword;            // DIR set register
    DIRCLR     : longword;            // DIR clear register
    LATCH      : longword;            // Latch register indicating what GPIO pins that have met the criteria
    DETECTMODE : longword;            // Select between default DETECT signal behaviour and LDETECT mode
    RESERVED1  : array[0..117] of longword;
    PIN_CNF    : array[0..31] of longword; // Description collection[0]: Configuration of GPIO pins
  end;

const
  FICR_BASE    = $10000000;
  UICR_BASE    = $10001000;
  BPROT_BASE   = $40000000;
  POWER_BASE   = $40000000;
  CLOCK_BASE   = $40000000;
  RADIO_BASE   = $40001000;
  UARTE0_BASE  = $40002000;
  UART0_BASE   = $40002000;
  SPIM0_BASE   = $40003000;
  SPIS0_BASE   = $40003000;
  TWIM0_BASE   = $40003000;
  TWIS0_BASE   = $40003000;
  SPI0_BASE    = $40003000;
  TWI0_BASE    = $40003000;
  SPIM1_BASE   = $40004000;
  SPIS1_BASE   = $40004000;
  TWIM1_BASE   = $40004000;
  TWIS1_BASE   = $40004000;
  SPI1_BASE    = $40004000;
  TWI1_BASE    = $40004000;
  NFCT_BASE    = $40005000;
  GPIOTE_BASE  = $40006000;
  SAADC_BASE   = $40007000;
  TIMER0_BASE  = $40008000;
  TIMER1_BASE  = $40009000;
  TIMER2_BASE  = $4000A000;
  RTC0_BASE    = $4000B000;
  TEMP_BASE    = $4000C000;
  RNG_BASE     = $4000D000;
  ECB_BASE     = $4000E000;
  CCM_BASE     = $4000F000;
  AAR_BASE     = $4000F000;
  WDT_BASE     = $40010000;
  RTC1_BASE    = $40011000;
  QDEC_BASE    = $40012000;
  COMP_BASE    = $40013000;
  LPCOMP_BASE  = $40013000;
  SWI0_BASE    = $40014000;
  EGU0_BASE    = $40014000;
  SWI1_BASE    = $40015000;
  EGU1_BASE    = $40015000;
  SWI2_BASE    = $40016000;
  EGU2_BASE    = $40016000;
  SWI3_BASE    = $40017000;
  EGU3_BASE    = $40017000;
  SWI4_BASE    = $40018000;
  EGU4_BASE    = $40018000;
  SWI5_BASE    = $40019000;
  EGU5_BASE    = $40019000;
  TIMER3_BASE  = $4001A000;
  TIMER4_BASE  = $4001B000;
  PWM0_BASE    = $4001C000;
  PDM_BASE     = $4001D000;
  NVMC_BASE    = $4001E000;
  PPI_BASE     = $4001F000;
  MWU_BASE     = $40020000;
  PWM1_BASE    = $40021000;
  PWM2_BASE    = $40022000;
  SPIM2_BASE   = $40023000;
  SPIS2_BASE   = $40023000;
  SPI2_BASE    = $40023000;
  RTC2_BASE    = $40024000;
  I2S_BASE     = $40025000;
  FPU_BASE     = $40026000;
  P0_BASE      = $50000000;

var
  FICR         : TFICR_Registers absolute FICR_BASE;
  UICR         : TUICR_Registers absolute UICR_BASE;
  BPROT        : TBPROT_Registers absolute BPROT_BASE;
  POWER        : TPOWER_Registers absolute POWER_BASE;
  CLOCK        : TCLOCK_Registers absolute CLOCK_BASE;
  RADIO        : TRADIO_Registers absolute RADIO_BASE;
  UARTE0       : TUARTE_Registers absolute UARTE0_BASE;
  UART0        : TUART_Registers absolute UART0_BASE;
  SPIM0        : TSPIM_Registers absolute SPIM0_BASE;
  SPIS0        : TSPIS_Registers absolute SPIS0_BASE;
  TWIM0        : TTWIM_Registers absolute TWIM0_BASE;
  TWIS0        : TTWIS_Registers absolute TWIS0_BASE;
  SPI0         : TSPI_Registers absolute SPI0_BASE;
  TWI0         : TTWI_Registers absolute TWI0_BASE;
  SPIM1        : TSPIM_Registers absolute SPIM1_BASE;
  SPIS1        : TSPIS_Registers absolute SPIS1_BASE;
  TWIM1        : TTWIM_Registers absolute TWIM1_BASE;
  TWIS1        : TTWIS_Registers absolute TWIS1_BASE;
  SPI1         : TSPI_Registers absolute SPI1_BASE;
  TWI1         : TTWI_Registers absolute TWI1_BASE;
  NFCT         : TNFCT_Registers absolute NFCT_BASE;
  GPIOTE       : TGPIOTE_Registers absolute GPIOTE_BASE;
  SAADC        : TSAADC_Registers absolute SAADC_BASE;
  TIMER0       : TTIMER_Registers absolute TIMER0_BASE;
  TIMER1       : TTIMER_Registers absolute TIMER1_BASE;
  TIMER2       : TTIMER_Registers absolute TIMER2_BASE;
  RTC0         : TRTC_Registers absolute RTC0_BASE;
  TEMP         : TTEMP_Registers absolute TEMP_BASE;
  RNG          : TRNG_Registers absolute RNG_BASE;
  ECB          : TECB_Registers absolute ECB_BASE;
  CCM          : TCCM_Registers absolute CCM_BASE;
  AAR          : TAAR_Registers absolute AAR_BASE;
  WDT          : TWDT_Registers absolute WDT_BASE;
  RTC1         : TRTC_Registers absolute RTC1_BASE;
  QDEC         : TQDEC_Registers absolute QDEC_BASE;
  COMP         : TCOMP_Registers absolute COMP_BASE;
  LPCOMP       : TLPCOMP_Registers absolute LPCOMP_BASE;
  SWI0         : TSWI_Registers absolute SWI0_BASE;
  EGU0         : TEGU_Registers absolute EGU0_BASE;
  SWI1         : TSWI_Registers absolute SWI1_BASE;
  EGU1         : TEGU_Registers absolute EGU1_BASE;
  SWI2         : TSWI_Registers absolute SWI2_BASE;
  EGU2         : TEGU_Registers absolute EGU2_BASE;
  SWI3         : TSWI_Registers absolute SWI3_BASE;
  EGU3         : TEGU_Registers absolute EGU3_BASE;
  SWI4         : TSWI_Registers absolute SWI4_BASE;
  EGU4         : TEGU_Registers absolute EGU4_BASE;
  SWI5         : TSWI_Registers absolute SWI5_BASE;
  EGU5         : TEGU_Registers absolute EGU5_BASE;
  TIMER3       : TTIMER_Registers absolute TIMER3_BASE;
  TIMER4       : TTIMER_Registers absolute TIMER4_BASE;
  PWM0         : TPWM_Registers absolute PWM0_BASE;
  PDM          : TPDM_Registers absolute PDM_BASE;
  NVMC         : TNVMC_Registers absolute NVMC_BASE;
  PPI          : TPPI_Registers absolute PPI_BASE;
  MWU          : TMWU_Registers absolute MWU_BASE;
  PWM1         : TPWM_Registers absolute PWM1_BASE;
  PWM2         : TPWM_Registers absolute PWM2_BASE;
  SPIM2        : TSPIM_Registers absolute SPIM2_BASE;
  SPIS2        : TSPIS_Registers absolute SPIS2_BASE;
  SPI2         : TSPI_Registers absolute SPI2_BASE;
  RTC2         : TRTC_Registers absolute RTC2_BASE;
  I2S          : TI2S_Registers absolute I2S_BASE;
  FPU          : TFPU_Registers absolute FPU_BASE;
  P0           : TGPIO_Registers absolute P0_BASE;

implementation

procedure Reset_interrupt; external name 'Reset_interrupt';
procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
procedure HardFault_interrupt; external name 'HardFault_interrupt';
procedure MemoryManagement_interrupt; external name 'MemoryManagement_interrupt';
procedure BusFault_interrupt; external name 'BusFault_interrupt';
procedure UsageFault_interrupt; external name 'UsageFault_interrupt';
procedure SVCall_interrupt; external name 'SVCall_interrupt';
procedure DebugMonitor_interrupt; external name 'DebugMonitor_interrupt';
procedure PendSV_interrupt; external name 'PendSV_interrupt';
procedure SysTick_interrupt; external name 'SysTick_interrupt';
procedure POWER_CLOCK_interrupt; external name 'POWER_CLOCK_interrupt';
procedure RADIO_interrupt; external name 'RADIO_interrupt';
procedure UARTE0_UART0_interrupt; external name 'UARTE0_UART0_interrupt';
procedure NFCT_interrupt; external name 'NFCT_interrupt';
procedure GPIOTE_interrupt; external name 'GPIOTE_interrupt';
procedure SAADC_interrupt; external name 'SAADC_interrupt';
procedure TIMER0_interrupt; external name 'TIMER0_interrupt';
procedure TIMER1_interrupt; external name 'TIMER1_interrupt';
procedure TIMER2_interrupt; external name 'TIMER2_interrupt';
procedure RTC0_interrupt; external name 'RTC0_interrupt';
procedure TEMP_interrupt; external name 'TEMP_interrupt';
procedure RNG_interrupt; external name 'RNG_interrupt';
procedure ECB_interrupt; external name 'ECB_interrupt';
procedure CCM_AAR_interrupt; external name 'CCM_AAR_interrupt';
procedure WDT_interrupt; external name 'WDT_interrupt';
procedure RTC1_interrupt; external name 'RTC1_interrupt';
procedure QDEC_interrupt; external name 'QDEC_interrupt';
procedure COMP_LPCOMP_interrupt; external name 'COMP_LPCOMP_interrupt';
procedure SWI0_EGU0_interrupt; external name 'SWI0_EGU0_interrupt';
procedure SWI1_EGU1_interrupt; external name 'SWI1_EGU1_interrupt';
procedure SWI2_EGU2_interrupt; external name 'SWI2_EGU2_interrupt';
procedure SWI3_EGU3_interrupt; external name 'SWI3_EGU3_interrupt';
procedure SWI4_EGU4_interrupt; external name 'SWI4_EGU4_interrupt';
procedure SWI5_EGU5_interrupt; external name 'SWI5_EGU5_interrupt';
procedure TIMER3_interrupt; external name 'TIMER3_interrupt';
procedure TIMER4_interrupt; external name 'TIMER4_interrupt';
procedure PWM0_interrupt; external name 'PWM0_interrupt';
procedure PDM_interrupt; external name 'PDM_interrupt';
procedure MWU_interrupt; external name 'MWU_interrupt';
procedure PWM1_interrupt; external name 'PWM1_interrupt';
procedure PWM2_interrupt; external name 'PWM2_interrupt';
procedure SPIM2_SPIS2_SPI2_interrupt; external name 'SPIM2_SPIS2_SPI2_interrupt';
procedure RTC2_interrupt; external name 'RTC2_interrupt';
procedure I2S_interrupt; external name 'I2S_interrupt';
procedure FPU_interrupt; external name 'FPU_interrupt';

{$i cortexm4f_start.inc}

procedure Vectors; assembler; nostackframe;
label interrupt_vectors;
asm
  .section ".init.interrupt_vectors"
  interrupt_vectors:
  .long _stack_top
  .long Startup
  .long Reset_interrupt
  .long NonMaskableInt_interrupt
  .long HardFault_interrupt
  .long MemoryManagement_interrupt
  .long BusFault_interrupt
  .long UsageFault_interrupt
  .long 0
  .long 0
  .long 0
  .long SVCall_interrupt
  .long DebugMonitor_interrupt
  .long 0
  .long PendSV_interrupt
  .long SysTick_interrupt
  .long POWER_CLOCK_interrupt
  .long RADIO_interrupt
  .long UARTE0_UART0_interrupt
  .long 0
  .long 0
  .long NFCT_interrupt
  .long GPIOTE_interrupt
  .long SAADC_interrupt
  .long TIMER0_interrupt
  .long TIMER1_interrupt
  .long TIMER2_interrupt
  .long RTC0_interrupt
  .long TEMP_interrupt
  .long RNG_interrupt
  .long ECB_interrupt
  .long CCM_AAR_interrupt
  .long WDT_interrupt
  .long RTC1_interrupt
  .long QDEC_interrupt
  .long COMP_LPCOMP_interrupt
  .long SWI0_EGU0_interrupt
  .long SWI1_EGU1_interrupt
  .long SWI2_EGU2_interrupt
  .long SWI3_EGU3_interrupt
  .long SWI4_EGU4_interrupt
  .long SWI5_EGU5_interrupt
  .long TIMER3_interrupt
  .long TIMER4_interrupt
  .long PWM0_interrupt
  .long PDM_interrupt
  .long 0
  .long 0
  .long MWU_interrupt
  .long PWM1_interrupt
  .long PWM2_interrupt
  .long SPIM2_SPIS2_SPI2_interrupt
  .long RTC2_interrupt
  .long I2S_interrupt
  .long FPU_interrupt
  .weak Reset_interrupt
  .weak NonMaskableInt_interrupt
  .weak HardFault_interrupt
  .weak MemoryManagement_interrupt
  .weak BusFault_interrupt
  .weak UsageFault_interrupt
  .weak SVCall_interrupt
  .weak DebugMonitor_interrupt
  .weak PendSV_interrupt
  .weak SysTick_interrupt
  .weak POWER_CLOCK_interrupt
  .weak RADIO_interrupt
  .weak UARTE0_UART0_interrupt
  .weak NFCT_interrupt
  .weak GPIOTE_interrupt
  .weak SAADC_interrupt
  .weak TIMER0_interrupt
  .weak TIMER1_interrupt
  .weak TIMER2_interrupt
  .weak RTC0_interrupt
  .weak TEMP_interrupt
  .weak RNG_interrupt
  .weak ECB_interrupt
  .weak CCM_AAR_interrupt
  .weak WDT_interrupt
  .weak RTC1_interrupt
  .weak QDEC_interrupt
  .weak COMP_LPCOMP_interrupt
  .weak SWI0_EGU0_interrupt
  .weak SWI1_EGU1_interrupt
  .weak SWI2_EGU2_interrupt
  .weak SWI3_EGU3_interrupt
  .weak SWI4_EGU4_interrupt
  .weak SWI5_EGU5_interrupt
  .weak TIMER3_interrupt
  .weak TIMER4_interrupt
  .weak PWM0_interrupt
  .weak PDM_interrupt
  .weak MWU_interrupt
  .weak PWM1_interrupt
  .weak PWM2_interrupt
  .weak SPIM2_SPIS2_SPI2_interrupt
  .weak RTC2_interrupt
  .weak I2S_interrupt
  .weak FPU_interrupt
  .set Reset_interrupt, HaltProc
  .set NonMaskableInt_interrupt, HaltProc
  .set HardFault_interrupt, HaltProc
  .set MemoryManagement_interrupt, HaltProc
  .set BusFault_interrupt, HaltProc
  .set UsageFault_interrupt, HaltProc
  .set SVCall_interrupt, HaltProc
  .set DebugMonitor_interrupt, HaltProc
  .set PendSV_interrupt, HaltProc
  .set SysTick_interrupt, HaltProc
  .set POWER_CLOCK_interrupt, HaltProc
  .set RADIO_interrupt, HaltProc
  .set UARTE0_UART0_interrupt, HaltProc
  .set NFCT_interrupt, HaltProc
  .set GPIOTE_interrupt, HaltProc
  .set SAADC_interrupt, HaltProc
  .set TIMER0_interrupt, HaltProc
  .set TIMER1_interrupt, HaltProc
  .set TIMER2_interrupt, HaltProc
  .set RTC0_interrupt, HaltProc
  .set TEMP_interrupt, HaltProc
  .set RNG_interrupt, HaltProc
  .set ECB_interrupt, HaltProc
  .set CCM_AAR_interrupt, HaltProc
  .set WDT_interrupt, HaltProc
  .set RTC1_interrupt, HaltProc
  .set QDEC_interrupt, HaltProc
  .set COMP_LPCOMP_interrupt, HaltProc
  .set SWI0_EGU0_interrupt, HaltProc
  .set SWI1_EGU1_interrupt, HaltProc
  .set SWI2_EGU2_interrupt, HaltProc
  .set SWI3_EGU3_interrupt, HaltProc
  .set SWI4_EGU4_interrupt, HaltProc
  .set SWI5_EGU5_interrupt, HaltProc
  .set TIMER3_interrupt, HaltProc
  .set TIMER4_interrupt, HaltProc
  .set PWM0_interrupt, HaltProc
  .set PDM_interrupt, HaltProc
  .set MWU_interrupt, HaltProc
  .set PWM1_interrupt, HaltProc
  .set PWM2_interrupt, HaltProc
  .set SPIM2_SPIS2_SPI2_interrupt, HaltProc
  .set RTC2_interrupt, HaltProc
  .set I2S_interrupt, HaltProc
  .set FPU_interrupt, HaltProc
  .text
end;
end.
