unit nrf51;
interface
{$PACKRECORDS 2}
{$GOTO ON}
{$MODESWITCH ADVANCEDRECORDS}
// *****************************************************************************************************
// * @file     nrf51.h
//    CMSIS Cortex-M0 Peripheral Access Layer Header File for
// *           nrf51 from Nordic Semiconductor.
// *
// * @version  V522
// * @date     18. November 2016
// *
// * @note     Generated with SVDConv V2.81d
// *           from CMSIS SVD File 'nrf51.svd' Version 522,
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
    SVCall_IRQn = -5,                 //  11  System Service Call via SVC instruction
    DebugMonitor_IRQn = -4,           //  12  Debug Monitor
    PendSV_IRQn = -2,                 //  14  Pendable request for system service
    SysTick_IRQn = -1,                //  15  System Tick Timer
    POWER_CLOCK_IRQn = 0,             //   0  POWER_CLOCK
    RADIO_IRQn = 1,                   //   1  RADIO
    UART0_IRQn = 2,                   //   2  UART0
    SPI0_TWI0_IRQn = 3,               //   3  SPI0_TWI0
    SPI1_TWI1_IRQn = 4,               //   4  SPI1_TWI1
    GPIOTE_IRQn = 6,                  //   6  GPIOTE
    ADC_IRQn   = 7,                   //   7  ADC
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
    LPCOMP_IRQn = 19,                 //  19  LPCOMP
    SWI0_IRQn  = 20,                  //  20  SWI0
    SWI1_IRQn  = 21,                  //  21  SWI1
    SWI2_IRQn  = 22,                  //  22  SWI2
    SWI3_IRQn  = 23,                  //  23  SWI3
    SWI4_IRQn  = 24,                  //  24  SWI4
    SWI5_IRQn  = 25                   //  25  SWI5
  );

  TPPI_TASKS_CHG_Registers = record
    EN         : longword;            // Enable channel group.
    DIS        : longword;            // Disable channel group.
  end;

  TPPI_CH_Registers = record
    EEP        : longword;            // Channel event end-point.
    TEP        : longword;            // Channel task end-point.
  end;

  TPOWER_Registers = record           // POWER Structure
    RESERVED0  : array[0..29] of longword;
    TASKS_CONSTLAT : longword;        // Enable constant latency mode.
    TASKS_LOWPWR : longword;          // Enable low power mode (variable latency).
    RESERVED1  : array[0..33] of longword;
    EVENTS_POFWARN : longword;        // Power failure warning.
    RESERVED2  : array[0..125] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED3  : array[0..60] of longword;
    RESETREAS  : longword;            // Reset reason.
    RESERVED4  : array[0..8] of longword;
    RAMSTATUS  : longword;            // Ram status register.
    RESERVED5  : array[0..52] of longword;
    SYSTEMOFF  : longword;            // System off register.
    RESERVED6  : array[0..2] of longword;
    POFCON     : longword;            // Power failure configuration.
    RESERVED7  : array[0..1] of longword;
    GPREGRET   : longword;            // General purpose retention register. This register is a retained
    RESERVED8  : longword;
    RAMON      : longword;            // Ram on/off.
    RESERVED9  : array[0..6] of longword;
    RESET      : longword;            // Pin reset functionality configuration register. This register
    RESERVED10 : array[0..2] of longword;
    RAMONB     : longword;            // Ram on/off.
    RESERVED11 : array[0..7] of longword;
    DCDCEN     : longword;            // DCDC converter enable configuration register.
    RESERVED12 : array[0..290] of longword;
    DCDCFORCE  : longword;            // DCDC power-up force register.
  end;

  TCLOCK_Registers = record           // CLOCK Structure
    TASKS_HFCLKSTART : longword;      // Start HFCLK clock source.
    TASKS_HFCLKSTOP : longword;       // Stop HFCLK clock source.
    TASKS_LFCLKSTART : longword;      // Start LFCLK clock source.
    TASKS_LFCLKSTOP : longword;       // Stop LFCLK clock source.
    TASKS_CAL  : longword;            // Start calibration of LFCLK RC oscillator.
    TASKS_CTSTART : longword;         // Start calibration timer.
    TASKS_CTSTOP : longword;          // Stop calibration timer.
    RESERVED0  : array[0..56] of longword;
    EVENTS_HFCLKSTARTED : longword;   // HFCLK oscillator started.
    EVENTS_LFCLKSTARTED : longword;   // LFCLK oscillator started.
    RESERVED1  : longword;
    EVENTS_DONE : longword;           // Calibration of LFCLK RC oscillator completed.
    EVENTS_CTTO : longword;           // Calibration timer timeout.
    RESERVED2  : array[0..123] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED3  : array[0..62] of longword;
    HFCLKRUN   : longword;            // Task HFCLKSTART trigger status.
    HFCLKSTAT  : longword;            // High frequency clock status.
    RESERVED4  : longword;
    LFCLKRUN   : longword;            // Task LFCLKSTART triggered status.
    LFCLKSTAT  : longword;            // Low frequency clock status.
    LFCLKSRCCOPY : longword;          // Clock source for the LFCLK clock, set when task LKCLKSTART is
    RESERVED5  : array[0..61] of longword;
    LFCLKSRC   : longword;            // Clock source for the LFCLK clock.
    RESERVED6  : array[0..6] of longword;
    CTIV       : longword;            // Calibration timer interval.
    RESERVED7  : array[0..4] of longword;
    XTALFREQ   : longword;            // Crystal frequency.
  end;

  TMPU_Registers = record             // MPU Structure
    RESERVED0  : array[0..329] of longword;
    PERR0      : longword;            // Configuration of peripherals in mpu regions.
    RLENR0     : longword;            // Length of RAM region 0.
    RESERVED1  : array[0..51] of longword;
    PROTENSET0 : longword;            // Erase and write protection bit enable set register.
    PROTENSET1 : longword;            // Erase and write protection bit enable set register.
    DISABLEINDEBUG : longword;        // Disable erase and write protection mechanism in debug mode.
    PROTBLOCKSIZE : longword;         // Erase and write protection block size.
  end;

  TRADIO_Registers = record           // RADIO Structure
    TASKS_TXEN : longword;            // Enable radio in TX mode.
    TASKS_RXEN : longword;            // Enable radio in RX mode.
    TASKS_START : longword;           // Start radio.
    TASKS_STOP : longword;            // Stop radio.
    TASKS_DISABLE : longword;         // Disable radio.
    TASKS_RSSISTART : longword;       // Start the RSSI and take one sample of the receive signal strength.
    TASKS_RSSISTOP : longword;        // Stop the RSSI measurement.
    TASKS_BCSTART : longword;         // Start the bit counter.
    TASKS_BCSTOP : longword;          // Stop the bit counter.
    RESERVED0  : array[0..54] of longword;
    EVENTS_READY : longword;          // Ready event.
    EVENTS_ADDRESS : longword;        // Address event.
    EVENTS_PAYLOAD : longword;        // Payload event.
    EVENTS_END : longword;            // End event.
    EVENTS_DISABLED : longword;       // Disable event.
    EVENTS_DEVMATCH : longword;       // A device address match occurred on the last received packet.
    EVENTS_DEVMISS : longword;        // No device address match occurred on the last received packet.
    EVENTS_RSSIEND : longword;        // Sampling of the receive signal strength complete. A new RSSI
    RESERVED1  : array[0..1] of longword;
    EVENTS_BCMATCH : longword;        // Bit counter reached bit count value specified in BCC register.
    RESERVED2  : array[0..52] of longword;
    SHORTS     : longword;            // Shortcuts for the radio.
    RESERVED3  : array[0..63] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED4  : array[0..60] of longword;
    CRCSTATUS  : longword;            // CRC status of received packet.
    RESERVED5  : longword;
    RXMATCH    : longword;            // Received address.
    RXCRC      : longword;            // Received CRC.
    DAI        : longword;            // Device address match index.
    RESERVED6  : array[0..59] of longword;
    PACKETPTR  : longword;            // Packet pointer. Decision point: START task.
    FREQUENCY  : longword;            // Frequency.
    TXPOWER    : longword;            // Output power.
    MODE       : longword;            // Data rate and modulation.
    PCNF0      : longword;            // Packet configuration 0.
    PCNF1      : longword;            // Packet configuration 1.
    BASE0      : longword;            // Radio base address 0. Decision point: START task.
    BASE1      : longword;            // Radio base address 1. Decision point: START task.
    PREFIX0    : longword;            // Prefixes bytes for logical addresses 0 to 3.
    PREFIX1    : longword;            // Prefixes bytes for logical addresses 4 to 7.
    TXADDRESS  : longword;            // Transmit address select.
    RXADDRESSES : longword;           // Receive address select.
    CRCCNF     : longword;            // CRC configuration.
    CRCPOLY    : longword;            // CRC polynomial.
    CRCINIT    : longword;            // CRC initial value.
    TEST       : longword;            // Test features enable register.
    TIFS       : longword;            // Inter Frame Spacing in microseconds.
    RSSISAMPLE : longword;            // RSSI sample.
    RESERVED7  : longword;
    STATE      : longword;            // Current radio state.
    DATAWHITEIV : longword;           // Data whitening initial value.
    RESERVED8  : array[0..1] of longword;
    BCC        : longword;            // Bit counter compare.
    RESERVED9  : array[0..38] of longword;
    DAB        : array[0..7] of longword; // Device address base segment.
    DAP        : array[0..7] of longword; // Device address prefix.
    DACNF      : longword;            // Device address match configuration.
    RESERVED10 : array[0..55] of longword;
    OVERRIDE0  : longword;            // Trim value override register 0.
    OVERRIDE1  : longword;            // Trim value override register 1.
    OVERRIDE2  : longword;            // Trim value override register 2.
    OVERRIDE3  : longword;            // Trim value override register 3.
    OVERRIDE4  : longword;            // Trim value override register 4.
    RESERVED11 : array[0..560] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TUART_Registers = record            // UART Structure
    TASKS_STARTRX : longword;         // Start UART receiver.
    TASKS_STOPRX : longword;          // Stop UART receiver.
    TASKS_STARTTX : longword;         // Start UART transmitter.
    TASKS_STOPTX : longword;          // Stop UART transmitter.
    RESERVED0  : array[0..2] of longword;
    TASKS_SUSPEND : longword;         // Suspend UART.
    RESERVED1  : array[0..55] of longword;
    EVENTS_CTS : longword;            // CTS activated.
    EVENTS_NCTS : longword;           // CTS deactivated.
    EVENTS_RXDRDY : longword;         // Data received in RXD.
    RESERVED2  : array[0..3] of longword;
    EVENTS_TXDRDY : longword;         // Data sent from TXD.
    RESERVED3  : longword;
    EVENTS_ERROR : longword;          // Error detected.
    RESERVED4  : array[0..6] of longword;
    EVENTS_RXTO : longword;           // Receiver timeout.
    RESERVED5  : array[0..45] of longword;
    SHORTS     : longword;            // Shortcuts for UART.
    RESERVED6  : array[0..63] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED7  : array[0..92] of longword;
    ERRORSRC   : longword;            // Error source. Write error field to 1 to clear error.
    RESERVED8  : array[0..30] of longword;
    ENABLE     : longword;            // Enable UART and acquire IOs.
    RESERVED9  : longword;
    PSELRTS    : longword;            // Pin select for RTS.
    PSELTXD    : longword;            // Pin select for TXD.
    PSELCTS    : longword;            // Pin select for CTS.
    PSELRXD    : longword;            // Pin select for RXD.
    RXD        : longword;            // RXD register. On read action the buffer pointer is displaced.
    TXD        : longword;            // TXD register.
    RESERVED10 : longword;
    BAUDRATE   : longword;            // UART Baudrate.
    RESERVED11 : array[0..16] of longword;
    CONFIG     : longword;            // Configuration of parity and hardware flow control register.
    RESERVED12 : array[0..674] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TSPI_Registers = record             // SPI Structure
    RESERVED0  : array[0..65] of longword;
    EVENTS_READY : longword;          // TXD byte sent and RXD byte received.
    RESERVED1  : array[0..125] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED2  : array[0..124] of longword;
    ENABLE     : longword;            // Enable SPI.
    RESERVED3  : longword;
    PSELSCK    : longword;            // Pin select for SCK.
    PSELMOSI   : longword;            // Pin select for MOSI.
    PSELMISO   : longword;            // Pin select for MISO.
    RESERVED4  : longword;
    RXD        : longword;            // RX data.
    TXD        : longword;            // TX data.
    RESERVED5  : longword;
    FREQUENCY  : longword;            // SPI frequency
    RESERVED6  : array[0..10] of longword;
    CONFIG     : longword;            // Configuration register.
    RESERVED7  : array[0..680] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TTWI_Registers = record             // TWI Structure
    TASKS_STARTRX : longword;         // Start 2-Wire master receive sequence.
    RESERVED0  : longword;
    TASKS_STARTTX : longword;         // Start 2-Wire master transmit sequence.
    RESERVED1  : array[0..1] of longword;
    TASKS_STOP : longword;            // Stop 2-Wire transaction.
    RESERVED2  : longword;
    TASKS_SUSPEND : longword;         // Suspend 2-Wire transaction.
    TASKS_RESUME : longword;          // Resume 2-Wire transaction.
    RESERVED3  : array[0..55] of longword;
    EVENTS_STOPPED : longword;        // Two-wire stopped.
    EVENTS_RXDREADY : longword;       // Two-wire ready to deliver new RXD byte received.
    RESERVED4  : array[0..3] of longword;
    EVENTS_TXDSENT : longword;        // Two-wire finished sending last TXD byte.
    RESERVED5  : longword;
    EVENTS_ERROR : longword;          // Two-wire error detected.
    RESERVED6  : array[0..3] of longword;
    EVENTS_BB  : longword;            // Two-wire byte boundary.
    RESERVED7  : array[0..2] of longword;
    EVENTS_SUSPENDED : longword;      // Two-wire suspended.
    RESERVED8  : array[0..44] of longword;
    SHORTS     : longword;            // Shortcuts for TWI.
    RESERVED9  : array[0..63] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED10 : array[0..109] of longword;
    ERRORSRC   : longword;            // Two-wire error source. Write error field to 1 to clear error.
    RESERVED11 : array[0..13] of longword;
    ENABLE     : longword;            // Enable two-wire master.
    RESERVED12 : longword;
    PSELSCL    : longword;            // Pin select for SCL.
    PSELSDA    : longword;            // Pin select for SDA.
    RESERVED13 : array[0..1] of longword;
    RXD        : longword;            // RX data register.
    TXD        : longword;            // TX data register.
    RESERVED14 : longword;
    FREQUENCY  : longword;            // Two-wire frequency.
    RESERVED15 : array[0..23] of longword;
    ADDRESS    : longword;            // Address used in the two-wire transfer.
    RESERVED16 : array[0..667] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TSPIS_Registers = record            // SPIS Structure
    RESERVED0  : array[0..8] of longword;
    TASKS_ACQUIRE : longword;         // Acquire SPI semaphore.
    TASKS_RELEASE : longword;         // Release SPI semaphore.
    RESERVED1  : array[0..53] of longword;
    EVENTS_END : longword;            // Granted transaction completed.
    RESERVED2  : array[0..1] of longword;
    EVENTS_ENDRX : longword;          // End of RXD buffer reached
    RESERVED3  : array[0..4] of longword;
    EVENTS_ACQUIRED : longword;       // Semaphore acquired.
    RESERVED4  : array[0..52] of longword;
    SHORTS     : longword;            // Shortcuts for SPIS.
    RESERVED5  : array[0..63] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED6  : array[0..60] of longword;
    SEMSTAT    : longword;            // Semaphore status.
    RESERVED7  : array[0..14] of longword;
    STATUS     : longword;            // Status from last transaction.
    RESERVED8  : array[0..46] of longword;
    ENABLE     : longword;            // Enable SPIS.
    RESERVED9  : longword;
    PSELSCK    : longword;            // Pin select for SCK.
    PSELMISO   : longword;            // Pin select for MISO.
    PSELMOSI   : longword;            // Pin select for MOSI.
    PSELCSN    : longword;            // Pin select for CSN.
    RESERVED10 : array[0..6] of longword;
    RXDPTR     : longword;            // RX data pointer.
    MAXRX      : longword;            // Maximum number of bytes in the receive buffer.
    AMOUNTRX   : longword;            // Number of bytes received in last granted transaction.
    RESERVED11 : longword;
    TXDPTR     : longword;            // TX data pointer.
    MAXTX      : longword;            // Maximum number of bytes in the transmit buffer.
    AMOUNTTX   : longword;            // Number of bytes transmitted in last granted transaction.
    RESERVED12 : longword;
    CONFIG     : longword;            // Configuration register.
    RESERVED13 : longword;
    DEF        : longword;            // Default character.
    RESERVED14 : array[0..23] of longword;
    ORC        : longword;            // Over-read character.
    RESERVED15 : array[0..653] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TGPIOTE_Registers = record          // GPIOTE Structure
    TASKS_OUT  : array[0..3] of longword; // Tasks asssociated with GPIOTE channels.
    RESERVED0  : array[0..59] of longword;
    EVENTS_IN  : array[0..3] of longword; // Tasks asssociated with GPIOTE channels.
    RESERVED1  : array[0..26] of longword;
    EVENTS_PORT : longword;           // Event generated from multiple pins.
    RESERVED2  : array[0..96] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED3  : array[0..128] of longword;
    CONFIG     : array[0..3] of longword; // Channel configuration registers.
    RESERVED4  : array[0..694] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TADC_Registers = record             // ADC Structure
    TASKS_START : longword;           // Start an ADC conversion.
    TASKS_STOP : longword;            // Stop ADC.
    RESERVED0  : array[0..61] of longword;
    EVENTS_END : longword;            // ADC conversion complete.
    RESERVED1  : array[0..127] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED2  : array[0..60] of longword;
    BUSY       : longword;            // ADC busy register.
    RESERVED3  : array[0..62] of longword;
    ENABLE     : longword;            // ADC enable.
    CONFIG     : longword;            // ADC configuration register.
    RESULT     : longword;            // Result of ADC conversion.
    RESERVED4  : array[0..699] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TTIMER_Registers = record           // TIMER Structure
    TASKS_START : longword;           // Start Timer.
    TASKS_STOP : longword;            // Stop Timer.
    TASKS_COUNT : longword;           // Increment Timer (In counter mode).
    TASKS_CLEAR : longword;           // Clear timer.
    TASKS_SHUTDOWN : longword;        // Shutdown timer.
    RESERVED0  : array[0..10] of longword;
    TASKS_CAPTURE : array[0..3] of longword; // Capture Timer value to CC[n] registers.
    RESERVED1  : array[0..59] of longword;
    EVENTS_COMPARE : array[0..3] of longword; // Compare event on CC[n] match.
    RESERVED2  : array[0..43] of longword;
    SHORTS     : longword;            // Shortcuts for Timer.
    RESERVED3  : array[0..63] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED4  : array[0..125] of longword;
    MODE       : longword;            // Timer Mode selection.
    BITMODE    : longword;            // Sets timer behaviour.
    RESERVED5  : longword;
    PRESCALER  : longword;            // 4-bit prescaler to source clock frequency (max value 9). Source
    RESERVED6  : array[0..10] of longword;
    CC         : array[0..3] of longword; // Capture/compare registers.
    RESERVED7  : array[0..682] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TRTC_Registers = record             // RTC Structure
    TASKS_START : longword;           // Start RTC Counter.
    TASKS_STOP : longword;            // Stop RTC Counter.
    TASKS_CLEAR : longword;           // Clear RTC Counter.
    TASKS_TRIGOVRFLW : longword;      // Set COUNTER to 0xFFFFFFF0.
    RESERVED0  : array[0..59] of longword;
    EVENTS_TICK : longword;           // Event on COUNTER increment.
    EVENTS_OVRFLW : longword;         // Event on COUNTER overflow.
    RESERVED1  : array[0..13] of longword;
    EVENTS_COMPARE : array[0..3] of longword; // Compare event on CC[n] match.
    RESERVED2  : array[0..108] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED3  : array[0..12] of longword;
    EVTEN      : longword;            // Configures event enable routing to PPI for each RTC event.
    EVTENSET   : longword;            // Enable events routing to PPI. The reading of this register gives
    EVTENCLR   : longword;            // Disable events routing to PPI. The reading of this register
    RESERVED4  : array[0..109] of longword;
    COUNTER    : longword;            // Current COUNTER value.
    PRESCALER  : longword;            // 12-bit prescaler for COUNTER frequency (32768/(PRESCALER+1)).
    RESERVED5  : array[0..12] of longword;
    CC         : array[0..3] of longword; // Capture/compare registers.
    RESERVED6  : array[0..682] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TTEMP_Registers = record            // TEMP Structure
    TASKS_START : longword;           // Start temperature measurement.
    TASKS_STOP : longword;            // Stop temperature measurement.
    RESERVED0  : array[0..61] of longword;
    EVENTS_DATARDY : longword;        // Temperature measurement complete, data ready event.
    RESERVED1  : array[0..127] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED2  : array[0..126] of longword;
    TEMP       : longint;             // Die temperature in degC, 2's complement format, 0.25 degC pecision.
    RESERVED3  : array[0..699] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TRNG_Registers = record             // RNG Structure
    TASKS_START : longword;           // Start the random number generator.
    TASKS_STOP : longword;            // Stop the random number generator.
    RESERVED0  : array[0..61] of longword;
    EVENTS_VALRDY : longword;         // New random number generated and written to VALUE register.
    RESERVED1  : array[0..62] of longword;
    SHORTS     : longword;            // Shortcuts for the RNG.
    RESERVED2  : array[0..63] of longword;
    INTENSET   : longword;            // Interrupt enable set register
    INTENCLR   : longword;            // Interrupt enable clear register
    RESERVED3  : array[0..125] of longword;
    CONFIG     : longword;            // Configuration register.
    VALUE      : longword;            // RNG random number.
    RESERVED4  : array[0..699] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TECB_Registers = record             // ECB Structure
    TASKS_STARTECB : longword;        // Start ECB block encrypt. If a crypto operation is running, this
    TASKS_STOPECB : longword;         // Stop current ECB encryption. If a crypto operation is running,
    RESERVED0  : array[0..61] of longword;
    EVENTS_ENDECB : longword;         // ECB block encrypt complete.
    EVENTS_ERRORECB : longword;       // ECB block encrypt aborted due to a STOPECB task or due to an
    RESERVED1  : array[0..126] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED2  : array[0..125] of longword;
    ECBDATAPTR : longword;            // ECB block encrypt memory pointer.
    RESERVED3  : array[0..700] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TAAR_Registers = record             // AAR Structure
    TASKS_START : longword;           // Start resolving addresses based on IRKs specified in the IRK
    RESERVED0  : longword;
    TASKS_STOP : longword;            // Stop resolving addresses.
    RESERVED1  : array[0..60] of longword;
    EVENTS_END : longword;            // Address resolution procedure completed.
    EVENTS_RESOLVED : longword;       // Address resolved.
    EVENTS_NOTRESOLVED : longword;    // Address not resolved.
    RESERVED2  : array[0..125] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED3  : array[0..60] of longword;
    STATUS     : longword;            // Resolution status.
    RESERVED4  : array[0..62] of longword;
    ENABLE     : longword;            // Enable AAR.
    NIRK       : longword;            // Number of Identity root Keys in the IRK data structure.
    IRKPTR     : longword;            // Pointer to the IRK data structure.
    RESERVED5  : longword;
    ADDRPTR    : longword;            // Pointer to the resolvable address (6 bytes).
    SCRATCHPTR : longword;            // Pointer to a scratch data area used for temporary storage during
    RESERVED6  : array[0..696] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TCCM_Registers = record             // CCM Structure
    TASKS_KSGEN : longword;           // Start generation of key-stream. This operation will stop by
    TASKS_CRYPT : longword;           // Start encrypt/decrypt. This operation will stop by itself when
    TASKS_STOP : longword;            // Stop encrypt/decrypt.
    RESERVED0  : array[0..60] of longword;
    EVENTS_ENDKSGEN : longword;       // Keystream generation completed.
    EVENTS_ENDCRYPT : longword;       // Encrypt/decrypt completed.
    EVENTS_ERROR : longword;          // Error happened.
    RESERVED1  : array[0..60] of longword;
    SHORTS     : longword;            // Shortcuts for the CCM.
    RESERVED2  : array[0..63] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED3  : array[0..60] of longword;
    MICSTATUS  : longword;            // CCM RX MIC check result.
    RESERVED4  : array[0..62] of longword;
    ENABLE     : longword;            // CCM enable.
    MODE       : longword;            // Operation mode.
    CNFPTR     : longword;            // Pointer to a data structure holding AES key and NONCE vector.
    INPTR      : longword;            // Pointer to the input packet.
    OUTPTR     : longword;            // Pointer to the output packet.
    SCRATCHPTR : longword;            // Pointer to a scratch data area used for temporary storage during
    RESERVED5  : array[0..696] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TWDT_Registers = record             // WDT Structure
    TASKS_START : longword;           // Start the watchdog.
    RESERVED0  : array[0..62] of longword;
    EVENTS_TIMEOUT : longword;        // Watchdog timeout.
    RESERVED1  : array[0..127] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED2  : array[0..60] of longword;
    RUNSTATUS  : longword;            // Watchdog running status.
    REQSTATUS  : longword;            // Request status.
    RESERVED3  : array[0..62] of longword;
    CRV        : longword;            // Counter reload value in number of 32kiHz clock cycles.
    RREN       : longword;            // Reload request enable.
    CONFIG     : longword;            // Configuration register.
    RESERVED4  : array[0..59] of longword;
    RR         : array[0..7] of longword; // Reload requests registers.
    RESERVED5  : array[0..630] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TQDEC_Registers = record            // QDEC Structure
    TASKS_START : longword;           // Start the quadrature decoder.
    TASKS_STOP : longword;            // Stop the quadrature decoder.
    TASKS_READCLRACC : longword;      // Transfers the content from ACC registers to ACCREAD registers,
    RESERVED0  : array[0..60] of longword;
    EVENTS_SAMPLERDY : longword;      // A new sample is written to the sample register.
    EVENTS_REPORTRDY : longword;      // REPORTPER number of samples accumulated in ACC register, and
    EVENTS_ACCOF : longword;          // ACC or ACCDBL register overflow.
    RESERVED1  : array[0..60] of longword;
    SHORTS     : longword;            // Shortcuts for the QDEC.
    RESERVED2  : array[0..63] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED3  : array[0..124] of longword;
    ENABLE     : longword;            // Enable the QDEC.
    LEDPOL     : longword;            // LED output pin polarity.
    SAMPLEPER  : longword;            // Sample period.
    SAMPLE     : longint;             // Motion sample value.
    REPORTPER  : longword;            // Number of samples to generate an EVENT_REPORTRDY.
    ACC        : longint;             // Accumulated valid transitions register.
    ACCREAD    : longint;             // Snapshot of ACC register. Value generated by the TASKS_READCLEACC
    PSELLED    : longword;            // Pin select for LED output.
    PSELA      : longword;            // Pin select for phase A input.
    PSELB      : longword;            // Pin select for phase B input.
    DBFEN      : longword;            // Enable debouncer input filters.
    RESERVED4  : array[0..4] of longword;
    LEDPRE     : longword;            // Time LED is switched ON before the sample.
    ACCDBL     : longword;            // Accumulated double (error) transitions register.
    ACCDBLREAD : longword;            // Snapshot of ACCDBL register. Value generated by the TASKS_READCLEACC
    RESERVED5  : array[0..683] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TLPCOMP_Registers = record          // LPCOMP Structure
    TASKS_START : longword;           // Start the comparator.
    TASKS_STOP : longword;            // Stop the comparator.
    TASKS_SAMPLE : longword;          // Sample comparator value.
    RESERVED0  : array[0..60] of longword;
    EVENTS_READY : longword;          // LPCOMP is ready and output is valid.
    EVENTS_DOWN : longword;           // Input voltage crossed the threshold going down.
    EVENTS_UP  : longword;            // Input voltage crossed the threshold going up.
    EVENTS_CROSS : longword;          // Input voltage crossed the threshold in any direction.
    RESERVED1  : array[0..59] of longword;
    SHORTS     : longword;            // Shortcuts for the LPCOMP.
    RESERVED2  : array[0..63] of longword;
    INTENSET   : longword;            // Interrupt enable set register.
    INTENCLR   : longword;            // Interrupt enable clear register.
    RESERVED3  : array[0..60] of longword;
    RESULT     : longword;            // Result of last compare.
    RESERVED4  : array[0..62] of longword;
    ENABLE     : longword;            // Enable the LPCOMP.
    PSEL       : longword;            // Input pin select.
    REFSEL     : longword;            // Reference select.
    EXTREFSEL  : longword;            // External reference select.
    RESERVED5  : array[0..3] of longword;
    ANADETECT  : longword;            // Analog detect configuration.
    RESERVED6  : array[0..693] of longword;
    POWER      : longword;            // Peripheral power control.
  end;

  TSWI_Registers = record             // SWI Structure
    UNUSED     : longword;            // Unused.
  end;

  TNVMC_Registers = record            // NVMC Structure
    RESERVED0  : array[0..255] of longword;
    READY      : longword;            // Ready flag.
    RESERVED1  : array[0..63] of longword;
    CONFIG     : longword;            // Configuration register.
    ERASEPAGE  : longword;            // Register for erasing a non-protected non-volatile memory page.
    ERASEALL   : longword;            // Register for erasing all non-volatile user memory.
    ERASEPCR0  : longword;            // Register for erasing a protected non-volatile memory page.
    ERASEUICR  : longword;            // Register for start erasing User Information Congfiguration Registers.
  end;

  TPPI_Registers = record             // PPI Structure
    TASKS_CHG  : array[0..3] of TPPI_TASKS_CHG_Registers; // Channel group tasks.
    RESERVED0  : array[0..311] of longword;
    CHEN       : longword;            // Channel enable.
    CHENSET    : longword;            // Channel enable set.
    CHENCLR    : longword;            // Channel enable clear.
    RESERVED1  : longword;
    CH         : array[0..15] of TPPI_CH_Registers; // PPI Channel.
    RESERVED2  : array[0..155] of longword;
    CHG        : array[0..3] of longword; // Channel group configuration.
  end;

  TFICR_Registers = record            // FICR Structure
    RESERVED0  : array[0..3] of longword;
    CODEPAGESIZE : longword;          // Code memory page size in bytes.
    CODESIZE   : longword;            // Code memory size in pages.
    RESERVED1  : array[0..3] of longword;
    CLENR0     : longword;            // Length of code region 0 in bytes.
    PPFC       : longword;            // Pre-programmed factory code present.
    RESERVED2  : longword;
    NUMRAMBLOCK : longword;           // Number of individualy controllable RAM blocks.
    SIZERAMBLOCK : array[0..3] of longword; // Deprecated array of size of RAM block in bytes. This name is
    RESERVED3  : array[0..4] of longword;
    CONFIGID   : longword;            // Configuration identifier.
    DEVICEID   : array[0..1] of longword; // Device identifier.
    RESERVED4  : array[0..5] of longword;
    ER         : array[0..3] of longword; // Encryption root.
    IR         : array[0..3] of longword; // Identity root.
    DEVICEADDRTYPE : longword;        // Device address type.
    DEVICEADDR : array[0..1] of longword; // Device address.
    OVERRIDEEN : longword;            // Radio calibration override enable.
    NRF_1MBIT  : array[0..4] of longword; // Override values for the OVERRIDEn registers in RADIO for NRF_1Mbit
    RESERVED5  : array[0..9] of longword;
    BLE_1MBIT  : array[0..4] of longword; // Override values for the OVERRIDEn registers in RADIO for BLE_1Mbit
  end;

  TUICR_Registers = record            // UICR Structure
    CLENR0     : longword;            // Length of code region 0.
    RBPCONF    : longword;            // Readback protection configuration.
    XTALFREQ   : longword;            // Reset value for CLOCK XTALFREQ register.
    RESERVED0  : longword;
    FWID       : longword;            // Firmware ID.
    NRFFW      : array[0..14] of longword; // Reserved for Nordic firmware design.
    NRFHW      : array[0..11] of longword; // Reserved for Nordic hardware design.
    CUSTOMER   : array[0..31] of longword; // Reserved for customer.
  end;

  TGPIO_Registers = record            // GPIO Structure
    RESERVED0  : array[0..320] of longword;
    OUT        : longword;            // Write GPIO port.
    OUTSET     : longword;            // Set individual bits in GPIO port.
    OUTCLR     : longword;            // Clear individual bits in GPIO port.
    &IN        : longword;            // Read GPIO port.
    DIR        : longword;            // Direction of GPIO pins.
    DIRSET     : longword;            // DIR set register.
    DIRCLR     : longword;            // DIR clear register.
    RESERVED1  : array[0..119] of longword;
    PIN_CNF    : array[0..31] of longword; // Configuration of GPIO pins.
  end;

const
  POWER_BASE   = $40000000;
  CLOCK_BASE   = $40000000;
  MPU_BASE     = $40000000;
  RADIO_BASE   = $40001000;
  UART0_BASE   = $40002000;
  SPI0_BASE    = $40003000;
  TWI0_BASE    = $40003000;
  SPI1_BASE    = $40004000;
  TWI1_BASE    = $40004000;
  SPIS1_BASE   = $40004000;
  GPIOTE_BASE  = $40006000;
  ADC_BASE     = $40007000;
  TIMER0_BASE  = $40008000;
  TIMER1_BASE  = $40009000;
  TIMER2_BASE  = $4000A000;
  RTC0_BASE    = $4000B000;
  TEMP_BASE    = $4000C000;
  RNG_BASE     = $4000D000;
  ECB_BASE     = $4000E000;
  AAR_BASE     = $4000F000;
  CCM_BASE     = $4000F000;
  WDT_BASE     = $40010000;
  RTC1_BASE    = $40011000;
  QDEC_BASE    = $40012000;
  LPCOMP_BASE  = $40013000;
  SWI_BASE     = $40014000;
  NVMC_BASE    = $4001E000;
  PPI_BASE     = $4001F000;
  FICR_BASE    = $10000000;
  UICR_BASE    = $10001000;
  GPIO_BASE    = $50000000;

var
  POWER        : TPOWER_Registers absolute POWER_BASE;
  CLOCK        : TCLOCK_Registers absolute CLOCK_BASE;
  MPU          : TMPU_Registers absolute MPU_BASE;
  RADIO        : TRADIO_Registers absolute RADIO_BASE;
  UART0        : TUART_Registers absolute UART0_BASE;
  SPI0         : TSPI_Registers absolute SPI0_BASE;
  TWI0         : TTWI_Registers absolute TWI0_BASE;
  SPI1         : TSPI_Registers absolute SPI1_BASE;
  TWI1         : TTWI_Registers absolute TWI1_BASE;
  SPIS1        : TSPIS_Registers absolute SPIS1_BASE;
  GPIOTE       : TGPIOTE_Registers absolute GPIOTE_BASE;
  ADC          : TADC_Registers absolute ADC_BASE;
  TIMER0       : TTIMER_Registers absolute TIMER0_BASE;
  TIMER1       : TTIMER_Registers absolute TIMER1_BASE;
  TIMER2       : TTIMER_Registers absolute TIMER2_BASE;
  RTC0         : TRTC_Registers absolute RTC0_BASE;
  TEMP         : TTEMP_Registers absolute TEMP_BASE;
  RNG          : TRNG_Registers absolute RNG_BASE;
  ECB          : TECB_Registers absolute ECB_BASE;
  AAR          : TAAR_Registers absolute AAR_BASE;
  CCM          : TCCM_Registers absolute CCM_BASE;
  WDT          : TWDT_Registers absolute WDT_BASE;
  RTC1         : TRTC_Registers absolute RTC1_BASE;
  QDEC         : TQDEC_Registers absolute QDEC_BASE;
  LPCOMP       : TLPCOMP_Registers absolute LPCOMP_BASE;
  SWI          : TSWI_Registers absolute SWI_BASE;
  NVMC         : TNVMC_Registers absolute NVMC_BASE;
  PPI          : TPPI_Registers absolute PPI_BASE;
  FICR         : TFICR_Registers absolute FICR_BASE;
  UICR         : TUICR_Registers absolute UICR_BASE;
  GPIO         : TGPIO_Registers absolute GPIO_BASE;

implementation

procedure Reset_interrupt; external name 'Reset_interrupt';
procedure NonMaskableInt_interrupt; external name 'NonMaskableInt_interrupt';
procedure HardFault_interrupt; external name 'HardFault_interrupt';
procedure SVCall_interrupt; external name 'SVCall_interrupt';
procedure DebugMonitor_interrupt; external name 'DebugMonitor_interrupt';
procedure PendSV_interrupt; external name 'PendSV_interrupt';
procedure SysTick_interrupt; external name 'SysTick_interrupt';
procedure POWER_CLOCK_interrupt; external name 'POWER_CLOCK_interrupt';
procedure RADIO_interrupt; external name 'RADIO_interrupt';
procedure UART0_interrupt; external name 'UART0_interrupt';
procedure SPI0_TWI0_interrupt; external name 'SPI0_TWI0_interrupt';
procedure SPI1_TWI1_interrupt; external name 'SPI1_TWI1_interrupt';
procedure GPIOTE_interrupt; external name 'GPIOTE_interrupt';
procedure ADC_interrupt; external name 'ADC_interrupt';
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
procedure LPCOMP_interrupt; external name 'LPCOMP_interrupt';
procedure SWI0_interrupt; external name 'SWI0_interrupt';
procedure SWI1_interrupt; external name 'SWI1_interrupt';
procedure SWI2_interrupt; external name 'SWI2_interrupt';
procedure SWI3_interrupt; external name 'SWI3_interrupt';
procedure SWI4_interrupt; external name 'SWI4_interrupt';
procedure SWI5_interrupt; external name 'SWI5_interrupt';

{$i cortexm0_start.inc}

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
  .long 0
  .long 0
  .long 0
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
  .long UART0_interrupt
  .long SPI0_TWI0_interrupt
  .long SPI1_TWI1_interrupt
  .long 0
  .long GPIOTE_interrupt
  .long ADC_interrupt
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
  .long LPCOMP_interrupt
  .long SWI0_interrupt
  .long SWI1_interrupt
  .long SWI2_interrupt
  .long SWI3_interrupt
  .long SWI4_interrupt
  .long SWI5_interrupt
  .weak Reset_interrupt
  .weak NonMaskableInt_interrupt
  .weak HardFault_interrupt
  .weak SVCall_interrupt
  .weak DebugMonitor_interrupt
  .weak PendSV_interrupt
  .weak SysTick_interrupt
  .weak POWER_CLOCK_interrupt
  .weak RADIO_interrupt
  .weak UART0_interrupt
  .weak SPI0_TWI0_interrupt
  .weak SPI1_TWI1_interrupt
  .weak GPIOTE_interrupt
  .weak ADC_interrupt
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
  .weak LPCOMP_interrupt
  .weak SWI0_interrupt
  .weak SWI1_interrupt
  .weak SWI2_interrupt
  .weak SWI3_interrupt
  .weak SWI4_interrupt
  .weak SWI5_interrupt
  .set Reset_interrupt, HaltProc
  .set NonMaskableInt_interrupt, HaltProc
  .set HardFault_interrupt, HaltProc
  .set SVCall_interrupt, HaltProc
  .set DebugMonitor_interrupt, HaltProc
  .set PendSV_interrupt, HaltProc
  .set SysTick_interrupt, HaltProc
  .set POWER_CLOCK_interrupt, HaltProc
  .set RADIO_interrupt, HaltProc
  .set UART0_interrupt, HaltProc
  .set SPI0_TWI0_interrupt, HaltProc
  .set SPI1_TWI1_interrupt, HaltProc
  .set GPIOTE_interrupt, HaltProc
  .set ADC_interrupt, HaltProc
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
  .set LPCOMP_interrupt, HaltProc
  .set SWI0_interrupt, HaltProc
  .set SWI1_interrupt, HaltProc
  .set SWI2_interrupt, HaltProc
  .set SWI3_interrupt, HaltProc
  .set SWI4_interrupt, HaltProc
  .set SWI5_interrupt, HaltProc
  .text
end;
end.
