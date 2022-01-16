unit AT90PWM161;

{$goto on}

interface

var
  // PORTB
  PORTB : byte absolute $00+$25; // Port B Data Register
  DDRB : byte absolute $00+$24; // Port B Data Direction Register
  PINB : byte absolute $00+$23; // Port B Input Pins
  // PORTD
  PORTD : byte absolute $00+$2B; // Port D Data Register
  DDRD : byte absolute $00+$2A; // Port D Data Direction Register
  PIND : byte absolute $00+$29; // Port D Input Pins
  // DA_CONVERTER
  DACH : byte absolute $00+$59; // DAC Data Register High Byte
  DACL : byte absolute $00+$58; // DAC Data Register Low Byte
  DACON : byte absolute $00+$76; // DAC Control Register
  // PORTE
  PORTE : byte absolute $00+$2E; // Port E Data Register
  DDRE : byte absolute $00+$2D; // Port E Data Direction Register
  PINE : byte absolute $00+$2C; // Port E Input Pins
  // SPI
  SPCR : byte absolute $00+$37; // SPI Control Register
  SPSR : byte absolute $00+$38; // SPI Status Register
  SPDR : byte absolute $00+$56; // SPI Data Register
  // WATCHDOG
  WDTCSR : byte absolute $00+$82; // Watchdog Timer Control Register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$89; // External Interrupt Control Register A
  EIMSK : byte absolute $00+$41; // External Interrupt Mask Register
  EIFR : byte absolute $00+$40; // External Interrupt Flag Register
  // AD_CONVERTER
  ADMUX : byte absolute $00+$28; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$26; // The ADC Control and Status register
  ADC : word absolute $00+$4C; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$4C; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$4C+1; // ADC Data Register  Bytes
  ADCSRB : byte absolute $00+$27; // ADC Control and Status Register B
  DIDR0 : byte absolute $00+$77; // Digital Input Disable Register 0
  DIDR1 : byte absolute $00+$78; // Digital Input Disable Register 0
  AMP0CSR : byte absolute $00+$79; // 
  // ANALOG_COMPARATOR
  AC3CON : byte absolute $00+$7F; // Analog Comparator3 Control Register
  AC1CON : byte absolute $00+$7D; // Analog Comparator 1 Control Register
  AC2CON : byte absolute $00+$7E; // Analog Comparator 2 Control Register
  ACSR : byte absolute $00+$20; // Analog Comparator Status Register
  AC3ECON : byte absolute $00+$7C; // 
  AC2ECON : byte absolute $00+$7B; // 
  AC1ECON : byte absolute $00+$7A; // 
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  OSCCAL : byte absolute $00+$88; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$83; // 
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  GPIOR2 : byte absolute $00+$3B; // General Purpose IO Register 2
  GPIOR1 : byte absolute $00+$3A; // General Purpose IO Register 1
  GPIOR0 : byte absolute $00+$39; // General Purpose IO Register 0
  PLLCSR : byte absolute $00+$87; // PLL Control And Status Register
  PRR : byte absolute $00+$86; // Power Reduction Register
  CLKCSR : byte absolute $00+$84; // 
  CLKSELR : byte absolute $00+$85; // 
  BGCCR : byte absolute $00+$81; // BandGap Current Calibration Register
  BGCRR : byte absolute $00+$80; // BandGap Resistor Calibration Register
  // EEPROM
  EEAR : word absolute $00+$3E; // EEPROM Read/Write Access  Bytes
  EEARL : byte absolute $00+$3E; // EEPROM Read/Write Access  Bytes
  EEARH : byte absolute $00+$3E+1; // EEPROM Read/Write Access  Bytes
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register
  // PSC0
  PICR0 : word absolute $00+$68; // PSC 0 Input Capture Register 
  PICR0L : byte absolute $00+$68; // PSC 0 Input Capture Register 
  PICR0H : byte absolute $00+$68+1; // PSC 0 Input Capture Register 
  PFRC0B : byte absolute $00+$63; // PSC 0 Input B Control
  PFRC0A : byte absolute $00+$62; // PSC 0 Input A Control
  PCTL0 : byte absolute $00+$32; // PSC 0 Control Register
  PCNF0 : byte absolute $00+$31; // PSC 0 Configuration Register
  OCR0RB : word absolute $00+$44; // Output Compare RB Register 
  OCR0RBL : byte absolute $00+$44; // Output Compare RB Register 
  OCR0RBH : byte absolute $00+$44+1; // Output Compare RB Register 
  OCR0SB : word absolute $00+$42; // Output Compare SB Register 
  OCR0SBL : byte absolute $00+$42; // Output Compare SB Register 
  OCR0SBH : byte absolute $00+$42+1; // Output Compare SB Register 
  OCR0RA : word absolute $00+$4A; // Output Compare RA Register 
  OCR0RAL : byte absolute $00+$4A; // Output Compare RA Register 
  OCR0RAH : byte absolute $00+$4A+1; // Output Compare RA Register 
  OCR0SA : word absolute $00+$60; // Output Compare SA Register 
  OCR0SAL : byte absolute $00+$60; // Output Compare SA Register 
  OCR0SAH : byte absolute $00+$60+1; // Output Compare SA Register 
  PSOC0 : byte absolute $00+$6A; // PSC0 Synchro and Output Configuration
  PIM0 : byte absolute $00+$2F; // PSC0 Interrupt Mask Register
  PIFR0 : byte absolute $00+$30; // PSC0 Interrupt Flag Register
  // PSC2
  PICR2H : byte absolute $00+$6D; // PSC 2 Input Capture Register High
  PICR2L : byte absolute $00+$6C; // PSC 2 Input Capture Register Low
  PFRC2B : byte absolute $00+$67; // PSC 2 Input B Control
  PFRC2A : byte absolute $00+$66; // PSC 2 Input B Control
  PCTL2 : byte absolute $00+$36; // PSC 2 Control Register
  PCNF2 : byte absolute $00+$35; // PSC 2 Configuration Register
  PCNFE2 : byte absolute $00+$70; // PSC 2 Enhanced Configuration Register
  OCR2RB : word absolute $00+$48; // Output Compare RB Register 
  OCR2RBL : byte absolute $00+$48; // Output Compare RB Register 
  OCR2RBH : byte absolute $00+$48+1; // Output Compare RB Register 
  OCR2SB : word absolute $00+$46; // Output Compare SB Register 
  OCR2SBL : byte absolute $00+$46; // Output Compare SB Register 
  OCR2SBH : byte absolute $00+$46+1; // Output Compare SB Register 
  OCR2RA : word absolute $00+$4E; // Output Compare RA Register 
  OCR2RAL : byte absolute $00+$4E; // Output Compare RA Register 
  OCR2RAH : byte absolute $00+$4E+1; // Output Compare RA Register 
  OCR2SA : word absolute $00+$64; // Output Compare SA Register 
  OCR2SAL : byte absolute $00+$64; // Output Compare SA Register 
  OCR2SAH : byte absolute $00+$64+1; // Output Compare SA Register 
  POM2 : byte absolute $00+$6F; // PSC 2 Output Matrix
  PSOC2 : byte absolute $00+$6E; // PSC2 Synchro and Output Configuration
  PIM2 : byte absolute $00+$33; // PSC2 Interrupt Mask Register
  PIFR2 : byte absolute $00+$34; // PSC2 Interrupt Flag Register
  PASDLY2 : byte absolute $00+$71; // Analog Synchronization Delay Register
  // TIMER_COUNTER_1
  TIMSK1 : byte absolute $00+$21; // Timer/Counter Interrupt Mask Register
  TIFR1 : byte absolute $00+$22; // Timer/Counter Interrupt Flag register
  TCCR1B : byte absolute $00+$8A; // Timer/Counter1 Control Register B
  TCNT1 : word absolute $00+$5A; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$5A; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$5A+1; // Timer/Counter1  Bytes
  ICR1 : word absolute $00+$8C; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$8C; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$8C+1; // Timer/Counter1 Input Capture Register  Bytes
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register

const
  // DACH
  // DACL
  // DACON
  DAATE = 7; // DAC Auto Trigger Enable Bit
  DATS = 4; // DAC Trigger Selection Bits
  DALA = 2; // DAC Left Adjust
  DAEN = 0; // DAC Enable Bit
  // SPCR
  SPIE = 7; // SPI Interrupt Enable
  SPE = 6; // SPI Enable
  DORD = 5; // Data Order
  MSTR = 4; // Master/Slave Select
  CPOL = 3; // Clock polarity
  CPHA = 2; // Clock Phase
  SPR = 0; // SPI Clock Rate Selects
  // SPSR
  SPIF = 7; // SPI Interrupt Flag
  WCOL = 6; // Write Collision Flag
  SPI2X = 0; // Double SPI Speed Bit
  // WDTCSR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  // EICRA
  ISC2 = 4; // External Interrupt Sense Control Bit
  ISC1 = 2; // External Interrupt Sense Control Bit
  ISC0 = 0; // External Interrupt Sense Control Bit
  // EIMSK
  INT = 0; // External Interrupt Request 2 Enable
  // EIFR
  INTF = 0; // External Interrupt Flags
  // ADMUX
  REFS = 6; // Reference Selection Bits
  ADLAR = 5; // Left Adjust Result
  MUX = 0; // Analog Channel and Gain Selection Bits
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADATE = 5; // ADC Auto Trigger Enable
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits
  // ADCSRB
  ADHSM = 7; // ADC High Speed Mode
  ADNCDIS = 6; // ADC Noise Canceller Disable
  ADSSEN = 4; // ADC Single Shot Enable on PSC's Synchronisation Signals
  ADTS = 0; // ADC Auto Trigger Sources
  // DIDR0
  ADC7D = 7; // 
  ADC6D = 6; // ADC7 Digital input Disable
  ADC5D = 5; // ADC5 Digital input Disable
  ADC4D = 4; // ADC4 Digital input Disable
  ADC3D = 3; // ADC3 Digital input Disable
  ADC2D = 2; // ADC2 Digital input Disable
  ADC1D = 1; // ADC1 Digital input Disable
  ADC0D = 0; // ADC0 Digital input Disable
  // DIDR1
  ACMP1MD = 3; // 
  AMP0POSD = 2; // 
  ADC10D = 1; // 
  ADC9D = 0; // 
  // AMP0CSR
  AMP0EN = 7; // 
  AMP0IS = 6; // 
  AMP0G = 4; // 
  AMP0GS = 3; // 
  AMP0TS = 0; // 
  // AC3CON
  AC3EN = 7; // Analog Comparator3 Enable Bit
  AC3IE = 6; // Analog Comparator 3 Interrupt Enable Bit
  AC3IS = 4; // Analog Comparator 3  Interrupt Select Bit
  AC3OEA = 3; // Analog Comparator 3 Alternate Output Enable
  AC3M = 0; // Analog Comparator 3 Multiplexer Register
  // AC1CON
  AC1EN = 7; // Analog Comparator 1 Enable Bit
  AC1IE = 6; // Analog Comparator 1 Interrupt Enable Bit
  AC1IS = 4; // Analog Comparator 1  Interrupt Select Bit
  AC1M = 0; // Analog Comparator 1 Multiplexer Register
  // AC2CON
  AC2EN = 7; // Analog Comparator 2 Enable Bit
  AC2IE = 6; // Analog Comparator 2 Interrupt Enable Bit
  AC2IS = 4; // Analog Comparator 2  Interrupt Select Bit
  AC2M = 0; // Analog Comparator 2 Multiplexer Register
  // ACSR
  AC3IF = 7; // Analog Comparator 3 Interrupt Flag Bit
  AC2IF = 6; // Analog Comparator 2 Interrupt Flag Bit
  AC1IF = 5; // Analog Comparator 1  Interrupt Flag Bit
  AC3O = 3; // Analog Comparator 3 Output Bit
  AC2O = 2; // Analog Comparator 2 Output Bit
  AC1O = 1; // Analog Comparator 1 Output Bit
  // AC3ECON
  AC3OI = 5; // Analog Comparator Ouput Invert
  AC3OE = 4; // Analog Comparator Ouput Enable
  AC3H = 0; // Analog Comparator Hysteresis Select
  // AC2ECON
  AC2OI = 5; // Analog Comparator Ouput Invert
  AC2OE = 4; // Analog Comparator Ouput Enable
  AC2H = 0; // Analog Comparator Hysteresis Select
  // AC1ECON
  AC1OI = 5; // Analog Comparator Ouput Invert
  AC1OE = 4; // Analog Comparator Ouput Enable
  AC1ICE = 3; // Analog Comparator Interrupt Capture Enable
  AC1H = 0; // Analog Comparator Hysteresis Select
  // SREG
  I = 7; // Global Interrupt Enable
  T = 6; // Bit Copy Storage
  H = 5; // Half Carry Flag
  S = 4; // Sign Bit
  V = 3; // Two's Complement Overflow Flag
  N = 2; // Negative Flag
  Z = 1; // Zero Flag
  C = 0; // Carry Flag
  // MCUCR
  PUD = 4; // Pull-up disable
  RSTDIS = 3; // Reset Pin Disable
  CKRC81 = 2; // Frequency Selection of the Calibrated RC Oscillator
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // CLKPR
  CLKPCE = 7; // 
  CLKPS = 0; // 
  // SMCR
  SM = 1; // Sleep Mode Select bits
  SE = 0; // Sleep Enable
  // GPIOR2
  GPIOR = 0; // General Purpose IO Register 2 bis
  // GPIOR1
  // GPIOR0
  GPIOR07 = 7; // General Purpose IO Register 0 bit 7
  GPIOR06 = 6; // General Purpose IO Register 0 bit 6
  GPIOR05 = 5; // General Purpose IO Register 0 bit 5
  GPIOR04 = 4; // General Purpose IO Register 0 bit 4
  GPIOR03 = 3; // General Purpose IO Register 0 bit 3
  GPIOR02 = 2; // General Purpose IO Register 0 bit 2
  GPIOR01 = 1; // General Purpose IO Register 0 bit 1
  GPIOR00 = 0; // General Purpose IO Register 0 bit 0
  // PLLCSR
  PLLF = 2; // 
  PLLE = 1; // PLL Enable
  PLOCK = 0; // PLL Lock Detector
  // PRR
  PRPSC2 = 7; // Power Reduction PSC2
  PRPSCR = 5; // Power Reduction PSC0
  PRTIM1 = 4; // Power Reduction Timer/Counter1
  PRSPI = 2; // Power Reduction Serial Peripheral Interface
  PRADC = 0; // Power Reduction ADC
  // CLKCSR
  CLKCCE = 7; // Clock Control Change Enable
  CLKRDY = 4; // Clock Ready Flag
  CLKC = 0; // Clock Control
  // CLKSELR
  COUT = 6; // Clock OUT
  CSUT = 4; // Clock Start up Time
  CKSEL = 0; // Clock Source Select
  // BGCCR
  BGCC = 0; // 
  // BGCRR
  BGCR = 0; // 
  // EECR
  NVMBSY = 7; // None Volatile Busy Memory Busy
  EEPAGE = 6; // EEPROM Page Access
  EEPM = 4; // EEPROM Programming Mode
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMWE = 2; // EEPROM Master Write Enable
  EEWE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // PFRC0B
  PCAE0B = 7; // PSC 0 Capture Enable Input Part B
  PISEL0B = 6; // PSC 0 Input Select for Part B
  PELEV0B = 5; // PSC 0 Edge Level Selector on Input Part B
  PFLTE0B = 4; // PSC 0 Filter Enable on Input Part B
  PRFM0B = 0; // PSC 0 Retrigger and Fault Mode for Part B
  // PFRC0A
  PCAE0A = 7; // PSC 0 Capture Enable Input Part A
  PISEL0A = 6; // PSC 0 Input Select for Part A
  PELEV0A = 5; // PSC 0 Edge Level Selector on Input Part A
  PFLTE0A = 4; // PSC 0 Filter Enable on Input Part A
  PRFM0A = 0; // PSC 0 Retrigger and Fault Mode for Part A
  // PCTL0
  PPRE0 = 6; // PSC 0 Prescaler Selects
  PBFM0 = 2; // PSC 0 Balance Flank Width Modulation
  PAOC0B = 4; // PSC 0 Asynchronous Output Control B
  PAOC0A = 3; // PSC 0 Asynchronous Output Control A
  PCCYC0 = 1; // PSC0 Complete Cycle
  PRUN0 = 0; // PSC 0 Run
  // PCNF0
  PFIFTY0 = 7; // PSC 0 Fifty
  PALOCK0 = 6; // PSC 0 Autolock
  PLOCK0 = 5; // PSC 0 Lock
  PMODE0 = 3; // PSC 0 Mode
  POP0 = 2; // PSC 0 Output Polarity
  PCLKSEL0 = 1; // PSC 0 Input Clock Select
  // PSOC0
  PISEL0A1 = 7; // PSC Input Select
  PISEL0B1 = 6; // PSC Input Select
  PSYNC0 = 4; // Synchronisation out for ADC selection
  POEN0B = 2; // PSCOUT01 Output Enable
  POEN0A = 0; // PSCOUT00 Output Enable
  // PIM0
  PEVE0B = 4; // External Event B Interrupt Enable
  PEVE0A = 3; // External Event A Interrupt Enable
  PEOEPE0 = 1; // End of Enhanced Cycle Enable
  PEOPE0 = 0; // End of Cycle Interrupt Enable
  // PIFR0
  POAC0B = 7; // PSC 0 Output A Activity
  POAC0A = 6; // PSC 0 Output A Activity
  PEV0B = 4; // External Event B Interrupt
  PEV0A = 3; // External Event A Interrupt
  PRN0 = 1; // Ramp Number
  PEOP0 = 0; // End of PSC0 Interrupt
  // PICR2H
  PCST2 = 7; // PSC 2 Capture Software Trigger Bit
  PICR21 = 2; // 
  PICR2 = 0; // 
  // PFRC2B
  PCAE2B = 7; // PSC 2 Capture Enable Input Part B
  PISEL2B = 6; // PSC 2 Input Select for Part B
  PELEV2B = 5; // PSC 2 Edge Level Selector on Input Part B
  PFLTE2B = 4; // PSC 2 Filter Enable on Input Part B
  PRFM2B = 0; // PSC 2 Retrigger and Fault Mode for Part B
  // PFRC2A
  PCAE2A = 7; // PSC 2 Capture Enable Input Part A
  PISEL2A = 6; // PSC 2 Input Select for Part A
  PELEV2A = 5; // PSC 2 Edge Level Selector on Input Part A
  PFLTE2A = 4; // PSC 2 Filter Enable on Input Part A
  PRFM2A = 0; // PSC 2 Retrigger and Fault Mode for Part A
  // PCTL2
  PPRE2 = 6; // PSC 2 Prescaler Selects
  PBFM2 = 5; // Balance Flank Width Modulation
  PAOC2B = 4; // PSC 2 Asynchronous Output Control B
  PAOC2A = 3; // PSC 2 Asynchronous Output Control A
  PARUN2 = 2; // PSC2 Auto Run
  PCCYC2 = 1; // PSC2 Complete Cycle
  PRUN2 = 0; // PSC 2 Run
  // PCNF2
  PFIFTY2 = 7; // PSC 2 Fifty
  PALOCK2 = 6; // PSC 2 Autolock
  PLOCK2 = 5; // PSC 2 Lock
  PMODE2 = 3; // PSC 2 Mode
  POP2 = 2; // PSC 2 Output Polarity
  PCLKSEL2 = 1; // PSC 2 Input Clock Select
  POME2 = 0; // PSC 2 Output Matrix Enable
  // PCNFE2
  PASDLK2 = 5; // 
  PBFM21 = 4; // 
  PELEV2A1 = 3; // 
  PELEV2B1 = 2; // 
  PISEL2A1 = 1; // 
  PISEL2B1 = 0; // 
  // POM2
  POMV2B = 4; // Output Matrix Output B Ramps
  POMV2A = 0; // Output Matrix Output A Ramps
  // PSOC2
  POS2 = 6; // PSC 2 Output 23 Select
  PSYNC2 = 4; // Synchronization Out for ADC Selection
  POEN2D = 3; // PSCOUT23 Output Enable
  POEN2B = 2; // PSCOUT21 Output Enable
  POEN2C = 1; // PSCOUT22 Output Enable
  POEN2A = 0; // PSCOUT20 Output Enable
  // PIM2
  PSEIE2 = 5; // PSC 2 Synchro Error Interrupt Enable
  PEVE2B = 4; // External Event B Interrupt Enable
  PEVE2A = 3; // External Event A Interrupt Enable
  PEOEPE2 = 1; // End of Enhanced Cycle Interrupt Enable
  PEOPE2 = 0; // End of Cycle Interrupt Enable
  // PIFR2
  POAC2B = 7; // PSC 2 Output A Activity
  POAC2A = 6; // PSC 2 Output A Activity
  PSEI2 = 5; // PSC 2 Synchro Error Interrupt
  PEV2B = 4; // External Event B Interrupt
  PEV2A = 3; // External Event A Interrupt
  PRN2 = 1; // Ramp Number
  PEOP2 = 0; // End of PSC2 Interrupt
  // TIMSK1
  ICIE1 = 5; // Timer/Counter1 Input Capture Interrupt Enable
  TOIE1 = 0; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR1
  ICF1 = 5; // Input Capture Flag 1
  TOV1 = 0; // Timer/Counter1 Overflow Flag
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceler
  ICES1 = 6; // Input Capture 1 Edge Select
  WGM13 = 4; // Waveform Generation Mode
  CS1 = 0; // Prescaler source of Timer/Counter 1
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read While Write Section Busy
  SIGRD = 5; // Signature Row Read
  RWWSRE = 4; // Read While Write section read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure PSC2_CAPT_ISR; external name 'PSC2_CAPT_ISR'; // Interrupt 1 PSC2 Capture Event
procedure PSC2_EC_ISR; external name 'PSC2_EC_ISR'; // Interrupt 2 PSC2 End Cycle
procedure PSC2_EEC_ISR; external name 'PSC2_EEC_ISR'; // Interrupt 3 PSC2 End Of Enhanced Cycle
procedure PSC0_CAPT_ISR; external name 'PSC0_CAPT_ISR'; // Interrupt 4 PSC0 Capture Event
procedure PSC0_EC_ISR; external name 'PSC0_EC_ISR'; // Interrupt 5 PSC0 End Cycle
procedure PSC0_EEC_ISR; external name 'PSC0_EEC_ISR'; // Interrupt 6 PSC0 End Of Enhanced Cycle
procedure ANALOG_COMP_1_ISR; external name 'ANALOG_COMP_1_ISR'; // Interrupt 7 Analog Comparator 1
procedure ANALOG_COMP_2_ISR; external name 'ANALOG_COMP_2_ISR'; // Interrupt 8 Analog Comparator 2
procedure ANALOG_COMP_3_ISR; external name 'ANALOG_COMP_3_ISR'; // Interrupt 9 Analog Comparator 3
procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 10 External Interrupt Request 0
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 11 Timer/Counter1 Capture Event
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 12 Timer/Counter1 Overflow
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 13 ADC Conversion Complete
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 14 External Interrupt Request 1
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 15 SPI Serial Transfer Complet
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 16 External Interrupt Request 2
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 17 Watchdog Timeout Interrupt
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 18 EEPROM Ready
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 19 Store Program Memory Read

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp PSC2_CAPT_ISR
   rjmp PSC2_EC_ISR
   rjmp PSC2_EEC_ISR
   rjmp PSC0_CAPT_ISR
   rjmp PSC0_EC_ISR
   rjmp PSC0_EEC_ISR
   rjmp ANALOG_COMP_1_ISR
   rjmp ANALOG_COMP_2_ISR
   rjmp ANALOG_COMP_3_ISR
   rjmp INT0_ISR
   rjmp TIMER1_CAPT_ISR
   rjmp TIMER1_OVF_ISR
   rjmp ADC_ISR
   rjmp INT1_ISR
   rjmp SPI__STC_ISR
   rjmp INT2_ISR
   rjmp WDT_ISR
   rjmp EE_READY_ISR
   rjmp SPM_READY_ISR

   {$i start.inc}

   .weak PSC2_CAPT_ISR
   .weak PSC2_EC_ISR
   .weak PSC2_EEC_ISR
   .weak PSC0_CAPT_ISR
   .weak PSC0_EC_ISR
   .weak PSC0_EEC_ISR
   .weak ANALOG_COMP_1_ISR
   .weak ANALOG_COMP_2_ISR
   .weak ANALOG_COMP_3_ISR
   .weak INT0_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_OVF_ISR
   .weak ADC_ISR
   .weak INT1_ISR
   .weak SPI__STC_ISR
   .weak INT2_ISR
   .weak WDT_ISR
   .weak EE_READY_ISR
   .weak SPM_READY_ISR

   .set PSC2_CAPT_ISR, Default_IRQ_handler
   .set PSC2_EC_ISR, Default_IRQ_handler
   .set PSC2_EEC_ISR, Default_IRQ_handler
   .set PSC0_CAPT_ISR, Default_IRQ_handler
   .set PSC0_EC_ISR, Default_IRQ_handler
   .set PSC0_EEC_ISR, Default_IRQ_handler
   .set ANALOG_COMP_1_ISR, Default_IRQ_handler
   .set ANALOG_COMP_2_ISR, Default_IRQ_handler
   .set ANALOG_COMP_3_ISR, Default_IRQ_handler
   .set INT0_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set SPM_READY_ISR, Default_IRQ_handler
 end;

end.
