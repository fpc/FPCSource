unit ATmega32M1;

{$goto on}

interface

var
  // PORTB
  PORTB : byte absolute $00+$25; // Port B Data Register
  DDRB : byte absolute $00+$24; // Port B Data Direction Register
  PINB : byte absolute $00+$23; // Port B Input Pins
  // PORTC
  PORTC : byte absolute $00+$28; // Port C Data Register
  DDRC : byte absolute $00+$27; // Port C Data Direction Register
  PINC : byte absolute $00+$26; // Port C Input Pins
  // PORTD
  PORTD : byte absolute $00+$2B; // Port D Data Register
  DDRD : byte absolute $00+$2A; // Port D Data Direction Register
  PIND : byte absolute $00+$29; // Port D Input Pins
  // CAN
  CANGCON : byte absolute $00+$D8; // CAN General Control Register
  CANGSTA : byte absolute $00+$D9; // CAN General Status Register
  CANGIT : byte absolute $00+$DA; // CAN General Interrupt Register Flags
  CANGIE : byte absolute $00+$DB; // CAN General Interrupt Enable Register
  CANEN2 : byte absolute $00+$DC; // Enable MOb Register 2
  CANEN1 : byte absolute $00+$DD; // Enable MOb Register 1(empty)
  CANIE2 : byte absolute $00+$DE; // Enable Interrupt MOb Register 2
  CANIE1 : byte absolute $00+$DF; // Enable Interrupt MOb Register 1 (empty)
  CANSIT2 : byte absolute $00+$E0; // CAN Status Interrupt MOb Register 2
  CANSIT1 : byte absolute $00+$E1; // CAN Status Interrupt MOb Register 1 (empty)
  CANBT1 : byte absolute $00+$E2; // CAN Bit Timing Register 1
  CANBT2 : byte absolute $00+$E3; // CAN Bit Timing Register 2
  CANBT3 : byte absolute $00+$E4; // CAN Bit Timing Register 3
  CANTCON : byte absolute $00+$E5; // Timer Control Register
  CANTIML : byte absolute $00+$E6; // Timer Register Low
  CANTIMH : byte absolute $00+$E7; // Timer Register High
  CANTTCL : byte absolute $00+$E8; // TTC Timer Register Low
  CANTTCH : byte absolute $00+$E9; // TTC Timer Register High
  CANTEC : byte absolute $00+$EA; // Transmit Error Counter Register
  CANREC : byte absolute $00+$EB; // Receive Error Counter Register
  CANHPMOB : byte absolute $00+$EC; // Highest Priority MOb Register
  CANPAGE : byte absolute $00+$ED; // Page MOb Register
  CANSTMOB : byte absolute $00+$EE; // MOb Status Register
  CANCDMOB : byte absolute $00+$EF; // MOb Control and DLC Register
  CANIDT4 : byte absolute $00+$F0; // Identifier Tag Register 4
  CANIDT3 : byte absolute $00+$F1; // Identifier Tag Register 3
  CANIDT2 : byte absolute $00+$F2; // Identifier Tag Register 2
  CANIDT1 : byte absolute $00+$F3; // Identifier Tag Register 1
  CANIDM4 : byte absolute $00+$F4; // Identifier Mask Register 4
  CANIDM3 : byte absolute $00+$F5; // Identifier Mask Register 3
  CANIDM2 : byte absolute $00+$F6; // Identifier Mask Register 2
  CANIDM1 : byte absolute $00+$F7; // Identifier Mask Register 1
  CANSTML : byte absolute $00+$F8; // Time Stamp Register Low
  CANSTMH : byte absolute $00+$F9; // Time Stamp Register High
  CANMSG : byte absolute $00+$FA; // Message Data Register
  // ANALOG_COMPARATOR
  AC0CON : byte absolute $00+$94; // Analog Comparator 0 Control Register
  AC1CON : byte absolute $00+$95; // Analog Comparator 1 Control Register
  AC2CON : byte absolute $00+$96; // Analog Comparator 2 Control Register
  AC3CON : byte absolute $00+$97; // Analog Comparator 3 Control Register
  ACSR : byte absolute $00+$50; // Analog Comparator Status Register
  // DA_CONVERTER
  DACH : byte absolute $00+$92; // DAC Data Register High Byte
  DACL : byte absolute $00+$91; // DAC Data Register Low Byte
  DACON : byte absolute $00+$90; // DAC Control Register
  // CPU
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  OSCCAL : byte absolute $00+$66; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$61; // 
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  GPIOR2 : byte absolute $00+$3A; // General Purpose IO Register 2
  GPIOR1 : byte absolute $00+$39; // General Purpose IO Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose IO Register 0
  PLLCSR : byte absolute $00+$49; // PLL Control And Status Register
  PRR : byte absolute $00+$64; // Power Reduction Register
  // PORTE
  PORTE : byte absolute $00+$2E; // Port E Data Register
  DDRE : byte absolute $00+$2D; // Port E Data Direction Register
  PINE : byte absolute $00+$2C; // Port E Input Pins
  // TIMER_COUNTER_0
  TIMSK0 : byte absolute $00+$6E; // Timer/Counter0 Interrupt Mask Register
  TIFR0 : byte absolute $00+$35; // Timer/Counter0 Interrupt Flag register
  TCCR0A : byte absolute $00+$44; // Timer/Counter  Control Register A
  TCCR0B : byte absolute $00+$45; // Timer/Counter Control Register B
  TCNT0 : byte absolute $00+$46; // Timer/Counter0
  OCR0A : byte absolute $00+$47; // Timer/Counter0 Output Compare Register
  OCR0B : byte absolute $00+$48; // Timer/Counter0 Output Compare Register
  GTCCR : byte absolute $00+$43; // General Timer/Counter Control Register
  // TIMER_COUNTER_1
  TIMSK1 : byte absolute $00+$6F; // Timer/Counter Interrupt Mask Register
  TIFR1 : byte absolute $00+$36; // Timer/Counter Interrupt Flag register
  TCCR1A : byte absolute $00+$80; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$81; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$82; // Timer/Counter1 Control Register C
  TCNT1 : word absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$84+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$88; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AL : byte absolute $00+$88; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AH : byte absolute $00+$88+1; // Timer/Counter1 Output Compare Register  Bytes
  OCR1B : word absolute $00+$8A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BL : byte absolute $00+$8A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BH : byte absolute $00+$8A+1; // Timer/Counter1 Output Compare Register  Bytes
  ICR1 : word absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$86+1; // Timer/Counter1 Input Capture Register  Bytes
  // AD_CONVERTER
  ADMUX : byte absolute $00+$7C; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$7A; // The ADC Control and Status register
  ADC : word absolute $00+$78; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$78; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$78+1; // ADC Data Register  Bytes
  ADCSRB : byte absolute $00+$7B; // ADC Control and Status Register B
  DIDR0 : byte absolute $00+$7E; // Digital Input Disable Register 0
  DIDR1 : byte absolute $00+$7F; // Digital Input Disable Register 0
  AMP0CSR : byte absolute $00+$75; // 
  AMP1CSR : byte absolute $00+$76; // 
  AMP2CSR : byte absolute $00+$77; // 
  // LINUART
  LINCR : byte absolute $00+$C8; // LIN Control Register
  LINSIR : byte absolute $00+$C9; // LIN Status and Interrupt Register
  LINENIR : byte absolute $00+$CA; // LIN Enable Interrupt Register
  LINERR : byte absolute $00+$CB; // LIN Error Register
  LINBTR : byte absolute $00+$CC; // LIN Bit Timing Register
  LINBRRL : byte absolute $00+$CD; // LIN Baud Rate Low Register
  LINBRRH : byte absolute $00+$CE; // LIN Baud Rate High Register
  LINDLR : byte absolute $00+$CF; // LIN Data Length Register
  LINIDR : byte absolute $00+$D0; // LIN Identifier Register
  LINSEL : byte absolute $00+$D1; // LIN Data Buffer Selection Register
  LINDAT : byte absolute $00+$D2; // LIN Data Register
  // SPI
  SPCR : byte absolute $00+$4C; // SPI Control Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPDR : byte absolute $00+$4E; // SPI Data Register
  // WATCHDOG
  WDTCSR : byte absolute $00+$60; // Watchdog Timer Control Register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  PCICR : byte absolute $00+$68; // Pin Change Interrupt Control Register
  PCMSK3 : byte absolute $00+$6D; // Pin Change Mask Register 3
  PCMSK2 : byte absolute $00+$6C; // Pin Change Mask Register 2
  PCMSK1 : byte absolute $00+$6B; // Pin Change Mask Register 1
  PCMSK0 : byte absolute $00+$6A; // Pin Change Mask Register 0
  PCIFR : byte absolute $00+$3B; // Pin Change Interrupt Flag Register
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Read/Write Access
  EEARL : byte absolute $00+$41; // EEPROM Read/Write Access
  EEARH : byte absolute $00+$41+1; // EEPROM Read/Write Access
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // PSC
  PIFR : byte absolute $00+$BC; // PSC Interrupt Flag Register
  PIM : byte absolute $00+$BB; // PSC Interrupt Mask Register
  PMIC2 : byte absolute $00+$BA; // PSC Module 2 Input Control Register
  PMIC1 : byte absolute $00+$B9; // PSC Module 1 Input Control Register
  PMIC0 : byte absolute $00+$B8; // PSC Module 0 Input Control Register
  PCTL : byte absolute $00+$B7; // PSC Control Register
  POC : byte absolute $00+$B6; // PSC Output Configuration
  PCNF : byte absolute $00+$B5; // PSC Configuration Register
  PSYNC : byte absolute $00+$B4; // PSC Synchro Configuration
  POCR_RB : word absolute $00+$B2; // PSC Output Compare RB Register 
  POCR_RBL : byte absolute $00+$B2; // PSC Output Compare RB Register 
  POCR_RBH : byte absolute $00+$B2+1; // PSC Output Compare RB Register 
  POCR2SB : word absolute $00+$B0; // PSC Module 2 Output Compare SB Register 
  POCR2SBL : byte absolute $00+$B0; // PSC Module 2 Output Compare SB Register 
  POCR2SBH : byte absolute $00+$B0+1; // PSC Module 2 Output Compare SB Register 
  POCR2RA : word absolute $00+$AE; // PSC Module 2 Output Compare RA Register 
  POCR2RAL : byte absolute $00+$AE; // PSC Module 2 Output Compare RA Register 
  POCR2RAH : byte absolute $00+$AE+1; // PSC Module 2 Output Compare RA Register 
  POCR2SA : word absolute $00+$AC; // PSC Module 2 Output Compare SA Register 
  POCR2SAL : byte absolute $00+$AC; // PSC Module 2 Output Compare SA Register 
  POCR2SAH : byte absolute $00+$AC+1; // PSC Module 2 Output Compare SA Register 
  POCR1SB : word absolute $00+$AA; // PSC Module 1 Output Compare SB Register 
  POCR1SBL : byte absolute $00+$AA; // PSC Module 1 Output Compare SB Register 
  POCR1SBH : byte absolute $00+$AA+1; // PSC Module 1 Output Compare SB Register 
  POCR1RA : word absolute $00+$A8; // PSC Module 1 Output Compare RA Register 
  POCR1RAL : byte absolute $00+$A8; // PSC Module 1 Output Compare RA Register 
  POCR1RAH : byte absolute $00+$A8+1; // PSC Module 1 Output Compare RA Register 
  POCR1SA : word absolute $00+$A6; // PSC Output Compare SA Register 
  POCR1SAL : byte absolute $00+$A6; // PSC Output Compare SA Register 
  POCR1SAH : byte absolute $00+$A6+1; // PSC Output Compare SA Register 
  POCR0SB : word absolute $00+$A4; // PSC Output Compare SB Register 
  POCR0SBL : byte absolute $00+$A4; // PSC Output Compare SB Register 
  POCR0SBH : byte absolute $00+$A4+1; // PSC Output Compare SB Register 
  POCR0RA : word absolute $00+$A2; // PSC Module 0 Output Compare RA Register 
  POCR0RAL : byte absolute $00+$A2; // PSC Module 0 Output Compare RA Register 
  POCR0RAH : byte absolute $00+$A2+1; // PSC Module 0 Output Compare RA Register 
  POCR0SA : word absolute $00+$A0; // PSC Module 0 Output Compare SA Register 
  POCR0SAL : byte absolute $00+$A0; // PSC Module 0 Output Compare SA Register 
  POCR0SAH : byte absolute $00+$A0+1; // PSC Module 0 Output Compare SA Register 

const
  // CANGCON
  ABRQ = 7; // Abort Request
  OVRQ = 6; // Overload Frame Request
  TTC = 5; // Time Trigger Communication
  SYNTTC = 4; // Synchronization of TTC
  LISTEN = 3; // Listening Mode
  TEST = 2; // Test Mode
  ENASTB = 1; // Enable / Standby
  SWRES = 0; // Software Reset Request
  // CANGSTA
  OVFG = 6; // Overload Frame Flag
  TXBSY = 4; // Transmitter Busy
  RXBSY = 3; // Receiver Busy
  ENFG = 2; // Enable Flag
  BOFF = 1; // Bus Off Mode
  ERRP = 0; // Error Passive Mode
  // CANGIT
  CANIT = 7; // General Interrupt Flag
  BOFFIT = 6; // Bus Off Interrupt Flag
  OVRTIM = 5; // Overrun CAN Timer Flag
  BXOK = 4; // Burst Receive Interrupt Flag
  SERG = 3; // Stuff Error General Flag
  CERG = 2; // CRC Error General Flag
  FERG = 1; // Form Error General Flag
  AERG = 0; // Ackknowledgement Error General Flag
  // CANGIE
  ENIT = 7; // Enable all Interrupts
  ENBOFF = 6; // Enable Bus Off Interrupt
  ENRX = 5; // Enable Receive Interrupt
  ENTX = 4; // Enable Transmitt Interrupt
  ENERR = 3; // Enable MOb Error Interrupt
  ENBX = 2; // Enable Burst Receive Interrupt
  ENERG = 1; // Enable General Error Interrupt
  ENOVRT = 0; // Enable CAN Timer Overrun Interrupt
  // CANEN2
  ENMOB = 0; // Enable MObs
  // CANIE2
  IEMOB = 0; // Interrupt Enable  MObs
  // CANSIT2
  SIT = 0; // Status of Interrupt MObs
  // CANBT1
  BRP = 1; // Baud Rate Prescaler bits
  // CANBT2
  SJW = 5; // Re-Sync Jump Width bits
  PRS = 1; // Propagation Time Segment bits
  // CANBT3
  PHS2 = 4; // Phase Segment 2 bits
  PHS1 = 1; // Phase Segment 1 bits
  SMP = 0; // Sample Type
  // CANHPMOB
  HPMOB = 4; // Highest Priority MOb Number bits
  CGP = 0; // CAN General Purpose bits
  // CANPAGE
  MOBNB = 4; // MOb Number bits
  AINC = 3; // MOb Data Buffer Auto Increment (Active Low)
  INDX = 0; // Data Buffer Index bits
  // CANSTMOB
  DLCW = 7; // Data Length Code Warning on MOb
  TXOK = 6; // Transmit OK on MOb
  RXOK = 5; // Receive OK on MOb
  BERR = 4; // Bit Error on MOb
  SERR = 3; // Stuff Error on MOb
  CERR = 2; // CRC Error on MOb
  FERR = 1; // Form Error on MOb
  AERR = 0; // Ackknowledgement Error on MOb
  // CANCDMOB
  CONMOB = 6; // MOb Config bits
  RPLV = 5; // Reply Valid
  IDE = 4; // Identifier Extension
  DLC = 0; // Data Length Code bits
  // CANIDT4
  IDT = 3; // 
  RTRTAG = 2; // 
  RB1TAG = 1; // 
  RB0TAG = 0; // 
  // AC0CON
  AC0EN = 7; // Analog Comparator 0 Enable Bit
  AC0IE = 6; // Analog Comparator 0 Interrupt Enable Bit
  AC0IS = 4; // Analog Comparator 0  Interrupt Select Bits
  ACCKSEL = 3; // Analog Comparator Clock Select
  AC0M = 0; // Analog Comparator 0 Multiplexer Register
  // AC1CON
  AC1EN = 7; // Analog Comparator 1 Enable Bit
  AC1IE = 6; // Analog Comparator 1 Interrupt Enable Bit
  AC1IS = 4; // Analog Comparator 1  Interrupt Select Bit
  AC1ICE = 3; // Analog Comparator 1 Interrupt Capture Enable Bit
  AC1M = 0; // Analog Comparator 1 Multiplexer Register
  // AC2CON
  AC2EN = 7; // Analog Comparator 2 Enable Bit
  AC2IE = 6; // Analog Comparator 2 Interrupt Enable Bit
  AC2IS = 4; // Analog Comparator 2  Interrupt Select Bit
  AC2M = 0; // Analog Comparator 2 Multiplexer Register
  // AC3CON
  AC3EN = 7; // Analog Comparator 3 Enable Bit
  AC3IE = 6; // Analog Comparator 3 Interrupt Enable Bit
  AC3IS = 4; // Analog Comparator 3  Interrupt Select Bit
  AC3M = 0; // Analog Comparator 3 Multiplexer Register
  // ACSR
  AC3IF = 7; // Analog Comparator 3 Interrupt Flag Bit
  AC2IF = 6; // Analog Comparator 2 Interrupt Flag Bit
  AC1IF = 5; // Analog Comparator 1  Interrupt Flag Bit
  AC0IF = 4; // Analog Comparator 0 Interrupt Flag Bit
  AC3O = 3; // Analog Comparator 3 Output Bit
  AC2O = 2; // Analog Comparator 2 Output Bit
  AC1O = 1; // Analog Comparator 1 Output Bit
  AC0O = 0; // Analog Comparator 0 Output Bit
  // DACH
  // DACL
  // DACON
  DAATE = 7; // DAC Auto Trigger Enable Bit
  DATS = 4; // DAC Trigger Selection Bits
  DALA = 2; // DAC Left Adjust
  DAEN = 0; // DAC Enable Bit
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read While Write Section Busy
  SIGRD = 5; // Signature Row Read
  RWWSRE = 4; // Read While Write section read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
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
  SPIPS = 7; // SPI Pin Select
  PUD = 4; // Pull-up disable
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
  PLLF = 2; // PLL Factor
  PLLE = 1; // PLL Enable
  PLOCK = 0; // PLL Lock Detector
  // PRR
  PRCAN = 6; // Power Reduction CAN
  PRPSC = 5; // Power Reduction PSC
  PRTIM1 = 4; // Power Reduction Timer/Counter1
  PRTIM0 = 3; // Power Reduction Timer/Counter0
  PRSPI = 2; // Power Reduction Serial Peripheral Interface
  PRLIN = 1; // Power Reduction LIN UART
  PRADC = 0; // Power Reduction ADC
  // TIMSK0
  OCIE0B = 2; // Timer/Counter0 Output Compare Match B Interrupt Enable
  OCIE0A = 1; // Timer/Counter0 Output Compare Match A Interrupt Enable
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR0
  OCF0B = 2; // Timer/Counter0 Output Compare Flag 0B
  OCF0A = 1; // Timer/Counter0 Output Compare Flag 0A
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // TCCR0A
  COM0A = 6; // Compare Output Mode, Phase Correct PWM Mode
  COM0B = 4; // Compare Output Mode, Fast PWm
  WGM0 = 0; // Waveform Generation Mode
  // TCCR0B
  FOC0A = 7; // Force Output Compare A
  FOC0B = 6; // Force Output Compare B
  WGM02 = 3; // 
  CS0 = 0; // Clock Select
  // GTCCR
  TSM = 7; // Timer/Counter Synchronization Mode
  ICPSEL1 = 6; // Timer1 Input Capture Selection Bit
  PSR10 = 0; // Prescaler Reset Timer/Counter1 and Timer/Counter0
  // TIMSK1
  ICIE1 = 5; // Timer/Counter1 Input Capture Interrupt Enable
  OCIE1B = 2; // Timer/Counter1 Output CompareB Match Interrupt Enable
  OCIE1A = 1; // Timer/Counter1 Output CompareA Match Interrupt Enable
  TOIE1 = 0; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR1
  ICF1 = 5; // Input Capture Flag 1
  OCF1B = 2; // Output Compare Flag 1B
  OCF1A = 1; // Output Compare Flag 1A
  TOV1 = 0; // Timer/Counter1 Overflow Flag
  // TCCR1A
  COM1A = 6; // Compare Output Mode 1A, bits
  COM1B = 4; // Compare Output Mode 1B, bits
  WGM1 = 0; // Waveform Generation Mode
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceler
  ICES1 = 6; // Input Capture 1 Edge Select
  CS1 = 0; // Prescaler source of Timer/Counter 1
  // TCCR1C
  FOC1A = 7; // 
  FOC1B = 6; // 
  // GTCCR
  PSRSYNC = 0; // Prescaler Reset Timer/Counter1 and Timer/Counter0
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
  ISRCEN = 6; // Current Source Enable
  AREFEN = 5; // Analog Reference pin Enable
  ADTS = 0; // ADC Auto Trigger Sources
  // DIDR0
  ADC7D = 7; // ADC7 Digital input Disable
  ADC6D = 6; // ADC6 Digital input Disable
  ADC5D = 5; // ADC5 Digital input Disable
  ADC4D = 4; // ADC4 Digital input Disable
  ADC3D = 3; // ADC3 Digital input Disable
  ADC2D = 2; // ADC2 Digital input Disable
  ADC1D = 1; // ADC1 Digital input Disable
  ADC0D = 0; // ADC0 Digital input Disable
  // DIDR1
  AMP2PD = 6; // AMP2P Pin Digital input Disable
  ACMP0D = 5; // ACMP0 Pin Digital input Disable
  AMP0PD = 4; // AMP0P Pin Digital input Disable
  AMP0ND = 3; // AMP0N Pin Digital input Disable
  ADC10D = 2; // ADC10 Pin Digital input Disable
  ADC9D = 1; // ADC9 Pin Digital input Disable
  ADC8D = 0; // ADC8 Pin Digital input Disable
  // AMP0CSR
  AMP0EN = 7; // 
  AMP0IS = 6; // 
  AMP0G = 4; // 
  AMPCMP0 = 3; // Amplifier 0 - Comparator 0 Connection
  AMP0TS = 0; // 
  // AMP1CSR
  AMP1EN = 7; // 
  AMP1IS = 6; // 
  AMP1G = 4; // 
  AMPCMP1 = 3; // Amplifier 1 - Comparator 1 Connection
  AMP1TS = 0; // 
  // AMP2CSR
  AMP2EN = 7; // 
  AMP2IS = 6; // 
  AMP2G = 4; // 
  AMPCMP2 = 3; // Amplifier 2 - Comparator 2 Connection
  AMP2TS = 0; // 
  // LINCR
  LSWRES = 7; // Software Reset
  LIN13 = 6; // LIN Standard
  LCONF = 4; // LIN Configuration bits
  LENA = 3; // LIN or UART Enable
  LCMD = 0; // LIN Command and Mode bits
  // LINSIR
  LIDST = 5; // Identifier Status bits
  LBUSY = 4; // Busy Signal
  LERR = 3; // Error Interrupt
  LIDOK = 2; // Identifier Interrupt
  LTXOK = 1; // Transmit Performed Interrupt
  LRXOK = 0; // Receive Performed Interrupt
  // LINENIR
  LENERR = 3; // Enable Error Interrupt
  LENIDOK = 2; // Enable Identifier Interrupt
  LENTXOK = 1; // Enable Transmit Performed Interrupt
  LENRXOK = 0; // Enable Receive Performed Interrupt
  // LINERR
  LABORT = 7; // Abort Flag
  LTOERR = 6; // Frame Time Out Error Flag
  LOVERR = 5; // Overrun Error Flag
  LFERR = 4; // Framing Error Flag
  LSERR = 3; // Synchronization Error Flag
  LPERR = 2; // Parity Error Flag
  LCERR = 1; // Checksum Error Flag
  LBERR = 0; // Bit Error Flag
  // LINBTR
  LDISR = 7; // Disable Bit Timing Resynchronization
  LBT = 0; // LIN Bit Timing bits
  // LINBRRL
  LDIV = 0; // 
  // LINBRRH
  // LINDLR
  LTXDL = 4; // LIN Transmit Data Length bits
  LRXDL = 0; // LIN Receive Data Length bits
  // LINIDR
  LP = 6; // Parity bits
  LID = 0; // Identifier bit 5 or Data Length bits
  // LINSEL
  LAINC = 3; // Auto Increment of Data Buffer Index (Active Low)
  LINDX = 0; // FIFO LIN Data Buffer Index bits
  // LINDAT
  LDATA = 0; // 
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
  ISC3 = 6; // External Interrupt Sense Control Bit
  ISC2 = 4; // External Interrupt Sense Control Bit
  ISC1 = 2; // External Interrupt Sense Control 1 Bits
  ISC0 = 0; // External Interrupt Sense Control 0 Bits
  // EIMSK
  INT = 0; // External Interrupt Request 3 Enable
  // EIFR
  INTF = 0; // External Interrupt Flags
  // PCICR
  PCIE = 0; // Pin Change Interrupt Enables
  // PCMSK3
  PCINT = 0; // Pin Change Enable Masks
  // PCMSK2
  // PCMSK1
  // PCMSK0
  // PCIFR
  PCIF = 0; // Pin Change Interrupt Flags
  // EECR
  EEPM = 4; // 
  EERIE = 3; // EEProm Ready Interrupt Enable
  EEMWE = 2; // EEPROM Master Write Enable
  EEWE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // PIFR
  PEV = 1; // PSC External Event 2 Interrupt
  PEOP = 0; // PSC End of Cycle Interrupt
  // PIM
  PEVE = 1; // External Event 2 Interrupt Enable
  PEOPE = 0; // PSC End of Cycle Interrupt Enable
  // PMIC2
  POVEN2 = 7; // PSC Module 2 Overlap Enable
  PISEL2 = 6; // PSC Module 2 Input Select
  PELEV2 = 5; // PSC Module 2 Input Level Selector
  PFLTE2 = 4; // PSC Module 2 Input Filter Enable
  PAOC2 = 3; // PSC Module 2 Asynchronous Output Control
  PRFM2 = 0; // PSC Module 2 Input Mode bits
  // PMIC1
  POVEN1 = 7; // PSC Module 1 Overlap Enable
  PISEL1 = 6; // PSC Module 1 Input Select
  PELEV1 = 5; // PSC Module 1 Input Level Selector
  PFLTE1 = 4; // PSC Module 1 Input Filter Enable
  PAOC1 = 3; // PSC Module 1 Asynchronous Output Control
  PRFM1 = 0; // PSC Module 1 Input Mode bits
  // PMIC0
  POVEN0 = 7; // PSC Module 0 Overlap Enable
  PISEL0 = 6; // PSC Module 0 Input Select
  PELEV0 = 5; // PSC Module 0 Input Level Selector
  PFLTE0 = 4; // PSC Module 0 Input Filter Enable
  PAOC0 = 3; // PSC Module 0 Asynchronous Output Control
  PRFM0 = 0; // PSC Module 0 Input Mode bits
  // PCTL
  PPRE = 6; // PSC Prescaler Select bits
  PCLKSEL = 5; // PSC Input Clock Select
  PCCYC = 1; // PSC Complete Cycle
  PRUN = 0; // PSC Run
  // POC
  POEN2B = 5; // PSC Output 2B Enable
  POEN2A = 4; // PSC Output 2A Enable
  POEN1B = 3; // PSC Output 1B Enable
  POEN1A = 2; // PSC Output 1A Enable
  POEN0B = 1; // PSC Output 0B Enable
  POEN0A = 0; // PSC Output 0A Enable
  // PCNF
  PULOCK = 5; // PSC Update Lock
  PMODE = 4; // PSC Mode
  POPB = 3; // PSC Output B Polarity
  POPA = 2; // PSC Output A Polarity
  // PSYNC
  PSYNC2 = 4; // Selection of Synchronization Out for ADC
  PSYNC1 = 2; // Selection of Synchronization Out for ADC
  PSYNC0 = 0; // Selection of Synchronization Out for ADC

implementation

{$i avrcommon.inc}

procedure ANACOMP0_ISR; external name 'ANACOMP0_ISR'; // Interrupt 1 Analog Comparator 0
procedure ANACOMP1_ISR; external name 'ANACOMP1_ISR'; // Interrupt 2 Analog Comparator 1
procedure ANACOMP2_ISR; external name 'ANACOMP2_ISR'; // Interrupt 3 Analog Comparator 2
procedure ANACOMP3_ISR; external name 'ANACOMP3_ISR'; // Interrupt 4 Analog Comparator 3
procedure PSC_FAULT_ISR; external name 'PSC_FAULT_ISR'; // Interrupt 5 PSC Fault
procedure PSC_EC_ISR; external name 'PSC_EC_ISR'; // Interrupt 6 PSC End of Cycle
procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 7 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 8 External Interrupt Request 1
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 9 External Interrupt Request 2
procedure INT3_ISR; external name 'INT3_ISR'; // Interrupt 10 External Interrupt Request 3
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 11 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 12 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 13 Timer/Counter1 Compare Match B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 14 Timer1/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 15 Timer/Counter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 16 Timer/Counter0 Compare Match B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 17 Timer/Counter0 Overflow
procedure CAN_INT_ISR; external name 'CAN_INT_ISR'; // Interrupt 18 CAN MOB, Burst, General Errors
procedure CAN_TOVF_ISR; external name 'CAN_TOVF_ISR'; // Interrupt 19 CAN Timer Overflow
procedure LIN_TC_ISR; external name 'LIN_TC_ISR'; // Interrupt 20 LIN Transfer Complete
procedure LIN_ERR_ISR; external name 'LIN_ERR_ISR'; // Interrupt 21 LIN Error
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 22 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 23 Pin Change Interrupt Request 1
procedure PCINT2_ISR; external name 'PCINT2_ISR'; // Interrupt 24 Pin Change Interrupt Request 2
procedure PCINT3_ISR; external name 'PCINT3_ISR'; // Interrupt 25 Pin Change Interrupt Request 3
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 26 SPI Serial Transfer Complete
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 27 ADC Conversion Complete
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 28 Watchdog Time-Out Interrupt
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 29 EEPROM Ready
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 30 Store Program Memory Read

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   jmp _start
   jmp ANACOMP0_ISR
   jmp ANACOMP1_ISR
   jmp ANACOMP2_ISR
   jmp ANACOMP3_ISR
   jmp PSC_FAULT_ISR
   jmp PSC_EC_ISR
   jmp INT0_ISR
   jmp INT1_ISR
   jmp INT2_ISR
   jmp INT3_ISR
   jmp TIMER1_CAPT_ISR
   jmp TIMER1_COMPA_ISR
   jmp TIMER1_COMPB_ISR
   jmp TIMER1_OVF_ISR
   jmp TIMER0_COMPA_ISR
   jmp TIMER0_COMPB_ISR
   jmp TIMER0_OVF_ISR
   jmp CAN_INT_ISR
   jmp CAN_TOVF_ISR
   jmp LIN_TC_ISR
   jmp LIN_ERR_ISR
   jmp PCINT0_ISR
   jmp PCINT1_ISR
   jmp PCINT2_ISR
   jmp PCINT3_ISR
   jmp SPI__STC_ISR
   jmp ADC_ISR
   jmp WDT_ISR
   jmp EE_READY_ISR
   jmp SPM_READY_ISR

   {$i start.inc}

   .weak ANACOMP0_ISR
   .weak ANACOMP1_ISR
   .weak ANACOMP2_ISR
   .weak ANACOMP3_ISR
   .weak PSC_FAULT_ISR
   .weak PSC_EC_ISR
   .weak INT0_ISR
   .weak INT1_ISR
   .weak INT2_ISR
   .weak INT3_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak TIMER0_OVF_ISR
   .weak CAN_INT_ISR
   .weak CAN_TOVF_ISR
   .weak LIN_TC_ISR
   .weak LIN_ERR_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak PCINT2_ISR
   .weak PCINT3_ISR
   .weak SPI__STC_ISR
   .weak ADC_ISR
   .weak WDT_ISR
   .weak EE_READY_ISR
   .weak SPM_READY_ISR

   .set ANACOMP0_ISR, Default_IRQ_handler
   .set ANACOMP1_ISR, Default_IRQ_handler
   .set ANACOMP2_ISR, Default_IRQ_handler
   .set ANACOMP3_ISR, Default_IRQ_handler
   .set PSC_FAULT_ISR, Default_IRQ_handler
   .set PSC_EC_ISR, Default_IRQ_handler
   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set INT3_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set CAN_INT_ISR, Default_IRQ_handler
   .set CAN_TOVF_ISR, Default_IRQ_handler
   .set LIN_TC_ISR, Default_IRQ_handler
   .set LIN_ERR_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set PCINT2_ISR, Default_IRQ_handler
   .set PCINT3_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set SPM_READY_ISR, Default_IRQ_handler
 end;

end.
