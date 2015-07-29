unit AT90PWM216;

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
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  // EUSART
  EUDR : byte absolute $00+$CE; // EUSART I/O Data Register
  EUCSRA : byte absolute $00+$C8; // EUSART Control and Status Register A
  EUCSRB : byte absolute $00+$C9; // EUSART Control Register B
  EUCSRC : byte absolute $00+$CA; // EUSART Status Register C
  MUBRRH : byte absolute $00+$CD; // Manchester Receiver Baud Rate Register High Byte
  MUBRRL : byte absolute $00+$CC; // Manchester Receiver Baud Rate Register Low Byte
  // ANALOG_COMPARATOR
  AC0CON : byte absolute $00+$AD; // Analog Comparator 0 Control Register
  AC1CON : byte absolute $00+$AE; // Analog Comparator 1 Control Register
  AC2CON : byte absolute $00+$AF; // Analog Comparator 2 Control Register
  ACSR : byte absolute $00+$50; // Analog Comparator Status Register
  // DA_CONVERTER
  DACH : byte absolute $00+$AC; // DAC Data Register High Byte
  DACL : byte absolute $00+$AB; // DAC Data Register Low Byte
  DACON : byte absolute $00+$AA; // DAC Control Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  OSCCAL : byte absolute $00+$66; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$61; // 
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  GPIOR3 : byte absolute $00+$3B; // General Purpose IO Register 3
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
  AMP0CSR : byte absolute $00+$76; // 
  AMP1CSR : byte absolute $00+$77; // 
  // USART
  UDR : byte absolute $00+$C6; // USART I/O Data Register
  UCSRA : byte absolute $00+$C0; // USART Control and Status register A
  UCSRB : byte absolute $00+$C1; // USART Control an Status register B
  UCSRC : byte absolute $00+$C2; // USART Control an Status register C
  UBRRH : byte absolute $00+$C5; // USART Baud Rate Register High Byte
  UBRRL : byte absolute $00+$C4; // USART Baud Rate Register Low Byte
  // SPI
  SPCR : byte absolute $00+$4C; // SPI Control Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPDR : byte absolute $00+$4E; // SPI Data Register
  // WATCHDOG
  WDTCSR : byte absolute $00+$60; // Watchdog Timer Control Register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register A
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Read/Write Access  Bytes
  EEARL : byte absolute $00+$41; // EEPROM Read/Write Access  Bytes
  EEARH : byte absolute $00+$41+1; // EEPROM Read/Write Access  Bytes
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // PSC0
  PICR0 : word absolute $00+$DE; // PSC 0 Input Capture Register 
  PICR0L : byte absolute $00+$DE; // PSC 0 Input Capture Register 
  PICR0H : byte absolute $00+$DE+1; // PSC 0 Input Capture Register 
  PFRC0B : byte absolute $00+$DD; // PSC 0 Input B Control
  PFRC0A : byte absolute $00+$DC; // PSC 0 Input A Control
  PCTL0 : byte absolute $00+$DB; // PSC 0 Control Register
  PCNF0 : byte absolute $00+$DA; // PSC 0 Configuration Register
  OCR0RB : word absolute $00+$D8; // Output Compare RB Register 
  OCR0RBL : byte absolute $00+$D8; // Output Compare RB Register 
  OCR0RBH : byte absolute $00+$D8+1; // Output Compare RB Register 
  OCR0SB : word absolute $00+$D6; // Output Compare SB Register 
  OCR0SBL : byte absolute $00+$D6; // Output Compare SB Register 
  OCR0SBH : byte absolute $00+$D6+1; // Output Compare SB Register 
  OCR0RA : word absolute $00+$D4; // Output Compare RA Register 
  OCR0RAL : byte absolute $00+$D4; // Output Compare RA Register 
  OCR0RAH : byte absolute $00+$D4+1; // Output Compare RA Register 
  OCR0SA : word absolute $00+$D2; // Output Compare SA Register 
  OCR0SAL : byte absolute $00+$D2; // Output Compare SA Register 
  OCR0SAH : byte absolute $00+$D2+1; // Output Compare SA Register 
  PSOC0 : byte absolute $00+$D0; // PSC0 Synchro and Output Configuration
  PIM0 : byte absolute $00+$A1; // PSC0 Interrupt Mask Register
  PIFR0 : byte absolute $00+$A0; // PSC0 Interrupt Flag Register
  // PSC2
  PICR2 : word absolute $00+$FE; // PSC 2 Input Capture Register 
  PICR2L : byte absolute $00+$FE; // PSC 2 Input Capture Register 
  PICR2H : byte absolute $00+$FE+1; // PSC 2 Input Capture Register 
  PFRC2B : byte absolute $00+$FD; // PSC 2 Input B Control
  PFRC2A : byte absolute $00+$FC; // PSC 2 Input B Control
  PCTL2 : byte absolute $00+$FB; // PSC 2 Control Register
  PCNF2 : byte absolute $00+$FA; // PSC 2 Configuration Register
  OCR2RB : word absolute $00+$F8; // Output Compare RB Register 
  OCR2RBL : byte absolute $00+$F8; // Output Compare RB Register 
  OCR2RBH : byte absolute $00+$F8+1; // Output Compare RB Register 
  OCR2SB : word absolute $00+$F6; // Output Compare SB Register 
  OCR2SBL : byte absolute $00+$F6; // Output Compare SB Register 
  OCR2SBH : byte absolute $00+$F6+1; // Output Compare SB Register 
  OCR2RA : word absolute $00+$F4; // Output Compare RA Register 
  OCR2RAL : byte absolute $00+$F4; // Output Compare RA Register 
  OCR2RAH : byte absolute $00+$F4+1; // Output Compare RA Register 
  OCR2SA : word absolute $00+$F2; // Output Compare SA Register 
  OCR2SAL : byte absolute $00+$F2; // Output Compare SA Register 
  OCR2SAH : byte absolute $00+$F2+1; // Output Compare SA Register 
  POM2 : byte absolute $00+$F1; // PSC 2 Output Matrix
  PSOC2 : byte absolute $00+$F0; // PSC2 Synchro and Output Configuration
  PIM2 : byte absolute $00+$A5; // PSC2 Interrupt Mask Register
  PIFR2 : byte absolute $00+$A4; // PSC2 Interrupt Flag Register

const
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read While Write Section Busy
  RWWSRE = 4; // Read While Write section read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // EUCSRA
  UTxS = 4; // EUSART Control and Status Register A Bits
  URxS = 0; // EUSART Control and Status Register A Bits
  // EUCSRB
  EUSART = 4; // EUSART Enable Bit
  EUSBS = 3; // EUSBS Enable Bit
  EMCH = 1; // Manchester Mode Bit
  BODR = 0; // Order Bit
  // EUCSRC
  FEM = 3; // Frame Error Manchester Bit
  F1617 = 2; // F1617 Bit
  STP = 0; // Stop Bits
  // MUBRRH
  MUBRR = 0; // Manchester Receiver Baud Rate Register Bits
  // MUBRRL
  // AC0CON
  AC0EN = 7; // Analog Comparator 0 Enable Bit
  AC0IE = 6; // Analog Comparator 0 Interrupt Enable Bit
  AC0IS = 4; // Analog Comparator 0  Interrupt Select Bit
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
  // ACSR
  ACCKDIV = 7; // Analog Comparator Clock Divider
  AC2IF = 6; // Analog Comparator 2 Interrupt Flag Bit
  AC1IF = 5; // Analog Comparator 1  Interrupt Flag Bit
  AC0IF = 4; // Analog Comparator 0 Interrupt Flag Bit
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
  // GPIOR3
  GPIOR = 0; // General Purpose IO Register 3 bis
  // GPIOR2
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
  PRPSC = 5; // Power Reduction PSC2
  PRTIM1 = 4; // Power Reduction Timer/Counter1
  PRTIM0 = 3; // Power Reduction Timer/Counter0
  PRSPI = 2; // Power Reduction Serial Peripheral Interface
  PRUSART0 = 1; // Power Reduction USART
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
  // DIDR1
  ACMP0D = 5; // 
  AMP0PD = 4; // 
  AMP0ND = 3; // 
  ADC10D = 2; // 
  ADC9D = 1; // 
  ADC8D = 0; // 
  // AMP0CSR
  AMP0EN = 7; // 
  AMP0IS = 6; // 
  AMP0G = 4; // 
  AMP0TS = 0; // 
  // AMP1CSR
  AMP1EN = 7; // 
  AMP1IS = 6; // 
  AMP1G = 4; // 
  AMP1TS = 0; // 
  // UCSRA
  RXC = 7; // USART Receive Complete
  TXC = 6; // USART Transmitt Complete
  UDRE = 5; // USART Data Register Empty
  FE = 4; // Framing Error
  DOR = 3; // Data Overrun
  UPE = 2; // USART Parity Error
  U2X = 1; // Double USART Transmission Bit
  MPCM = 0; // Multi-processor Communication Mode
  // UCSRB
  RXCIE = 7; // RX Complete Interrupt Enable
  TXCIE = 6; // TX Complete Interrupt Enable
  UDRIE = 5; // USART Data Register Empty Interrupt Enable
  RXEN = 4; // Receiver Enable
  TXEN = 3; // Transmitter Enable
  UCSZ2 = 2; // Character Size
  RXB8 = 1; // Receive Data Bit 8
  TXB8 = 0; // Transmit Data Bit 8
  // UCSRC
  UMSEL0 = 6; // USART Mode Select
  UPM = 4; // Parity Mode Bits
  USBS = 3; // Stop Bit Select
  UCSZ = 1; // Character Size Bits
  UCPOL = 0; // Clock Polarity
  // UBRRH
  UBRR = 0; // USART Baud Rate Register Bits
  // UBRRL
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
  // EECR
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
  PBFM0 = 5; // PSC 0 Balance Flank Width Modulation
  PAOC0B = 4; // PSC 0 Asynchronous Output Control B
  PAOC0A = 3; // PSC 0 Asynchronous Output Control A
  PARUN0 = 2; // PSC0 Auto Run
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
  PSYNC0 = 4; // Synchronization Out for ADC Selection
  POEN0B = 2; // PSCOUT01 Output Enable
  POEN0A = 0; // PSCOUT00 Output Enable
  // PIM0
  PSEIE0 = 5; // PSC 0 Synchro Error Interrupt Enable
  PEVE0B = 4; // External Event B Interrupt Enable
  PEVE0A = 3; // External Event A Interrupt Enable
  PEOPE0 = 0; // End of Cycle Interrupt Enable
  // PIFR0
  POAC0B = 7; // PSC 0 Output A Activity
  POAC0A = 6; // PSC 0 Output A Activity
  PSEI0 = 5; // PSC 0 Synchro Error Interrupt
  PEV0B = 4; // External Event B Interrupt
  PEV0A = 3; // External Event A Interrupt
  PRN0 = 1; // Ramp Number
  PEOP0 = 0; // End of PSC0 Interrupt
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
  // POM2
  POMV2B = 4; // Output Matrix Output B Ramps
  POMV2A = 0; // Output Matrix Output A Ramps
  // PSOC2
  POS2 = 6; // PSC 2 Output 23 Select
  PSYNC2_ = 4; // Synchronization Out for ADC Selection
  POEN2D = 3; // PSCOUT23 Output Enable
  POEN2B = 2; // PSCOUT21 Output Enable
  POEN2C = 1; // PSCOUT22 Output Enable
  POEN2A = 0; // PSCOUT20 Output Enable
  // PIM2
  PSEIE2 = 5; // PSC 2 Synchro Error Interrupt Enable
  PEVE2B = 4; // External Event B Interrupt Enable
  PEVE2A = 3; // External Event A Interrupt Enable
  PEOPE2 = 0; // End of Cycle Interrupt Enable
  // PIFR2
  POAC2B = 7; // PSC 2 Output A Activity
  POAC2A = 6; // PSC 2 Output A Activity
  PSEI2 = 5; // PSC 2 Synchro Error Interrupt
  PEV2B = 4; // External Event B Interrupt
  PEV2A = 3; // External Event A Interrupt
  PRN2 = 1; // Ramp Number
  PEOP2 = 0; // End of PSC2 Interrupt

implementation

{$i avrcommon.inc}

procedure PSC2_CAPT_ISR; external name 'PSC2_CAPT_ISR'; // Interrupt 1 PSC2 Capture Event
procedure PSC2_EC_ISR; external name 'PSC2_EC_ISR'; // Interrupt 2 PSC2 End Cycle
procedure PSC1_CAPT_ISR; external name 'PSC1_CAPT_ISR'; // Interrupt 3 PSC1 Capture Event
procedure PSC1_EC_ISR; external name 'PSC1_EC_ISR'; // Interrupt 4 PSC1 End Cycle
procedure PSC0_CAPT_ISR; external name 'PSC0_CAPT_ISR'; // Interrupt 5 PSC0 Capture Event
procedure PSC0_EC_ISR; external name 'PSC0_EC_ISR'; // Interrupt 6 PSC0 End Cycle
procedure ANALOG_COMP_0_ISR; external name 'ANALOG_COMP_0_ISR'; // Interrupt 7 Analog Comparator 0
procedure ANALOG_COMP_1_ISR; external name 'ANALOG_COMP_1_ISR'; // Interrupt 8 Analog Comparator 1
procedure ANALOG_COMP_2_ISR; external name 'ANALOG_COMP_2_ISR'; // Interrupt 9 Analog Comparator 2
procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 10 External Interrupt Request 0
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 11 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 12 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 13 Timer/Counter Compare Match B
procedure RESERVED15_ISR; external name 'RESERVED15_ISR'; // Interrupt 14 
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 15 Timer/Counter1 Overflow
procedure TIMER0_COMP_A_ISR; external name 'TIMER0_COMP_A_ISR'; // Interrupt 16 Timer/Counter0 Compare Match A
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 17 Timer/Counter0 Overflow
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 18 ADC Conversion Complete
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 19 External Interrupt Request 1
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 20 SPI Serial Transfer Complete
procedure USART__RX_ISR; external name 'USART__RX_ISR'; // Interrupt 21 USART, Rx Complete
procedure USART__UDRE_ISR; external name 'USART__UDRE_ISR'; // Interrupt 22 USART Data Register Empty
procedure USART__TX_ISR; external name 'USART__TX_ISR'; // Interrupt 23 USART, Tx Complete
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 24 External Interrupt Request 2
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 25 Watchdog Timeout Interrupt
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 26 EEPROM Ready
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 27 Timer Counter 0 Compare Match B
procedure INT3_ISR; external name 'INT3_ISR'; // Interrupt 28 External Interrupt Request 3
procedure RESERVED30_ISR; external name 'RESERVED30_ISR'; // Interrupt 29 
procedure RESERVED31_ISR; external name 'RESERVED31_ISR'; // Interrupt 30 
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 31 Store Program Memory Read

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   jmp _start
   jmp PSC2_CAPT_ISR
   jmp PSC2_EC_ISR
   jmp PSC1_CAPT_ISR
   jmp PSC1_EC_ISR
   jmp PSC0_CAPT_ISR
   jmp PSC0_EC_ISR
   jmp ANALOG_COMP_0_ISR
   jmp ANALOG_COMP_1_ISR
   jmp ANALOG_COMP_2_ISR
   jmp INT0_ISR
   jmp TIMER1_CAPT_ISR
   jmp TIMER1_COMPA_ISR
   jmp TIMER1_COMPB_ISR
   jmp RESERVED15_ISR
   jmp TIMER1_OVF_ISR
   jmp TIMER0_COMP_A_ISR
   jmp TIMER0_OVF_ISR
   jmp ADC_ISR
   jmp INT1_ISR
   jmp SPI__STC_ISR
   jmp USART__RX_ISR
   jmp USART__UDRE_ISR
   jmp USART__TX_ISR
   jmp INT2_ISR
   jmp WDT_ISR
   jmp EE_READY_ISR
   jmp TIMER0_COMPB_ISR
   jmp INT3_ISR
   jmp RESERVED30_ISR
   jmp RESERVED31_ISR
   jmp SPM_READY_ISR

   {$i start.inc}

   .weak PSC2_CAPT_ISR
   .weak PSC2_EC_ISR
   .weak PSC1_CAPT_ISR
   .weak PSC1_EC_ISR
   .weak PSC0_CAPT_ISR
   .weak PSC0_EC_ISR
   .weak ANALOG_COMP_0_ISR
   .weak ANALOG_COMP_1_ISR
   .weak ANALOG_COMP_2_ISR
   .weak INT0_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak RESERVED15_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMP_A_ISR
   .weak TIMER0_OVF_ISR
   .weak ADC_ISR
   .weak INT1_ISR
   .weak SPI__STC_ISR
   .weak USART__RX_ISR
   .weak USART__UDRE_ISR
   .weak USART__TX_ISR
   .weak INT2_ISR
   .weak WDT_ISR
   .weak EE_READY_ISR
   .weak TIMER0_COMPB_ISR
   .weak INT3_ISR
   .weak RESERVED30_ISR
   .weak RESERVED31_ISR
   .weak SPM_READY_ISR

   .set PSC2_CAPT_ISR, Default_IRQ_handler
   .set PSC2_EC_ISR, Default_IRQ_handler
   .set PSC1_CAPT_ISR, Default_IRQ_handler
   .set PSC1_EC_ISR, Default_IRQ_handler
   .set PSC0_CAPT_ISR, Default_IRQ_handler
   .set PSC0_EC_ISR, Default_IRQ_handler
   .set ANALOG_COMP_0_ISR, Default_IRQ_handler
   .set ANALOG_COMP_1_ISR, Default_IRQ_handler
   .set ANALOG_COMP_2_ISR, Default_IRQ_handler
   .set INT0_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set RESERVED15_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMP_A_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set USART__RX_ISR, Default_IRQ_handler
   .set USART__UDRE_ISR, Default_IRQ_handler
   .set USART__TX_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set INT3_ISR, Default_IRQ_handler
   .set RESERVED30_ISR, Default_IRQ_handler
   .set RESERVED31_ISR, Default_IRQ_handler
   .set SPM_READY_ISR, Default_IRQ_handler
 end;

end.
