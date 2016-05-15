unit AT90CAN64;

{$goto on}

interface

var
  // PORTA
  PORTA : byte absolute $00+$22; // Port A Data Register
  DDRA : byte absolute $00+$21; // Port A Data Direction Register
  PINA : byte absolute $00+$20; // Port A Input Pins
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
  // PORTE
  PORTE : byte absolute $00+$2E; // Data Register, Port E
  DDRE : byte absolute $00+$2D; // Data Direction Register, Port E
  PINE : byte absolute $00+$2C; // Input Pins, Port E
  // PORTF
  PORTF : byte absolute $00+$31; // Data Register, Port F
  DDRF : byte absolute $00+$30; // Data Direction Register, Port F
  PINF : byte absolute $00+$2F; // Input Pins, Port F
  // JTAG
  OCDR : byte absolute $00+$51; // On-Chip Debug Related Register in I/O Memory
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  // SPI
  SPCR : byte absolute $00+$4C; // SPI Control Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPDR : byte absolute $00+$4E; // SPI Data Register
  // TWI
  TWBR : byte absolute $00+$B8; // TWI Bit Rate register
  TWCR : byte absolute $00+$BC; // TWI Control Register
  TWSR : byte absolute $00+$B9; // TWI Status Register
  TWDR : byte absolute $00+$BB; // TWI Data register
  TWAR : byte absolute $00+$BA; // TWI (Slave) Address register
  // USART0
  UDR0 : byte absolute $00+$C6; // USART I/O Data Register
  UCSR0A : byte absolute $00+$C0; // USART Control and Status Register A
  UCSR0B : byte absolute $00+$C1; // USART Control and Status Register B
  UCSR0C : byte absolute $00+$C2; // USART Control and Status Register C
  UBRR0 : word absolute $00+$C4; // USART Baud Rate Register t Bytes
  UBRR0L : byte absolute $00+$C4; // USART Baud Rate Register t Bytes
  UBRR0H : byte absolute $00+$C4+1; // USART Baud Rate Register t Bytes
  // USART1
  UDR1 : byte absolute $00+$CE; // USART I/O Data Register
  UCSR1A : byte absolute $00+$C8; // USART Control and Status Register A
  UCSR1B : byte absolute $00+$C9; // USART Control and Status Register B
  UCSR1C : byte absolute $00+$CA; // USART Control and Status Register C
  UBRR1 : word absolute $00+$CC; // USART Baud Rate Register t Bytes
  UBRR1L : byte absolute $00+$CC; // USART Baud Rate Register t Bytes
  UBRR1H : byte absolute $00+$CC+1; // USART Baud Rate Register t Bytes
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  XMCRA : byte absolute $00+$74; // External Memory Control Register A
  XMCRB : byte absolute $00+$75; // External Memory Control Register B
  OSCCAL : byte absolute $00+$66; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$61; // Clock Prescale Register
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  RAMPZ : byte absolute $00+$5B; // RAM Page Z Select Register - Not used.
  GPIOR2 : byte absolute $00+$4B; // General Purpose IO Register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose IO Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose IO Register 0
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register A
  EICRB : byte absolute $00+$6A; // External Interrupt Control Register B
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Read/Write Access  Bytes - Only bit 10..8 are used in AT90CAN64 - Only bit 9..8 are used in AT90CAN32
  EEARL : byte absolute $00+$41; // EEPROM Read/Write Access  Bytes - Only bit 10..8 are used in AT90CAN64 - Only bit 9..8 are used in AT90CAN32
  EEARH : byte absolute $00+$41+1; // EEPROM Read/Write Access  Bytes - Only bit 10..8 are used in AT90CAN64 - Only bit 9..8 are used in AT90CAN32
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // PORTG
  PORTG : byte absolute $00+$34; // Data Register, Port G
  DDRG : byte absolute $00+$33; // Data Direction Register, Port G
  PING : byte absolute $00+$32; // Input Pins, Port G
  // TIMER_COUNTER_0
  TCCR0A : byte absolute $00+$44; // Timer/Counter0 Control Register
  TCNT0 : byte absolute $00+$46; // Timer/Counter0
  OCR0A : byte absolute $00+$47; // Timer/Counter0 Output Compare Register
  TIMSK0 : byte absolute $00+$6E; // Timer/Counter0 Interrupt Mask Register
  TIFR0 : byte absolute $00+$35; // Timer/Counter0 Interrupt Flag register
  GTCCR : byte absolute $00+$43; // General Timer/Control Register
  // TIMER_COUNTER_1
  TCCR1A : byte absolute $00+$80; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$81; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$82; // Timer/Counter 1 Control Register C
  TCNT1 : word absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$84+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$88; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AL : byte absolute $00+$88; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AH : byte absolute $00+$88+1; // Timer/Counter1 Output Compare Register  Bytes
  OCR1B : word absolute $00+$8A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BL : byte absolute $00+$8A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BH : byte absolute $00+$8A+1; // Timer/Counter1 Output Compare Register  Bytes
  OCR1C : word absolute $00+$8C; // Timer/Counter1 Output Compare Register  Bytes
  OCR1CL : byte absolute $00+$8C; // Timer/Counter1 Output Compare Register  Bytes
  OCR1CH : byte absolute $00+$8C+1; // Timer/Counter1 Output Compare Register  Bytes
  ICR1 : word absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$86+1; // Timer/Counter1 Input Capture Register  Bytes
  TIMSK1 : byte absolute $00+$6F; // Timer/Counter Interrupt Mask Register
  TIFR1 : byte absolute $00+$36; // Timer/Counter Interrupt Flag register
  // TIMER_COUNTER_3
  TCCR3A : byte absolute $00+$90; // Timer/Counter3 Control Register A
  TCCR3B : byte absolute $00+$91; // Timer/Counter3 Control Register B
  TCCR3C : byte absolute $00+$92; // Timer/Counter 3 Control Register C
  TCNT3 : word absolute $00+$94; // Timer/Counter3  Bytes
  TCNT3L : byte absolute $00+$94; // Timer/Counter3  Bytes
  TCNT3H : byte absolute $00+$94+1; // Timer/Counter3  Bytes
  OCR3A : word absolute $00+$98; // Timer/Counter3 Output Compare Register  Bytes
  OCR3AL : byte absolute $00+$98; // Timer/Counter3 Output Compare Register  Bytes
  OCR3AH : byte absolute $00+$98+1; // Timer/Counter3 Output Compare Register  Bytes
  OCR3B : word absolute $00+$9A; // Timer/Counter3 Output Compare Register  Bytes
  OCR3BL : byte absolute $00+$9A; // Timer/Counter3 Output Compare Register  Bytes
  OCR3BH : byte absolute $00+$9A+1; // Timer/Counter3 Output Compare Register  Bytes
  OCR3C : word absolute $00+$9C; // Timer/Counter3 Output Compare Register  Bytes
  OCR3CL : byte absolute $00+$9C; // Timer/Counter3 Output Compare Register  Bytes
  OCR3CH : byte absolute $00+$9C+1; // Timer/Counter3 Output Compare Register  Bytes
  ICR3 : word absolute $00+$96; // Timer/Counter3 Input Capture Register  Bytes
  ICR3L : byte absolute $00+$96; // Timer/Counter3 Input Capture Register  Bytes
  ICR3H : byte absolute $00+$96+1; // Timer/Counter3 Input Capture Register  Bytes
  TIMSK3 : byte absolute $00+$71; // Timer/Counter Interrupt Mask Register
  TIFR3 : byte absolute $00+$38; // Timer/Counter Interrupt Flag register
  // TIMER_COUNTER_2
  TCCR2 : byte absolute $00+$B0; // Timer/Counter2 Control Register
  TCNT2 : byte absolute $00+$B2; // Timer/Counter2
  OCR2A : byte absolute $00+$B3; // Timer/Counter2 Output Compare Register
  TIMSK2 : byte absolute $00+$70; // Timer/Counter Interrupt Mask register
  TIFR2 : byte absolute $00+$37; // Timer/Counter Interrupt Flag Register
  ASSR : byte absolute $00+$B6; // Asynchronous Status Register
  // WATCHDOG
  WDTCR : byte absolute $00+$60; // Watchdog Timer Control Register
  // AD_CONVERTER
  ADMUX : byte absolute $00+$7C; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$7A; // The ADC Control and Status register
  ADC : word absolute $00+$78; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$78; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$78+1; // ADC Data Register  Bytes
  ADCSRB : byte absolute $00+$7B; // ADC Control and Status Register B
  DIDR0 : byte absolute $00+$7E; // Digital Input Disable Register 1
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$50; // Analog Comparator Control And Status Register
  DIDR1 : byte absolute $00+$7F; // 
  // CAN
  CANGCON : byte absolute $00+$D8; // CAN General Control Register
  CANGSTA : byte absolute $00+$D9; // CAN General Status Register
  CANGIT : byte absolute $00+$DA; // CAN General Interrupt Register
  CANGIE : byte absolute $00+$DB; // CAN General Interrupt Enable Register
  CANEN2 : byte absolute $00+$DC; // Enable MOb Register
  CANEN1 : byte absolute $00+$DD; // Enable MOb Register
  CANIE2 : byte absolute $00+$DE; // Enable Interrupt MOb Register
  CANIE1 : byte absolute $00+$DF; // Enable Interrupt MOb Register
  CANSIT2 : byte absolute $00+$E0; // CAN Status Interrupt MOb Register
  CANSIT1 : byte absolute $00+$E1; // CAN Status Interrupt MOb Register
  CANBT1 : byte absolute $00+$E2; // Bit Timing Register 1
  CANBT2 : byte absolute $00+$E3; // Bit Timing Register 2
  CANBT3 : byte absolute $00+$E4; // Bit Timing Register 3
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

const
  // MCUCR
  JTD = 7; // JTAG Interface Disable
  // MCUSR
  JTRF = 4; // JTAG Reset Flag
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
  // TWCR
  TWINT = 7; // TWI Interrupt Flag
  TWEA = 6; // TWI Enable Acknowledge Bit
  TWSTA = 5; // TWI Start Condition Bit
  TWSTO = 4; // TWI Stop Condition Bit
  TWWC = 3; // TWI Write Collition Flag
  TWEN = 2; // TWI Enable Bit
  TWIE = 0; // TWI Interrupt Enable
  // TWSR
  TWS = 3; // TWI Status
  TWPS = 0; // TWI Prescaler
  // TWAR
  TWA = 1; // TWI (Slave) Address register Bits
  TWGCE = 0; // TWI General Call Recognition Enable Bit
  // UCSR0A
  RXC0 = 7; // USART Receive Complete
  TXC0 = 6; // USART Transmitt Complete
  UDRE0 = 5; // USART Data Register Empty
  FE0 = 4; // Framing Error
  DOR0 = 3; // Data overRun
  UPE0 = 2; // Parity Error
  U2X0 = 1; // Double the USART transmission speed
  MPCM0 = 0; // Multi-processor Communication Mode
  // UCSR0B
  RXCIE0 = 7; // RX Complete Interrupt Enable
  TXCIE0 = 6; // TX Complete Interrupt Enable
  UDRIE0 = 5; // USART Data register Empty Interrupt Enable
  RXEN0 = 4; // Receiver Enable
  TXEN0 = 3; // Transmitter Enable
  UCSZ02 = 2; // Character Size
  RXB80 = 1; // Receive Data Bit 8
  TXB80 = 0; // Transmit Data Bit 8
  // UCSR0C
  UMSEL0 = 6; // USART Mode Select
  UPM0 = 4; // Parity Mode Bits
  USBS0 = 3; // Stop Bit Select
  UCSZ0 = 1; // Character Size
  UCPOL0 = 0; // Clock Polarity
  // UCSR1A
  RXC1 = 7; // USART Receive Complete
  TXC1 = 6; // USART Transmitt Complete
  UDRE1 = 5; // USART Data Register Empty
  FE1 = 4; // Framing Error
  DOR1 = 3; // Data overRun
  UPE1 = 2; // Parity Error
  U2X1 = 1; // Double the USART transmission speed
  MPCM1 = 0; // Multi-processor Communication Mode
  // UCSR1B
  RXCIE1 = 7; // RX Complete Interrupt Enable
  TXCIE1 = 6; // TX Complete Interrupt Enable
  UDRIE1 = 5; // USART Data register Empty Interrupt Enable
  RXEN1 = 4; // Receiver Enable
  TXEN1 = 3; // Transmitter Enable
  UCSZ12 = 2; // Character Size
  RXB81 = 1; // Receive Data Bit 8
  TXB81 = 0; // Transmit Data Bit 8
  // UCSR1C
  UMSEL1 = 6; // USART Mode Select
  UPM1 = 4; // Parity Mode Bits
  USBS1 = 3; // Stop Bit Select
  UCSZ1 = 1; // Character Size
  UCPOL1 = 0; // Clock Polarity
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
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // XMCRA
  SRE = 7; // External SRAM Enable
  SRL = 4; // Wait state page limit
  SRW1 = 2; // Wait state select bit upper page
  SRW0 = 0; // Wait state select bit lower page
  // XMCRB
  XMBK = 7; // External Memory Bus Keeper Enable
  XMM = 0; // External Memory High Mask
  // CLKPR
  CLKPCE = 7; // 
  CLKPS = 0; // 
  // SMCR
  SM = 1; // Sleep Mode Select bits
  SE = 0; // Sleep Enable
  // RAMPZ
  RAMPZ0 = 0; // RAM Page Z Select Register Bit 0
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
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read While Write Section Busy
  RWWSRE = 4; // Read While Write section read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // EICRA
  ISC3 = 6; // External Interrupt Sense Control Bit
  ISC2 = 4; // External Interrupt Sense Control Bit
  ISC1 = 2; // External Interrupt Sense Control Bit
  ISC0 = 0; // External Interrupt Sense Control Bit
  // EICRB
  ISC7 = 6; // External Interrupt 7-4 Sense Control Bit
  ISC6 = 4; // External Interrupt 7-4 Sense Control Bit
  ISC5 = 2; // External Interrupt 7-4 Sense Control Bit
  ISC4 = 0; // External Interrupt 7-4 Sense Control Bit
  // EIMSK
  INT = 0; // External Interrupt Request 7 Enable
  // EIFR
  INTF = 0; // External Interrupt Flags
  // EECR
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMWE = 2; // EEPROM Master Write Enable
  EEWE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // TCCR0A
  FOC0A = 7; // Force Output Compare
  WGM00 = 6; // Waveform Generation Mode 0
  COM0A = 4; // Compare Match Output Modes
  WGM01 = 3; // Waveform Generation Mode 1
  CS0 = 0; // Clock Selects
  // TIMSK0
  OCIE0A = 1; // Timer/Counter0 Output Compare Match Interrupt Enable
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR0
  OCF0A = 1; // Timer/Counter0 Output Compare Flag 0
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // GTCCR
  TSM = 7; // Timer/Counter Synchronization Mode
  PSR310 = 0; // Prescaler Reset Timer/Counter1 and Timer/Counter0
  // TCCR1A
  COM1A = 6; // Compare Output Mode 1A, bits
  COM1B = 4; // Compare Output Mode 1B, bits
  COM1C = 2; // Compare Output Mode 1C, bits
  WGM1 = 0; // Waveform Generation Mode
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceler
  ICES1 = 6; // Input Capture 1 Edge Select
  CS1 = 0; // Prescaler source of Timer/Counter 1
  // TCCR1C
  FOC1A = 7; // Force Output Compare 1A
  FOC1B = 6; // Force Output Compare 1B
  FOC1C = 5; // Force Output Compare 1C
  // TIMSK1
  ICIE1 = 5; // Timer/Counter1 Input Capture Interrupt Enable
  OCIE1C = 3; // Timer/Counter1 Output CompareC Match Interrupt Enable
  OCIE1B = 2; // Timer/Counter1 Output CompareB Match Interrupt Enable
  OCIE1A = 1; // Timer/Counter1 Output CompareA Match Interrupt Enable
  TOIE1 = 0; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR1
  ICF1 = 5; // Input Capture Flag 1
  OCF1C = 3; // Output Compare Flag 1C
  OCF1B = 2; // Output Compare Flag 1B
  OCF1A = 1; // Output Compare Flag 1A
  TOV1 = 0; // Timer/Counter1 Overflow Flag
  // TCCR3A
  COM3A = 6; // Compare Output Mode 3A, bits
  COM3B = 4; // Compare Output Mode 3B, bits
  COM3C = 2; // Compare Output Mode 3C, bits
  WGM3 = 0; // Waveform Generation Mode
  // TCCR3B
  ICNC3 = 7; // Input Capture 3 Noise Canceler
  ICES3 = 6; // Input Capture 3 Edge Select
  CS3 = 0; // Prescaler source of Timer/Counter 3
  // TCCR3C
  FOC3A = 7; // Force Output Compare 3A
  FOC3B = 6; // Force Output Compare 3B
  FOC3C = 5; // Force Output Compare 3C
  // TIMSK3
  ICIE3 = 5; // Timer/Counter3 Input Capture Interrupt Enable
  OCIE3C = 3; // Timer/Counter3 Output CompareC Match Interrupt Enable
  OCIE3B = 2; // Timer/Counter3 Output CompareB Match Interrupt Enable
  OCIE3A = 1; // Timer/Counter3 Output CompareA Match Interrupt Enable
  TOIE3 = 0; // Timer/Counter3 Overflow Interrupt Enable
  // TIFR3
  ICF3 = 5; // Input Capture Flag 3
  OCF3C = 3; // Output Compare Flag 3C
  OCF3B = 2; // Output Compare Flag 3B
  OCF3A = 1; // Output Compare Flag 3A
  TOV3 = 0; // Timer/Counter3 Overflow Flag
  // TCCR2
  FOC2A = 7; // Force Output Compare
  WGM20 = 6; // Waveform Genration Mode
  COM2A = 4; // Compare Output Mode bits
  WGM21 = 3; // Waveform Generation Mode
  CS2 = 0; // Clock Select bits
  // TIMSK2
  OCIE2A = 1; // Timer/Counter2 Output Compare Match Interrupt Enable
  TOIE2 = 0; // Timer/Counter2 Overflow Interrupt Enable
  // TIFR2
  OCF2A = 1; // Output Compare Flag 2
  TOV2 = 0; // Timer/Counter2 Overflow Flag
  // GTCCR
  PSR2 = 1; // Prescaler Reset Timer/Counter2
  // ASSR
  EXCLK = 4; // Enable External Clock Interrupt
  AS2 = 3; // AS2: Asynchronous Timer/Counter2
  TCN2UB = 2; // TCN2UB: Timer/Counter2 Update Busy
  OCR2UB = 1; // Output Compare Register2 Update Busy
  TCR2UB = 0; // TCR2UB: Timer/Counter Control Register2 Update Busy
  // WDTCR
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  WDP = 0; // Watch Dog Timer Prescaler bits
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
  // ADCSRB
  ACME = 6; // Analog Comparator Multiplexer Enable
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // DIDR1
  AIN1D = 1; // AIN1 Digital Input Disable
  AIN0D = 0; // AIN0 Digital Input Disable
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
  OVRG = 6; // Overload Frame Flag
  TXBSY = 4; // Transmitter Busy
  RXBSY = 3; // Receiver Busy
  ENFG = 2; // Enable Flag
  BOFF = 1; // Bus Off Mode
  ERRP = 0; // Error Passive Mode
  // CANGIT
  CANIT = 7; // General Interrupt Flag
  BOFFIT = 6; // Bus Off Interrupt Flag
  OVRTIM = 5; // Overrun CAN Timer
  BXOK = 4; // Burst Receive Interrupt
  SERG = 3; // Stuff Error General
  CERG = 2; // CRC Error General
  FERG = 1; // Form Error General
  AERG = 0; // Ackknowledgement Error General
  // CANGIE
  ENIT = 7; // Enable all Interrupts
  ENBOFF = 6; // Enable Bus Off INterrupt
  ENRX = 5; // Enable Receive Interrupt
  ENTX = 4; // Enable Transmitt Interrupt
  ENERR = 3; // Enable MOb Error Interrupt
  ENBX = 2; // Enable Burst Receive Interrupt
  ENERG = 1; // Enable General Error Interrupt
  ENOVRT = 0; // Enable CAN Timer Overrun Interrupt
  // CANBT1
  BRP = 1; // Baud Rate Prescaler bits
  // CANBT2
  SJW = 5; // Re-Sync Jump Width
  PRS = 1; // Propagation Time Segment
  // CANBT3
  PHS2 = 4; // Phase Segments
  PHS1 = 1; // Phase Segment 1
  SMP = 0; // Sample Type
  // CANPAGE
  MOBNB = 4; // MOb Number Bits
  AINC = 3; // MOb Data Buffer Auto Increment
  INDX = 0; // Data Buffer Index Bits
  // CANSTMOB
  DLCW = 7; // Data Length Code Warning
  TXOK = 6; // Transmit OK
  RXOK = 5; // Receive OK
  BERR = 4; // Bit Error
  SERR = 3; // Stuff Error
  CERR = 2; // CRC Error
  FERR = 1; // Form Error
  AERR = 0; // Ackknowledgement Error
  // CANCDMOB
  CONMOB = 6; // MOb Config Bits
  RPLV = 5; // Reply Valid
  IDE = 4; // Identifier Extension
  DLC = 0; // Data Length Code Bits

implementation

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 3 External Interrupt Request 2
procedure INT3_ISR; external name 'INT3_ISR'; // Interrupt 4 External Interrupt Request 3
procedure INT4_ISR; external name 'INT4_ISR'; // Interrupt 5 External Interrupt Request 4
procedure INT5_ISR; external name 'INT5_ISR'; // Interrupt 6 External Interrupt Request 5
procedure INT6_ISR; external name 'INT6_ISR'; // Interrupt 7 External Interrupt Request 6
procedure INT7_ISR; external name 'INT7_ISR'; // Interrupt 8 External Interrupt Request 7
procedure TIMER2_COMP_ISR; external name 'TIMER2_COMP_ISR'; // Interrupt 9 Timer/Counter2 Compare Match
procedure TIMER2_OVF_ISR; external name 'TIMER2_OVF_ISR'; // Interrupt 10 Timer/Counter2 Overflow
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 11 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 12 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 13 Timer/Counter Compare Match B
procedure TIMER1_COMPC_ISR; external name 'TIMER1_COMPC_ISR'; // Interrupt 14 Timer/Counter1 Compare Match C
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 15 Timer/Counter1 Overflow
procedure TIMER0_COMP_ISR; external name 'TIMER0_COMP_ISR'; // Interrupt 16 Timer/Counter0 Compare Match
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 17 Timer/Counter0 Overflow
procedure CANIT_ISR; external name 'CANIT_ISR'; // Interrupt 18 CAN Transfer Complete or Error
procedure OVRIT_ISR; external name 'OVRIT_ISR'; // Interrupt 19 CAN Timer Overrun
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 20 SPI Serial Transfer Complete
procedure USART0__RX_ISR; external name 'USART0__RX_ISR'; // Interrupt 21 USART0, Rx Complete
procedure USART0__UDRE_ISR; external name 'USART0__UDRE_ISR'; // Interrupt 22 USART0 Data Register Empty
procedure USART0__TX_ISR; external name 'USART0__TX_ISR'; // Interrupt 23 USART0, Tx Complete
procedure ANALOG_COMP_ISR; external name 'ANALOG_COMP_ISR'; // Interrupt 24 Analog Comparator
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 25 ADC Conversion Complete
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 26 EEPROM Ready
procedure TIMER3_CAPT_ISR; external name 'TIMER3_CAPT_ISR'; // Interrupt 27 Timer/Counter3 Capture Event
procedure TIMER3_COMPA_ISR; external name 'TIMER3_COMPA_ISR'; // Interrupt 28 Timer/Counter3 Compare Match A
procedure TIMER3_COMPB_ISR; external name 'TIMER3_COMPB_ISR'; // Interrupt 29 Timer/Counter3 Compare Match B
procedure TIMER3_COMPC_ISR; external name 'TIMER3_COMPC_ISR'; // Interrupt 30 Timer/Counter3 Compare Match C
procedure TIMER3_OVF_ISR; external name 'TIMER3_OVF_ISR'; // Interrupt 31 Timer/Counter3 Overflow
procedure USART1__RX_ISR; external name 'USART1__RX_ISR'; // Interrupt 32 USART1, Rx Complete
procedure USART1__UDRE_ISR; external name 'USART1__UDRE_ISR'; // Interrupt 33 USART1, Data Register Empty
procedure USART1__TX_ISR; external name 'USART1__TX_ISR'; // Interrupt 34 USART1, Tx Complete
procedure TWI_ISR; external name 'TWI_ISR'; // Interrupt 35 2-wire Serial Interface
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 36 Store Program Memory Read

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   jmp _start
   jmp INT0_ISR
   jmp INT1_ISR
   jmp INT2_ISR
   jmp INT3_ISR
   jmp INT4_ISR
   jmp INT5_ISR
   jmp INT6_ISR
   jmp INT7_ISR
   jmp TIMER2_COMP_ISR
   jmp TIMER2_OVF_ISR
   jmp TIMER1_CAPT_ISR
   jmp TIMER1_COMPA_ISR
   jmp TIMER1_COMPB_ISR
   jmp TIMER1_COMPC_ISR
   jmp TIMER1_OVF_ISR
   jmp TIMER0_COMP_ISR
   jmp TIMER0_OVF_ISR
   jmp CANIT_ISR
   jmp OVRIT_ISR
   jmp SPI__STC_ISR
   jmp USART0__RX_ISR
   jmp USART0__UDRE_ISR
   jmp USART0__TX_ISR
   jmp ANALOG_COMP_ISR
   jmp ADC_ISR
   jmp EE_READY_ISR
   jmp TIMER3_CAPT_ISR
   jmp TIMER3_COMPA_ISR
   jmp TIMER3_COMPB_ISR
   jmp TIMER3_COMPC_ISR
   jmp TIMER3_OVF_ISR
   jmp USART1__RX_ISR
   jmp USART1__UDRE_ISR
   jmp USART1__TX_ISR
   jmp TWI_ISR
   jmp SPM_READY_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak INT2_ISR
   .weak INT3_ISR
   .weak INT4_ISR
   .weak INT5_ISR
   .weak INT6_ISR
   .weak INT7_ISR
   .weak TIMER2_COMP_ISR
   .weak TIMER2_OVF_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_COMPC_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMP_ISR
   .weak TIMER0_OVF_ISR
   .weak CANIT_ISR
   .weak OVRIT_ISR
   .weak SPI__STC_ISR
   .weak USART0__RX_ISR
   .weak USART0__UDRE_ISR
   .weak USART0__TX_ISR
   .weak ANALOG_COMP_ISR
   .weak ADC_ISR
   .weak EE_READY_ISR
   .weak TIMER3_CAPT_ISR
   .weak TIMER3_COMPA_ISR
   .weak TIMER3_COMPB_ISR
   .weak TIMER3_COMPC_ISR
   .weak TIMER3_OVF_ISR
   .weak USART1__RX_ISR
   .weak USART1__UDRE_ISR
   .weak USART1__TX_ISR
   .weak TWI_ISR
   .weak SPM_READY_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set INT3_ISR, Default_IRQ_handler
   .set INT4_ISR, Default_IRQ_handler
   .set INT5_ISR, Default_IRQ_handler
   .set INT6_ISR, Default_IRQ_handler
   .set INT7_ISR, Default_IRQ_handler
   .set TIMER2_COMP_ISR, Default_IRQ_handler
   .set TIMER2_OVF_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_COMPC_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMP_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set CANIT_ISR, Default_IRQ_handler
   .set OVRIT_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set USART0__RX_ISR, Default_IRQ_handler
   .set USART0__UDRE_ISR, Default_IRQ_handler
   .set USART0__TX_ISR, Default_IRQ_handler
   .set ANALOG_COMP_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set TIMER3_CAPT_ISR, Default_IRQ_handler
   .set TIMER3_COMPA_ISR, Default_IRQ_handler
   .set TIMER3_COMPB_ISR, Default_IRQ_handler
   .set TIMER3_COMPC_ISR, Default_IRQ_handler
   .set TIMER3_OVF_ISR, Default_IRQ_handler
   .set USART1__RX_ISR, Default_IRQ_handler
   .set USART1__UDRE_ISR, Default_IRQ_handler
   .set USART1__TX_ISR, Default_IRQ_handler
   .set TWI_ISR, Default_IRQ_handler
   .set SPM_READY_ISR, Default_IRQ_handler
 end;

end.
