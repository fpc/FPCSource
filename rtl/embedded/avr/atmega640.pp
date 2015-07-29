unit ATmega640;

{$goto on}

interface

var
  // ANALOG_COMPARATOR
  ADCSRB : byte absolute $00+$7B; // ADC Control and Status Register B
  ACSR : byte absolute $00+$50; // Analog Comparator Control And Status Register
  DIDR1 : byte absolute $00+$7F; // Digital Input Disable Register 1
  // USART0
  UDR0 : byte absolute $00+$C6; // USART I/O Data Register
  UCSR0A : byte absolute $00+$C0; // USART Control and Status Register A
  UCSR0B : byte absolute $00+$C1; // USART Control and Status Register B
  UCSR0C : byte absolute $00+$C2; // USART Control and Status Register C
  UBRR0 : word absolute $00+$C4; // USART Baud Rate Register  Bytes
  UBRR0L : byte absolute $00+$C4; // USART Baud Rate Register  Bytes
  UBRR0H : byte absolute $00+$C4+1; // USART Baud Rate Register  Bytes
  // TWI
  TWAMR : byte absolute $00+$BD; // TWI (Slave) Address Mask Register
  TWBR : byte absolute $00+$B8; // TWI Bit Rate register
  TWCR : byte absolute $00+$BC; // TWI Control Register
  TWSR : byte absolute $00+$B9; // TWI Status Register
  TWDR : byte absolute $00+$BB; // TWI Data register
  TWAR : byte absolute $00+$BA; // TWI (Slave) Address register
  // SPI
  SPCR : byte absolute $00+$4C; // SPI Control Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPDR : byte absolute $00+$4E; // SPI Data Register
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
  // PORTG
  PORTG : byte absolute $00+$34; // Data Register, Port G
  DDRG : byte absolute $00+$33; // Data Direction Register, Port G
  PING : byte absolute $00+$32; // Input Pins, Port G
  // PORTH
  PORTH : byte absolute $00+$102; // PORT H Data Register
  DDRH : byte absolute $00+$101; // PORT H Data Direction Register
  PINH : byte absolute $00+$100; // PORT H Input Pins
  // PORTJ
  PORTJ : byte absolute $00+$105; // PORT J Data Register
  DDRJ : byte absolute $00+$104; // PORT J Data Direction Register
  PINJ : byte absolute $00+$103; // PORT J Input Pins
  // PORTK
  PORTK : byte absolute $00+$108; // PORT K Data Register
  DDRK : byte absolute $00+$107; // PORT K Data Direction Register
  PINK : byte absolute $00+$106; // PORT K Input Pins
  // PORTL
  PORTL : byte absolute $00+$10B; // PORT L Data Register
  DDRL : byte absolute $00+$10A; // PORT L Data Direction Register
  PINL : byte absolute $00+$109; // PORT L Input Pins
  // TIMER_COUNTER_0
  OCR0B : byte absolute $00+$48; // Timer/Counter0 Output Compare Register
  OCR0A : byte absolute $00+$47; // Timer/Counter0 Output Compare Register
  TCNT0 : byte absolute $00+$46; // Timer/Counter0
  TCCR0B : byte absolute $00+$45; // Timer/Counter Control Register B
  TCCR0A : byte absolute $00+$44; // Timer/Counter  Control Register A
  TIMSK0 : byte absolute $00+$6E; // Timer/Counter0 Interrupt Mask Register
  TIFR0 : byte absolute $00+$35; // Timer/Counter0 Interrupt Flag register
  GTCCR : byte absolute $00+$43; // General Timer/Counter Control Register
  // TIMER_COUNTER_2
  TIMSK2 : byte absolute $00+$70; // Timer/Counter Interrupt Mask register
  TIFR2 : byte absolute $00+$37; // Timer/Counter Interrupt Flag Register
  TCCR2A : byte absolute $00+$B0; // Timer/Counter2 Control Register A
  TCCR2B : byte absolute $00+$B1; // Timer/Counter2 Control Register B
  TCNT2 : byte absolute $00+$B2; // Timer/Counter2
  OCR2B : byte absolute $00+$B4; // Timer/Counter2 Output Compare Register B
  OCR2A : byte absolute $00+$B3; // Timer/Counter2 Output Compare Register A
  ASSR : byte absolute $00+$B6; // Asynchronous Status Register
  // WATCHDOG
  WDTCSR : byte absolute $00+$60; // Watchdog Timer Control Register
  // USART1
  UDR1 : byte absolute $00+$CE; // USART I/O Data Register
  UCSR1A : byte absolute $00+$C8; // USART Control and Status Register A
  UCSR1B : byte absolute $00+$C9; // USART Control and Status Register B
  UCSR1C : byte absolute $00+$CA; // USART Control and Status Register C
  UBRR1 : word absolute $00+$CC; // USART Baud Rate Register  Bytes
  UBRR1L : byte absolute $00+$CC; // USART Baud Rate Register  Bytes
  UBRR1H : byte absolute $00+$CC+1; // USART Baud Rate Register  Bytes
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Address Register Low Bytes
  EEARL : byte absolute $00+$41; // EEPROM Address Register Low Bytes
  EEARH : byte absolute $00+$41+1; // EEPROM Address Register Low Bytes
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // TIMER_COUNTER_5
  TCCR5A : byte absolute $00+$120; // Timer/Counter5 Control Register A
  TCCR5B : byte absolute $00+$121; // Timer/Counter5 Control Register B
  TCCR5C : byte absolute $00+$122; // Timer/Counter 5 Control Register C
  TCNT5 : word absolute $00+$124; // Timer/Counter5  Bytes
  TCNT5L : byte absolute $00+$124; // Timer/Counter5  Bytes
  TCNT5H : byte absolute $00+$124+1; // Timer/Counter5  Bytes
  OCR5A : word absolute $00+$128; // Timer/Counter5 Output Compare Register A  Bytes
  OCR5AL : byte absolute $00+$128; // Timer/Counter5 Output Compare Register A  Bytes
  OCR5AH : byte absolute $00+$128+1; // Timer/Counter5 Output Compare Register A  Bytes
  OCR5B : word absolute $00+$12A; // Timer/Counter5 Output Compare Register B  Bytes
  OCR5BL : byte absolute $00+$12A; // Timer/Counter5 Output Compare Register B  Bytes
  OCR5BH : byte absolute $00+$12A+1; // Timer/Counter5 Output Compare Register B  Bytes
  OCR5C : word absolute $00+$12C; // Timer/Counter5 Output Compare Register B  Bytes
  OCR5CL : byte absolute $00+$12C; // Timer/Counter5 Output Compare Register B  Bytes
  OCR5CH : byte absolute $00+$12C+1; // Timer/Counter5 Output Compare Register B  Bytes
  ICR5 : word absolute $00+$126; // Timer/Counter5 Input Capture Register  Bytes
  ICR5L : byte absolute $00+$126; // Timer/Counter5 Input Capture Register  Bytes
  ICR5H : byte absolute $00+$126+1; // Timer/Counter5 Input Capture Register  Bytes
  TIMSK5 : byte absolute $00+$73; // Timer/Counter5 Interrupt Mask Register
  TIFR5 : byte absolute $00+$3A; // Timer/Counter5 Interrupt Flag register
  // TIMER_COUNTER_4
  TCCR4A : byte absolute $00+$A0; // Timer/Counter4 Control Register A
  TCCR4B : byte absolute $00+$A1; // Timer/Counter4 Control Register B
  TCCR4C : byte absolute $00+$A2; // Timer/Counter 4 Control Register C
  TCNT4 : word absolute $00+$A4; // Timer/Counter4  Bytes
  TCNT4L : byte absolute $00+$A4; // Timer/Counter4  Bytes
  TCNT4H : byte absolute $00+$A4+1; // Timer/Counter4  Bytes
  OCR4A : word absolute $00+$A8; // Timer/Counter4 Output Compare Register A  Bytes
  OCR4AL : byte absolute $00+$A8; // Timer/Counter4 Output Compare Register A  Bytes
  OCR4AH : byte absolute $00+$A8+1; // Timer/Counter4 Output Compare Register A  Bytes
  OCR4B : word absolute $00+$AA; // Timer/Counter4 Output Compare Register B  Bytes
  OCR4BL : byte absolute $00+$AA; // Timer/Counter4 Output Compare Register B  Bytes
  OCR4BH : byte absolute $00+$AA+1; // Timer/Counter4 Output Compare Register B  Bytes
  OCR4C : word absolute $00+$AC; // Timer/Counter4 Output Compare Register B  Bytes
  OCR4CL : byte absolute $00+$AC; // Timer/Counter4 Output Compare Register B  Bytes
  OCR4CH : byte absolute $00+$AC+1; // Timer/Counter4 Output Compare Register B  Bytes
  ICR4 : word absolute $00+$A6; // Timer/Counter4 Input Capture Register  Bytes
  ICR4L : byte absolute $00+$A6; // Timer/Counter4 Input Capture Register  Bytes
  ICR4H : byte absolute $00+$A6+1; // Timer/Counter4 Input Capture Register  Bytes
  TIMSK4 : byte absolute $00+$72; // Timer/Counter4 Interrupt Mask Register
  TIFR4 : byte absolute $00+$39; // Timer/Counter4 Interrupt Flag register
  // TIMER_COUNTER_3
  TCCR3A : byte absolute $00+$90; // Timer/Counter3 Control Register A
  TCCR3B : byte absolute $00+$91; // Timer/Counter3 Control Register B
  TCCR3C : byte absolute $00+$92; // Timer/Counter 3 Control Register C
  TCNT3 : word absolute $00+$94; // Timer/Counter3  Bytes
  TCNT3L : byte absolute $00+$94; // Timer/Counter3  Bytes
  TCNT3H : byte absolute $00+$94+1; // Timer/Counter3  Bytes
  OCR3A : word absolute $00+$98; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AL : byte absolute $00+$98; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AH : byte absolute $00+$98+1; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3B : word absolute $00+$9A; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3BL : byte absolute $00+$9A; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3BH : byte absolute $00+$9A+1; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3C : word absolute $00+$9C; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3CL : byte absolute $00+$9C; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3CH : byte absolute $00+$9C+1; // Timer/Counter3 Output Compare Register B  Bytes
  ICR3 : word absolute $00+$96; // Timer/Counter3 Input Capture Register  Bytes
  ICR3L : byte absolute $00+$96; // Timer/Counter3 Input Capture Register  Bytes
  ICR3H : byte absolute $00+$96+1; // Timer/Counter3 Input Capture Register  Bytes
  TIMSK3 : byte absolute $00+$71; // Timer/Counter3 Interrupt Mask Register
  TIFR3 : byte absolute $00+$38; // Timer/Counter3 Interrupt Flag register
  // TIMER_COUNTER_1
  TCCR1A : byte absolute $00+$80; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$81; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$82; // Timer/Counter 1 Control Register C
  TCNT1 : word absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$84+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$88; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AL : byte absolute $00+$88; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AH : byte absolute $00+$88+1; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1B : word absolute $00+$8A; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BL : byte absolute $00+$8A; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BH : byte absolute $00+$8A+1; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1C : word absolute $00+$8C; // Timer/Counter1 Output Compare Register C  Bytes
  OCR1CL : byte absolute $00+$8C; // Timer/Counter1 Output Compare Register C  Bytes
  OCR1CH : byte absolute $00+$8C+1; // Timer/Counter1 Output Compare Register C  Bytes
  ICR1 : word absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$86+1; // Timer/Counter1 Input Capture Register  Bytes
  TIMSK1 : byte absolute $00+$6F; // Timer/Counter1 Interrupt Mask Register
  TIFR1 : byte absolute $00+$36; // Timer/Counter1 Interrupt Flag register
  // JTAG
  OCDR : byte absolute $00+$51; // On-Chip Debug Related Register in I/O Memory
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register A
  EICRB : byte absolute $00+$6A; // External Interrupt Control Register B
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  PCMSK2 : byte absolute $00+$6D; // Pin Change Mask Register 2
  PCMSK1 : byte absolute $00+$6C; // Pin Change Mask Register 1
  PCMSK0 : byte absolute $00+$6B; // Pin Change Mask Register 0
  PCIFR : byte absolute $00+$3B; // Pin Change Interrupt Flag Register
  PCICR : byte absolute $00+$68; // Pin Change Interrupt Control Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  XMCRA : byte absolute $00+$74; // External Memory Control Register A
  XMCRB : byte absolute $00+$75; // External Memory Control Register B
  OSCCAL : byte absolute $00+$66; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$61; // 
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  EIND : byte absolute $00+$5C; // Extended Indirect Register
  GPIOR2 : byte absolute $00+$4B; // General Purpose IO Register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose IO Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose IO Register 0
  PRR1 : byte absolute $00+$65; // Power Reduction Register1
  PRR0 : byte absolute $00+$64; // Power Reduction Register0
  // AD_CONVERTER
  ADMUX : byte absolute $00+$7C; // The ADC multiplexer Selection Register
  ADC : word absolute $00+$78; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$78; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$78+1; // ADC Data Register  Bytes
  ADCSRA : byte absolute $00+$7A; // The ADC Control and Status register A
  DIDR2 : byte absolute $00+$7D; // Digital Input Disable Register
  DIDR0 : byte absolute $00+$7E; // Digital Input Disable Register
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  // USART2
  UDR2 : byte absolute $00+$D6; // USART I/O Data Register
  UCSR2A : byte absolute $00+$D0; // USART Control and Status Register A
  UCSR2B : byte absolute $00+$D1; // USART Control and Status Register B
  UCSR2C : byte absolute $00+$D2; // USART Control and Status Register C
  UBRR2 : word absolute $00+$D4; // USART Baud Rate Register  Bytes
  UBRR2L : byte absolute $00+$D4; // USART Baud Rate Register  Bytes
  UBRR2H : byte absolute $00+$D4+1; // USART Baud Rate Register  Bytes
  // USART3
  UDR3 : byte absolute $00+$136; // USART I/O Data Register
  UCSR3A : byte absolute $00+$130; // USART Control and Status Register A
  UCSR3B : byte absolute $00+$131; // USART Control and Status Register B
  UCSR3C : byte absolute $00+$132; // USART Control and Status Register C
  UBRR3 : word absolute $00+$134; // USART Baud Rate Register  Bytes
  UBRR3L : byte absolute $00+$134; // USART Baud Rate Register  Bytes
  UBRR3H : byte absolute $00+$134+1; // USART Baud Rate Register  Bytes

const
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
  // TWAMR
  TWAM = 1; // 
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
  // TCCR0B
  FOC0A = 7; // Force Output Compare A
  FOC0B = 6; // Force Output Compare B
  WGM02 = 3; // 
  CS0 = 0; // Clock Select
  // TCCR0A
  COM0A = 6; // Compare Output Mode, Phase Correct PWM Mode
  COM0B = 4; // Compare Output Mode, Fast PWm
  WGM0 = 0; // Waveform Generation Mode
  // TIMSK0
  OCIE0B = 2; // Timer/Counter0 Output Compare Match B Interrupt Enable
  OCIE0A = 1; // Timer/Counter0 Output Compare Match A Interrupt Enable
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR0
  OCF0B = 2; // Timer/Counter0 Output Compare Flag 0B
  OCF0A = 1; // Timer/Counter0 Output Compare Flag 0A
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // GTCCR
  TSM = 7; // Timer/Counter Synchronization Mode
  PSRSYNC = 0; // Prescaler Reset Timer/Counter1 and Timer/Counter0
  // TIMSK2
  OCIE2B = 2; // Timer/Counter2 Output Compare Match B Interrupt Enable
  OCIE2A = 1; // Timer/Counter2 Output Compare Match A Interrupt Enable
  TOIE2 = 0; // Timer/Counter2 Overflow Interrupt Enable
  // TIFR2
  OCF2B = 2; // Output Compare Flag 2B
  OCF2A = 1; // Output Compare Flag 2A
  TOV2 = 0; // Timer/Counter2 Overflow Flag
  // TCCR2A
  COM2A = 6; // Compare Output Mode bits
  COM2B = 4; // Compare Output Mode bits
  WGM2 = 0; // Waveform Genration Mode
  // TCCR2B
  FOC2A = 7; // Force Output Compare A
  FOC2B = 6; // Force Output Compare B
  WGM22 = 3; // Waveform Generation Mode
  CS2 = 0; // Clock Select bits
  // ASSR
  EXCLK = 6; // Enable External Clock Input
  AS2 = 5; // Asynchronous Timer/Counter2
  TCN2UB = 4; // Timer/Counter2 Update Busy
  OCR2AUB = 3; // Output Compare Register2 Update Busy
  OCR2BUB = 2; // Output Compare Register 2 Update Busy
  TCR2AUB = 1; // Timer/Counter Control Register2 Update Busy
  TCR2BUB = 0; // Timer/Counter Control Register2 Update Busy
  // GTCCR
  PSRASY = 1; // Prescaler Reset Timer/Counter2
  // WDTCSR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
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
  // EECR
  EEPM = 4; // EEPROM Programming Mode Bits
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // TCCR5A
  COM5A = 6; // Compare Output Mode 1A, bits
  COM5B = 4; // Compare Output Mode 5B, bits
  COM5C = 2; // Compare Output Mode 5C, bits
  WGM5 = 0; // Waveform Generation Mode
  // TCCR5B
  ICNC5 = 7; // Input Capture 5 Noise Canceler
  ICES5 = 6; // Input Capture 5 Edge Select
  CS5 = 0; // Prescaler source of Timer/Counter 5
  // TCCR5C
  FOC5A = 7; // Force Output Compare 5A
  FOC5B = 6; // Force Output Compare 5B
  FOC5C = 5; // Force Output Compare 5C
  // TIMSK5
  ICIE5 = 5; // Timer/Counter5 Input Capture Interrupt Enable
  OCIE5C = 3; // Timer/Counter5 Output Compare C Match Interrupt Enable
  OCIE5B = 2; // Timer/Counter5 Output Compare B Match Interrupt Enable
  OCIE5A = 1; // Timer/Counter5 Output Compare A Match Interrupt Enable
  TOIE5 = 0; // Timer/Counter5 Overflow Interrupt Enable
  // TIFR5
  ICF5 = 5; // Input Capture Flag 5
  OCF5C = 3; // Output Compare Flag 5C
  OCF5B = 2; // Output Compare Flag 5B
  OCF5A = 1; // Output Compare Flag 5A
  TOV5 = 0; // Timer/Counter5 Overflow Flag
  // TCCR4A
  COM4A = 6; // Compare Output Mode 1A, bits
  COM4B = 4; // Compare Output Mode 4B, bits
  COM4C = 2; // Compare Output Mode 4C, bits
  WGM4 = 0; // Waveform Generation Mode
  // TCCR4B
  ICNC4 = 7; // Input Capture 4 Noise Canceler
  ICES4 = 6; // Input Capture 4 Edge Select
  CS4 = 0; // Prescaler source of Timer/Counter 4
  // TCCR4C
  FOC4A = 7; // Force Output Compare 4A
  FOC4B = 6; // Force Output Compare 4B
  FOC4C = 5; // Force Output Compare 4C
  // TIMSK4
  ICIE4 = 5; // Timer/Counter4 Input Capture Interrupt Enable
  OCIE4C = 3; // Timer/Counter4 Output Compare C Match Interrupt Enable
  OCIE4B = 2; // Timer/Counter4 Output Compare B Match Interrupt Enable
  OCIE4A = 1; // Timer/Counter4 Output Compare A Match Interrupt Enable
  TOIE4 = 0; // Timer/Counter4 Overflow Interrupt Enable
  // TIFR4
  ICF4 = 5; // Input Capture Flag 4
  OCF4C = 3; // Output Compare Flag 4C
  OCF4B = 2; // Output Compare Flag 4B
  OCF4A = 1; // Output Compare Flag 4A
  TOV4 = 0; // Timer/Counter4 Overflow Flag
  // TCCR3A
  COM3A = 6; // Compare Output Mode 1A, bits
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
  OCIE3C = 3; // Timer/Counter3 Output Compare C Match Interrupt Enable
  OCIE3B = 2; // Timer/Counter3 Output Compare B Match Interrupt Enable
  OCIE3A = 1; // Timer/Counter3 Output Compare A Match Interrupt Enable
  TOIE3 = 0; // Timer/Counter3 Overflow Interrupt Enable
  // TIFR3
  ICF3 = 5; // Input Capture Flag 3
  OCF3C = 3; // Output Compare Flag 3C
  OCF3B = 2; // Output Compare Flag 3B
  OCF3A = 1; // Output Compare Flag 3A
  TOV3 = 0; // Timer/Counter3 Overflow Flag
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
  OCIE1C = 3; // Timer/Counter1 Output Compare C Match Interrupt Enable
  OCIE1B = 2; // Timer/Counter1 Output Compare B Match Interrupt Enable
  OCIE1A = 1; // Timer/Counter1 Output Compare A Match Interrupt Enable
  TOIE1 = 0; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR1
  ICF1 = 5; // Input Capture Flag 1
  OCF1C = 3; // Output Compare Flag 1C
  OCF1B = 2; // Output Compare Flag 1B
  OCF1A = 1; // Output Compare Flag 1A
  TOV1 = 0; // Timer/Counter1 Overflow Flag
  // MCUCR
  JTD = 7; // JTAG Interface Disable
  // MCUSR
  JTRF = 4; // JTAG Reset Flag
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
  // PCIFR
  PCIF = 0; // Pin Change Interrupt Flags
  // PCICR
  PCIE = 0; // Pin Change Interrupt Enables
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
  // PRR1
  PRTIM5 = 5; // Power Reduction Timer/Counter5
  PRTIM4 = 4; // Power Reduction Timer/Counter4
  PRTIM3 = 3; // Power Reduction Timer/Counter3
  PRUSART = 0; // Power Reduction USART3
  // PRR0
  PRTWI = 7; // Power Reduction TWI
  PRTIM2 = 6; // Power Reduction Timer/Counter2
  PRTIM0 = 5; // Power Reduction Timer/Counter0
  PRTIM1 = 3; // Power Reduction Timer/Counter1
  PRSPI = 2; // Power Reduction Serial Peripheral Interface
  PRUSART0 = 1; // Power Reduction USART
  PRADC = 0; // Power Reduction ADC
  // ADMUX
  REFS = 6; // Reference Selection Bits
  ADLAR = 5; // Left Adjust Result
  MUX = 0; // Analog Channel and Gain Selection Bits
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADATE = 5; // ADC  Auto Trigger Enable
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits
  // ADCSRB
  MUX5 = 3; // Analog Channel and Gain Selection Bits
  ADTS = 0; // ADC Auto Trigger Source bits
  // DIDR2
  ADC15D = 7; // 
  ADC14D = 6; // 
  ADC13D = 5; // 
  ADC12D = 4; // 
  ADC11D = 3; // 
  ADC10D = 2; // 
  ADC9D = 1; // 
  ADC8D = 0; // 
  // DIDR0
  ADC7D = 7; // 
  ADC6D = 6; // 
  ADC5D = 5; // 
  ADC4D = 4; // 
  ADC3D = 3; // 
  ADC2D = 2; // 
  ADC1D = 1; // 
  ADC0D = 0; // 
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read While Write Section Busy
  SIGRD = 5; // Signature Row Read
  RWWSRE = 4; // Read While Write section read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // UCSR2A
  RXC2 = 7; // USART Receive Complete
  TXC2 = 6; // USART Transmitt Complete
  UDRE2 = 5; // USART Data Register Empty
  FE2 = 4; // Framing Error
  DOR2 = 3; // Data overRun
  UPE2 = 2; // Parity Error
  U2X2 = 1; // Double the USART transmission speed
  MPCM2 = 0; // Multi-processor Communication Mode
  // UCSR2B
  RXCIE2 = 7; // RX Complete Interrupt Enable
  TXCIE2 = 6; // TX Complete Interrupt Enable
  UDRIE2 = 5; // USART Data register Empty Interrupt Enable
  RXEN2 = 4; // Receiver Enable
  TXEN2 = 3; // Transmitter Enable
  UCSZ22 = 2; // Character Size
  RXB82 = 1; // Receive Data Bit 8
  TXB82 = 0; // Transmit Data Bit 8
  // UCSR2C
  UMSEL2 = 6; // USART Mode Select
  UPM2 = 4; // Parity Mode Bits
  USBS2 = 3; // Stop Bit Select
  UCSZ2 = 1; // Character Size
  UCPOL2 = 0; // Clock Polarity
  // UCSR3A
  RXC3 = 7; // USART Receive Complete
  TXC3 = 6; // USART Transmitt Complete
  UDRE3 = 5; // USART Data Register Empty
  FE3 = 4; // Framing Error
  DOR3 = 3; // Data overRun
  UPE3 = 2; // Parity Error
  U2X3 = 1; // Double the USART transmission speed
  MPCM3 = 0; // Multi-processor Communication Mode
  // UCSR3B
  RXCIE3 = 7; // RX Complete Interrupt Enable
  TXCIE3 = 6; // TX Complete Interrupt Enable
  UDRIE3 = 5; // USART Data register Empty Interrupt Enable
  RXEN3 = 4; // Receiver Enable
  TXEN3 = 3; // Transmitter Enable
  UCSZ32 = 2; // Character Size
  RXB83 = 1; // Receive Data Bit 8
  TXB83 = 0; // Transmit Data Bit 8
  // UCSR3C
  UMSEL3 = 6; // USART Mode Select
  UPM3 = 4; // Parity Mode Bits
  USBS3 = 3; // Stop Bit Select
  UCSZ3 = 1; // Character Size
  UCPOL3 = 0; // Clock Polarity

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
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 9 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 10 Pin Change Interrupt Request 1
procedure PCINT2_ISR; external name 'PCINT2_ISR'; // Interrupt 11 Pin Change Interrupt Request 2
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 12 Watchdog Time-out Interrupt
procedure TIMER2_COMPA_ISR; external name 'TIMER2_COMPA_ISR'; // Interrupt 13 Timer/Counter2 Compare Match A
procedure TIMER2_COMPB_ISR; external name 'TIMER2_COMPB_ISR'; // Interrupt 14 Timer/Counter2 Compare Match B
procedure TIMER2_OVF_ISR; external name 'TIMER2_OVF_ISR'; // Interrupt 15 Timer/Counter2 Overflow
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 16 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 17 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 18 Timer/Counter1 Compare Match B
procedure TIMER1_COMPC_ISR; external name 'TIMER1_COMPC_ISR'; // Interrupt 19 Timer/Counter1 Compare Match C
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 20 Timer/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 21 Timer/Counter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 22 Timer/Counter0 Compare Match B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 23 Timer/Counter0 Overflow
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 24 SPI Serial Transfer Complete
procedure USART0__RX_ISR; external name 'USART0__RX_ISR'; // Interrupt 25 USART0, Rx Complete
procedure USART0__UDRE_ISR; external name 'USART0__UDRE_ISR'; // Interrupt 26 USART0 Data register Empty
procedure USART0__TX_ISR; external name 'USART0__TX_ISR'; // Interrupt 27 USART0, Tx Complete
procedure ANALOG_COMP_ISR; external name 'ANALOG_COMP_ISR'; // Interrupt 28 Analog Comparator
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 29 ADC Conversion Complete
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 30 EEPROM Ready
procedure TIMER3_CAPT_ISR; external name 'TIMER3_CAPT_ISR'; // Interrupt 31 Timer/Counter3 Capture Event
procedure TIMER3_COMPA_ISR; external name 'TIMER3_COMPA_ISR'; // Interrupt 32 Timer/Counter3 Compare Match A
procedure TIMER3_COMPB_ISR; external name 'TIMER3_COMPB_ISR'; // Interrupt 33 Timer/Counter3 Compare Match B
procedure TIMER3_COMPC_ISR; external name 'TIMER3_COMPC_ISR'; // Interrupt 34 Timer/Counter3 Compare Match C
procedure TIMER3_OVF_ISR; external name 'TIMER3_OVF_ISR'; // Interrupt 35 Timer/Counter3 Overflow
procedure USART1__RX_ISR; external name 'USART1__RX_ISR'; // Interrupt 36 USART1, Rx Complete
procedure USART1__UDRE_ISR; external name 'USART1__UDRE_ISR'; // Interrupt 37 USART1 Data register Empty
procedure USART1__TX_ISR; external name 'USART1__TX_ISR'; // Interrupt 38 USART1, Tx Complete
procedure TWI_ISR; external name 'TWI_ISR'; // Interrupt 39 2-wire Serial Interface
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 40 Store Program Memory Read
procedure TIMER4_CAPT_ISR; external name 'TIMER4_CAPT_ISR'; // Interrupt 41 Timer/Counter4 Capture Event
procedure TIMER4_COMPA_ISR; external name 'TIMER4_COMPA_ISR'; // Interrupt 42 Timer/Counter4 Compare Match A
procedure TIMER4_COMPB_ISR; external name 'TIMER4_COMPB_ISR'; // Interrupt 43 Timer/Counter4 Compare Match B
procedure TIMER4_COMPC_ISR; external name 'TIMER4_COMPC_ISR'; // Interrupt 44 Timer/Counter4 Compare Match C
procedure TIMER4_OVF_ISR; external name 'TIMER4_OVF_ISR'; // Interrupt 45 Timer/Counter4 Overflow
procedure TIMER5_CAPT_ISR; external name 'TIMER5_CAPT_ISR'; // Interrupt 46 Timer/Counter5 Capture Event
procedure TIMER5_COMPA_ISR; external name 'TIMER5_COMPA_ISR'; // Interrupt 47 Timer/Counter5 Compare Match A
procedure TIMER5_COMPB_ISR; external name 'TIMER5_COMPB_ISR'; // Interrupt 48 Timer/Counter5 Compare Match B
procedure TIMER5_COMPC_ISR; external name 'TIMER5_COMPC_ISR'; // Interrupt 49 Timer/Counter5 Compare Match C
procedure TIMER5_OVF_ISR; external name 'TIMER5_OVF_ISR'; // Interrupt 50 Timer/Counter5 Overflow
procedure USART2__RX_ISR; external name 'USART2__RX_ISR'; // Interrupt 51 USART2, Rx Complete
procedure USART2__UDRE_ISR; external name 'USART2__UDRE_ISR'; // Interrupt 52 USART2 Data register Empty
procedure USART2__TX_ISR; external name 'USART2__TX_ISR'; // Interrupt 53 USART2, Tx Complete
procedure USART3__RX_ISR; external name 'USART3__RX_ISR'; // Interrupt 54 USART3, Rx Complete
procedure USART3__UDRE_ISR; external name 'USART3__UDRE_ISR'; // Interrupt 55 USART3 Data register Empty
procedure USART3__TX_ISR; external name 'USART3__TX_ISR'; // Interrupt 56 USART3, Tx Complete

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
   jmp PCINT0_ISR
   jmp PCINT1_ISR
   jmp PCINT2_ISR
   jmp WDT_ISR
   jmp TIMER2_COMPA_ISR
   jmp TIMER2_COMPB_ISR
   jmp TIMER2_OVF_ISR
   jmp TIMER1_CAPT_ISR
   jmp TIMER1_COMPA_ISR
   jmp TIMER1_COMPB_ISR
   jmp TIMER1_COMPC_ISR
   jmp TIMER1_OVF_ISR
   jmp TIMER0_COMPA_ISR
   jmp TIMER0_COMPB_ISR
   jmp TIMER0_OVF_ISR
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
   jmp TIMER4_CAPT_ISR
   jmp TIMER4_COMPA_ISR
   jmp TIMER4_COMPB_ISR
   jmp TIMER4_COMPC_ISR
   jmp TIMER4_OVF_ISR
   jmp TIMER5_CAPT_ISR
   jmp TIMER5_COMPA_ISR
   jmp TIMER5_COMPB_ISR
   jmp TIMER5_COMPC_ISR
   jmp TIMER5_OVF_ISR
   jmp USART2__RX_ISR
   jmp USART2__UDRE_ISR
   jmp USART2__TX_ISR
   jmp USART3__RX_ISR
   jmp USART3__UDRE_ISR
   jmp USART3__TX_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak INT2_ISR
   .weak INT3_ISR
   .weak INT4_ISR
   .weak INT5_ISR
   .weak INT6_ISR
   .weak INT7_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak PCINT2_ISR
   .weak WDT_ISR
   .weak TIMER2_COMPA_ISR
   .weak TIMER2_COMPB_ISR
   .weak TIMER2_OVF_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_COMPC_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak TIMER0_OVF_ISR
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
   .weak TIMER4_CAPT_ISR
   .weak TIMER4_COMPA_ISR
   .weak TIMER4_COMPB_ISR
   .weak TIMER4_COMPC_ISR
   .weak TIMER4_OVF_ISR
   .weak TIMER5_CAPT_ISR
   .weak TIMER5_COMPA_ISR
   .weak TIMER5_COMPB_ISR
   .weak TIMER5_COMPC_ISR
   .weak TIMER5_OVF_ISR
   .weak USART2__RX_ISR
   .weak USART2__UDRE_ISR
   .weak USART2__TX_ISR
   .weak USART3__RX_ISR
   .weak USART3__UDRE_ISR
   .weak USART3__TX_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set INT3_ISR, Default_IRQ_handler
   .set INT4_ISR, Default_IRQ_handler
   .set INT5_ISR, Default_IRQ_handler
   .set INT6_ISR, Default_IRQ_handler
   .set INT7_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set PCINT2_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set TIMER2_COMPA_ISR, Default_IRQ_handler
   .set TIMER2_COMPB_ISR, Default_IRQ_handler
   .set TIMER2_OVF_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_COMPC_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
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
   .set TIMER4_CAPT_ISR, Default_IRQ_handler
   .set TIMER4_COMPA_ISR, Default_IRQ_handler
   .set TIMER4_COMPB_ISR, Default_IRQ_handler
   .set TIMER4_COMPC_ISR, Default_IRQ_handler
   .set TIMER4_OVF_ISR, Default_IRQ_handler
   .set TIMER5_CAPT_ISR, Default_IRQ_handler
   .set TIMER5_COMPA_ISR, Default_IRQ_handler
   .set TIMER5_COMPB_ISR, Default_IRQ_handler
   .set TIMER5_COMPC_ISR, Default_IRQ_handler
   .set TIMER5_OVF_ISR, Default_IRQ_handler
   .set USART2__RX_ISR, Default_IRQ_handler
   .set USART2__UDRE_ISR, Default_IRQ_handler
   .set USART2__TX_ISR, Default_IRQ_handler
   .set USART3__RX_ISR, Default_IRQ_handler
   .set USART3__UDRE_ISR, Default_IRQ_handler
   .set USART3__TX_ISR, Default_IRQ_handler
 end;

end.
