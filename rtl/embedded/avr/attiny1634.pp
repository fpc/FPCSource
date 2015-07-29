unit ATtiny1634;

{$goto on}

interface

var
  // TWI
  TWSCRA : byte absolute $00+$7F; // TWI Slave Control Register A
  TWSCRB : byte absolute $00+$7E; // TWI Slave Control Register B
  TWSSRA : byte absolute $00+$7D; // TWI Slave Status Register A
  TWSA : byte absolute $00+$7C; // TWI Slave Address Register
  TWSD : byte absolute $00+$7A; // TWI Slave Data Register
  TWSAM : byte absolute $00+$7B; // TWI Slave Address Mask Register
  // PORTB
  PORTCR : byte absolute $00+$33; // Port Control Register
  PUEB : byte absolute $00+$2E; // Pull-up Enable Control Register
  DDRB : byte absolute $00+$2C; // Data Direction Register, Port B
  PINB : byte absolute $00+$2B; // Port B Data register
  PORTB : byte absolute $00+$2D; // Input Pins, Port B
  // PORTC
  PUEC : byte absolute $00+$2A; // Pull-up Enable Control Register
  PORTC : byte absolute $00+$29; // Port C Data Register
  DDRC : byte absolute $00+$28; // Data Direction Register, Port C
  PINC : byte absolute $00+$27; // Port C Input Pins
  // PORTA
  PUEA : byte absolute $00+$32; // Pull-up Enable Control Register
  PORTA : byte absolute $00+$31; // Port A Data Register
  DDRA : byte absolute $00+$30; // Data Direction Register, Port A
  PINA : byte absolute $00+$2F; // Port A Input Pins
  // AD_CONVERTER
  ADMUX : byte absolute $00+$24; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$23; // The ADC Control and Status register
  ADC : word absolute $00+$20; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$20; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$20+1; // ADC Data Register  Bytes
  ADCSRB : byte absolute $00+$22; // ADC Control and Status Register B
  DIDR2 : byte absolute $00+$62; // Digital Input Disable Register 2
  DIDR1 : byte absolute $00+$61; // Digital Input Disable Register 1
  DIDR0 : byte absolute $00+$60; // Digital Input Disable Register 0
  // ANALOG_COMPARATOR
  ACSRB : byte absolute $00+$25; // Analog Comparator Control And Status Register B
  ACSRA : byte absolute $00+$26; // Analog Comparator Control And Status Register A
  // EEPROM
  EEAR : byte absolute $00+$3E; // EEPROM Read/Write Access
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register
  // TIMER_COUNTER_1
  TIMSK : byte absolute $00+$5A; // Timer/Counter Interrupt Mask Register
  TIFR : byte absolute $00+$59; // Timer/Counter Interrupt Flag register
  TCCR1A : byte absolute $00+$72; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$71; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$70; // Timer/Counter1 Control Register C
  TCNT1 : word absolute $00+$6E; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$6E; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$6E+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$6C; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AL : byte absolute $00+$6C; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AH : byte absolute $00+$6C+1; // Timer/Counter1 Output Compare Register  Bytes
  OCR1B : word absolute $00+$6A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BL : byte absolute $00+$6A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BH : byte absolute $00+$6A+1; // Timer/Counter1 Output Compare Register  Bytes
  ICR1 : word absolute $00+$68; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$68; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$68+1; // Timer/Counter1 Input Capture Register  Bytes
  // TIMER_COUNTER_0
  OCR0B : byte absolute $00+$37; // Timer/Counter0 Output Compare Register
  OCR0A : byte absolute $00+$38; // Timer/Counter0 Output Compare Register
  TCCR0A : byte absolute $00+$3B; // Timer/Counter  Control Register A
  TCNT0 : byte absolute $00+$39; // Timer/Counter0
  TCCR0B : byte absolute $00+$3A; // Timer/Counter Control Register B
  // EXTERNAL_INTERRUPT
  PCMSK1 : byte absolute $00+$49; // Pin Change Mask Register 1
  PCMSK0 : byte absolute $00+$47; // Pin Change Mask Register 0
  GIFR : byte absolute $00+$5B; // General Interrupt Flag Register
  GIMSK : byte absolute $00+$5C; // General Interrupt Mask Register
  // CPU
  PRR : byte absolute $00+$54; // Power Reduction Register
  CCP : byte absolute $00+$4F; // Configuration Change Protection
  OSCCAL0 : byte absolute $00+$63; // Oscillator Calibration Value
  OSCCAL1 : byte absolute $00+$66; // 
  OSCTCAL0A : byte absolute $00+$64; // 
  OSCTCAL0B : byte absolute $00+$65; // 
  CLKPR : byte absolute $00+$53; // Clock Prescale Register
  CLKSR : byte absolute $00+$52; // Clock Setting Register
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$56; // MCU Control Register
  MCUSR : byte absolute $00+$55; // MCU Status Register
  GPIOR2 : byte absolute $00+$36; // General Purpose I/O Register 2
  GPIOR1 : byte absolute $00+$35; // General Purpose I/O Register 1
  GPIOR0 : byte absolute $00+$35; // General Purpose I/O Register 0
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control and Status Register
  // USI
  USIBR : byte absolute $00+$4D; // USI Buffer Register
  USIDR : byte absolute $00+$4C; // USI Data Register
  USISR : byte absolute $00+$4B; // USI Status Register
  USICR : byte absolute $00+$4A; // USI Control Register
  // USART0
  UDR0 : byte absolute $00+$40; // USART I/O Data Register
  UCSR0A : byte absolute $00+$46; // USART Control and Status Register A
  UCSR0B : byte absolute $00+$45; // USART Control and Status Register B
  UCSR0C : byte absolute $00+$44; // USART Control and Status Register C
  UCSR0D : byte absolute $00+$43; // USART Control and Status Register D
  UBRR0 : word absolute $00+$41; // USART Baud Rate Register  Bytes
  UBRR0L : byte absolute $00+$41; // USART Baud Rate Register  Bytes
  UBRR0H : byte absolute $00+$41+1; // USART Baud Rate Register  Bytes
  // USART1
  UDR1 : byte absolute $00+$73; // USART I/O Data Register
  UCSR1A : byte absolute $00+$79; // USART Control and Status Register A
  UCSR1B : byte absolute $00+$78; // USART Control and Status Register B
  UCSR1C : byte absolute $00+$77; // USART Control and Status Register C
  UCSR1D : byte absolute $00+$76; // USART Control and Status Register D
  UBRR1 : word absolute $00+$74; // USART Baud Rate Register  Bytes
  UBRR1L : byte absolute $00+$74; // USART Baud Rate Register  Bytes
  UBRR1H : byte absolute $00+$74+1; // USART Baud Rate Register  Bytes
  // WATCHDOG
  WDTCSR : byte absolute $00+$50; // Watchdog Timer Control and Status Register

const
  // TWSCRA
  TWSHE = 7; // TWI SDA Hold Time Enable
  TWDIE = 5; // TWI Data Interrupt Enable
  TWASIE = 4; // TWI Address/Stop Interrupt Enable
  TWEN = 3; // Two-Wire Interface Enable
  TWSIE = 2; // TWI Stop Interrupt Enable
  TWPME = 1; // TWI Promiscuous Mode Enable
  TWSME = 0; // TWI Smart Mode Enable
  // TWSCRB
  TWAA = 2; // TWI Acknowledge Action
  TWCMD = 0; // 
  // TWSA
  // TWSD
  // PORTCR
  BBMB = 1; // Break-Before-Make Mode Enable
  // PORTCR
  BBMC = 2; // Break-Before-Make Mode Enable
  // PORTCR
  BBMA = 0; // Break-Before-Make Mode Enable
  // ADMUX
  REFS = 6; // Reference Selection Bit
  MUX = 0; // Analog Channel and Gain Selection Bits
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADATE = 5; // ADC Auto Trigger Enable
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC Prescaler Select Bits
  // ADCSRB
  ADLAR = 3; // 
  ADTS = 0; // ADC Auto Trigger Sources
  // DIDR2
  ADC11D = 2; // ADC11 Digital input Disable
  ADC10D = 1; // ADC10 Digital input Disable
  ADC9D = 0; // ADC9 Digital input Disable
  // DIDR1
  ADC8D = 3; // ADC8 Digital Input Disable
  ADC7D = 2; // ADC7 Digital input Disable
  ADC6D = 1; // ADC6 Digital input Disable
  ADC5D = 0; // ADC5 Digital input Disable
  // DIDR0
  ADC4D = 7; // ADC4 Digital input Disable
  ADC3D = 6; // ADC3 Digital input Disable
  ADC2D = 5; // ADC2 Digital input Disable
  ADC1D = 4; // ADC1 Digital input Disable
  ADC0D = 3; // ADC0 Digital Input Disable
  AIN1D = 2; // AIN1 Digital input Disable
  AIN0D = 1; // AIN0 Digital input Disable
  AREFD = 0; // AREF Digital input Disable
  // ACSRB
  HSEL = 7; // Hysteresis Select
  HLEV = 6; // Hysteresis Level
  ACME = 2; // Analog Comparator Multiplexer Enable
  // ACSRA
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // EECR
  EEPM = 4; // EEPROM Programming Mode Bits
  EERIE = 3; // EEProm Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // TIMSK
  TOIE1 = 7; // Timer/Counter1 Overflow Interrupt Enable
  OCIE1A = 6; // Timer/Counter1 Output CompareA Match Interrupt Enable
  OCIE1B = 5; // Timer/Counter1 Output CompareB Match Interrupt Enable
  ICIE1 = 3; // Timer/Counter1 Input Capture Interrupt Enable
  // TIFR
  TOV1 = 7; // Timer/Counter1 Overflow Flag
  OCF1A = 6; // Output Compare Flag 1A
  OCF1B = 5; // Output Compare Flag 1B
  ICF1 = 3; // Input Capture Flag 1
  // TCCR1A
  COM1A = 6; // Compare Output Mode 1A, bits
  COM1B = 4; // Compare Output Mode 1B, bits
  WGM1 = 0; // Pulse Width Modulator Select Bits
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceler
  ICES1 = 6; // Input Capture 1 Edge Select
  CS1 = 0; // Clock Select1 bits
  // TCCR1C
  FOC1A = 7; // Force Output Compare for Channel A
  FOC1B = 6; // Force Output Compare for Channel B
  // TIMSK
  OCIE0B = 2; // Timer/Counter0 Output Compare Match B Interrupt Enable
  TOIE0 = 1; // Timer/Counter0 Overflow Interrupt Enable
  OCIE0A = 0; // Timer/Counter0 Output Compare Match A Interrupt Enable
  // TIFR
  OCF0B = 2; // Timer/Counter0 Output Compare Flag 0B
  TOV0 = 1; // Timer/Counter0 Overflow Flag
  OCF0A = 0; // Timer/Counter0 Output Compare Flag 0A
  // TCCR0A
  COM0A = 6; // Compare Match Output A Mode
  COM0B = 4; // Compare Match Output B Mode
  WGM0 = 0; // Waveform Generation Mode
  // TCCR0B
  FOC0A = 7; // Force Output Compare B
  FOC0B = 6; // Force Output Compare B
  WGM02 = 3; // 
  CS0 = 0; // Clock Select
  // PCMSK1
  PCINT = 0; // Pin Change Enable Masks
  // PCMSK1
  // PCMSK0
  // GIFR
  INTF0 = 6; // External Interrupt Flag 0
  PCIF = 3; // Pin Change Interrupt Flags
  // GIMSK
  INT0 = 6; // External Interrupt Request 0 Enable
  PCIE = 3; // Pin Change Interrupt Enables
  // PRR
  PRTWI = 6; // Power Reduction TWI
  PRTIM1 = 5; // Power Reduction Timer/Counter1
  PRTIM0 = 4; // Power Reduction Timer/Counter0
  PRUSI = 3; // Power Reduction USI
  PRUSART = 1; // Power Reduction USARTs
  PRADC = 0; // Power Reduction ADC
  // CLKPR
  CLKPS = 0; // Clock Prescaler Select Bits
  // CLKSR
  OSCRDY = 7; // Oscillator Ready
  CSTR = 6; // Clock Switch Trigger
  CKOUT_IO = 5; // Clock Output (active low)
  SUT = 4; // Start-up Time
  CKSEL = 0; // Clock Select Bits
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
  SM = 5; // Sleep Mode Select Bits
  SE = 4; // Sleep Enable
  ISC0 = 0; // Interrupt Sense Control 0 bits
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // SPMCSR
  RSIG = 5; // Read Device Signature Imprint Table
  CTPB = 4; // Clear Temporary Page Buffer
  RFLB = 3; // Read Fuse and Lock Bits
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store program Memory Enable
  // USISR
  USISIF = 7; // Start Condition Interrupt Flag
  USIOIF = 6; // Counter Overflow Interrupt Flag
  USIPF = 5; // Stop Condition Flag
  USIDC = 4; // Data Output Collision
  USICNT = 0; // USI Counter Value Bits
  // USICR
  USISIE = 7; // Start Condition Interrupt Enable
  USIOIE = 6; // Counter Overflow Interrupt Enable
  USIWM = 4; // USI Wire Mode Bits
  USICS = 2; // USI Clock Source Select Bits
  USICLK = 1; // Clock Strobe
  USITC = 0; // Toggle Clock Port Pin
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
  // UCSR0D
  RXSIE0 = 7; // USART RX Start Interrupt Enable
  RXS0 = 6; // USART RX Start Flag
  SFDE0 = 5; // USART RX Start Frame Detection Enable
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
  // UCSR1D
  RXSIE1 = 7; // USART RX Start Interrupt Enable
  RXS1 = 6; // USART RX Start Flag
  SFDE1 = 5; // USART RX Start Frame Detection Enable
  // WDTCSR
  WDIF = 7; // Watchdog Timer Interrupt Flag
  WDIE = 6; // Watchdog Timer Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDE = 3; // Watch Dog Enable

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 2 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 3 Pin Change Interrupt Request 1
procedure PCINT2_ISR; external name 'PCINT2_ISR'; // Interrupt 4 Pin Change Interrupt Request 2
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 5 Watchdog Time-out Interrupt
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 6 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 7 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 8 Timer/Counter1 Compare Match B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 9 Timer/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 10 TimerCounter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 11 TimerCounter0 Compare Match B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 12 Timer/Couner0 Overflow
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 13 Analog Comparator
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 14 ADC Conversion Complete
procedure USART0__START_ISR; external name 'USART0__START_ISR'; // Interrupt 15 USART0, Start
procedure USART0__RX_ISR; external name 'USART0__RX_ISR'; // Interrupt 16 USART0, Rx Complete
procedure USART0__UDRE_ISR; external name 'USART0__UDRE_ISR'; // Interrupt 17 USART0 Data Register Empty
procedure USART0__TX_ISR; external name 'USART0__TX_ISR'; // Interrupt 18 USART0, Tx Complete
procedure USART1__START_ISR; external name 'USART1__START_ISR'; // Interrupt 19 USART1, Start
procedure USART1__RX_ISR; external name 'USART1__RX_ISR'; // Interrupt 20 USART1, Rx Complete
procedure USART1__UDRE_ISR; external name 'USART1__UDRE_ISR'; // Interrupt 21 USART1 Data Register Empty
procedure USART1__TX_ISR; external name 'USART1__TX_ISR'; // Interrupt 22 USART1, Tx Complete
procedure USI_START_ISR; external name 'USI_START_ISR'; // Interrupt 23 USI Start Condition
procedure USI_OVERFLOW_ISR; external name 'USI_OVERFLOW_ISR'; // Interrupt 24 USI Overflow
procedure TWI_SLAVE_ISR; external name 'TWI_SLAVE_ISR'; // Interrupt 25 Two-wire Serial Interface
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 26 EEPROM Ready
procedure QTRIP_ISR; external name 'QTRIP_ISR'; // Interrupt 27 Touch Sensing

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp PCINT0_ISR
   rjmp PCINT1_ISR
   rjmp PCINT2_ISR
   rjmp WDT_ISR
   rjmp TIMER1_CAPT_ISR
   rjmp TIMER1_COMPA_ISR
   rjmp TIMER1_COMPB_ISR
   rjmp TIMER1_OVF_ISR
   rjmp TIMER0_COMPA_ISR
   rjmp TIMER0_COMPB_ISR
   rjmp TIMER0_OVF_ISR
   rjmp ANA_COMP_ISR
   rjmp ADC_ISR
   rjmp USART0__START_ISR
   rjmp USART0__RX_ISR
   rjmp USART0__UDRE_ISR
   rjmp USART0__TX_ISR
   rjmp USART1__START_ISR
   rjmp USART1__RX_ISR
   rjmp USART1__UDRE_ISR
   rjmp USART1__TX_ISR
   rjmp USI_START_ISR
   rjmp USI_OVERFLOW_ISR
   rjmp TWI_SLAVE_ISR
   rjmp EE_RDY_ISR
   rjmp QTRIP_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak PCINT2_ISR
   .weak WDT_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak TIMER0_OVF_ISR
   .weak ANA_COMP_ISR
   .weak ADC_ISR
   .weak USART0__START_ISR
   .weak USART0__RX_ISR
   .weak USART0__UDRE_ISR
   .weak USART0__TX_ISR
   .weak USART1__START_ISR
   .weak USART1__RX_ISR
   .weak USART1__UDRE_ISR
   .weak USART1__TX_ISR
   .weak USI_START_ISR
   .weak USI_OVERFLOW_ISR
   .weak TWI_SLAVE_ISR
   .weak EE_RDY_ISR
   .weak QTRIP_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set PCINT2_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set USART0__START_ISR, Default_IRQ_handler
   .set USART0__RX_ISR, Default_IRQ_handler
   .set USART0__UDRE_ISR, Default_IRQ_handler
   .set USART0__TX_ISR, Default_IRQ_handler
   .set USART1__START_ISR, Default_IRQ_handler
   .set USART1__RX_ISR, Default_IRQ_handler
   .set USART1__UDRE_ISR, Default_IRQ_handler
   .set USART1__TX_ISR, Default_IRQ_handler
   .set USI_START_ISR, Default_IRQ_handler
   .set USI_OVERFLOW_ISR, Default_IRQ_handler
   .set TWI_SLAVE_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set QTRIP_ISR, Default_IRQ_handler
 end;

end.
