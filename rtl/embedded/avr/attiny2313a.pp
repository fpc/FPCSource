unit ATtiny2313A;

{$goto on}

interface

var
  // PORTB
  PORTB : byte absolute $00+$38; // Port B Data Register
  DDRB : byte absolute $00+$37; // Port B Data Direction Register
  PINB : byte absolute $00+$36; // Port B Input Pins
  // TIMER_COUNTER_0
  TIMSK : byte absolute $00+$59; // Timer/Counter Interrupt Mask Register
  TIFR : byte absolute $00+$58; // Timer/Counter Interrupt Flag register
  OCR0B : byte absolute $00+$5C; // Timer/Counter0 Output Compare Register
  OCR0A : byte absolute $00+$56; // Timer/Counter0 Output Compare Register
  TCCR0A : byte absolute $00+$50; // Timer/Counter  Control Register A
  TCNT0 : byte absolute $00+$52; // Timer/Counter0
  TCCR0B : byte absolute $00+$53; // Timer/Counter Control Register B
  // TIMER_COUNTER_1
  TCCR1A : byte absolute $00+$4F; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$4E; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$42; // Timer/Counter1 Control Register C
  TCNT1 : word absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$4C+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$4A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AL : byte absolute $00+$4A; // Timer/Counter1 Output Compare Register  Bytes
  OCR1AH : byte absolute $00+$4A+1; // Timer/Counter1 Output Compare Register  Bytes
  OCR1B : word absolute $00+$48; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BL : byte absolute $00+$48; // Timer/Counter1 Output Compare Register  Bytes
  OCR1BH : byte absolute $00+$48+1; // Timer/Counter1 Output Compare Register  Bytes
  ICR1 : word absolute $00+$44; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$44; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$44+1; // Timer/Counter1 Input Capture Register  Bytes
  // WATCHDOG
  WDTCR : byte absolute $00+$41; // Watchdog Timer Control Register
  // USART
  UDR : byte absolute $00+$2C; // USART I/O Data Register
  UCSRA : byte absolute $00+$02B; // USART Control and Status Register A
  UCSRB : byte absolute $00+$02A; // USART Control and Status Register B
  UCSRC : byte absolute $00+$23; // USART Control and Status Register C
  UBRRH : byte absolute $00+$22; // USART Baud Rate Register High Byte
  UBRRL : byte absolute $00+$29; // USART Baud Rate Register Low Byte
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$28; // Analog Comparator Control And Status Register
  DIDR : byte absolute $00+$21; // Digital Input Disable Register 1
  // PORTD
  PORTD : byte absolute $00+$32; // Data Register, Port D
  DDRD : byte absolute $00+$31; // Data Direction Register, Port D
  PIND : byte absolute $00+$30; // Input Pins, Port D
  // EEPROM
  EEAR : byte absolute $00+$3E; // EEPROM Read/Write Access
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register
  // PORTA
  PORTA : byte absolute $00+$3B; // Port A Data Register
  DDRA : byte absolute $00+$3A; // Port A Data Direction Register
  PINA : byte absolute $00+$39; // Port A Input Pins
  // USI
  USIDR : byte absolute $00+$2F; // USI Data Register
  USISR : byte absolute $00+$2E; // USI Status Register
  USICR : byte absolute $00+$2D; // USI Control Register
  // EXTERNAL_INTERRUPT
  GIMSK : byte absolute $00+$5B; // General Interrupt Mask Register
  EIFR : byte absolute $00+$5A; // Extended Interrupt Flag Register
  PCMSK2 : byte absolute $00+$25; // Pin Change Interrupt Mask Register 2
  PCMSK1 : byte absolute $00+$24; // Pin Change Interrupt Mask Register 1
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SPL : byte absolute $00+$5D; // Stack Pointer Low Byte
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control and Status register
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status register
  OSCCAL : byte absolute $00+$51; // Oscillator Calibration Register
  CLKPR : byte absolute $00+$46; // Clock Prescale Register
  GTCCR : byte absolute $00+$43; // General Timer Counter Control Register
  PCMSK : byte absolute $00+$40; // Pin-Change Mask register
  GPIOR2 : byte absolute $00+$35; // General Purpose I/O Register 2
  GPIOR1 : byte absolute $00+$34; // General Purpose I/O Register 1
  GPIOR0 : byte absolute $00+$33; // General Purpose I/O Register 0
  PRR : byte absolute $00+$26; // Power reduction register
  BODCR : byte absolute $00+$27; // BOD control register

const
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
  // WDTCR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  // UCSRA
  RXC = 7; // USART Receive Complete
  TXC = 6; // USART Transmitt Complete
  UDRE = 5; // USART Data Register Empty
  FE = 4; // Framing Error
  DOR = 3; // Data overRun
  UPE = 2; // USART Parity Error
  U2X = 1; // Double the USART Transmission Speed
  MPCM = 0; // Multi-processor Communication Mode
  // UCSRB
  RXCIE = 7; // RX Complete Interrupt Enable
  TXCIE = 6; // TX Complete Interrupt Enable
  UDRIE = 5; // USART Data register Empty Interrupt Enable
  RXEN = 4; // Receiver Enable
  TXEN = 3; // Transmitter Enable
  UCSZ2 = 2; // Character Size
  RXB8 = 1; // Receive Data Bit 8
  TXB8 = 0; // Transmit Data Bit 8
  // UCSRC
  UMSEL = 6; // USART Mode Select
  UPM = 4; // Parity Mode Bits
  USBS = 3; // Stop Bit Select
  UCSZ = 1; // Character Size Bits
  UCPOL = 0; // Clock Polarity
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // 
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // EECR
  EEPM = 4; // 
  EERIE = 3; // EEProm Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
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
  // GIMSK
  INT = 6; // External Interrupt Request 1 Enable
  PCIE = 5; // 
  // EIFR
  INTF = 6; // External Interrupt Flags
  PCIF = 5; // 
  // PCMSK2
  PCINT = 0; // Pin Change Interrupt Masks
  // PCMSK1
  // SREG
  I = 7; // Global Interrupt Enable
  T = 6; // Bit Copy Storage
  H = 5; // Half Carry Flag
  S = 4; // Sign Bit
  V = 3; // Two's Complement Overflow Flag
  N = 2; // Negative Flag
  Z = 1; // Zero Flag
  C = 0; // Carry Flag
  // SPMCSR
  CTPB = 4; // Clear Temporary Page Buffer
  RFLB = 3; // Read Fuse and Lock Bits
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // MCUCR
  PUD = 7; // Pull-up Disable
  SM = 4; // Sleep Mode Select Bits
  SE = 5; // Sleep Enable
  ISC1 = 2; // Interrupt Sense Control 1 bits
  ISC0 = 0; // Interrupt Sense Control 0 bits
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-On Reset Flag
  // CLKPR
  CLKPCE = 7; // Clock Prescaler Change Enable
  CLKPS = 0; // Clock Prescaler Select Bits
  // GTCCR
  PSR10 = 0; // 
  // PRR
  PRTIM = 2; // 
  PRUSI = 1; // 
  PRUSART = 0; // 
  // BODCR
  BPDS = 1; // 
  BPDSE = 0; // 

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 3 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 4 Timer/Counter1 Compare Match A
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 5 Timer/Counter1 Overflow
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 6 Timer/Counter0 Overflow
procedure USART__RX_ISR; external name 'USART__RX_ISR'; // Interrupt 7 USART, Rx Complete
procedure USART__UDRE_ISR; external name 'USART__UDRE_ISR'; // Interrupt 8 USART Data Register Empty
procedure USART__TX_ISR; external name 'USART__TX_ISR'; // Interrupt 9 USART, Tx Complete
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 10 Analog Comparator
procedure PCINT_B_ISR; external name 'PCINT_B_ISR'; // Interrupt 11 Pin Change Interrupt Request B
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 12 
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 13 
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 14 
procedure USI_START_ISR; external name 'USI_START_ISR'; // Interrupt 15 USI Start Condition
procedure USI_OVERFLOW_ISR; external name 'USI_OVERFLOW_ISR'; // Interrupt 16 USI Overflow
procedure EEPROM_Ready_ISR; external name 'EEPROM_Ready_ISR'; // Interrupt 17 
procedure WDT_OVERFLOW_ISR; external name 'WDT_OVERFLOW_ISR'; // Interrupt 18 Watchdog Timer Overflow
procedure PCINT_A_ISR; external name 'PCINT_A_ISR'; // Interrupt 19 Pin Change Interrupt Request A
procedure PCINT_D_ISR; external name 'PCINT_D_ISR'; // Interrupt 20 Pin Change Interrupt Request D

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp INT1_ISR
   rjmp TIMER1_CAPT_ISR
   rjmp TIMER1_COMPA_ISR
   rjmp TIMER1_OVF_ISR
   rjmp TIMER0_OVF_ISR
   rjmp USART__RX_ISR
   rjmp USART__UDRE_ISR
   rjmp USART__TX_ISR
   rjmp ANA_COMP_ISR
   rjmp PCINT_B_ISR
   rjmp TIMER1_COMPB_ISR
   rjmp TIMER0_COMPA_ISR
   rjmp TIMER0_COMPB_ISR
   rjmp USI_START_ISR
   rjmp USI_OVERFLOW_ISR
   rjmp EEPROM_Ready_ISR
   rjmp WDT_OVERFLOW_ISR
   rjmp PCINT_A_ISR
   rjmp PCINT_D_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_OVF_ISR
   .weak USART__RX_ISR
   .weak USART__UDRE_ISR
   .weak USART__TX_ISR
   .weak ANA_COMP_ISR
   .weak PCINT_B_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak USI_START_ISR
   .weak USI_OVERFLOW_ISR
   .weak EEPROM_Ready_ISR
   .weak WDT_OVERFLOW_ISR
   .weak PCINT_A_ISR
   .weak PCINT_D_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set USART__RX_ISR, Default_IRQ_handler
   .set USART__UDRE_ISR, Default_IRQ_handler
   .set USART__TX_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set PCINT_B_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set USI_START_ISR, Default_IRQ_handler
   .set USI_OVERFLOW_ISR, Default_IRQ_handler
   .set EEPROM_Ready_ISR, Default_IRQ_handler
   .set WDT_OVERFLOW_ISR, Default_IRQ_handler
   .set PCINT_A_ISR, Default_IRQ_handler
   .set PCINT_D_ISR, Default_IRQ_handler
 end;

end.
