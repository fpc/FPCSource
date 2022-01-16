unit ATmega88P;

{$goto on}

interface

var
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
  // AD_CONVERTER
  ADMUX : byte absolute $00+$7C; // The ADC multiplexer Selection Register
  ADC : word absolute $00+$78; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$78; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$78+1; // ADC Data Register  Bytes
  ADCSRA : byte absolute $00+$7A; // The ADC Control and Status register A
  ADCSRB : byte absolute $00+$7B; // The ADC Control and Status register B
  DIDR0 : byte absolute $00+$7E; // Digital Input Disable Register
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$50; // Analog Comparator Control And Status Register
  DIDR1 : byte absolute $00+$7F; // Digital Input Disable Register 1
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
  // TIMER_COUNTER_0
  OCR0B : byte absolute $00+$48; // Timer/Counter0 Output Compare Register
  OCR0A : byte absolute $00+$47; // Timer/Counter0 Output Compare Register
  TCNT0 : byte absolute $00+$46; // Timer/Counter0
  TCCR0B : byte absolute $00+$45; // Timer/Counter Control Register B
  TCCR0A : byte absolute $00+$44; // Timer/Counter  Control Register A
  TIMSK0 : byte absolute $00+$6E; // Timer/Counter0 Interrupt Mask Register
  TIFR0 : byte absolute $00+$35; // Timer/Counter0 Interrupt Flag register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  PCICR : byte absolute $00+$68; // Pin Change Interrupt Control Register
  PCMSK2 : byte absolute $00+$6D; // Pin Change Mask Register 2
  PCMSK1 : byte absolute $00+$6C; // Pin Change Mask Register 1
  PCMSK0 : byte absolute $00+$6B; // Pin Change Mask Register 0
  PCIFR : byte absolute $00+$3B; // Pin Change Interrupt Flag Register
  // SPI
  SPDR : byte absolute $00+$4E; // SPI Data Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPCR : byte absolute $00+$4C; // SPI Control Register
  // WATCHDOG
  WDTCSR : byte absolute $00+$60; // Watchdog Timer Control Register
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Address Register  Bytes
  EEARL : byte absolute $00+$41; // EEPROM Address Register  Bytes
  EEARH : byte absolute $00+$41+1; // EEPROM Address Register  Bytes
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // CPU
  PRR : byte absolute $00+$64; // Power Reduction Register
  OSCCAL : byte absolute $00+$66; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$61; // Clock Prescale Register
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control and Status Register
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  GPIOR2 : byte absolute $00+$4B; // General Purpose I/O Register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose I/O Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose I/O Register 0

const
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
  ACME = 6; // 
  ADTS = 0; // ADC Auto Trigger Source bits
  // DIDR0
  ADC5D = 5; // 
  ADC4D = 4; // 
  ADC3D = 3; // 
  ADC2D = 2; // 
  ADC1D = 1; // 
  ADC0D = 0; // 
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
  // EICRA
  ISC1 = 2; // External Interrupt Sense Control 1 Bits
  ISC0 = 0; // External Interrupt Sense Control 0 Bits
  // EIMSK
  INT = 0; // External Interrupt Request 1 Enable
  // EIFR
  INTF = 0; // External Interrupt Flags
  // PCICR
  PCIE = 0; // Pin Change Interrupt Enables
  // PCMSK2
  PCINT = 0; // Pin Change Enable Masks
  // PCMSK1
  // PCMSK0
  // PCIFR
  PCIF = 0; // Pin Change Interrupt Flags
  // SPSR
  SPIF = 7; // SPI Interrupt Flag
  WCOL = 6; // Write Collision Flag
  SPI2X = 0; // Double SPI Speed Bit
  // SPCR
  SPIE = 7; // SPI Interrupt Enable
  SPE = 6; // SPI Enable
  DORD = 5; // Data Order
  MSTR = 4; // Master/Slave Select
  CPOL = 3; // Clock polarity
  CPHA = 2; // Clock Phase
  SPR = 0; // SPI Clock Rate Selects
  // WDTCSR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  // EECR
  EEPM = 4; // EEPROM Programming Mode Bits
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // PRR
  PRTWI = 7; // Power Reduction TWI
  PRTIM2 = 6; // Power Reduction Timer/Counter2
  PRTIM0 = 5; // Power Reduction Timer/Counter0
  PRTIM1 = 3; // Power Reduction Timer/Counter1
  PRSPI = 2; // Power Reduction Serial Peripheral Interface
  PRUSART0 = 1; // Power Reduction USART
  PRADC = 0; // Power Reduction ADC
  // CLKPR
  CLKPCE = 7; // Clock Prescaler Change Enable
  CLKPS = 0; // Clock Prescaler Select Bits
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
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read-While-Write Section Busy
  RWWSRE = 4; // Read-While-Write section read enable
  BLBSET = 3; // Boot Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SELFPRGEN = 0; // Self Programming Enable
  // MCUCR
  BODS = 6; // BOD Sleep
  BODSE = 5; // BOD Sleep Enable
  PUD = 4; // 
  IVSEL = 1; // 
  IVCE = 0; // 
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // SMCR
  SM = 1; // Sleep Mode Select Bits
  SE = 0; // Sleep Enable

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 3 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 4 Pin Change Interrupt Request 0
procedure PCINT2_ISR; external name 'PCINT2_ISR'; // Interrupt 5 Pin Change Interrupt Request 1
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 6 Watchdog Time-out Interrupt
procedure TIMER2_COMPA_ISR; external name 'TIMER2_COMPA_ISR'; // Interrupt 7 Timer/Counter2 Compare Match A
procedure TIMER2_COMPB_ISR; external name 'TIMER2_COMPB_ISR'; // Interrupt 8 Timer/Counter2 Compare Match A
procedure TIMER2_OVF_ISR; external name 'TIMER2_OVF_ISR'; // Interrupt 9 Timer/Counter2 Overflow
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 10 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 11 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 12 Timer/Counter1 Compare Match B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 13 Timer/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 14 TimerCounter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 15 TimerCounter0 Compare Match B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 16 Timer/Couner0 Overflow
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 17 SPI Serial Transfer Complete
procedure USART__RX_ISR; external name 'USART__RX_ISR'; // Interrupt 18 USART Rx Complete
procedure USART__UDRE_ISR; external name 'USART__UDRE_ISR'; // Interrupt 19 USART, Data Register Empty
procedure USART__TX_ISR; external name 'USART__TX_ISR'; // Interrupt 20 USART Tx Complete
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 21 ADC Conversion Complete
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 22 EEPROM Ready
procedure ANALOG_COMP_ISR; external name 'ANALOG_COMP_ISR'; // Interrupt 23 Analog Comparator
procedure TWI_ISR; external name 'TWI_ISR'; // Interrupt 24 Two-wire Serial Interface
procedure SPM_Ready_ISR; external name 'SPM_Ready_ISR'; // Interrupt 25 Store Program Memory Read

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp INT1_ISR
   rjmp PCINT0_ISR
   rjmp PCINT1_ISR
   rjmp PCINT2_ISR
   rjmp WDT_ISR
   rjmp TIMER2_COMPA_ISR
   rjmp TIMER2_COMPB_ISR
   rjmp TIMER2_OVF_ISR
   rjmp TIMER1_CAPT_ISR
   rjmp TIMER1_COMPA_ISR
   rjmp TIMER1_COMPB_ISR
   rjmp TIMER1_OVF_ISR
   rjmp TIMER0_COMPA_ISR
   rjmp TIMER0_COMPB_ISR
   rjmp TIMER0_OVF_ISR
   rjmp SPI__STC_ISR
   rjmp USART__RX_ISR
   rjmp USART__UDRE_ISR
   rjmp USART__TX_ISR
   rjmp ADC_ISR
   rjmp EE_READY_ISR
   rjmp ANALOG_COMP_ISR
   rjmp TWI_ISR
   rjmp SPM_Ready_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
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
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak TIMER0_OVF_ISR
   .weak SPI__STC_ISR
   .weak USART__RX_ISR
   .weak USART__UDRE_ISR
   .weak USART__TX_ISR
   .weak ADC_ISR
   .weak EE_READY_ISR
   .weak ANALOG_COMP_ISR
   .weak TWI_ISR
   .weak SPM_Ready_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
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
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set USART__RX_ISR, Default_IRQ_handler
   .set USART__UDRE_ISR, Default_IRQ_handler
   .set USART__TX_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set ANALOG_COMP_ISR, Default_IRQ_handler
   .set TWI_ISR, Default_IRQ_handler
   .set SPM_Ready_ISR, Default_IRQ_handler
 end;

end.
