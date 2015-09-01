unit ATtiny828;

{$goto on}

interface

var
  // SPI
  SPDR : byte absolute $00+$4E; // SPI Data Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPCR : byte absolute $00+$4C; // SPI Control Register
  // PORTA
  PUEA : byte absolute $00+$23; // Pull-up Enable Control Register
  PORTA : byte absolute $00+$22; // Port A Data Register
  DDRA : byte absolute $00+$21; // Data Direction Register, Port A
  PINA : byte absolute $00+$20; // Port A Input Pins
  // PORTB
  PUEB : byte absolute $00+$27; // Pull-up Enable Control Register
  PORTB : byte absolute $00+$26; // Port B Data Register
  DDRB : byte absolute $00+$25; // Data Direction Register, Port B
  PINB : byte absolute $00+$24; // Port B Input Pins
  // PORTC
  PHDE : byte absolute $00+$34; // Port High Drive Enable Register
  PUEC : byte absolute $00+$2B; // Pull-up Enable Control Register
  PORTC : byte absolute $00+$2A; // Port C Data Register
  DDRC : byte absolute $00+$29; // Data Direction Register, Port C
  PINC : byte absolute $00+$28; // Port C Input Pins
  // PORTD
  PUED : byte absolute $00+$2F; // Pull-up Enable Control Register
  PORTD : byte absolute $00+$2E; // Port D Data Register
  DDRD : byte absolute $00+$2D; // Data Direction Register, Port D
  PIND : byte absolute $00+$2C; // Port D Input Pins
  // CPU
  PRR : byte absolute $00+$64; // Power Reduction Register
  CCP : byte absolute $00+$56; // Configuration Change Protection
  OSCCAL0 : byte absolute $00+$66; // Oscillator Calibration Value
  OSCCAL1 : byte absolute $00+$67; // 
  OSCTCAL0A : byte absolute $00+$F0; // 
  OSCTCAL0B : byte absolute $00+$F1; // 
  CLKPR : byte absolute $00+$61; // Clock Prescale Register
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  GPIOR2 : byte absolute $00+$4B; // General Purpose I/O Register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose I/O Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose I/O Register 0
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control and Status Register
  // TIMER_COUNTER_0
  OCR0B : byte absolute $00+$48; // Timer/Counter0 Output Compare Register
  OCR0A : byte absolute $00+$47; // Timer/Counter0 Output Compare Register
  TCNT0 : byte absolute $00+$46; // Timer/Counter0
  TCCR0B : byte absolute $00+$45; // Timer/Counter Control Register B
  TCCR0A : byte absolute $00+$44; // Timer/Counter  Control Register A
  TIMSK0 : byte absolute $00+$6E; // Timer/Counter0 Interrupt Mask Register
  TIFR0 : byte absolute $00+$35; // Timer/Counter0 Interrupt Flag register
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
  // TOCPM
  TOCPMSA1 : byte absolute $00+$E9; // Timer Output Compare Pin Mux Selection 1
  TOCPMSA0 : byte absolute $00+$E8; // Timer Output Compare Pin Mux Selection 0
  TOCPMCOE : byte absolute $00+$E2; // Timer Output Compare Pin Mux Channel Output Enable
  // AD_CONVERTER
  ADMUXA : byte absolute $00+$7C; // The ADC multiplexer Selection Register A
  ADMUXB : byte absolute $00+$7D; // The ADC multiplexer Selection Register B
  ADCSRA : byte absolute $00+$7A; // The ADC Control and Status register
  ADC : word absolute $00+$78; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$78; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$78+1; // ADC Data Register  Bytes
  ADCSRB : byte absolute $00+$7B; // ADC Control and Status Register B
  DIDR3 : byte absolute $00+$DF; // Digital Input Disable Register 2
  DIDR2 : byte absolute $00+$DE; // Digital Input Disable Register 2
  DIDR1 : byte absolute $00+$7F; // Digital Input Disable Register 1
  DIDR0 : byte absolute $00+$7E; // Digital Input Disable Register 0
  // ANALOG_COMPARATOR
  ACSRB : byte absolute $00+$4F; // Analog Comparator Control And Status Register B
  ACSRA : byte absolute $00+$50; // Analog Comparator Control And Status Register A
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  PCICR : byte absolute $00+$68; // Pin Change Interrupt Control Register
  PCMSK3 : byte absolute $00+$73; // Pin Change Mask Register 3
  PCMSK2 : byte absolute $00+$6D; // Pin Change Mask Register 2
  PCMSK1 : byte absolute $00+$6C; // Pin Change Mask Register 1
  PCMSK0 : byte absolute $00+$6B; // Pin Change Mask Register 0
  PCIFR : byte absolute $00+$3B; // Pin Change Interrupt Flag Register
  // WATCHDOG
  WDTCSR : byte absolute $00+$60; // Watchdog Timer Control and Status Register
  // EEPROM
  EEAR : byte absolute $00+$41; // EEPROM Read/Write Access
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // TWI
  TWSCRA : byte absolute $00+$B8; // TWI Slave Control Register A
  TWSCRB : byte absolute $00+$B9; // TWI Slave Control Register B
  TWSSRA : byte absolute $00+$BA; // TWI Slave Status Register A
  TWSA : byte absolute $00+$BC; // TWI Slave Address Register
  TWSD : byte absolute $00+$BD; // TWI Slave Data Register
  TWSAM : byte absolute $00+$BB; // TWI Slave Address Mask Register
  // USART
  UDR : byte absolute $00+$C6; // USART I/O Data Register
  UCSRA : byte absolute $00+$C0; // USART Control and Status Register A
  UCSRB : byte absolute $00+$C1; // USART Control and Status Register B
  UCSRC : byte absolute $00+$C2; // USART Control and Status Register C
  UCSRD : byte absolute $00+$C3; // USART Control and Status Register D
  UBRR : word absolute $00+$C4; // USART Baud Rate Register  Bytes
  UBRRL : byte absolute $00+$C4; // USART Baud Rate Register  Bytes
  UBRRH : byte absolute $00+$C4+1; // USART Baud Rate Register  Bytes

const
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
  // PHDE
  PHDEC = 2; // Port C High Drive Enable
  // PRR
  PRTWI = 7; // Power Reduction TWI
  PRTIM0 = 5; // Power Reduction Timer/Counter0
  PRTIM1 = 3; // Power Reduction Timer/Counter1
  PRSPI = 2; // Power Reduction SPI
  PRUSART0 = 1; // Power Reduction USART 0
  PRADC = 0; // Power Reduction ADC
  // CLKPR
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
  // MCUCR
  IVSEL = 1; // Interrupt Vector Select
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // SMCR
  SM = 1; // Sleep Mode Select Bits
  SE = 0; // Sleep Enable
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read-While-Write Section Busy
  RSIG = 5; // Read Device Signature Imprint Table
  RWWSRE = 4; // Read-While-Write section read enable
  RWFLB = 3; // Read/Write Fuse and Lock Bits
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
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
  // TOCPMSA1
  TOCC7S = 6; // Timer Output Compare Channel 7 Selection Bits
  TOCC6S = 4; // Timer Output Compare Channel 6 Selection Bits
  TOCC5S = 2; // Timer Output Compare Channel 5 Selection Bits
  TOCC4S = 0; // Timer Output Compare Channel 4 Selection Bits
  // TOCPMSA0
  TOCC3S = 6; // Timer Output Compare Channel 3 Selection Bits
  TOCC2S = 4; // Timer Output Compare Channel 2 Selection Bits
  TOCC1S = 2; // Timer Output Compare Channel 1 Selection Bits
  TOCC0S = 0; // Timer Output Compare Channel 0 Selection Bits
  // TOCPMCOE
  TOCC7OE = 7; // Timer Output Compare Channel 7 Output Enable
  TOCC6OE = 6; // Timer Output Compare Channel 6 Output Enable
  TOCC5OE = 5; // Timer Output Compare Channel 5 Output Enable
  TOCC4OE = 4; // Timer Output Compare Channel 4 Output Enable
  TOCC3OE = 3; // Timer Output Compare Channel 3 Output Enable
  TOCC2OE = 2; // Timer Output Compare Channel 2 Output Enable
  TOCC1OE = 1; // Timer Output Compare Channel 1 Output Enable
  TOCC0OE = 0; // Timer Output Compare Channel 0 Output Enable
  // ADMUXA
  MUX = 0; // Analog Channel Selection Bits 4:0
  // ADMUXB
  REFS = 5; // Reference Selection Bit
  MUX5 = 0; // Analog Channel Selection Bit 5
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
  // DIDR3
  ADC27D = 3; // ADC27 Digital input Disable
  ADC26D = 2; // ADC26 Digital input Disable
  ADC25D = 1; // ADC25 Digital input Disable
  ADC24D = 0; // ADC24 Digital input Disable
  // DIDR2
  ADC23D = 7; // ADC23 Digital input Disable
  ADC22D = 6; // ADC22 Digital input Disable
  ADC21D = 5; // ADC21 Digital input Disable
  ADC20D = 4; // ADC20 Digital input Disable
  ADC19D = 3; // ADC19 Digital input Disable
  ADC18D = 2; // ADC18 Digital input Disable
  ADC17D = 1; // ADC17 Digital input Disable
  ADC16D = 0; // ADC16 Digital input Disable
  // DIDR1
  ADC15D = 7; // ADC15 Digital input Disable
  ADC14D = 6; // ADC14 Digital input Disable
  ADC13D = 5; // ADC13 Digital input Disable
  ADC12D = 4; // ADC12 Digital input Disable
  ADC11D = 3; // ADC11 Digital input Disable
  ADC10D = 2; // ADC10 Digital input Disable
  ADC9D = 1; // ADC9 Digital input Disable
  ADC8D = 0; // ADC8 Digital input Disable
  // DIDR0
  ADC7D = 7; // ADC7 Digital input Disable
  ADC6D = 6; // ADC6 Digital input Disable
  ADC5D = 5; // ADC5 Digital input Disable
  ADC4D = 4; // ADC4 Digital input Disable
  ADC3D = 3; // ADC3 Digital input Disable
  ADC2D = 2; // ADC2 Digital input Disable
  ADC1D = 1; // ADC1 Digital input Disable
  ADC0D = 0; // ADC0 Digital input Disable
  // ACSRB
  HSEL = 7; // Hysteresis Select
  HLEV = 6; // Hysteresis Level
  ACNMUX = 2; // Analog Comparator Negative Input Multiplexer
  ACPMUX = 0; // Analog Comparator Positive Input Multiplexer Bits 1:0
  // ACSRA
  ACD = 7; // Analog Comparator Disable
  ACPMUX2 = 6; // Analog Comparator Positive Input Multiplexer Bit 2
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // EICRA
  ISC1 = 2; // External Interrupt Sense Control 1 Bits
  ISC0 = 0; // External Interrupt Sense Control 0 Bits
  // EIMSK
  INT = 0; // External Interrupt Request Enables
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
  // WDTCSR
  WDIF = 7; // Watchdog Timer Interrupt Flag
  WDIE = 6; // Watchdog Timer Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDE = 3; // Watch Dog Enable
  // EECR
  EEPM = 4; // EEPROM Programming Mode Bits
  EERIE = 3; // EEProm Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // TWSCRA
  TWSHE = 7; // TWI SDA Hold Time Enable
  TWDIE = 5; // TWI Data Interrupt Enable
  TWASIE = 4; // TWI Address/Stop Interrupt Enable
  TWEN = 3; // Two-Wire Interface Enable
  TWSIE = 2; // TWI Stop Interrupt Enable
  TWPME = 1; // TWI Promiscuous Mode Enable
  TWSME = 0; // TWI Smart Mode Enable
  // TWSCRB
  TWHNM = 3; // TWI High Noise Mode
  TWAA = 2; // TWI Acknowledge Action
  TWCMD = 0; // 
  // TWSSRA
  TWDIF = 7; // TWI Data Interrupt Flag.
  TWASIF = 6; // TWI Address/Stop Interrupt Flag
  TWCH = 5; // TWI Clock Hold
  TWRA = 4; // TWI Receive Acknowledge
  TWC = 3; // TWI Collision
  TWBE = 2; // TWI Bus Error
  TWDIR = 1; // TWI Read/Write Direction
  TWAS = 0; // TWI Address or Stop
  // TWSD
  // TWSAM
  TWAE = 0; // TWI Address Enable
  // UCSRA
  RXC = 7; // USART Receive Complete
  TXC = 6; // USART Transmitt Complete
  UDRE = 5; // USART Data Register Empty
  FE = 4; // Framing Error
  DOR = 3; // Data overRun
  UPE = 2; // Parity Error
  U2X = 1; // Double the USART transmission speed
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
  UCSZ = 1; // Character Size
  UCPOL = 0; // Clock Polarity
  // UCSRD
  RXSIE = 7; // USART RX Start Interrupt Enable
  RXS = 6; // USART RX Start Flag
  SFDE = 5; // USART RX Start Frame Detection Enable

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 3 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 4 Pin Change Interrupt Request 1
procedure PCINT2_ISR; external name 'PCINT2_ISR'; // Interrupt 5 Pin Change Interrupt Request 2
procedure PCINT3_ISR; external name 'PCINT3_ISR'; // Interrupt 6 Pin Change Interrupt Request 3
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 7 Watchdog Time-out Interrupt
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 8 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 9 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 10 Timer/Counter1 Compare Match B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 11 Timer/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 12 Timer/Counter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 13 Timer/Counter0 Compare Match B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 14 Timer/Counter0 Overflow
procedure SPI__STC_ISR; external name 'SPI__STC_ISR'; // Interrupt 15 SPI Serial Transfer Complete
procedure USART__START_ISR; external name 'USART__START_ISR'; // Interrupt 16 USART, Start
procedure USART__RX_ISR; external name 'USART__RX_ISR'; // Interrupt 17 USART Rx Complete
procedure USART__UDRE_ISR; external name 'USART__UDRE_ISR'; // Interrupt 18 USART, Data Register Empty
procedure USART__TX_ISR; external name 'USART__TX_ISR'; // Interrupt 19 USART Tx Complete
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 20 ADC Conversion Complete
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 21 EEPROM Ready
procedure ANALOG_COMP_ISR; external name 'ANALOG_COMP_ISR'; // Interrupt 22 Analog Comparator
procedure TWI_SLAVE_ISR; external name 'TWI_SLAVE_ISR'; // Interrupt 23 Two-wire Serial Interface
procedure SPM_Ready_ISR; external name 'SPM_Ready_ISR'; // Interrupt 24 Store Program Memory Read
procedure QTRIP_ISR; external name 'QTRIP_ISR'; // Interrupt 25 Touch Sensing

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
   rjmp PCINT3_ISR
   rjmp WDT_ISR
   rjmp TIMER1_CAPT_ISR
   rjmp TIMER1_COMPA_ISR
   rjmp TIMER1_COMPB_ISR
   rjmp TIMER1_OVF_ISR
   rjmp TIMER0_COMPA_ISR
   rjmp TIMER0_COMPB_ISR
   rjmp TIMER0_OVF_ISR
   rjmp SPI__STC_ISR
   rjmp USART__START_ISR
   rjmp USART__RX_ISR
   rjmp USART__UDRE_ISR
   rjmp USART__TX_ISR
   rjmp ADC_ISR
   rjmp EE_READY_ISR
   rjmp ANALOG_COMP_ISR
   rjmp TWI_SLAVE_ISR
   rjmp SPM_Ready_ISR
   rjmp QTRIP_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak PCINT2_ISR
   .weak PCINT3_ISR
   .weak WDT_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak TIMER0_OVF_ISR
   .weak SPI__STC_ISR
   .weak USART__START_ISR
   .weak USART__RX_ISR
   .weak USART__UDRE_ISR
   .weak USART__TX_ISR
   .weak ADC_ISR
   .weak EE_READY_ISR
   .weak ANALOG_COMP_ISR
   .weak TWI_SLAVE_ISR
   .weak SPM_Ready_ISR
   .weak QTRIP_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set PCINT2_ISR, Default_IRQ_handler
   .set PCINT3_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set SPI__STC_ISR, Default_IRQ_handler
   .set USART__START_ISR, Default_IRQ_handler
   .set USART__RX_ISR, Default_IRQ_handler
   .set USART__UDRE_ISR, Default_IRQ_handler
   .set USART__TX_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set ANALOG_COMP_ISR, Default_IRQ_handler
   .set TWI_SLAVE_ISR, Default_IRQ_handler
   .set SPM_Ready_ISR, Default_IRQ_handler
   .set QTRIP_ISR, Default_IRQ_handler
 end;

end.
