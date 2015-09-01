unit ATtiny167;

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
  // USI
  USIPP : byte absolute $00+$BC; // USI Pin Position
  USIBR : byte absolute $00+$BB; // USI Buffer Register
  USIDR : byte absolute $00+$BA; // USI Data Register
  USISR : byte absolute $00+$B9; // USI Status Register
  USICR : byte absolute $00+$B8; // USI Control Register
  // TIMER_COUNTER_0
  TIMSK0 : byte absolute $00+$6E; // Timer/Counter0 Interrupt Mask register
  TIFR0 : byte absolute $00+$35; // Timer/Counter0 Interrupt Flag Register
  TCCR0A : byte absolute $00+$45; // Timer/Counter0 Control Register A
  TCCR0B : byte absolute $00+$46; // Timer/Counter0 Control Register B
  TCNT0 : byte absolute $00+$47; // Timer/Counter0
  OCR0A : byte absolute $00+$48; // Timer/Counter0 Output Compare Register A
  ASSR : byte absolute $00+$B6; // Asynchronous Status Register
  GTCCR : byte absolute $00+$43; // General Timer Counter Control register
  // TIMER_COUNTER_1
  TIMSK1 : byte absolute $00+$6F; // Timer/Counter1 Interrupt Mask Register
  TIFR1 : byte absolute $00+$36; // Timer/Counter1 Interrupt Flag register
  TCCR1A : byte absolute $00+$80; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$81; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$82; // Timer/Counter1 Control Register C
  TCCR1D : byte absolute $00+$83; // Timer/Counter1 Control Register D
  TCNT1 : word absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$84+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$88; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AL : byte absolute $00+$88; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AH : byte absolute $00+$88+1; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1B : word absolute $00+$8A; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BL : byte absolute $00+$8A; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BH : byte absolute $00+$8A+1; // Timer/Counter1 Output Compare Register B  Bytes
  ICR1 : word absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$86+1; // Timer/Counter1 Input Capture Register  Bytes
  // WATCHDOG
  WDTCR : byte absolute $00+$60; // Watchdog Timer Control Register
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Address Register  Bytes
  EEARL : byte absolute $00+$41; // EEPROM Address Register  Bytes
  EEARH : byte absolute $00+$41+1; // EEPROM Address Register  Bytes
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // SPI
  SPDR : byte absolute $00+$4E; // SPI Data Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPCR : byte absolute $00+$4C; // SPI Control Register
  // AD_CONVERTER
  ADMUX : byte absolute $00+$7C; // The ADC multiplexer Selection Register
  ADC : word absolute $00+$78; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$78; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$78+1; // ADC Data Register  Bytes
  ADCSRA : byte absolute $00+$7A; // The ADC Control and Status register A
  ADCSRB : byte absolute $00+$7B; // The ADC Control and Status register B (Shared with ANALOG_COMPARATOR IO_MODULE)
  AMISCR : byte absolute $00+$77; // Analog Miscellaneous Control Register (Shared with CURRENT_SOURCE IO_MODULE)
  DIDR1 : byte absolute $00+$7F; // Digital Input Disable Register 1
  DIDR0 : byte absolute $00+$7E; // Digital Input Disable Register 0
  // CURRENT_SOURCE
  // ANALOG_COMPARATOR
  ACSR : byte absolute $00+$50; // Analog Comparator Control And Status Register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  PCICR : byte absolute $00+$68; // Pin Change Interrupt Control Register
  PCIFR : byte absolute $00+$3B; // Pin Change Interrupt Flag Register
  PCMSK1 : byte absolute $00+$6C; // Pin Change Mask Register 1
  PCMSK0 : byte absolute $00+$6B; // Pin Change Mask Register 0
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  PRR : byte absolute $00+$64; // Power Reduction Register
  SP : word absolute $00+$5D; // Stack Pointer  Bytes
  SPL : byte absolute $00+$5D; // Stack Pointer  Bytes
  SPH : byte absolute $00+$5D+1; // Stack Pointer  Bytes
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status register
  OSCCAL : byte absolute $00+$66; // Oscillator Calibration Register
  CLKPR : byte absolute $00+$61; // Clock Prescale Register
  CLKSELR : byte absolute $00+$63; // Clock Selection Register
  CLKCSR : byte absolute $00+$62; // Clock Control & Status Register
  DWDR : byte absolute $00+$51; // DebugWire data register
  GPIOR2 : byte absolute $00+$4B; // General Purpose IO register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose register 1
  GPIOR0 : byte absolute $00+$3E; // General purpose register 0
  PORTCR : byte absolute $00+$32; // General purpose register 0

const
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
  // TIMSK0
  OCIE0A = 1; // Timer/Counter0 Output Compare Match A Interrupt Enable
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR0
  OCF0A = 1; // Output Compare Flag 0A
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // TCCR0A
  COM0A = 6; // Compare Output Mode bits
  WGM0 = 0; // Waveform Genration Mode bits
  // TCCR0B
  FOC0A = 7; // Force Output Compare A
  CS0 = 0; // Clock Select bits
  // ASSR
  EXCLK = 6; // Enable External Clock Input
  AS0 = 5; // Asynchronous Timer/Counter0
  TCN0UB = 4; // Timer/Counter0 Update Busy
  OCR0AUB = 3; // Output Compare Register 0A  Update Busy
  TCR0AUB = 1; // Timer/Counter0 Control Register A Update Busy
  TCR0BUB = 0; // Timer/Counter0 Control Register B Update Busy
  // GTCCR
  TSM = 7; // Timer/Counter Synchronization Mode
  PSR0 = 1; // Prescaler Reset Asynchronous 8-bit Timer/Counter0
  PSR1 = 0; // Prescaler Reset Synchronous 16-bit Timer/Counter1
  // TIMSK1
  ICIE1 = 5; // Timer/Counter1 Input Capture Interrupt Enable
  OCIE1B = 2; // Timer/Counter1 Output Compare B Match Interrupt Enable
  OCIE1A = 1; // Timer/Counter1 Output Compare A Match Interrupt Enable
  TOIE1 = 0; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR1
  ICF1 = 5; // Timer/Counter1 Input Capture Flag
  OCF1B = 2; // Timer/Counter1 Output Compare B Match Flag
  OCF1A = 1; // Timer/Counter1 Output Compare A Match Flag
  TOV1 = 0; // Timer/Counter1 Overflow Flag
  // TCCR1A
  COM1A = 6; // Compare Output Mode 1A, bits
  COM1B = 4; // Compare Output Mode 1B, bits
  WGM1 = 0; // Pulse Width Modulator Select Bits
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceler
  ICES1 = 6; // Input Capture 1 Edge Select
  CS1 = 0; // Timer/Counter1 Clock Select bits
  // TCCR1C
  FOC1A = 7; // Timer/Counter1 Force Output Compare for Channel A
  FOC1B = 6; // Timer/Counter1 Force Output Compare for Channel B
  // TCCR1D
  OC1BX = 7; // Timer/Counter1 Output Compare X-pin Enable for Channel B
  OC1BW = 6; // Timer/Counter1 Output Compare W-pin Enable for Channel B
  OC1BV = 5; // Timer/Counter1 Output Compare V-pin Enable for Channel B
  OC1BU = 4; // Timer/Counter1 Output Compare U-pin Enable for Channel B
  OC1AX = 3; // Timer/Counter1 Output Compare X-pin Enable for Channel A
  OC1AW = 2; // Timer/Counter1 Output Compare W-pin Enable for Channel A
  OC1AV = 1; // Timer/Counter1 Output Compare V-pin Enable for Channel A
  OC1AU = 0; // Timer/Counter1 Output Compare U-pin Enable for Channel A
  // WDTCR
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
  BIN = 7; // Bipolar Input Mode
  ADTS = 0; // ADC Auto Trigger Source bits
  // AMISCR
  AREFEN = 2; // External Voltage Reference Input Enable
  XREFEN = 1; // Internal Voltage Reference Output Enable
  // DIDR1
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
  // AMISCR
  ISRCEN = 0; // Current Source Enable
  // ADCSRB
  ACME = 6; // Analog Comparator Multiplexer Enable
  ACIR = 4; // Analog Comparator Internal Voltage Reference Select Bits
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACIRS = 6; // Analog Comparator Internal Reference Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIC = 2; // Analog Comparator Input Capture Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // EICRA
  ISC1 = 2; // External Interrupt Sense Control 1 Bits
  ISC0 = 0; // External Interrupt Sense Control 0 Bits
  // EIMSK
  INT = 0; // External Interrupt Request 1 Enable
  // EIFR
  INTF = 0; // External Interrupt Flags
  // PCICR
  PCIE = 0; // Pin Change Interrupt Enable  on any PCINT14..8 pin
  // PCIFR
  PCIF = 0; // Pin Change Interrupt Flags
  // PCMSK1
  PCINT = 0; // Pin Change Enable Masks
  // PCMSK0
  // SPMCSR
  RWWSB = 6; // Read While Write Section Busy
  SIGRD = 5; // Signature Row Read
  CTPB = 4; // Clear Temporary Page Buffer
  RFLB = 3; // Read Fuse and Lock Bits
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
  // PRR
  PRLIN = 5; // Power Reduction LINUART
  PRSPI = 4; // Power Reduction SPI
  PRTIM1 = 3; // Power Reduction Timer/Counter1
  PRTIM0 = 2; // Power Reduction Timer/Counter0
  PRUSI = 1; // Power Reduction USI
  PRADC = 0; // Power Reduction ADC
  // MCUCR
  BODSE = 6; // BOD Sleep Enable
  BODS = 5; // BOD Sleep
  PUD = 4; // Pull-up Disable
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-On Reset Flag
  // MCUSR
  SM = 1; // Sleep Mode Select Bits
  SE = 0; // Sleep Enable
  // CLKPR
  CLKPCE = 7; // Clock Prescaler Change Enable
  CLKPS = 0; // Clock Prescaler Select Bits
  // CLKSELR
  COUT = 6; // Clock Out - CKOUT fuse substitution
  CSUT = 4; // Clock Start-up Time bit 1 - SUT1 fuse substitution
  CSEL = 0; // Clock Source Select bit 3 - CKSEL3 fuse substitution
  // CLKCSR
  CLKCCE = 7; // Clock Control Change Enable
  CLKRDY = 4; // Clock Ready Flag
  CLKC = 0; // Clock Control bits

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 3 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 4 Pin Change Interrupt Request 1
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 5 Watchdog Time-Out Interrupt
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 6 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 7 Timer/Counter1 Compare Match 1A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 8 Timer/Counter1 Compare Match 1B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 9 Timer/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 10 Timer/Counter0 Compare Match 0A
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 11 Timer/Counter0 Overflow
procedure LIN_TC_ISR; external name 'LIN_TC_ISR'; // Interrupt 12 LIN Transfer Complete
procedure LIN_ERR_ISR; external name 'LIN_ERR_ISR'; // Interrupt 13 LIN Error
procedure SPI_STC_ISR; external name 'SPI_STC_ISR'; // Interrupt 14 SPI Serial Transfer Complete
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 15 ADC Conversion Complete
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 16 EEPROM Ready
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 17 Analog Comparator
procedure USI_START_ISR; external name 'USI_START_ISR'; // Interrupt 18 USI Start
procedure USI_OVF_ISR; external name 'USI_OVF_ISR'; // Interrupt 19 USI Overflow

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
   rjmp WDT_ISR
   rjmp TIMER1_CAPT_ISR
   rjmp TIMER1_COMPA_ISR
   rjmp TIMER1_COMPB_ISR
   rjmp TIMER1_OVF_ISR
   rjmp TIMER0_COMPA_ISR
   rjmp TIMER0_OVF_ISR
   rjmp LIN_TC_ISR
   rjmp LIN_ERR_ISR
   rjmp SPI_STC_ISR
   rjmp ADC_ISR
   rjmp EE_RDY_ISR
   rjmp ANA_COMP_ISR
   rjmp USI_START_ISR
   rjmp USI_OVF_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak WDT_ISR
   .weak TIMER1_CAPT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_OVF_ISR
   .weak LIN_TC_ISR
   .weak LIN_ERR_ISR
   .weak SPI_STC_ISR
   .weak ADC_ISR
   .weak EE_RDY_ISR
   .weak ANA_COMP_ISR
   .weak USI_START_ISR
   .weak USI_OVF_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set TIMER1_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set LIN_TC_ISR, Default_IRQ_handler
   .set LIN_ERR_ISR, Default_IRQ_handler
   .set SPI_STC_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set USI_START_ISR, Default_IRQ_handler
   .set USI_OVF_ISR, Default_IRQ_handler
 end;

end.
