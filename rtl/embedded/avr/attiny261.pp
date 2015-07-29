unit ATtiny261;

{$goto on}

interface

var
  // PORTA
  PORTA : byte absolute $00+$3B; // Port A Data Register
  DDRA : byte absolute $00+$3A; // Port A Data Direction Register
  PINA : byte absolute $00+$39; // Port A Input Pins
  // PORTB
  PORTB : byte absolute $00+$38; // Port B Data Register
  DDRB : byte absolute $00+$37; // Port B Data Direction Register
  PINB : byte absolute $00+$36; // Port B Input Pins
  // AD_CONVERTER
  ADMUX : byte absolute $00+$27; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$26; // The ADC Control and Status register
  ADC : word absolute $00+$24; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$24; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$24+1; // ADC Data Register  Bytes
  ADCSRB : byte absolute $00+$23; // ADC Control and Status Register B
  DIDR1 : byte absolute $00+$22; // Digital Input Disable Register 1
  DIDR0 : byte absolute $00+$21; // Digital Input Disable Register 0
  // ANALOG_COMPARATOR
  ACSRB : byte absolute $00+$29; // Analog Comparator Control And Status Register B
  ACSRA : byte absolute $00+$28; // Analog Comparator Control And Status Register A
  // USI
  USIPP : byte absolute $00+$31; // USI Pin Position
  USIBR : byte absolute $00+$30; // USI Buffer Register
  USIDR : byte absolute $00+$2F; // USI Data Register
  USISR : byte absolute $00+$2E; // USI Status Register
  USICR : byte absolute $00+$2D; // USI Control Register
  // EEPROM
  EEAR : word absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARL : byte absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARH : byte absolute $00+$3E+1; // EEPROM Address Register  Bytes
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register
  // WATCHDOG
  WDTCR : byte absolute $00+$41; // Watchdog Timer Control Register
  // TIMER_COUNTER_0
  TIMSK : byte absolute $00+$59; // Timer/Counter Interrupt Mask Register
  TIFR : byte absolute $00+$58; // Timer/Counter0 Interrupt Flag register
  TCCR0A : byte absolute $00+$35; // Timer/Counter  Control Register A
  TCCR0B : byte absolute $00+$53; // Timer/Counter Control Register B
  TCNT0H : byte absolute $00+$34; // Timer/Counter0 High
  TCNT0L : byte absolute $00+$52; // Timer/Counter0 Low
  OCR0A : byte absolute $00+$33; // Timer/Counter0 Output Compare Register
  OCR0B : byte absolute $00+$32; // Timer/Counter0 Output Compare Register
  // TIMER_COUNTER_1
  TCCR1A : byte absolute $00+$50; // Timer/Counter Control Register A
  TCCR1B : byte absolute $00+$4F; // Timer/Counter Control Register B
  TCCR1C : byte absolute $00+$47; // Timer/Counter Control Register C
  TCCR1D : byte absolute $00+$46; // Timer/Counter Control Register D
  TCCR1E : byte absolute $00+$20; // Timer/Counter1 Control Register E
  TCNT1 : byte absolute $00+$4E; // Timer/Counter Register
  TC1H : byte absolute $00+$45; // Timer/Counter 1 Register High
  OCR1A : byte absolute $00+$4D; // Output Compare Register
  OCR1B : byte absolute $00+$4C; // Output Compare Register
  OCR1C : byte absolute $00+$4B; // Output compare register
  OCR1D : byte absolute $00+$4A; // Output compare register
  DT1 : byte absolute $00+$44; // Timer/Counter 1 Dead Time Value
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  // EXTERNAL_INTERRUPT
  MCUCR : byte absolute $00+$55; // MCU Control Register
  GIMSK : byte absolute $00+$5B; // General Interrupt Mask Register
  GIFR : byte absolute $00+$5A; // General Interrupt Flag register
  PCMSK1 : byte absolute $00+$42; // Pin Change Enable Mask 1
  PCMSK0 : byte absolute $00+$43; // Pin Change Enable Mask 0
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  PRR : byte absolute $00+$56; // Power Reduction Register
  SPL : byte absolute $00+$5D; // Stack Pointer Low Byte
  MCUSR : byte absolute $00+$54; // MCU Status register
  OSCCAL : byte absolute $00+$51; // Oscillator Calibration Register
  CLKPR : byte absolute $00+$48; // Clock Prescale Register
  PLLCSR : byte absolute $00+$49; // PLL Control and status register
  DWDR : byte absolute $00+$40; // debugWire data register
  GPIOR2 : byte absolute $00+$2C; // General Purpose IO register 2
  GPIOR1 : byte absolute $00+$2B; // General Purpose register 1
  GPIOR0 : byte absolute $00+$2A; // General purpose register 0

const
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
  ADPS = 0; // ADC Prescaler Select Bits
  // ADCSRB
  BIN = 7; // Bipolar Input Mode
  GSEL = 6; // Gain Select
  IPR = 5; // Input Polarity Mode
  REFS2 = 4; // 
  MUX5 = 3; // 
  ADTS = 0; // ADC Auto Trigger Sources
  // DIDR1
  ADC10D = 7; // ADC10 Digital input Disable
  ADC9D = 6; // ADC9 Digital input Disable
  ADC8D = 5; // ADC8 Digital input Disable
  ADC7D = 4; // ADC7 Digital input Disable
  // DIDR0
  ADC6D = 7; // ADC6 Digital input Disable
  ADC5D = 6; // ADC5 Digital input Disable
  ADC4D = 5; // ADC4 Digital input Disable
  ADC3D = 4; // ADC3 Digital input Disable
  AREFD = 3; // AREF Digital Input Disable
  ADC2D = 2; // ADC2 Digital input Disable
  ADC1D = 1; // ADC1 Digital input Disable
  ADC0D = 0; // ADC0 Digital input Disable
  // ACSRB
  HSEL = 7; // Hysteresis Select
  HLEV = 6; // Hysteresis Level
  ACM = 0; // Analog Comparator Multiplexer
  // ACSRA
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACME = 2; // Analog Comparator Multiplexer Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
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
  // EECR
  EEPM = 4; // EEPROM Programming Mode Bits
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // WDTCR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  // TIMSK
  OCIE0A = 4; // Timer/Counter0 Output Compare Match A Interrupt Enable
  OCIE0B = 3; // Timer/Counter0 Output Compare Match B Interrupt Enable
  TOIE0 = 1; // Timer/Counter0 Overflow Interrupt Enable
  TICIE0 = 0; // Timer/Counter0 Input Capture Interrupt Enable
  // TIFR
  OCF0A = 4; // Timer/Counter0 Output Compare Flag 0A
  OCF0B = 3; // Timer/Counter0 Output Compare Flag 0B
  TOV0 = 1; // Timer/Counter0 Overflow Flag
  ICF0 = 0; // Timer/Counter0 Input Capture Flag
  // TCCR0A
  TCW0 = 7; // Timer/Counter 0 Width
  ICEN0 = 6; // Input Capture Mode Enable
  ICNC0 = 5; // Input Capture Noice Canceler
  ICES0 = 4; // Input Capture Edge Select
  ACIC0 = 3; // Analog Comparator Input Capture Enable
  WGM00 = 0; // Waveform Generation Mode
  // TCCR0B
  TSM = 4; // Timer/Counter Synchronization Mode
  PSR0 = 3; // Timer/Counter 0 Prescaler Reset
  CS0 = 0; // Clock Select
  // TCCR1A
  COM1A = 6; // Compare Output Mode, Bits
  COM1B = 4; // Compare Output Mode, Bits
  FOC1A = 3; // Force Output Compare Match 1A
  FOC1B = 2; // Force Output Compare Match 1B
  PWM1A = 1; // Pulse Width Modulator Enable
  PWM1B = 0; // Pulse Width Modulator Enable
  // TCCR1B
  PSR1 = 6; // Timer/Counter 1 Prescaler reset
  DTPS1 = 4; // Dead Time Prescaler
  CS1 = 0; // Clock Select Bits
  // TCCR1C
  COM1A1S = 7; // COM1A1 Shadow Bit
  COM1A0S = 6; // COM1A0 Shadow Bit
  COM1B1S = 5; // COM1B1 Shadow Bit
  COM1B0S = 4; // COM1B0 Shadow Bit
  COM1D = 2; // Comparator D output mode
  FOC1D = 1; // Force Output Compare Match 1D
  PWM1D = 0; // Pulse Width Modulator D Enable
  // TCCR1D
  FPIE1 = 7; // Fault Protection Interrupt Enable
  FPEN1 = 6; // Fault Protection Mode Enable
  FPNC1 = 5; // Fault Protection Noise Canceler
  FPES1 = 4; // Fault Protection Edge Select
  FPAC1 = 3; // Fault Protection Analog Comparator Enable
  FPF1 = 2; // Fault Protection Interrupt Flag
  WGM1 = 0; // Waveform Generation Mode Bit
  // TCCR1E
  OC1OE = 0; // Ouput Compare Override Enable Bits
  // TIMSK
  OCIE1D = 7; // OCIE1D: Timer/Counter1 Output Compare Interrupt Enable
  OCIE1A = 6; // OCIE1A: Timer/Counter1 Output Compare Interrupt Enable
  OCIE1B = 5; // OCIE1A: Timer/Counter1 Output Compare B Interrupt Enable
  TOIE1 = 2; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR
  OCF1D = 7; // Timer/Counter1 Output Compare Flag 1D
  OCF1A = 6; // Timer/Counter1 Output Compare Flag 1A
  OCF1B = 5; // Timer/Counter1 Output Compare Flag 1B
  TOV1 = 2; // Timer/Counter1 Overflow Flag
  // DT1
  DT1H = 4; // 
  DT1L = 0; // 
  // SPMCSR
  CTPB = 4; // Clear temporary page buffer
  RFLB = 3; // Read fuse and lock bits
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // MCUCR
  ISC01 = 1; // Interrupt Sense Control 0 Bit 1
  ISC00 = 0; // Interrupt Sense Control 0 Bit 0
  // GIMSK
  INT = 6; // External Interrupt Request 1 Enable
  PCIE = 4; // Pin Change Interrupt Enables
  // GIFR
  INTF = 6; // External Interrupt Flags
  PCIF = 5; // Pin Change Interrupt Flag
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
  PRTIM1 = 3; // Power Reduction Timer/Counter1
  PRTIM0 = 2; // Power Reduction Timer/Counter0
  PRUSI = 1; // Power Reduction USI
  PRADC = 0; // Power Reduction ADC
  // MCUCR
  PUD = 6; // Pull-up Disable
  SE = 5; // Sleep Enable
  SM = 3; // Sleep Mode Select Bits
  ISC0 = 0; // Interrupt Sense Control 0 bits
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-On Reset Flag
  // CLKPR
  CLKPCE = 7; // Clock Prescaler Change Enable
  CLKPS = 0; // Clock Prescaler Select Bits
  // PLLCSR
  LSM = 7; // Low speed mode
  PCKE = 2; // PCK Enable
  PLLE = 1; // PLL Enable
  PLOCK = 0; // PLL Lock detector

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt 0
procedure PCINT_ISR; external name 'PCINT_ISR'; // Interrupt 2 Pin Change Interrupt
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 3 Timer/Counter1 Compare Match 1A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 4 Timer/Counter1 Compare Match 1B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 5 Timer/Counter1 Overflow
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 6 Timer/Counter0 Overflow
procedure USI_START_ISR; external name 'USI_START_ISR'; // Interrupt 7 USI Start
procedure USI_OVF_ISR; external name 'USI_OVF_ISR'; // Interrupt 8 USI Overflow
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 9 EEPROM Ready
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 10 Analog Comparator
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 11 ADC Conversion Complete
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 12 Watchdog Time-Out
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 13 External Interrupt 1
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 14 Timer/Counter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 15 Timer/Counter0 Compare Match B
procedure TIMER0_CAPT_ISR; external name 'TIMER0_CAPT_ISR'; // Interrupt 16 ADC Conversion Complete
procedure TIMER1_COMPD_ISR; external name 'TIMER1_COMPD_ISR'; // Interrupt 17 Timer/Counter1 Compare Match D
procedure FAULT_PROTECTION_ISR; external name 'FAULT_PROTECTION_ISR'; // Interrupt 18 Timer/Counter1 Fault Protection

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp PCINT_ISR
   rjmp TIMER1_COMPA_ISR
   rjmp TIMER1_COMPB_ISR
   rjmp TIMER1_OVF_ISR
   rjmp TIMER0_OVF_ISR
   rjmp USI_START_ISR
   rjmp USI_OVF_ISR
   rjmp EE_RDY_ISR
   rjmp ANA_COMP_ISR
   rjmp ADC_ISR
   rjmp WDT_ISR
   rjmp INT1_ISR
   rjmp TIMER0_COMPA_ISR
   rjmp TIMER0_COMPB_ISR
   rjmp TIMER0_CAPT_ISR
   rjmp TIMER1_COMPD_ISR
   rjmp FAULT_PROTECTION_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak PCINT_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_OVF_ISR
   .weak USI_START_ISR
   .weak USI_OVF_ISR
   .weak EE_RDY_ISR
   .weak ANA_COMP_ISR
   .weak ADC_ISR
   .weak WDT_ISR
   .weak INT1_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak TIMER0_CAPT_ISR
   .weak TIMER1_COMPD_ISR
   .weak FAULT_PROTECTION_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set PCINT_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set USI_START_ISR, Default_IRQ_handler
   .set USI_OVF_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_CAPT_ISR, Default_IRQ_handler
   .set TIMER1_COMPD_ISR, Default_IRQ_handler
   .set FAULT_PROTECTION_ISR, Default_IRQ_handler
 end;

end.
