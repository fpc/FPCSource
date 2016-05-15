unit ATtiny44A;

{$goto on}

interface

var
  // PORTA
  PORTA : byte absolute $00+$3B; // Port A Data Register
  DDRA : byte absolute $00+$3A; // Port A Data Direction Register
  PINA : byte absolute $00+$39; // Port A Input Pins
  // PORTB
  PORTB : byte absolute $00+$38; // Data Register, Port B
  DDRB : byte absolute $00+$37; // Data Direction Register, Port B
  PINB : byte absolute $00+$36; // Input Pins, Port B
  // ANALOG_COMPARATOR
  ADCSRB : byte absolute $00+$23; // ADC Control and Status Register B
  ACSR : byte absolute $00+$28; // Analog Comparator Control And Status Register
  DIDR0 : byte absolute $00+$21; // 
  // AD_CONVERTER
  ADMUX : byte absolute $00+$27; // ADC Multiplexer Selection Register
  ADCSRA : byte absolute $00+$26; // ADC Control and Status Register A
  ADC : word absolute $00+$24; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$24; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$24+1; // ADC Data Register  Bytes
  // USI
  USIBR : byte absolute $00+$30; // USI Buffer Register
  USIDR : byte absolute $00+$2F; // USI Data Register
  USISR : byte absolute $00+$2E; // USI Status Register
  USICR : byte absolute $00+$2D; // USI Control Register
  // EXTERNAL_INTERRUPT
  MCUCR : byte absolute $00+$55; // MCU Control Register
  GIMSK : byte absolute $00+$5B; // General Interrupt Mask Register
  GIFR : byte absolute $00+$5A; // General Interrupt Flag register
  PCMSK1 : byte absolute $00+$40; // Pin Change Enable Mask 1
  PCMSK0 : byte absolute $00+$32; // Pin Change Enable Mask 0
  // EEPROM
  EEAR : word absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARL : byte absolute $00+$3E; // EEPROM Address Register  Bytes
  EEARH : byte absolute $00+$3E+1; // EEPROM Address Register  Bytes
  EEDR : byte absolute $00+$3D; // EEPROM Data Register
  EECR : byte absolute $00+$3C; // EEPROM Control Register
  // WATCHDOG
  WDTCSR : byte absolute $00+$41; // Watchdog Timer Control Register
  // TIMER_COUNTER_0
  TIMSK0 : byte absolute $00+$59; // Timer/Counter Interrupt Mask Register
  TIFR0 : byte absolute $00+$58; // Timer/Counter0 Interrupt Flag Register
  TCCR0A : byte absolute $00+$50; // Timer/Counter  Control Register A
  TCCR0B : byte absolute $00+$53; // Timer/Counter Control Register B
  TCNT0 : byte absolute $00+$52; // Timer/Counter0
  OCR0A : byte absolute $00+$56; // Timer/Counter0 Output Compare Register A
  OCR0B : byte absolute $00+$5C; // Timer/Counter0 Output Compare Register B
  GTCCR : byte absolute $00+$43; // General Timer/Counter Control Register
  // TIMER_COUNTER_1
  TIMSK1 : byte absolute $00+$2C; // Timer/Counter1 Interrupt Mask Register
  TIFR1 : byte absolute $00+$2B; // Timer/Counter Interrupt Flag register
  TCCR1A : byte absolute $00+$4F; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$4E; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$42; // Timer/Counter1 Control Register C
  TCNT1 : word absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$4C; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$4C+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$4A; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AL : byte absolute $00+$4A; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AH : byte absolute $00+$4A+1; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1B : word absolute $00+$48; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BL : byte absolute $00+$48; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BH : byte absolute $00+$48+1; // Timer/Counter1 Output Compare Register B  Bytes
  ICR1 : word absolute $00+$44; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$44; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$44+1; // Timer/Counter1 Input Capture Register  Bytes
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  // CPU
  PRR : byte absolute $00+$20; // Power Reduction Register
  OSCCAL : byte absolute $00+$51; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$46; // Clock Prescale Register
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUSR : byte absolute $00+$54; // MCU Status Register
  GPIOR2 : byte absolute $00+$35; // General Purpose I/O Register 2
  GPIOR1 : byte absolute $00+$34; // General Purpose I/O Register 1
  GPIOR0 : byte absolute $00+$33; // General Purpose I/O Register 0

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
  // DIDR0
  ADC1D = 1; // ADC 1 Digital input buffer disable
  ADC0D = 0; // ADC 0 Digital input buffer disable
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADATE = 5; // ADC Auto Trigger Enable
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits
  // ADCSRB
  BIN = 7; // Bipolar Input Mode
  ADLAR = 4; // ADC Left Adjust Result
  ADTS = 0; // ADC Auto Trigger Source bits
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
  // MCUCR
  ISC01 = 1; // Interrupt Sense Control 0 Bit 1
  ISC00 = 0; // Interrupt Sense Control 0 Bit 0
  // GIMSK
  INT0 = 6; // External Interrupt Request 0 Enable
  PCIE = 4; // Pin Change Interrupt Enables
  // GIFR
  INTF0 = 6; // External Interrupt Flag 0
  PCIF = 4; // Pin Change Interrupt Flags
  // EECR
  EEPM = 4; // EEPROM Programming Mode Bits
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // WDTCSR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  // TIMSK0
  OCIE0B = 2; // Timer/Counter0 Output Compare Match B Interrupt Enable
  OCIE0A = 1; // Timer/Counter0 Output Compare Match A Interrupt Enable
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR0
  OCF0B = 2; // Timer/Counter0 Output Compare Flag B
  OCF0A = 1; // Timer/Counter0 Output Compare Flag A
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // TCCR0A
  COM0A = 6; // Compare Match Output A Mode bits
  COM0B = 4; // Compare Match Output B Mode bits
  WGM0 = 0; // Waveform Generation Mode bits
  // TCCR0B
  FOC0A = 7; // Force Output Compare A
  FOC0B = 6; // Force Output Compare B
  WGM02 = 3; // Waveform Generation Mode bit 2
  CS0 = 0; // Clock Select bits
  // GTCCR
  TSM = 7; // Timer/Counter Synchronization Mode
  PSR10 = 0; // Prescaler Reset Timer/CounterN
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
  CS1 = 0; // Clock Select1 bits
  // TCCR1C
  FOC1A = 7; // Force Output Compare for Channel A
  FOC1B = 6; // Force Output Compare for Channel B
  // SPMCSR
  CTPB = 4; // Clear temporary page buffer
  RFLB = 3; // Read fuse and lock bits
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable
  // PRR
  PRTIM1 = 3; // Power Reduction Timer/Counter1
  PRTIM0 = 2; // Power Reduction Timer/Counter0
  PRUSI = 1; // Power Reduction USI
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
  // MCUCR
  PUD = 6; // 
  SE = 5; // Sleep Enable
  SM = 3; // Sleep Mode Select Bits
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure EXT_INT0_ISR; external name 'EXT_INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 2 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 3 Pin Change Interrupt Request 1
procedure WATCHDOG_ISR; external name 'WATCHDOG_ISR'; // Interrupt 4 Watchdog Time-out
procedure TIM1_CAPT_ISR; external name 'TIM1_CAPT_ISR'; // Interrupt 5 Timer/Counter1 Capture Event
procedure TIM1_COMPA_ISR; external name 'TIM1_COMPA_ISR'; // Interrupt 6 Timer/Counter1 Compare Match A
procedure TIM1_COMPB_ISR; external name 'TIM1_COMPB_ISR'; // Interrupt 7 Timer/Counter1 Compare Match B
procedure TIM1_OVF_ISR; external name 'TIM1_OVF_ISR'; // Interrupt 8 Timer/Counter1 Overflow
procedure TIM0_COMPA_ISR; external name 'TIM0_COMPA_ISR'; // Interrupt 9 Timer/Counter0 Compare Match A
procedure TIM0_COMPB_ISR; external name 'TIM0_COMPB_ISR'; // Interrupt 10 Timer/Counter0 Compare Match B
procedure TIM0_OVF_ISR; external name 'TIM0_OVF_ISR'; // Interrupt 11 Timer/Counter0 Overflow
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 12 Analog Comparator
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 13 ADC Conversion Complete
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 14 EEPROM Ready
procedure USI_STR_ISR; external name 'USI_STR_ISR'; // Interrupt 15 USI START
procedure USI_OVF_ISR; external name 'USI_OVF_ISR'; // Interrupt 16 USI Overflow

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp EXT_INT0_ISR
   rjmp PCINT0_ISR
   rjmp PCINT1_ISR
   rjmp WATCHDOG_ISR
   rjmp TIM1_CAPT_ISR
   rjmp TIM1_COMPA_ISR
   rjmp TIM1_COMPB_ISR
   rjmp TIM1_OVF_ISR
   rjmp TIM0_COMPA_ISR
   rjmp TIM0_COMPB_ISR
   rjmp TIM0_OVF_ISR
   rjmp ANA_COMP_ISR
   rjmp ADC_ISR
   rjmp EE_RDY_ISR
   rjmp USI_STR_ISR
   rjmp USI_OVF_ISR

   {$i start.inc}

   .weak EXT_INT0_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak WATCHDOG_ISR
   .weak TIM1_CAPT_ISR
   .weak TIM1_COMPA_ISR
   .weak TIM1_COMPB_ISR
   .weak TIM1_OVF_ISR
   .weak TIM0_COMPA_ISR
   .weak TIM0_COMPB_ISR
   .weak TIM0_OVF_ISR
   .weak ANA_COMP_ISR
   .weak ADC_ISR
   .weak EE_RDY_ISR
   .weak USI_STR_ISR
   .weak USI_OVF_ISR

   .set EXT_INT0_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set WATCHDOG_ISR, Default_IRQ_handler
   .set TIM1_CAPT_ISR, Default_IRQ_handler
   .set TIM1_COMPA_ISR, Default_IRQ_handler
   .set TIM1_COMPB_ISR, Default_IRQ_handler
   .set TIM1_OVF_ISR, Default_IRQ_handler
   .set TIM0_COMPA_ISR, Default_IRQ_handler
   .set TIM0_COMPB_ISR, Default_IRQ_handler
   .set TIM0_OVF_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set USI_STR_ISR, Default_IRQ_handler
   .set USI_OVF_ISR, Default_IRQ_handler
 end;

end.
