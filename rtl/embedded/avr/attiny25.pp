unit ATtiny25;

{$goto on}

interface

var
  // PORTB
  PORTB : byte absolute $00+$38; // Data Register, Port B
  DDRB : byte absolute $00+$37; // Data Direction Register, Port B
  PINB : byte absolute $00+$36; // Input Pins, Port B
  // ANALOG_COMPARATOR
  ADCSRB : byte absolute $00+$23; // ADC Control and Status Register B
  ACSR : byte absolute $00+$28; // Analog Comparator Control And Status Register
  DIDR0 : byte absolute $00+$34; // 
  // AD_CONVERTER
  ADMUX : byte absolute $00+$27; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$26; // The ADC Control and Status register
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
  PCMSK : byte absolute $00+$35; // Pin Change Enable Mask
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
  TCCR0A : byte absolute $00+$4A; // Timer/Counter  Control Register A
  TCCR0B : byte absolute $00+$53; // Timer/Counter Control Register B
  TCNT0 : byte absolute $00+$52; // Timer/Counter0
  OCR0A : byte absolute $00+$49; // Timer/Counter0 Output Compare Register
  OCR0B : byte absolute $00+$48; // Timer/Counter0 Output Compare Register
  GTCCR : byte absolute $00+$4C; // General Timer/Counter Control Register
  // TIMER_COUNTER_1
  TCCR1 : byte absolute $00+$50; // Timer/Counter Control Register
  TCNT1 : byte absolute $00+$4F; // Timer/Counter Register
  OCR1A : byte absolute $00+$4E; // Output Compare Register
  OCR1B : byte absolute $00+$4B; // Output Compare Register
  OCR1C : byte absolute $00+$4D; // Output compare register
  DTPS : byte absolute $00+$43; // Dead time prescaler register
  DT1A : byte absolute $00+$45; // Dead time value register
  DT1B : byte absolute $00+$44; // Dead time value B
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  PRR : byte absolute $00+$40; // Power Reduction Register
  SPL : byte absolute $00+$5D; // Stack Pointer Low Byte
  MCUSR : byte absolute $00+$54; // MCU Status register
  OSCCAL : byte absolute $00+$51; // Oscillator Calibration Register
  CLKPR : byte absolute $00+$46; // Clock Prescale Register
  PLLCSR : byte absolute $00+$47; // PLL Control and status register
  DWDR : byte absolute $00+$42; // debugWire data register
  GPIOR2 : byte absolute $00+$33; // General Purpose IO register 2
  GPIOR1 : byte absolute $00+$32; // General Purpose register 1
  GPIOR0 : byte absolute $00+$31; // General purpose register 0

const
  // ADCSRB
  ACME = 6; // Analog Comparator Multiplexer Enable
  // ACSR
  ACD = 7; // Analog Comparator Disable
  ACBG = 6; // Analog Comparator Bandgap Select
  ACO = 5; // Analog Compare Output
  ACI = 4; // Analog Comparator Interrupt Flag
  ACIE = 3; // Analog Comparator Interrupt Enable
  ACIS = 0; // Analog Comparator Interrupt Mode Select bits
  // DIDR0
  AIN1D = 1; // AIN1 Digital Input Disable
  AIN0D = 0; // AIN0 Digital Input Disable
  // ADMUX
  REFS = 6; // Reference Selection Bits
  ADLAR = 5; // Left Adjust Result
  REFS2 = 4; // Reference Selection Bit 2
  MUX = 0; // Analog Channel and Gain Selection Bits
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADATE = 5; // ADC Auto Trigger Enable
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits
  // ADCSRB
  BIN = 7; // Bipolar Input Mode
  IPR = 5; // Input Polarity Mode
  ADTS = 0; // ADC Auto Trigger Sources
  // DIDR0
  ADC0D = 5; // ADC0 Digital input Disable
  ADC2D = 4; // ADC2 Digital input Disable
  ADC3D = 3; // ADC3 Digital input Disable
  ADC1D = 2; // ADC1 Digital input Disable
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
  PCIE = 5; // Pin Change Interrupt Enable
  // GIFR
  INTF0 = 6; // External Interrupt Flag 0
  PCIF = 5; // Pin Change Interrupt Flag
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
  // TIFR
  OCF0A = 4; // Timer/Counter0 Output Compare Flag 0A
  OCF0B = 3; // Timer/Counter0 Output Compare Flag 0B
  TOV0 = 1; // Timer/Counter0 Overflow Flag
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
  PSR0 = 0; // Prescaler Reset Timer/Counter1 and Timer/Counter0
  // TCCR1
  CTC1 = 7; // Clear Timer/Counter on Compare Match
  PWM1A = 6; // Pulse Width Modulator Enable
  COM1A = 4; // Compare Output Mode, Bits
  CS1 = 0; // Clock Select Bits
  // TIMSK
  OCIE1A = 6; // OCIE1A: Timer/Counter1 Output Compare Interrupt Enable
  OCIE1B = 5; // OCIE1A: Timer/Counter1 Output Compare B Interrupt Enable
  TOIE1 = 2; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR
  OCF1A = 6; // Timer/Counter1 Output Compare Flag 1A
  OCF1B = 5; // Timer/Counter1 Output Compare Flag 1B
  TOV1 = 2; // Timer/Counter1 Overflow Flag
  // GTCCR
  PWM1B = 6; // Pulse Width Modulator B Enable
  COM1B = 4; // Comparator B Output Mode
  FOC1B = 3; // Force Output Compare Match 1B
  FOC1A = 2; // Force Output Compare 1A
  PSR1 = 1; // Prescaler Reset Timer/Counter1
  // DTPS
  // DT1A
  DTVH = 4; // 
  DTVL = 0; // 
  // DT1B
  // SPMCSR
  CTPB = 4; // Clear temporary page buffer
  RFLB = 3; // Read fuse and lock bits
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
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 2 Pin change Interrupt Request 0
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 3 Timer/Counter1 Compare Match 1A
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 4 Timer/Counter1 Overflow
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 5 Timer/Counter0 Overflow
procedure EE_RDY_ISR; external name 'EE_RDY_ISR'; // Interrupt 6 EEPROM Ready
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 7 Analog comparator
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 8 ADC Conversion ready
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 9 Timer/Counter1 Compare Match B
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 10 Timer/Counter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 11 Timer/Counter0 Compare Match B
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 12 Watchdog Time-out
procedure USI_START_ISR; external name 'USI_START_ISR'; // Interrupt 13 USI START
procedure USI_OVF_ISR; external name 'USI_OVF_ISR'; // Interrupt 14 USI Overflow

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   rjmp _start
   rjmp INT0_ISR
   rjmp PCINT0_ISR
   rjmp TIMER1_COMPA_ISR
   rjmp TIMER1_OVF_ISR
   rjmp TIMER0_OVF_ISR
   rjmp EE_RDY_ISR
   rjmp ANA_COMP_ISR
   rjmp ADC_ISR
   rjmp TIMER1_COMPB_ISR
   rjmp TIMER0_COMPA_ISR
   rjmp TIMER0_COMPB_ISR
   rjmp WDT_ISR
   rjmp USI_START_ISR
   rjmp USI_OVF_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak PCINT0_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_OVF_ISR
   .weak EE_RDY_ISR
   .weak ANA_COMP_ISR
   .weak ADC_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak WDT_ISR
   .weak USI_START_ISR
   .weak USI_OVF_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set EE_RDY_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set USI_START_ISR, Default_IRQ_handler
   .set USI_OVF_ISR, Default_IRQ_handler
 end;

end.
