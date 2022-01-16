unit ATtiny40;

{$goto on}

interface

var
  // WATCHDOG
  WDTCSR : byte absolute $00+$31; // Watchdog Timer Control and Status Register
  // AD_CONVERTER
  ADMUX : byte absolute $00+$10; // The ADC multiplexer Selection Register
  ADCSRA : byte absolute $00+$12; // The ADC Control and Status register
  ADC : word absolute $00+$0E; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$0E; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$0E+1; // ADC Data Register  Bytes
  ADCSRB : byte absolute $00+$11; // ADC Control and Status Register B
  DIDR0 : byte absolute $00+$0D; // Digital Input Disable Register 0
  // ANALOG_COMPARATOR
  ACSRB : byte absolute $00+$13; // Analog Comparator Control And Status Register B
  ACSRA : byte absolute $00+$14; // Analog Comparator Control And Status Register A
  // TWI
  TWSCRA : byte absolute $00+$2D; // TWI Slave Control Register A
  TWSCRB : byte absolute $00+$2C; // TWI Slave Control Register B
  TWSSRA : byte absolute $00+$2B; // TWI Slave Status Register A
  TWSA : byte absolute $00+$2A; // TWI Slave Address Register
  TWSD : byte absolute $00+$28; // TWI Slave Data Register
  TWSAM : byte absolute $00+$29; // TWI Slave Address Mask Register
  // CPU
  CCP : byte absolute $00+$3C; // Configuration Change Protection
  SP : word absolute $00+$3D; // Stack Pointer 
  SPL : byte absolute $00+$3D; // Stack Pointer 
  SPH : byte absolute $00+$3D+1; // Stack Pointer 
  SREG : byte absolute $00+$3F; // Status Register
  CLKMSR : byte absolute $00+$37; // Clock Main Settings Register
  CLKPSR : byte absolute $00+$36; // Clock Prescale Register
  OSCCAL : byte absolute $00+$39; // Oscillator Calibration Value
  PRR : byte absolute $00+$35; // Power Reduction Register
  RSTFLR : byte absolute $00+$3B; // Reset Flag Register
  NVMCSR : byte absolute $00+$32; // Non-Volatile Memory Control and Status Register
  NVMCMD : byte absolute $00+$33; // Non-Volatile Memory Command
  MCUCR : byte absolute $00+$3A; // MCU Control Register
  GIMSK : byte absolute $00+$0C; // General Interrupt Mask Register
  GIFR : byte absolute $00+$0B; // General Interrupt Flag Register
  RAMAR : byte absolute $00+$20; // RAM Address Register
  RAMDR : byte absolute $00+$1F; // RAM Data Register
  // EXTERNAL_INTERRUPT
  PCMSK2 : byte absolute $00+$1A; // Pin Change Mask Register 2
  PCMSK1 : byte absolute $00+$0A; // Pin Change Mask Register 1
  PCMSK0 : byte absolute $00+$09; // Pin Change Mask Register 0
  // PORTB
  PORTCR : byte absolute $00+$08; // Port Control Register
  PUEB : byte absolute $00+$07; // Pull-up Enable Control Register
  DDRB : byte absolute $00+$05; // Data Direction Register, Port B
  PINB : byte absolute $00+$04; // Port B Data register
  PORTB : byte absolute $00+$06; // Input Pins, Port B
  // PORTC
  PUEC : byte absolute $00+$1E; // Pull-up Enable Control Register
  PORTC : byte absolute $00+$1D; // Port C Data Register
  DDRC : byte absolute $00+$1C; // Data Direction Register, Port C
  PINC : byte absolute $00+$1B; // Port C Input Pins
  // TIMER_COUNTER_0
  TCCR0A : byte absolute $00+$19; // Timer/Counter 0 Control Register A
  TCCR0B : byte absolute $00+$18; // Timer/Counter 0 Control Register B
  TCCR1A : byte absolute $00+$24; // Timer/Counter1 Control Register A
  TCNT1H : byte absolute $00+$27; // Timer/Counter1 High
  TCNT1L : byte absolute $00+$23; // Timer/Counter1 Low
  OCR1A : byte absolute $00+$22; // Timer/Counter 1 Output Compare Register A
  OCR1B : byte absolute $00+$21; // Timer/Counter 1 Output Compare Register B
  TIMSK : byte absolute $00+$26; // Timer Interrupt Mask Register
  TIFR : byte absolute $00+$25; // Overflow Interrupt Enable
  TCNT0 : byte absolute $00+$17; // Timer/Counter0
  OCR0A : byte absolute $00+$16; // Timer/Counter0 Output Compare Register
  OCR0B : byte absolute $00+$15; // Timer/Counter0 Output Compare Register
  // PORTA
  PUEA : byte absolute $00+$03; // Pull-up Enable Control Register
  PORTA : byte absolute $00+$02; // Port A Data Register
  DDRA : byte absolute $00+$01; // Data Direction Register, Port A
  PINA : byte absolute $00+$00; // Port A Input Pins
  // SPI
  SPCR : byte absolute $00+$30; // SPI Control Register
  SPSR : byte absolute $00+$2F; // SPI Status Register
  SPDR : byte absolute $00+$2E; // SPI Data Register

const
  // WDTCSR
  WDIF = 7; // Watchdog Timer Interrupt Flag
  WDIE = 6; // Watchdog Timer Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDE = 3; // Watch Dog Enable
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
  // DIDR0
  ADC7D = 7; // ADC6 Digital input Disable
  ADC6D = 6; // ADC5 Digital input Disable
  ADC5D = 5; // ADC4 Digital input Disable
  ADC4D = 4; // ADC3 Digital input Disable
  ADC3D = 3; // AREF Digital Input Disable
  ADC2D = 2; // ADC2 Digital input Disable
  ADC1D = 1; // ADC1 Digital input Disable
  ADC0D = 0; // ADC0 Digital input Disable
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
  // SREG
  I = 7; // Global Interrupt Enable
  T = 6; // Bit Copy Storage
  H = 5; // Half Carry Flag
  S = 4; // Sign Bit
  V = 3; // Two's Complement Overflow Flag
  N = 2; // Negative Flag
  Z = 1; // Zero Flag
  C = 0; // Carry Flag
  // CLKMSR
  CLKMS = 0; // Clock Main Select Bits
  // CLKPSR
  CLKPS = 0; // Clock Prescaler Select Bits
  // PRR
  PRTWI = 4; // Power Reduction TWI
  PRSPI = 3; // Power Reduction Serial Peripheral Interface
  PRTIM1 = 2; // Power Reduction Timer/Counter1
  PRTIM0 = 1; // Power Reduction Timer/Counter0
  PRADC = 0; // Power Reduction ADC
  // RSTFLR
  WDRF = 3; // Watchdog Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on Reset Flag
  // NVMCSR
  NVMBSY = 7; // Non-Volatile Memory Busy
  // PCMSK2
  PCINT = 0; // Pin Change Enable Mask 3
  // PCMSK1
  // PCMSK0
  // PORTCR
  ADC11D = 7; // 
  ADC10D = 6; // 
  ADC9D = 5; // 
  ADC8D = 4; // 
  BBMC = 2; // Break-Before-Make Mode Enable
  BBMB = 1; // Break-Before-Make Mode Enable
  BBMA = 0; // Break-Before-Make Mode Enable
  // PORTCR
  // TCCR0A
  COM0A = 6; // Compare Output Mode for Channel A bits
  COM0B = 4; // Compare Output Mode for Channel B bits
  WGM0 = 0; // Waveform Generation Mode
  // TCCR0B
  FOC0A = 7; // Force Output Compare A
  FOC0B = 6; // Force Output Compare B
  TSM = 5; // Timer/Counter Synchronization Mode
  PSR = 4; // Prescaler Reset Timer/Counter
  WGM02 = 3; // Waveform Generation Mode
  CS0 = 0; // Clock Select
  // TCCR1A
  TCW1 = 7; // Timer/Counter1 Width
  ICEN1 = 6; // Input Capture Mode Enable
  ICNC1 = 5; // : Input Capture Noise Canceler
  ICES1 = 4; // Input Capture Edge Select
  CTC1 = 3; // Waveform Generation Mode
  CS1 = 0; // The Clock Select1 bits 2, 1, and 0 define the prescaling source of Timer1.
  // TIMSK
  ICIE1 = 7; // Input Capture Interrupt Enable
  OCIE1B = 5; // Output Compare B Match Interrupt Enable
  OCIE1A = 4; // Output Compare A Match Interrupt Enable
  TOIE = 0; // Overflow Interrupt Enable
  OCIE0B = 2; // Timer/Counter Output Compare Match B Interrupt Enable
  OCIE0A = 1; // Timer/Counter0 Output Compare Match A Interrupt Enable
  // TIFR
  ICF1 = 7; // Input Capture Flag
  OCF1B = 5; // Timer Output Compare Flag 1B
  OCF1A = 4; // Timer Output Compare Flag 1A
  TOV = 0; // Timer Overflow Flag
  OCF0B = 2; // Output Compare Flag 0 B
  OCF0A = 1; // Output Compare Flag 0 A
  // PORTCR
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

implementation

{$define RELBRANCHES}

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 2 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 3 Pin Change Interrupt Request 1
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 4 Watchdog Time-out
procedure TIM1_CAPT_ISR; external name 'TIM1_CAPT_ISR'; // Interrupt 5 Timer/Counter1 Input Capture
procedure TIM1_COMPA_ISR; external name 'TIM1_COMPA_ISR'; // Interrupt 6  Timer/Counter1 Compare Match A
procedure TIM1_COMPB_ISR; external name 'TIM1_COMPB_ISR'; // Interrupt 7  Timer/Counter1 Compare Match B
procedure TIM1_OVF_ISR; external name 'TIM1_OVF_ISR'; // Interrupt 8 Timer/Counter1 Overflow
procedure TIM0_COMPA_ISR; external name 'TIM0_COMPA_ISR'; // Interrupt 9 Timer/Counter0 Compare Match A
procedure TIM0_COMPB_ISR; external name 'TIM0_COMPB_ISR'; // Interrupt 10 Timer/Counter0 Compare Match B
procedure TIM0_OVF_ISR; external name 'TIM0_OVF_ISR'; // Interrupt 11 Timer/Counter0 Overflow
procedure ANA_COMP_ISR; external name 'ANA_COMP_ISR'; // Interrupt 12 Analog Comparator
procedure ADC_ADC_ISR; external name 'ADC_ADC_ISR'; // Interrupt 13 Conversion Complete
procedure TWI_SLAVE_ISR; external name 'TWI_SLAVE_ISR'; // Interrupt 14 Two-Wire Interface
procedure SPI_ISR; external name 'SPI_ISR'; // Interrupt 15 Serial Peripheral Interface
procedure QTRIP_ISR; external name 'QTRIP_ISR'; // Interrupt 16 Touch Sensing

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
   rjmp WDT_ISR
   rjmp TIM1_CAPT_ISR
   rjmp TIM1_COMPA_ISR
   rjmp TIM1_COMPB_ISR
   rjmp TIM1_OVF_ISR
   rjmp TIM0_COMPA_ISR
   rjmp TIM0_COMPB_ISR
   rjmp TIM0_OVF_ISR
   rjmp ANA_COMP_ISR
   rjmp ADC_ADC_ISR
   rjmp TWI_SLAVE_ISR
   rjmp SPI_ISR
   rjmp QTRIP_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak WDT_ISR
   .weak TIM1_CAPT_ISR
   .weak TIM1_COMPA_ISR
   .weak TIM1_COMPB_ISR
   .weak TIM1_OVF_ISR
   .weak TIM0_COMPA_ISR
   .weak TIM0_COMPB_ISR
   .weak TIM0_OVF_ISR
   .weak ANA_COMP_ISR
   .weak ADC_ADC_ISR
   .weak TWI_SLAVE_ISR
   .weak SPI_ISR
   .weak QTRIP_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set TIM1_CAPT_ISR, Default_IRQ_handler
   .set TIM1_COMPA_ISR, Default_IRQ_handler
   .set TIM1_COMPB_ISR, Default_IRQ_handler
   .set TIM1_OVF_ISR, Default_IRQ_handler
   .set TIM0_COMPA_ISR, Default_IRQ_handler
   .set TIM0_COMPB_ISR, Default_IRQ_handler
   .set TIM0_OVF_ISR, Default_IRQ_handler
   .set ANA_COMP_ISR, Default_IRQ_handler
   .set ADC_ADC_ISR, Default_IRQ_handler
   .set TWI_SLAVE_ISR, Default_IRQ_handler
   .set SPI_ISR, Default_IRQ_handler
   .set QTRIP_ISR, Default_IRQ_handler
 end;

end.
