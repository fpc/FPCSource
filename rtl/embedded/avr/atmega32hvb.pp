unit ATmega32HVB;

{$goto on}

interface

var
  // AD_CONVERTER
  VADMUX : byte absolute $00+$7C; // The VADC multiplexer Selection Register
  VADC : word absolute $00+$78; // VADC Data Register  Bytes
  VADCL : byte absolute $00+$78; // VADC Data Register  Bytes
  VADCH : byte absolute $00+$78+1; // VADC Data Register  Bytes
  VADCSR : byte absolute $00+$7A; // The VADC Control and Status register
  // WATCHDOG
  WDTCSR : byte absolute $00+$60; // Watchdog Timer Control Register
  // FET
  FCSR : byte absolute $00+$F0; // FET Control and Status Register
  // SPI
  SPCR : byte absolute $00+$4c; // SPI Control Register
  SPSR : byte absolute $00+$4d; // SPI Status Register
  SPDR : byte absolute $00+$4e; // SPI Data Register
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Read/Write Access
  EEARL : byte absolute $00+$41; // EEPROM Read/Write Access
  EEARH : byte absolute $00+$41+1; // EEPROM Read/Write Access
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // COULOMB_COUNTER
  CADCSRA : byte absolute $00+$E6; // CC-ADC Control and Status Register A
  CADCSRB : byte absolute $00+$E7; // CC-ADC Control and Status Register B
  CADCSRC : byte absolute $00+$E8; // CC-ADC Control and Status Register C
  CADIC : word absolute $00+$E4; // CC-ADC Instantaneous Current
  CADICL : byte absolute $00+$E4; // CC-ADC Instantaneous Current
  CADICH : byte absolute $00+$E4+1; // CC-ADC Instantaneous Current
  CADAC3 : byte absolute $00+$E3; // ADC Accumulate Current
  CADAC2 : byte absolute $00+$E2; // ADC Accumulate Current
  CADAC1 : byte absolute $00+$E1; // ADC Accumulate Current
  CADAC0 : byte absolute $00+$E0; // ADC Accumulate Current
  CADRCC : byte absolute $00+$E9; // CC-ADC Regular Charge Current
  CADRDC : byte absolute $00+$EA; // CC-ADC Regular Discharge Current
  // TWI
  TWBCSR : byte absolute $00+$BE; // TWI Bus Control and Status Register
  TWAMR : byte absolute $00+$BD; // TWI (Slave) Address Mask Register
  TWBR : byte absolute $00+$B8; // TWI Bit Rate register
  TWCR : byte absolute $00+$BC; // TWI Control Register
  TWSR : byte absolute $00+$B9; // TWI Status Register
  TWDR : byte absolute $00+$BB; // TWI Data register
  TWAR : byte absolute $00+$BA; // TWI (Slave) Address register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  PCICR : byte absolute $00+$68; // Pin Change Interrupt Control Register
  PCIFR : byte absolute $00+$3B; // Pin Change Interrupt Flag Register
  PCMSK1 : byte absolute $00+$6C; // Pin Change Enable Mask Register 1
  PCMSK0 : byte absolute $00+$6B; // Pin Change Enable Mask Register 0
  // TIMER_COUNTER_1
  TCCR1B : byte absolute $00+$81; // Timer/Counter1 Control Register B
  TCCR1A : byte absolute $00+$80; // Timer/Counter 1 Control Register A
  TCNT1 : word absolute $00+$84; // Timer Counter 1  Bytes
  TCNT1L : byte absolute $00+$84; // Timer Counter 1  Bytes
  TCNT1H : byte absolute $00+$84+1; // Timer Counter 1  Bytes
  OCR1A : byte absolute $00+$88; // Output Compare Register 1A
  OCR1B : byte absolute $00+$89; // Output Compare Register B
  TIMSK1 : byte absolute $00+$6F; // Timer/Counter Interrupt Mask Register
  TIFR1 : byte absolute $00+$36; // Timer/Counter Interrupt Flag register
  GTCCR : byte absolute $00+$43; // General Timer/Counter Control Register
  // CELL_BALANCING
  CBCR : byte absolute $00+$F1; // Cell Balancing Control Register
  // BATTERY_PROTECTION
  BPPLR : byte absolute $00+$FE; // Battery Protection Parameter Lock Register
  BPCR : byte absolute $00+$FD; // Battery Protection Control Register
  BPHCTR : byte absolute $00+$FC; // Battery Protection Short-current Timing Register
  BPOCTR : byte absolute $00+$FB; // Battery Protection Over-current Timing Register
  BPSCTR : byte absolute $00+$FA; // Battery Protection Short-current Timing Register
  BPCHCD : byte absolute $00+$F9; // Battery Protection Charge-High-current Detection Level Register
  BPDHCD : byte absolute $00+$F8; // Battery Protection Discharge-High-current Detection Level Register
  BPCOCD : byte absolute $00+$F7; // Battery Protection Charge-Over-current Detection Level Register
  BPDOCD : byte absolute $00+$F6; // Battery Protection Discharge-Over-current Detection Level Register
  BPSCD : byte absolute $00+$F5; // Battery Protection Short-Circuit Detection Level Register
  BPIFR : byte absolute $00+$F3; // Battery Protection Interrupt Flag Register
  BPIMSK : byte absolute $00+$F2; // Battery Protection Interrupt Mask Register
  // CHARGER_DETECT
  CHGDCSR : byte absolute $00+$D4; // Charger Detect Control and Status Register
  // VOLTAGE_REGULATOR
  ROCR : byte absolute $00+$C8; // Regulator Operating Condition Register
  // BANDGAP
  BGCSR : byte absolute $00+$D2; // Bandgap Control and Status Register
  BGCRR : byte absolute $00+$D1; // Bandgap Calibration of Resistor Ladder
  BGCCR : byte absolute $00+$D0; // Bandgap Calibration Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  FOSCCAL : byte absolute $00+$66; // Fast Oscillator Calibration Value
  OSICSR : byte absolute $00+$37; // Oscillator Sampling Interface Control and Status Register
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  GPIOR2 : byte absolute $00+$4B; // General Purpose IO Register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose IO Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose IO Register 0
  DIDR0 : byte absolute $00+$7E; // Digital Input Disable Register
  PRR0 : byte absolute $00+$64; // Power Reduction Register 0
  CLKPR : byte absolute $00+$61; // Clock Prescale Register
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
  PINC : byte absolute $00+$26; // Port C Input Pins
  // TIMER_COUNTER_0
  TCCR0B : byte absolute $00+$45; // Timer/Counter0 Control Register B
  TCCR0A : byte absolute $00+$44; // Timer/Counter 0 Control Register A
  TCNT0 : word absolute $00+$46; // Timer Counter 0  Bytes
  TCNT0L : byte absolute $00+$46; // Timer Counter 0  Bytes
  TCNT0H : byte absolute $00+$46+1; // Timer Counter 0  Bytes
  OCR0A : byte absolute $00+$48; // Output Compare Register 0A
  OCR0B : byte absolute $00+$49; // Output Compare Register B
  TIMSK0 : byte absolute $00+$6E; // Timer/Counter Interrupt Mask Register
  TIFR0 : byte absolute $00+$35; // Timer/Counter Interrupt Flag register
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control and Status Register

const
  // VADMUX
  // VADCSR
  VADEN = 3; // VADC Enable
  VADSC = 2; // VADC Satrt Conversion
  VADCCIF = 1; // VADC Conversion Complete Interrupt Flag
  VADCCIE = 0; // VADC Conversion Complete Interrupt Enable
  // WDTCSR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  // FCSR
  DUVRD = 3; // Deep Under-Voltage Recovery Disable
  CPS = 2; // Current Protection Status
  DFE = 1; // Discharge FET Enable
  CFE = 0; // Charge FET Enable
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
  // EECR
  EEPM = 4; // 
  EERIE = 3; // EEProm Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Write Enable
  EERE = 0; // EEPROM Read Enable
  // CADCSRA
  CADEN = 7; // When the CADEN bit is cleared (zero), the CC-ADC is disabled. When the CADEN bit is set (one), the CC-ADC will continuously measure the voltage drop over the external sense resistor RSENSE. In Power-down, only the Regular Current detection is active. In Power-off, the CC-ADC is always disabled.
  CADPOL = 6; // 
  CADUB = 5; // CC_ADC Update Busy
  CADAS = 3; // CC_ADC Accumulate Current Select Bits
  CADSI = 1; // The CADSI bits determine the current sampling interval for the Regular Current detection in Power-down mode. The actual settings remain to be determined.
  CADSE = 0; // When the CADSE bit is written to one, the ongoing CC-ADC conversion is aborted, and the CC-ADC enters Regular Current detection mode.
  // CADCSRB
  CADACIE = 6; // 
  CADRCIE = 5; // Regular Current Interrupt Enable
  CADICIE = 4; // CAD Instantenous Current Interrupt Enable
  CADACIF = 2; // CC-ADC Accumulate Current Interrupt Flag
  CADRCIF = 1; // CC-ADC Accumulate Current Interrupt Flag
  CADICIF = 0; // CC-ADC Instantaneous Current Interrupt Flag
  // CADCSRC
  CADVSE = 0; // CC-ADC Voltage Scaling Enable
  // TWBCSR
  TWBCIF = 7; // TWI Bus Connect/Disconnect Interrupt Flag
  TWBCIE = 6; // TWI Bus Connect/Disconnect Interrupt Enable
  TWBDT = 1; // TWI Bus Disconnect Time-out Period
  TWBCIP = 0; // TWI Bus Connect/Disconnect Interrupt Polarity
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
  // EICRA
  ISC3 = 6; // External Interrupt Sense Control 3 Bits
  ISC2 = 4; // External Interrupt Sense Control 2 Bits
  ISC1 = 2; // External Interrupt Sense Control 1 Bits
  ISC0 = 0; // External Interrupt Sense Control 0 Bits
  // EIMSK
  INT = 0; // External Interrupt Request 3 Enable
  // EIFR
  INTF = 0; // External Interrupt Flags
  // PCICR
  PCIE = 0; // Pin Change Interrupt Enables
  // PCIFR
  PCIF = 0; // Pin Change Interrupt Flags
  // TCCR1B
  CS = 0; // Clock Select1 bis
  // TCCR1A
  TCW1 = 7; // Timer/Counter Width
  ICEN1 = 6; // Input Capture Mode Enable
  ICNC1 = 5; // Input Capture Noise Canceler
  ICES1 = 4; // Input Capture Edge Select
  ICS1 = 3; // Input Capture Select
  WGM10 = 0; // Waveform Generation Mode
  // TIMSK1
  ICIE1 = 3; // Timer/Counter n Input Capture Interrupt Enable
  OCIE1B = 2; // Timer/Counter1 Output Compare B Interrupt Enable
  OCIE1A = 1; // Timer/Counter1 Output Compare A Interrupt Enable
  TOIE1 = 0; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR1
  ICF1 = 3; // Timer/Counter 1 Input Capture Flag
  OCF1B = 2; // Timer/Counter1 Output Compare Flag B
  OCF1A = 1; // Timer/Counter1 Output Compare Flag A
  TOV1 = 0; // Timer/Counter1 Overflow Flag
  // GTCCR
  TSM = 7; // Timer/Counter Synchronization Mode
  PSRSYNC = 0; // Prescaler Reset
  // CBCR
  CBE = 0; // Cell Balancing Enables
  // BPPLR
  BPPLE = 1; // Battery Protection Parameter Lock Enable
  BPPL = 0; // Battery Protection Parameter Lock
  // BPCR
  EPID = 5; // External Protection Input Disable
  SCD = 4; // Short Circuit Protection Disabled
  DOCD = 3; // Discharge Over-current Protection Disabled
  COCD = 2; // Charge Over-current Protection Disabled
  DHCD = 1; // Discharge High-current Protection Disable
  CHCD = 0; // Charge High-current Protection Disable
  // BPIFR
  SCIF = 4; // Short-circuit Protection Activated Interrupt Flag
  DOCIF = 3; // Discharge Over-current Protection Activated Interrupt Flag
  COCIF = 2; // Charge Over-current Protection Activated Interrupt Flag
  DHCIF = 1; // Disharge High-current Protection Activated Interrupt
  CHCIF = 0; // Charge High-current Protection Activated Interrupt
  // BPIMSK
  SCIE = 4; // Short-circuit Protection Activated Interrupt Enable
  DOCIE = 3; // Discharge Over-current Protection Activated Interrupt Enable
  COCIE = 2; // Charge Over-current Protection Activated Interrupt Enable
  DHCIE = 1; // Discharger High-current Protection Activated Interrupt
  CHCIE = 0; // Charger High-current Protection Activated Interrupt
  // CHGDCSR
  BATTPVL = 4; // BATT Pin Voltage Level
  CHGDISC = 2; // Charger Detect Interrupt Sense Control
  CHGDIF = 1; // Charger Detect Interrupt Flag
  CHGDIE = 0; // Charger Detect Interrupt Enable
  // ROCR
  ROCS = 7; // ROC Status
  ROCD = 4; // ROC Disable
  ROCWIF = 1; // ROC Warning Interrupt Flag
  ROCWIE = 0; // ROC Warning Interrupt Enable
  // BGCSR
  BGD = 5; // Bandgap Disable
  BGSCDE = 4; // Bandgap Short Circuit Detection Enabled
  BGSCDIF = 1; // Bandgap Short Circuit Detection Interrupt Flag
  BGSCDIE = 0; // Bandgap Short Circuit Detection Interrupt Enable
  // BGCCR
  BGCC = 0; // BG Calibration of PTAT Current Bits
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
  CKOE = 5; // Clock Output Enable
  PUD = 4; // Pull-up disable
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // MCUSR
  OCDRF = 4; // OCD Reset Flag
  WDRF = 3; // Watchdog Reset Flag
  BODRF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on reset flag
  // OSICSR
  OSISEL0 = 4; // Oscillator Sampling Interface Select 0
  OSIST = 1; // Oscillator Sampling Interface Status
  OSIEN = 0; // Oscillator Sampling Interface Enable
  // SMCR
  SM = 1; // Sleep Mode Select bits
  SE = 0; // Sleep Enable
  // DIDR0
  PA1DID = 1; // When this bit is written logic one, the digital input buffer of the corresponding V_ADC pin is disabled.
  PA0DID = 0; // When this bit is written logic one, the digital input buffer of the corresponding V_ADC pin is disabled.
  // PRR0
  PRTWI = 6; // Power Reduction TWI
  PRVRM = 5; // Power Reduction Voltage Regulator Monitor
  PRSPI = 3; // Power reduction SPI
  PRTIM1 = 2; // Power Reduction Timer/Counter1
  PRTIM0 = 1; // Power Reduction Timer/Counter0
  PRVADC = 0; // Power Reduction V-ADC
  // CLKPR
  CLKPCE = 7; // Clock Prescaler Change Enable
  CLKPS = 0; // Clock Prescaler Select Bits
  // TCCR0B
  CS02 = 2; // Clock Select0 bit 2
  CS01 = 1; // Clock Select0 bit 1
  CS00 = 0; // Clock Select0 bit 0
  // TCCR0A
  TCW0 = 7; // Timer/Counter Width
  ICEN0 = 6; // Input Capture Mode Enable
  ICNC0 = 5; // Input Capture Noise Canceler
  ICES0 = 4; // Input Capture Edge Select
  ICS0 = 3; // Input Capture Select
  WGM00 = 0; // Waveform Generation Mode
  // TIMSK0
  ICIE0 = 3; // Timer/Counter n Input Capture Interrupt Enable
  OCIE0B = 2; // Timer/Counter0 Output Compare B Interrupt Enable
  OCIE0A = 1; // Timer/Counter0 Output Compare A Interrupt Enable
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR0
  ICF0 = 3; // Timer/Counter 0 Input Capture Flag
  OCF0B = 2; // Timer/Counter0 Output Compare Flag B
  OCF0A = 1; // Timer/Counter0 Output Compare Flag A
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // GTCCR
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read-While-Write Section Busy
  SIGRD = 5; // Signature Row Read
  RWWSRE = 4; // Read-While-Write Section Read Enable
  LBSET = 3; // Lock Bit Set
  PGWRT = 2; // Page Write
  PGERS = 1; // Page Erase
  SPMEN = 0; // Store Program Memory Enable

implementation

{$i avrcommon.inc}

procedure BPINT_ISR; external name 'BPINT_ISR'; // Interrupt 1 Battery Protection Interrupt
procedure VREGMON_ISR; external name 'VREGMON_ISR'; // Interrupt 2 Voltage regulator monitor interrupt
procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 3 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 4 External Interrupt Request 1
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 5 External Interrupt Request 2
procedure INT3_ISR; external name 'INT3_ISR'; // Interrupt 6 External Interrupt Request 3
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 7 Pin Change Interrupt 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 8 Pin Change Interrupt 1
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 9 Watchdog Timeout Interrupt
procedure BGSCD_ISR; external name 'BGSCD_ISR'; // Interrupt 10 Bandgap Buffer Short Circuit Detected
procedure CHDET_ISR; external name 'CHDET_ISR'; // Interrupt 11 Charger Detect
procedure TIMER1_IC_ISR; external name 'TIMER1_IC_ISR'; // Interrupt 12 Timer 1 Input capture
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 13 Timer 1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 14 Timer 1 Compare Match B
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 15 Timer 1 overflow
procedure TIMER0_IC_ISR; external name 'TIMER0_IC_ISR'; // Interrupt 16 Timer 0 Input Capture
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 17 Timer 0 Comapre Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 18 Timer 0 Compare Match B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 19 Timer 0 Overflow
procedure TWIBUSCD_ISR; external name 'TWIBUSCD_ISR'; // Interrupt 20 Two-Wire Bus Connect/Disconnect
procedure TWI_ISR; external name 'TWI_ISR'; // Interrupt 21 Two-Wire Serial Interface
procedure SPI_STC_ISR; external name 'SPI_STC_ISR'; // Interrupt 22 SPI Serial transfer complete
procedure VADC_ISR; external name 'VADC_ISR'; // Interrupt 23 Voltage ADC Conversion Complete
procedure CCADC_CONV_ISR; external name 'CCADC_CONV_ISR'; // Interrupt 24 Coulomb Counter ADC Conversion Complete
procedure CCADC_REG_CUR_ISR; external name 'CCADC_REG_CUR_ISR'; // Interrupt 25 Coloumb Counter ADC Regular Current
procedure CCADC_ACC_ISR; external name 'CCADC_ACC_ISR'; // Interrupt 26 Coloumb Counter ADC Accumulator
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 27 EEPROM Ready
procedure SPM_ISR; external name 'SPM_ISR'; // Interrupt 28 SPM Ready

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   jmp _start
   jmp BPINT_ISR
   jmp VREGMON_ISR
   jmp INT0_ISR
   jmp INT1_ISR
   jmp INT2_ISR
   jmp INT3_ISR
   jmp PCINT0_ISR
   jmp PCINT1_ISR
   jmp WDT_ISR
   jmp BGSCD_ISR
   jmp CHDET_ISR
   jmp TIMER1_IC_ISR
   jmp TIMER1_COMPA_ISR
   jmp TIMER1_COMPB_ISR
   jmp TIMER1_OVF_ISR
   jmp TIMER0_IC_ISR
   jmp TIMER0_COMPA_ISR
   jmp TIMER0_COMPB_ISR
   jmp TIMER0_OVF_ISR
   jmp TWIBUSCD_ISR
   jmp TWI_ISR
   jmp SPI_STC_ISR
   jmp VADC_ISR
   jmp CCADC_CONV_ISR
   jmp CCADC_REG_CUR_ISR
   jmp CCADC_ACC_ISR
   jmp EE_READY_ISR
   jmp SPM_ISR

   {$i start.inc}

   .weak BPINT_ISR
   .weak VREGMON_ISR
   .weak INT0_ISR
   .weak INT1_ISR
   .weak INT2_ISR
   .weak INT3_ISR
   .weak PCINT0_ISR
   .weak PCINT1_ISR
   .weak WDT_ISR
   .weak BGSCD_ISR
   .weak CHDET_ISR
   .weak TIMER1_IC_ISR
   .weak TIMER1_COMPA_ISR
   .weak TIMER1_COMPB_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_IC_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak TIMER0_OVF_ISR
   .weak TWIBUSCD_ISR
   .weak TWI_ISR
   .weak SPI_STC_ISR
   .weak VADC_ISR
   .weak CCADC_CONV_ISR
   .weak CCADC_REG_CUR_ISR
   .weak CCADC_ACC_ISR
   .weak EE_READY_ISR
   .weak SPM_ISR

   .set BPINT_ISR, Default_IRQ_handler
   .set VREGMON_ISR, Default_IRQ_handler
   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set INT3_ISR, Default_IRQ_handler
   .set PCINT0_ISR, Default_IRQ_handler
   .set PCINT1_ISR, Default_IRQ_handler
   .set WDT_ISR, Default_IRQ_handler
   .set BGSCD_ISR, Default_IRQ_handler
   .set CHDET_ISR, Default_IRQ_handler
   .set TIMER1_IC_ISR, Default_IRQ_handler
   .set TIMER1_COMPA_ISR, Default_IRQ_handler
   .set TIMER1_COMPB_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_IC_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set TWIBUSCD_ISR, Default_IRQ_handler
   .set TWI_ISR, Default_IRQ_handler
   .set SPI_STC_ISR, Default_IRQ_handler
   .set VADC_ISR, Default_IRQ_handler
   .set CCADC_CONV_ISR, Default_IRQ_handler
   .set CCADC_REG_CUR_ISR, Default_IRQ_handler
   .set CCADC_ACC_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set SPM_ISR, Default_IRQ_handler
 end;

end.
