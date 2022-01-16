unit ATmega128RFA1;

{$goto on}

interface

var
  // ANALOG_COMPARATOR
  ADCSRB : byte absolute $00+$7B; // ADC Control and Status Register B
  ACSR : byte absolute $00+$50; // Analog Comparator Control And Status Register
  DIDR1 : byte absolute $00+$7F; // Digital Input Disable Register 1
  // USART0
  UDR0 : byte absolute $00+$C6; // USART0 I/O Data Register
  UCSR0A : byte absolute $00+$C0; // USART0 Control and Status Register A
  UCSR0B : byte absolute $00+$C1; // USART0 Control and Status Register B
  UCSR0C : byte absolute $00+$C2; // USART0 Control and Status Register C
  UBRR0 : word absolute $00+$C4; // USART0 Baud Rate Register  Bytes
  UBRR0L : byte absolute $00+$C4; // USART0 Baud Rate Register  Bytes
  UBRR0H : byte absolute $00+$C4+1; // USART0 Baud Rate Register  Bytes
  // USART1
  UDR1 : byte absolute $00+$CE; // USART1 I/O Data Register
  UCSR1A : byte absolute $00+$C8; // USART1 Control and Status Register A
  UCSR1B : byte absolute $00+$C9; // USART1 Control and Status Register B
  UCSR1C : byte absolute $00+$CA; // USART1 Control and Status Register C
  UBRR1 : word absolute $00+$CC; // USART1 Baud Rate Register  Bytes
  UBRR1L : byte absolute $00+$CC; // USART1 Baud Rate Register  Bytes
  UBRR1H : byte absolute $00+$CC+1; // USART1 Baud Rate Register  Bytes
  // TWI
  TWAMR : byte absolute $00+$BD; // TWI (Slave) Address Mask Register
  TWBR : byte absolute $00+$B8; // TWI Bit Rate Register
  TWCR : byte absolute $00+$BC; // TWI Control Register
  TWSR : byte absolute $00+$B9; // TWI Status Register
  TWDR : byte absolute $00+$BB; // TWI Data Register
  TWAR : byte absolute $00+$BA; // TWI (Slave) Address Register
  // SPI
  SPCR : byte absolute $00+$4C; // SPI Control Register
  SPSR : byte absolute $00+$4D; // SPI Status Register
  SPDR : byte absolute $00+$4E; // SPI Data Register
  // PORTA
  PORTA : byte absolute $00+$22; // Port A Data Register
  DDRA : byte absolute $00+$21; // Port A Data Direction Register
  PINA : byte absolute $00+$20; // Port A Input Pins Address
  // PORTB
  PORTB : byte absolute $00+$25; // Port B Data Register
  DDRB : byte absolute $00+$24; // Port B Data Direction Register
  PINB : byte absolute $00+$23; // Port B Input Pins Address
  // PORTC
  PORTC : byte absolute $00+$28; // Port C Data Register
  DDRC : byte absolute $00+$27; // Port C Data Direction Register
  PINC : byte absolute $00+$26; // Port C Input Pins Address
  // PORTD
  PORTD : byte absolute $00+$2B; // Port D Data Register
  DDRD : byte absolute $00+$2A; // Port D Data Direction Register
  PIND : byte absolute $00+$29; // Port D Input Pins Address
  // PORTE
  PORTE : byte absolute $00+$2E; // Port E Data Register
  DDRE : byte absolute $00+$2D; // Port E Data Direction Register
  PINE : byte absolute $00+$2C; // Port E Input Pins Address
  // PORTF
  PORTF : byte absolute $00+$31; // Port F Data Register
  DDRF : byte absolute $00+$30; // Port F Data Direction Register
  PINF : byte absolute $00+$2F; // Port F Input Pins Address
  // PORTG
  PORTG : byte absolute $00+$34; // Port G Data Register
  DDRG : byte absolute $00+$33; // Port G Data Direction Register
  PING : byte absolute $00+$32; // Port G Input Pins Address
  // TIMER_COUNTER_0
  OCR0B : byte absolute $00+$48; // Timer/Counter0 Output Compare Register B
  OCR0A : byte absolute $00+$47; // Timer/Counter0 Output Compare Register
  TCNT0 : byte absolute $00+$46; // Timer/Counter0 Register
  TCCR0B : byte absolute $00+$45; // Timer/Counter0 Control Register B
  TCCR0A : byte absolute $00+$44; // Timer/Counter0 Control Register A
  TIMSK0 : byte absolute $00+$6E; // Timer/Counter0 Interrupt Mask Register
  TIFR0 : byte absolute $00+$35; // Timer/Counter0 Interrupt Flag Register
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
  // WATCHDOG
  WDTCSR : byte absolute $00+$60; // Watchdog Timer Control Register
  // TIMER_COUNTER_5
  TCCR5A : byte absolute $00+$120; // Timer/Counter5 Control Register A
  TCCR5B : byte absolute $00+$121; // Timer/Counter5 Control Register B
  TCCR5C : byte absolute $00+$122; // Timer/Counter5 Control Register C
  TCNT5 : word absolute $00+$124; // Timer/Counter5  Bytes
  TCNT5L : byte absolute $00+$124; // Timer/Counter5  Bytes
  TCNT5H : byte absolute $00+$124+1; // Timer/Counter5  Bytes
  OCR5A : word absolute $00+$128; // Timer/Counter5 Output Compare Register A  Bytes
  OCR5AL : byte absolute $00+$128; // Timer/Counter5 Output Compare Register A  Bytes
  OCR5AH : byte absolute $00+$128+1; // Timer/Counter5 Output Compare Register A  Bytes
  OCR5B : word absolute $00+$12A; // Timer/Counter5 Output Compare Register B  Bytes
  OCR5BL : byte absolute $00+$12A; // Timer/Counter5 Output Compare Register B  Bytes
  OCR5BH : byte absolute $00+$12A+1; // Timer/Counter5 Output Compare Register B  Bytes
  OCR5C : word absolute $00+$12C; // Timer/Counter5 Output Compare Register C  Bytes
  OCR5CL : byte absolute $00+$12C; // Timer/Counter5 Output Compare Register C  Bytes
  OCR5CH : byte absolute $00+$12C+1; // Timer/Counter5 Output Compare Register C  Bytes
  ICR5 : word absolute $00+$126; // Timer/Counter5 Input Capture Register  Bytes
  ICR5L : byte absolute $00+$126; // Timer/Counter5 Input Capture Register  Bytes
  ICR5H : byte absolute $00+$126+1; // Timer/Counter5 Input Capture Register  Bytes
  TIMSK5 : byte absolute $00+$73; // Timer/Counter5 Interrupt Mask Register
  TIFR5 : byte absolute $00+$3A; // Timer/Counter5 Interrupt Flag Register
  // TIMER_COUNTER_4
  TCCR4A : byte absolute $00+$A0; // Timer/Counter4 Control Register A
  TCCR4B : byte absolute $00+$A1; // Timer/Counter4 Control Register B
  TCCR4C : byte absolute $00+$A2; // Timer/Counter4 Control Register C
  TCNT4 : word absolute $00+$A4; // Timer/Counter4  Bytes
  TCNT4L : byte absolute $00+$A4; // Timer/Counter4  Bytes
  TCNT4H : byte absolute $00+$A4+1; // Timer/Counter4  Bytes
  OCR4A : word absolute $00+$A8; // Timer/Counter4 Output Compare Register A  Bytes
  OCR4AL : byte absolute $00+$A8; // Timer/Counter4 Output Compare Register A  Bytes
  OCR4AH : byte absolute $00+$A8+1; // Timer/Counter4 Output Compare Register A  Bytes
  OCR4B : word absolute $00+$AA; // Timer/Counter4 Output Compare Register B  Bytes
  OCR4BL : byte absolute $00+$AA; // Timer/Counter4 Output Compare Register B  Bytes
  OCR4BH : byte absolute $00+$AA+1; // Timer/Counter4 Output Compare Register B  Bytes
  OCR4C : word absolute $00+$AC; // Timer/Counter4 Output Compare Register C  Bytes
  OCR4CL : byte absolute $00+$AC; // Timer/Counter4 Output Compare Register C  Bytes
  OCR4CH : byte absolute $00+$AC+1; // Timer/Counter4 Output Compare Register C  Bytes
  ICR4 : word absolute $00+$A6; // Timer/Counter4 Input Capture Register  Bytes
  ICR4L : byte absolute $00+$A6; // Timer/Counter4 Input Capture Register  Bytes
  ICR4H : byte absolute $00+$A6+1; // Timer/Counter4 Input Capture Register  Bytes
  TIMSK4 : byte absolute $00+$72; // Timer/Counter4 Interrupt Mask Register
  TIFR4 : byte absolute $00+$39; // Timer/Counter4 Interrupt Flag Register
  // TIMER_COUNTER_3
  TCCR3A : byte absolute $00+$90; // Timer/Counter3 Control Register A
  TCCR3B : byte absolute $00+$91; // Timer/Counter3 Control Register B
  TCCR3C : byte absolute $00+$92; // Timer/Counter3 Control Register C
  TCNT3 : word absolute $00+$94; // Timer/Counter3  Bytes
  TCNT3L : byte absolute $00+$94; // Timer/Counter3  Bytes
  TCNT3H : byte absolute $00+$94+1; // Timer/Counter3  Bytes
  OCR3A : word absolute $00+$98; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AL : byte absolute $00+$98; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3AH : byte absolute $00+$98+1; // Timer/Counter3 Output Compare Register A  Bytes
  OCR3B : word absolute $00+$9A; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3BL : byte absolute $00+$9A; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3BH : byte absolute $00+$9A+1; // Timer/Counter3 Output Compare Register B  Bytes
  OCR3C : word absolute $00+$9C; // Timer/Counter3 Output Compare Register C  Bytes
  OCR3CL : byte absolute $00+$9C; // Timer/Counter3 Output Compare Register C  Bytes
  OCR3CH : byte absolute $00+$9C+1; // Timer/Counter3 Output Compare Register C  Bytes
  ICR3 : word absolute $00+$96; // Timer/Counter3 Input Capture Register  Bytes
  ICR3L : byte absolute $00+$96; // Timer/Counter3 Input Capture Register  Bytes
  ICR3H : byte absolute $00+$96+1; // Timer/Counter3 Input Capture Register  Bytes
  TIMSK3 : byte absolute $00+$71; // Timer/Counter3 Interrupt Mask Register
  TIFR3 : byte absolute $00+$38; // Timer/Counter3 Interrupt Flag Register
  // TIMER_COUNTER_1
  TCCR1A : byte absolute $00+$80; // Timer/Counter1 Control Register A
  TCCR1B : byte absolute $00+$81; // Timer/Counter1 Control Register B
  TCCR1C : byte absolute $00+$82; // Timer/Counter1 Control Register C
  TCNT1 : word absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1L : byte absolute $00+$84; // Timer/Counter1  Bytes
  TCNT1H : byte absolute $00+$84+1; // Timer/Counter1  Bytes
  OCR1A : word absolute $00+$88; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AL : byte absolute $00+$88; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1AH : byte absolute $00+$88+1; // Timer/Counter1 Output Compare Register A  Bytes
  OCR1B : word absolute $00+$8A; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BL : byte absolute $00+$8A; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1BH : byte absolute $00+$8A+1; // Timer/Counter1 Output Compare Register B  Bytes
  OCR1C : word absolute $00+$8C; // Timer/Counter1 Output Compare Register C  Bytes
  OCR1CL : byte absolute $00+$8C; // Timer/Counter1 Output Compare Register C  Bytes
  OCR1CH : byte absolute $00+$8C+1; // Timer/Counter1 Output Compare Register C  Bytes
  ICR1 : word absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1L : byte absolute $00+$86; // Timer/Counter1 Input Capture Register  Bytes
  ICR1H : byte absolute $00+$86+1; // Timer/Counter1 Input Capture Register  Bytes
  TIMSK1 : byte absolute $00+$6F; // Timer/Counter1 Interrupt Mask Register
  TIFR1 : byte absolute $00+$36; // Timer/Counter1 Interrupt Flag Register
  // TRX24
  AES_CTRL : byte absolute $00+$13C; // AES Control Register
  AES_STATUS : byte absolute $00+$13D; // AES Status Register
  AES_STATE : byte absolute $00+$13E; // AES Plain and Cipher Text Buffer Register
  AES_KEY : byte absolute $00+$13F; // AES Encryption and Decryption Key Buffer Register
  TRX_STATUS : byte absolute $00+$141; // Transceiver Status Register
  TRX_STATE : byte absolute $00+$142; // Transceiver State Control Register
  TRX_CTRL_0 : byte absolute $00+$143; // Reserved
  TRX_CTRL_1 : byte absolute $00+$144; // Transceiver Control Register 1
  PHY_TX_PWR : byte absolute $00+$145; // Transceiver Transmit Power Control Register
  PHY_RSSI : byte absolute $00+$146; // Receiver Signal Strength Indicator Register
  PHY_ED_LEVEL : byte absolute $00+$147; // Transceiver Energy Detection Level Register
  PHY_CC_CCA : byte absolute $00+$148; // Transceiver Clear Channel Assessment (CCA) Control Register
  CCA_THRES : byte absolute $00+$149; // Transceiver CCA Threshold Setting Register
  RX_CTRL : byte absolute $00+$14A; // Transceiver Receive Control Register
  SFD_VALUE : byte absolute $00+$14B; // Start of Frame Delimiter Value Register
  TRX_CTRL_2 : byte absolute $00+$14C; // Transceiver Control Register 2
  ANT_DIV : byte absolute $00+$14D; // Antenna Diversity Control Register
  IRQ_MASK : byte absolute $00+$14E; // Transceiver Interrupt Enable Register
  IRQ_STATUS : byte absolute $00+$14F; // Transceiver Interrupt Status Register
  VREG_CTRL : byte absolute $00+$150; // Voltage Regulator Control and Status Register
  BATMON : byte absolute $00+$151; // Battery Monitor Control and Status Register
  XOSC_CTRL : byte absolute $00+$152; // Crystal Oscillator Control Register
  RX_SYN : byte absolute $00+$155; // Transceiver Receiver Sensitivity Control Register
  XAH_CTRL_1 : byte absolute $00+$157; // Transceiver Acknowledgment Frame Control Register 1
  FTN_CTRL : byte absolute $00+$158; // Transceiver Filter Tuning Control Register
  PLL_CF : byte absolute $00+$15A; // Transceiver Center Frequency Calibration Control Register
  PLL_DCU : byte absolute $00+$15B; // Transceiver Delay Cell Calibration Control Register
  PART_NUM : byte absolute $00+$15C; // Device Identification Register (Part Number)
  VERSION_NUM : byte absolute $00+$15D; // Device Identification Register (Version Number)
  MAN_ID_0 : byte absolute $00+$15E; // Device Identification Register (Manufacture ID Low Byte)
  MAN_ID_1 : byte absolute $00+$15F; // Device Identification Register (Manufacture ID High Byte)
  SHORT_ADDR_0 : byte absolute $00+$160; // Transceiver MAC Short Address Register (Low Byte)
  SHORT_ADDR_1 : byte absolute $00+$161; // Transceiver MAC Short Address Register (High Byte)
  PAN_ID_0 : byte absolute $00+$162; // Transceiver Personal Area Network ID Register (Low Byte)
  PAN_ID_1 : byte absolute $00+$163; // Transceiver Personal Area Network ID Register (High Byte)
  IEEE_ADDR_0 : byte absolute $00+$164; // Transceiver MAC IEEE Address Register 0
  IEEE_ADDR_1 : byte absolute $00+$165; // Transceiver MAC IEEE Address Register 1
  IEEE_ADDR_2 : byte absolute $00+$166; // Transceiver MAC IEEE Address Register 2
  IEEE_ADDR_3 : byte absolute $00+$167; // Transceiver MAC IEEE Address Register 3
  IEEE_ADDR_4 : byte absolute $00+$168; // Transceiver MAC IEEE Address Register 4
  IEEE_ADDR_5 : byte absolute $00+$169; // Transceiver MAC IEEE Address Register 5
  IEEE_ADDR_6 : byte absolute $00+$16A; // Transceiver MAC IEEE Address Register 6
  IEEE_ADDR_7 : byte absolute $00+$16B; // Transceiver MAC IEEE Address Register 7
  XAH_CTRL_0 : byte absolute $00+$16C; // Transceiver Extended Operating Mode Control Register
  CSMA_SEED_0 : byte absolute $00+$16D; // Transceiver CSMA-CA Random Number Generator Seed Register
  CSMA_SEED_1 : byte absolute $00+$16E; // Transceiver Acknowledgment Frame Control Register 2
  CSMA_BE : byte absolute $00+$16F; // Transceiver CSMA-CA Back-off Exponent Control Register
  TST_CTRL_DIGI : byte absolute $00+$176; // Transceiver Digital Test Control Register
  TST_RX_LENGTH : byte absolute $00+$17B; // Transceiver Received Frame Length Register
  TRXFBST : byte absolute $00+$180; // Start of frame buffer
  TRXFBEND : byte absolute $00+$1FF; // End of frame buffer
  // SYMCNT
  SCOCR1HH : byte absolute $00+$F8; // Symbol Counter Output Compare Register 1 HH-Byte
  SCOCR1HL : byte absolute $00+$F7; // Symbol Counter Output Compare Register 1 HL-Byte
  SCOCR1LH : byte absolute $00+$F6; // Symbol Counter Output Compare Register 1 LH-Byte
  SCOCR1LL : byte absolute $00+$F5; // Symbol Counter Output Compare Register 1 LL-Byte
  SCOCR2HH : byte absolute $00+$F4; // Symbol Counter Output Compare Register 2 HH-Byte
  SCOCR2HL : byte absolute $00+$F3; // Symbol Counter Output Compare Register 2 HL-Byte
  SCOCR2LH : byte absolute $00+$F2; // Symbol Counter Output Compare Register 2 LH-Byte
  SCOCR2LL : byte absolute $00+$F1; // Symbol Counter Output Compare Register 2 LL-Byte
  SCOCR3HH : byte absolute $00+$F0; // Symbol Counter Output Compare Register 3 HH-Byte
  SCOCR3HL : byte absolute $00+$EF; // Symbol Counter Output Compare Register 3 HL-Byte
  SCOCR3LH : byte absolute $00+$EE; // Symbol Counter Output Compare Register 3 LH-Byte
  SCOCR3LL : byte absolute $00+$ED; // Symbol Counter Output Compare Register 3 LL-Byte
  SCTSRHH : byte absolute $00+$EC; // Symbol Counter Frame Timestamp Register HH-Byte
  SCTSRHL : byte absolute $00+$EB; // Symbol Counter Frame Timestamp Register HL-Byte
  SCTSRLH : byte absolute $00+$EA; // Symbol Counter Frame Timestamp Register LH-Byte
  SCTSRLL : byte absolute $00+$E9; // Symbol Counter Frame Timestamp Register LL-Byte
  SCBTSRHH : byte absolute $00+$E8; // Symbol Counter Beacon Timestamp Register HH-Byte
  SCBTSRHL : byte absolute $00+$E7; // Symbol Counter Beacon Timestamp Register HL-Byte
  SCBTSRLH : byte absolute $00+$E6; // Symbol Counter Beacon Timestamp Register LH-Byte
  SCBTSRLL : byte absolute $00+$E5; // Symbol Counter Beacon Timestamp Register LL-Byte
  SCCNTHH : byte absolute $00+$E4; // Symbol Counter Register HH-Byte
  SCCNTHL : byte absolute $00+$E3; // Symbol Counter Register HL-Byte
  SCCNTLH : byte absolute $00+$E2; // Symbol Counter Register LH-Byte
  SCCNTLL : byte absolute $00+$E1; // Symbol Counter Register LL-Byte
  SCIRQS : byte absolute $00+$E0; // Symbol Counter Interrupt Status Register
  SCIRQM : byte absolute $00+$DF; // Symbol Counter Interrupt Mask Register
  SCSR : byte absolute $00+$DE; // Symbol Counter Status Register
  SCCR1 : byte absolute $00+$DD; // Symbol Counter Control Register 1
  SCCR0 : byte absolute $00+$DC; // Symbol Counter Control Register 0
  // EEPROM
  EEAR : word absolute $00+$41; // EEPROM Address Register  Bytes
  EEARL : byte absolute $00+$41; // EEPROM Address Register  Bytes
  EEARH : byte absolute $00+$41+1; // EEPROM Address Register  Bytes
  EEDR : byte absolute $00+$40; // EEPROM Data Register
  EECR : byte absolute $00+$3F; // EEPROM Control Register
  // JTAG
  OCDR : byte absolute $00+$51; // On-Chip Debug Register
  MCUCR : byte absolute $00+$55; // MCU Control Register
  MCUSR : byte absolute $00+$54; // MCU Status Register
  // EXTERNAL_INTERRUPT
  EICRA : byte absolute $00+$69; // External Interrupt Control Register A
  EICRB : byte absolute $00+$6A; // External Interrupt Control Register B
  EIMSK : byte absolute $00+$3D; // External Interrupt Mask Register
  EIFR : byte absolute $00+$3C; // External Interrupt Flag Register
  PCMSK2 : byte absolute $00+$6D; // Pin Change Mask Register 2
  PCMSK1 : byte absolute $00+$6C; // Pin Change Mask Register 1
  PCMSK0 : byte absolute $00+$6B; // Pin Change Mask Register 0
  PCIFR : byte absolute $00+$3B; // Pin Change Interrupt Flag Register
  PCICR : byte absolute $00+$68; // Pin Change Interrupt Control Register
  // AD_CONVERTER
  ADMUX : byte absolute $00+$7C; // The ADC Multiplexer Selection Register
  ADC : word absolute $00+$78; // ADC Data Register  Bytes
  ADCL : byte absolute $00+$78; // ADC Data Register  Bytes
  ADCH : byte absolute $00+$78+1; // ADC Data Register  Bytes
  ADCSRA : byte absolute $00+$7A; // The ADC Control and Status Register A
  ADCSRC : byte absolute $00+$77; // The ADC Control and Status Register C
  DIDR2 : byte absolute $00+$7D; // Digital Input Disable Register 2
  DIDR0 : byte absolute $00+$7E; // Digital Input Disable Register 0
  // BOOT_LOAD
  SPMCSR : byte absolute $00+$57; // Store Program Memory Control Register
  // CPU
  SREG : byte absolute $00+$5F; // Status Register
  SP : word absolute $00+$5D; // Stack Pointer 
  SPL : byte absolute $00+$5D; // Stack Pointer 
  SPH : byte absolute $00+$5D+1; // Stack Pointer 
  OSCCAL : byte absolute $00+$66; // Oscillator Calibration Value
  CLKPR : byte absolute $00+$61; // Clock Prescale Register
  SMCR : byte absolute $00+$53; // Sleep Mode Control Register
  RAMPZ : byte absolute $00+$5B; // Extended Z-pointer Register for ELPM/SPM
  GPIOR2 : byte absolute $00+$4B; // General Purpose I/O Register 2
  GPIOR1 : byte absolute $00+$4A; // General Purpose IO Register 1
  GPIOR0 : byte absolute $00+$3E; // General Purpose IO Register 0
  PRR2 : byte absolute $00+$63; // Power Reduction Register 2
  PRR1 : byte absolute $00+$65; // Power Reduction Register 1
  PRR0 : byte absolute $00+$64; // Power Reduction Register0
  // FLASH
  NEMCR : byte absolute $00+$75; // Flash Extended-Mode Control-Register
  BGCR : byte absolute $00+$67; // Reference Voltage Calibration Register
  // PWRCTRL
  TRXPR : byte absolute $00+$139; // Transceiver Pin Register
  DRTRAM0 : byte absolute $00+$135; // Data Retention Configuration Register of SRAM 0
  DRTRAM1 : byte absolute $00+$134; // Data Retention Configuration Register of SRAM 1
  DRTRAM2 : byte absolute $00+$133; // Data Retention Configuration Register of SRAM 2
  DRTRAM3 : byte absolute $00+$132; // Data Retention Configuration Register of SRAM 3
  LLDRL : byte absolute $00+$130; // Low Leakage Voltage Regulator Data Register (Low-Byte)
  LLDRH : byte absolute $00+$131; // Low Leakage Voltage Regulator Data Register (High-Byte)
  LLCR : byte absolute $00+$12F; // Low Leakage Voltage Regulator Control Register
  DPDS0 : byte absolute $00+$136; // Port Driver Strength Register 0
  DPDS1 : byte absolute $00+$137; // Port Driver Strength Register 1
  // USART0_SPI
  // USART1_SPI

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
  ACIS = 0; // Analog Comparator Interrupt Mode Select
  // DIDR1
  AIN1D = 1; // AIN1 Digital Input Disable
  AIN0D = 0; // AIN0 Digital Input Disable
  // UCSR0A
  RXC0 = 7; // USART Receive Complete
  TXC0 = 6; // USART Transmit Complete
  UDRE0 = 5; // USART Data Register Empty
  FE0 = 4; // Frame Error
  DOR0 = 3; // Data OverRun
  UPE0 = 2; // USART Parity Error
  U2X0 = 1; // Double the USART Transmission Speed
  MPCM0 = 0; // Multi-processor Communication Mode
  // UCSR0B
  RXCIE0 = 7; // RX Complete Interrupt Enable
  TXCIE0 = 6; // TX Complete Interrupt Enable
  UDRIE0 = 5; // USART Data Register Empty Interrupt Enable
  RXEN0 = 4; // Receiver Enable
  TXEN0 = 3; // Transmitter Enable
  UCSZ02 = 2; // Character Size
  RXB80 = 1; // Receive Data Bit 8
  TXB80 = 0; // Transmit Data Bit 8
  // UCSR0C
  UMSEL0 = 6; // USART Mode Select
  UPM0 = 4; // Parity Mode
  USBS0 = 3; // Stop Bit Select
  UCSZ0 = 1; // Character Size
  UCPOL0 = 0; // Clock Polarity
  // UCSR1A
  RXC1 = 7; // USART Receive Complete
  TXC1 = 6; // USART Transmit Complete
  UDRE1 = 5; // USART Data Register Empty
  FE1 = 4; // Frame Error
  DOR1 = 3; // Data OverRun
  UPE1 = 2; // USART Parity Error
  U2X1 = 1; // Double the USART Transmission Speed
  MPCM1 = 0; // Multi-processor Communication Mode
  // UCSR1B
  RXCIE1 = 7; // RX Complete Interrupt Enable
  TXCIE1 = 6; // TX Complete Interrupt Enable
  UDRIE1 = 5; // USART Data Register Empty Interrupt Enable
  RXEN1 = 4; // Receiver Enable
  TXEN1 = 3; // Transmitter Enable
  UCSZ12 = 2; // Character Size
  RXB81 = 1; // Receive Data Bit 8
  TXB81 = 0; // Transmit Data Bit 8
  // UCSR1C
  UMSEL1 = 6; // USART Mode Select
  UPM1 = 4; // Parity Mode
  USBS1 = 3; // Stop Bit Select
  UCSZ1 = 1; // Character Size
  UCPOL1 = 0; // Clock Polarity
  // TWAMR
  TWAM = 1; // TWI Address Mask
  Res = 0; // Reserved Bit
  // TWCR
  TWINT = 7; // TWI Interrupt Flag
  TWEA = 6; // TWI Enable Acknowledge Bit
  TWSTA = 5; // TWI START Condition Bit
  TWSTO = 4; // TWI STOP Condition Bit
  TWWC = 3; // TWI Write Collision Flag
  TWEN = 2; // TWI Enable Bit
  TWIE = 0; // TWI Interrupt Enable
  // TWSR
  TWS = 3; // TWI Status
  TWPS = 0; // TWI Prescaler Bits
  // TWAR
  TWA = 1; // TWI (Slave) Address
  TWGCE = 0; // TWI General Call Recognition Enable Bit
  // SPCR
  SPIE = 7; // SPI Interrupt Enable
  SPE = 6; // SPI Enable
  DORD = 5; // Data Order
  MSTR = 4; // Master/Slave Select
  CPOL = 3; // Clock polarity
  CPHA = 2; // Clock Phase
  SPR = 0; // SPI Clock Rate Select 1 and 0
  // SPSR
  SPIF = 7; // SPI Interrupt Flag
  WCOL = 6; // Write Collision Flag
  SPI2X = 0; // Double SPI Speed Bit
  // TCCR0B
  FOC0A = 7; // Force Output Compare A
  FOC0B = 6; // Force Output Compare B
  WGM02 = 3; // 
  CS0 = 0; // Clock Select
  // TCCR0A
  COM0A = 6; // Compare Match Output A Mode
  COM0B = 4; // Compare Match Output B Mode
  WGM0 = 0; // Waveform Generation Mode
  // TIMSK0
  OCIE0B = 2; // Timer/Counter0 Output Compare Match B Interrupt Enable
  OCIE0A = 1; // Timer/Counter0 Output Compare Match A Interrupt Enable
  TOIE0 = 0; // Timer/Counter0 Overflow Interrupt Enable
  // TIFR0
  OCF0B = 2; // Timer/Counter0 Output Compare B Match Flag
  OCF0A = 1; // Timer/Counter0 Output Compare A Match Flag
  TOV0 = 0; // Timer/Counter0 Overflow Flag
  // GTCCR
  TSM = 7; // Timer/Counter Synchronization Mode
  PSRASY = 1; // Prescaler Reset Timer/Counter2
  PSRSYNC = 0; // Prescaler Reset for Synchronous Timer/Counters
  // TIMSK2
  OCIE2B = 2; // Timer/Counter2 Output Compare Match B Interrupt Enable
  OCIE2A = 1; // Timer/Counter2 Output Compare Match A Interrupt Enable
  TOIE2 = 0; // Timer/Counter2 Overflow Interrupt Enable
  // TIFR2
  OCF2B = 2; // Output Compare Flag 2 B
  OCF2A = 1; // Output Compare Flag 2 A
  TOV2 = 0; // Timer/Counter2 Overflow Flag
  // TCCR2A
  COM2A = 6; // Compare Match Output A Mode
  COM2B = 4; // Compare Match Output B Mode
  WGM2 = 0; // Waveform Generation Mode
  // TCCR2B
  FOC2A = 7; // Force Output Compare A
  FOC2B = 6; // Force Output Compare B
  WGM22 = 3; // Waveform Generation Mode
  CS2 = 0; // Clock Select
  // ASSR
  EXCLKAMR = 7; // Enable External Clock Input for AMR
  EXCLK = 6; // Enable External Clock Input
  AS2 = 5; // Timer/Counter2 Asynchronous Mode
  TCN2UB = 4; // Timer/Counter2 Update Busy
  OCR2AUB = 3; // Timer/Counter2 Output Compare Register A Update Busy
  OCR2BUB = 2; // Timer/Counter2 Output Compare Register B Update Busy
  TCR2AUB = 1; // Timer/Counter2 Control Register A Update Busy
  TCR2BUB = 0; // Timer/Counter2 Control Register B Update Busy
  // GTCCR
  // WDTCSR
  WDIF = 7; // Watchdog Timeout Interrupt Flag
  WDIE = 6; // Watchdog Timeout Interrupt Enable
  WDP = 0; // Watchdog Timer Prescaler Bits
  WDCE = 4; // Watchdog Change Enable
  WDE = 3; // Watch Dog Enable
  // TCCR5A
  COM5A = 6; // Compare Output Mode for Channel A
  COM5B = 4; // Compare Output Mode for Channel B
  COM5C = 2; // Compare Output Mode for Channel C
  WGM5 = 0; // Waveform Generation Mode
  // TCCR5B
  ICNC5 = 7; // Input Capture 5 Noise Canceller
  ICES5 = 6; // Input Capture 5 Edge Select
  CS5 = 0; // Clock Select
  // TCCR5C
  FOC5A = 7; // Force Output Compare for Channel A
  FOC5B = 6; // Force Output Compare for Channel B
  FOC5C = 5; // Force Output Compare for Channel C
  // TIMSK5
  ICIE5 = 5; // Timer/Counter5 Input Capture Interrupt Enable
  OCIE5C = 3; // Timer/Counter5 Output Compare C Match Interrupt Enable
  OCIE5B = 2; // Timer/Counter5 Output Compare B Match Interrupt Enable
  OCIE5A = 1; // Timer/Counter5 Output Compare A Match Interrupt Enable
  TOIE5 = 0; // Timer/Counter5 Overflow Interrupt Enable
  // TIFR5
  ICF5 = 5; // Timer/Counter5 Input Capture Flag
  OCF5C = 3; // Timer/Counter5 Output Compare C Match Flag
  OCF5B = 2; // Timer/Counter5 Output Compare B Match Flag
  OCF5A = 1; // Timer/Counter5 Output Compare A Match Flag
  TOV5 = 0; // Timer/Counter5 Overflow Flag
  // TCCR4A
  COM4A = 6; // Compare Output Mode for Channel A
  COM4B = 4; // Compare Output Mode for Channel B
  COM4C = 2; // Compare Output Mode for Channel C
  WGM4 = 0; // Waveform Generation Mode
  // TCCR4B
  ICNC4 = 7; // Input Capture 4 Noise Canceller
  ICES4 = 6; // Input Capture 4 Edge Select
  CS4 = 0; // Clock Select
  // TCCR4C
  FOC4A = 7; // Force Output Compare for Channel A
  FOC4B = 6; // Force Output Compare for Channel B
  FOC4C = 5; // Force Output Compare for Channel C
  // TIMSK4
  ICIE4 = 5; // Timer/Counter4 Input Capture Interrupt Enable
  OCIE4C = 3; // Timer/Counter4 Output Compare C Match Interrupt Enable
  OCIE4B = 2; // Timer/Counter4 Output Compare B Match Interrupt Enable
  OCIE4A = 1; // Timer/Counter4 Output Compare A Match Interrupt Enable
  TOIE4 = 0; // Timer/Counter4 Overflow Interrupt Enable
  // TIFR4
  ICF4 = 5; // Timer/Counter4 Input Capture Flag
  OCF4C = 3; // Timer/Counter4 Output Compare C Match Flag
  OCF4B = 2; // Timer/Counter4 Output Compare B Match Flag
  OCF4A = 1; // Timer/Counter4 Output Compare A Match Flag
  TOV4 = 0; // Timer/Counter4 Overflow Flag
  // TCCR3A
  COM3A = 6; // Compare Output Mode for Channel A
  COM3B = 4; // Compare Output Mode for Channel B
  COM3C = 2; // Compare Output Mode for Channel C
  WGM3 = 0; // Waveform Generation Mode
  // TCCR3B
  ICNC3 = 7; // Input Capture 3 Noise Canceller
  ICES3 = 6; // Input Capture 3 Edge Select
  CS3 = 0; // Clock Select
  // TCCR3C
  FOC3A = 7; // Force Output Compare for Channel A
  FOC3B = 6; // Force Output Compare for Channel B
  FOC3C = 5; // Force Output Compare for Channel C
  // TIMSK3
  ICIE3 = 5; // Timer/Counter3 Input Capture Interrupt Enable
  OCIE3C = 3; // Timer/Counter3 Output Compare C Match Interrupt Enable
  OCIE3B = 2; // Timer/Counter3 Output Compare B Match Interrupt Enable
  OCIE3A = 1; // Timer/Counter3 Output Compare A Match Interrupt Enable
  TOIE3 = 0; // Timer/Counter3 Overflow Interrupt Enable
  // TIFR3
  ICF3 = 5; // Timer/Counter3 Input Capture Flag
  OCF3C = 3; // Timer/Counter3 Output Compare C Match Flag
  OCF3B = 2; // Timer/Counter3 Output Compare B Match Flag
  OCF3A = 1; // Timer/Counter3 Output Compare A Match Flag
  TOV3 = 0; // Timer/Counter3 Overflow Flag
  // TCCR1A
  COM1A = 6; // Compare Output Mode for Channel A
  COM1B = 4; // Compare Output Mode for Channel B
  COM1C = 2; // Compare Output Mode for Channel C
  WGM1 = 0; // Waveform Generation Mode
  // TCCR1B
  ICNC1 = 7; // Input Capture 1 Noise Canceller
  ICES1 = 6; // Input Capture 1 Edge Select
  CS1 = 0; // Clock Select
  // TCCR1C
  FOC1A = 7; // Force Output Compare for Channel A
  FOC1B = 6; // Force Output Compare for Channel B
  FOC1C = 5; // Force Output Compare for Channel C
  // TIMSK1
  ICIE1 = 5; // Timer/Counter1 Input Capture Interrupt Enable
  OCIE1C = 3; // Timer/Counter1 Output Compare C Match Interrupt Enable
  OCIE1B = 2; // Timer/Counter1 Output Compare B Match Interrupt Enable
  OCIE1A = 1; // Timer/Counter1 Output Compare A Match Interrupt Enable
  TOIE1 = 0; // Timer/Counter1 Overflow Interrupt Enable
  // TIFR1
  ICF1 = 5; // Timer/Counter1 Input Capture Flag
  OCF1C = 3; // Timer/Counter1 Output Compare C Match Flag
  OCF1B = 2; // Timer/Counter1 Output Compare B Match Flag
  OCF1A = 1; // Timer/Counter1 Output Compare A Match Flag
  TOV1 = 0; // Timer/Counter1 Overflow Flag
  // AES_CTRL
  AES_REQUEST = 7; // Request AES Operation.
  AES_MODE = 5; // Set AES Operation Mode
  AES_DIR = 3; // Set AES Operation Direction
  AES_IM = 2; // AES Interrupt Enable
  // AES_STATUS
  AES_ER = 7; // AES Operation Finished with Error
  AES_DONE = 0; // AES Operation Finished with Success
  // AES_STATE
  // AES_KEY
  // TRX_STATUS
  CCA_DONE = 7; // CCA Algorithm Status
  CCA_STATUS = 6; // CCA Status Result
  TST_STATUS = 5; // Test mode status
  // TRX_STATE
  TRAC_STATUS = 5; // Transaction Status
  TRX_CMD = 0; // State Control Command
  // TRX_CTRL_0
  // TRX_CTRL_1
  PA_EXT_EN = 7; // External PA support enable
  IRQ_2_EXT_EN = 6; // Connect Frame Start IRQ to TC1
  TX_AUTO_CRC_ON = 5; // Enable Automatic CRC Calculation
  // PHY_TX_PWR
  PA_BUF_LT = 6; // Power Amplifier Buffer Lead Time
  PA_LT = 4; // Power Amplifier Lead Time
  TX_PWR = 0; // Transmit Power Setting
  // PHY_RSSI
  RX_CRC_VALID = 7; // Received Frame CRC Status
  RND_VALUE = 5; // Random Value
  RSSI = 0; // Receiver Signal Strength Indicator
  // PHY_ED_LEVEL
  ED_LEVEL = 0; // Energy Detection Level
  // PHY_CC_CCA
  CCA_REQUEST = 7; // Manual CCA Measurement Request
  CCA_MODE = 5; // Select CCA Measurement Mode
  CHANNEL = 0; // RX/TX Channel Selection
  // CCA_THRES
  CCA_CS_THRES = 4; // CS Threshold Level for CCA Measurement
  CCA_ED_THRES = 0; // ED Threshold Level for CCA Measurement
  // RX_CTRL
  PDT_THRES = 0; // Receiver Sensitivity Control
  // SFD_VALUE
  // TRX_CTRL_2
  RX_SAFE_MODE = 7; // RX Safe Mode
  OQPSK_DATA_RATE = 0; // Data Rate Selection
  // ANT_DIV
  ANT_SEL = 7; // Antenna Diversity Antenna Status
  ANT_DIV_EN = 3; // Enable Antenna Diversity
  ANT_EXT_SW_EN = 2; // Enable External Antenna Switch Control
  ANT_CTRL = 0; // Static Antenna Diversity Switch Control
  // IRQ_MASK
  AWAKE_EN = 7; // Awake Interrupt Enable
  TX_END_EN = 6; // TX_END Interrupt Enable
  AMI_EN = 5; // Address Match Interrupt Enable
  CCA_ED_DONE_EN = 4; // End of ED Measurement Interrupt Enable
  RX_END_EN = 3; // RX_END Interrupt Enable
  RX_START_EN = 2; // RX_START Interrupt Enable
  PLL_UNLOCK_EN = 1; // PLL Unlock Interrupt Enable
  PLL_LOCK_EN = 0; // PLL Lock Interrupt Enable
  // IRQ_STATUS
  AWAKE = 7; // Awake Interrupt Status
  TX_END = 6; // TX_END Interrupt Status
  AMI = 5; // Address Match Interrupt Status
  CCA_ED_DONE = 4; // End of ED Measurement Interrupt Status
  RX_END = 3; // RX_END Interrupt Status
  RX_START = 2; // RX_START Interrupt Status
  PLL_UNLOCK = 1; // PLL Unlock Interrupt Status
  PLL_LOCK = 0; // PLL Lock Interrupt Status
  // VREG_CTRL
  AVREG_EXT = 7; // Use External AVDD Regulator
  AVDD_OK = 6; // AVDD Supply Voltage Valid
  DVREG_EXT = 3; // Use External DVDD Regulator
  DVDD_OK = 2; // DVDD Supply Voltage Valid
  // BATMON
  BAT_LOW = 7; // Battery Monitor Interrupt Status
  BAT_LOW_EN = 6; // Battery Monitor Interrupt Enable
  BATMON_OK = 5; // Battery Monitor Status
  BATMON_HR = 4; // Battery Monitor Voltage Range
  BATMON_VTH = 0; // Battery Monitor Threshold Voltage
  // XOSC_CTRL
  XTAL_MODE = 4; // Crystal Oscillator Operating Mode
  XTAL_TRIM = 0; // Crystal Oscillator Load Capacitance Trimming
  // RX_SYN
  RX_PDT_DIS = 7; // Prevent Frame Reception
  RX_PDT_LEVEL = 0; // Reduce Receiver Sensitivity
  // XAH_CTRL_1
  AACK_FLTR_RES_FT = 5; // Filter Reserved Frames
  AACK_UPLD_RES_FT = 4; // Process Reserved Frames
  AACK_ACK_TIME = 2; // Reduce Acknowledgment Time
  AACK_PROM_MODE = 1; // Enable Promiscuous Mode
  // FTN_CTRL
  FTN_START = 7; // Start Calibration Loop of Filter Tuning Network
  // PLL_CF
  PLL_CF_START = 7; // Start Center Frequency Calibration
  // PLL_DCU
  PLL_DCU_START = 7; // Start Delay Cell Calibration
  // PART_NUM
  // VERSION_NUM
  // MAN_ID_0
  MAN_ID_07 = 7; // Manufacturer ID (Low Byte)
  MAN_ID_06 = 6; // Manufacturer ID (Low Byte)
  MAN_ID_05 = 5; // Manufacturer ID (Low Byte)
  MAN_ID_04 = 4; // Manufacturer ID (Low Byte)
  MAN_ID_03 = 3; // Manufacturer ID (Low Byte)
  MAN_ID_02 = 2; // Manufacturer ID (Low Byte)
  MAN_ID_01 = 1; // Manufacturer ID (Low Byte)
  MAN_ID_00 = 0; // Manufacturer ID (Low Byte)
  // MAN_ID_1
  MAN_ID_ = 0; // Manufacturer ID (High Byte)
  // SHORT_ADDR_0
  SHORT_ADDR_07 = 7; // MAC Short Address
  SHORT_ADDR_06 = 6; // MAC Short Address
  SHORT_ADDR_05 = 5; // MAC Short Address
  SHORT_ADDR_04 = 4; // MAC Short Address
  SHORT_ADDR_03 = 3; // MAC Short Address
  SHORT_ADDR_02 = 2; // MAC Short Address
  SHORT_ADDR_01 = 1; // MAC Short Address
  SHORT_ADDR_00 = 0; // MAC Short Address
  // SHORT_ADDR_1
  SHORT_ADDR_ = 0; // MAC Short Address
  // PAN_ID_0
  PAN_ID_07 = 7; // MAC Personal Area Network ID
  PAN_ID_06 = 6; // MAC Personal Area Network ID
  PAN_ID_05 = 5; // MAC Personal Area Network ID
  PAN_ID_04 = 4; // MAC Personal Area Network ID
  PAN_ID_03 = 3; // MAC Personal Area Network ID
  PAN_ID_02 = 2; // MAC Personal Area Network ID
  PAN_ID_01 = 1; // MAC Personal Area Network ID
  PAN_ID_00 = 0; // MAC Personal Area Network ID
  // PAN_ID_1
  PAN_ID_ = 0; // MAC Personal Area Network ID
  // IEEE_ADDR_0
  IEEE_ADDR_07 = 7; // MAC IEEE Address
  IEEE_ADDR_06 = 6; // MAC IEEE Address
  IEEE_ADDR_05 = 5; // MAC IEEE Address
  IEEE_ADDR_04 = 4; // MAC IEEE Address
  IEEE_ADDR_03 = 3; // MAC IEEE Address
  IEEE_ADDR_02 = 2; // MAC IEEE Address
  IEEE_ADDR_01 = 1; // MAC IEEE Address
  IEEE_ADDR_00 = 0; // MAC IEEE Address
  // IEEE_ADDR_1
  IEEE_ADDR_ = 0; // MAC IEEE Address
  // IEEE_ADDR_2
  // IEEE_ADDR_3
  // IEEE_ADDR_4
  // IEEE_ADDR_5
  // IEEE_ADDR_6
  // IEEE_ADDR_7
  // XAH_CTRL_0
  MAX_FRAME_RETRIES = 4; // Maximum Number of Frame Re-transmission Attempts
  MAX_CSMA_RETRIES = 1; // Maximum Number of CSMA-CA Procedure Repetition Attempts
  SLOTTED_OPERATION = 0; // Set Slotted Acknowledgment
  // CSMA_SEED_0
  CSMA_SEED_07 = 7; // Seed Value for CSMA Random Number Generator
  CSMA_SEED_06 = 6; // Seed Value for CSMA Random Number Generator
  CSMA_SEED_05 = 5; // Seed Value for CSMA Random Number Generator
  CSMA_SEED_04 = 4; // Seed Value for CSMA Random Number Generator
  CSMA_SEED_03 = 3; // Seed Value for CSMA Random Number Generator
  CSMA_SEED_02 = 2; // Seed Value for CSMA Random Number Generator
  CSMA_SEED_01 = 1; // Seed Value for CSMA Random Number Generator
  CSMA_SEED_00 = 0; // Seed Value for CSMA Random Number Generator
  // CSMA_SEED_1
  AACK_FVN_MODE = 6; // Acknowledgment Frame Filter Mode
  AACK_SET_PD = 5; // Set Frame Pending Sub-field
  AACK_DIS_ACK = 4; // Disable Acknowledgment Frame Transmission
  AACK_I_AM_COORD = 3; // Set Personal Area Network Coordinator
  // CSMA_BE
  MAX_BE = 4; // Maximum Back-off Exponent
  MIN_BE = 0; // Minimum Back-off Exponent
  // TST_CTRL_DIGI
  TST_CTRL_DIG = 0; // Digital Test Controller Register
  // TST_RX_LENGTH
  RX_LENGTH = 0; // Received Frame Length
  // SCOCR1HH
  // SCOCR1HL
  // SCOCR1LH
  // SCOCR1LL
  // SCOCR2HH
  // SCOCR2HL
  // SCOCR2LH
  // SCOCR2LL
  // SCOCR3HH
  // SCOCR3HL
  // SCOCR3LH
  // SCOCR3LL
  // SCTSRHH
  // SCTSRHL
  // SCTSRLH
  // SCTSRLL
  // SCBTSRHH
  // SCBTSRHL
  // SCBTSRLH
  // SCBTSRLL
  // SCCNTHH
  // SCCNTHL
  // SCCNTLH
  // SCCNTLL
  // SCIRQS
  IRQSBO = 4; // Backoff Slot Counter IRQ
  IRQSOF = 3; // Symbol Counter Overflow IRQ
  IRQSCP = 0; // Compare Unit 3 Compare Match IRQ
  // SCIRQM
  IRQMBO = 4; // Backoff Slot Counter IRQ enable
  IRQMOF = 3; // Symbol Counter Overflow IRQ enable
  IRQMCP = 0; // Symbol Counter Compare Match 3 IRQ enable
  // SCSR
  SCBSY = 0; // Symbol Counter busy
  // SCCR1
  SCENBO = 0; // Backoff Slot Counter enable
  // SCCR0
  SCRES = 7; // Symbol Counter Synchronization
  SCMBTS = 6; // Manual Beacon Timestamp
  SCEN = 5; // Symbol Counter enable
  SCCKSEL = 4; // Symbol Counter Clock Source select
  SCTSE = 3; // Symbol Counter Automatic Timestamping enable
  SCCMP = 0; // Symbol Counter Compare Unit 3 Mode select
  // EECR
  EEPM = 4; // EEPROM Programming Mode
  EERIE = 3; // EEPROM Ready Interrupt Enable
  EEMPE = 2; // EEPROM Master Write Enable
  EEPE = 1; // EEPROM Programming Enable
  EERE = 0; // EEPROM Read Enable
  // OCDR
  // MCUCR
  JTD = 7; // JTAG Interface Disable
  // MCUSR
  JTRF = 4; // JTAG Reset Flag
  // EICRA
  ISC3 = 6; // External Interrupt 3 Sense Control Bit
  ISC2 = 4; // External Interrupt 2 Sense Control Bit
  ISC1 = 2; // External Interrupt 1 Sense Control Bit
  ISC0 = 0; // External Interrupt 0 Sense Control Bit
  // EICRB
  ISC7 = 6; // External Interrupt 7 Sense Control Bit
  ISC6 = 4; // External Interrupt 6 Sense Control Bit
  ISC5 = 2; // External Interrupt 5 Sense Control Bit
  ISC4 = 0; // External Interrupt 4 Sense Control Bit
  // EIMSK
  INT = 0; // External Interrupt Request Enable
  // EIFR
  INTF = 0; // External Interrupt Flag
  // PCMSK2
  PCINT = 0; // Pin Change Enable Mask
  // PCMSK1
  // PCIFR
  PCIF = 0; // Pin Change Interrupt Flags
  // PCICR
  PCIE = 0; // Pin Change Interrupt Enables
  // ADMUX
  REFS = 6; // Reference Selection Bits
  ADLAR = 5; // ADC Left Adjust Result
  MUX = 0; // Analog Channel and Gain Selection Bits
  // ADCSRA
  ADEN = 7; // ADC Enable
  ADSC = 6; // ADC Start Conversion
  ADATE = 5; // ADC Auto Trigger Enable
  ADIF = 4; // ADC Interrupt Flag
  ADIE = 3; // ADC Interrupt Enable
  ADPS = 0; // ADC  Prescaler Select Bits
  // ADCSRB
  AVDDOK = 7; // AVDD Supply Voltage OK
  REFOK = 5; // Reference Voltage OK
  ACCH = 4; // Analog Channel Change
  MUX5 = 3; // Analog Channel and Gain Selection Bits
  ADTS = 0; // ADC Auto Trigger Source
  // ADCSRC
  ADTHT = 6; // ADC Track-and-Hold Time
  Res0 = 5; // Reserved
  ADSUT = 0; // ADC Start-up Time
  // DIDR2
  ADC15D = 7; // Reserved Bits
  ADC14D = 6; // Reserved Bits
  ADC13D = 5; // Reserved Bits
  ADC12D = 4; // Reserved Bits
  ADC11D = 3; // Reserved Bits
  ADC10D = 2; // Reserved Bits
  ADC9D = 1; // Reserved Bits
  ADC8D = 0; // Reserved Bits
  // DIDR0
  ADC7D = 7; // Disable ADC7:0 Digital Input
  ADC6D = 6; // Disable ADC7:0 Digital Input
  ADC5D = 5; // Disable ADC7:0 Digital Input
  ADC4D = 4; // Disable ADC7:0 Digital Input
  ADC3D = 3; // Disable ADC7:0 Digital Input
  ADC2D = 2; // Disable ADC7:0 Digital Input
  ADC1D = 1; // Disable ADC7:0 Digital Input
  ADC0D = 0; // Disable ADC7:0 Digital Input
  // SPMCSR
  SPMIE = 7; // SPM Interrupt Enable
  RWWSB = 6; // Read While Write Section Busy
  SIGRD = 5; // Signature Row Read
  RWWSRE = 4; // Read While Write Section Read Enable
  BLBSET = 3; // Boot Lock Bit Set
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
  // MCUCR
  PUD = 4; // Pull-up Disable
  IVSEL = 1; // Interrupt Vector Select
  IVCE = 0; // Interrupt Vector Change Enable
  // MCUSR
  WDRF = 3; // Watchdog Reset Flag
  BORF = 2; // Brown-out Reset Flag
  EXTRF = 1; // External Reset Flag
  PORF = 0; // Power-on Reset Flag
  // OSCCAL
  CAL = 0; // Oscillator Calibration Tuning Value
  // CLKPR
  CLKPCE = 7; // Clock Prescaler Change Enable
  CLKPS = 0; // Clock Prescaler Select Bits
  // SMCR
  SM = 1; // Sleep Mode Select bits
  SE = 0; // Sleep Enable
  // RAMPZ
  // GPIOR2
  GPIOR = 0; // General Purpose I/O Register 2 Value
  // GPIOR1
  // GPIOR0
  GPIOR07 = 7; // General Purpose I/O Register 0 Value
  GPIOR06 = 6; // General Purpose I/O Register 0 Value
  GPIOR05 = 5; // General Purpose I/O Register 0 Value
  GPIOR04 = 4; // General Purpose I/O Register 0 Value
  GPIOR03 = 3; // General Purpose I/O Register 0 Value
  GPIOR02 = 2; // General Purpose I/O Register 0 Value
  GPIOR01 = 1; // General Purpose I/O Register 0 Value
  GPIOR00 = 0; // General Purpose I/O Register 0 Value
  // PRR2
  PRRAM = 0; // Power Reduction SRAMs
  // PRR1
  PRTRX24 = 6; // Power Reduction Transceiver
  PRTIM5 = 5; // Power Reduction Timer/Counter5
  PRTIM4 = 4; // Power Reduction Timer/Counter4
  PRTIM3 = 3; // Power Reduction Timer/Counter3
  PRUSART = 0; // Reserved
  // PRR0
  PRTWI = 7; // Power Reduction TWI
  PRTIM2 = 6; // Power Reduction Timer/Counter2
  PRTIM0 = 5; // Power Reduction Timer/Counter0
  PRPGA = 4; // Power Reduction PGA
  PRTIM1 = 3; // Power Reduction Timer/Counter1
  PRSPI = 2; // Power Reduction Serial Peripheral Interface
  PRUSART0 = 1; // Power Reduction USART
  PRADC = 0; // Power Reduction ADC
  // NEMCR
  ENEAM = 6; // Enable Extended Address Mode for Extra Rows
  AEAM = 4; // Address for Extended Address Mode of Extra Rows
  // BGCR
  BGCAL_FINE = 3; // Fine Calibration Bits
  BGCAL = 0; // Coarse Calibration Bits
  // TRXPR
  SLPTR = 1; // Multi-purpose Transceiver Control Bit
  TRXRST = 0; // Force Transceiver Reset
  // DRTRAM0
  DRTSWOK = 5; // DRT Switch OK
  ENDRT = 4; // Enable SRAM Data Retention
  // DRTRAM1
  // DRTRAM2
  // DRTRAM3
  // LLDRL
  // LLDRH
  // LLCR
  LLDONE = 5; // Calibration Done
  LLCOMP = 4; // Comparator Output
  LLCAL = 3; // Calibration Active
  LLTCO = 2; // Temperature Coefficient of Current Source
  LLSHORT = 1; // Short Lower Calibration Circuit
  LLENCAL = 0; // Enable Automatic Calibration
  // DPDS0
  PFDRV = 6; // Driver Strength Port F
  PEDRV = 4; // Driver Strength Port E
  PDDRV = 2; // Driver Strength Port D
  PBDRV = 0; // Driver Strength Port B
  // DPDS1
  PGDRV = 0; // Driver Strength Port G
  // MCUCR
  // UCSR0A
  // UCSR0B
  // UCSR0C
  UDORD0 = 2; // Data Order
  UCPHA0 = 1; // Clock Phase
  // UCSR1A
  // UCSR1B
  // UCSR1C
  UDORD1 = 2; // Data Order
  UCPHA1 = 1; // Clock Phase

implementation

{$i avrcommon.inc}

procedure INT0_ISR; external name 'INT0_ISR'; // Interrupt 1 External Interrupt Request 0
procedure INT1_ISR; external name 'INT1_ISR'; // Interrupt 2 External Interrupt Request 1
procedure INT2_ISR; external name 'INT2_ISR'; // Interrupt 3 External Interrupt Request 2
procedure INT3_ISR; external name 'INT3_ISR'; // Interrupt 4 External Interrupt Request 3
procedure INT4_ISR; external name 'INT4_ISR'; // Interrupt 5 External Interrupt Request 4
procedure INT5_ISR; external name 'INT5_ISR'; // Interrupt 6 External Interrupt Request 5
procedure INT6_ISR; external name 'INT6_ISR'; // Interrupt 7 External Interrupt Request 6
procedure INT7_ISR; external name 'INT7_ISR'; // Interrupt 8 External Interrupt Request 7
procedure PCINT0_ISR; external name 'PCINT0_ISR'; // Interrupt 9 Pin Change Interrupt Request 0
procedure PCINT1_ISR; external name 'PCINT1_ISR'; // Interrupt 10 Pin Change Interrupt Request 1
procedure PCINT2_ISR; external name 'PCINT2_ISR'; // Interrupt 11 Pin Change Interrupt Request 2
procedure WDT_ISR; external name 'WDT_ISR'; // Interrupt 12 Watchdog Time-out Interrupt
procedure TIMER2_COMPA_ISR; external name 'TIMER2_COMPA_ISR'; // Interrupt 13 Timer/Counter2 Compare Match A
procedure TIMER2_COMPB_ISR; external name 'TIMER2_COMPB_ISR'; // Interrupt 14 Timer/Counter2 Compare Match B
procedure TIMER2_OVF_ISR; external name 'TIMER2_OVF_ISR'; // Interrupt 15 Timer/Counter2 Overflow
procedure TIMER1_CAPT_ISR; external name 'TIMER1_CAPT_ISR'; // Interrupt 16 Timer/Counter1 Capture Event
procedure TIMER1_COMPA_ISR; external name 'TIMER1_COMPA_ISR'; // Interrupt 17 Timer/Counter1 Compare Match A
procedure TIMER1_COMPB_ISR; external name 'TIMER1_COMPB_ISR'; // Interrupt 18 Timer/Counter1 Compare Match B
procedure TIMER1_COMPC_ISR; external name 'TIMER1_COMPC_ISR'; // Interrupt 19 Timer/Counter1 Compare Match C
procedure TIMER1_OVF_ISR; external name 'TIMER1_OVF_ISR'; // Interrupt 20 Timer/Counter1 Overflow
procedure TIMER0_COMPA_ISR; external name 'TIMER0_COMPA_ISR'; // Interrupt 21 Timer/Counter0 Compare Match A
procedure TIMER0_COMPB_ISR; external name 'TIMER0_COMPB_ISR'; // Interrupt 22 Timer/Counter0 Compare Match B
procedure TIMER0_OVF_ISR; external name 'TIMER0_OVF_ISR'; // Interrupt 23 Timer/Counter0 Overflow
procedure SPI_STC_ISR; external name 'SPI_STC_ISR'; // Interrupt 24 SPI Serial Transfer Complete
procedure USART0_RX_ISR; external name 'USART0_RX_ISR'; // Interrupt 25 USART0, Rx Complete
procedure USART0_UDRE_ISR; external name 'USART0_UDRE_ISR'; // Interrupt 26 USART0 Data register Empty
procedure USART0_TX_ISR; external name 'USART0_TX_ISR'; // Interrupt 27 USART0, Tx Complete
procedure ANALOG_COMP_ISR; external name 'ANALOG_COMP_ISR'; // Interrupt 28 Analog Comparator
procedure ADC_ISR; external name 'ADC_ISR'; // Interrupt 29 ADC Conversion Complete
procedure EE_READY_ISR; external name 'EE_READY_ISR'; // Interrupt 30 EEPROM Ready
procedure TIMER3_CAPT_ISR; external name 'TIMER3_CAPT_ISR'; // Interrupt 31 Timer/Counter3 Capture Event
procedure TIMER3_COMPA_ISR; external name 'TIMER3_COMPA_ISR'; // Interrupt 32 Timer/Counter3 Compare Match A
procedure TIMER3_COMPB_ISR; external name 'TIMER3_COMPB_ISR'; // Interrupt 33 Timer/Counter3 Compare Match B
procedure TIMER3_COMPC_ISR; external name 'TIMER3_COMPC_ISR'; // Interrupt 34 Timer/Counter3 Compare Match C
procedure TIMER3_OVF_ISR; external name 'TIMER3_OVF_ISR'; // Interrupt 35 Timer/Counter3 Overflow
procedure USART1_RX_ISR; external name 'USART1_RX_ISR'; // Interrupt 36 USART1, Rx Complete
procedure USART1_UDRE_ISR; external name 'USART1_UDRE_ISR'; // Interrupt 37 USART1 Data register Empty
procedure USART1_TX_ISR; external name 'USART1_TX_ISR'; // Interrupt 38 USART1, Tx Complete
procedure TWI_ISR; external name 'TWI_ISR'; // Interrupt 39 2-wire Serial Interface
procedure SPM_READY_ISR; external name 'SPM_READY_ISR'; // Interrupt 40 Store Program Memory Read
procedure TIMER4_CAPT_ISR; external name 'TIMER4_CAPT_ISR'; // Interrupt 41 Timer/Counter4 Capture Event
procedure TIMER4_COMPA_ISR; external name 'TIMER4_COMPA_ISR'; // Interrupt 42 Timer/Counter4 Compare Match A
procedure TIMER4_COMPB_ISR; external name 'TIMER4_COMPB_ISR'; // Interrupt 43 Timer/Counter4 Compare Match B
procedure TIMER4_COMPC_ISR; external name 'TIMER4_COMPC_ISR'; // Interrupt 44 Timer/Counter4 Compare Match C
procedure TIMER4_OVF_ISR; external name 'TIMER4_OVF_ISR'; // Interrupt 45 Timer/Counter4 Overflow
procedure TIMER5_CAPT_ISR; external name 'TIMER5_CAPT_ISR'; // Interrupt 46 Timer/Counter5 Capture Event
procedure TIMER5_COMPA_ISR; external name 'TIMER5_COMPA_ISR'; // Interrupt 47 Timer/Counter5 Compare Match A
procedure TIMER5_COMPB_ISR; external name 'TIMER5_COMPB_ISR'; // Interrupt 48 Timer/Counter5 Compare Match B
procedure TIMER5_COMPC_ISR; external name 'TIMER5_COMPC_ISR'; // Interrupt 49 Timer/Counter5 Compare Match C
procedure TIMER5_OVF_ISR; external name 'TIMER5_OVF_ISR'; // Interrupt 50 Timer/Counter5 Overflow
procedure USART2_RX_ISR; external name 'USART2_RX_ISR'; // Interrupt 51 USART2, Rx Complete
procedure USART2_UDRE_ISR; external name 'USART2_UDRE_ISR'; // Interrupt 52 USART2 Data register Empty
procedure USART2_TX_ISR; external name 'USART2_TX_ISR'; // Interrupt 53 USART2, Tx Complete
procedure USART3_RX_ISR; external name 'USART3_RX_ISR'; // Interrupt 54 USART3, Rx Complete
procedure USART3_UDRE_ISR; external name 'USART3_UDRE_ISR'; // Interrupt 55 USART3 Data register Empty
procedure USART3_TX_ISR; external name 'USART3_TX_ISR'; // Interrupt 56 USART3, Tx Complete
procedure TRX24_PLL_LOCK_ISR; external name 'TRX24_PLL_LOCK_ISR'; // Interrupt 57 TRX24 - PLL lock interrupt
procedure TRX24_PLL_UNLOCK_ISR; external name 'TRX24_PLL_UNLOCK_ISR'; // Interrupt 58 TRX24 - PLL unlock interrupt
procedure TRX24_RX_START_ISR; external name 'TRX24_RX_START_ISR'; // Interrupt 59 TRX24 - Receive start interrupt
procedure TRX24_RX_END_ISR; external name 'TRX24_RX_END_ISR'; // Interrupt 60 TRX24 - RX_END interrupt
procedure TRX24_CCA_ED_DONE_ISR; external name 'TRX24_CCA_ED_DONE_ISR'; // Interrupt 61 TRX24 - CCA/ED done interrupt
procedure TRX24_XAH_AMI_ISR; external name 'TRX24_XAH_AMI_ISR'; // Interrupt 62 TRX24 - XAH - AMI
procedure TRX24_TX_END_ISR; external name 'TRX24_TX_END_ISR'; // Interrupt 63 TRX24 - TX_END interrupt
procedure TRX24_AWAKE_ISR; external name 'TRX24_AWAKE_ISR'; // Interrupt 64 TRX24 AWAKE - tranceiver is reaching state TRX_OFF
procedure SCNT_CMP1_ISR; external name 'SCNT_CMP1_ISR'; // Interrupt 65 Symbol counter - compare match 1 interrupt
procedure SCNT_CMP2_ISR; external name 'SCNT_CMP2_ISR'; // Interrupt 66 Symbol counter - compare match 2 interrupt
procedure SCNT_CMP3_ISR; external name 'SCNT_CMP3_ISR'; // Interrupt 67 Symbol counter - compare match 3 interrupt
procedure SCNT_OVFL_ISR; external name 'SCNT_OVFL_ISR'; // Interrupt 68 Symbol counter - overflow interrupt
procedure SCNT_BACKOFF_ISR; external name 'SCNT_BACKOFF_ISR'; // Interrupt 69 Symbol counter - backoff interrupt
procedure AES_READY_ISR; external name 'AES_READY_ISR'; // Interrupt 70 AES engine ready interrupt
procedure BAT_LOW_ISR; external name 'BAT_LOW_ISR'; // Interrupt 71 Battery monitor indicates supply voltage below threshold

procedure _FPC_start; assembler; nostackframe;
label
   _start;
 asm
   .init
   .globl _start

   jmp _start
   jmp INT0_ISR
   jmp INT1_ISR
   jmp INT2_ISR
   jmp INT3_ISR
   jmp INT4_ISR
   jmp INT5_ISR
   jmp INT6_ISR
   jmp INT7_ISR
   jmp PCINT0_ISR
   jmp PCINT1_ISR
   jmp PCINT2_ISR
   jmp WDT_ISR
   jmp TIMER2_COMPA_ISR
   jmp TIMER2_COMPB_ISR
   jmp TIMER2_OVF_ISR
   jmp TIMER1_CAPT_ISR
   jmp TIMER1_COMPA_ISR
   jmp TIMER1_COMPB_ISR
   jmp TIMER1_COMPC_ISR
   jmp TIMER1_OVF_ISR
   jmp TIMER0_COMPA_ISR
   jmp TIMER0_COMPB_ISR
   jmp TIMER0_OVF_ISR
   jmp SPI_STC_ISR
   jmp USART0_RX_ISR
   jmp USART0_UDRE_ISR
   jmp USART0_TX_ISR
   jmp ANALOG_COMP_ISR
   jmp ADC_ISR
   jmp EE_READY_ISR
   jmp TIMER3_CAPT_ISR
   jmp TIMER3_COMPA_ISR
   jmp TIMER3_COMPB_ISR
   jmp TIMER3_COMPC_ISR
   jmp TIMER3_OVF_ISR
   jmp USART1_RX_ISR
   jmp USART1_UDRE_ISR
   jmp USART1_TX_ISR
   jmp TWI_ISR
   jmp SPM_READY_ISR
   jmp TIMER4_CAPT_ISR
   jmp TIMER4_COMPA_ISR
   jmp TIMER4_COMPB_ISR
   jmp TIMER4_COMPC_ISR
   jmp TIMER4_OVF_ISR
   jmp TIMER5_CAPT_ISR
   jmp TIMER5_COMPA_ISR
   jmp TIMER5_COMPB_ISR
   jmp TIMER5_COMPC_ISR
   jmp TIMER5_OVF_ISR
   jmp USART2_RX_ISR
   jmp USART2_UDRE_ISR
   jmp USART2_TX_ISR
   jmp USART3_RX_ISR
   jmp USART3_UDRE_ISR
   jmp USART3_TX_ISR
   jmp TRX24_PLL_LOCK_ISR
   jmp TRX24_PLL_UNLOCK_ISR
   jmp TRX24_RX_START_ISR
   jmp TRX24_RX_END_ISR
   jmp TRX24_CCA_ED_DONE_ISR
   jmp TRX24_XAH_AMI_ISR
   jmp TRX24_TX_END_ISR
   jmp TRX24_AWAKE_ISR
   jmp SCNT_CMP1_ISR
   jmp SCNT_CMP2_ISR
   jmp SCNT_CMP3_ISR
   jmp SCNT_OVFL_ISR
   jmp SCNT_BACKOFF_ISR
   jmp AES_READY_ISR
   jmp BAT_LOW_ISR

   {$i start.inc}

   .weak INT0_ISR
   .weak INT1_ISR
   .weak INT2_ISR
   .weak INT3_ISR
   .weak INT4_ISR
   .weak INT5_ISR
   .weak INT6_ISR
   .weak INT7_ISR
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
   .weak TIMER1_COMPC_ISR
   .weak TIMER1_OVF_ISR
   .weak TIMER0_COMPA_ISR
   .weak TIMER0_COMPB_ISR
   .weak TIMER0_OVF_ISR
   .weak SPI_STC_ISR
   .weak USART0_RX_ISR
   .weak USART0_UDRE_ISR
   .weak USART0_TX_ISR
   .weak ANALOG_COMP_ISR
   .weak ADC_ISR
   .weak EE_READY_ISR
   .weak TIMER3_CAPT_ISR
   .weak TIMER3_COMPA_ISR
   .weak TIMER3_COMPB_ISR
   .weak TIMER3_COMPC_ISR
   .weak TIMER3_OVF_ISR
   .weak USART1_RX_ISR
   .weak USART1_UDRE_ISR
   .weak USART1_TX_ISR
   .weak TWI_ISR
   .weak SPM_READY_ISR
   .weak TIMER4_CAPT_ISR
   .weak TIMER4_COMPA_ISR
   .weak TIMER4_COMPB_ISR
   .weak TIMER4_COMPC_ISR
   .weak TIMER4_OVF_ISR
   .weak TIMER5_CAPT_ISR
   .weak TIMER5_COMPA_ISR
   .weak TIMER5_COMPB_ISR
   .weak TIMER5_COMPC_ISR
   .weak TIMER5_OVF_ISR
   .weak USART2_RX_ISR
   .weak USART2_UDRE_ISR
   .weak USART2_TX_ISR
   .weak USART3_RX_ISR
   .weak USART3_UDRE_ISR
   .weak USART3_TX_ISR
   .weak TRX24_PLL_LOCK_ISR
   .weak TRX24_PLL_UNLOCK_ISR
   .weak TRX24_RX_START_ISR
   .weak TRX24_RX_END_ISR
   .weak TRX24_CCA_ED_DONE_ISR
   .weak TRX24_XAH_AMI_ISR
   .weak TRX24_TX_END_ISR
   .weak TRX24_AWAKE_ISR
   .weak SCNT_CMP1_ISR
   .weak SCNT_CMP2_ISR
   .weak SCNT_CMP3_ISR
   .weak SCNT_OVFL_ISR
   .weak SCNT_BACKOFF_ISR
   .weak AES_READY_ISR
   .weak BAT_LOW_ISR

   .set INT0_ISR, Default_IRQ_handler
   .set INT1_ISR, Default_IRQ_handler
   .set INT2_ISR, Default_IRQ_handler
   .set INT3_ISR, Default_IRQ_handler
   .set INT4_ISR, Default_IRQ_handler
   .set INT5_ISR, Default_IRQ_handler
   .set INT6_ISR, Default_IRQ_handler
   .set INT7_ISR, Default_IRQ_handler
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
   .set TIMER1_COMPC_ISR, Default_IRQ_handler
   .set TIMER1_OVF_ISR, Default_IRQ_handler
   .set TIMER0_COMPA_ISR, Default_IRQ_handler
   .set TIMER0_COMPB_ISR, Default_IRQ_handler
   .set TIMER0_OVF_ISR, Default_IRQ_handler
   .set SPI_STC_ISR, Default_IRQ_handler
   .set USART0_RX_ISR, Default_IRQ_handler
   .set USART0_UDRE_ISR, Default_IRQ_handler
   .set USART0_TX_ISR, Default_IRQ_handler
   .set ANALOG_COMP_ISR, Default_IRQ_handler
   .set ADC_ISR, Default_IRQ_handler
   .set EE_READY_ISR, Default_IRQ_handler
   .set TIMER3_CAPT_ISR, Default_IRQ_handler
   .set TIMER3_COMPA_ISR, Default_IRQ_handler
   .set TIMER3_COMPB_ISR, Default_IRQ_handler
   .set TIMER3_COMPC_ISR, Default_IRQ_handler
   .set TIMER3_OVF_ISR, Default_IRQ_handler
   .set USART1_RX_ISR, Default_IRQ_handler
   .set USART1_UDRE_ISR, Default_IRQ_handler
   .set USART1_TX_ISR, Default_IRQ_handler
   .set TWI_ISR, Default_IRQ_handler
   .set SPM_READY_ISR, Default_IRQ_handler
   .set TIMER4_CAPT_ISR, Default_IRQ_handler
   .set TIMER4_COMPA_ISR, Default_IRQ_handler
   .set TIMER4_COMPB_ISR, Default_IRQ_handler
   .set TIMER4_COMPC_ISR, Default_IRQ_handler
   .set TIMER4_OVF_ISR, Default_IRQ_handler
   .set TIMER5_CAPT_ISR, Default_IRQ_handler
   .set TIMER5_COMPA_ISR, Default_IRQ_handler
   .set TIMER5_COMPB_ISR, Default_IRQ_handler
   .set TIMER5_COMPC_ISR, Default_IRQ_handler
   .set TIMER5_OVF_ISR, Default_IRQ_handler
   .set USART2_RX_ISR, Default_IRQ_handler
   .set USART2_UDRE_ISR, Default_IRQ_handler
   .set USART2_TX_ISR, Default_IRQ_handler
   .set USART3_RX_ISR, Default_IRQ_handler
   .set USART3_UDRE_ISR, Default_IRQ_handler
   .set USART3_TX_ISR, Default_IRQ_handler
   .set TRX24_PLL_LOCK_ISR, Default_IRQ_handler
   .set TRX24_PLL_UNLOCK_ISR, Default_IRQ_handler
   .set TRX24_RX_START_ISR, Default_IRQ_handler
   .set TRX24_RX_END_ISR, Default_IRQ_handler
   .set TRX24_CCA_ED_DONE_ISR, Default_IRQ_handler
   .set TRX24_XAH_AMI_ISR, Default_IRQ_handler
   .set TRX24_TX_END_ISR, Default_IRQ_handler
   .set TRX24_AWAKE_ISR, Default_IRQ_handler
   .set SCNT_CMP1_ISR, Default_IRQ_handler
   .set SCNT_CMP2_ISR, Default_IRQ_handler
   .set SCNT_CMP3_ISR, Default_IRQ_handler
   .set SCNT_OVFL_ISR, Default_IRQ_handler
   .set SCNT_BACKOFF_ISR, Default_IRQ_handler
   .set AES_READY_ISR, Default_IRQ_handler
   .set BAT_LOW_ISR, Default_IRQ_handler
 end;

end.
