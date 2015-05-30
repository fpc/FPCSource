{******************************************************************************
Register definitions and startup code for ATMEL ATmega48/88

******************************************************************************}
unit atmega48fam;

{$goto on}

  interface

    var
      PINB    : byte absolute $23;
      DDRB    : byte absolute $24;
      PORTB   : byte absolute $25;
      PINC    : byte absolute $26;
      DDRC    : byte absolute $27;
      PORTC   : byte absolute $28;
      PIND    : byte absolute $29;
      DDRD    : byte absolute $2A;
      PORTD   : byte absolute $2B;
      TIFR0   : byte absolute $35;
      TIFR1   : byte absolute $36;
      PCIFR   : byte absolute $3B;
      EIFR    : byte absolute $3C;
      EIMSK   : byte absolute $3D;
      GPIOR0  : byte absolute $3E;
      EECR    : byte absolute $3F;
      EEDR    : byte absolute $40;
      EEARL   : byte absolute $41;
      EEARH   : byte absolute $42;
      EEAR    : word absolute $41;
      GTCCR   : byte absolute $43;
      TCCR0A  : byte absolute $44;
      TCCR0B  : byte absolute $45;
      TCNT0   : byte absolute $46;
      OCR0A   : byte absolute $47;
      OCR0B   : byte absolute $48;
      GPIOR1  : byte absolute $4A;
      GPIOR2  : byte absolute $4B;
      SPCR    : byte absolute $4C;
      SPSR    : byte absolute $4D;
      SPDR    : byte absolute $4E;
      ACSR    : byte absolute $50;
      SMCR    : byte absolute $53;
      MCUSR   : byte absolute $54;
      MCUCR   : byte absolute $55;
      SPMCSR  : byte absolute $57;
      SPL     : byte absolute $5D;
      SPH     : byte absolute $5E;
      SP      : word absolute $5D;
      SREG    : byte absolute $5F;
      WDTCSR  : byte absolute $60;
      CLKPR   : byte absolute $61;
      PRR     : byte absolute $64;
      OSCCAL  : byte absolute $66;
      PCICR   : byte absolute $68;
      EICRA   : byte absolute $69;
      PCMSK0  : byte absolute $6B;
      PCMSK1  : byte absolute $6C;
      PCMSK2  : byte absolute $6D;
      TIMSK0  : byte absolute $6E;
      TIMSK1  : byte absolute $6F;
      TIMSK2  : byte absolute $70;
      ADCL    : byte absolute $78;
      ADCH    : byte absolute $79;
      ADC     : word absolute $78;
      ADCSRA  : byte absolute $7A;
      ADCSRB  : byte absolute $7B;
      ADMUX   : byte absolute $7C;
      DIDR0   : byte absolute $7E;
      DIDR1   : byte absolute $7F;
      TCCR1A  : byte absolute $80;
      TCCR1B  : byte absolute $81;
      TCCR1C  : byte absolute $82;
      TCNT1L  : byte absolute $84;
      TCNT1H  : byte absolute $85;
      TCNT1   : word absolute $84;
      ICRL    : byte absolute $86;
      ICR1H   : byte absolute $87;
      OCR1AL  : byte absolute $88;
      OCR1AH  : byte absolute $89;
      OCR1A   : word absolute $88;
      OCR1BL  : byte absolute $8A;
      OCR1BH  : byte absolute $8B;
      OCR1B   : word absolute $8A;
      TCCR2A  : byte absolute $B0;
      TCCR2B  : byte absolute $B1;
      TCNT2   : byte absolute $B2;
      OCR2A   : byte absolute $B3;
      OCR2B   : byte absolute $B4;
      ASSR    : byte absolute $B6;
      TWBR    : byte absolute $B8;
      TWSR    : byte absolute $B9;
      TWAR    : byte absolute $BA;
      TWDR    : byte absolute $BB;
      TWCR    : byte absolute $BC;
      TWAMR   : byte absolute $BD;
      UCSR0A  : byte absolute $C0;
      UCSR0B  : byte absolute $C1;
      UCSR0C  : byte absolute $C2;
      UBRR0L  : byte absolute $C4;
      UBRR0H  : byte absolute $C5;
      UBRR0   : word absolute $C4;
      UDR0    : byte absolute $C6;

    const
      { PINB }
      PINB7    = 7;
      PINB6    = 6;
      PINB5    = 5;
      PINB4    = 4;
      PINB3    = 3;
      PINB2    = 2;
      PINB1    = 1;
      PINB0    = 0;

      { DDRB }
      DDB7     = 7;
      DDB6     = 6;
      DDB5     = 5;
      DDB4     = 4;
      DDB3     = 3;
      DDB2     = 2;
      DDB1     = 1;
      DDB0     = 0;

      { PORTB }
      PORTB7   = 7;
      PORTB6   = 6;
      PORTB5   = 5;
      PORTB4   = 4;
      PORTB3   = 3;
      PORTB2   = 2;
      PORTB1   = 1;
      PORTB0   = 0;

      { PINC }
      PINC6    = 6;
      PINC5    = 5;
      PINC4    = 4;
      PINC3    = 3;
      PINC2    = 2;
      PINC1    = 1;
      PINC0    = 0;

      { DDRC }
      DDC6     = 6;
      DDC5     = 5;
      DDC4     = 4;
      DDC3     = 3;
      DDC2     = 2;
      DDC1     = 1;
      DDC0     = 0;

      { PORTC }
      PORTC6   = 6;
      PORTC5   = 5;
      PORTC4   = 4;
      PORTC3   = 3;
      PORTC2   = 2;
      PORTC1   = 1;
      PORTC0   = 0;

      { PIND }
      PIND7    = 7;
      PIND6    = 6;
      PIND5    = 5;
      PIND4    = 4;
      PIND3    = 3;
      PIND2    = 2;
      PIND1    = 1;
      PIND0    = 0;

      { DDRD }
      DDD7     = 7;
      DDD6     = 6;
      DDD5     = 5;
      DDD4     = 4;
      DDD3     = 3;
      DDD2     = 2;
      DDD1     = 1;
      DDD0     = 0;

      { PORTD }
      PORTD7   = 7;
      PORTD6   = 6;
      PORTD5   = 5;
      PORTD4   = 4;
      PORTD3   = 3;
      PORTD2   = 2;
      PORTD1   = 1;
      PORTD0   = 0;

      { TIFR0 }
      OCF0B    = 2;
      OCF0A    = 1;
      TOV0     = 0;

      { TIFR1 }
      ICF1     = 5;
      OCF1B    = 2;
      OCF1A    = 1;
      TOV1     = 0;

      { PCIFR }
      PCIF2    = 2;
      PCIF1    = 1;
      PCIF0    = 0;

      { EIFR }
      INTF1    = 1;
      INTF0    = 0;

      { EIMSK }
      INT1     = 1;
      INT0     = 0;

      { EECR }
      EEPM1    = 5;
      EEPM0    = 4;
      EERIE    = 3;
      EEMPE    = 2;
      EEPE     = 1;
      EERE     = 0;

      { GTCCR }
      TSM      = 7;
      PSRASY   = 1;
      PSRSYNC  = 0;

      { TCCR0A }
      COM0A1   = 7;
      COM0A0   = 6;
      COM0B1   = 5;
      COM0B0   = 4;
      WGM01    = 1;
      WGM00    = 0;

      { TCCR0B }
      FOC0A    = 7;
      FOC0B    = 6;
      WGM02    = 3;
      CS02     = 2;
      CS01     = 1;
      CS00     = 0;

      { SPCR }
      SPIE     = 7;
      SPE      = 6;
      DORD     = 5;
      MSTR     = 4;
      CPOL     = 3;
      CPHA     = 2;
      SPR1     = 1;
      SPR0     = 0;

      { SPSR }
      SPIF     = 7;
      WCOL     = 6;
      SPI2X    = 0;

      { ACSR }
      ACD      = 7;
      ACBG     = 6;
      ACO      = 5;
      ACI      = 4;
      ACIE     = 3;
      ACIC     = 2;
      ACIS1    = 1;
      ACIS0    = 0;

      { SMCR }
      SM2      = 3;
      SM1      = 2;
      SM0      = 1;
      SE       = 0;

      { MCUSR }
      WDRF     = 3;
      BORF     = 2;
      EXTRF    = 1;
      PORF     = 0;

      { MCUCR }
      PUD      = 4;
      IVSEL    = 1;
      IVCE     = 0;

      { SPMCSR }
      SPMIE    = 7;
      RWWSB    = 6;
      RWWSRE   = 4;
      BLBSET   = 3;
      PGWRT    = 2;
      PGERS    = 1;
      SPMEN    = 0;

      { WDTCSR }
      WDIF     = 7;
      WDIE     = 6;
      WDP3     = 5;
      WDCE     = 4;
      WDE      = 3;
      WDP2     = 2;
      WDP1     = 1;
      WDP0     = 0;

      { CLKPR }
      CLKPCE   = 7;
      CLKPS3   = 3;
      CLKPS2   = 2;
      CLKPS1   = 1;
      CLKPS0   = 0;

      { PRR }
      PRTWI    = 7;
      PRTIM2   = 6;
      PRTIM0   = 5;
      PRTIM1   = 3;
      PRSPI    = 2;
      PRUSART0 = 1;
      PRADC    = 0;

      { PCICR }
      PCIE2    = 2;
      PCIE1    = 1;
      PCIE0    = 0;

      { EICRA }
      ISC11    = 3;
      ISC10    = 2;
      ISC01    = 1;
      ISC00    = 0;

      { PCMSK0 }
      PCINT7   = 7;
      PCINT6   = 6;
      PCINT5   = 5;
      PCINT4   = 4;
      PCINT3   = 3;
      PCINT2   = 2;
      PCINT1   = 1;
      PCINT0   = 0;

      { PCMSK1 }
      PCINT14  = 6;
      PCINT13  = 5;
      PCINT12  = 4;
      PCINT11  = 3;
      PCINT10  = 2;
      PCINT9   = 1;
      PCINT8   = 0;

      { PCMSK2 }
      PCINT23  = 7;
      PCINT22  = 6;
      PCINT21  = 5;
      PCINT20  = 4;
      PCINT19  = 3;
      PCINT18  = 2;
      PCINT17  = 1;
      PCINT16  = 0;

      { TIMSK0 }
      OCIE0B   = 2;
      OCIE0A   = 1;
      TOIE0    = 0;

      { TIMSK1 }
      ICIE1    = 5;
      OCIE1B   = 2;
      OCIE1A   = 1;
      TOIE1    = 0;

      { TIMSK2 }
      OCIE2B   = 2;
      OCIE2A   = 1;
      TOIE2    = 0;

      { ADCSRA }
      ADEN     = 7;
      ADSC     = 6;
      ADATE    = 5;
      ADIF     = 4;
      ADIE     = 3;
      ADPS2    = 2;
      ADPS1    = 1;
      ADPS0    = 0;

      { ADCSRB }
      ACME     = 6;
      ADTS2    = 2;
      ADTS1    = 1;
      ADTS0    = 0;

      { ADMUX }
      REFS1    = 7;
      REFS0    = 6;
      ADLAR    = 5;
      MUX3     = 3;
      MUX2     = 2;
      MUX1     = 1;
      MUX0     = 0;

      { DIDR0 }
      ADC5D    = 5;
      ADC4D    = 4;
      ADC3D    = 3;
      ADC2D    = 2;
      ADC1D    = 1;
      ADC0D    = 0;

      { DIDR1 }
      AIN1D    = 1;
      AIN0D    = 0;

      { TCCR1A }
      COM1A1   = 7;
      COM1A0   = 6;
      COM1B1   = 5;
      COM1B0   = 4;
      WGM11    = 1;
      WGM10    = 0;

      { TCCR1B }
      ICNC1    = 7;
      ICES1    = 6;
      WGM13    = 4;
      WGM12    = 3;
      CS12     = 2;
      CS11     = 1;
      CS10     = 0;

      { TCCR1C }
      FOC1A    = 7;
      FOC1B    = 6;

      { TCCR2A }
      COM2A1   = 7;
      COM2A0   = 6;
      COM2B1   = 5;
      COM2B0   = 4;
      WGM21    = 1;
      WGM20    = 0;

      { TCCR2B }
      FOC2A    = 7;
      FOC2B    = 6;
      WGM22    = 3;
      CS22     = 2;
      CS21     = 1;
      CS20     = 0;

      { ASSR }
      EXCLK    = 6;
      AS2      = 5;
      TCN2UB   = 4;
      OCR2AUB  = 3;
      OCR2BUB  = 2;
      TCR2AUB  = 1;
      TCR2BUB  = 0;

      { TWSR }
      TWS7     = 7;
      TWS6     = 6;
      TWS5     = 5;
      TWS4     = 4;
      TWS3     = 3;
      TWPS1    = 1;
      TWPS0    = 0;

      { TWAR }
      TWA6     = 7;
      TWA5     = 6;
      TWA4     = 5;
      TWA3     = 4;
      TWA2     = 3;
      TWA1     = 2;
      TWA0     = 1;

      { TWCR }
      TWINT    = 7;
      TWEA     = 6;
      TWSTA    = 5;
      TWSTO    = 4;
      TWWC     = 3;
      TWEN     = 2;
      TWIE     = 0;

      { TWAMR }
      TWAM6    = 7;
      TWAM5    = 6;
      TWAM4    = 5;
      TWAM3    = 4;
      TWAM2    = 3;
      TWAM1    = 2;
      TWAM0    = 1;

      { UCSR0A }
      RXC0     = 7;
      TXC0     = 6;
      UDRE0    = 5;
      FE0      = 4;
      DOR0     = 3;
      UPE0     = 2;
      U2X0     = 1;
      MPCM0    = 0;

      { UCSR0B }
      RXCIE0   = 7;
      TXCIE0   = 6;
      UDRIE0   = 5;
      RXEN0    = 4;
      TXEN0    = 3;
      UCVSZ02  = 2;
      RXB80    = 1;
      TXB80    = 0;

      { UCSR0C }
      UMSEL01  = 7;
      UMSEL00  = 6;
      UPM01    = 5;
      UPM00    = 4;
      USBS0    = 3;
      UCVSZ01  = 2;
      UCVSZ00  = 1;
      UCPOL0   = 0;

  implementation

{$define RELBRANCHES}

{$i avrcommon.inc}
	
    procedure Int00Handler; external name 'Int00Handler';
    procedure Int01Handler; external name 'Int01Handler';
    procedure Int02Handler; external name 'Int02Handler';
    procedure Int03Handler; external name 'Int03Handler';
    procedure Int04Handler; external name 'Int04Handler';
    procedure Int05Handler; external name 'Int05Handler';
    procedure Int06Handler; external name 'Int06Handler';
    procedure Int07Handler; external name 'Int07Handler';
    procedure Int08Handler; external name 'Int08Handler';
    procedure Int09Handler; external name 'Int09Handler';
    procedure Int10Handler; external name 'Int10Handler';
    procedure Int11Handler; external name 'Int11Handler';
    procedure Int12Handler; external name 'Int12Handler';
    procedure Int13Handler; external name 'Int13Handler';
    procedure Int14Handler; external name 'Int14Handler';
    procedure Int15Handler; external name 'Int15Handler';
    procedure Int16Handler; external name 'Int16Handler';
    procedure Int17Handler; external name 'Int17Handler';
    procedure Int18Handler; external name 'Int18Handler';
    procedure Int19Handler; external name 'Int19Handler';
    procedure Int20Handler; external name 'Int20Handler';
    procedure Int21Handler; external name 'Int21Handler';
    procedure Int22Handler; external name 'Int22Handler';
    procedure Int23Handler; external name 'Int23Handler';
    procedure Int24Handler; external name 'Int24Handler';
    procedure Int25Handler; external name 'Int25Handler';
    procedure Int26Handler; external name 'Int26Handler';
    procedure Int27Handler; external name 'Int27Handler';
    procedure Int28Handler; external name 'Int28Handler';
    procedure Int29Handler; external name 'Int29Handler';
    procedure Int30Handler; external name 'Int30Handler';
    procedure Int31Handler; external name 'Int31Handler';
    procedure Int32Handler; external name 'Int32Handler';
    procedure Int33Handler; external name 'Int33Handler';
    procedure Int34Handler; external name 'Int34Handler';

    procedure _FPC_start; assembler; nostackframe;
      label
        _start;
      asm
        .init
        .globl _start

        rjmp _start
        rjmp Int00Handler
        rjmp Int01Handler
        rjmp Int02Handler
        rjmp Int03Handler
        rjmp Int04Handler
        rjmp Int05Handler
        rjmp Int06Handler
        rjmp Int07Handler
        rjmp Int08Handler
        rjmp Int09Handler
        rjmp Int10Handler
        rjmp Int11Handler
        rjmp Int12Handler
        rjmp Int13Handler
        rjmp Int14Handler
        rjmp Int15Handler
        rjmp Int16Handler
        rjmp Int17Handler
        rjmp Int18Handler
        rjmp Int19Handler
        rjmp Int20Handler
        rjmp Int21Handler
        rjmp Int22Handler
        rjmp Int23Handler
        rjmp Int24Handler

        {
          all ATMEL MCUs use the same startup code, the details are
          governed by defines
        }
        {$i start.inc}

        .weak Int00Handler
        .weak Int01Handler
        .weak Int02Handler
        .weak Int03Handler
        .weak Int04Handler
        .weak Int05Handler
        .weak Int06Handler
        .weak Int07Handler
        .weak Int08Handler
        .weak Int09Handler
        .weak Int10Handler
        .weak Int11Handler
        .weak Int12Handler
        .weak Int13Handler
        .weak Int14Handler
        .weak Int15Handler
        .weak Int16Handler
        .weak Int17Handler
        .weak Int18Handler
        .weak Int19Handler
        .weak Int20Handler
        .weak Int21Handler
        .weak Int22Handler
        .weak Int23Handler
        .weak Int24Handler

        .set Int00Handler, Default_IRQ_handler
        .set Int01Handler, Default_IRQ_handler
        .set Int02Handler, Default_IRQ_handler
        .set Int03Handler, Default_IRQ_handler
        .set Int04Handler, Default_IRQ_handler
        .set Int05Handler, Default_IRQ_handler
        .set Int06Handler, Default_IRQ_handler
        .set Int07Handler, Default_IRQ_handler
        .set Int08Handler, Default_IRQ_handler
        .set Int09Handler, Default_IRQ_handler
        .set Int10Handler, Default_IRQ_handler
        .set Int11Handler, Default_IRQ_handler
        .set Int12Handler, Default_IRQ_handler
        .set Int13Handler, Default_IRQ_handler
        .set Int14Handler, Default_IRQ_handler
        .set Int15Handler, Default_IRQ_handler
        .set Int16Handler, Default_IRQ_handler
        .set Int17Handler, Default_IRQ_handler
        .set Int18Handler, Default_IRQ_handler
        .set Int19Handler, Default_IRQ_handler
        .set Int20Handler, Default_IRQ_handler
        .set Int21Handler, Default_IRQ_handler
        .set Int22Handler, Default_IRQ_handler
        .set Int23Handler, Default_IRQ_handler
        .set Int24Handler, Default_IRQ_handler
      end;

end.
