{******************************************************************************
Register definitions and startup code for ATMEL ATtiny 24/44/84

******************************************************************************}
unit attinyx4;

{$goto on}

  interface
    var
      PRR     : byte absolute $20;
      DIDR0   : byte absolute $21;
      ADCSRB  : byte absolute $23;
      ADCL    : byte absolute $24;
      ADCH    : byte absolute $25;
      ADC     : word absolute $24;
      ADCSRA  : byte absolute $26;
      ADMUX   : byte absolute $27;
      ACSR    : byte absolute $28;
      TIFR1   : byte absolute $2B;
      TIMSK1  : byte absolute $2C;
      USICR   : byte absolute $2D;
      USISR   : byte absolute $2E;
      USIDR   : byte absolute $2F;
      USIBR   : byte absolute $30;
      PCMSK0  : byte absolute $32;
      GPIOR0  : byte absolute $33;
      GPIOR1  : byte absolute $34;
      GPIOR2  : byte absolute $35;
      PINB    : byte absolute $36;
      DDRB    : byte absolute $37;
      PORTB   : byte absolute $38;
      PINA    : byte absolute $39;
      DDRA    : byte absolute $3A;
      PORTA   : byte absolute $3B;
      EECR    : byte absolute $3C;
      EEDR    : byte absolute $3D;
      EEARL   : byte absolute $3E;
      EEARH   : byte absolute $3F;
      EEAR    : word absolute $3E;
      PCMSK1  : byte absolute $40;
      WDTCSR  : byte absolute $41;
      TCCR1C  : byte absolute $42;
      GTCCR   : byte absolute $43;
      ICR1L   : byte absolute $44;
      ICR1H   : byte absolute $45;
      ICR1    : word absolute $44;
      CLKPR   : byte absolute $46;
      DWDR    : byte absolute $47;
      OCR1BL  : byte absolute $48;
      OCR1BH  : byte absolute $49;
      OCR1B   : word absolute $48;
      OCR1AL  : byte absolute $4A;
      OCR1AH  : byte absolute $4B;
      OCR1A   : word absolute $4A;
      TCNT1L  : byte absolute $4C;
      TCNT1H  : byte absolute $4D;
      TCNT1   : word absolute $4C;
      TCCR1B  : byte absolute $4E;
      TCCR1A  : byte absolute $4F;
      TCCR0A  : byte absolute $50;
      OSCCAL  : byte absolute $51;
      TCNT0   : byte absolute $52;
      TCCR0B  : byte absolute $53;
      MCUSR   : byte absolute $54;
      MCUCR   : byte absolute $55;
      OCR0A   : byte absolute $56;
      SPMCSR  : byte absolute $57;
      TIFR0   : byte absolute $58;
      TIMSK0  : byte absolute $59;
      GIFR    : byte absolute $5A;
      GIMSK   : byte absolute $5B;
      OCR0B   : byte absolute $5C;
      SPL     : byte absolute $5D;
      SPH     : byte absolute $5E;
      SP      : word absolute $5D;
      SREG    : byte absolute $5F;

    const
      { PRR }
      PRTIM1   = 3;
      PRTIM0   = 2;
      PRUSI    = 1;
      PRADC    = 0;

      { DIDR0 }
      ADC7D    = 7;
      ADC6D    = 6;
      ADC5D    = 5;
      ADC4D    = 4;
      ADC3D    = 3;
      ADC2D    = 2;
      ADC1D    = 1;
      ADC0D    = 0;

      { ADCSRB }
      BIN      = 7;
      ACME     = 6;
      ADLAR    = 4;
      ADTS2    = 2;
      ADTS1    = 1;
      ADTS0    = 0;

      { ADCSRA }
      ADEN     = 7;
      ADSC     = 6;
      ADATE    = 5;
      ADIF     = 4;
      ADIE     = 3;
      ADPS2    = 2;
      ADPS1    = 1;
      ADPS0    = 0;

      { ADMUX }
      REFS1    = 7;
      REFS0    = 6;
      MUX5     = 5;
      MUX4     = 4;
      MUX3     = 3;
      MUX2     = 2;
      MUX1     = 1;
      MUX0     = 0;

      { ACSR }
      ACD      = 7;
      ACBG     = 6;
      ACO      = 5;
      ACI      = 4;
      ACIE     = 3;
      ACIC     = 2;
      ACIS1    = 1;
      ACIS0    = 0;

      { TIFR1 }
      ICF1     = 5;
      OCF1B    = 2;
      OCF1A    = 1;
      TOV1     = 0;

      { TIMSK1 }
      ICIE1    = 5;
      OCIE1B   = 2;
      OCIE1A   = 1;
      TOIE1    = 0;

      { USICR }
      USISIE   = 7;
      USIOIE   = 6;
      USIWM1   = 5;
      USIWM0   = 4;
      USICS1   = 3;
      USICS0   = 2;
      USICLK   = 1;
      USITC    = 0;

      { USISR }
      USISIF   = 7;
      USIOIF   = 6;
      USIPF    = 5;
      USIDC    = 4;
      USICNT3  = 3;
      USICNT2  = 2;
      USICNT1  = 1;
      USICNT0  = 0;

      { PCMSK0 }
      PCINT7   = 7;
      PCINT6   = 6;
      PCINT5   = 5;
      PCINT4   = 4;
      PCINT3   = 3;
      PCINT2   = 2;
      PCINT1   = 1;
      PCINT0   = 0;

      { PINB }
      PINB3    = 3;
      PINB2    = 2;
      PINB1    = 1;
      PINB0    = 0;

      { DDRB }
      DDB3     = 3;
      DDB2     = 2;
      DDB1     = 1;
      DDB0     = 0;

      { PORTB }
      PORTB3   = 3;
      PORTB2   = 2;
      PORTB1   = 1;
      PORTB0   = 0;

      { PINA }
      PINA7    = 7;
      PINA6    = 6;
      PINA5    = 5;
      PINA4    = 4;
      PINA3    = 3;
      PINA2    = 2;
      PINA1    = 1;
      PINA0    = 0;

      { DDRA }
      DDA7     = 7;
      DDA6     = 6;
      DDA5     = 5;
      DDA4     = 4;
      DDA3     = 3;
      DDA2     = 2;
      DDA1     = 1;
      DDA0     = 0;

      { PORTA }
      PORTA7   = 7;
      PORTA6   = 6;
      PORTA5   = 5;
      PORTA4   = 4;
      PORTA3   = 3;
      PORTA2   = 2;
      PORTA1   = 1;
      PORTA0   = 0;

      { EECR }
      EEPM1    = 5;
      EEPM0    = 4;
      EERIE    = 3;
      EEMPE    = 2;
      EEPE     = 1;
      EERE     = 0;

      { EEARL }
      EEAR7    = 7;
      EEAR6    = 6;
      EEAR5    = 5;
      EEAR4    = 4;
      EEAR3    = 3;
      EEAR2    = 2;
      EEAR1    = 1;
      EEAR0    = 0;

      { EEARH }
      EEAR8    = 0;

      { PCMSK1 }
      PCINT11  = 3;
      PCINT10  = 2;
      PCINT9   = 1;
      PCINT8   = 0;

      { WDTCSR }
      WDIF     = 7;
      WDIE     = 6;
      WDP3     = 5;
      WDCE     = 4;
      WDE      = 3;
      WDP2     = 2;
      WDP1     = 1;
      WDP0     = 0;

      { TCCR1C }
      FOC1A    = 7;
      FOC1B    = 6;

      { GTCCR }
      TSM      = 7;
      PSR10    = 0;

      { CLKPR }
      CLKPCE   = 7;
      CLKPS3   = 3;
      CLKPS2   = 2;
      CLKPS1   = 1;
      CLKPS0   = 0;

      { TCCR1B }
      ICNC1    = 7;
      ICES1    = 6;
      WGM13    = 4;
      WGM12    = 3;
      CS12     = 2;
      CS11     = 1;
      CS10     = 0;

      { TCCR1A }
      COM1A1   = 7;
      COM1A0   = 6;
      COM1B1   = 5;
      COM1B0   = 4;
      WGM11    = 1;
      WGM10    = 0;

      { TCCR0A }
      COM0A1   = 7;
      COM0A0   = 6;
      COM0B1   = 5;
      COM0B0   = 4;
      WGM01    = 1;
      WGM00    = 0;

      { OSCCAL }
      CAL7     = 7;
      CAL6     = 6;
      CAL5     = 5;
      CAL4     = 4;
      CAL3     = 3;
      CAL2     = 2;
      CAL1     = 1;
      CAL0     = 0;

      { TCCR0B }
      FOC0A    = 7;
      FOC0B    = 6;
      WGM02    = 3;
      CS02     = 2;
      CS01     = 1;
      CS00     = 0;

      { MCUSR }
      WDRF     = 3;
      BORF     = 2;
      EXTRF    = 1;
      PORF     = 0;

      { MCUCR }
      BODS     = 7;
      PUD      = 6;
      SE       = 5;
      SM1      = 4;
      SM0      = 3;
      BODSE    = 2;
      ISC01    = 1;
      ISC00    = 0;

      { SPMCSR }
      RSIG     = 5;
      CTPB     = 4;
      RFLB     = 3;
      PGWRT    = 2;
      PGERS    = 1;
      SPMEN    = 0;

      { TIFR0 }
      OCF0B    = 2;
      OCF0A    = 1;
      TOV0     = 0;

      { TIMSK0 }
      OCIE0B   = 2;
      OCIE0A   = 1;
      TOIE0    = 0;

      { GIFR }
      INTF0    = 6;
      PCIF1    = 5;
      PCIF0    = 4;

      { GIMSK }
      INT0     = 6;
      PCIE1    = 5;
      PCIE0    = 4;

    {$define RELBRANCHES}

  implementation

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
      end;

end.
