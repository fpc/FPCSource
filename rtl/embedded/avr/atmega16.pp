{******************************************************************************
Register definitions and startup code for ATMEL ATmega16

******************************************************************************}
unit atmega16;

{$goto on}

  interface
    const
      _SFR_OFFSET = $20; //indirect addressing
    var
      TWBR    : byte absolute $00+_SFR_OFFSET;
      TWSR    : byte absolute $01+_SFR_OFFSET;
      TWAR    : byte absolute $02+_SFR_OFFSET;
      TWDR    : byte absolute $03+_SFR_OFFSET;
      ADCW    : word absolute $04+_SFR_OFFSET;
      ADC     : word absolute $04+_SFR_OFFSET;
      ADCL    : byte absolute $04+_SFR_OFFSET;
      ADCH    : byte absolute $05+_SFR_OFFSET;
      ADCSRA  : byte absolute $06+_SFR_OFFSET;
      ADMUX   : byte absolute $07+_SFR_OFFSET;
      ACSR    : byte absolute $08+_SFR_OFFSET;
      UBRRL   : byte absolute $09+_SFR_OFFSET;
      UCSRB   : byte absolute $0A+_SFR_OFFSET;
      UCSRA   : byte absolute $0B+_SFR_OFFSET;
      UDR     : byte absolute $0C+_SFR_OFFSET;
      SPCR    : byte absolute $0D+_SFR_OFFSET;
      SPSR    : byte absolute $0E+_SFR_OFFSET;
      SPDR    : byte absolute $0F+_SFR_OFFSET;
      PIND    : byte absolute $10+_SFR_OFFSET;
      DDRD    : byte absolute $11+_SFR_OFFSET;
      PORTD   : byte absolute $12+_SFR_OFFSET;
      PINC    : byte absolute $13+_SFR_OFFSET;
      DDRC    : byte absolute $14+_SFR_OFFSET;
      PORTC   : byte absolute $15+_SFR_OFFSET;
      PINB    : byte absolute $16+_SFR_OFFSET;
      DDRB    : byte absolute $17+_SFR_OFFSET;
      PORTB   : byte absolute $18+_SFR_OFFSET;
      PINA    : byte absolute $19+_SFR_OFFSET;
      DDRA    : byte absolute $1A+_SFR_OFFSET;
      PORTA   : byte absolute $1B+_SFR_OFFSET;
      EECR    : byte absolute $1C+_SFR_OFFSET;
      EEDR    : byte absolute $1D+_SFR_OFFSET;
      EEAR    : word absolute $1E+_SFR_OFFSET;
      EEARL   : byte absolute $1E+_SFR_OFFSET;
      EEARH   : byte absolute $1F+_SFR_OFFSET;
      UCSRC   : byte absolute $20+_SFR_OFFSET;
      UBRRH   : byte absolute $20+_SFR_OFFSET;
      WDTCR   : byte absolute $21+_SFR_OFFSET;
      ASSR    : byte absolute $22+_SFR_OFFSET;
      OCR2    : byte absolute $23+_SFR_OFFSET;
      TCNT2   : byte absolute $24+_SFR_OFFSET;
      TCCR2   : byte absolute $25+_SFR_OFFSET;
      ICR1    : word absolute $26+_SFR_OFFSET;
      ICR1L   : byte absolute $26+_SFR_OFFSET;
      ICR1H   : byte absolute $27+_SFR_OFFSET;
      OCR1B   : word absolute $28+_SFR_OFFSET;
      OCR1BL  : byte absolute $28+_SFR_OFFSET;
      OCR1BH  : byte absolute $29+_SFR_OFFSET;
      OCR1A   : word absolute $2A+_SFR_OFFSET;
      OCR1AL  : byte absolute $2A+_SFR_OFFSET;
      OCR1AH  : byte absolute $2B+_SFR_OFFSET;
      TCNT1   : word absolute $2C+_SFR_OFFSET;
      TCNT1L  : byte absolute $2C+_SFR_OFFSET;
      TCNT1H  : byte absolute $2D+_SFR_OFFSET;
      TCCR1B  : byte absolute $2E+_SFR_OFFSET;
      TCCR1A  : byte absolute $2F+_SFR_OFFSET;
      SFIOR   : byte absolute $30+_SFR_OFFSET;
      OSCCAL  : byte absolute $31+_SFR_OFFSET;
      TCNT0   : byte absolute $32+_SFR_OFFSET;
      TCCR0   : byte absolute $33+_SFR_OFFSET;
      MCUSR   : byte absolute $34+_SFR_OFFSET;
      MCUCSR  : byte absolute $34+_SFR_OFFSET;
      MCUCR   : byte absolute $35+_SFR_OFFSET;
      TWCR    : byte absolute $36+_SFR_OFFSET;
      SPMCR   : byte absolute $37+_SFR_OFFSET;
      TIFR    : byte absolute $38+_SFR_OFFSET;
      TIMSK   : byte absolute $39+_SFR_OFFSET;
      GIFR    : byte absolute $3A+_SFR_OFFSET;
      GICR    : byte absolute $3B+_SFR_OFFSET;
      OCR0    : byte absolute $3C+_SFR_OFFSET;
      SP      : word absolute $3D+_SFR_OFFSET;
      SPL     : byte absolute $3D+_SFR_OFFSET;
      SPH     : byte absolute $3E+_SFR_OFFSET;
      SREG    : byte absolute $3F+_SFR_OFFSET;

    const
       TWINT =  7;
       TWEA =  6;
       TWSTA = 5;
       TWSTO = 4;
       TWWC =  3;
       TWEN =  2;
       TWIE =  0;

       TWA6 =  7;
       TWA5 =  6;
       TWA4 =  5;
       TWA3 =  4;
       TWA2 =  3;
       TWA1 =  2;
       TWA0 =  1;
       TWGCE = 0;

       TWS7 =  7;
       TWS6 =  6;
       TWS5 =  5;
       TWS4 =  4;
       TWS3 =  3;
       TWPS1 = 1;
       TWPS0 = 0;

       XDIVEN = 7;
       XDIV6 = 6;
       XDIV5 = 5;
       XDIV4 = 4;
       XDIV3 = 3;
       XDIV2 = 2;
       XDIV1 = 1;
       XDIV0 = 0;

       ISC11 = 3;
       ISC10 = 2;
       ISC01 = 1;
       ISC00 = 0;

       INT1 =  7;
       INT0 =  6;

       INTF1 = 7;
       INTF0 = 6;

       OCIE2 = 7;
       TOIE2 = 6;
       TICIE1 = 5;
       OCIE1A = 4;
       OCIE1B = 3;
       TOIE1 = 2;
       TOIE0 = 0;

       OCF2 =  7;
       TOV2 =  6;
       ICF1 =  5;
       OCF1A = 4;
       OCF1B = 3;
       TOV1 =  2;
       TOV0 =  0;

       SPMIE = 7;
       RWWSB = 6;
       RWWSRE = 4;
       BLBSET = 3;
       PGWRT = 2;
       PGERS = 1;
       SPMEN = 0;

       COM1A1 = 7;
       COM1A0 = 6;
       COM1B1 = 5;
       COM1B0 = 4;
       COM1C1 = 3;
       COM1C0 = 2;
       WGM11 = 1;
       WGM10 = 0;

       ICNC =  7;
       ICES =  6;
       WGMB3 = 4;
       WGMB2 = 3;
       CSB2 =  2;
       CSB1 =  1;
       CSB0 =  0;

       ICNC1 = 7;
       ICES1 = 6;
       WGM13 = 4;
       WGM12 = 3;
       CS12 =  2;
       CS11 =  1;
       CS10 =  0;

       FOC2 =  7;
       WGM20 = 6;
       COM21 = 5;
       COM20 = 4;
       WGM21 = 3;
       CS22 =  2;
       CS21 =  1;
       CS20 =  0;

       SPIF =  7;
       WCOL =  6;
       SPI2X = 0;

       SPIE =  7;
       SPE =   6;
       DORD =  5;
       MSTR =  4;
       CPOL =  3;
       CPHA =  2;
       SPR1 =  1;
       SPR0 =  0;

       URSEL = 7;
       UMSEL = 6;
       UPM1 =  5;
       UPM0 =  4;
       USBS =  3;
       UCSZ1 = 2;
       UCSZ0 = 1;
       UCPOL = 0;

       RXC =   7;
       TXC =   6;
       UDRE =  5;
       FE =    4;
       DOR =   3;
       UPE =   2;
       U2X =   1;
       MPCM =  0;

       RXCIE = 7;
       TXCIE = 6;
       UDRIE = 5;
       RXEN =  4;
       TXEN =  3;
       UCSZ =  2;
       UCSZ2 = 2;
       RXB8 =  1;
       TXB8 =  0;

       ACD =   7;
       ACBG =  6;
       ACO =   5;
       ACI =   4;
       ACIE =  3;
       ACIC =  2;
       ACIS1 = 1;
       ACIS0 = 0;

       ADEN =  7;
       ADSC =  6;
       ADFR =  5;
       ADIF =  4;
       ADIE =  3;
       ADPS2 = 2;
       ADPS1 = 1;
       ADPS0 = 0;

       REFS1 = 7;
       REFS0 = 6;
       ADLAR = 5;
       MUX3 =  3;
       MUX2 =  2;
       MUX1 =  1;
       MUX0 =  0;

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
    procedure Int17Handler; external name 'Int17Handler';
    procedure Int18Handler; external name 'Int18Handler';
    procedure Int19Handler; external name 'Int19Handler';

    procedure _FPC_start; assembler; nostackframe;
      label
        _start;
      asm
        .init
        .globl _start

        jmp _start
        jmp Int00Handler
        jmp Int01Handler
        jmp Int02Handler
        jmp Int03Handler
        jmp Int04Handler
        jmp Int05Handler
        jmp Int06Handler
        jmp Int07Handler
        jmp Int08Handler
        jmp Int09Handler
        jmp Int10Handler
        jmp Int11Handler
        jmp Int12Handler
        jmp Int13Handler
        jmp Int14Handler
        jmp Int15Handler
        jmp Int16Handler
        jmp Int17Handler
        jmp Int18Handler
        jmp Int19Handler

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
      end;

end.
