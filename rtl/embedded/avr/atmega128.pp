{******************************************************************************
Register definitions and startup code for ATMEL ATmega128

******************************************************************************}
unit atmega128;

{$goto on}

  interface
    const
      _SFR_OFFSET = $20; //indirect addressing
    var
      PINF    : byte absolute $00+_SFR_OFFSET;
      PINE    : byte absolute $01+_SFR_OFFSET;
      DDRE    : byte absolute $02+_SFR_OFFSET;
      PORTE   : byte absolute $03+_SFR_OFFSET;
      ADCW    : word absolute $04+_SFR_OFFSET;
      ADC     : word absolute $04+_SFR_OFFSET;
      ADCL    : byte absolute $04+_SFR_OFFSET;
      ADCH    : byte absolute $05+_SFR_OFFSET;
      ADCSR   : byte absolute $06+_SFR_OFFSET;
      ADCSRA  : byte absolute $06+_SFR_OFFSET;
      ADMUX   : byte absolute $07+_SFR_OFFSET;
      ACSR    : byte absolute $08+_SFR_OFFSET;
      UBRR0L  : byte absolute $09+_SFR_OFFSET;
      UCSR0B  : byte absolute $0A+_SFR_OFFSET;
      UCSR0A  : byte absolute $0B+_SFR_OFFSET;
      UDR0    : byte absolute $0C+_SFR_OFFSET;
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
      SFIOR   : byte absolute $20+_SFR_OFFSET;
      WDTCR   : byte absolute $21+_SFR_OFFSET;
      OCDR    : byte absolute $22+_SFR_OFFSET;
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
      TCCR1A  : byte absolute $2F+_SFR_OFFSET;
      TCCR1B  : byte absolute $2E+_SFR_OFFSET;
      ASSR    : byte absolute $30+_SFR_OFFSET;
      OCR0    : byte absolute $31+_SFR_OFFSET;
      TCNT0   : byte absolute $32+_SFR_OFFSET;
      TCCR0   : byte absolute $33+_SFR_OFFSET;
      MCUSR   : byte absolute $34+_SFR_OFFSET;
      MCUCSR  : byte absolute $34+_SFR_OFFSET;
      MCUCR   : byte absolute $35+_SFR_OFFSET;
      TIFR    : byte absolute $36+_SFR_OFFSET;
      TIMSK   : byte absolute $37+_SFR_OFFSET;
      EIFR    : byte absolute $38+_SFR_OFFSET;
      EIMSK   : byte absolute $39+_SFR_OFFSET;
      EICRB   : byte absolute $3A+_SFR_OFFSET;
      RAMPZ   : byte absolute $3B+_SFR_OFFSET;
      XDIV    : byte absolute $3C+_SFR_OFFSET;
      DDRF    : byte absolute $61;
      PORTF   : byte absolute $62;
      PING    : byte absolute $63;
      DDRG    : byte absolute $64;
      PORTG   : byte absolute $65;
      SPMCSR  : byte absolute $68;
      EICRA   : byte absolute $6A;
      XMCRB   : byte absolute $6C;
      XMCRA   : byte absolute $6D;
      OSCCAL  : byte absolute $6F;
      TWBR    : byte absolute $70;
      TWSR    : byte absolute $71;
      TWAR    : byte absolute $72;
      TWDR    : byte absolute $73;
      TWCR    : byte absolute $74;
      OCR1C   : word absolute $78;
      OCR1CL  : byte absolute $78;
      OCR1CH  : byte absolute $79;
      TCCR1C  : byte absolute $7A;
      ETIFR   : byte absolute $7C;
      ETIMSK  : byte absolute $7D;
      ICR3    : word absolute $80;
      ICR3L   : byte absolute $80;
      ICR3H   : byte absolute $81;
      OCR3C   : word absolute $82;
      OCR3CL  : byte absolute $82;
      OCR3CH  : byte absolute $83;
      OCR3B   : word absolute $84;
      OCR3BL  : byte absolute $84;
      OCR3BH  : byte absolute $85;
      OCR3A   : word absolute $86;
      OCR3AL  : byte absolute $86;
      OCR3AH  : byte absolute $87;
      TCNT3   : word absolute $88;
      TCNT3L  : byte absolute $88;
      TCNT3H  : byte absolute $89;
      TCCR3B  : byte absolute $8A;
      TCCR3A  : byte absolute $8B;
      TCCR3C  : byte absolute $8C;
      UBRR0H  : byte absolute $90;
      UCSR0C  : byte absolute $95;
      UBRR1H  : byte absolute $98;
      UBRR1L  : byte absolute $99;
      UCSR1B  : byte absolute $9A;
      UCSR1A  : byte absolute $9B;
      UDR1    : byte absolute $9C;
      UCSR1C  : byte absolute $9D;
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

       SRL2 =  6;
       SRL1 =  5;
       SRL0 =  4;
       SRW01 = 3;
       SRW00 = 2;
       SRW11 = 1;

       XMBK =  7;
       XMM2 =  2;
       XMM1 =  1;
       XMM0 =  0;

       XDIVEN = 7;
       XDIV6 = 6;
       XDIV5 = 5;
       XDIV4 = 4;
       XDIV3 = 3;
       XDIV2 = 2;
       XDIV1 = 1;
       XDIV0 = 0;

       RAMPZ0 = 0;

       ISC31 = 7;
       ISC30 = 6;
       ISC21 = 5;
       ISC20 = 4;
       ISC11 = 3;
       ISC10 = 2;
       ISC01 = 1;
       ISC00 = 0;

       ISC71 = 7;
       ISC70 = 6;
       ISC61 = 5;
       ISC60 = 4;
       ISC51 = 3;
       ISC50 = 2;
       ISC41 = 1;
       ISC40 = 0;

       SPMIE = 7;
       RWWSB = 6;
       RWWSRE = 4;
       BLBSET = 3;
       PGWRT = 2;
       PGERS = 1;
       SPMEN = 0;

       INT7 =  7;
       INT6 =  6;
       INT5 =  5;
       INT4 =  4;
       INT3 =  3;
       INT2 =  2;
       INT1 =  1;
       INT0 =  0;

       INTF7 = 7;
       INTF6 = 6;
       INTF5 = 5;
       INTF4 = 4;
       INTF3 = 3;
       INTF2 = 2;
       INTF1 = 1;
       INTF0 = 0;

       OCIE2 = 7;
       TOIE2 = 6;
       TICIE1 = 5;
       OCIE1A = 4;
       OCIE1B = 3;
       TOIE1 = 2;
       OCIE0 = 1;
       TOIE0 = 0;

       OCF2 =  7;
       TOV2 =  6;
       ICF1 =  5;
       OCF1A = 4;
       OCF1B = 3;
       TOV1 =  2;
       OCF0 =  1;
       TOV0 =  0;

       TICIE3 = 5;
       OCIE3A = 4;
       OCIE3B = 3;
       TOIE3 = 2;
       OCIE3C = 1;
       OCIE1C = 0;

       ICF3 =  5;
       OCF3A = 4;
       OCF3B = 3;
       TOV3 =  2;
       OCF3C = 1;
       OCF1C = 0;

       SRE =   7;
       SRW =   6;
       SRW10 = 6;
       SE =    5;
       SM1 =   4;
       SM0 =   3;
       SM2 =   2;
       IVSEL = 1;
       IVCE =  0;

       JTD =   7;
       JTRF =  4;
       WDRF =  3;
       BORF =  2;
       EXTRF = 1;
       PORF =  0;

       FOC =   7;
       WGM0 =  6;
       COM1 =  5;
       COM0 =  4;
       WGM1 =  3;
       CS2 =   2;
       CS1 =   1;
       CS0 =   0;

       FOC0 =  7;
       WGM00 = 6;
       COM01 = 5;
       COM00 = 4;
       WGM01 = 3;
       CS02 =  2;
       CS01 =  1;
       CS00 =  0;

       FOC2 =  7;
       WGM20 = 6;
       COM21 = 5;
       COM20 = 4;
       WGM21 = 3;
       CS22 =  2;
       CS21 =  1;
       CS20 =  0;

       AS0 =   3;
       TCN0UB = 2;
       OCR0UB = 1;
       TCR0UB = 0;

       COMA1 = 7;
       COMA0 = 6;
       COMB1 = 5;
       COMB0 = 4;
       COMC1 = 3;
       COMC0 = 2;
       WGMA1 = 1;
       WGMA0 = 0;

       COM1A1 = 7;
       COM1A0 = 6;
       COM1B1 = 5;
       COM1B0 = 4;
       COM1C1 = 3;
       COM1C0 = 2;
       WGM11 = 1;
       WGM10 = 0;

       COM3A1 = 7;
       COM3A0 = 6;
       COM3B1 = 5;
       COM3B0 = 4;
       COM3C1 = 3;
       COM3C0 = 2;
       WGM31 = 1;
       WGM30 = 0;

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

       ICNC3 = 7;
       ICES3 = 6;
       WGM33 = 4;
       WGM32 = 3;
       CS32 =  2;
       CS31 =  1;
       CS30 =  0;

       FOCA =  7;
       FOCB =  6;
       FOCC =  5;

       FOC3A = 7;
       FOC3B = 6;
       FOC3C = 5;

       FOC1A = 7;
       FOC1B = 6;
       FOC1C = 5;

       IDRD =  7;
       OCDR7 = 7;
       OCDR6 = 6;
       OCDR5 = 5;
       OCDR4 = 4;
       OCDR3 = 3;
       OCDR2 = 2;
       OCDR1 = 1;
       OCDR0 = 0;

       WDCE =  4;
       WDE =   3;
       WDP2 =  2;
       WDP1 =  1;
       WDP0 =  0;

       TSM =   7;
       ACME =  3;
       PUD =   2;
       PSR0 =  1;
       PSR321 = 0;

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

       UMSEL = 6;
       UPM1 =  5;
       UPM0 =  4;
       USBS =  3;
       UCSZ1 = 2;
       UCSZ0 = 1;
       UCPOL = 0;

       UMSEL1 = 6;
       UPM11 = 5;
       UPM10 = 4;
       USBS1 = 3;
       UCSZ11 = 2;
       UCSZ10 = 1;
       UCPOL1 = 0;

       UMSEL0 = 6;
       UPM01 = 5;
       UPM00 = 4;
       USBS0 = 3;
       UCSZ01 = 2;
       UCSZ00 = 1;
       UCPOL0 = 0;

       RXC =   7;
       TXC =   6;
       UDRE =  5;
       FE =    4;
       DOR =   3;
       UPE =   2;
       U2X =   1;
       MPCM =  0;

       RXC1 =  7;
       TXC1 =  6;
       UDRE1 = 5;
       FE1 =   4;
       DOR1 =  3;
       UPE1 =  2;
       U2X1 =  1;
       MPCM1 = 0;

       RXC0 =  7;
       TXC0 =  6;
       UDRE0 = 5;
       FE0 =   4;
       DOR0 =  3;
       UPE0 =  2;
       U2X0 =  1;
       MPCM0 = 0;

       RXCIE = 7;
       TXCIE = 6;
       UDRIE = 5;
       RXEN =  4;
       TXEN =  3;
       UCSZ =  2;
       UCSZ2 = 2;
       RXB8 =  1;
       TXB8 =  0;

       RXCIE1 = 7;
       TXCIE1 = 6;
       UDRIE1 = 5;
       RXEN1 = 4;
       TXEN1 = 3;
       UCSZ12 = 2;
       RXB81 = 1;
       TXB81 = 0;

       RXCIE0 = 7;
       TXCIE0 = 6;
       UDRIE0 = 5;
       RXEN0 = 4;
       TXEN0 = 3;
       UCSZ02 = 2;
       RXB80 = 1;
       TXB80 = 0;

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
       MUX4 =  4;
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
        jmp Int20Handler
        jmp Int21Handler
        jmp Int22Handler
        jmp Int23Handler
        jmp Int24Handler
        jmp Int25Handler
        jmp Int26Handler
        jmp Int27Handler
        jmp Int28Handler
        jmp Int29Handler
        jmp Int30Handler
        jmp Int31Handler
        jmp Int32Handler
        jmp Int33Handler
        jmp Int34Handler

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
        .weak Int25Handler
        .weak Int26Handler
        .weak Int27Handler
        .weak Int28Handler
        .weak Int29Handler
        .weak Int30Handler
        .weak Int31Handler
        .weak Int32Handler
        .weak Int33Handler
        .weak Int34Handler

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
        .set Int25Handler, Default_IRQ_handler
        .set Int26Handler, Default_IRQ_handler
        .set Int27Handler, Default_IRQ_handler
        .set Int28Handler, Default_IRQ_handler
        .set Int29Handler, Default_IRQ_handler
        .set Int30Handler, Default_IRQ_handler
        .set Int31Handler, Default_IRQ_handler
        .set Int32Handler, Default_IRQ_handler
        .set Int33Handler, Default_IRQ_handler
        .set Int34Handler, Default_IRQ_handler
      end;

end.
