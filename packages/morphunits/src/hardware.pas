{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    Hardware definitions unit for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}
unit hardware;

interface

uses exec;



{ * adkcon bit defines
  * Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
  * }

const
  ADKB_SETCLR   = 15;
  ADKB_PRECOMP1 = 14;
  ADKB_PRECOMP0 = 13;
  ADKB_MFMPREC  = 12;
  ADKB_UARTBRK  = 11;
  ADKB_WORDSYNC = 10;
  ADKB_MSBSYNC  = 9;
  ADKB_FAST     = 8;
  ADKB_USE3PN   = 7;
  ADKB_USE2P3   = 6;
  ADKB_USE1P2   = 5;
  ADKB_USE0P1   = 4;
  ADKB_USE3VN   = 3;
  ADKB_USE2V3   = 2;
  ADKB_USE1V2   = 1;
  ADKB_USE0V1   = 0;

const
  ADKF_SETCLR   = (1 Shl ADKB_SETCLR);
  ADKF_PRECOMP1 = (1 Shl ADKB_PRECOMP1);
  ADKF_PRECOMP0 = (1 Shl ADKB_PRECOMP0);
  ADKF_MFMPREC  = (1 Shl ADKB_MFMPREC);
  ADKF_UARTBRK  = (1 Shl ADKB_UARTBRK);
  ADKF_WORDSYNC = (1 Shl ADKB_WORDSYNC);
  ADKF_MSBSYNC  = (1 Shl ADKB_MSBSYNC);
  ADKF_FAST     = (1 Shl ADKB_FAST);
  ADKF_USE3PN   = (1 Shl ADKB_USE3PN);
  ADKF_USE2P3   = (1 Shl ADKB_USE2P3);
  ADKF_USE1P2   = (1 Shl ADKB_USE1P2);
  ADKF_USE0P1   = (1 Shl ADKB_USE0P1);
  ADKF_USE3VN   = (1 Shl ADKB_USE3VN);
  ADKF_USE2V3   = (1 Shl ADKB_USE2V3);
  ADKF_USE1V2   = (1 Shl ADKB_USE1V2);
  ADKF_USE0V1   = (1 Shl ADKB_USE0V1);

const
  ADKF_PRE000NS = 0;
  ADKF_PRE140NS = (ADKF_PRECOMP0);
  ADKF_PRE280NS = (ADKF_PRECOMP1);
  ADKF_PRE560NS = (ADKF_PRECOMP0 or ADKF_PRECOMP1);



{ * blitter defines
  * Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
  * }

const
  HSIZEBITS = 6;
  VSIZEBITS = (16 - HSIZEBITS);
  HSIZEMASK = $3F;
  VSIZEMASK = $3FF;

{$IFNDEF NO_BIG_BLITS}
  MINBYTESPERROW = 128;
  MAXBYTESPERROW = 4096;
{$ELSE}
  MAXBYTESPERROW = 128;
{$ENDIF}

const
  ABC    = $80;
  ABNC   = $40;
  ANBC   = $20;
  ANBNC  = $10;
  NABC   = $8;
  NABNC  = $4;
  NANBC  = $2;
  NANBNC = $1;

const
  A_OR_B  = (ABC or ANBC or NABC or ABNC or ANBNC or NABNC);
  A_OR_C  = (ABC or NABC or ABNC or ANBC or NANBC or ANBNC);
  A_XOR_C = (NABC or ABNC or NANBC or ANBNC);
  A_TO_D  = (ABC or ANBC or ABNC or ANBNC);

const
  BC0B_DEST = 8;
  BC0B_SRCC = 9;
  BC0B_SRCB = 10;
  BC0B_SRCA = 11;
  BC0F_DEST = (1 Shl BC0B_DEST);
  BC0F_SRCC = (1 Shl BC0B_SRCC);
  BC0F_SRCB = (1 Shl BC0B_SRCB);
  BC0F_SRCA = (1 Shl BC0B_SRCA);

  BC1F_DESC = 2;

  DEST = BC0F_DEST;
  SRCC = BC0F_SRCC;
  SRCB = BC0F_SRCB;
  SRCA = BC0F_SRCA;

  ASHIFTSHIFT = 12;
  BSHIFTSHIFT = 12;

const
  LINEMODE     = $1;
  FILL_OR      = $8;
  FILL_XOR     = $10;
  FILL_CARRYIN = $4;
  ONEDOT       = $2;
  OVFLAG       = $20;
  SIGNFLAG     = $40;
  BLITREVERSE  = $2;

  SUD = $10;
  SUL = $8;
  AUL = $4;

  OCTANT8 = 24;
  OCTANT7 = 4;
  OCTANT6 = 12;
  OCTANT5 = 28;
  OCTANT4 = 20;
  OCTANT3 = 8;
  OCTANT2 = 0;
  OCTANT1 = 16;

type
  Pbltnode = ^Tbltnode;
  Tbltnode = record
    n        : Pbltnode;
    _function: Pointer;
    stat     : Byte;
    blitsize : SmallInt;
    beamsync : SmallInt;
    cleanup  : Pointer;
  end;

const
  CLEANUP = $40;
  CLEANME = CLEANUP;



{ * byteswap routines
  * Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
  * }
{$WARNING Byteswap macros not yet converted!}



{ * cia registers and bits
  * Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
  * }


type
  PCIA = ^TCIA;
  TCIA = record
    ciapra   : Byte;
    pad0     : Array[0..254] of Byte;
    ciaprb   : Byte;
    pad1     : array[0..254] of Byte;
    ciaddra  : Byte;
    pad2     : array[0..254] of Byte;
    ciaddrb  : Byte;
    pad3     : array[0..254] of Byte;
    ciatalo  : Byte;
    pad4     : array[0..254] of Byte;
    ciatahi  : Byte;
    pad5     : array[0..254] of Byte;
    ciatblo  : Byte;
    pad6     : array[0..254] of Byte;
    ciatbhi  : Byte;
    pad7     : array[0..254] of Byte;
    ciatodlow: Byte;
    pad8     : array[0..254] of Byte;
    ciatodmid: Byte;
    pad9     : array[0..254] of Byte;
    ciatodhi : Byte;
    pad10    : array[0..254] of Byte;
    unusedreg: Byte;
    pad11    : array[0..254] of Byte;
    ciasdr   : Byte;
    pad12    : array[0..254] of Byte;
    ciaicr   : Byte;
    pad13    : array[0..254] of Byte;
    ciacra   : Byte;
    pad14    : array[0..254] of Byte;
    ciacrb   : Byte;
  end;

const
  CIAICRB_TA     = 0;
  CIAICRB_TB     = 1;
  CIAICRB_ALRM   = 2;
  CIAICRB_SP     = 3;
  CIAICRB_FLG    = 4;
  CIAICRB_IR     = 7;
  CIAICRB_SETCLR = 7;

  CIAICRF_TA     = (1 Shl CIAICRB_TA);
  CIAICRF_TB     = (1 Shl CIAICRB_TB);
  CIAICRF_ALRM   = (1 Shl CIAICRB_ALRM);
  CIAICRF_SP     = (1 Shl CIAICRB_SP);
  CIAICRF_FLG    = (1 Shl CIAICRB_FLG);
  CIAICRF_IR     = (1 Shl CIAICRB_IR);
  CIAICRF_SETCLR = (1 Shl CIAICRB_SETCLR);

const
  CIACRAB_START   = 0;
  CIACRAB_PBON    = 1;
  CIACRAB_OUTMODE = 2;
  CIACRAB_RUNMODE = 3;
  CIACRAB_LOAD    = 4;
  CIACRAB_INMODE  = 5;
  CIACRAB_SPMODE  = 6;
  CIACRAB_TODIN   = 7;

  CIACRAF_START   = (1 Shl CIACRAB_START);
  CIACRAF_PBON    = (1 Shl CIACRAB_PBON);
  CIACRAF_OUTMODE = (1 Shl CIACRAB_OUTMODE);
  CIACRAF_RUNMODE = (1 Shl CIACRAB_RUNMODE);
  CIACRAF_LOAD    = (1 Shl CIACRAB_LOAD);
  CIACRAF_INMODE  = (1 Shl CIACRAB_INMODE);
  CIACRAF_SPMODE  = (1 Shl CIACRAB_SPMODE);
  CIACRAF_TODIN   = (1 Shl CIACRAB_TODIN);

const
  CIACRBB_START   = 0;
  CIACRBB_PBON    = 1;
  CIACRBB_OUTMODE = 2;
  CIACRBB_RUNMODE = 3;
  CIACRBB_LOAD    = 4;
  CIACRBB_INMODE0 = 5;
  CIACRBB_INMODE1 = 6;
  CIACRBB_ALARM   = 7;

  CIACRBF_START   = (1 Shl CIACRBB_START);
  CIACRBF_PBON    = (1 Shl CIACRBB_PBON);
  CIACRBF_OUTMODE = (1 Shl CIACRBB_OUTMODE);
  CIACRBF_RUNMODE = (1 Shl CIACRBB_RUNMODE);
  CIACRBF_LOAD    = (1 Shl CIACRBB_LOAD);
  CIACRBF_INMODE0 = (1 Shl CIACRBB_INMODE0);
  CIACRBF_INMODE1 = (1 Shl CIACRBB_INMODE1);
  CIACRBF_ALARM   = (1 Shl CIACRBB_ALARM);

const
  CIACRBF_IN_PHI2   = 0;
  CIACRBF_IN_CNT    = (CIACRBF_INMODE0);
  CIACRBF_IN_TA     = (CIACRBF_INMODE1);
  CIACRBF_IN_CNT_TA = (CIACRBF_INMODE0 or CIACRBF_INMODE1);

const
  CIAB_GAMEPORT1 = (7);
  CIAB_GAMEPORT0 = (6);
  CIAB_DSKRDY    = (5);
  CIAB_DSKTRACK0 = (4);
  CIAB_DSKPROT   = (3);
  CIAB_DSKCHANGE = (2);
  CIAB_LED       = (1);
  CIAB_OVERLAY   = (0);

  CIAF_GAMEPORT1 = (1 Shl CIAB_GAMEPORT1);
  CIAF_GAMEPORT0 = (1 Shl CIAB_GAMEPORT0);
  CIAF_DSKRDY    = (1 Shl CIAB_DSKRDY);
  CIAF_DSKTRACK0 = (1 Shl CIAB_DSKTRACK0);
  CIAF_DSKPROT   = (1 Shl CIAB_DSKPROT);
  CIAF_DSKCHANGE = (1 Shl CIAB_DSKCHANGE);
  CIAF_LED       = (1 Shl CIAB_LED);
  CIAF_OVERLAY   = (1 Shl CIAB_OVERLAY);

const
  CIAB_COMDTR    = (7);
  CIAB_COMRTS    = (6);
  CIAB_COMCD     = (5);
  CIAB_COMCTS    = (4);
  CIAB_COMDSR    = (3);
  CIAB_PRTRSEL   = (2);
  CIAB_PRTRPOUT  = (1);
  CIAB_PRTRBUSY  = (0);

  CIAF_COMDTR    = (1 Shl CIAB_COMDTR);
  CIAF_COMRTS    = (1 Shl CIAB_COMRTS);
  CIAF_COMCD     = (1 Shl CIAB_COMCD);
  CIAF_COMCTS    = (1 Shl CIAB_COMCTS);
  CIAF_COMDSR    = (1 Shl CIAB_COMDSR);
  CIAF_PRTRSEL   = (1 Shl CIAB_PRTRSEL);
  CIAF_PRTRPOUT  = (1 Shl CIAB_PRTRPOUT);
  CIAF_PRTRBUSY  = (1 Shl CIAB_PRTRBUSY);

const
  CIAB_DSKMOTOR  = (7);
  CIAB_DSKSEL3   = (6);
  CIAB_DSKSEL2   = (5);
  CIAB_DSKSEL1   = (4);
  CIAB_DSKSEL0   = (3);
  CIAB_DSKSIDE   = (2);
  CIAB_DSKDIREC  = (1);
  CIAB_DSKSTEP   = (0);

  CIAF_DSKMOTOR  = (1 Shl CIAB_DSKMOTOR);
  CIAF_DSKSEL3   = (1 Shl CIAB_DSKSEL3);
  CIAF_DSKSEL2   = (1 Shl CIAB_DSKSEL2);
  CIAF_DSKSEL1   = (1 Shl CIAB_DSKSEL1);
  CIAF_DSKSEL0   = (1 Shl CIAB_DSKSEL0);
  CIAF_DSKSIDE   = (1 Shl CIAB_DSKSIDE);
  CIAF_DSKDIREC  = (1 Shl CIAB_DSKDIREC);
  CIAF_DSKSTEP   = (1 Shl CIAB_DSKSTEP);



{ * custom-chip registers and bits
  * Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
  * }

type
  PAudChannel = ^TAudChannel;
  TAudChannel = record
    ac_ptr: Pointer;
    ac_len: Word;
    ac_per: Word;
    ac_vol: Word;
    ac_dat: Word;
    ac_pad: array[0..1] of Word;
  end;

  PSpriteDef = ^TSpriteDef;
  TSpriteDef = record
    pos  : Word;
    ctl  : Word;
    dataa: Word;
    datab: Word;
  end;

  PCustom = ^TCustom;
  TCustom = record
    bltddat : Word;
    dmaconr : Word;
    vposr   : Word;
    vhposr  : Word;
    dskdatr : Word;
    joy0dat : Word;
    joy1dat : Word;
    clxdat  : Word;
    adkconr : Word;
    pot0dat : Word;
    pot1dat : Word;
    potinp  : Word;
    serdatr : Word;
    dskbytr : Word;
    intenar : Word;
    intreqr : Word;
    dskpt   : Pointer;
    dsklen  : Word;
    dskdat  : Word;
    refptr  : Word;
    vposw   : Word;
    vhposw  : Word;
    copcon  : Word;
    serdat  : Word;
    serper  : Word;
    potgo   : Word;
    joytest : Word;
    strequ  : Word;
    strvbl  : Word;
    strhor  : Word;
    strlong : Word;
    bltcon0 : Word;
    bltcon1 : Word;
    bltafwm : Word;
    bltalwm : Word;
    bltcpt  : Pointer;
    bltbpt  : Pointer;
    bltapt  : Pointer;
    bltdpt  : Pointer;
    bltsize : Word;
    pad2d   : Byte;
    bltcon0l: Byte;
    bltsizv : Word;
    bltsizh : Word;
    bltcmod : Word;
    bltbmod : Word;
    bltamod : Word;
    bltdmod : Word;
    pad34   : array[0..3] of Word;
    bltcdat : Word;
    bltbdat : Word;
    bltadat : Word;
    pad3b   : array[0..2] of Word;
    deniseid: Word;
    dsksync : Word;
    cop1lc  : Longint;
    cop2lc  : Longint;
    copjmp1 : Word;
    copjmp2 : Word;
    copins  : Word;
    diwstrt : Word;
    diwstop : Word;
    ddfstrt : Word;
    ddfstop : Word;
    dmacon  : Word;
    clxcon  : Word;
    intena  : Word;
    intreq  : Word;
    adkcon  : Word;
    aud     : array[0..3] of TAudChannel;
    bplpt   : array[0..7] of Pointer;
    bplcon0 : Word;
    bplcon1 : Word;
    bplcon2 : Word;
    bplcon3 : Word;
    bpl1mod : Word;
    bpl2mod : Word;
    bplcon4 : Word;
    clxcon2 : Word;
    bpldat  : array[0..7] of Word;
    sprpt   : array[0..7] of Pointer;
    spr     : array[0..7] of TSpriteDef;
    color   : array[0..31] of Word;
    htotal  : Word;
    hsstop  : Word;
    hbstrt  : Word;
    hbstop  : Word;
    vtotal  : Word;
    vsstop  : Word;
    vbstrt  : Word;
    vbstop  : Word;
    sprhstrt: Word;
    sprhstop: Word;
    bplhstrt: Word;
    bplhstop: Word;
    hhposw  : Word;
    hhposr  : Word;
    beamcon0: Word;
    hsstrt  : Word;
    vsstrt  : Word;
    hcenter : Word;
    diwhigh : Word;
    padf3   : array[0..10] of Word;
    fmode   : Word;
  end;


const
  VARVBLANK   = $1000;
  LOLDIS      = $0800;
  CSCBLANKEN  = $0400;
  VARVSYNC    = $0200;
  VARHSYNC    = $0100;
  VARBEAM     = $0080;
  DISPLAYDUAL = $0040;
  DISPLAYPAL  = $0020;
  VARCSYNC    = $0010;
  CSBLANK     = $0008;
  CSYNCTRUE   = $0004;
  VSYNCTRUE   = $0002;
  HSYNCTRUE   = $0001;

  USE_BPLCON3 = 1;

  BPLCON2_ZDCTEN    = (1 Shl 10);
  BPLCON2_ZDBPEN    = (1 Shl 11);
  BPLCON2_ZDBPSEL0  = (1 Shl 12);
  BPLCON2_ZDBPSEL1  = (1 Shl 13);
  BPLCON2_ZDBPSEL2  = (1 Shl 14);

  BPLCON3_EXTBLNKEN = (1 Shl 0);
  BPLCON3_EXTBLKZD  = (1 Shl 1);
  BPLCON3_ZDCLKEN   = (1 Shl 2);
  BPLCON3_BRDNTRAN  = (1 Shl 4);
  BPLCON3_BRDNBLNK  = (1 Shl 5);



{ * dma bits
  * Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
  * }

const
  DMAB_AUD0     = 0;
  DMAB_AUD1     = 1;
  DMAB_AUD2     = 2;
  DMAB_AUD3     = 3;
  DMAB_DISK     = 4;
  DMAB_SPRITE   = 5;
  DMAB_BLITTER  = 6;
  DMAB_COPPER   = 7;
  DMAB_RASTER   = 8;
  DMAB_MASTER   = 9;
  DMAB_BLITHOG  = 10;
  DMAB_BLTNZERO = 13;
  DMAB_BLTDONE  = 14;
  DMAB_SETCLR   = 15;

  DMAF_AUD0     = (1 Shl DMAB_AUD0);
  DMAF_AUD1     = (1 Shl DMAB_AUD1);
  DMAF_AUD2     = (1 Shl DMAB_AUD2);
  DMAF_AUD3     = (1 Shl DMAB_AUD3);
  DMAF_DISK     = (1 Shl DMAB_DISK);
  DMAF_SPRITE   = (1 Shl DMAB_SPRITE);
  DMAF_BLITTER  = (1 Shl DMAB_BLITTER);
  DMAF_COPPER   = (1 Shl DMAB_COPPER);
  DMAF_RASTER   = (1 Shl DMAB_RASTER);
  DMAF_MASTER   = (1 Shl DMAB_MASTER);
  DMAF_BLITHOG  = (1 Shl DMAB_BLITHOG);
  DMAF_BLTNZERO = (1 Shl DMAB_BLTNZERO);
  DMAF_BLTDONE  = (1 Shl DMAB_BLTDONE);
  DMAF_SETCLR   = (1 Shl DMAB_SETCLR);

const
  DMAF_AUDIO    = (DMAF_AUD0 or DMAF_AUD1 or DMAF_AUD2 or DMAF_AUD3);
  DMAF_ALL      = (DMAF_AUD0 or DMAF_AUD1 or DMAF_AUD2 or DMAF_AUD3 or DMAF_DISK or DMAF_SPRITE or DMAF_BLITTER or DMAF_COPPER or DMAF_RASTER);



{ * interrupt bits
  * Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
  * }

const
  INTB_SETCLR  = (15);
  INTB_INTEN   = (14);
  INTB_EXTER   = (13);
  INTB_DSKSYNC = (12);
  INTB_RBF     = (11);
  INTB_AUD3    = (10);
  INTB_AUD2    = (9);
  INTB_AUD1    = (8);
  INTB_AUD0    = (7);
  INTB_BLIT    = (6);
  INTB_VERTB   = (5);
  INTB_COPER   = (4);
  INTB_PORTS   = (3);
  INTB_SOFTINT = (2);
  INTB_DSKBLK  = (1);
  INTB_TBE     = (0);

  INTF_SETCLR  = (1 Shl INTB_SETCLR);
  INTF_INTEN   = (1 Shl INTB_INTEN);
  INTF_EXTER   = (1 Shl INTB_EXTER);
  INTF_DSKSYNC = (1 Shl INTB_DSKSYNC);
  INTF_RBF     = (1 Shl INTB_RBF);
  INTF_AUD3    = (1 Shl INTB_AUD3);
  INTF_AUD2    = (1 Shl INTB_AUD2);
  INTF_AUD1    = (1 Shl INTB_AUD1);
  INTF_AUD0    = (1 Shl INTB_AUD0);
  INTF_BLIT    = (1 Shl INTB_BLIT);
  INTF_VERTB   = (1 Shl INTB_VERTB);
  INTF_COPER   = (1 Shl INTB_COPER);
  INTF_PORTS   = (1 Shl INTB_PORTS);
  INTF_SOFTINT = (1 Shl INTB_SOFTINT);
  INTF_DSKBLK  = (1 Shl INTB_DSKBLK);
  INTF_TBE     = (1 Shl INTB_TBE);

implementation

end.
