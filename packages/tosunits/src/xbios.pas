{
    Copyright (c) 2016 by Free Pascal development team

    XBIOS interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}
{$IFNDEF FPC_DOTTEDUNITS}
unit xbios;
{$ENDIF FPC_DOTTEDUNITS}

interface

{ The API description of this file is based on the information available
  online at: https://freemint.github.io/tos.hyp/en/index.html }

const
{* Codes used with Cursconf() *}
    CURS_HIDE       = 0;
    CURS_SHOW       = 1;
    CURS_BLINK      = 2;
    CURS_NOBLINK    = 3;
    CURS_SETRATE    = 4;
    CURS_GETRATE    = 5;

{* Modes for Initmous() *}
    IM_DISABLE      = 0;
    IM_RELATIVE     = 1;
    IM_ABSOLUTE     = 2;
    IM_KEYCODE      = 4;

    IM_YBOT         = 0;
    IM_YTOP         = 1;
    IM_PACKETS      = 2;
    IM_KEYS         = 3;

{* VsetScreen() modes *}
    SCR_NOCHANGE    = -1;
    SCR_MODECODE    = 3;

    COL_INQUIRE     = -1;

{* Floprd() devices *}
    FLOP_DRIVEA     = 0;
    FLOP_DRIVEB     = 1;

{* Flopfmt() params *}
    FLOP_NOSKEW      = 1;
    FLOP_SKEW       = -1;

    FLOP_MAGIC      = $8754321;
    FLOP_VIRGIN     = $e5e5;

    FLOPPY_DSDD     = 0;
    FLOPPY_DSHD     = 1;
    FLOPPY_DSED     = 2;

{* Dbmsg() messages *}
    DB_NULLSTRING   = $f000;
    DB_COMMAND      = $f100;

{* Mfpint() vector indices *}
    MFP_PARALLEL    = 0;
    MFP_DCD         = 1;
    MFP_CTS         = 2;
    MFP_BITBLT      = 3;
    MFP_TIMERD      = 4;
    MFP_BAUDRATE    = MFP_TIMERD;
    MFP_200HZ       = 5;
    MFP_ACIA        = 6;
    MFP_DISK        = 7;
    MFP_TIMERB      = 8;
    MFP_HBLANK      = MFP_TIMERB;
    MFP_TERR        = 9;
    MFP_TBE         = 10;
    MFP_RERR        = 11;
    MFP_RBF         = 12;
    MFP_TIMERA      = 13;
    MFP_DMASOUND    = MFP_TIMERA;
    MFP_RING        = 14;
    MFP_MONODETECT  = 15;

{* Iorec() devices *}
    IO_SERIAL       = 0;
    IO_KEYBOARD     = 1;
    IO_MIDI         = 2;

{* Rsconf() speeds *}
    BAUD_19200      = 0;
    BAUD_9600       = 1;
    BAUD_4800       = 2;
    BAUD_3600       = 3;
    BAUD_2400       = 4;
    BAUD_2000       = 5;
    BAUD_1800       = 6;
    BAUD_1200       = 7;
    BAUD_600        = 8;
    BAUD_300        = 9;
    BAUD_200        = 10;
    BAUD_150        = 11;
    BAUD_134        = 12;
    BAUD_110        = 13;
    BAUD_75         = 14;
    BAUD_50         = 15;
    BAUD_INQUIRE    = -2;

{* Rsconf() params *}
    FLOW_NONE       = 0;
    FLOW_SOFT       = 1;
    FLOW_HARD       = 2;
    FLOW_BOTH       = 3;

    RS_RECVENABLE   = $01;
    RS_SYNCSTRIP    = $02;
    RS_MATCHBUSY    = $04;
    RS_BRKDETECT    = $08;
    RS_FRAMEERR     = $10;
    RS_PARITYERR    = $20;
    RS_OVERRUNERR   = $40;
    RS_BUFFUL       = $80;

    RS_ODDPARITY    = $02;
    RS_EVENPARITY   = $00;
    RS_PARITYENABLE = $04;

    RS_NOSTOP       = $00;
    RS_1STOP        = $08;
    RS_15STOP       = $10;
    RS_2STOP        = $18;

    RS_8BITS        = $00;
    RS_7BITS        = $20;
    RS_6BITS        = $40;
    RS_5BITS        = $60;

    RS_CLK16        = $80;

    RS_INQUIRE      = -1;
    RS_LASTBAUD     = -2;

{* Keytbl() param *}
    KT_NOCHANGE : Pointer   = Pointer(-1);

{* Protobt() params *}
    SERIAL_NOCHANGE = -1;
    SERIAL_RANDOM   = $01000001;

    DISK_NOCHANGE   = -1;
    DISK_SSSD       = 0;
    DISK_DSSD       = 1;
    DISK_SSDD       = 2;
    DISK_DSDD       = 3;
    DISK_DSHD       = 4;
    DISK_DSED       = 5;

    EXEC_NOCHANGE   = -1;
    EXEC_NO         = 0;
    EXEC_YES        = 1;

{* Giaccess() registers *}
    PSG_APITCHLOW   = 0;
    PSG_APITCHHIGH  = 1;
    PSG_BPITCHLOW   = 2;
    PSG_BPTICHHIGH  = 3;
    PSG_CPITCHLOW   = 4;
    PSG_CPITCHHIGH  = 5;
    PSG_NOISEPITCH  = 6;
    PSG_MODE        = 7;
    PSG_AVOLUME     = 8;
    PSG_BVOLUME     = 9;
    PSG_CVOLUME     = 10;
    PSG_FREQLOW     = 11;
    PSG_FREQHIGH    = 12;
    PSG_ENVELOPE    = 13;
    PSG_PORTA       = 14;
    PSG_PORTB       = 15;

    PSG_ENABLEA     = $01;
    PSG_ENABLEB     = $02;
    PSG_ENABLEC     = $04;
    PSG_NOISEA      = $08;
    PSG_NOISEB      = $10;
    PSG_NOISEC      = $20;
    PSG_PRTAOUT     = $40;
    PSG_PRTBOUT     = $80;

{* Bitmasks for Offgibit() *}
    GI_FLOPPYSIDE   = $01;
    GI_FLOPPYA      = $02;
    GI_FLOPPYB      = $04;
    GI_RTS          = $08;
    GI_DTR          = $10;
    GI_STROBE       = $20;
    GI_GPO          = $40;
    GI_SCCPORT      = $80;

{* Xbtimer() values *}
    XB_TIMERA       = 0;
    XB_TIMERB       = 1;
    XB_TIMERC       = 2;
    XB_TIMERD       = 3;

{* Dosound() param *}
    DS_INQUIRE      = -1;

{* Setprt() modes *}
    PRT_DOTMATRIX   = $01;
    PRT_MONO        = $02;
    PRT_ATARI       = $04;
    PRT_DRAFT       = $08;
    PRT_PARALLEL    = $10;
    PRT_CONTINUOUS  = $20;

    PRT_DAISY       = $01;
    PRT_COLOR       = $02;
    PRT_EPSON       = $04;
    PRT_FINAL       = $08;
    PRT_SERIAL      = $10;
    PRT_SINGLE      = $20;

    PRT_INQUIRE     = -1;

{* Kbrate() param *}
    KB_INQUIRE      = -1;

{* Floprate() seek rates *}
    FRATE_6         = 0;
    FRATE_12        = 1;
    FRATE_2         = 2;
    FRATE_3         = 3;
    FRATE_INQUIRE   = -1;

{* Bconmap() params *}
    BMAP_CHECK      = 0;
    BMAP_INQUIRE    = -1;
    BMAP_MAPTAB     = -2;

{* NVMaccess params *}
    NVM_READ        = 0;
    NVM_WRITE       = 1;
    NVM_RESET       = 2;

{* Blitmode() modes *}
    BLIT_SOFT       = 0;
    BLIT_HARD       = 1;

{* EsetShift() modes *}
    ST_LOW          = $0000;
    ST_MED          = $0100;
    ST_HIGH         = $0200;
    TT_MED          = $0400;
    TT_HIGH         = $0600;
    TT_LOW          = $0700;

    ES_GRAY         = 12;
    ES_SMEAR        = 15;

{* Esetbank() params *}
    ESB_INQUIRE     = -1;
    EC_INQUIRE      = -1;

{* EsetGray() modes *}
    ESG_INQUIRE     = -1;
    ESG_COLOR       = 0;
    ESG_GRAY        = 1;

{* EsetSmear() modes *}
    ESM_INQUIRE     = 1;
    ESM_NORMAL      = 0;
    ESM_SMEAR       = 1;

{* Bitmasks for Vsetmode() *}
    VERTFLAG    = $0100;
    STMODES     = $0080;
    OVERSCAN    = $0040;
    PAL         = $0020;
    VGA_FALCON  = $0010;
    TV          = $0000;

    COL80       = $08;
    COL40       = $00;

    BPS16       = 4;
    BPS8        = 3;
    BPS4        = 2;
    BPS2        = 1;
    BPS1        = 0;

    NUMCOLS     = 7;

    VM_INQUIRE  = -1;

{* Values returned by VgetMonitor() *}
    MON_MONO        = 0;
    MON_COLOR       = 1;
    MON_VGA         = 2;
    MON_TV          = 3;

{* VsetSync flags - 0=internal, 1=external *}
    VID_CLOCK   = 1;
    VID_VSYNC   = 2;
    VID_HSYNC   = 4;

{* VsetSync() params *}
    VCLK_EXTERNAL   = 0;
    VCLK_EXTVSYNC   = 1;
    VCLK_EXTHSYNC   = 2;

    OVERLAY_ON      = 1;
    OVERLAY_OFF     = 0;

{* Bitmasks for Dsp_RemoveInterrupts() *}
    RTS_OFF         = $01;
    RTR_OFF         = $02;

{* Dsp_Hf0() params *}
    HF_CLEAR        = 0;
    HF_SET          = 1;
    HF_INQUIRE      = -1;

{* Dsp_Hstat() bits *}
    ICR_RXDF        = 0;
    ICR_TXDE        = 1;
    ICR_TRDY        = 2;
    ICR_HF2         = 3;
    ICR_HF3         = 4;
    ICR_DMA         = 6;
    ICR_HREQ        = 7;

{* Dsp_SetVectors() params *}
    DSPSEND_NOTHING = $00000000;
    DSPSEND_ZERO    = $ff000000;

{* Dsp_MultBlocks() params *}
    BLOCK_LONG = 0;
    BLOCK_WORD = 1;
    BLOCK_UBYTE = 2;

{* _SND cookie values *}
    SND_PSG     = $01;  {* Yamaha PSG *}
    SND_8BIT    = $02;  {* 8-bit DMA *}
    SND_16BIT   = $04;  {* 16-bit CODEC *}
    SND_DSP     = $08;  {* DSP *}
    SND_MATRIX  = $10;  {* Connection matrix *}
    SND_EXT     = $20;  {* Extended XBIOS routines (Milan, GSXB) *}
    SND_GSXB    = SND_EXT;

{* Setbuffer regions *}
    SR_PLAY     = 0;    {* Set playback registers *}
    SR_RECORD   = 1;    {* Set record registers *}

{* Soundcmd Modes *}
    LTATTEN     = 0;    {* Left-channel output attenuation *}
    RTATTEN     = 1;    {* Right channel atten *}
    LTGAIN      = 2;    {* Left input gain *}
    RTGAIN      = 3;    {* Right channel gain *}
    {* gain and attenuation in 1.5 dB units, $00V0, V:0-15 *}
    ADDERIN     = 4;    {* Select inputs to adder 0=off, 1=on *}
    ADCIN       = 1;    {* Input from ADC *}
    MATIN       = 2;    {* Input from connection matrix *}
    ADCINPUT    = 5;    {* Select input to ADC, 0=mic, 1=PSG *}
    ADCRT       = 1;    {* Right channel input *}
    ADCLT       = 2;    {* Left input *}
    SETPRESCALE = 6;    {* Set TT compatibility prescaler *}
    PREMUTE     = 0;    {* was /1280, now is invalid, mutes *}
    PRE1280     = PREMUTE;
    PRE640      = 1;    {* divide by 640 *}
    PRE320      = 2;    {* / 320 *}
    PRE160      = 3;    {* / 160 *}

{* Record/Playback modes *}

    STEREO8     = 0;    {* 8 bit stereo *}
    STEREO16    = 1;    {* 16 bit stereo *}
    MONO8       = 2;    {* 8 bit mono *}

{* Record/Playback tracks range from 0 to 3 *}

{* XXX Doc for Settracks could be clearer. Can we individually set, e.g.,
   tracks 0 & 2 for playback, or must track selections be contiguous? *}

{* Sound buffer interrupts *}
    {* sources *}
    SI_TIMERA   = 0;    {* Timer A interrupt *}
    SI_MFPI7    = 1;    {* MFP interrupt 7 *}
    {* causes *}
    SI_NONE     = 0;    {* No interrupts *}
    SI_PLAY     = 1;    {* Intr at end of play buffer *}
    SI_RECORD   = 2;    {* Intr at end of record buffer *}
    SI_BOTH     = 3;    {* Interrupt for either play or record *}

{* Buffoper flags *}
    SB_PLA_ENA  = 1;    {* Play enable *}
    SB_PLA_RPT  = 2;    {* Play repeat (continuous loop mode) *}
    SB_REC_ENA  = 4;    {* Record enable *}
    SB_REC_RPT  = 8;    {* Record repeat *}

{* Dsptristate - 0=tristate, 1=enable *}

{* Gpio modes *}
    GPIO_SET    = 0;    {* Set I/O direction, 0=in, 1=out *}
    GPIO_READ   = 1;    {* Read bits - only 3 bits on gpio *}
    GPIO_WRITE  = 2;    {* Write gpio data bits *}

{* Devconnect (connection matrix) source devices *}
    DMAPLAY     = 0;    {* DMA playback *}
    DSPXMIT     = 1;    {* DSP transmit *}
    EXTINP      = 2;    {* External input *}
    ADC     = 3;    {* Microphone/PSG, see Soundcmd(ADCINPUT) *}

{* Devconnect destination devices, bitmapped *}
    DMAREC      = 1;    {* DMA record *}
    DSPRECV     = 2;    {* DSP receive *}
    EXTOUT      = 4;    {* External output *}
    DAC     = 8;    {* Headphone, internal speaker, monitor *}

{* Devconnect clock sources *}
    CLK25M      = 0;    {* Internal 25.175 MHz clock *}
    CLKEXT      = 1;    {* External clock *}
    CLK32M      = 2;    {* Internal 32 MHz. Invalid for CODEC *}

{* Devconnect clock prescaler values *}
    CLKOLD      = 0;    {* TT compatible, see Soundcmd(SETPRESCALE) *}
    CLK50K      = 1;    {* 49170 hz *}
    CLK33K      = 2;    {* 32780 hz *}
    CLK25K      = 3;    {* 24585 hz *}
    CLK20K      = 4;    {* 19668 hz *}
    CLK16K      = 5;    {* 16390 hz *}
        {*  6   (14049 hz) invalid for CODEC *}
    CLK12K      = 7;    {* 12292 hz *}
        {*  8   (10927 hz) invalid for CODEC *}
    CLK10K      = 9;    {* 9834 hz *}
        {*  10  (8940 hz) invalid for CODEC *}
    CLK8K       = 11;   {* 8195 hz *}
        {*  12  (7565 hz) invalid *}
        {*  13  (7024 hz) invalid *}
        {*  14  (6556 hz) invalid *}
        {*  15  (6146 hz) invalid *}

{* Sndstatus command *}
    SND_CHECK   = 0;    {* Check current status *}
    SND_RESET   = 1;    {* Reset sound system *}
    {*
     * Reset effects: DSP tristated, gain=atten=0, matrix reset,
     * ADDERIN=0, Mode=STEREO8, Play=Record=Monitor tracks=0,
     * interrupts disabled, buffer operation disabled.
     *}

{* Sndstatus status return *}
    SS_OK       = 0;    {* No errors *}
    SS_CTRL     = 1;    {* Invalid control field (Data assumed OK) *}
    SS_SYNC     = 2;    {* Invalid sync format (mutes) *}
    SS_SCLK     = 3;    {* Serial clock out of valid range (mutes) *}

    SS_RTCLIP   = $10;  {* Right channel is clipping *}
    SS_LTCLIP   = $20;  {* Left channel is clipping *}

    SS_ERROR    = $f;


{* Soundcmd() params *}
    LEFT_MIC        = $00;
    LEFT_PSG        = $02;
    RIGHT_MIC       = $00;
    RIGHT_PSG       = $01;

    SND_INQUIRE     = -1;

{* Value returned by Locksnd() *}
    SNDLOCKED       = -129;

{* Value returned by Unlocksnd() *}
    SNDNOTLOCK      = -128;

{* Setmode() modes *}
    MODE_STEREO8    = 0;
    MODE_STEREO16   = 1;
    MODE_MONO       = 2;

{* Dsptristate() params *}
    DSP_TRISTATE    = 0;
    DSP_ENABLE      = 1;

    HANDSHAKE       = 0;
    NO_SHAKE        = 1;

type
{* Structure returned by Iorec() *}
    PIORECORD = ^TIORECORD;
    TIORECORD = record
        ibuf:       Pointer;
        ibufsiz:    smallint;
        ibufhd:     smallint;
        ibuftl:     smallint;
        ibuflow:    smallint;
        ibufhi:     smallint;
    end;

{* Structure used by Initmouse() *}
    PMOUSE = ^TMOUSE;
    TMOUSE = record
        topmode:        Byte;
        buttons:        Byte;
        x_scale:        Byte;
        y_scale:        Byte;
        x_max:          smallint;
        y_max:          smallint;
        x_start:        smallint;
        y_start:        smallint;
    end;

{*
 * Structure returned by Kbdvbase()
 * no need to declare the functions cdecl,
 * since the arg is passed in a0 AND on stack
 *}
    PKBDVECS = ^TKBDVECS;
    TKBDVECS = record
        midivec:        Pointer;
        vkbderr:        Pointer;
        vmiderr:        Pointer;
        statvec:        Pointer;
        mousevec:       Pointer;
        clockvec:       Pointer;
        joyvec:         Pointer;
        midisys:        Pointer;
        ikdbsys:        Pointer;
        drvstat:        Byte;
    end;

{* Structure returned by Keytbl() *}
    PKEYTAB = ^TKEYTAB;
    TKEYTAB = record
        unshift:        Pointer;
        shift:          Pointer;
        capslock:       Pointer;
    {* Entries below available
     * when _AKP cookie is present.
     *}
        alt:            Pointer;
        altshift:       Pointer;
        altcaps:        Pointer;
   {* Entry below is available
    * on MilanTOS and as of FreeMiNT 1.16.1
    *}
        altgr:          Pointer;
    end;

{* Structure used by Prtblk() *}
    PPBDEF = ^TPBDEF;
    TPBDEF = record
        pb_scrptr:      Pointer;
        pb_offset:      smallint;
        pb_width:       smallint;
        pb_height:      smallint;
        pb_left:        smallint;
        pb_right:       smallint;
        pb_screz:       smallint;
        pb_prrez:       smallint;
        pb_colptr:      Pointer;
        pb_prtype:      smallint;
        pb_prport:      smallint;
        pb_mask:        Pointer;
    end;

{* Structure used by VgetRGB *}
    PRGB = ^TRGB;
    TRGB = record
        reserved: byte;
        red: byte;
        green: byte;
        blue: byte;
    end;

{* Structure used by Bconmap() *}
    PMAPTAB = ^TMAPTAB;
    TMAPTAB = record
        Bconstat: Pointer;
        Bconin: Pointer;
        Bcostat: Pointer;
        Bconout: Pointer;
        Rsconf: Pointer;
        iorec: PIORECORD;
    end;

{* Structure used by Bconmap() *}
    PBCONMAP = ^TBCONMAP;
    TBCONMAP = record
        maptab: PMAPTAB;
        maptabsize: smallint;
    end;

{* Structure used by Settime *}
    PBIOSTIME = ^TBIOSTIME;
    TBIOSTIME = bitpacked record
        year: 0..127;
        month: 0..15;
        day: 0..31;
        hour: 0..31;
        minute: 0..63;
        second: 0..31;
    end;

    PDSPBLOCK = ^TDSPBLOCK;
    TDSPBLOCK = record
        blocktype:      smallint;
        blocksize:      LongInt;
        blockaddr:      Pointer;
    end;

    PSndBuf = ^TSndBuf;
    TSndBuf = record
        play: Pointer;
        record_: Pointer;
        reserve1: longint;
        reserve2: longint;
    end;


    TLongIntFunc = Function: LongInt;

procedure xbios_Initmouse(typ: smallint; var param: TMOUSE; vec: Pointer); syscall 14 0;
function xbios_Ssbrk(amount: smallint): Pointer; syscall 14 1;
function xbios_physbase: pointer; syscall 14 2;
function xbios_logbase: pointer; syscall 14 3;
function xbios_getrez: longint; syscall 14 4;
procedure xbios_setscreen(laddr: pointer; paddr: pointer; rez: smallint); syscall 14 5;
procedure xbios_vsetscreen(laddr: pointer; paddr: pointer; rez: smallint; mode: smallint); syscall 14 5;
procedure xbios_setpalette(palette: pointer); syscall 14 6;
function xbios_setcolor(colornum: smallint; color: smallint): smallint; syscall 14 7;
function xbios_Floprd(buf, filler: Pointer; devno, sectno, trackno, sideno, count: smallint): smallint; syscall 14 8;
function xbios_Flopwr(buf, filler: Pointer; devno, sectno, trackno, sideno, count: smallint): smallint; syscall 14 9;
function xbios_Flopfmt(buf, filler: Pointer; devno, spt, trackno, sideno, interlv: smallint; magic: LongInt; virgin: Word): smallint; syscall 14 10;
procedure xbios_dbmsg(rsrvd: smallint; msg_num: smallint; msg_arg: longint); syscall 14 11;
procedure xbios_Midiws(cnt: smallint; ptr: Pointer); syscall 14 12;
procedure xbios_Mfpint(interno: smallint; vec: Pointer); syscall 14 13;
function xbios_Iorec(devno: smallint): PIORECORD; syscall 14 14;
function xbios_Rsconf(speed, flowctl, ucr, rsr, tsr, scr: smallint): LongInt; syscall 14 15;
function xbios_Keytbl(unshift, shift, capslock: Pointer): PKEYTAB; syscall 14 16;
function xbios_random: longint; syscall 14 17;
procedure xbios_Protobt(buf: Pointer; serialno: LongInt; disktype, execflag: smallint); syscall 14 18;
function xbios_Flopver(buf, filler: Pointer; devno, sectno, trackno, sideno, count: smallint): smallint; syscall 14 19;
procedure xbios_Scrdmp; syscall 14 20;
function xbios_Cursconf(mode, operand: smallint): smallint; syscall 14 21;
procedure xbios_Settime(datetime: LongInt); syscall 14 22;
function xbios_Gettime: LongInt; syscall 14 23;
procedure xbios_Bioskeys; syscall 14 24;
procedure xbios_Ikbdws(cnt: smallint; ptr: Pointer); syscall 14 25;
procedure xbios_Jdisint(intno: smallint); syscall 14 26;
procedure xbios_Jenabint(intno: smallint); syscall 14 27;
function xbios_Giaccess(data: Byte; regno: smallint): Byte; syscall 14 28;
procedure xbios_Offgibit(bitno: smallint); syscall 14 29;
procedure xbios_Ongibit(bitno: smallint); syscall 14 30;
procedure xbios_Xbtimer(timer, control, data: smallint; vec: Pointer); syscall 14 31;
procedure xbios_Dosound(ptr: Pointer); syscall 14 32;
function xbios_Setprt(config: smallint): smallint; syscall 14 33;
function xbios_Kbdvbase: PKBDVECS; syscall 14 34;
function xbios_Kbrate(initial, speed: smallint): smallint; syscall 14 35;
procedure xbios_Prtblk(var defptr: TPBDEF); syscall 14 36;
procedure xbios_vsync; syscall 14 37;
function xbios_Supexec(codeptr: TLongIntFunc): LongInt; syscall 14 38;
procedure xbios_Puntaes; syscall 14 39;
function xbios_Floprate(drive, seekrate: smallint): smallint; syscall 14 41;
function xbios_DMAread(sector: LongInt; count: smallint; buffer: Pointer; devno: smallint): LongInt; syscall 14 42;
function xbios_DMAwrite(sector: LongInt; count: smallint; buffer: Pointer; devno: smallint): LongInt; syscall 14 43;
function xbios_Bconmap(devno: smallint): LongInt; syscall 14 44;
function xbios_NVMaccess(op, start, count: smallint; ptr: Pointer): smallint; syscall 14 46;
{* 48-63 reserved for MetaDOS *}
function xbios_Blitmode(mode: smallint): smallint; syscall 14 64;
{* 65-79 used by CENTScreen *}
function xbios_EsetShift(shftmode: smallint): smallint; syscall 14 80;
function xbios_EgetShift: smallint; syscall 14 81;
function xbios_EsetBank(bankNum: smallint): smallint; syscall 14 82;
function xbios_EsetColor(colorNum, color: smallint): smallint; syscall 14 83;
procedure xbios_EsetPalette(colorNum, count: smallint; palettePtr: Pointer); syscall 14 84;
procedure xbios_EgetPalette(colorNum, count: smallint; palettePtr: Pointer); syscall 14 85;
function xbios_EsetGray(switch: smallint): smallint; syscall 14 86;
function xbios_EsetSmear(switch: smallint): smallint; syscall 14 87;
function xbios_Vsetmode(modecode: smallint): smallint; syscall 14 88;
function xbios_mon_type: smallint; syscall 14 89;
procedure xbios_VsetSync(flag: smallint); syscall 14 90;
function xbios_VgetSize(mode: smallint): LongInt; syscall 14 91;
procedure xbios_VsetRGB(index, count: smallint; xrgbArray: PRGB); syscall 14 93;
procedure xbios_VgetRGB(index, count: smallint; xrgbArray: PRGB); syscall 14 94;
function xbios_Validmode(mode: smallint): smallint; syscall 14 95;
procedure xbios_Dsp_DoBlock(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 96;
procedure xbios_Dsp_BlkHandShake(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 97;
procedure xbios_Dsp_BlkUnpacked(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 98;
procedure xbios_Dsp_InStream(data_in: Pointer; block_size, num_blocks: LongInt; var blocks_done: LongInt); syscall 14 99;
procedure xbios_Dsp_OutStream(data_out: Pointer; block_size, num_blocks: LongInt; var blocks_done: LongInt); syscall 14 100;
procedure xbios_Dsp_IOStream(data_in, data_out: Pointer; block_insize, block_outsize, num_blocks: LongInt;var blocks_done: LongInt); syscall 14 101;
procedure xbios_Dsp_RemoveInterrupts(mask: smallint); syscall 14 102;
function xbios_Dsp_GetWordSize: smallint; syscall 14 103;
function xbios_Dsp_Lock: smallint; syscall 14 104;
procedure xbios_Dsp_Unlock; syscall 14 105;
procedure xbios_Dsp_Available(var xavailable, yavailable: LongInt); syscall 14 106;
function xbios_Dsp_Reserve(xreserve, yreserve: LongInt): smallint; syscall 14 107;
function xbios_Dsp_LoadProg(filename: PAnsiChar; ability: smallint; buffer: Pointer): smallint; syscall 14 108;
procedure xbios_Dsp_ExecProg(codeptr: Pointer; codesize: LongInt; ability: smallint); syscall 14 109;
procedure xbios_Dsp_ExecBoot(codeptr: Pointer; codesize: LongInt; ability: smallint); syscall 14 110;
function xbios_Dsp_LodToBinary(filename: PAnsiChar; codeptr: Pointer): LongInt; syscall 14 111;
procedure xbios_Dsp_TriggerHC(vector: smallint); syscall 14 112;
function xbios_Dsp_RequestUniqueAbility: smallint; syscall 14 113;
function xbios_Dsp_GetProgAbility: smallint; syscall 14 114;
procedure xbios_Dsp_FlushSubroutines; syscall 14 115;
function xbios_Dsp_LoadSubroutine(codeptr: Pointer; codesize: LongInt; ability: smallint): smallint; syscall 14 116;
function xbios_Dsp_InqSubrAbility(ability: smallint): smallint; syscall 14 117;
function xbios_Dsp_RunSubroutine(handle: smallint): smallint; syscall 14 118;
function xbios_Dsp_Hf0(flag: smallint): smallint; syscall 14 119;
function xbios_Dsp_Hf1(flag: smallint): smallint; syscall 14 120;
function xbios_Dsp_Hf2: smallint; syscall 14 121;
function xbios_Dsp_Hf3: smallint; syscall 14 122;
procedure xbios_Dsp_BlkWords(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 123;
procedure xbios_Dsp_BlkBytes(data_in: Pointer; size_in: LongInt; data_out: Pointer; size_out: LongInt); syscall 14 124;
function xbios_Dsp_HStat: Byte; syscall 14 125;
procedure xbios_Dsp_SetVectors(receiver, transmitter: Pointer); syscall 14 126;
procedure xbios_Dsp_MultBlocks(numsend, numreceive: LongInt; sendblocks, receiveblocks: PDSPBLOCK); syscall 14 127;
function xbios_locksnd: LongInt; syscall 14 128;
function xbios_unlocksnd: LongInt; syscall 14 129;
function xbios_soundcmd(mode, data: smallint): LongInt; syscall 14 130;
function xbios_nsoundcmd(mode, data: smallint; data2: longint): LongInt; syscall 14 130;
function xbios_setbuffer(reg: smallint; begaddr, endaddr: Pointer): LongInt; syscall 14 131;
function xbios_setmode(mode: smallint): LongInt; syscall 14 132;
function xbios_settracks(playtracks, rectracks: smallint): LongInt; syscall 14 133;
function xbios_setmontracks(montrack: smallint): LongInt; syscall 14 134;
function xbios_setinterrupt(src_inter, cause: smallint): LongInt; syscall 14 135;
function xbios_buffoper(mode: smallint): LongInt; syscall 14 136;
function xbios_dsptristate(dspxmit, dsprec: smallint): LongInt; syscall 14 137;
function xbios_gpio(mode, data: smallint): LongInt; syscall 14 138;
function xbios_devconnect(src, dst, srcclk, prescale, protocol: smallint): LongInt; syscall 14 139;
function xbios_sndstatus(res: smallint): LongInt; syscall 14 140;
function xbios_buffptr(bptr: Pointer): LongInt; syscall 14 141;

procedure xbios_VsetMask(ormask, andmask: LongInt; overlay: smallint); syscall 14 150;

implementation

end.
