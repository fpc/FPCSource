{
    Copyright (c) 2016 by Free Pascal development team

    BIOS interface unit for Atari TOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}
{$IFNDEF FPC_DOTTEDUNITS}
unit bios;
{$ENDIF FPC_DOTTEDUNITS}

interface

const
{* Device codes for Bconin(), Bconout(), Bcostat(), Bconstat() *}
    _PRT    = 0;
    _AUX    = 1;
    _CON    = 2;
    _MIDI   = 3;
    _IKBD   = 4;
    _RAWCON = 5;

    DEV_PRINTER     = _PRT;
    DEV_AUX         = _AUX;
    DEV_CONSOLE     = _CON;
    DEV_MIDI        = _MIDI;
    DEV_IKBD        = _IKBD;
    DEV_RAW         = _RAWCON;

{* Mode bitmask used in Rwabs() *}
    RW_READ         = 0;
    RW_WRITE        = 0;
    RW_NOMEDIACH    = 1;
    RW_NORETRIES    = 2;
    RW_NOTRANSLATE  = 3;

{* Vector numbers used in Setexc() *}
    VEC_BUSERROR    = $02;
    VEC_ADDRESSERROR    = $03;
    VEC_ILLEGALINSTRUCTION  = $04;
    VEC_GEMDOS      = $21;
    VEC_GEM         = $22;
    VEC_BIOS        = $2d;
    VEC_XBIOS       = $2e;
    VEC_TIMER       = $100;
    VEC_CRITICALERROR   = $101;
    VEC_CRITICALERR = VEC_CRITICALERROR;
    VEC_TERMINATE   = $102;
    VEC_PROCTERM    = VEC_TERMINATE;
    VEC_INQUIRE:    Pointer = Pointer(-1);

{* Values returned by Mediach() *}
    MED_NOCHANGE    = 0;
    MED_UNKNOWN     = 1;
    MED_CHANGED     = 2;

{* Mode bitmask for Kbshift() *}
    K_RSHIFT        = $0001;
    K_LSHIFT        = $0002;
    K_SHIFT         = $0003;
    K_CTRL          = $0004;
    K_ALT           = $0008;
    K_CAPSLOCK      = $0010;
    K_CLRHOME       = $0020;
    K_INSERT        = $0040;


type
    PMD = ^TMD;
    TMD = record
        m_link :        PMD;
        m_start :       Pointer;
        m_length :      LongInt;
        m_own :         Pointer; {* to PD *}
    end;

    PMPB = ^TMPB;
    TMPB = record
        mp_mfl :        PMD;
        mp_mal :        PMD;
        mp_rover :      PMD;
    end;

    PBPB = ^TBPB;
    TBPB = record
        recsiz :        word;
        clsiz :         word;
        clsizb :        word;
        rdlen :         word;
        fsiz :          word;
        fatrec :        word;
        datrec :        word;
        numcl :         word;
        bflags :        word;
    end;

procedure bios_Getmpb(var p_mpb: TMPB); syscall 13 0;
function bios_Bconstat(dev: smallint): smallint; syscall 13 1;
function bios_Bconin(dev: smallint): LongInt; syscall 13 2;
procedure bios_Bconout(dev, c: smallint); syscall 13 3;
function bios_Rwabs(rwflag: smallint; buf: Pointer; count, recno, dev: smallint; lrecno: LongInt): LongInt; syscall 13 4;
function bios_Setexc(vecnum: smallint; vec: Pointer): Pointer; syscall 13 5;
function bios_Tickcal: LongInt; syscall 13 6;
function bios_Getbpb(dev: smallint): PBPB; syscall 13 7;
function bios_Bcostat(dev: smallint): LongInt; syscall 13 8;
function bios_Mediach(dev: smallint): LongInt; syscall 13 9;
function bios_Drvmap: LongInt; syscall 13 10;
function bios_Kbshift(mode: smallint): LongInt; syscall 13 11;

implementation

end.
