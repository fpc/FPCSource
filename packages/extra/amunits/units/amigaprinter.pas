{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit amigaprinter;

INTERFACE

uses exec, graphics;


Const

    PRD_RAWWRITE        = CMD_NONSTD + 0;
    PRD_PRTCOMMAND      = CMD_NONSTD + 1;
    PRD_DUMPRPORT       = CMD_NONSTD + 2;
    PRD_QUERY           = CMD_NONSTD + 3;

{ printer command definitions }

    aRIS        = 0;            { ESCc  reset                   ISO }
    aRIN        = 1;            { ESC#1 initialize              +++ }
    aIND        = 2;            { ESCD  lf                      ISO }
    aNEL        = 3;            { ESCE  return,lf               ISO }
    aRI         = 4;            { ESCM  reverse lf              ISO }

    aSGR0       = 5;            { ESC[0m normal char set        ISO }
    aSGR3       = 6;            { ESC[3m italics on             ISO }
    aSGR23      = 7;            { ESC[23m italics off           ISO }
    aSGR4       = 8;            { ESC[4m underline on           ISO }
    aSGR24      = 9;            { ESC[24m underline off         ISO }
    aSGR1       = 10;           { ESC[1m boldface on            ISO }
    aSGR22      = 11;           { ESC[22m boldface off          ISO }
    aSFC        = 12;           { SGR30-39 set foreground color ISO }
    aSBC        = 13;           { SGR40-49 set background color ISO }

    aSHORP0     = 14;           { ESC[0w normal pitch           DEC }
    aSHORP2     = 15;           { ESC[2w elite on               DEC }
    aSHORP1     = 16;           { ESC[1w elite off              DEC }
    aSHORP4     = 17;           { ESC[4w condensed fine on      DEC }
    aSHORP3     = 18;           { ESC[3w condensed off          DEC }
    aSHORP6     = 19;           { ESC[6w enlarged on            DEC }
    aSHORP5     = 20;           { ESC[5w enlarged off           DEC }

    aDEN6       = 21;           { ESC[6"z shadow print on       DEC (sort of) }
    aDEN5       = 22;           { ESC[5"z shadow print off      DEC }
    aDEN4       = 23;           { ESC[4"z doublestrike on       DEC }
    aDEN3       = 24;           { ESC[3"z doublestrike off      DEC }
    aDEN2       = 25;           { ESC[2"z NLQ on                DEC }
    aDEN1       = 26;           { ESC[1"z NLQ off               DEC }

    aSUS2       = 27;           { ESC[2v superscript on         +++ }
    aSUS1       = 28;           { ESC[1v superscript off        +++ }
    aSUS4       = 29;           { ESC[4v subscript on           +++ }
    aSUS3       = 30;           { ESC[3v subscript off          +++ }
    aSUS0       = 31;           { ESC[0v normalize the line     +++ }
    aPLU        = 32;           { ESCL  partial line up         ISO }
    aPLD        = 33;           { ESCK  partial line down       ISO }

    aFNT0       = 34;           { ESC(B US char set or Typeface  0 (default) }
    aFNT1       = 35;           { ESC(R French char set or Typeface  1 }
    aFNT2       = 36;           { ESC(K German char set or Typeface  2 }
    aFNT3       = 37;           { ESC(A UK char set or Typeface  3 }
    aFNT4       = 38;           { ESC(E Danish I char set or Typeface  4 }
    aFNT5       = 39;           { ESC(H Sweden char set or Typeface  5 }
    aFNT6       = 40;           { ESC(Y Italian char set or Typeface  6 }
    aFNT7       = 41;           { ESC(Z Spanish char set or Typeface  7 }
    aFNT8       = 42;           { ESC(J Japanese char set or Typeface  8 }
    aFNT9       = 43;           { ESC(6 Norweign char set or Typeface  9 }
    aFNT10      = 44;           { ESC(C Danish II char set or Typeface 10 }

{
        Suggested typefaces are:

         0 - default typeface.
         1 - Line Printer or equiv.
         2 - Pica or equiv.
         3 - Elite or equiv.
         4 - Helvetica or equiv.
         5 - Times Roman or equiv.
         6 - Gothic or equiv.
         7 - Script or equiv.
         8 - Prestige or equiv.
         9 - Caslon or equiv.
        10 - Orator or equiv.
}

    aPROP2      = 45;           { ESC[2p  proportional on       +++ }
    aPROP1      = 46;           { ESC[1p  proportional off      +++ }
    aPROP0      = 47;           { ESC[0p  proportional clear    +++ }
    aTSS        = 48;           { ESC[n E set proportional offset ISO }
    aJFY5       = 49;           { ESC[5 F auto left justify     ISO }
    aJFY7       = 50;           { ESC[7 F auto right justify    ISO }
    aJFY6       = 51;           { ESC[6 F auto full justify     ISO }
    aJFY0       = 52;           { ESC[0 F auto justify off      ISO }
    aJFY3       = 53;           { ESC[3 F letter space (justify) ISO (special) }
    aJFY1       = 54;           { ESC[1 F word fill(auto center) ISO (special) }

    aVERP0      = 55;           { ESC[0z  1/8" line spacing     +++ }
    aVERP1      = 56;           { ESC[1z  1/6" line spacing     +++ }
    aSLPP       = 57;           { ESC[nt  set form length n     DEC }
    aPERF       = 58;           { ESC[nq  perf skip n (n>0)     +++ }
    aPERF0      = 59;           { ESC[0q  perf skip off         +++ }

    aLMS        = 60;           { ESC#9   Left margin set       +++ }
    aRMS        = 61;           { ESC#0   Right margin set      +++ }
    aTMS        = 62;           { ESC#8   Top margin set        +++ }
    aBMS        = 63;           { ESC#2   Bottom marg set       +++ }
    aSTBM       = 64;           { ESC[Pn1;Pn2r  T&B margins     DEC }
    aSLRM       = 65;           { ESC[Pn1;Pn2s  L&R margin      DEC }
    aCAM        = 66;           { ESC#3   Clear margins         +++ }

    aHTS        = 67;           { ESCH    Set horiz tab         ISO }
    aVTS        = 68;           { ESCJ    Set vertical tabs     ISO }
    aTBC0       = 69;           { ESC[0g  Clr horiz tab         ISO }
    aTBC3       = 70;           { ESC[3g  Clear all h tab       ISO }
    aTBC1       = 71;           { ESC[1g  Clr vertical tabs     ISO }
    aTBC4       = 72;           { ESC[4g  Clr all v tabs        ISO }
    aTBCALL     = 73;           { ESC#4   Clr all h & v tabs    +++ }
    aTBSALL     = 74;           { ESC#5   Set default tabs      +++ }
    aEXTEND     = 75;           { ESC[Pn"x extended commands    +++ }

    aRAW        = 76;           { ESC[Pn"r Next 'Pn' chars are raw +++ }

Type

    pIOPrtCmdReq = ^tIOPrtCmdReq;
    tIOPrtCmdReq = record
        io_Message      : tMessage;
        io_Device       : pDevice;      { (DevicePtr) device node pointer  }
        io_Unit         : pUnit;        { (UnitPtr) unit (driver private)}
        io_Command      : Word;         { device command }
        io_Flags        : Byte;
        io_Error        : Shortint;     { error or warning num }
        io_PrtCommand   : Word;         { printer command }
        io_Parm0        : Byte;         { first command parameter }
        io_Parm1        : Byte;         { second command parameter }
        io_Parm2        : Byte;         { third command parameter }
        io_Parm3        : Byte;         { fourth command parameter }
    end;


    pIODRPReq = ^tIODRPReq;
    tIODRPReq = record
        io_Message      : tMessage;
        io_Device       : pDevice;      { (DevicePtr) device node pointer  }
        io_Unit         : pUnit;        { (UnitPtr) unit (driver private)}
        io_Command      : Word;         { device command }
        io_Flags        : Byte;
        io_Error        : Shortint;     { error or warning num }
        io_RastPort     : pRastPort;    { (RastPortPtr) raster port }
        io_ColorMap     : pColorMap;    { (ColorMapPtr) color map }
        io_Modes        : ULONG;        { graphics viewport modes }
        io_SrcX         : Word;         { source x origin }
        io_SrcY         : Word;         { source y origin }
        io_SrcWidth     : Word;         { source x width }
        io_SrcHeight    : Word;         { source x height }
        io_DestCols     : Longint;      { destination x width }
        io_DestRows     : Longint;      { destination y height }
        io_Special      : Word;         { option flags }
    end;

Const

    SPECIAL_MILCOLS     = $0001;        { DestCols specified in 1/1000" }
    SPECIAL_MILROWS     = $0002;        { DestRows specified in 1/1000" }
    SPECIAL_FULLCOLS    = $0004;        { make DestCols maximum possible }
    SPECIAL_FULLROWS    = $0008;        { make DestRows maximum possible }
    SPECIAL_FRACCOLS    = $0010;        { DestCols is fraction of FULLCOLS }
    SPECIAL_FRACROWS    = $0020;        { DestRows is fraction of FULLROWS }
    SPECIAL_CENTER      = $0040;        { center image on paper }
    SPECIAL_ASPECT      = $0080;        { ensure correct aspect ratio }
    SPECIAL_DENSITY1    = $0100;        { lowest resolution (dpi) }
    SPECIAL_DENSITY2    = $0200;        { next res }
    SPECIAL_DENSITY3    = $0300;        { next res }
    SPECIAL_DENSITY4    = $0400;        { next res }
    SPECIAL_DENSITY5    = $0500;        { next res }
    SPECIAL_DENSITY6    = $0600;        { next res }
    SPECIAL_DENSITY7    = $0700;        { highest res }
    SPECIAL_NOFORMFEED  = $0800;        { don't eject paper on gfx prints }
    SPECIAL_TRUSTME     = $1000;        { don't reset on gfx prints }

{
        Compute print size, set 'io_DestCols' and 'io_DestRows' in the calling
        program's 'IODRPReq' structure and exit, DON'T PRINT.  This allows the
        calling program to see what the final print size would be in printer
        pixels.  Note that it modifies the 'io_DestCols' and 'io_DestRows'
        fields of your 'IODRPReq' structure.  Also, set the print density and
        update the 'MaxXDots', 'MaxYDots', 'XDotsInch', and 'YDotsInch' fields
        of the 'PrinterExtendedData' structure.
}

    SPECIAL_NOPRINT     = $2000;        { see above }

    PDERR_NOERR         = 0;            { clean exit, no errors }
    PDERR_CANCEL        = 1;            { user cancelled print }
    PDERR_NOTGRAPHICS   = 2;            { printer cannot output graphics }
    PDERR_INVERTHAM     = 3;            { OBSOLETE }
    PDERR_BADDIMENSION  = 4;            { print dimensions illegal }
    PDERR_DIMENSIONOVFLOW       = 5;    { OBSOLETE }
    PDERR_INTERNALMEMORY        = 6;    { no memory for internal variables }
    PDERR_BUFFERMEMORY  = 7;            { no memory for print buffer }

{
        Note : this is an internal error that can be returned from the render
        function to the printer device.  It is NEVER returned to the user.
        If the printer device sees this error it converts it 'PDERR_NOERR'
        and exits gracefully.  Refer to the document on
        'How to Write a Graphics Printer Driver' for more info.
}

    PDERR_TOOKCONTROL   = 8;            { Took control in case 0 of render }

{ internal use }

    SPECIAL_DENSITYMASK = $0700;        { masks out density values }
    SPECIAL_DIMENSIONSMASK = SPECIAL_MILCOLS + SPECIAL_MILROWS +
                        SPECIAL_FULLCOLS + SPECIAL_FULLROWS + SPECIAL_FRACCOLS +
                        SPECIAL_FRACROWS + SPECIAL_ASPECT;

IMPLEMENTATION

end.


