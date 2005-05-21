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

{
    History:

    Update for AmigaOS 3.9.
    31 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se
}

unit prtgfx;

INTERFACE

uses exec,utility;
Const

    PCMYELLOW           = 0;            { byte index for yellow }
    PCMMAGENTA          = 1;            { byte index for magenta }
    PCMCYAN             = 2;            { byte index for cyan }
    PCMBLACK            = 3;            { byte index for black }
    PCMBLUE             = PCMYELLOW;    { byte index for blue }
    PCMGREEN            = PCMMAGENTA;   { byte index for green }
    PCMRED              = PCMCYAN;      { byte index for red }
    PCMWHITE            = PCMBLACK;     { byte index for white }

Type
    PUWORD = ^UWORD;

    pColorEntry = ^tColorEntry;
    tcolorEntry = record
        colorLong       : ULONG;      { quick access to all of YMCB }
        colorByte       : Array [0..3] of Byte; { 1 entry for each of YMCB }
        colorSByte      : Array [0..3] of Shortint; { ditto (except signed) }
    end;

    pPrtInfo = ^tPrtInfo;
    tPrtInfo = record { printer info }
        pi_render       : Pointer;      { PRIVATE - DO NOT USE! }
        pi_rp           : Pointer;      { PRIVATE - DO NOT USE! (RastPortPtr)}
        pi_temprp       : Pointer;      { PRIVATE - DO NOT USE! (RastPortPtr)}
        pi_RowBuf       : Pointer;      { PRIVATE - DO NOT USE! }
        pi_HamBuf       : Pointer;      { PRIVATE - DO NOT USE! }
        pi_ColorMap     : pcolorEntry;  { PRIVATE - DO NOT USE! }
        pi_ColorInt     : pcolorEntry;  { color intensities for entire row }
        pi_HamInt       : pcolorEntry;  { PRIVATE - DO NOT USE! }
        pi_Dest1Int     : pcolorEntry;  { PRIVATE - DO NOT USE! }
        pi_Dest2Int     : pcolorEntry;  { PRIVATE - DO NOT USE! }
        pi_ScaleX       : Pointer;      { array of scale values for X }
        pi_ScaleXAlt    : Pointer;      { PRIVATE - DO NOT USE! }
        pi_dmatrix      : Pointer;      { pointer to dither matrix }
        pi_TopBuf       : Pointer;      { PRIVATE - DO NOT USE! }
        pi_BotBuf       : Pointer;      { PRIVATE - DO NOT USE! }

        pi_RowBufSize   : Word;         { PRIVATE - DO NOT USE! }
        pi_HamBufSize   : Word;         { PRIVATE - DO NOT USE! }
        pi_ColorMapSize : Word;         { PRIVATE - DO NOT USE! }
        pi_ColorIntSize : Word;         { PRIVATE - DO NOT USE! }
        pi_HamIntSize   : Word;         { PRIVATE - DO NOT USE! }
        pi_Dest1IntSize : Word;         { PRIVATE - DO NOT USE! }
        pi_Dest2IntSize : Word;         { PRIVATE - DO NOT USE! }
        pi_ScaleXSize   : Word;         { PRIVATE - DO NOT USE! }
        pi_ScaleXAltSize : Word;        { PRIVATE - DO NOT USE! }

        pi_PrefsFlags   : Word;         { PRIVATE - DO NOT USE! }
        pi_special      : ULONG;        { PRIVATE - DO NOT USE! }
        pi_xstart       : Word;         { PRIVATE - DO NOT USE! }
        pi_ystart       : Word;         { PRIVATE - DO NOT USE! }
        pi_width        : Word;         { source width (in pixels) }
        pi_height       : Word;         { PRIVATE - DO NOT USE! }
        pi_pc           : ULONG;        { PRIVATE - DO NOT USE! }
        pi_pr           : ULONG;        { PRIVATE - DO NOT USE! }
        pi_ymult        : Word;         { PRIVATE - DO NOT USE! }
        pi_ymod         : Word;         { PRIVATE - DO NOT USE! }
        pi_ety          : Shortint;     { PRIVATE - DO NOT USE! }
        pi_xpos         : Word;         { offset to start printing picture }
        pi_threshold    : Word;         { threshold value (from prefs) }
        pi_tempwidth    : Word;         { PRIVATE - DO NOT USE! }
        pi_flags        : Word;         { PRIVATE - DO NOT USE! }
        { V44 additions }
        pi_ReduceBuf : PUWORD;          { PRIVATE - DO NOT USE! }
        pi_ReduceBufSize : UWORD;       { PRIVATE - DO NOT USE! }
        pi_SourceHook : PHook;          { PRIVATE - DO NOT USE! }
        pi_InvertHookBuf : PULONG;      { RESERVED - DO NOT USE! }
    end;

IMPLEMENTATION

end.
