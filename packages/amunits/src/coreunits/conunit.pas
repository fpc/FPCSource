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

    Changed integer > smallint.
    09 Feb 2003.
}

unit conunit;

INTERFACE

uses exec, console, keymap, inputevent;

const
{ ---- console unit numbers for OpenDevice() }
 CONU_LIBRARY   = -1;      { no unit, just fill in IO_DEVICE field }
 CONU_STANDARD  = 0;       { standard unmapped console }

{ ---- New unit numbers for OpenDevice() - (V36) }

 CONU_CHARMAP   = 1;       { bind character map to console }
 CONU_SNIPMAP   = 3;       { bind character map w/ snip to console }

{ ---- New flag defines for OpenDevice() - (V37) }

 CONFLAG_DEFAULT               =  0;
 CONFLAG_NODRAW_ON_NEWSIZE     =  1;


    PMB_ASM     = M_LNM + 1;    { internal storage bit for AS flag }
    PMB_AWM     = PMB_ASM + 1;  { internal storage bit for AW flag }
    MAXTABS     = 80;


type

    pConUnit = ^tConUnit;
    tConUnit = record
        cu_MP   : tMsgPort;
        { ---- read only variables }
        cu_Window       : Pointer;      { (WindowPtr) intuition window bound to this unit }
        cu_XCP          : smallint;        { character position }
        cu_YCP          : smallint;
        cu_XMax         : smallint;        { max character position }
        cu_YMax         : smallint;
        cu_XRSize       : smallint;        { character raster size }
        cu_YRSize       : smallint;
        cu_XROrigin     : smallint;        { raster origin }
        cu_YROrigin     : smallint;
        cu_XRExtant     : smallint;        { raster maxima }
        cu_YRExtant     : smallint;
        cu_XMinShrink   : smallint;        { smallest area intact from resize process }
        cu_YMinShrink   : smallint;
        cu_XCCP         : smallint;        { cursor position }
        cu_YCCP         : smallint;

   { ---- read/write variables (writes must must be protected) }
   { ---- storage for AskKeyMap and SetKeyMap }

        cu_KeyMapStruct : tKeyMap;

   { ---- tab stops }

        cu_TabStops     : Array [0..MAXTABS-1] of Word;
                                { 0 at start, -1 at end of list }

   { ---- console rastport attributes }

        cu_Mask         : Shortint;
        cu_FgPen        : Shortint;
        cu_BgPen        : Shortint;
        cu_AOLPen       : Shortint;
        cu_DrawMode     : Shortint;
        cu_AreaPtSz     : Shortint;
        cu_AreaPtrn     : Pointer;      { cursor area pattern }
        cu_Minterms     : Array [0..7] of Byte; { console minterms }
        cu_Font         : Pointer;      { (TextFontPtr) }
        cu_AlgoStyle    : Byte;
        cu_TxFlags      : Byte;
        cu_TxHeight     : Word;
        cu_TxWidth      : Word;
        cu_TxBaseline   : Word;
        cu_TxSpacing    : Word;

   { ---- console MODES and RAW EVENTS switches }

        cu_Modes        : Array [0..(PMB_AWM+7) div 8 - 1] of Byte;
                                { one bit per mode }
        cu_RawEvents    : Array [0..(IECLASS_MAX+7) div 8 - 1] of Byte;
    end;

IMPLEMENTATION

end.
