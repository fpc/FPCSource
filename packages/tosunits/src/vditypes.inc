{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 Thorsten Otto

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{* return values for vq_vgdos() inquiry *}
const
    GDOS_NONE       = -2;           (* no GDOS installed           *)
    GDOS_FSM        = $5F46534D;    (* '_FSM' - FSMGDOS installed  *)
    GDOS_FNT        = $5F464E54;    (* '_FNT' - FONTGDOS installed *)
    GDOS_ATARI      = $0007E88A;    (* GDOS 1.1 von Atari Corp.    *)
    GDOS_AMC        = $0007E864;    (* AMCGDos von Arnd Beissner   *)
    GDOS_AMCLIGHT   = $0007E8BA;    (* GEMINI-Spezial-GDos von Arnd Beissner *)
    GDOS_NVDI       = $00000000;    (* NVDI von Bela GmbH *)
    GDOS_TTF        = $3e5d0957;    (* TTF-GDOS *)

const
{* vst_alignment modes *}
    TA_LEFT         = 0;
    TA_CENTER       = 1;
    TA_RIGHT        = 2;
    TA_BASELINE     = 0;
    TA_HALF         = 1;
    TA_ASCENT       = 2;
    TA_BOTTOM       = 3;
    TA_DESCENT      = 4;
    TA_TOP          = 5;

{* gsx modes *}
    MD_REPLACE      = 1;
    MD_TRANS        = 2;
    MD_XOR          = 3;
    MD_ERASE        = 4;

{$IFNDEF IP_HOLLOW_defined}
{* patterns (also in AES) *}
    IP_HOLLOW       = 0;
    IP_1PATT        = 1;
    IP_2PATT        = 2;
    IP_3PATT        = 3;
    IP_4PATT        = 4;
    IP_5PATT        = 5;
    IP_6PATT        = 6;
    IP_SOLID        = 7;
{$DEFINE IP_HOLLOW_defined}
{$ENDIF}

{* gsx styles *}
    FIS_HOLLOW      = 0;
    FIS_SOLID       = 1;
    FIS_PATTERN     = 2;
    FIS_HATCH       = 3;
    FIS_USER        = 4;

{* polymarker types *}
    MT_DOT          = 1;
    MT_PLUS         = 2;
    MT_ASTERISK     = 3;
    MT_SQUARE       = 4;
    MT_DCROSS       = 5;
    MT_DIAMOND      = 6;

{* linetypes *}
    LT_SOLID        = 1;
    LT_LONGDASH     = 2;
    LT_DOTTED       = 3;
    LT_DASHDOT      = 4;
    LT_DASHED       = 5;
    LT_DASHDOTDOT   = 6;
    LT_USERDEF      = 7;

{* line ends *}
    LE_SQUARED      = 0;
    LE_ARROWED      = 1;
    LE_ROUNDED      = 2;

{* text effects *}
    TF_NORMAL       = 0;
    TF_THICKENED    = 1;
    TF_LIGHTENED    = 2;
    TF_SLANTED      = 4;
    TF_UNDERLINED   = 8;
    TF_OUTLINED     = 16;
    TF_SHADOWED     = 32;

{* bit blt rules *}
    ALL_WHITE       = 0;
    S_AND_D         = 1;
    S_AND_NOTD      = 2;
    S_ONLY          = 3;
    NOTS_AND_D      = 4;
    D_ONLY          = 5;
    S_XOR_D         = 6;
    S_OR_D          = 7;
    NOT_SORD        = 8;
    NOT_SXORD       = 9;
    D_INVERT        = 10;
    S_OR_NOTD       = 11;
    NOT_D           = 12;
    NOTS_OR_D       = 13;
    NOT_SANDD       = 14;
    ALL_BLACK       = 15;

{* input mode *}
    MODE_REQUEST    = 1;
    MODE_SAMPLE     = 2;

{* vqin_mode & vsin_mode modes *}
    DEV_LOCATOR     = 1;
    DEV_VALUATOR    = 2;
    DEV_CHOICE      = 3;
    DEV_STRING      = 4;

{* v_bez modes *}
    BEZ_BEZIER      = $01;
    BEZ_POLYLINE    = $00;
    BEZ_NODRAW      = $02;

{* v_bit_image modes *}
    IMAGE_LEFT      = 0;
    IMAGE_CENTER    = 1;
    IMAGE_RIGHT     = 2;
    IMAGE_TOP       = 0;
    IMAGE_BOTTOM    = 2;

{* vq_color modes *}
    COLOR_REQUESTED     = 0;
    COLOR_ACTUAL        = 1;

{* vst_charmap modes *}
    MAP_BITSTREAM   = 0;
    MAP_ATARI       = 1;
    MAP_UNICODE     = 2; {* for vst_map_mode, NVDI 4 *}

{* vst_kern tmodes *}
    TRACK_NONE      = 0;
    TRACK_NORMAL    = 1;
    TRACK_TIGHT     = 2;
    TRACK_VERYTIGHT = 3;

{* vst_kern pmodes *}
    PAIR_OFF        = 0;
    PAIR_ON         = 1;

{* vst_scratch modes *}
    SCRATCH_BOTH        = 0;
    SCRATCH_BITMAP      = 1;
    SCRATCH_NONE        = 2;

{* definitions for vs_mute *}
    MUTE_RETURN     = -1;
    MUTE_ENABLE      = 0;
    MUTE_DISABLE     = 1;

{* definitions for v_orient *}
    OR_PORTRAIT      = 0;
    OR_LANDSCAPE     = 1;

{* definitions for v_tray *}
    TRAY_MANUAL     = -1;
    TRAY_DEFAULT     = 0;
    TRAY_FIRSTOPT    = 1;

{* definitions for v_xbit_image *}
    XBIT_FRACT       = 0;
    XBIT_INTEGER     = 1;

    XBIT_LEFT        = 0;
    XBIT_CENTER      = 1;
    XBIT_RIGHT       = 2;

    XBIT_TOP         = 0;
    XBIT_MIDDLE      = 1;
    XBIT_BOTTOM      = 2;


type
  PCOLOR_RGB = ^TCOLOR_RGB;
  TCOLOR_RGB = record
      reserved: word;     {* Set to 0 or the index of the entry *}
      red: word;          {* Red:   0<->65535 *}
      green: word;        {* Green: 0<->65535 *}
      blue: word;         {* Blue:  0<->65535 *}
  end;

{$WARNING type TCOLOR_ENTRY is incomplete}
type
  TCOLOR_ENTRY = record
    case byte of
      0: ( rgb: TCOLOR_RGB; );
      1: ( cymk: array[0..1] of longint; ); // dummy
  end;

type
  PCOLOR_TAB = ^TCOLOR_TAB;
  TCOLOR_TAB = record             {* Colour table                    *}
      magic: array[0..3] of char; {* 'ctab'                          *}
      length: longint;
      format: longint;            {* Format (0)                      *}
      reserved: longint;          {* Reserved, set to 0              *}
      map_id: longint;            {* Colour table ID                 *}
      color_space: longint;       {* Colour space (at present only
                                     CSPACE_RGB)                     *}
      flags: longint;             {* VDI-internal flags, set to 0    *}
      no_colors: longint;         {* Number of colour entries        *}
      reserved1: longint;         {* Reserved, must be 0             *}
      reserved2: longint;         {* Reserved, must be 0             *}
      reserved3: longint;         {* Reserved, must be 0             *}
      reserved4: longint;         {* Reserved, must be 0             *}
      colors: array[0..0] of TCOLOR_ENTRY; { repeated no_colors times }
  end;

type
  PPOINT16 = ^TPOINT16;
  TPOINT16 = record               {* Point for 16-bit coordinates *}
      x: smallint;
      y: smallint;
  end;

type
  PPOINT32 = ^TPOINT32;
  TPOINT32 = record               {* Point for 32-bit coordinates *}
      x: longint;
      y: longint;
  end;

type
  PRECT16 = ^TRECT16;
  TRECT16 = record                {* Rectangle for 16-bit coordinates *}
      x1: smallint;
      y1: smallint;
      x2: smallint;
      y2: smallint;
  end;

type
  PRECT32 = ^TRECT32;
  TRECT32 = record                {* Rectangle for 32-bit coordinates *}
      x1: longint;
      y1: longint;
      x2: longint;
      y2: longint;
  end;

type
  PMFDB = ^TMFDB;
  TMFDB = record
      fd_addr: pointer;          {* Pointer to the start of the
                                    memory block, e.g. the
                                    screen memory base address  *}
      fd_w: smallint;            {* Width in pixels             *}
      fd_h: smallint;            {* Height in pixels            *}
      fd_wdwidth: smallint;      {* Width of a line in words    *}
      fd_stand: smallint;        {* 0 = Device-specific format  *}
                                 {* 1 = Standard format         *}
      fd_nplanes: smallint;      {* Number of planes            *}
      fd_r1: smallint;           {* Reserved, must be 0         *}
      fd_r2: smallint;           {* Reserved, must be 0         *}
      fd_r3: smallint;           {* Reserved, must be 0         *}
  end;

type
    String33    = String[33];
    String80    = String[80];
    String125   = String[125];

type
  PVDIContrl = ^TVDIContrl;
  TVDIContrl = ARRAY[0..14] of smallint;

  PVDIPtsIn  = ^TVDIPtsIn;
  TVDIPtsIn  = array[0..1023] of smallint;

  PVDIPtsOut = ^TVDIPtsOut;
  TVDIPtsOut = array[0..255] of smallint;

  PVDIIntIn  = ^TVDIIntIn;
  TVDIIntIn  = array[0..1023] of smallint;

  PVDIIntOut = ^TVDIIntOut;
  TVDIIntOut = array[0..511] of smallint;

type
  PVDIPB = ^TVDIPB;
  TVDIPB = record
      control: PVDIContrl;       {* Pointer to contrl array *}
      intin: PVDIIntIn;          {* Pointer to intin array  *}
      ptsin: PVDIPtsIn;          {* Pointer to ptsin array  *}
      intout: PVDIIntOut;        {* Pointer to intout array *}
      ptsout: PVDIPtsOut;        {* Pointer to ptsout array *}
  end;

    PFONT_HDR = ^TFONT_HDR;
    TFONT_HDR = record
        font_id         : smallint;
        point           : smallint;
        name            : Array[0..31] of Char;
        first_ade       : Word;
        last_ade        : Word;
        top             : Word;
        ascent          : Word;
        half            : Word;
        descent         : Word;
        bottom          : Word;
        max_char_width  : Word;
        max_cell_width  : Word;
        left_offset     : Word;
        right_offset    : Word;
        thicken         : Word;
        ul_size         : Word;
        lighten         : Word;
        skew            : Word;
        flags           : Word;
        hor_table       : Pointer;
        off_table       : Pointer;
        dat_table       : Pointer;
        form_width      : Word;
        form_height     : Word;
        next_font       : PFONT_HDR;
    end;

type
    fix31 = LongInt;
    ARRAY_2     = ARRAY[0..1] of smallint;
    ARRAY_3     = ARRAY[0..2] of smallint;
    ARRAY_4     = ARRAY[0..3] of smallint;
    ARRAY_5     = ARRAY[0..4] of smallint;
    ARRAY_6     = ARRAY[0..5] of smallint;
    ARRAY_10    = ARRAY[0..9] of smallint;
    ARRAY_273   = ARRAY[0..272] of smallint;
