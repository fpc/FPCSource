{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    MorphOS version (c) 2015 by Karoly Balogh

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}

unit diskfont;

interface

uses
  exec, agraphics, utility;

const
  MAXFONTPATH = 256;

type
  PFontContents = ^TFontContents;
  TFontContents = record
    fc_FileName: array[0..MAXFONTPATH - 1] of Char;
    fc_YSize: Word;
    fc_Style: Byte;
    fc_Flags: Byte;
  end;

  PTFontContents = ^TTFontContents;
  TTFontContents = record
    tfc_FileName: array[0..MAXFONTPATH - 3] of Char;
    tfc_TagCount: Word;
    tfc_YSize: Word;
    tfc_Style: Byte;
    tfc_Flags: Byte;
  end;

const
  FCH_ID  = $0f00;
  TFCH_ID = $0f02;
  OFCH_ID = $0f03;

type
  PFontContentsHeader = ^TFontContentsHeader;
  TFontContentsHeader = record
    fch_FileID: Word;
    fch_NumEntries: Word;
  end;

const
  DFH_ID = $0f80;

  MAXFONTNAME = 32;

type
  PDiskFontHeader = ^TDiskFontHeader;
  TDiskFontHeader = record
    dfh_DF: TNode;
    dfh_FileID: Word;
    dfh_Revision: Word;
    dfh_Segment: LongInt;
    dfh_Name: array[0..MAXFONTNAME - 1] of Char;
    dfh_TF: TTextFont;
  end;

const
  AFB_MEMORY = 0;
  AFF_MEMORY = 1 shl AFB_MEMORY;
  AFB_DISK   = 1;
  AFF_DISK   = 1 shl AFB_DISK;
  AFB_SCALED = 2;
  AFF_SCALED = 1 shl AFB_SCALED;
  AFB_BITMAP = 3;
  AFF_BITMAP = 1 shl AFB_BITMAP;

  AFB_TAGGED = 16;
  AFF_TAGGED = 1 shl AFB_TAGGED;

type
  PAvailFonts = ^TAvailFonts;
  TAvailFonts = record
    af_Type: Word;
    af_Attr: TTextAttr;
  end;

  PTAvailFonts = ^TTAvailFonts;
  TTAvailFonts = record
    taf_Type: Word;
    taf_Attr: tTTextAttr;
  end;

  PAvailFontsHeader = ^TAvailFontsHeader;
  TAvailFontsHeader = record
    afh_NumEntries: Word;
  end;

// diskfont tag defines
const
  OT_Level0   = TAG_USER;
  OT_Level1   = TAG_USER or $1000;
  OT_Level2   = TAG_USER or $2000;
  OT_Level3   = TAG_USER or $3000;
  OT_Indirect = $8000;

  OT_DeviceDPI      = OT_Level0 or $01;
  OT_DotSize        = OT_Level0 or $02;
  OT_PointHeight    = OT_Level0 or $08;
  OT_SetFactor      = OT_Level0 or $09;
  OT_ShearSin       = OT_Level0 or $0a;
  OT_ShearCos       = OT_Level0 or $0b;
  OT_RotateSin      = OT_Level0 or $0c;
  OT_RotateCos      = OT_Level0 or $0d;
  OT_EmboldenX      = OT_Level0 or $0e;
  OT_EmboldenY      = OT_Level0 or $0f;
  OT_PointSize      = OT_Level0 or $10;
  OT_GlyphCode      = OT_Level0 or $11;
  OT_GlyphCode2     = OT_Level0 or $12;
  OT_GlyphWidth     = OT_Level0 or $13;
  OT_OTagPath       = OT_Level0 or OT_Indirect or $14;
  OT_OTagList       = OT_Level0 or OT_Indirect or $15;
  OT_GlyphMap       = OT_Level0 or OT_Indirect or $20;
  OT_WidthList      = OT_Level0 or OT_Indirect or $21;
  OT_TextKernPair   = OT_Level0 or OT_Indirect or $22;
  OT_DesignKernPair = OT_Level0 or OT_Indirect or $23;
  OT_UnderLined     = OT_Level0 or $24;
  OT_StrikeThrough  = OT_Level0 or $25;
  OT_GlyphMap8Bits  = OT_Level0 or OT_Indirect or $50;

// More sane support of real bold and italic fonts via families and/or algostyling
  OT_StyleFlags        = OT_Level0 or $101;  // Obtain with or OT_Indirect

{ Setting OTSF_Designed flags tells engine to try to open a styled
  font in the same family, failing that it will algorithmically create
  the right style = if you require only real designed styles, obtain the
  actual flags afterwards and compare against desired result;.

  OTSF_Algo flags tells engine to algorithmically style the font for
  you, this can be applied on top of OTSF_Designed to achieve whichever
  effect you need.}

  OTSF_DesignedBold   = 1 shl 0;
  OTSF_DesignedItalic = 1 shl 1;
  OTSF_AlgoBold       = 1 shl 16;
  OTSF_AlgoItalic     = 1 shl 17;


  OTUL_None         = 0;
  OTUL_Solid        = 1;
  OTUL_Broken       = 2;
  OTUL_DoubleSolid  = 3;
  OTUL_DoubleBroken = 4;
  OUTL_DoubleBroken = OTUL_DoubleBroken;

  OTSUFFIX   = '.otag';
  OTE_Bullet = 'bullet';

  OT_FileIdent        = OT_Level1 or $01;
  OT_Engine           = OT_Level1 or OT_Indirect or $02;
  OT_Family           = OT_Level1 or OT_Indirect or $03;

  OT_BName            = OT_Level2 or OT_Indirect or $05;
  OT_IName            = OT_Level2 or OT_Indirect or $06;
  OT_BIName           = OT_Level2 or OT_Indirect or $07;
  OT_RName            = OT_Level2 or OT_Indirect or $09;

  OT_SymbolSet        = OT_Level1 or $10;
  OT_YSizeFactor      = OT_Level1 or $11;
  OT_SpaceWidth       = OT_Level2 or $12;
  OT_IsFixed          = OT_Level2 or $13;
  OT_SerifFlag        = OT_Level1 or $14;
  OT_StemWeight       = OT_Level1 or $15;
  OT_SlantStyle       = OT_Level1 or $16;
  OT_HorizStyle       = OT_Level1 or $17;
  OT_SpaceFactor      = OT_Level2 or $18;
  OT_InhibitAlgoStyle = OT_Level2 or $19;
  OT_AvailSizes       = OT_Level1 or OT_Indirect or $20;

  OT_MAXAVAILSIZES = 20;

  OTS_Upright    = 0;
  OTS_Italic     = 1;
  OTS_LeftItalic = 2;
  OTS_UltraThin  = 8;
  OTS_ExtraThin  = 24;
  OTS_Thin       = 40;
  OTS_ExtraLight = 56;
  OTS_Light      = 72;
  OTS_DemiLight  = 88;
  OTS_SemiLight  = 104;
  OTS_Book       = 120;
  OTS_Medium     = 136;
  OTS_SemiBold   = 152;
  OTS_DemiBold   = 168;
  OTS_Bold       = 184;
  OTS_ExtraBold  = 200;
  OTS_Black      = 216;
  OTS_ExtraBlack = 232;
  OTS_UltraBlack = 248;

  OTH_UltraCompressed = 16;
  OTH_ExtraCompressed = 48;
  OTH_Compressed      = 80;
  OTH_Condensed       = 112;
  OTH_Normal          = 144;
  OTH_SemiExpanded    = 176;
  OTH_Expanded        = 208;
  OTH_ExtraExpanded   = 240;

  OT_SpecCount = OT_Level1 or $100;
  OT_Spec      = OT_Level1 or $100;
  OT_Spec1     = OT_Level1 or $101;

  DFCTRL_BASE     = TAG_USER + $0B000000;
  DFCTRL_XDPI     = DFCTRL_BASE + 1;
  DFCTRL_YDPI     = DFCTRL_BASE + 2;
  DFCTRL_XDOTP    = DFCTRL_BASE + 3;
  DFCTRL_YDOTP    = DFCTRL_BASE + 4;
  DFCTRL_CACHE    = DFCTRL_BASE + 5;
  DFCTRL_SORTMODE = DFCTRL_BASE + 6;

  DFCTRL_SORT_OFF = 0;
  DFCTRL_SORT_ASC = 1;
  DFCTRL_SORT_DES = -1;

// diskfont glyph defines
type
  PGlyphEngine = ^TGlyphEngine;
  TGlyphEngine = record
    gle_Library: PLibrary;
    gle_Name: PChar;
  end;

  FIXED = LongInt;

  PGlyphMap = ^TGlyphMap;
  TGlyphMap = record
    glm_BMModulo: Word;
    glm_BMRows: Word;
    glm_BlackLeft: Word;
    glm_BlackTop: Word;
    glm_BlackWidth: Word;
    glm_BlackHeight: Word;
    glm_XOrigin: FIXED;
    glm_YOrigin: FIXED;
    glm_X0: SmallInt;
    glm_Y0: SmallInt;
    glm_X1: SmallInt;
    glm_Y1: SmallInt;
    glm_Width: FIXED;
    glm_BitMap: PByte;
  end;

  PGlyphWidthEntry = ^TGlyphWidthEntry;
  TGlyphWidthEntry = record
    gwe_Node: TMinNode;
    gwe_Code: Word;
    gwe_Width: FIXED;
  end;

// outline errors
const
  OTERR_Failure      = -1;
  OTERR_Success      = 0;
  OTERR_BadTag       = 1;
  OTERR_UnknownTag   = 2;
  OTERR_BadData      = 3;
  OTERR_NoMemory     = 4;
  OTERR_NoFace       = 5;
  OTERR_BadFace      = 6;
  OTERR_NoGlyph      = 7;
  OTERR_BadGlyph     = 8;
  OTERR_NoShear      = 9;
  OTERR_NoRotate     = 10;
  OTERR_TooSmall     = 11;
  OTERR_UnknownGlyph = 12;

const
    DISKFONTNAME : PChar = 'diskfont.library';

var DiskfontBase : pLibrary = nil;

function OpenDiskFont(TextAttr: PTextAttr location 'a0'): PTextFont; syscall DiskfontBase 030;
function AvailFonts(Buffer: STRPTR location 'a0'; BufBytes: LongInt location 'd0'; Flags: LongInt location 'd1'): LongInt; syscall DiskfontBase 036;
function NewFontContents(FontsLock: BPTR location 'a0'; FontName: STRPTR location 'a1'): PFontContentsHeader; syscall DiskfontBase 042;
procedure DisposeFontContents(FontContentsHeader: PFontContentsHeader location 'a1'); syscall DiskfontBase 048;
function NewScaledDiskFont(SourceFont: PTextFont location 'a0'; DestTextAttr: PTextAttr location 'a1'): PDiskFontHeader; syscall DiskfontBase 054;

// MorphOS actually supports these V45+ calls
function GetDiskFontCtrl(TagID: LongInt location 'd0'): LongInt; syscall DiskfontBase 060;
procedure SetDiskFontCtrlA(TagList: PTagItem location 'a0'); syscall DiskfontBase 066;

// vartags Version
procedure SetDiskFontCtrl(const Tags: array of PtrUInt); inline;

function InitDiskFontLibrary: boolean; inline;

implementation

procedure SetDiskFontCtrl(const Tags: array of PtrUInt);
begin
  SetDiskFontCtrlA(@Tags);
end;

const
  LIBVERSION: LongWord = 0;

function InitDiskFontLibrary: boolean; inline;
begin
  InitDiskFontLibrary := Assigned(DiskFontBase);
end;

initialization
  DiskFontBase := OpenLibrary(DISKFONTNAME, LIBVERSION);
finalization
  if Assigned(DiskFontBase) then
    CloseLibrary(DiskFontBase);
END.
