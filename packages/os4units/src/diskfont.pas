{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014-2016 by Free Pascal development team

    diskfont.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit diskfont;

interface

uses
  exec, agraphics,utility;

const
  MAXFONTPATH = 256; // including null terminator

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
    tfc_TagCount: Word;  // including the TAG_DONE tag
    // if tfc_TagCount is non-zero, tfc_FileName is overlayed with Text Tags starting at:
    //     PTagItem(@tfc_FileName[MAXFONTPATH - (tfc_TagCount * sizeof(TTagItem))])
    tfc_YSize: Word;
    tfc_Style: Byte;
    tfc_Flags: Byte;
   end;

const
  FCH_ID   = $0f00; // FontContentsHeader, then FontContents
  TFCH_ID  = $0f02; // FontContentsHeader, then TFontContents
  OFCH_ID  = $0f03; // FontContentsHeader, then TFontContents, associated with outline font

type
  PFontContentsHeader = ^TFontContentsHeader;
  TFontContentsHeader = record
    fch_FileID: Word;     // FCH_ID
    fch_NumEntries: Word; // the number of FontContents elements
  end;

const
  DFH_ID = $0f80;
  MAXFONTNAME = 32; // font name including '.font'#0

type
  PDiskFontHeader = ^TDiskFontHeader;
  TDiskFontHeader = record
    // the following 8 bytes are not actually considered a part of the  DiskFontHeader, but immediately preceed it.
    // The NextSegment is supplied by the linker/loader, and the ReturnCode is the code at the beginning of the font in case someone runs it...
    // Warning: you can find those bytes on disk but not in memory.
    //   uint32 dfh_NextSegment; \* actually a BPTR
    //   uint32 dfh_ReturnCode;  \* MOVEQ #0,D0 : RTS
    // here then is the official start of the DiskFontHeader...
    dfh_DF: TNode;        // node to link disk fonts
    dfh_FileID: Word;     // DFH_ID
    dfh_Revision: Word;   // the font revision
    dfh_Segment: Longint; // the segment address when loaded
    dfh_Name: array [0..MAXFONTNAME-1] of Char; // stripped font name (null terminated)
    dfh_TF: TTextFont;    // loaded TextFont structure, dfh_TF.tf_Message.mn_Node.ln_Name points to the full font name
  end;

// unfortunately, this needs to be explicitly typed used only if dfh_TF.tf_Style FSB_TAGGED bit is set moved to dfh_TF.tf_Extension->tfe_Tags during loading
const
  AFB_MEMORY  = 0;     // dont filter out memory fonts
  AFF_MEMORY  = $0001;
  AFB_DISK    = 1;     // dont filter out disk fonts
  AFF_DISK    = $0002;
  AFB_SCALED  = 2;     // dont filter out scaled fonts
  AFF_SCALED  = $0004;
  AFB_BITMAP  = 3;     // filter out .otag files
  AFF_BITMAP  = $0008;
  AFB_OTAG    = 4;     // show .otag files only,
  AFF_OTAG    = $0010; // implemented since V50
  AFB_CHARSET = 5;     // show fonts in all charsets,
  AFF_CHARSET = $0020; // implemented since V50
  AFB_TYPE    = 6;     // return diskfont type in [t]af_Type: AFF_DISK|AFF_BITMAP for bitmap fonts, AFF_DISK|AFF_OTAG for .otag fonts, AFF_DISK|AFF_OTAG|AFF_SCALED for scalable .otag fonts.
  AFF_TYPE    = $0040; // implemented since V50

  AFB_TAGGED  = 16;
  AFF_TAGGED  = $10000;

type
  PAvailFonts = ^TAvailFonts;
  TAvailFonts = record
    af_Type: Word;      // AFF_MEMORY, AFF_DISK or AFF_SCALED
    af_Attr: TTextAttr; // text attributes for font
  end;

  PTAvailFonts = ^TTAvailFonts;
  TTAvailFonts = record
    taf_Type: Word;       // AFF_MEMORY, AFF_DISK or AFF_SCALED
    taf_Attr: TTTextAttr; // text attributes for font
  end;

  PAvailFontsHeader = ^TAvailFontsHeader;
  TAvailFontsHeader = record
    afh_NumEntries: Word; // number of AvailFonts elements
     // PAvailFonts afh_AF, or PTAvailFonts afh_TAF;
  end;

  type
// A GlyphEngine must be acquired via OpenEngine and is read-only
  PGlyphEngine = ^TGlyphEngine;
  TGlyphEngine = record
    gle_Library: PLibrary; // engine library
    gle_Name: PChar;       // library basename: e.g. 'bullet'
    // private library data follows...
  end;

// FIXED = 32 bit signed w/ 16 bits of fraction */

  PGlyphMap = ^TGlyphMap;
  TGlyphMap = record
    glm_BMModulo: Word;    // # of bytes in row: always multiple of 4
    glm_BMRows: Word;      // # of rows in bitmap
    glm_BlackLeft: Word;   // # of blank pixel columns at left
    glm_BlackTop: Word;    // # of blank rows at top
    glm_BlackWidth: Word;  // span of contiguous non-blank columns
    glm_BlackHeight: Word; // span of contiguous non-blank rows
    glm_XOrigin: LongInt;  // FIXED, distance from upper left corner of bitmap
    glm_YOrigin: LongInt;  // FIXED  to initial CP, in fractional pixels
    glm_X0: SmallInt;      // approximation of XOrigin in whole pixels
    glm_Y0: SmallInt;      // approximation of YOrigin in whole pixels
    glm_X1: SmallInt;      // approximation of XOrigin + Width
    glm_Y1: SmallInt;      // approximation of YOrigin + Width
    glm_Width: LongInt;    // FIXED, character advance, as fraction of em width
    glm_BitMap: PByte;     // actual glyph bitmap
  end;

  PGlyphWidthEntry = ^TGlyphWidthEntry;
  TGlyphWidthEntry = record
    gwe_Node: TMinNode; // on list returned by OT_WidthList inquiry
    gwe_Code: Word;     // entry's character code value
    gwe_Width: LongInt; // FIXED character advance, as fraction of em width
  end;

// Structure returned when asking for OT_WidthList32, the original GlyphWidthEntry structure is limited to 16 bit glyph code (gwe_Code: Word).
  PGlyphWidthEntry32 = ^TGlyphWidthEntry32;
  TGlyphWidthEntry32 = record
    gwe32_Node: TNode;    // on list returned by OT_WidthList32 inquiry
    gwe32_reserved: Word; // for alignment and backwards compatibility
    gwe32_Width: LongInt; // FIXED character advance, as fraction of em width
    gwe32_Code: LongWord; // entry's character code value
  end;

// structure used by EOpenEngine() ESetInfo() etc (V50)
  PEGlyphEngine = ^TEGlyphEngine;
  TEGlyphEngine = record
    ege_IBullet: PInterface;    // NULL for 68K font engines
    ege_BulletBase: PLibrary;
    ege_GlyphEngine: PGlyphEngine;
  end;

const
// flags for OpenOutlineFont() (V50)
  OFB_OPEN = 0;
  OFF_OPEN = $00000001;

type
// structure returned by OpenOutlineFont() (V50)
  POutlineFont = ^TOutlineFont;
  TOutlineFont = record
    olf_OTagPath: STRPTR;       // full path & name of the .otag file
    olf_OTagList: PTagItem;     // relocated .otag file in memory
    olf_EngineName: STRPTR;     // OT_Engine name
    olf_LibraryName: STRPTR;    // OT_Engine name + ".library"
    olf_EEngine: PEGlyphEngine; // All NULL if OFF_OPEN not specified
    olf_Reserved: APTR;         // for future expansion
    olf_UserData: APTR;         // for private use
  end;

const
  OT_Level0   = TAG_USER;          // Level 0 entries never appear in the .otag tag list, but appear in font specifications
  OT_Level1   = TAG_USER or $1000; // Level 1 entries are required to exist in the .otag tag list
  OT_Level2   = TAG_USER or $2000; // Level 2 entries are optional typeface metric tags
  OT_Level3   = TAG_USER or $3000; // Level 3 entries are required for some OT_Engines
  OT_Indirect = $8000; // Indirect entries are at (tag address + data offset)

// font specification and inquiry tags
  OT_DeviceDPI        = OT_Level0 or $01;   // = TA_DeviceDPI
  OT_DotSize          = OT_Level0 or $02;
  OT_PointHeight      = OT_Level0 or $08;
  OT_SetFactor        = OT_Level0 or $09;
  OT_ShearSin         = OT_Level0 or $0a;
  OT_ShearCos         = OT_Level0 or $0b;
  OT_RotateSin        = OT_Level0 or $0c;
  OT_RotateCos        = OT_Level0 or $0d;
  OT_EmboldenX        = OT_Level0 or $0e;
  OT_EmboldenY        = OT_Level0 or $0f;
  OT_PointSize        = OT_Level0 or $10;
  OT_GlyphCode        = OT_Level0 or $11;
  OT_GlyphCode2       = OT_Level0 or $12;
  OT_GlyphCode_32     = OT_Level0 or $18;
  OT_GlyphCode2_32    = OT_Level0 or $19;
  OT_GlyphWidth       = OT_Level0 or $13;
  OT_OTagPath         = OT_Level0 or OT_Indirect or $14;
  OT_OTagList         = OT_Level0 or OT_Indirect or $15;
  OT_MemPtr           = OT_Level0 or OT_Indirect or $16;
  OT_MemSize          = OT_Level0 or $17;
  OT_GlyphMap         = OT_Level0 or OT_Indirect or $20;
  OT_GlyphMap8Bit     = OT_Level0 or OT_Indirect or $1a;
  OT_GlyphMap8BitLCD  = OT_Level0 or OT_Indirect or $27;
  OT_GlyphMap8BitLCDV = OT_Level0 or OT_Indirect or $28;
  OT_WidthList        = OT_Level0 or OT_Indirect or $21;
  OT_WidthList32      = OT_Level0 or OT_Indirect or $1b;
  OT_TextKernPair     = OT_Level0 or OT_Indirect or $22;
  OT_DesignKernPair   = OT_Level0 or OT_Indirect or $23;
  OT_UnderLined       = OT_Level0 or $24;

  OTUL_None         = 0;
  OTUL_Solid        = 1;
  OTUL_Broken       = 2;
  OTUL_DoubleSolid  = 3;
  OTUL_DoubleBroken = 4;
// old versions had a typo here
  OUTL_DoubleBroken = OTUL_DoubleBroken;
  OT_StrikeThrough  = OT_Level0 or $25;
  OT_BaseLine       = OT_Level0 or OT_Indirect or $1c;
  OT_NumGlyphs      = OT_Level0 or OT_Indirect or $1d;
  OT_NumKernPairs   = OT_Level0 or OT_Indirect or $1e;
  OT_HasGlyphs      = OT_Level0 or OT_Indirect or $1f;
  OT_GlyphName      = OT_Level0 or OT_Indirect or $26;

// .otag tags
// suffix for files in FONTS: that contain these tags
  OTSUFFIX       = '.otag';
  OT_FileIdent   = OT_Level1 or $01;
  OT_Engine      = OT_Level1 or OT_Indirect or $02;
  OTE_Bullet     = 'bullet';
  OT_Family      = OT_Level1 or OT_Indirect or $03;
  OT_FontFile    = OT_Level2 or OT_Indirect or $24;
  OT_MetricsFile = OT_Level2 or OT_Indirect or $25;
  OT_AFMFile     = OT_MetricsFile; // backwards compatibility
  OT_FontFormat  = OT_Level2 or OT_Indirect or $26;

  OTF_BDF         = 'bdf';         // Adobe Bitmap Distribution Format
  OTF_CFF         = 'cff';         // Adobe Compact Font Format
  OTF_Intellifont = 'intellifont'; // Agfa MonoType CompuGraphic
  OTF_PCF         = 'pcf';         // X Portable Compiled Format
  OTF_PFR         = 'pfr';         // Bitstream Portable Font Resource
  OTF_TrueType    = 'truetype';    // AppleorMicroSoft TrueTypeorOpenType
  OTF_Type1       = 'type1';       // Adobe PostScript Type1
  OTF_Type1CID    = 't1cid';       // Adobe CID-keyed PostScript Type1
  OTF_Type42      = 'type42';      // Adobe PostScript Type42
  OTF_WinFonts    = 'winfonts';    // MicroSoft .fntor.fon

  OT_BName  = OT_Level2 or OT_Indirect or $05;
  OT_IName  = OT_Level2 or OT_Indirect or $06;
  OT_BIName = OT_Level2 or OT_Indirect or $07;
  OT_RName  = OT_Level2 or OT_Indirect or $09;
  OT_YSizeFactor  = OT_Level1 or $11;
  OT_SpaceWidth   = OT_Level2 or $12;
  OT_IsFixed      = OT_Level2 or $13;
  OT_IsUnderlined = OT_Level2 or $23;
  OT_SerifFlag    = OT_Level1 or $14;
  OT_StemWeight   = OT_Level1 or $15;

  OTS_UltraThin  = 8;   //   0- 15
  OTS_ExtraThin  = 24;  //  16- 31
  OTS_Thin       = 40;  //  32- 47
  OTS_ExtraLight = 56;  //  48- 63
  OTS_Light      = 72;  //  64- 79
  OTS_DemiLight  = 88;  //  80- 95
  OTS_SemiLight  = 104; //  96-111
  OTS_Book       = 120; // 112-127
  OTS_Medium     = 136; // 128-143
  OTS_SemiBold   = 152; // 144-159
  OTS_DemiBold   = 168; // 160-175
  OTS_Bold       = 184; // 176-191
  OTS_ExtraBold  = 200; // 192-207
  OTS_Black      = 216; // 208-223
  OTS_ExtraBlack = 232; // 224-239
  OTS_UltraBlack = 248; // 240-255

  OT_SlantStyle  = OT_Level1 or $16;
  OTS_Upright    = 0;
  OTS_Italic     = 1; // Oblique, Slanted, etc.
  OTS_LeftItalic = 2; // Reverse Slant
  OT_HorizStyle  = OT_Level1 or $17;

  OTH_UltraCompressed = 16;  //   0- 31
  OTH_ExtraCompressed = 48;  //  32- 63
  OTH_Compressed      = 80;  //  64- 95
  OTH_Condensed       = 112; //  96-127
  OTH_Normal          = 144; // 128-159
  OTH_SemiExpanded    = 176; // 160-191
  OTH_Expanded        = 208; // 192-223
  OTH_ExtraExpanded   = 240; // 224-255

  OT_SpaceFactor      = OT_Level2 or $18;
  OT_InhibitAlgoStyle = OT_Level2 or $19;
  OT_AvailSizes       = OT_Level1 or OT_Indirect or $20;
  OT_MAXAVAILSIZES    = 20; // no more than 20 sizes allowed

  OT_BMSize = OT_Level2 or $21;
  OT_UnicodeRanges = OT_Level2 or OT_Indirect or $22;

  UCR_BASIC_LATIN                   = 1 shl  0;
  UCR_LATIN1_SUPPLEMENT             = 1 shl  1;
  UCR_LATIN_EXTENDED_A              = 1 shl  2;
  UCR_LATIN_EXTENDED_B              = 1 shl  3;
  UCR_IPA_EXTENSIONS                = 1 shl  4;
  UCR_SPACING_MODIFIER              = 1 shl  5;
  UCR_COMBINING_DIACRITICS          = 1 shl  6;
  UCR_GREEK                         = 1 shl  7;
  UCR_CYRILLIC                      = 1 shl  9;
  UCR_ARMENIAN                      = 1 shl 10;
  UCR_HEBREW                        = 1 shl 11;
  UCR_ARABIC                        = 1 shl 13;
  UCR_DEVANAGARI                    = 1 shl 15;
  UCR_BENGALI                       = 1 shl 16;
  UCR_GURMUKHI                      = 1 shl 17;
  UCR_GUJARATI                      = 1 shl 18;
  UCR_ORIYA                         = 1 shl 19;
  UCR_TAMIL                         = 1 shl 20;
  UCR_TELUGU                        = 1 shl 21;
  UCR_KANNADA                       = 1 shl 22;
  UCR_MALAYALAM                     = 1 shl 23;
  UCR_THAI                          = 1 shl 24;
  UCR_LAO                           = 1 shl 25;
  UCR_GEORGIAN                      = 1 shl 26;
  UCR_HANGUL_JAMO                   = 1 shl 28;
  UCR_LATIN_EXTENDED_ADDITIONAL     = 1 shl 29;
  UCR_GREEK_EXTENDED                = 1 shl 30;
  UCR_GENERAL_PUNCTUATION           = 1 shl 31;
  UCR_SUPERSCRIPTS_SUBSCRIPTS       = 1 shl  0;
  UCR_CURRENCY_SYMBOLS              = 1 shl  1;
  UCR_COMBINING_DIACRITICS_SYMB     = 1 shl  2;
  UCR_LETTERLIKE_SYMBOLS            = 1 shl  3;
  UCR_NUMBER_FORMS                  = 1 shl  4;
  UCR_ARROWS                        = 1 shl  5;
  UCR_MATHEMATICAL_OPERATORS        = 1 shl  6;
  UCR_MISCELLANEOUS_TECHNICAL       = 1 shl  7;
  UCR_CONTROL_PICTURES              = 1 shl  8;
  UCR_OCR                           = 1 shl  9;
  UCR_ENCLOSED_ALPHANUMERICS        = 1 shl 10;
  UCR_BOX_DRAWING                   = 1 shl 11;
  UCR_BLOCK_ELEMENTS                = 1 shl 12;
  UCR_GEOMETRIC_SHAPES              = 1 shl 13;
  UCR_MISCELLANEOUS_SYMBOLS         = 1 shl 14;
  UCR_DINGBATS                      = 1 shl 15;
  UCR_CJK_SYMBOLS                   = 1 shl 16;
  UCR_HIRAGANA                      = 1 shl 17;
  UCR_KATAKANA                      = 1 shl 18;
  UCR_BOPOMOFO                      = 1 shl 19;
  UCR_HANGUL_COMPATIBILITY_JAMO     = 1 shl 20;
  UCR_CJK_MISC                      = 1 shl 21;
  UCR_KANBUN                        = UCR_CJK_MISC;
  UCR_ENCLOSED_CJK_LETTERS_MONTHS   = 1 shl 22;
  UCR_CJK_COMPATIBILITY             = 1 shl 23;
  UCR_HANGUL                        = 1 shl 24;
  UCR_SURROGATES                    = 1 shl 25;
  UCR_CJK_UNIFIED_IDEOGRAPHS        = 1 shl 27;
  UCR_PRIVATE_USE                   = 1 shl 28;
  UCR_CJK_COMPATIBILITY_IDEOGRAPHS  = 1 shl 29;
  UCR_ALPHABETIC_PRESENTATION_FORMS = 1 shl 30;
  UCR_ARABIC_PRESENTATIONS_A        = 1 shl 31;
  UCR_COMBINING_HALF_MARKS          = 1 shl  0;
  UCR_CJK_COMPATIBILITY_FORMS       = 1 shl  1;
  UCR_SMALL_FORM_VARIANTS           = 1 shl  2;
  UCR_ARABIC_PRESENTATIONS_B        = 1 shl  3;
  UCR_HALFWIDTH_FULLWIDTH_FORMS     = 1 shl  4;
  UCR_SPECIALS                      = 1 shl  5;
  UCR_TIBETAN                       = 1 shl  6;
  UCR_SYRIAC                        = 1 shl  7;
  UCR_THAANA                        = 1 shl  8;
  UCR_SINHALA                       = 1 shl  9;
  UCR_MYANMAR                       = 1 shl 10;
  UCR_ETHIOPIC                      = 1 shl 11;
  UCR_CHEROKEE                      = 1 shl 12;
  UCR_CANADIAN_ABORIGINAL_SYLLABICS = 1 shl 13;
  UCR_OGHAM                         = 1 shl 14;
  UCR_RUNIC                         = 1 shl 15;
  UCR_KHMER                         = 1 shl 16;
  UCR_MONGOLIAN                     = 1 shl 17;
  UCR_BRAILLE                       = 1 shl 18;
  UCR_YI                            = 1 shl 19;

  OT_SpecCount = OT_Level1 or $100;
  OT_Spec      = OT_Level1 or $100;
  OT_Spec1     = OT_Level1 or $101;

// GetDiskFontCtrl and SetDiskFontCtrl tags
  DFCTRL_BASE         = TAG_USER + $0B000000;

  DFCTRL_XDPI         = DFCTRL_BASE + 1;  // X and Y DPI device default settings for the bullet library font generator. Default is 72 dpi.
  DFCTRL_YDPI         = DFCTRL_BASE + 2;
  DFCTRL_XDOTP        = DFCTRL_BASE + 3;  // X and Y DPI dot size settings for the font generator. Default is 100dpi.
  DFCTRL_YDOTP        = DFCTRL_BASE + 4;
  DFCTRL_CACHE        = DFCTRL_BASE + 5;  // AvailFonts cache enable flag. Either TRUE or FALSE.
  DFCTRL_SORTMODE     = DFCTRL_BASE + 6;  // Availfonts font sorting flag. See below for available values.

  DFCTRL_SORT_OFF     = 0;   // No sorting: Default.

  DFCTRL_SORT_ASC     = 1;     // Ascending sort order, localized with default locale.
  DFCTRL_SORT_DES     = not 0; // Descending sort order, localized with default locale.
  DFCTRL_CACHEFLUSH   = DFCTRL_BASE + 7; // Flush the cache? = BOOL; If TRUE, a cache flush is initiated.
  DFCTRL_CHARSET      = DFCTRL_BASE + 8; // Default character set identifier = LongWord;.
  DFCTRL_ANTIALIASING = DFCTRL_BASE + 9;
  DFCS_NUMBER         = DFCTRL_BASE + 0;
  DFCS_NEXTNUMBER     = DFCTRL_BASE + 1; // Next IANA CharSet number = ULONG; for browsing charsets. The lowest possible number to start from is 3.
  DFCS_NAME           = DFCTRL_BASE + 2; // IANA CharSet name = STRPTR;
  DFCS_MIMENAME       = DFCTRL_BASE + 3; // IANA CharSet name for MIME = STRPTR;
  DFCS_MAPTABLE       = DFCTRL_BASE + 4; // { Pointer to mapping table of 256 ULONGs from CharSet to Unicode = PLongWord;

// PRELIMINARY
  OTERR_Failure      = -1; // catch-all for error
  OTERR_Success      =  0; // no error
  OTERR_BadTag       =  1; // inappropriate tag for function
  OTERR_UnknownTag   =  2; // unknown tag for function
  OTERR_BadData      =  3; // catch-all for bad tag data
  OTERR_NoMemory     =  4; // insufficient memory for operation
  OTERR_NoFace       =  5; // no typeface currently specified
  OTERR_BadFace      =  6; // typeface specification problem
  OTERR_NoGlyph      =  7; // no glyph specified
  OTERR_BadGlyph     =  8; // bad glyph code or glyph range
  OTERR_NoShear      =  9; // shear only partially specified
  OTERR_NoRotate     = 10; // rotate only partially specified
  OTERR_TooSmall     = 11; // typeface metrics yield tiny glyphs
  OTERR_UnknownGlyph = 12; // glyph not known by engine

const
  DISKFONTNAME: PChar = 'diskfont.library';

var
  DiskfontBase: PLibrary;
  IDiskfont: PInterface;

function DiskfontObtain(): LongWord; syscall IDiskfont 60;
function DiskfontRelease(): LongWord; syscall IDiskfont 64;
procedure DiskfontExpunge(); syscall IDiskfont 68;
function DiskfontClone(): PInterface; syscall IDiskfont 72;
function OpenDiskFont(TextAttr: PTextAttr): PTextFont; syscall IDiskfont 76;
function AvailFonts(Buffer: STRPTR; BufBytes: LongInt; Flags: LongWord): LongInt; syscall IDiskfont 80;
function NewFontContents(FontsLock: BPTR; const FontName: STRPTR): PFontContentsHeader; syscall IDiskfont 84;
procedure DisposeFontContents(FontContentsHeader: PFontContentsHeader); syscall IDiskfont 88;
function NewScaledDiskFont(SourceFont: PTextFont; DestTextAttr: PTextAttr): PDiskFontHeader; syscall IDiskfont 92;
function GetDiskFontCtrl(TagId: LongInt): LongInt; syscall IDiskfont 96;
procedure SetDiskFontCtrlA(TagList: PTagItem); syscall IDiskfont 100;
// 104 SetDiskFontCtrl
function EOpenEngine(EEngine: PEGlyphEngine): LongInt;  syscall IDiskfont 108;
procedure ECloseEngine(EEngine: PEGlyphEngine);  syscall IDiskfont 112;
function ESetInfoA(EEngine: PEGlyphEngine; TagList: PTagItem): LongWord;  syscall IDiskfont 116;
// 120 ESetInfo
function EObtainInfoA(EEngine: PEGlyphEngine; TagList: PTagItem): LongWord;  syscall IDiskfont 124;
// 128 EObtainInfo
function EReleaseInfoA(EEngine: PEGlyphEngine; TagList: PTagItem): LongWord;  syscall IDiskfont 132;
// 136 EReleaseInfo
function OpenOutlineFont(const Name: STRPTR; List: PList; Flags: LongWord): POutlineFont;  syscall IDiskfont 140;
procedure CloseOutlineFont(Olf: POutlineFont; List: PList);  syscall IDiskfont 144;
function WriteFontContents(FontsLock: BPTR; const FontName: STRPTR; FontContentsHeader: PFontContentsHeader): LongInt;  syscall IDiskfont 148;
function WriteDiskFontHeaderA(Font: PTextFont; const FileName: STRPTR; const TagList: PTagItem): LongInt;  syscall IDiskfont 152;
// 156 WriteDiskFontHeader
function ObtainCharsetInfo(KnownTag, KnowValue, WantedTag: LongWord): LongWord;  syscall IDiskfont 160;
function ObtainTTextAttr(Font: PTextFont): PTextFont;  syscall IDiskfont 164;
procedure FreeTTextAttr(Tta: PTextFont);  syscall IDiskfont 168;

implementation

initialization
  DiskfontBase := OpenLibrary(DISKFONTNAME, 36);
  if Assigned(DiskfontBase) then
    IDiskfont := GetInterface(DiskfontBase, 'main', 1, nil);
finalization
  if Assigned(IDiskfont) then
    DropInterface(IDiskfont);
  if Assigned(DiskfontBase) then
    CloseLibrary(DiskfontBase);
end.



