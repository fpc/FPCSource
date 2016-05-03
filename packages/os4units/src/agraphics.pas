{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    graphics.library functions for Amiga OS 4.x

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$PACKRECORDS 2}

unit agraphics;

interface

uses
  exec, utility;

const
// tag definitions for BltBitMapTagList, BltBitMapTags
  BLTBMTAGS_BASE  = TAG_USER; // attributes specifying the source and destination coordinates of the blit operation. Default is 0 (WORD). */
  BLITA_SrcX      = BLTBMTAGS_BASE + 1;
  BLITA_SrcY      = BLTBMTAGS_BASE + 2;
  BLITA_DestX     = BLTBMTAGS_BASE + 3;
  BLITA_DestY     = BLTBMTAGS_BASE + 4;  // size of blit operation, default is 0 (WORD)
  BLITA_Width     = BLTBMTAGS_BASE + 5;
  BLITA_Height    = BLTBMTAGS_BASE + 6;  // source and destination object, also see BLITT definitions below default is NULL (APTR)
  BLITA_Source    = BLTBMTAGS_BASE + 7;
  BLITA_Dest      = BLTBMTAGS_BASE + 8;  // minterm of blit, default is 0xc0 (copy blit, UBYTE)
  BLITA_Minterm   = BLTBMTAGS_BASE + 9;  // blit mask, default is $ff (Byte) if BLITA_Dest is not a rastport otherwise the rp^.Mask value of the destination rastport is used.
  BLITA_Mask      = BLTBMTAGS_BASE + 10; // see BltMaskBitMapRastPort()
  BLITA_MaskPlane = BLTBMTAGS_BASE + 11; // 8bit alpha mask, default is nil (APTR). Mask and source bitmap must have the same dimension
  BLITA_AlphaMask = BLTBMTAGS_BASE + 12; // specifies the bytes per row value of the source object, if source is neither a rastport nor a bitmap (UWORD)
  BLITA_SrcBytesPerRow = BLTBMTAGS_BASE + 13; // type of source object, default is BLITT_BITMAP
  BLITA_SrcType  = BLTBMTAGS_BASE + 14; // specifies the bytes per row value of the destination object, if destination is neither a rastport nor a bitmap (UWORD)
  BLITA_DestBytesPerRow = BLTBMTAGS_BASE + 15; // type of destination object, default is BLITT_BITMAP
  BLITA_DestType = BLTBMTAGS_BASE + 16; // use alpha data stored in the source object for transparent blits, default is FALSE (BOOL). BLITA_UseSrcAlpha and BLITA_AlphaMask are mutually-exclusive.
  BLITA_UseSrcAlpha = BLTBMTAGS_BASE + 17; // not implemented yet
  BLITA_UseDestAlpha = BLTBMTAGS_BASE + 18; // global alpha value applied to each pixel, default is $ffffffff (LongWord, 32 bit left justified fraction). A value of ULONG_MAX means non-opaque blit unless
                                            // BLIT_UseSrcAlpha is TRUE or BLITA_AlphaMask is used. To convert an 8bit alpha value to 32bit one multiply it by $01010101. BLITA_Alpha can be used in combination with the two tags mentioned above.
  BLITA_Alpha        = BLTBMTAGS_BASE + 19; // color lookuptable (256 entries each 4bytes large, format $00RRGGBB) to use when blitting chunky bitmaps to
                                            // directmapped bitmaps. Default is NULL (APTR), means palette of destination bitmap is used (if available). Currently not supported by alpha blits.
  BLITA_CLUT         = BLTBMTAGS_BASE + 20;

// possible types for BLITA_SrcType / BLITA_DestType
  BLITT_BITMAP            = 0; // a bitmap (default)
  BLITT_RASTPORT          = 1; // a rastport
  BLITT_TEMPLATE          = 2; // 1bit template, source only!
  BLITT_ALPHATEMPLATE     = 3; // 8bit alpha template, source only!
  BLITT_CHUNKY            = 4; // 8bit chunky buffer
  BLITT_RGB24             = 5; // 24bit RGB buffer
  BLITT_ARGB32            = 6; // 32bit ARGB buffer
  BLITT_RGB16             = 7; // 16bit RGB buffer (same as RGBFB_R5G6B5)

// graphics board definitions
  GBD_TagBase            = TAG_USER;
  GBD_BoardName          = GBD_TagBase + 1;
  GBD_TotalMemory        = GBD_TagBase + 2;
  GBD_FreeMemory         = GBD_TagBase + 3;
  GBD_LargestFreeMemory  = GBD_TagBase + 4;
  GBD_InternalMemorySize = GBD_TagBase + 5;
  GBD_PCIVendorID        = GBD_TagBase + 6;
  GBD_PCIProductID       = GBD_TagBase + 7;
  GBD_BoardDriver        = GBD_TagBase + 8;
  GBD_ChipDriver         = GBD_TagBase + 9;

  BITSET = $8000;
  BITCLR = 0;
type
  PRectangle = ^TRectangle;
  TRectangle = record
    MinX, MinY: SmallInt;
    MaxX, MaxY: SmallInt;
  end;

  PRect32 = ^TRect32;
  TRect32 = record
    MinX, MinY: LongInt;
    MaxX, MaxY: LongInt;
  end;

  PPoint = ^TPoint;
  TPoint = record
    x, y: SmallInt;
  end;

  TPLANEPTR = PByte;

  PBitMap = ^TBitMap;
  TBitMap = record
    BytesPerRow: Word;
    Rows: Word;
    Flags: Byte;
    Depth: Byte;
    pad: Word;
    Planes: array[0..7] of TPLANEPTR;
  end;

  PRegionRectangle = ^TRegionRectangle;
  TRegionRectangle = record
    Next, Prev: PRegionRectangle;
    bounds: TRectangle;
  end;

  PRegion = ^tRegion;
  tRegion = record
    bounds: TRectangle;
    RegionRectangle: PRegionRectangle;
  end;

  PExtendedNode = ^TExtendedNode;
  TExtendedNode = record
    xln_Succ: PNode;
    xln_Pred: PNode;
    xln_Type: Byte;
    xln_Pri: ShortInt;
    xln_Name: STRPTR;
    xln_Subsystem: Byte;
    xln_Subtype: Byte;
    xln_Library: LongInt;
    xln_Init: Pointer;    // only used if MONITOR_SPEC_TYPE
  end;

const
  // flags for AllocBitMap, etc.
  BMB_CLEAR       = 0;
  BMB_DISPLAYABLE = 1;
  BMB_INTERLEAVED = 2;
  BMB_STANDARD    = 3;
  BMB_MINPLANES   = 4;
  // New V45 flags follow. If this bit combination is set,
  // the AllocBitMap() friends pointer points to a tag
  // list describing further data
  BMB_HIJACKED    = 7; // must be clear
  BMB_RTGTAGS     = 8; // must be one for tag extension
  BMB_RTGCHECK    = 9; // must be one for tag extension
  BMB_FRIENDISTAG = 10; // must be one as well
  BMB_INVALID     = 11; // must be clear
  // Flags introduced in V51 of graphics.library
  BMB_LOCKED      = 12; // private

  BMF_CLEAR       = 1 shl BMB_CLEAR;
  BMF_DISPLAYABLE = 1 shl BMB_DISPLAYABLE;
  BMF_INTERLEAVED = 1 shl BMB_INTERLEAVED;
  BMF_STANDARD    = 1 shl BMB_STANDARD;
  BMF_MINPLANES   = 1 shl BMB_MINPLANES;
  BMF_HIJACKED    = 1 shl BMB_HIJACKED;    // aka BMF_SPECIALFMT
  BMF_RTGTAGS     = 1 shl BMB_RTGTAGS;
  BMF_RTGCHECK    = 1 shl BMB_RTGCHECK;
  BMF_FRIENDISTAG = 1 shl BMB_FRIENDISTAG;
  BMF_INVALID     = 1 shl BMB_INVALID;
  BMF_LOCKED      = 1 shl BMB_LOCKED;

  BMF_CHECKMASK = BMF_HIJACKED or BMF_RTGTAGS or BMF_RTGCHECK or BMF_FRIENDISTAG or BMF_INVALID;
  BMF_CHECKVALUE = BMF_RTGTAGS or BMF_RTGCHECK or BMF_FRIENDISTAG;

  // tags for AllocBitMap
  BMATags_Friend      = TAG_USER + 0;  // Specify a friend-bitmap by tags Default is no friend bitmap
  BMATags_Depth       = TAG_USER + 1;  // depth of the bitmap. Default is the depth parameter of AllocBitMap
  BMATags_PixelFormat = TAG_USER + 2;  // bitmap data format (see enPixelFormat)
  BMATags_Clear       = TAG_USER + 3;  // clear bitmap? Default is the BMF_CLEAR flag specified value.
  BMATags_Displayable = TAG_USER + 4;  // bitmap usable for hardware? Default is the BMF_DISPLAYABLE flag.
  BMATags_Private1    = TAG_USER + 5;  // internal use only!
  BMATags_NoMemory    = TAG_USER + 6;  // do not provide memory for the bitmap, just allocate the structure Default is false.
  BMATags_NoSprite    = TAG_USER + 7;  // disallow generation of a sprite default is sprite enabled.
  BMATags_Private2    = TAG_USER + 8;  // internal use only!
  BMATags_Private3    = TAG_USER + 9;  // internal use only!
  BMATags_ModeWidth   = TAG_USER + 10; // width of the display mode in pixels. Default is the width of the displayID in the monitor database.
  BMATags_ModeHeight  = TAG_USER + 11; // height of the display mode in pixels. Default is the height of the displayID in the monitor database.
  BMATags_RenderFunc  = TAG_USER + 12; // Private. Do not use in new code.
  BMATags_SaveFunc    = TAG_USER + 13; // Pivate. Do not use in new code.
  BMATags_UserData    = TAG_USER + 14; // Private. Do not use in new code.
  BMATags_Alignment   = TAG_USER + 15; // specify additional alignment (power of two; for the bitmap rows. If this tag is set,
                                       // then bitplane rows are aligned to this boundary. Otherwise, the native alignment restriction is provided.
  BMATags_ConstantBytesPerRow = TAG_USER + 16; // set with the above to enforce alignment for displayable screens
  BMATags_UserPrivate = TAG_USER + 17; // user bitmap which will never be in video memory
  BMATags_Private4    = TAG_USER + 18; // internal use only!
  BMATags_Private5    = TAG_USER + 19; // internal use only!
  BMATags_Private6    = TAG_USER + 20; // internal use only!
  BMATags_Private7    = TAG_USER + 21; // internal use only!
  BMATags_DisplayID   = TAG_USER +$32; // a display ID from the monitor data base the system tries then to extract all necessary information
                                       // to build a suitable bitmap This is intentionally identical to intuition SA_DisplayID
  BMATags_BitmapInvisible = TAG_USER + $37; // if set to TRUE, the bitmap is not allocated on the graphics board directly, but may remain in an off-hardware location
                                            // if the screen is invisible. This is intentionally identically to SA_Behind. Default is FALSE
  BMATags_BitmapColors    = TAG_USER + $29; // ti_Data is an array of struct ColorSpec, terminated by ColorIndex = -1.  Specifies
                                            // initial screen palette colors. This is intentionally identically to SA_Colors
  BMATags_BitmapColors32  = TAG_USER + $43; // Tag to set the bitmaps's initial palette colors at 32 bits-per-gun.  ti_Data is a pointer
                                            // to a table to be passed to the graphics.library/LoadRGB32(; function. This format supports both runs of color
                                            // registers and sparse registers.  See the autodoc for that function for full details.
                                            // Any color set here has precedence over the same register set by ABMA_BitmapColors. Intentionally identical to SA_Colors32
  // the following are for GetBitMapAttr()
  BMA_HEIGHT        = 0;
  BMA_DEPTH         = 4;
  BMA_WIDTH         = 8;
  BMA_FLAGS         = 12;
  BMA_ISRTG         = 16; // (V54)
  BMA_BYTESPERPIXEL = 17; // (V54)
  BMA_BITSPERPIXEL  = 18; // (V54)
  BMA_PIXELFORMAT   = 19; // (V54)
  BMA_ACTUALWIDTH   = 20; // (V54)

// Supported pixel formats (V54)
// enPixelFormat = TPIX_FMT
  PIXF_NONE      = 0;  // no valid RGB format (error condition)
  PIXF_CLUT      = 1;  // palette mode, set colors when opening screen using tags or use SetRGB32/LoadRGB32(...)
  PIXF_R8G8B8    = 2;  // TrueColor RGB (8 bit each)
  PIXF_B8G8R8    = 3;  // TrueColor BGR (8 bit each)
  PIXF_R5G6B5PC  = 4;  // HiColor16 (5 bit R, 6 bit G, 5 bit B), format: gggbbbbbrrrrrggg
  PIXF_R5G5B5PC  = 5;  // HiColor15 (5 bit each) format: gggbbbbb0rrrrrgg
  PIXF_A8R8G8B8  = 6;  // 4 Byte TrueColor ARGB (A unused alpha channel)
  PIXF_A8B8G8R8  = 7;  // 4 Byte TrueColor ABGR (A unused alpha channel)
  PIXF_R8G8B8A8  = 8;  // 4 Byte TrueColor RGBA (A unused alpha channel)
  PIXF_B8G8R8A8  = 9;  // 4 Byte TrueColor BGRA (A unused alpha channel)
  PIXF_R5G6B5    = 10; // HiColor16 (5 bit R, 6 bit G, 5 bit B) format: rrrrrggggggbbbbb
  PIXF_R5G5B5    = 11; // HiColor15 (5 bit each) format: 0rrrrrgggggbbbbb
  PIXF_B5G6R5PC  = 12; // HiColor16 (5 bit R, 6 bit G, 5 bit B) format: gggrrrrrbbbbbggg
  PIXF_B5G5R5PC  = 13; // HiColor15 (5 bit each) format: gggrrrrr0bbbbbbgg
  PIXF_YUV422CGX = 14; // 2 Byte TrueColor YUV (CCIR recommendation CCIR601). Each two-pixel unit is stored as one longword
                       // containing luminance (Y) for each of the two pixels, and chrominance (U,V) for alternate pixels.
                       // The missing chrominance values are generated by interpolation. (Y0-U0-Y1-V0)
  PIXF_YUV411    = 15; // 1 Byte TrueColor ACCUPAK. Four adjacent pixels form a packet of 5 bits Y (luminance) each pixel and
                       // 6 bits U and V (chrominance) shared by the four pixels
  PIXF_YUV422PA  = 16; // 2 Byte TrueColor CCIR601 for use with YUV12 planar assist mode on Cirrus Logic base graphics chips. (Y0-Y1-V0-U0)
  PIXF_YUV422    = 17; // 2 Byte TrueColor YUV (CCIR recommendation CCIR601). Each two-pixel unit is stored as one longword
                       // containing luminance (Y) for each of the two pixels, and chrominance (U,V) for alternate pixels.
                       // The missing chrominance values are generated by interpolation. (Y1-U0-Y0-V0)
  PIXF_YUV422PC  = 18; // 2 Byte TrueColor CCIR601 byte swapped (V0-Y0-U0-Y1)
  PIXF_YUV420P   = 19; // 12 Bit TrueColor 3-plane YUV
  PIXF_YUV410P   = 20; // 9 Bit TrueColor 3-plane YUV
  PIXF_ALPHA8    = 21; // 8 bit alpha

// Tags for LockBitMapTagList() (V54)
  LBM_TagBase       = TAG_USER;
  LBM_BaseAddress   = LBM_TagBase + 1;
  LBM_BytesPerRow   = LBM_TagBase + 2;
  LBM_PixelFormat   = LBM_TagBase + 3;
  LBM_PlanarYUVInfo = LBM_TagBase + 4;
  LBM_IsOnBoard     = LBM_TagBase + 5;

// Basic minterm value.
// enMinterm
  MINTERM_ABC    = $80;
  MINTERM_ABNC   = $40;
  MINTERM_ANBC   = $20;
  MINTERM_ANBNC  = $10;
  MINTERM_NABC   = $08;
  MINTERM_NABNC  = $04;
  MINTERM_NANBC  = $02;
  MINTERM_NANBNC = $01;

// enMintermOperations
// Copy source and blit thru mask
  MINTERM_SRCMASK = MINTERM_ABC or MINTERM_ABNC or MINTERM_ANBC;
  // Invert source and blit thru mask
  MINTERM_NOTSRCMASK = MINTERM_ANBC;
  // Copy source and blit thru mask
  MINTERM_B_EQUALS_C = MINTERM_ABC or MINTERM_ANBNC or MINTERM_NABC or MINTERM_NANBNC;
  MINTERM_B_OR_C  = MINTERM_ABC or MINTERM_ABNC or MINTERM_NABC or MINTERM_NABNC or MINTERM_ANBC or MINTERM_NANBC;
  MINTERM_A_OR_B  = MINTERM_ABC or MINTERM_ANBC or MINTERM_NABC or MINTERM_ABNC or MINTERM_ANBNC or MINTERM_NABNC;
  MINTERM_A_OR_C  = MINTERM_ABC or MINTERM_NABC or MINTERM_ABNC or MINTERM_ANBC or MINTERM_NANBC or MINTERM_ANBNC;
  MINTERM_A_XOR_C = MINTERM_NABC or MINTERM_ABNC or MINTERM_NANBC or MINTERM_ANBNC;
  MINTERM_A_TO_D  = MINTERM_ABC or MINTERM_ANBC or MINTERM_ABNC or MINTERM_ANBNC;

type
  TPIX_FMT = LongWord;

  PPlanarYUVInfo = ^TPlanarYUVInfo;
  TPlanarYUVInfo = record
    YMemory: APTR;
    UMemory: APTR;
    VMemory: APTR;
    YBytesPerRow: LongWord;
    UBytesPerRow: LongWord;
    VBytesPerRow: LongWord;
  end;

const
// defines for code values for getcode this really belongs to graphics, and is of no
// use for layers. It's here only for traditional reasons.
  ISLESSX = 1;
  ISLESSY = 2;
  ISGRTRX = 4;
  ISGRTRY = 8;

//------ Font Styles
  FS_NORMAL           = 0;  // normal text (no style bits set)
  FSB_UNDERLINED      = 0;  // underlined (under baseline)
  FSF_UNDERLINED      = $01;
  FSB_BOLD            = 1;  // bold face text (ORed w/ shifted)
  FSF_BOLD            = $02;
  FSB_ITALIC          = 2;  // italic (slanted 1:2 right)
  FSF_ITALIC          = $04;
  FSB_EXTENDED        = 3;  // extended face (wider than normal)
  FSF_EXTENDED        = $08;
  FSB_ANTIALIASED     = 4;  // this uses AATextFont structure (V48)
  FSF_ANTIALIASED     = $10;

  FSB_COLORFONT       = 6;  // this uses ColorTextFont structure
  FSF_COLORFONT       = $40;
  FSB_TAGGED          = 7;  // the TextAttr is really an TTextAttr,
  FSF_TAGGED          = $80;

//------ Font Flags
  FPB_ROMFONT         = 0;  // font is in rom
  FPF_ROMFONT         = $01;
  FPB_DISKFONT        = 1;  // font is from diskfont.library
  FPF_DISKFONT        = $02;
  FPB_REVPATH         = 2;  // designed path is reversed (e.g. left)
  FPF_REVPATH         = $04;
  FPB_TALLDOT         = 3;  // designed for hires non-interlaced
  FPF_TALLDOT         = $08;
  FPB_WIDEDOT         = 4;  // designed for lores interlaced
  FPF_WIDEDOT         = $10;
  FPB_PROPORTIONAL    = 5;  // character sizes can vary from nominal
  FPF_PROPORTIONAL    = $20;
  FPB_DESIGNED        = 6;  // size is "designed", not constructed
  FPF_DESIGNED        = $40;
  FPB_REMOVED         = 7;  // the font has been removed
  FPF_REMOVED         = 1 shl 7;

const
// ****** Text Tags ****************************************************
  TA_DeviceDPI = 1 or TAG_USER; // Tag value is Point union: Hi LongInt XDPI, Lo LongInt YDPI
  TA_CharSet   = 3 or TAG_USER; // New in V46: IANA Charset number
  TA_Rebuild   = 4 or TAG_USER; // V50: boolean flag for OpenDiskFont() to force rebuilding a font
  MAXFONTMATCHWEIGHT = 32767;   // perfect match from WeighTAMatch

//----- tfe_Flags0 (partial definition) ----------------------------
  TE0B_NOREMFONT = 0;    // disallow RemFont for this font
  TE0F_NOREMFONT = $01;
//----- tfe_Flags1 (private) ---------------------------------------
  TE1F_NOUNDERLINING    = $01; // Cant make this font underlined
  TE1F_NOEMBOLDENING    = $02; // Cant make this font bold
  TE1F_NOSHEARING       = $04; // Cant make this font italic
  TE1F_NODEUNDERLINING  = $08; // Cant make this font non-underlined
  TE1F_NODEEMBOLDENING  = $10; // Cant make this font non-bold
  TE1F_NODESHEARING     = $20; // Cant make this font non-italic
  TE1F_NOANTIALIASING   = $40; // Cant make this font antialiased
  TE1F_GENFONT          = $80; // This font was generated via bullet API

//----- ctf_Flags --------------------------------------------------
const
  CT_COLORMASK  =  $000F; // mask to get to following color styles
  CT_COLORFONT  =  $0001; // color map contains designer's colors
  CT_GREYFONT   =  $0002; // color map describes even-stepped brightnesses from low to high
  CT_ANTIALIAS  =  $0004; // zero background thru fully saturated char
  CTB_MAPCOLOR  =  8;     // map ctf_FgColor to the rp_FgPen if it's
  CTF_MAPCOLOR  =  $0100; // is a valid color within ctf_Low..ctf_High

// drawing modes
  JAM1        = 0;  // jam 1 color into raster
  JAM2        = 1;  // jam 2 colors into raster
  COMPLEMENT  = 2;  // XOR bits into raster
  INVERSVID   = 4;  // inverse video for drawing modes
  BGBACKFILL  = 8;  // use backfill instead of BgPen
  FILLALPHA   = 16; // draw background under alpha pixels
  LEVELS      = 32; // fill text extent with alpha levels

// these are the flag bits for RastPort flags
  FRST_DOT    = $01; // draw the first dot of this line ?
  ONE_DOT     = $02; // use one dot mode for drawing lines
  DBUFFER     = $04; // flag set when RastPorts are double-buffered
// only used for bobs
  AREAOUTLINE = $08; // used by areafiller
  NOCROSSFILL = $20; // areafills have no crossovers
// graphics.library V51 extensions
  RPF_USE_FGCOLOR      = $80;   // draw with rp->FGPenColor
  RPF_USE_BGCOLOR      = $100;  // draw with rp->BGPenColor
  RPF_USE_OCOLOR       = $200;  // draw with rp->OPenColor
  RPF_FG_BLEND         = $800;  // private for now
  RPF_REMAP_COLORFONTS = $2000; // use ctf->ctf_ColorFontTable

// there is only one style of clipping: raster clipping
// this preserves the continuity of jaggies regardless of clip window
// When drawing into a RastPort, if the ptr to ClipRect is nil then there
// is no clipping done, this is dangerous but useful for speed

  // graphics copper list intstruction definitions
  COPPER_MOVE = 0;      // pseude opcode for move #XXXX,dir
  COPPER_WAIT = 1;      // pseudo opcode for wait y,x
  CPRNXTBUF   = 2;      // continue processing with next buffer
  CPR_NT_LOF  = $8000;  // copper instruction only for short frames
  CPR_NT_SHT  = $4000;  // copper instruction only for long frames
  CPR_NT_SYS  = $2000;  // copper user instruction only

  // mode coercion definitions
  PRESERVE_COLORS = 1; // Ensure that the mode coerced to can display just as many colours as the ViewPort being coerced.
  AVOID_FLICKER   = 2; // Ensure that the mode coerced to is not interlaced.
  IGNORE_MCOMPAT  = 4; // Coercion should ignore monitor compatibility issues.

  BIDTAG_COERCE   = 1; //  Private

// VSprite flags
// user-set VSprite flags:
  SUSERFLAGS  = $00FF; // mask of all user-settable VSprite-flags
  VSPRITE_f   = $0001; // set if VSprite, clear if Bob
  SAVEBACK    = $0002; // set if background is to be saved/restored
  OVERLAY     = $0004; // set to mask image of Bob onto background
  MUSTDRAW    = $0008; // set if VSprite absolutely must be drawn
// system-set VSprite flags:
  BACKSAVED   = $0100; // this Bob's background has been saved
  BOBUPDATE   = $0200; // temporary flag, useless to outside world
  GELGONE     = $0400; // set if gel is completely clipped (offscreen)
  VSOVERFLOW  = $0800; // VSprite overflow (if MUSTDRAW set we draw!)

// Bob flags
// these are the user flag bits
  BUSERFLAGS  = $00FF; // mask of all user-settable Bob-flags
  SAVEBOB     = $0001; // set to not erase Bob
  BOBISCOMP   = $0002; // set to identify Bob as AnimComp
// these are the system flag bits
  BWAITING    = $0100; // set while Bob is waiting on 'after'
  BDRAWN      = $0200; // set when Bob is drawn this DrawG pass
  BOBSAWAY    = $0400; // set to initiate removal of Bob
  BOBNIX      = $0800; // set when Bob is completely removed
  SAVEPRESERVE = $1000;// for back-restore during double-buffer
  OUTSTEP     = $2000; // for double-clearing if double-buffer

// defines for the animation procedures
  ANFRACSIZE  = 6;
  ANIMHALF    = $0020;
  RINGTRIGGER = $0001;

type
  PBob = ^TBob;
  PAnimComp = ^TAnimComp;
  PVSprite = ^TVSprite;
  PDBufPacket = ^TDBufPacket;
  PAnimOb = ^TAnimOb;
  PRastPort = ^TRastPort;
  PLayer = ^TLayer;

  // a structure to contain the 16 collision procedure addresses
  CollTable = array[0..15] of Pointer;
  PCollTable = ^CollTable;

  PClipRect = ^TClipRect;
  TClipRect = record
    Next: PClipRect;    // roms used to find next ClipRect
    OffX: SmallInt;     // offsets of cliprect into bitmap (V52)
    OffY: SmallInt;
    lobs: PLayer;       // In V45, this is no longer a valid pointer since a cliprect can be obscured by more than one
                        // layer. Just test for NULL or non-nil, do *NOT* dereference.
    BitMap: PBitMap;    // backing store bitmap if lobs <> nil
    bounds: TRectangle; // bounds of this cliprect
    vlink: PClipRect;   // Layers private use!!!
    home: Pointer;      // PLayer_Info where this cliprect belongs to.
    Flags: LongInt;     // Layers private field for cliprects
  end;

  TLayer = record
    front,
    back: PLayer;
    ClipRect: PClipRect; // singly linked list of active cliprects
    rp: PRastPort;       // rastport to draw into. Its layer is me
    bounds: TRectangle;  // screen bounds of this layer
    nlink: PLayer;       // new in V45: next back layer for display reorganization
    priority: Word;      // internal use: on layer front/back move,  relative priority of the layers. Topmost layer has lowest priority.
    Flags: Word;         // flags: LAYER*
    SuperBitMap: PBitMap;     // if non-nil, superbitmap layer
    SuperClipRect: PClipRect; // super bitmap cliprects if VBitMap <> nil else damage cliprect list for refresh
    Window: APTR;        // Intuition keeps its window here
    Scroll_X: Word;      // layer displacement
    Scroll_Y: Word;
    cr: PClipRect;       // used by moveinfrontof
    cr2: PClipRect;      // the new on- and off-screen cliprects to combine.
    cr3: PClipRect;      // keeps the new templates that are not clipped by user/damage list
    SuperSaveClipRects: PClipRect; // five preallocated super cr's
    _Cliprects: PClipRect;  // templates that are not clipped by damage list or user clip rect
    LayerInfo: Pointer;     // PLayer_Info points to head of the list
    Lock: TSignalSemaphore; // access to this layer
    BackFill: PHook;        // backfill hook
    ShapeRegion: PRegion;   // the region that comprises the shape of this layer
    ClipRegion: PRegion;    // user InstallClipRegion()'d region
    Clipped: PClipRect;     // clipped away by damage list or user clip rect
    Width: SmallInt;        // system use
    Height: SmallInt;       // system use
    ShapeHook: PHook;       // hook used to generate the shaperegion
    reserved2: array [0..3] of Byte; // more reserved fields
    BitMap: PBitMap;        // layer's own off-screen bitmap
    Extension: APTR;        // layer extension (system private)
    Reserved3: Byte;        // another reserved field
    Opaqueness: Byte;       // layer opaqueness
    DamageList: PRegion;    // list of rectangles to refresh
  end;

//****** TextAttr node, matches text attributes in RastPort
  PTextAttr = ^TTextAttr;
  TTextAttr = record
    ta_Name: STRPTR; // name of the font
    ta_YSize: Word;  // height of the font
    ta_Style: Byte;  // intrinsic font style
    ta_Flags: Byte;  // font preferences and flags
  end;

  PTTextAttr = ^TTTextAttr;
  TTTextAttr = record
    tta_Name: STRPTR;   // name of the font
    tta_YSize: Word;    // height of the font
    tta_Style: Byte;    // intrinsic font style
    tta_Flags: Byte;    // font preferences AND flags
    tta_Tags: PTagItem; // extended attributes
  end;

// ***** TextFonts node ************************************************

  PTextFont = ^TTextFont;
  TTextFont = record
    tf_Message: TMessage; // reply message for font removal
                          // font name in LN       \    used in this
    tf_YSize: Word;       // font height           |    order to best
    tf_Style: Byte;       // font style            |    match a font
    tf_Flags: Byte;       // preferences and flags /    request.
    tf_XSize: Word;       // nominal font width
    tf_Baseline: Word;    // distance from the top of char to baseline
    tf_BoldSmear: Word;   // smear to affect a bold enhancement

    tf_Accessors: Word;   // access count

    tf_LoChar: Byte;      // the first character described here
    tf_HiChar: Byte;      // the last character described here
    tf_CharData: APTR;    // the bit character data

    tf_Modulo: Word;      // the row modulo for the strike font data
    tf_CharLoc: APTR;     // ptr todata for the strike font
                          // 2 words: bit offset then size
    tf_CharSpace: APTR;   // ptr to words of proportional spacing data
    tf_CharKern: APTR;    // ptr to words of kerning data
  end;

  PTextFontExtension = ^TTextFontExtension;  // this structure is read-only
  TTextFontExtension = record    // this structure is read-only
    tfe_MatchWord: Word;         // a magic cookie for the extension
    tfe_Flags0: Byte;            // (system private flags)
    tfe_Flags1: Byte;            // (system private flags)
    tfe_BackPtr: PTextFont;      // validation of compilation
    tfe_OrigReplyPort: PMsgPort; // original value in tf_Extension
    tfe_Tags: PTagItem;          // Text Tags for the font
    tfe_OFontPatchS,             // (system private use)
    tfe_OFontPatchK: APTR;       // (system private use)
    tfe_Private0: APTR;          // (guess what)
    // this space is reserved for future expansion
   end;

//----- ColorFontColors --------------------------------------------
  PColorFontColors = ^TColorFontColors;
  TColorFontColors = record
    cfc_Reserved: Word;    // *must* be zero
    cfc_Count: Word;       // number of entries in cfc_ColorTable
    cfc_ColorTable: PWord; // 4 bit per component color map packed xRGB
  end;

//----- ColorTextFont ----------------------------------------------
  PColorTextFont = ^TColorTextFont;
  TColorTextFont = record
    ctf_TF: TTextFont;
    ctf_Flags: Word;      // extended flags
    ctf_Depth,            // number of bit planes
    ctf_FgColor,          // color that is remapped to FgPen
    ctf_Low,              // lowest color represented here
    ctf_High,             // highest color represented here
    ctf_PlanePick,        // PlanePick ala Images
    ctf_PlaneOnOff: Byte; // PlaneOnOff ala Images
    ctf_ColorFontColors: PColorFontColors; // colors for font
    ctf_CharData: array[0..7] of APTR;     //Pointers to bit planes ala tf_CharData
  end;

//----- AATextFont -------------------------------------------------
  PAATextFont = ^TAATextFont;
  TAATextFont = record
    aatf_CTF: TColorTextFont;
    aatf_Modulo: Word;         // Row modulo for the strike font data
    aatf_FreeMe: Word;         // Private, defaults to 0 */
    aatf_CharData: APTR;       // Chunky alpha maps for characters */
    aatf_CharLoc: APTR;        // Pointer to location data for the strike font. Two words: pixel offset then size
    aatf_SelfPtr: PAATextFont; // Pointer to the structure itself to be able to detect modified
                               // TextFont copies that are no real AATextFonts
    aatf_Reserved: array[0..1] of LongWord; // For future expansion, default to 0
  end;

// ***** TextExtent node ***********************************************
  PTextExtent = ^TTextExtent;
  TTextExtent = record
    te_Width: Word;        // same as TextLength
    te_Height: Word;       // same as tf_YSize
    te_Extent: TRectangle; // relative to CP
  end;

  PCopIns = ^TCopIns;
  TCopIns = record
    OpCode: SmallInt;    // 0 = move, 1 = wait
    VWaitAddr: SmallInt; // vertical or horizontal wait position
    HWaitData: SmallInt; // destination Pointer or data to send
  end;

// structure of cprlist that points to list that hardware actually executes }
  PCprList = ^TCprList;
  TCprList = record
    Next: PCprList;
    Start: PSmallInt;   // start of copper list
    MaxCount: SmallInt; // number of long instructions
  end;

  PCopList = ^TCopList;
  TCopList = record
    Next: PCopList;        // next block for this copper list
    CopList: PCopList;    // system use
    ViewPort : Pointer;   // system use
    CopIns: PCopIns;      // start of this block
    CopPtr: PCopIns;      // intermediate ptr
    CopLStart: PWord;     // mrgcop fills this in for Long Frame
    CopSStart: PWord;     // mrgcop fills this in for Short Frame
    Count: SmallInt;      // intermediate counter
    MaxCount: SmallInt;   // max # of copins for this block
    DyOffset: SmallInt;   // offset this copper list vertical waits
    SLRepeat: Word;
    Flags: Word;
  end;

  PUCopList = ^TUCopList;
  TUCopList = record
    Next: PUCopList;
    FirstCopList: PCopList; // head node of this copper list
    CopList: PCopList;      // node in use
  end;

// Private graphics data structure. This structure has changed in the past,
// and will continue to change in the future. Do Not Touch!
  PCopInit = ^TCopInit;
  TCopInit = record
    vsync_hblank: array[0..1] of word;
    diagstrt: array[0..11] of word;     // copper list for first bitplane
    fm0: array[0..1] of word;
    diwstart: array[0..9] of word;
    bplcon2: array[0..1] of word;
    sprfix: array[0..(2*8) - 1] of word;
    sprstrtup: array[0..(2*8*2) - 1] of Word;
    wait14: array[0..1] of word;
    norm_hblank: array[0..1] of word;
    jump: array[0..1] of word;
    wait_forever: array[0..5] of word;
    sprstop: array[0..7] of Word;
  end;

  PAreaInfo = ^TAreaInfo;
  TAreaInfo = record
    VctrTbl: PSmallInt; // ptr to start of vector table
    VctrPtr: PSmallInt; // ptr to current vertex
    FlagTbl: PByte;     // ptr to start of vector flag table
    FlagPtr: PByte;     // ptrs to areafill flags
    Count: SmallInt;    // number of vertices in list
    MaxCount: SmallInt; // AreaMove/Draw will not allow Count>MaxCount
    FirstX: SmallInt;
    FirstY: SmallInt;   // first point for this polygon
  end;

  PTmpRas = ^TTmpRas;
  TTmpRas = record
    RasPtr: TPlanePtr;
    Size: LongInt;
  end;

// unoptimized for 32bit alignment of Pointers
  PGelsInfo = ^TGelsInfo;
  TGelsInfo = record
    sprRsrvd: Shortint; // flag of which sprites to reserve from vsprite system
    Flags: Byte;        // system use
    gelHead: PVSprite;
    gelTail: PVSprite;  // dummy vSprites for list management
    // Pointer to array of 8 WORDS for sprite available lines
    nextLine: PWord;
    // Pointer to array of 8 Pointers for color-last-assigned to vSprites
    lastColor: Pointer;
    collHandler: PCollTable; // addresses of collision routines
    leftmost: SmallInt;
    rightmost: SmallInt;
    topmost: SmallInt;
    bottommost: SmallInt;
    firstBlissObj: APTR;
    lastBlissObj: APTR;   // system use only
  end;

  TRastPort = record
    Layer: PLayer;
    BitMap: PBitMap;
    AreaPtrn: PWord;      // ptr to areafill pattern
    TmpRas: PTmpRas;
    AreaInfo: PAreaInfo;
    GelsInfo: PGelsInfo;
    Mask: Byte;           // write mask for this raster
    FgPen: Shortint;      // foreground pen for this raster
    BgPen: Shortint;      // background pen
    AOlPen: Shortint;     // areafill outline pen
    DrawMode: Shortint;   // drawing mode for fill, lines, and text
    AreaPtSz: Shortint;   // 2^n words for areafill pattern
    linpatcnt: Shortint;  // current line drawing pattern preshift
    dummy: Shortint;
    Flags: Word;          // miscellaneous control bits
    LinePtrn: Word;       // 16 bits for textured lines
    cp_x, cp_y: SmallInt; // current pen position
    minterms: array[0..7] of Byte;
    PenWidth: SmallInt;
    PenHeight: SmallInt;
    Font: PTextFont;      // current font Pointer
    AlgoStyle: Byte;      // the algorithmically generated style
    TxFlags: Byte;        // text specific flags
    TxHeight: Word;       // text height
    TxWidth: Word;        // text nominal width
    TxBaseline: Word;     // text baseline
    TxSpacing: SmallInt;  // text spacing (per character)
    RP_User: Pointer;
    LongFlags: LongWord;  // V51: private
    ExtensionData: APTR;  // V51: private rastport extension pointer
    RenderDomain: TRectangle; // V51: private
    FGColor: LongWord;    // V51: foreground color, don't peek or poke directly!
    BGColor: LongWord;    // V51: background color, don't peek or poke directly!
    OColor: LongWord;     // V51: outline color, don't peek or poke directly!
    wordreserved: Word;   // for future use
  end;

// Extended RastPort data, new for v52.2. GfxNew() must be used to
// allocate this structure! Currently unused, reserved for future use.
  PRastPortExtra = ^TRastPortExtra;
  TRastPortExtra = record
    Node: TExtendedNode;                // graphics extended node
    RastPort: PRastPort;                // backlink
    Reserved: array[0..15] of LongWord; // avoid recompilation ;-)
  end;

// UserStuff definitions
// the user can define these to be a single variable or a sub-structure
//  if undefined by the user, the system turns these into innocuous variables
//  see the manual for a thorough definition of the UserStuff definitions

  VUserStuff  = SmallInt;  // Sprite user stuff
  BUserStuff  = SmallInt;  // Bob user stuff
  AUserStuff  = SmallInt;  // AnimOb user stuff

// ********************** GEL STRUCTURES *******************************
  TVSprite = record
    //-- SYSTEM VARIABLES
    NextVSprite: PVSprite; // GEL linked list forward/backward Pointers sorted by y,x value
    PrevVSprite: PVSprite;
    // GEL draw list constructed in the order the Bobs are actually drawn, then list is copied to clear list must be here in VSprite for system boundary detection
    DrawPath: PVSprite;    // Pointer of overlay drawing
    ClearPath: PVSprite;   // Pointer for overlay clearing
    // the VSprite positions are defined in (y,x) order to make sorting sorting easier, since (y,x) as a long LongInt
    OldY, OldX: SmallInt;  // previous position
    //-- COMMON VARIABLES
    Flags: SmallInt;       // VSprite flags
    //-- USER VARIABLES
    // the VSprite positions are defined in (y,x) order to make sorting sorting easier, since (y,x) as a long LongInt
    Y, X: SmallInt;        // screen position
    Height: SmallInt;
    Width: SmallInt;       // number of words per row of image data
    Depth: SmallInt;       // number of planes of data
    MeMask: SmallInt;      // which types can collide with this VSprite
    HitMask: SmallInt;     // which types this VSprite can collide with
    ImageData: PSmallInt;  // Pointer to VSprite image
    // borderLine is the one-dimensional logical OR of all the VSprite bits, used for fast collision detection of edge
    BorderLine: PSmallInt; // logical OR of all VSprite bits
    CollMask: PSmallInt;   // similar to above except this is a matrix
    // Pointer to this VSprite's color definitions (not used by Bobs)
    SprColors: PSmallInt;
    VSBob: PBob;           // points home if this VSprite is part of a Bob
    { planePick flag:  set bit selects a plane from image, clear bit selects
      use of shadow mask for that plane
      OnOff flag: if using shadow mask to fill plane, this bit (corresponding
      to bit in planePick) describes whether to fill with 0's or 1's
      There are two uses for these flags:
       - if this is the VSprite of a Bob, these flags describe how the Bob
         is to be drawn into memory
       - if this is a simple VSprite and the user intends on setting the
         MUSTDRAW flag of the VSprite, these flags must be set too to describe
         which color registers the user wants for the image}
    PlanePick: Shortint;
    PlaneOnOff: Shortint;
    VUserExt: VUserStuff;   // user definable: see note above
  end;

  // blitter-objects
  TBob = record
    //-- SYSTEM VARIABLES
    //-- COMMON VARIABLES
    Flags: SmallInt;       // general purpose flags (see definitions below)
    //-- USER VARIABLES
    SaveBuffer: PSmallInt; // Pointer to the buffer for background save
    // used by Bobs for "cookie-cutting" and multi-plane masking
    ImageShadow : PSmallInt;
    // Pointer to BOBs for sequenced drawing of Bobs for correct overlaying of multiple component animations
    Before: PBob;         // draw this Bob before Bob pointed to by before
    After: PBob;          // draw this Bob after Bob pointed to by after
    BobVSprite: PVSprite; // this Bob's VSprite definition
    BobComp: PAnimComp;   // Pointer to this Bob's AnimComp def
    DBuffer: PDBufPacket; // Pointer to this Bob's dBuf packet
    BUserExt: BUserStuff; // Bob user extension
  end;

  TAnimComp = record
    //-- SYSTEM VARIABLES
    //-- COMMON VARIABLES
    Flags: SmallInt;        // AnimComp flags for system & user
    // timer defines how long to keep this component active:
    //  if set non-zero, timer decrements to zero then switches to nextSeq
    //  if set to zero, AnimComp never switches
    Timer: SmallInt;
    //-- USER VARIABLES
    // initial value for timer when the AnimComp is activated by the system
    TimeSet: SmallInt;
    // Pointer to next and previous components of animation object
    NextComp: PAnimComp;
    PrevComp: PAnimComp;
    // Pointer to component component definition of next image in sequence
    NextSeq: PAnimComp;
    PrevSeq: PAnimComp;
    // address of special animation procedure
    AnimCRoutine: Pointer;
    YTrans: SmallInt;      // initial y translation (if this is a component)
    XTrans: SmallInt;      // initial x translation (if this is a component)
    HeadOb: PAnimOb;
    AnimBob: pBob;
  end;

  PPAnimOb = ^PAnimOb;
  TAnimOb = record
    //-- SYSTEM VARIABLES
    NextOb: PAnimOb;
    PrevOb: PAnimOb;
    Clock: LongInt;           // number of calls to Animate this AnimOb has endured
    AnOldY, AnOldX: SmallInt; // old y,x coordinates
    //-- COMMON VARIABLES
    AnY, AnX: SmallInt;       // y,x coordinates of the AnimOb
    //-- USER VARIABLES
    YVel, XVel: SmallInt;     // velocities of this object
    YAccel, XAccel: SmallInt; // accelerations of this object
    RingYTrans,
    RingXTrans: SmallInt;     // ring translation values
    AnimORoutine: Pointer;    // Pointer of special animation procedure
    HeadComp: PAnimComp;      // Pointer to first component
    AUserExt: AUserStuff;     // AnimOb user extension
  end;

// dBufPacket defines the values needed to be saved across buffer to buffer
// when in double-buffer mode
  tDBufPacket = record
    BufY, BufX: SmallInt; // save other buffers screen coordinates
    BufPath: PVSprite;    // carry the draw path over the gap
    // these Pointers must be filled in by the user
    // Pointer to other buffer's background save buffer
    BufBuffer: PSmallInt;
  end;
const
  B2NORM   = 0;
  B2SWAP   = 1;
  B2BOBBER = 2;

{ ************************************************************************ }
const
// internal TClipRect.flags
  CR_USERCLIPPED   = 16;   // out of user clip rectangle
  CR_DAMAGECLIPPED = 32;   // out of damage cliprects
  CR_LAYERBITMAP   = 64;   // don't free the BitMap
  CR_ALPHATYPE     = 128;  // not opaque, or covered by same
  CR_INVISIBLE     = 256;  // fully obscured, no own BitMap
  CR_INTUITION     = 1024; // internal system use

// defines for shape hooks
  SHAPEHOOKACTION_CREATELAYER      = 0;
  SHAPEHOOKACTION_MOVELAYER        = 1; // Only sent if LAYERMOVECHANGESSHAPE is set
  SHAPEHOOKACTION_SIZELAYER        = 2;
  SHAPEHOOKACTION_MOVESIZELAYER    = 3;
  SHAPEHOOKACTION_CHANGELAYERSHAPE = 4;
  SHAPEHOOKACTION_DELETELAYER      = 5;
  SHAPEHOOKACTION_GETHOOKACTIONS   = 6;
type
  PShapeHookMsg = ^TShapeHookMsg;
  TShapeHookMsg = record
    Action: LongWord;
    NewShape: PRegion;
    OldShape: PRegion;
    NewBounds: PRectangle;
    OldBounds: PRectangle;
  end;
// defines for alpha hooks
const
  ALPHAHOOKACTION_CREATELAYER      = 0;
  ALPHAHOOKACTION_MOVELAYER        = 1; // Only sent if LAYERMOVECHANGESSHAPE is set
  ALPHAHOOKACTION_SIZELAYER        = 2;
  ALPHAHOOKACTION_MOVESIZELAYER    = 3;
  ALPHAHOOKACTION_CHANGELAYERALPHA = 4;
  ALPHAHOOKACTION_DELETELAYER      = 5;
  ALPHAHOOKACTION_GETHOOKACTIONS   = 6;
type
  PAlphaHookMsg = ^TAlphaHookMsg;
  TAlphaHookMsg = record
    Action: LongWord;
    NewAlphaCR: PClipRect;
    OldAlphaCR: PClipRect;
    NewBounds: PRectangle;
    OldBounds: PRectangle;
  end;

const
{   These bit descriptors are used by the GEL collide routines.
 *  These bits are set in the hitMask and meMask variables of
 *  a GEL to describe whether or not these types of collisions
 *  can affect the GEL.  BNDRY_HIT is described further below;
 *  this bit is permanently assigned as the boundary-hit flag.
 *  The other bit GEL_HIT is meant only as a default to cover
 *  any GEL hitting any other; the user may redefine this bit.}
  BORDERHIT = 0;

{   These bit descriptors are used by the GEL boundry hit routines.
 *  When the user's boundry-hit routine is called (via the argument
 *  set by a call to SetCollision) the first argument passed to
 *  the user's routine is the Pointer of the GEL involved in the
 *  boundry-hit, and the second argument has the appropriate bit(s)
 *  set to describe which boundry was surpassed }
  TOPHIT    = 1;
  BOTTOMHIT = 2;
  LEFTHIT   = 4;
  RIGHTHIT  = 8;
// Porter/Duff Image Composition
  COMPOSITE_Clear         = 0;
  COMPOSITE_Src           = 1;
  COMPOSITE_Dest          = 2;
  COMPOSITE_Src_Over_Dest = 3;
  COMPOSITE_Dest_Over_Src = 4;
  COMPOSITE_Src_In_Dest   = 5;
  COMPOSITE_Dest_In_Src   = 6;
  COMPOSITE_Src_Out_Dest  = 7;
  COMPOSITE_Dest_Out_Src  = 8;
  COMPOSITE_Src_Atop_Dest = 9;
  COMPOSITE_Dest_Atop_Src = 10;
  COMPOSITE_Src_Xor_Dest  = 11;
  COMPOSITE_Plus          = 12;
  COMPOSITE_Maximum       = 13;
  COMPOSITE_Minimum       = 14;
  COMPOSITE_NumOperators  = 15;

// Tag items for the Composite call
  COMPTAG_Base  = TAG_USER;
  COMPTAG_SrcX       = COMPTAG_Base + 0;  // (uint32) X clip coordinate on source bitmap (defaults to 0)
  COMPTAG_SrcY       = COMPTAG_Base + 1;  // (uint32) Y clip coordinate on source bitmap (defaults to 0)
  COMPTAG_SrcWidth   = COMPTAG_Base + 2;  // (uint32) Width of clip rectangle on source (defaults to full width)
  COMPTAG_SrcHeight  = COMPTAG_Base + 3;  // (uint32) Height of clip rectangle on source (defaults to full height)
  COMPTAG_DestX      = COMPTAG_Base + 4;  // (uint32) X clip coordinate on dest bitmap (defaults to 0)
  COMPTAG_DestY      = COMPTAG_Base + 5;  // (uint32) Y clip coordinate on dest bitmap (defaults to 0)
  COMPTAG_DestWidth  = COMPTAG_Base + 6;  // (uint32) Width of clip rectangle on dest (defaults to full width)
  COMPTAG_DestHeight = COMPTAG_Base + 7;  // (uint32) Height of clip rectangle on dest (defaults to full height)
  COMPTAG_SrcAlpha   = COMPTAG_Base + 8;  // (fixpoint) (additional) Alpha for source bitmap (no default)
  COMPTAG_DestAlpha  = COMPTAG_Base + 9;  // (fixpoint) (additional) Alpha for destination bitmap (no default)
  COMPTAG_ScaleX     = COMPTAG_Base + 10; // (fixpoint) X scale factor for source bitmap (defaults to 1.0)
  COMPTAG_ScaleY     = COMPTAG_Base + 11; // (fixpoint) Y scale factor for source bitmap (defaults to 1.0)
  COMPTAG_SrcAlphaMask = COMPTAG_Base + 12; // (struct Bitmap *; Alpha mask for source. Specifying this tag overrides any alpha that might be present in the source bitmap.
  COMPTAG_DestAlphaMask = COMPTAG_Base + 13; // (struct Bitmap *; Alpha mask for the destination. Specifying this tag overrides any alpha that might be present in the destination bitmap.
  COMPTAG_Flags      = COMPTAG_Base + 18; // (uint32, see defines below; Specifies a set of flags that may modify the operation. See the defines below
  COMPTAG_OffsetX    = COMPTAG_Base + 20; // (int32; X Coordinate on the destination bitmap that the operation should be applied to. Defaults to zero.
  COMPTAG_OffsetY    = COMPTAG_Base + 21; // (int32; Y Coordinate on the destination bitmap that the operation should be applied to. Defaults to zero.
  COMPTAG_FriendBitMap = COMPTAG_Base + 22; // (struct BitMap *; when the source and/or destination bitmaps are located in main memory, this tag tells the graphics system to upload the bitmaps to the same board the friend bitmap is located on.
  COMPTAG_DisplayID  = COMPTAG_Base + 23; // (uint32; the same as above, but a DisplayID is used as reference to the board and not a bitmap.
  COMPTAG_SrcAlphaX  = COMPTAG_Base + 14; // (uint32; the X/Y coordinates on the src alpha map to use for compositing. If not specified, use the same as the SrcX and SrcY
  COMPTAG_SrcAlphaY  = COMPTAG_Base + 15;
  COMPTAG_DestAlphaX = COMPTAG_Base + 16; // (uint32; the X/Y coordinates on the destination alpha map to use. If not specified, use the DestX and DestY
  COMPTAG_DestAlphaY = COMPTAG_Base + 17;
  // The following group of tag items deals with direct triangle mapping. Read the autodoc for a detailed explanation
  COMPTAG_VertexArray  = COMPTAG_Base + 30;
  COMPTAG_IndexArray   = COMPTAG_Base + 31;
  COMPTAG_VertexFormat = COMPTAG_Base + 32;
  COMPTAG_NumTriangles = COMPTAG_Base + 33;

// This group of tag items can be used to specify up to four colors, either as an 32 bit ARGB value, or as a set of discreet fixpoint numbers.
// The fixpoint numbers range is 0 to 1. Specifying a fixpoint component overrides the ARGB value completely.
  COMPTAG_Color0        = COMPTAG_Base + 40;
  COMPTAG_Color1        = COMPTAG_Base + 41;
  COMPTAG_Color2        = COMPTAG_Base + 42;
  COMPTAG_Color3        = COMPTAG_Base + 43;

  COMPTAG_Color0_Red      = COMPTAG_Base + 44;
  COMPTAG_Color0_Green    = COMPTAG_Base + 45;
  COMPTAG_Color0_Blue     = COMPTAG_Base + 46;
  COMPTAG_Color0_Alpha    = COMPTAG_Base + 47;

  COMPTAG_Color1_Red      = COMPTAG_Base + 48;
  COMPTAG_Color1_Green    = COMPTAG_Base + 49;
  COMPTAG_Color1_Blue     = COMPTAG_Base + 50;
  COMPTAG_Color1_Alpha    = COMPTAG_Base + 51;

  COMPTAG_Color2_Red      = COMPTAG_Base + 52;
  COMPTAG_Color2_Green    = COMPTAG_Base + 53;
  COMPTAG_Color2_Blue     = COMPTAG_Base + 54;
  COMPTAG_Color2_Alpha    = COMPTAG_Base + 55;

  COMPTAG_Color3_Red      = COMPTAG_Base + 56;
  COMPTAG_Color3_Green    = COMPTAG_Base + 57;
  COMPTAG_Color3_Blue     = COMPTAG_Base + 58;
  COMPTAG_Color3_Alpha    = COMPTAG_Base + 59;

{ Specifies the YUV to RGB conversion standard to use for YUV source bitmaps.
  The options are:
  - COMPYUV_BT601 - SD video standard ITU-R BT.601
  - COMPYUV_BT709 - HD video standard ITU-R BT.709

  NOTE: This is only relevant if the source bitmap is in one of the YUV formats
  (e.g., RGBFB_YUV420P).}
  COMPTAG_SrcYUVStandard = COMPTAG_Base + 60;
  COMPYUV_BT601 = 0;
  COMPYUV_BT709 = 1;

  COMPTAG_SrcYUVMatrix    = COMPTAG_Base + 61; // (PSingle) Provide a custom YUV to RGB conversion matrix

//Reserved
  COMPTAG_Private       = COMPTAG_Base + 34;
  COMPTAG_Private2      = COMPTAG_Base + 35;

// Vertex Array format flags
  COMPVF_STW0_Present    = $02;
  COMPVF_STW1_Present    = $04;

  COMPFLAG_SrcAlphaOverride   = 1 shl 0; // If set, the value specified in SrcAlpha overrides the value in the source bitmap, which means that the source bitmap is
                                         // assumed to have a constant alpha over the entire image. If not set, the SrcAlpha value is used to modulate/scale any other alpha channel.
  COMPFLAG_DestAlphaOverride  = 1 shl 1; // Like COMPFLAG_SrcAlphaOverride, for the destination bitmap.
  COMPFLAG_SrcFilter          = 1 shl 2; // If set, enables bilinear filtering of the source bitmap while scaling. While this can improve the quality of scaled images,
                                         // it might cause a dramatic slowdown when the operation is emulated in software.
  COMPFLAG_HardwareOnly       = 1 shl 3; // If set, the call will fail with an error code if the operation cannot be performed in hardware. Reasons for this include software-only bitmaps, unsupported color formats, etc.
  COMPFLAG_IgnoreDestAlpha    = 1 shl 4;
  COMPFLAG_ForceSoftware      = 1 shl 7; // If set, the operation will be emulated in software even if it could be performed in hardware. This is mostly useful for testing purposes. Setting this overrides COMPFLAG_HardwareOnly.
  COMPFLAG_Color1Modulate     = 1 shl 8; // If set, then Color 1 is used as a modulate color for the src bitmap. That is, each color component of each pixel in the source bitmap is multiplied with the color 1 (including its
                                         // alpha). All other effects stay in effect. This flag can essentially be used to "tint" a bitmap in the given color
// Tags for GraphicsControlTagList()
  GCTRL_GetForceSWComposite    = TAG_USER + 1;
  GCTRL_SetForceSWComposite    = TAG_USER + 2;
  GCTRL_GetUseDMA              = TAG_USER + 3;
  GCTRL_SetUseDMA              = TAG_USER + 4;
  GCTRL_GetUseAltiVec          = TAG_USER + 5;
  GCTRL_SetUseAltiVec          = TAG_USER + 6;
  GCTRL_GetDisableAmigaBlitter = TAG_USER + 7;
  GCTRL_SetDisableAmigaBlitter = TAG_USER + 8;
  GCTRL_GetPlanesToFast        = TAG_USER + 9;
  GCTRL_SetPlanesToFast        = TAG_USER + 10;
  GCTRL_Get31KHzScanRate       = TAG_USER + 11;
  GCTRL_Set31KHzScanRate       = TAG_USER + 12;

// enCompositeError
  COMPERR_Success          = 0;
  COMPERR_Incompatible     = 1; // Incompatible bitmaps for operation
  COMPERR_Value            = 2; // An input value is out of range
  COMPERR_SoftwareFallback = 3; // Operation would fall back to software emulation and hardware only was requested
  COMPERR_OutOfMemory      = 4; // The operation tried to allocate memory but failed
  COMPERR_Generic          = 5; // Some generic error has occurred
  COMPERR_UnknownOperator  = 6; // Unknown operator specified
  COMPERR_MissingInput     = 7; // Missing a mandatory tag item

  GMD_TagBase        = TAG_USER;
  GMD_VendorID       = GMD_TagBase + 1;
  GMD_Product        = GMD_TagBase + 2;
  GMD_ProductID      = GMD_TagBase + 3;
  GMD_HSyncMin       = GMD_TagBase + 4;
  GMD_HSyncMax       = GMD_TagBase + 5;
  GMD_VSyncMin       = GMD_TagBase + 6;
  GMD_VSyncMax       = GMD_TagBase + 7;
  GMD_DotClockMin    = GMD_TagBase + 8;
  GMD_DotClockMax    = GMD_TagBase + 9;
  GMD_DisplayWidth   = GMD_TagBase + 10;
  GMD_DisplayHeight  = GMD_TagBase + 11;
  GMD_InputType      = GMD_TagBase + 12;
  GMD_EDIDVersion    = GMD_TagBase + 13;
  GMD_EDIDRevision   = GMD_TagBase + 14;
  GMD_BasicAudio     = GMD_TagBase + 15;

// enMonitorInputType
  INPUTTYPE_VGA = 0;
  INPUTTYPE_DVI = 1;

const
  SS_GRAPHICS   =  $02;

  VIEW_EXTRA_TYPE      = 1;
  VIEWPORT_EXTRA_TYPE  = 2;
  SPECIAL_MONITOR_TYPE = 3;
  MONITOR_SPEC_TYPE    = 4;

type
  // structure used by AddTOFTask
  PIsrvstr = ^TIsrvstr;
  TIsrvstr = record
    is_Node: TNode;
    Iptr: PIsrvstr; // passed to srvr by os
    code: Pointer;
    ccode: Pointer;
    Carg: APTR;
  end;

  PAnalogSignalInterval = ^TAnalogSignalInterval;
  TAnalogSignalInterval = record
    asi_Start: Word;
    asi_Stop: Word;
   end;

  PSpecialMonitor = ^TSpecialMonitor;
  TSpecialMonitor = record
    spm_Node: TExtendedNode;
    spm_Flags: Word;
    do_monitor: Pointer;
    reserved1: Pointer;
    reserved2: Pointer;
    reserved3: Pointer;
    hblank: TAnalogSignalInterval;
    vblank: TAnalogSignalInterval;
    hsync: TAnalogSignalInterval;
    vsync: TAnalogSignalInterval;
  end;


  PMonitorSpec = ^TMonitorSpec;
  TMonitorSpec = record
    ms_Node: TExtendedNode;
    ms_Flags: Word;
    ratioh: LongInt;
    ratiov: LongInt;
    total_rows: Word;
    total_colorclocks: Word;
    DeniseMaxDisplayColumn: Word;
    BeamCon0: Word;
    min_row: Word;
    ms_Special: PSpecialMonitor;
    ms_OpenCount: Word;
    ms_transform: Pointer;
    ms_translate: Pointer;
    ms_scale: Pointer;
    ms_xoffset: Word;
    ms_yoffset: Word;
    ms_LegalView: TRectangle;
    ms_maxoscan: Pointer;      // maximum legal overscan
    ms_videoscan: Pointer;     // video display overscan
    DeniseMinDisplayColumn: Word;
    DisplayCompatible: LongWord;
    DisplayInfoDataBase: TList;
    DisplayInfoDataBaseSemaphore: TSignalSemaphore;
    ms_MrgCop: Pointer;
    ms_LoadView: Pointer;
    ms_KillView: Pointer;
  end;

const
  TO_MONITOR           =  0;
  FROM_MONITOR         =  1;
  STANDARD_XOFFSET     =  9;
  STANDARD_YOFFSET     =  0;

  MSB_REQUEST_NTSC     =  0;
  MSB_REQUEST_PAL      =  1;
  MSB_REQUEST_SPECIAL  =  2;
  MSB_REQUEST_A2024    =  3;
  MSB_DOUBLE_SPRITES   =  4;
  MSF_REQUEST_NTSC     =  1 shl MSB_REQUEST_NTSC;
  MSF_REQUEST_PAL      =  1 shl MSB_REQUEST_PAL;
  MSF_REQUEST_SPECIAL  =  1 shl MSB_REQUEST_SPECIAL;
  MSF_REQUEST_A2024    =  1 shl MSB_REQUEST_A2024;
  MSF_DOUBLE_SPRITES   =  1 shl MSB_DOUBLE_SPRITES;

// obsolete, v37 compatible definitions follow
  REQUEST_NTSC    =  1 shl MSB_REQUEST_NTSC;
  REQUEST_PAL     =  1 shl MSB_REQUEST_PAL;
  REQUEST_SPECIAL =  1 shl MSB_REQUEST_SPECIAL;
  REQUEST_A2024   =  1 shl MSB_REQUEST_A2024;

  DEFAULT_MONITOR_NAME: PChar = 'default.monitor';
  NTSC_MONITOR_NAME: PChar    = 'ntsc.monitor';
  PAL_MONITOR_NAME: PChar     = 'pal.monitor';
  STANDARD_MONITOR_MASK = REQUEST_NTSC or REQUEST_PAL;

  STANDARD_NTSC_ROWS    = 262;
  STANDARD_PAL_ROWS     = 312;
  STANDARD_COLORCLOCKS  = 226;
  STANDARD_DENISE_MAX   = 455;
  STANDARD_DENISE_MIN   = 93 ;
  STANDARD_NTSC_BEAMCON = $0000;
  STANDARD_PAL_BEAMCON  = $0020;

  //SPECIAL_BEAMCON = VARVBLANK or LOLDIS or VARVSYNC or VARHSYNC or VARBEAM or CSBLANK or VSYNCTRUE;

  MIN_NTSC_ROW    = 21;
  MIN_PAL_ROW     = 29;
  STANDARD_VIEW_X = $81;
  STANDARD_VIEW_Y = $2C;
  STANDARD_HBSTRT = $06;
  STANDARD_HSSTRT = $0B;
  STANDARD_HSSTOP = $1C;
  STANDARD_HBSTOP = $2C;
  STANDARD_VBSTRT = $0122;
  STANDARD_VSSTRT = $02A6;
  STANDARD_VSSTOP = $03AA;
  STANDARD_VBSTOP = $1066;

  VGA_COLORCLOCKS = STANDARD_COLORCLOCKS / 2;
  VGA_TOTAL_ROWS  = STANDARD_NTSC_ROWS * 2;
  VGA_DENISE_MIN  = 59;
  MIN_VGA_ROW     = 29;
  VGA_HBSTRT      = $08;
  VGA_HSSTRT      = $0E;
  VGA_HSSTOP      = $1C;
  VGA_HBSTOP      = $1E;
  VGA_VBSTRT      = $0000;
  VGA_VSSTRT      = $0153;
  VGA_VSSTOP      = $0235;
  VGA_VBSTOP      = $0CCD;

  VGA_MONITOR_NAME: PChar = 'vga.monitor';

// NOTE: VGA70 definitions are obsolete - a VGA70 monitor has never been implemented.
  VGA70_COLORCLOCKS = STANDARD_COLORCLOCKS / 2;
  VGA70_TOTAL_ROWS  = 449;
  VGA70_DENISE_MIN  = 59;
  MIN_VGA70_ROW     = 35;
  VGA70_HBSTRT      = $08;
  VGA70_HSSTRT      = $0E;
  VGA70_HSSTOP      = $1C;
  VGA70_HBSTOP      = $1E;
  VGA70_VBSTRT      = $0000;
  VGA70_VSSTRT      = $02A6;
  VGA70_VSSTOP      = $0388;
  VGA70_VBSTOP      = $0F73;

  //VGA70_BEAMCON = SPECIAL_BEAMCON xor VSYNCTRUE;
  VGA70_MONITOR_NAME: PChar = 'vga70.monitor';

  BROADCAST_HBSTRT  = $01;
  BROADCAST_HSSTRT  = $06;
  BROADCAST_HSSTOP  = $17;
  BROADCAST_HBSTOP  = $27;
  BROADCAST_VBSTRT  = $0000;
  BROADCAST_VSSTRT  = $02A6;
  BROADCAST_VSSTOP  = $054C;
  BROADCAST_VBSTOP  = $1C40;
  //BROADCAST_BEAMCON = LOLDIS or CSBLANK;
  RATIO_FIXEDPART   = 4;
  RATIO_UNITY       = 1 shl RATIO_FIXEDPART;

Type
  PColorMap = ^TColorMap;
  PViewPort = ^TViewPort;
  PRasInfo = ^TRasInfo;
  TRasInfo = record     // used by callers to and InitDspC()
    Next: PRasInfo;     // used for dualpf
    BitMap: PBitMap;
    RxOffset: SmallInt; // scroll offsets in this BitMap
    RyOffset: SmallInt;
  end;

  PView = ^TView;
  TView = record
    ViewPort: PViewPort;
    LOFCprList: PCprList; // used for interlaced and noninterlaced
    SHFCprList: PCprList; // only used during interlace
    DyOffset: SmallInt;   // for complete View positioning
    DxOffset: SmallInt;   // offsets are +- adjustments to standard #s
    Modes: Word;          // such as INTERLACE, GENLOC
  end;

// these structures are obtained via GfxNew and disposed by GfxFree
  PViewExtra = ^TViewExtra;
  TViewExtra = record
    n: TExtendedNode;
    View: PView;           // backwards link
    Monitor: PMonitorSpec; // monitors for this view
    TopLine: Word;
  end;

  TViewPort = record
    Next: PViewPort;
    ColorMap: PColorMap; // table of colors for this viewport if this is nil, MakeVPort assumes default values
    DspIns: pCopList;    // user by MakeView()
    SprIns: pCopList;    // used by sprite stuff
    ClrIns: pCopList;    // used by sprite stuff
    UCopIns: PUCopList;  // User copper list
    DWidth, DHeight: SmallInt;
    DxOffset, DyOffset: SmallInt;
    Modes: Word;
    SpritePriorities: Byte;
    ExtendedModes: Byte;
    RasInfo: PRasInfo;
  end;

  //  private structure
  PVecTable = ^TVecTable;
  TVecTable = record
  end;

// this structure is obtained via GfxNew and disposed by GfxFree
  PViewPortExtra = ^tViewPortExtra;
  tViewPortExtra = record
    n: TExtendedNode;
    ViewPort: PViewPort;     // backwards link
    DisplayClip: TRectangle; // makevp display clipping information
     // These are added for V39
    VecTable: Pointer;       // Private
    DriverData: array[0..1] of APTR;
    Flags: Word;
    Origin: array[0..1] of TPoint; // First visible point relative to the DClip. One for each possible playfield.
    cop1ptr: LongWord;       // private
    cop2ptr: LongWord;       // private
    MonitorData: APTR;       // private
  end;

  TColorMap = record
    Flags: Byte;
    Type_: Byte;
    Count: Word;
    ColorTable: APTR;
    cm_vpe: PViewPortExtra;
    LowColorBits: APTR;
    TransparencyPlane: Byte;
    SpriteResolution: Byte;
    SpriteResDefault: Byte;
    AuxFlags: Byte;
    cm_vp: PViewPort;
    NormalDisplayInfo: APTR;
    CoerceDisplayInfo: APTR;
    cm_batch_items: PTagItem;
    VPModeID: LongWord;
    PalExtra: Pointer;   // PPaletteExtra
    SpriteBase_Even: Word;
    SpriteBase_Odd: Word;
    Bp_0_base: Word;
    Bp_1_base: Word;
  end;

const
// if Type = 0 then ColorMap is V1.2/V1.3  compatible
// if Type <> 0 then ColorMap is V36       compatible
// the system will never create other than V39 type colormaps when running V39
  COLORMAP_TYPE_V1_2 = $00;
  COLORMAP_TYPE_V1_4 = $01;
  COLORMAP_TYPE_V36  = COLORMAP_TYPE_V1_4; // use this definition
  COLORMAP_TYPE_V39  = $02;

// Flags variable
  COLORMAP_TRANSPARENCY   = $01;
  COLORPLANE_TRANSPARENCY = $02;
  BORDER_BLANKING         = $04;
  BORDER_NOTRANSPARENCY   = $08;
  VIDEOCONTROL_BATCH      = $10;
  USER_COPPER_CLIP        = $20;

  CMF_CMTRANS   =  0;
  CMF_CPTRANS   =  1;
  CMF_BRDRBLNK  =  2;
  CMF_BRDNTRAN  =  3;
  CMF_BRDRSPRT  =  6;

  // All these VPXF_ flags are private
  VPXB_FREE_ME        = 0;
  VPXF_FREE_ME        = 1 shl VPXB_FREE_ME;
  VPXB_LAST           = 1;
  VPXF_LAST           = 1 shl VPXB_LAST;
  VPXB_STRADDLES_256  = 4;
  VPXF_STRADDLES_256  = 1 shl VPXB_STRADDLES_256;
  VPXB_STRADDLES_512  = 5;
  VPXF_STRADDLES_512  = 1 shl VPXB_STRADDLES_512;

  EXTEND_VSTRUCT = $1000;  // unused bit in Modes field of View
// defines used for Modes in IVPargs
  GENLOCK_VIDEO  =  $0002;
  LACE           =  $0004;
  SUPERHIRES     =  $0020;
  PFBA           =  $0040;
  EXTRA_HALFBRITE=  $0080;
  GENLOCK_AUDIO  =  $0100;
  DUALPF         =  $0400;
  HAM            =  $0800;
  EXTENDED_MODE  =  $1000;
  VP_HIDE        =  $2000;
  SPRITES        =  $4000;
  HIRES          =  $8000;

  VPF_A2024      =  $40;
  VPF_AGNUS      =  $20;
  VPF_TENHZ      =  $20;

  BORDERSPRITES   = $40;

  SPRITERESN_ECS       =   0;
  // ^140ns, except in 35ns viewport, where it is 70ns.
  SPRITERESN_140NS     =   1;
  SPRITERESN_70NS      =   2;
  SPRITERESN_35NS      =   3;
  SPRITERESN_DEFAULT   =   -1;

// AuxFlags :
  CMAB_FULLPALETTE = 0;
  CMAF_FULLPALETTE = 1 shl CMAB_FULLPALETTE;
  CMAB_NO_INTERMED_UPDATE = 1;
  CMAF_NO_INTERMED_UPDATE = 1 shl CMAB_NO_INTERMED_UPDATE;
  CMAB_NO_COLOR_LOAD = 2;
  CMAF_NO_COLOR_LOAD = 1 shl CMAB_NO_COLOR_LOAD;
  CMAB_DUALPF_DISABLE = 3;
  CMAF_DUALPF_DISABLE = 1 shl CMAB_DUALPF_DISABLE;

type
  PPaletteExtra = ^TPaletteExtra;
  TPaletteExtra = record            // structure may be extended so watch out!
    pe_Semaphore: TSignalSemaphore; // shared semaphore for arbitration
    pe_FirstFree: Word;             // *private*
    pe_NFree: Word;                 // number of free colors
    pe_FirstShared: Word;           // *private*
    pe_NShared: Word;               // *private*
    pe_RefCnt: PByte;               // *private*
    pe_AllocList: PByte;            // *private*
    pe_ViewPort: PViewPort;         // back Pointer to viewport
    pe_SharableColors: Word;        // the number of sharable colors.
  end;

// flags values for ObtainPen
const
  PENB_EXCLUSIVE   = 0;
  PENB_NO_SETCOLOR = 1;
  PENF_EXCLUSIVE   = 1 shl PENB_EXCLUSIVE;
  PENF_NO_SETCOLOR = 1 shl PENB_NO_SETCOLOR;
// obsolete names for PENF_xxx flags:
  PEN_EXCLUSIVE = PENF_EXCLUSIVE;
  PEN_NO_SETCOLOR = PENF_NO_SETCOLOR;
// precision values for ObtainBestPen:
  PRECISION_EXACT = -1;
  PRECISION_IMAGE = 0;
  PRECISION_ICON  = 16;
  PRECISION_GUI   = 32;
// tags for ObtainBestPen:
  OBP_Precision = $84000000;
  OBP_FailIfBad = $84000001;

// From V39, MakeVPort() will return an error if there is not enough memory,
// or the requested mode cannot be opened with the requested depth with the
// given bitmap (for higher bandwidth alignments).

  MVP_OK        =  0; // you want to see this one
  MVP_NO_MEM    =  1; // insufficient memory for intermediate workspace
  MVP_NO_VPE    =  2; // ViewPort does not have a ViewPortExtra, and insufficient memory to allocate a temporary one.
  MVP_NO_DSPINS =  3; // insufficient memory for intermidiate copper instructions.
  MVP_NO_DISPLAY = 4; // BitMap data is misaligned for this viewport's mode and depth - see AllocBitMap().
  MVP_OFF_BOTTOM = 5; // PRIVATE - you will never see this.

// From V39, MrgCop() will return an error if there is not enough memory,
// or for some reason MrgCop() did not need to make any copper lists.
  MCOP_OK       =  0; // you want to see this one
  MCOP_NO_MEM   =  1; // insufficient memory to allocate the system copper lists.
  MCOP_NOP      =  2; // MrgCop() did not merge any copper lists (eg, no ViewPorts in the list, or all marked as hidden).

type
  PDBufInfo = ^TDBufInfo;
  TDBufInfo = record
    dbi_Link1: APTR;
    dbi_Count1: LongWord;
    dbi_SafeMessage: TMessage; // replied to when safe to write to old bitmap
    dbi_UserData1: Pointer;                     { first user data }

    dbi_Link2: Pointer;
    dbi_Count2: LongWord;
    dbi_DispMessage: tMessage; { replied to when new bitmap has been displayed at least
                                                    once }
    dbi_UserData2: Pointer;                  { second user data }
    dbi_MatchLong: LongWord;
    dbi_CopPtr1,
    dbi_CopPtr2,
    dbi_CopPtr3: Pointer;
    dbi_BeamPos1,
    dbi_BeamPos2: WORD;
  end;

const
  INVALID_ID =   not 0;

{ With all the new modes that are available under V38 and V39, it is highly
  recommended that you use either the asl.library screenmode requester,
  and/or the V39 graphics.library function BestModeIDA().

  DO NOT interpret the any of the bits in the ModeID for its meaning. For
  example, do not interpret bit 3 ($4) as meaning the ModeID is interlaced.
  Instead, use GetDisplayInfoData() with DTAG_DISP, and examine the DIPF_...
  flags to determine a ModeID's characteristics. The only exception to
  this rule is that bit 7 ($80) will always mean the ModeID is
  ExtraHalfBright, and bit 11 ($800) will always mean the ModeID is HAM.}
// normal identifiers
  MONITOR_ID_MASK    = $FFFF1000;
  DEFAULT_MONITOR_ID = $00000000;
  NTSC_MONITOR_ID    = $00011000;
  PAL_MONITOR_ID     = $00021000;

{ the following 22 composite keys are for Modes on the default Monitor.
  NTSC & PAL "flavors" of these particular keys may be made by or'ing
  the NTSC or PAL MONITOR_ID with the desired MODE_KEY...

  For example, to specifically open a PAL HAM interlaced ViewPort
  (or intuition screen), you would use the modeid of
  (PAL_MONITOR_ID OR HAMLACE_KEY)}

  LORES_KEY                     =  $00000000;
  HIRES_KEY                     =  $00008000;
  SUPER_KEY                     =  $00008020;
  HAM_KEY                       =  $00000800;
  LORESLACE_KEY                 =  $00000004;
  HIRESLACE_KEY                 =  $00008004;
  SUPERLACE_KEY                 =  $00008024;
  HAMLACE_KEY                   =  $00000804;
  LORESDPF_KEY                  =  $00000400;
  HIRESDPF_KEY                  =  $00008400;
  SUPERDPF_KEY                  =  $00008420;
  LORESLACEDPF_KEY              =  $00000404;
  HIRESLACEDPF_KEY              =  $00008404;
  SUPERLACEDPF_KEY              =  $00008424;
  LORESDPF2_KEY                 =  $00000440;
  HIRESDPF2_KEY                 =  $00008440;
  SUPERDPF2_KEY                 =  $00008460;
  LORESLACEDPF2_KEY             =  $00000444;
  HIRESLACEDPF2_KEY             =  $00008444;
  SUPERLACEDPF2_KEY             =  $00008464;
  EXTRAHALFBRITE_KEY            =  $00000080;
  EXTRAHALFBRITELACE_KEY        =  $00000084;
  // New for AA ChipSet (V39)
  HIRESHAM_KEY                  =  $00008800;
  SUPERHAM_KEY                  =  $00008820;
  HIRESEHB_KEY                  =  $00008080;
  SUPEREHB_KEY                  =  $000080a0;
  HIRESHAMLACE_KEY              =  $00008804;
  SUPERHAMLACE_KEY              =  $00008824;
  HIRESEHBLACE_KEY              =  $00008084;
  SUPEREHBLACE_KEY              =  $000080a4;
  // Added for V40 - may be useful modes for some games or animations.
  LORESSDBL_KEY                 =  $00000008;
  LORESHAMSDBL_KEY              =  $00000808;
  LORESEHBSDBL_KEY              =  $00000088;
  HIRESHAMSDBL_KEY              =  $00008808;
  // VGA identifiers
  VGA_MONITOR_ID                =  $00031000;
  VGAEXTRALORES_KEY             =  $00031004;
  VGALORES_KEY                  =  $00039004;
  VGAPRODUCT_KEY                =  $00039024;
  VGAHAM_KEY                    =  $00031804;
  VGAEXTRALORESLACE_KEY         =  $00031005;
  VGALORESLACE_KEY              =  $00039005;
  VGAPRODUCTLACE_KEY            =  $00039025;
  VGAHAMLACE_KEY                =  $00031805;
  VGAEXTRALORESDPF_KEY          =  $00031404;
  VGALORESDPF_KEY               =  $00039404;
  VGAPRODUCTDPF_KEY             =  $00039424;
  VGAEXTRALORESLACEDPF_KEY      =  $00031405;
  VGALORESLACEDPF_KEY           =  $00039405;
  VGAPRODUCTLACEDPF_KEY         =  $00039425;
  VGAEXTRALORESDPF2_KEY         =  $00031444;
  VGALORESDPF2_KEY              =  $00039444;
  VGAPRODUCTDPF2_KEY            =  $00039464;
  VGAEXTRALORESLACEDPF2_KEY     =  $00031445;
  VGALORESLACEDPF2_KEY          =  $00039445;
  VGAPRODUCTLACEDPF2_KEY        =  $00039465;
  VGAEXTRAHALFBRITE_KEY         =  $00031084;
  VGAEXTRAHALFBRITELACE_KEY     =  $00031085;
  // New for AA ChipSet (V39)
  VGAPRODUCTHAM_KEY             =  $00039824;
  VGALORESHAM_KEY               =  $00039804;
  VGAEXTRALORESHAM_KEY          =  VGAHAM_KEY;
  VGAPRODUCTHAMLACE_KEY         =  $00039825;
  VGALORESHAMLACE_KEY           =  $00039805;
  VGAEXTRALORESHAMLACE_KEY      =  VGAHAMLACE_KEY;
  VGAEXTRALORESEHB_KEY          =  VGAEXTRAHALFBRITE_KEY;
  VGAEXTRALORESEHBLACE_KEY      =  VGAEXTRAHALFBRITELACE_KEY;
  VGALORESEHB_KEY               =  $00039084;
  VGALORESEHBLACE_KEY           =  $00039085;
  VGAEHB_KEY                    =  $000390a4;
  VGAEHBLACE_KEY                =  $000390a5;
  // These ModeIDs are the scandoubled equivalents of the above, with the
  // exception of the DualPlayfield modes, as AA does not allow for scandoubling dualplayfield.
  VGAEXTRALORESDBL_KEY          =  $00031000;
  VGALORESDBL_KEY               =  $00039000;
  VGAPRODUCTDBL_KEY             =  $00039020;
  VGAEXTRALORESHAMDBL_KEY       =  $00031800;
  VGALORESHAMDBL_KEY            =  $00039800;
  VGAPRODUCTHAMDBL_KEY          =  $00039820;
  VGAEXTRALORESEHBDBL_KEY       =  $00031080;
  VGALORESEHBDBL_KEY            =  $00039080;
  VGAPRODUCTEHBDBL_KEY          =  $000390a0;
  // a2024 identifiers
  A2024_MONITOR_ID              =  $00041000;
  A2024TENHERTZ_KEY             =  $00041000;
  A2024FIFTEENHERTZ_KEY         =  $00049000;
  // prototype identifiers (private)
  PROTO_MONITOR_ID              =  $00051000;
  // These monitors and modes were added for the V38 release.
  EURO72_MONITOR_ID             =  $00061000;
  EURO72EXTRALORES_KEY          =  $00061004;
  EURO72LORES_KEY               =  $00069004;
  EURO72PRODUCT_KEY             =  $00069024;
  EURO72HAM_KEY                 =  $00061804;
  EURO72EXTRALORESLACE_KEY      =  $00061005;
  EURO72LORESLACE_KEY           =  $00069005;
  EURO72PRODUCTLACE_KEY         =  $00069025;
  EURO72HAMLACE_KEY             =  $00061805;
  EURO72EXTRALORESDPF_KEY       =  $00061404;
  EURO72LORESDPF_KEY            =  $00069404;
  EURO72PRODUCTDPF_KEY          =  $00069424;
  EURO72EXTRALORESLACEDPF_KEY   =  $00061405;
  EURO72LORESLACEDPF_KEY        =  $00069405;
  EURO72PRODUCTLACEDPF_KEY      =  $00069425;
  EURO72EXTRALORESDPF2_KEY      =  $00061444;
  EURO72LORESDPF2_KEY           =  $00069444;
  EURO72PRODUCTDPF2_KEY         =  $00069464;
  EURO72EXTRALORESLACEDPF2_KEY  =  $00061445;
  EURO72LORESLACEDPF2_KEY       =  $00069445;
  EURO72PRODUCTLACEDPF2_KEY     =  $00069465;
  EURO72EXTRAHALFBRITE_KEY      =  $00061084;
  EURO72EXTRAHALFBRITELACE_KEY  =  $00061085;
  // New AA modes (V39)
  EURO72PRODUCTHAM_KEY          =  $00069824;
  EURO72PRODUCTHAMLACE_KEY      =  $00069825;
  EURO72LORESHAM_KEY            =  $00069804;
  EURO72LORESHAMLACE_KEY        =  $00069805;
  EURO72EXTRALORESHAM_KEY       =  EURO72HAM_KEY;
  EURO72EXTRALORESHAMLACE_KEY   =  EURO72HAMLACE_KEY ;
  EURO72EXTRALORESEHB_KEY       =  EURO72EXTRAHALFBRITE_KEY;
  EURO72EXTRALORESEHBLACE_KEY   =  EURO72EXTRAHALFBRITELACE_KEY;
  EURO72LORESEHB_KEY            =  $00069084;
  EURO72LORESEHBLACE_KEY        =  $00069085;
  EURO72EHB_KEY                 =  $000690a4;
  EURO72EHBLACE_KEY             =  $000690a5;
  // These ModeIDs are the scandoubled equivalents of the above, with the
  // exception of the DualPlayfield modes, as AA does not allow for scandoubling dualplayfield.
  EURO72EXTRALORESDBL_KEY       =  $00061000;
  EURO72LORESDBL_KEY            =  $00069000;
  EURO72PRODUCTDBL_KEY          =  $00069020;
  EURO72EXTRALORESHAMDBL_KEY    =  $00061800;
  EURO72LORESHAMDBL_KEY         =  $00069800;
  EURO72PRODUCTHAMDBL_KEY       =  $00069820;
  EURO72EXTRALORESEHBDBL_KEY    =  $00061080;
  EURO72LORESEHBDBL_KEY         =  $00069080;
  EURO72PRODUCTEHBDBL_KEY       =  $000690a0;
  EURO36_MONITOR_ID             =  $00071000;
  // Euro36 modeids can be ORed with the default modeids a la NTSC and PAL.
  // For example, Euro36 SuperHires is (EURO36_MONITOR_ID OR SUPER_KEY)
  SUPER72_MONITOR_ID            =  $00081000;
  // Super72 modeids can be ORed with the default modeids a la NTSC and PAL. For example, Super72 SuperHiresLace (80$600) is
  // (SUPER72_MONITOR_ID OR SUPERLACE_KEY). The following scandoubled Modes are the exception:
  SUPER72LORESDBL_KEY           =  $00081008;
  SUPER72HIRESDBL_KEY           =  $00089008;
  SUPER72SUPERDBL_KEY           =  $00089028;
  SUPER72LORESHAMDBL_KEY        =  $00081808;
  SUPER72HIRESHAMDBL_KEY        =  $00089808;
  SUPER72SUPERHAMDBL_KEY        =  $00089828;
  SUPER72LORESEHBDBL_KEY        =  $00081088;
  SUPER72HIRESEHBDBL_KEY        =  $00089088;
  SUPER72SUPEREHBDBL_KEY        =  $000890a8;
  // These monitors and modes were added for the V39 release.
  DBLNTSC_MONITOR_ID            =  $00091000;
  DBLNTSCLORES_KEY              =  $00091000;
  DBLNTSCLORESFF_KEY            =  $00091004;
  DBLNTSCLORESHAM_KEY           =  $00091800;
  DBLNTSCLORESHAMFF_KEY         =  $00091804;
  DBLNTSCLORESEHB_KEY           =  $00091080;
  DBLNTSCLORESEHBFF_KEY         =  $00091084;
  DBLNTSCLORESLACE_KEY          =  $00091005;
  DBLNTSCLORESHAMLACE_KEY       =  $00091805;
  DBLNTSCLORESEHBLACE_KEY       =  $00091085;
  DBLNTSCLORESDPF_KEY           =  $00091400;
  DBLNTSCLORESDPFFF_KEY         =  $00091404;
  DBLNTSCLORESDPFLACE_KEY       =  $00091405;
  DBLNTSCLORESDPF2_KEY          =  $00091440;
  DBLNTSCLORESDPF2FF_KEY        =  $00091444;
  DBLNTSCLORESDPF2LACE_KEY      =  $00091445;
  DBLNTSCHIRES_KEY              =  $00099000;
  DBLNTSCHIRESFF_KEY            =  $00099004;
  DBLNTSCHIRESHAM_KEY           =  $00099800;
  DBLNTSCHIRESHAMFF_KEY         =  $00099804;
  DBLNTSCHIRESLACE_KEY          =  $00099005;
  DBLNTSCHIRESHAMLACE_KEY       =  $00099805;
  DBLNTSCHIRESEHB_KEY           =  $00099080;
  DBLNTSCHIRESEHBFF_KEY         =  $00099084;
  DBLNTSCHIRESEHBLACE_KEY       =  $00099085;
  DBLNTSCHIRESDPF_KEY           =  $00099400;
  DBLNTSCHIRESDPFFF_KEY         =  $00099404;
  DBLNTSCHIRESDPFLACE_KEY       =  $00099405;
  DBLNTSCHIRESDPF2_KEY          =  $00099440;
  DBLNTSCHIRESDPF2FF_KEY        =  $00099444;
  DBLNTSCHIRESDPF2LACE_KEY      =  $00099445;
  DBLNTSCEXTRALORES_KEY         =  $00091200;
  DBLNTSCEXTRALORESHAM_KEY      =  $00091a00;
  DBLNTSCEXTRALORESEHB_KEY      =  $00091280;
  DBLNTSCEXTRALORESDPF_KEY      =  $00091600;
  DBLNTSCEXTRALORESDPF2_KEY     =  $00091640;
  DBLNTSCEXTRALORESFF_KEY       =  $00091204;
  DBLNTSCEXTRALORESHAMFF_KEY    =  $00091a04;
  DBLNTSCEXTRALORESEHBFF_KEY    =  $00091284;
  DBLNTSCEXTRALORESDPFFF_KEY    =  $00091604;
  DBLNTSCEXTRALORESDPF2FF_KEY   =  $00091644;
  DBLNTSCEXTRALORESLACE_KEY     =  $00091205;
  DBLNTSCEXTRALORESHAMLACE_KEY  =  $00091a05;
  DBLNTSCEXTRALORESEHBLACE_KEY  =  $00091285;
  DBLNTSCEXTRALORESDPFLACE_KEY  =  $00091605;
  DBLNTSCEXTRALORESDPF2LACE_KEY =  $00091645;
  DBLPAL_MONITOR_ID             =  $000a1000;
  DBLPALLORES_KEY               =  $000a1000;
  DBLPALLORESFF_KEY             =  $000a1004;
  DBLPALLORESHAM_KEY            =  $000a1800;
  DBLPALLORESHAMFF_KEY          =  $000a1804;
  DBLPALLORESEHB_KEY            =  $000a1080;
  DBLPALLORESEHBFF_KEY          =  $000a1084;
  DBLPALLORESLACE_KEY           =  $000a1005;
  DBLPALLORESHAMLACE_KEY        =  $000a1805;
  DBLPALLORESEHBLACE_KEY        =  $000a1085;
  DBLPALLORESDPF_KEY            =  $000a1400;
  DBLPALLORESDPFFF_KEY          =  $000a1404;
  DBLPALLORESDPFLACE_KEY        =  $000a1405;
  DBLPALLORESDPF2_KEY           =  $000a1440;
  DBLPALLORESDPF2FF_KEY         =  $000a1444;
  DBLPALLORESDPF2LACE_KEY       =  $000a1445;
  DBLPALHIRES_KEY               =  $000a9000;
  DBLPALHIRESFF_KEY             =  $000a9004;
  DBLPALHIRESHAM_KEY            =  $000a9800;
  DBLPALHIRESHAMFF_KEY          =  $000a9804;
  DBLPALHIRESLACE_KEY           =  $000a9005;
  DBLPALHIRESHAMLACE_KEY        =  $000a9805;
  DBLPALHIRESEHB_KEY            =  $000a9080;
  DBLPALHIRESEHBFF_KEY          =  $000a9084;
  DBLPALHIRESEHBLACE_KEY        =  $000a9085;
  DBLPALHIRESDPF_KEY            =  $000a9400;
  DBLPALHIRESDPFFF_KEY          =  $000a9404;
  DBLPALHIRESDPFLACE_KEY        =  $000a9405;
  DBLPALHIRESDPF2_KEY           =  $000a9440;
  DBLPALHIRESDPF2FF_KEY         =  $000a9444;
  DBLPALHIRESDPF2LACE_KEY       =  $000a9445;
  DBLPALEXTRALORES_KEY          =  $000a1200;
  DBLPALEXTRALORESHAM_KEY       =  $000a1a00;
  DBLPALEXTRALORESEHB_KEY       =  $000a1280;
  DBLPALEXTRALORESDPF_KEY       =  $000a1600;
  DBLPALEXTRALORESDPF2_KEY      =  $000a1640;
  DBLPALEXTRALORESFF_KEY        =  $000a1204;
  DBLPALEXTRALORESHAMFF_KEY     =  $000a1a04;
  DBLPALEXTRALORESEHBFF_KEY     =  $000a1284;
  DBLPALEXTRALORESDPFFF_KEY     =  $000a1604;
  DBLPALEXTRALORESDPF2FF_KEY    =  $000a1644;
  DBLPALEXTRALORESLACE_KEY      =  $000a1205;
  DBLPALEXTRALORESHAMLACE_KEY   =  $000a1a05;
  DBLPALEXTRALORESEHBLACE_KEY   =  $000a1285;
  DBLPALEXTRALORESDPFLACE_KEY   =  $000a1605;
  DBLPALEXTRALORESDPF2LACE_KEY  =  $000a1645;

// Use these tags for passing to BestModeID() (V39)
  //SPECIAL_FLAGS = DIPF_IS_DUALPF or DIPF_IS_PF2PRI or DIPF_IS_HAM or DIPF_IS_EXTRAHALFBRITE;

  BIDTAG_DIPFMustHave    = $80000001; // mask of the DIPF_ flags the ModeID must have Default - nil
  BIDTAG_DIPFMustNotHave = $80000002; // mask of the DIPF_ flags the ModeID must not have Default - SPECIAL_FLAGS
  BIDTAG_ViewPort        = $80000003; // ViewPort for which a ModeID is sought. Default - nil
  BIDTAG_NominalWidth    = $80000004; // \ together make the aspect ratio and  |  Default - SourceID NominalDimensionInfo,
  BIDTAG_NominalHeight   = $80000005; // / override the vp->Width/Height.      |  or vp->DWidth/Height, or (640 * 200), in that preferred order.
  BIDTAG_DesiredWidth    = $80000006; // \ Nominal Width and Height of the
  BIDTAG_DesiredHeight   = $80000007; // / returned ModeID. Default - same as Nominal
  BIDTAG_Depth           = $80000008; // ModeID must support this depth. Default - vp->RasInfo->BitMap->Depth or 1
  BIDTAG_MonitorID       = $80000009; // ModeID must use this monitor. Default - use best monitor available
  BIDTAG_SourceID        = $8000000a; // instead of a ViewPort. Default - VPModeID(vp) if BIDTAG_ViewPort is specified, else leave the DIPFMustHave and DIPFMustNotHave values untouched.
  BIDTAG_RedBits         = $8000000b; // \
  BIDTAG_BlueBits        = $8000000c; //  > Match up from the database Default - 4
  BIDTAG_GreenBits       = $8000000d; // /
  BIDTAG_GfxPrivate      = $8000000e; // Private
  BIDTAG_MaxDepth        = $8000000f; // Maximum depth ModeID must have.

const
// bplcon0 defines
  MODE_640    = $8000;
  PLNCNTMSK   = $7;    // how many bit planes?  0 = none, 1->6 = 1->6, 7 = reserved
  PLNCNTSHFT  = 12;    // bits to shift for bplcon0
  PF2PRI      = $40;   // bplcon2 bit
  COLORON     = $0200; // disable color burst
  DBLPF       = $400;
  HOLDNMODIFY = $800;
  INTERLACE   = 4;     // interlace mode for 400

// bplcon1 defines
  PFA_FINE_SCROLL       = $F;
  PFB_FINE_SCROLL_SHIFT = 4;
  PF_FINE_SCROLL_MASK   = $F;

// display window start and stop defines
  DIW_HORIZ_POS       = $7F;  // horizontal start/stop
  DIW_VRTCL_POS       = $1FF; // vertical start/stop
  DIW_VRTCL_POS_SHIFT = $7;

// Data fetch start/stop horizontal position
  DFTCH_MASK  = $FF;

// vposr bits
  VPOSRLOF    = $8000;

// for displayinfo database
// datachunk type identifiers
  DTAG_DISP =   $80000000;
  DTAG_DIMS =   $80001000;
  DTAG_MNTR =   $80002000;
  DTAG_NAME =   $80003000;
  DTAG_VEC  =   $80004000; // internal use only

type
  // the "public" handle to a DisplayInfoRecord
  DisplayInfoHandle = APTR;

  PQueryHeader = ^TQueryHeader;
  TQueryHeader = record
    StructID: LongWord;  // datachunk type identifier
    DisplayID: LongWord; // copy of display record key
    SkipID: LongWord;    // TAG_SKIP -- see tagitems.h
    Length: LongWord;    // length of local data in double-longwords
  end;

  PDisplayInfo = ^TDisplayInfo;
  TDisplayInfo = record
    Header: TQueryHeader;
    NotAvailable: Word;        // if 0 available, else see defines
    PropertyFlags: LongWord;   // Properties of this mode see defines
    Resolution: TPoint;        // ticks-per-pixel X/Y
    PixelSpeed: Word;          // aproximation in nanoseconds
    NumStdSprites: Word;       // number of standard amiga sprites
    PaletteRange: Word;        // OBSOLETE - use Red/Green/Blue bits instead
    SpriteResolution: TPoint;  // std sprite ticks-per-pixel X/Y
    Pad: array[0..3] of Byte;  // used internally
    RedBits: Byte;             // number of Red bits display supports (V39)
    GreenBits: Byte;           // number of Green bits display supports (V39)
    BlueBits: Byte;            // number of Blue bits display supports (V39)
    Pad2: array[0..4] of Byte; // find some use for this.
    RTGBoardNum: Byte;         // RTG board number this mode belongs to (V53)
    PixelFormat: LongWord;     // Pixel format (enPixelFormat) (V54)
    Reserved: Array[0..1] of LongWord; // terminator
  end;

const
// availability
  DI_AVAIL_NOCHIPS        = $0001; // No custom chips
  DI_AVAIL_NOMONITOR      = $0002; // No suitable monitor
  DI_AVAIL_NOTWITHGENLOCK = $0004; // Not allowed with genlock
  DI_AVAIL_INVALID        = $1000; // Mode is invalid (V54)
  DI_AVAIL_MONITOOL       = $2000; // Reserved; do not use (V54)

// mode properties
  DIPF_IS_LACE     =  $00000001;
  DIPF_IS_DUALPF   =  $00000002;
  DIPF_IS_PF2PRI   =  $00000004;
  DIPF_IS_HAM      =  $00000008;
  DIPF_IS_ECS      =  $00000010; // note: ECS modes (SHIRES, VGA, AND PRODUCTIVITY) do not support attached sprites.
  DIPF_IS_AA       =  $00010000; // AA modes - may only be available if machine has correct memory type to support required
                                 // bandwidth - check availability. (V39)
  DIPF_IS_PAL       =  $00000020;
  DIPF_IS_SPRITES   =  $00000040;
  DIPF_IS_GENLOCK   =  $00000080;
  DIPF_IS_WB        =  $00000100;
  DIPF_IS_DRAGGABLE =  $00000200;
  DIPF_IS_PANELLED  =  $00000400;
  DIPF_IS_BEAMSYNC  =  $00000800;
  DIPF_IS_EXTRAHALFBRITE    = $00001000;
  DIPF_IS_SPRITES_ATT       = $00002000; // supports attached sprites (V39)
  DIPF_IS_SPRITES_CHNG_RES  = $00004000; // supports variable sprite resolution (V39)
  DIPF_IS_SPRITES_BORDER    = $00008000; // sprite can be displayed in the border (V39)
  DIPF_IS_SCANDBL           = $00020000; // scan doubled (V39)
  DIPF_IS_SPRITES_CHNG_BASE = $00040000; // can change the sprite base colour (V39)
  DIPF_IS_SPRITES_CHNG_PRI  = $00080000; // can change the sprite priority with respect to  the playfield(s). (V39)

  DIPF_IS_DBUFFER       = $00100000; // can support double buffering (V39)
  DIPF_IS_PROGBEAM      = $00200000; // is a programmed beam-sync mode (V39)
  DIPF_IS_HWCOMPOSITE   = $00400000; // can support hardware compositing (V53)
  DIPF_IS_PREFERREDMODE = $00800000; // preferred/native resolution of monitor (V53)
  DIPF_IS_RTG           = $80000000; // mode is RTG and does not use the native chip set

  DIPF_IS_FOREIGN       = DIPF_IS_RTG; // Old synonym not to be used in any new code.

Type
  PDimensionInfo =^TDimensionInfo;
  TDimensionInfo = record
    Header: TQueryHeader;
    MaxDepth: Word;                    // log2(max number of colors)
    MinRasterWidth: Word;              // minimum width in pixels
    MinRasterHeight: Word;             // minimum height in pixels
    MaxRasterWidth: Word;              // maximum width in pixels
    MaxRasterHeight: Word;             // maximum height in pixels
    Nominal: TRectangle;               // "standard" dimensions
    MaxOScan: TRectangle;              // fixed, hardware dependant
    VideoOScan: TRectangle;            // fixed, hardware dependant
    TxtOScan: TRectangle;              // editable via preferences
    StdOScan: TRectangle;              // editable via preferences
    Pad: array[0..13] of Byte;
    Reserved: array[0..1] of LongWord; // terminator
  end;

  PMonitorInfo = ^TMonitorInfo;
  TMonitorInfo = record
    Header: TQueryHeader;
    Mspc: PMonitorSpec;            // pointer to monitor spec.
    ViewPosition: TPoint;          // editable via preferences
    ViewResolution: TPoint;        // standard monitor ticks-per-pixel
    ViewPositionRange: TRectangle; // fixed, hardware dependent
    TotalRows: Word;               // display height in scanlines
    TotalColorClocks: Word;        // scanline width in 280 ns units
    MinRow: Word;                  // absolute min. active scanline
    Compatibility: SmallInt;       // how this coexists with others
    Pad: Array[0..31] of Byte;
    MouseTicks: TPoint;
    DefaultViewPosition: TPoint;   // original, never changes
    PreferredModeID: LongWord;     // for Preferences
    Reserved: array[0..1] of LongWord; // terminator
  end;

// monitor compatibility

const
  MCOMPAT_MIXED  =  0; // can share display with other MCOMPAT_MIXED
  MCOMPAT_SELF   =  1; // can share only within same monitor
  MCOMPAT_NOBODY = -1; // only one viewport at a time

  DISPLAYNAMELEN = 32;
type
  PNameInfo = ^TNameInfo;
  TNameInfo = record
    Header: tQueryHeader;
    Name: array[0..DISPLAYNAMELEN-1] of Char;
    Reserved: array[0..1] of LongWord;        // terminator
  end;


// *********************************************************************
// The following VecInfo structure is PRIVATE, for our use only
// Touch these, and burn! (V39)

  PVecInfo = ^TVecInfo;
  TVecInfo = record
    Header: TQueryHeader;
    Vec: APTR;
    Data: APTR;
    vi_Type: Word;
    Pad: array[0..2] of Word;
    Reserved: array[0..1] of LongWord;
  end;

const
  VTAG_END_CM            = $00000000;
  VTAG_CHROMAKEY_CLR     = $80000000;
  VTAG_CHROMAKEY_SET     = $80000001;
  VTAG_BITPLANEKEY_CLR   = $80000002;
  VTAG_BITPLANEKEY_SET   = $80000003;
  VTAG_BORDERBLANK_CLR   = $80000004;
  VTAG_BORDERBLANK_SET   = $80000005;
  VTAG_BORDERNOTRANS_CLR = $80000006;
  VTAG_BORDERNOTRANS_SET = $80000007;
  VTAG_CHROMA_PEN_CLR    = $80000008;
  VTAG_CHROMA_PEN_SET    = $80000009;
  VTAG_CHROMA_PLANE_SET  = $8000000A;
  VTAG_ATTACH_CM_SET     = $8000000B;
  VTAG_NEXTBUF_CM        = $8000000C;
  VTAG_BATCH_CM_CLR      = $8000000D;
  VTAG_BATCH_CM_SET      = $8000000E;
  VTAG_NORMAL_DISP_GET   = $8000000F;
  VTAG_NORMAL_DISP_SET   = $80000010;
  VTAG_COERCE_DISP_GET   = $80000011;
  VTAG_COERCE_DISP_SET   = $80000012;
  VTAG_VIEWPORTEXTRA_GET = $80000013;
  VTAG_VIEWPORTEXTRA_SET = $80000014;
  VTAG_CHROMAKEY_GET     = $80000015;
  VTAG_BITPLANEKEY_GET   = $80000016;
  VTAG_BORDERBLANK_GET   = $80000017;
  VTAG_BORDERNOTRANS_GET = $80000018;
  VTAG_CHROMA_PEN_GET    = $80000019;
  VTAG_CHROMA_PLANE_GET  = $8000001A;
  VTAG_ATTACH_CM_GET     = $8000001B;
  VTAG_BATCH_CM_GET      = $8000001C;
  VTAG_BATCH_ITEMS_GET   = $8000001D;
  VTAG_BATCH_ITEMS_SET   = $8000001E;
  VTAG_BATCH_ITEMS_ADD   = $8000001F;
  VTAG_VPMODEID_GET      = $80000020;
  VTAG_VPMODEID_SET      = $80000021;
  VTAG_VPMODEID_CLR      = $80000022;
  VTAG_USERCLIP_GET      = $80000023;
  VTAG_USERCLIP_SET      = $80000024;
  VTAG_USERCLIP_CLR      = $80000025;
  //The following tags are V39 specific. They will be ignored (returing error -3) by earlier versions
  VTAG_PF1_BASE_GET         =  $80000026;
  VTAG_PF2_BASE_GET         =  $80000027;
  VTAG_SPEVEN_BASE_GET      =  $80000028;
  VTAG_SPODD_BASE_GET       =  $80000029;
  VTAG_PF1_BASE_SET         =  $8000002a;
  VTAG_PF2_BASE_SET         =  $8000002b;
  VTAG_SPEVEN_BASE_SET      =  $8000002c;
  VTAG_SPODD_BASE_SET       =  $8000002d;
  VTAG_BORDERSPRITE_GET     =  $8000002e;
  VTAG_BORDERSPRITE_SET     =  $8000002f;
  VTAG_BORDERSPRITE_CLR     =  $80000030;
  VTAG_SPRITERESN_SET       =  $80000031;
  VTAG_SPRITERESN_GET       =  $80000032;
  VTAG_PF1_TO_SPRITEPRI_SET =  $80000033;
  VTAG_PF1_TO_SPRITEPRI_GET =  $80000034;
  VTAG_PF2_TO_SPRITEPRI_SET =  $80000035;
  VTAG_PF2_TO_SPRITEPRI_GET =  $80000036;
  VTAG_IMMEDIATE            =  $80000037;
  VTAG_FULLPALETTE_SET      =  $80000038;
  VTAG_FULLPALETTE_GET      =  $80000039;
  VTAG_FULLPALETTE_CLR      =  $8000003A;
  VTAG_DEFSPRITERESN_SET    =  $8000003B;
  VTAG_DEFSPRITERESN_GET    =  $8000003C;

  {all the following tags follow the new, rational standard for videocontrol tags:
   VC_xxx,state         set the state of attribute 'xxx' to value 'state'
   VC_xxx_QUERY,&var    get the state of attribute 'xxx' and store it into the longword
                        pointed to by &var.
   The following are new for V40:}
  VC_IntermediateCLUpdate       =  $80000080; // default=true. When set graphics will update the intermediate copper
                                              // lists on color changes, etc. When false, it won't, and will be faster.
  VC_IntermediateCLUpdate_Query =  $80000081;
  VC_NoColorPaletteLoad         =  $80000082; // default = false. When set, graphics will only load color 0 for this ViewPort, and so the ViewPort's colors will come
                                              // from the previous ViewPort's. NB - Using this tag and VTAG_FULLPALETTE_SET together is undefined.
  VC_NoColorPaletteLoad_Query   =  $80000083;
  VC_DUALPF_Disable             =  $80000084;
        { default = false. When this flag is set, the dual-pf bit
           in Dual-Playfield screens will be turned off. Even bitplanes
           will still come from the first BitMap and odd bitplanes
           from the second BitMap, and both R[xy]Offsets will be
           considered. This can be used (with appropriate palette
           selection) for cross-fades between differently scrolling
           images.
           When this flag is turned on, colors will be loaded for
           the viewport as if it were a single viewport of depth
           depth1+depth2 }
  VC_DUALPF_Disable_Query       = $80000085;
  VC_DPMSLevel                  = $80000086; //default = DPMSLEVEL_ON Sets the DPMS level for the associated viewport. See enDPMSLevel for values. (V54)

// enDPMSLevel
  DPMSLEVEL_ON      = 0; // Full operation (no power saving)
  DPMSLEVEL_STANDBY = 1; // Optional state of minimal power reduction
  DPMSLEVEL_SUSPEND = 2; // Significant reduction of power consumption
  DPMSLEVEL_OFF     = 3;  // Lowest level of power consumption

  SPRITE_ATTACHED = $80;

type
  PSimpleSprite = ^TSimpleSprite;
  TSimpleSprite = record
    posctldata: PWord;
    height: Word;
    x, y: Word;        // current position
    num: Word;
  end;

  PExtSprite = ^TExtSprite;
  TExtSprite = record
    es_SimpleSprite: TSimpleSprite; // conventional simple sprite structure
    es_wordwidth: Word;             // graphics use only, subject to change
    es_flags: Word;                 // graphics use only, subject to change
  end;

const
// tags for AllocSpriteData()
  SPRITEA_Width          = $81000000;
  SPRITEA_XReplication   = $81000002;
  SPRITEA_YReplication   = $81000004;
  SPRITEA_OutputHeight   = $81000006;
  SPRITEA_Attached       = $81000008;
  SPRITEA_OldDataFormat  = $8100000a; // MUST pass in outputheight if using this tag

// tags for GetExtSprite()
  GSTAG_SPRITE_NUM = $82000020;
  GSTAG_ATTACHED   = $82000022;
  GSTAG_SOFTSPRITE = $82000024;

// tags valid for either GetExtSprite or ChangeExtSprite
  GSTAG_SCANDOUBLED = $83000000; // request "NTSC-Like" height if possible.

type
  PBitScaleArgs = ^TBitScaleArgs;
  TBitScaleArgs = record
    bsa_SrcX, bsa_SrcY,                     // source origin
    bsa_SrcWidth, bsa_SrcHeight,            // source size
    bsa_XSrcFactor, bsa_YSrcFactor,         // scale factor denominators
    bsa_DestX, bsa_DestY,                   // destination origin
    bsa_DestWidth, bsa_DestHeight,          // destination size result
    bsa_XDestFactor, bsa_YDestFactor: Word; // scale factor numerators
    bsa_SrcBitMap,                          // source BitMap
    bsa_DestBitMap: PBitMap;                // destination BitMap
    bsa_Flags: LongWord;                    // reserved.  Must be zero!
    bsa_XDDA, bsa_YDDA: Word;               // reserved
    bsa_Reserved1,                          //    "
    bsa_Reserved2: LongInt;                 //    "
  end;

const
  BSAF_RESERVED1 = 1;
  BSAF_RESERVED2 = 2;
  BSAF_AVERAGE   = 4;
  BSAF_BILINEAR  = 8;

// tag definitions for GetRPAttr, SetRPAttr
const
  RPTAG_Font            =  $80000000; // get/set font
  RPTAG_APen            =  $80000002; // get/set apen
  RPTAG_BPen            =  $80000003; // get/set bpen
  RPTAG_DrMd            =  $80000004; // get/set draw mode
  RPTAG_OutlinePen      =  $80000005; // get/set outline pen
  RPTAG_OPen            =  $80000005; // get/set outline pen, short alias
  RPTAG_WriteMask       =  $80000006; // get/set WriteMask
  RPTAG_MaxPen          =  $80000007; // get/set maxpen
  RPTAG_DrawBounds      =  $80000008; // get only rastport draw bounds. pass @rect
  // V51 extensions
  RPTAG_APenColor       = $80000009; // get/set apen color = $aarrggbb
  RPTAG_BPenColor       = $8000000A; // get/set bpen color = $aarrggbb
  RPTAG_OPenColor       = $8000000B; // get/set open color = $aarrggbb
  RPTAG_OutlinePenColor = $8000000B; // get/set open color = $aarrggbb. alias.
  RPTAG_RemapColorFonts = $8000000C; // get/set
  RPTAG_BitMap          = $8000000D; // get/set bitmap of rastport
  // V54 extensions
  RPTAG_XPos            = $8000000E; // get/set x position
  RPTAG_YPos            = $8000000F; // get/set y position

type
  PBltNode= Pointer; // in Hardware

  PGfxBase = ^TGfxBase;
  TGfxBase = record
    LibNode: TLibrary;
    ActiView: PView;
    copinit: PCopInit; // ptr to copper start up list
    cia: PLongInt;     // for 8520 resource use
    blitter: PLongInt; // for future blitter resource use
    LOFlist: PWord;
    SHFlist: PWord;
    blthd: PBltNode;
    blttl: PBltNode;
    bsblthd: PBltNode;
    bsblttl: PBltNode;
    vbsrv: TInterrupt;
    timsrv: TInterrupt;
    bltsrv: TInterrupt;
    TextFonts: TList;
    DefaultFont: PTextFont;
    Modes: Word;              // copy of current first bplcon0
    VBlank: Shortint;
    Debug: Shortint;
    BeamSync: SmallInt;
    system_bplcon0: SmallInt; // it is ored into each bplcon0 for display
    SpriteReserved: Byte;
    bytereserved: Byte;
    Flags: Word;
    BlitLock: SmallInt;
    BlitNest: SmallInt;

    BlitWaitQ: TList;
    BlitOwner: PTask;
    TOF_WaitQ: TList;
    DisplayFlags: Word;        // NTSC PAL GENLOC etc. flags initialized at power on

    SimpleSprites: Pointer;    // PPSimpleSprite
    MaxDisplayRow: Word;       // hardware stuff, do not use
    MaxDisplayColumn: Word;    // hardware stuff, do not use
    NormalDisplayRows: Word;
    NormalDisplayColumns: Word;
    // the following are for standard non interlace, 1/2 wb width
    NormalDPMX: Word;          // Dots per meter on display
    NormalDPMY: Word;          // Dots per meter on display
    LastChanceMemory: PSignalSemaphore;
    LCMptr: Pointer;
    MicrosPerLine: Word;       // 256 time usec/line
    MinDisplayColumn: Word;
    ChipRevBits0: Byte;
    MemType: Byte;
    crb_reserved: array[0..3] of Byte;
    monitor_id: Word;
    hedley: array[0..7] of LongWord;
    hedley_sprites: array[0..7] of LongWord;  // sprite ptrs for intuition mouse
    hedley_sprites1: array[0..7] of LongWord; // sprite ptrs for intuition mouse
    hedley_count: SmallInt;
    hedley_flags: Word;
    hedley_tmp: smallint;
    hash_table: PLongInt;
    current_tot_rows: Word;
    current_tot_cclks: Word;
    hedley_hint: Byte;
    hedley_hint2: Byte;
    nreserved: array[0..3] of LongWord;
    a2024_sync_raster: PLongInt;
    control_delta_pal: Word;
    control_delta_ntsc: Word;
    current_monitor: PMonitorSpec;
    MonitorList: TList;
    default_monitor: PMonitorSpec;
    MonitorListSemaphore: PSignalSemaphore;
    DisplayInfoDataBase: Pointer;
    TopLine: Word;
    ActiViewCprSemaphore: PSignalSemaphore;
    UtilityBase: Pointer;           // for hook AND tag utilities
    ExecBase: Pointer;              // to link with rom.lib
    bwshifts: PByte;
    StrtFetchMasks: PWord;
    StopFetchMasks: PWord;
    Overrun: PWord;
    RealStops: PSmallInt;
    SpriteWidth: Word;             // current width (in words) of sprites
    SpriteFMode: Word;            // current sprite fmode bits
    SoftSprites: SHortInt;        // bit mask of size change knowledgeable sprites
    arraywidth: Shortint;
    DefaultSpriteWidth: Word;     // what width intuition wants
    SprMoveDisable: Shortint;
    WantChips: Byte;
    BoardMemType: Byte;
    Bugs: Byte;
    gb_LayersBase: Pointer;
    ColorMask: LongWord;
    IVector: APTR;
    IData: APTR;
    SpecialCounter: LongWord;     // special for double buffering
    DBList: APTR;
    MonitorFlags: Word;
    ScanDoubledSprites: Byte;
    BP3Bits: Byte;
    MonitorVBlank: TAnalogSignalInterval;
    natural_monitor: PMonitorSpec;
    ProgData: APTR;
    ExtSprites: Byte;
    ClassicAmiga: Byte;           // Private - dont use
    GfxFlags: Word;
    VBCounter: LongWord;
    HashTableSemaphore: PSignalSemaphore;
    HWEmul: array[0..8] of PLongWord;
    // Added in V46
    DefaultCharSet: LongWord;     // Private - for diskfont.library
    RegionPool: APTR;             // Private
    RegionPoolSemaphore: PSignalSemaphore; // private
    // Added in V47
    DiskfontBase: PLibrary;       // Private - for diskfont.library
    // Added in V48
    Antialiasing: LongWord;       // Private - for diskfont.library
    // There are more fields beyond here and they are all private.
  end;

const
  // Values for GfxBase^.DisplayFlags
  NTSC       = 1;
  GENLOC     = 2;
  PAL        = 4;
  TODA_SAFE  = 8;
  REALLY_PAL = 16; // what is actual crystal frequency (as opposed to what bootmenu set the agnus to)? (V39)

  LPEN_SWAP_FRAMES = 32;  // for LightpenSoftware (V40)

  // bits for dalestuff, which may go away when blitter becomes a resource
  QBOWNERn       = 1; // blitter owned by blit queuer
  BLITMSG_FAULTn = 2;
  QBOWNER        = 1 shl QBOWNERn;
  BLITMSG_FAULT  = 1 shl BLITMSG_FAULTn;

// bits defs for ChipRevBits
  GFXB_BIG_BLITS = 0;
  GFXB_HR_AGNUS  = 0;
  GFXB_HR_DENISE = 1;
  GFXB_AA_ALICE  = 2;
  GFXB_AA_LISA   = 3;
  GFXB_AA_MLISA  = 4; // internal use only.

  GFXF_BIG_BLITS = 1;
  GFXF_HR_AGNUS  = 1;
  GFXF_HR_DENISE = 2;
  GFXF_AA_ALICE  = 4;
  GFXF_AA_LISA   = 8;
  GFXF_AA_MLISA  = 16; // internal use only

// Pass ONE of these to SetChipRev()
   SETCHIPREV_A   = GFXF_HR_AGNUS;
   SETCHIPREV_ECS = GFXF_HR_AGNUS or GFXF_HR_DENISE;
   SETCHIPREV_AA  = GFXF_AA_ALICE or GFXF_AA_LISA or SETCHIPREV_ECS;
   SETCHIPREV_BEST= $ffffffff;

// memory type
   BUS_16          = 0;
   NML_CAS         = 0;
   BUS_32          = 1;
   DBL_CAS         = 2;
   BANDWIDTH_1X    = BUS_16 or NML_CAS;
   BANDWIDTH_2XNML = BUS_32;
   BANDWIDTH_2XDBL = DBL_CAS;
   BANDWIDTH_4X    = BUS_32 or DBL_CAS;

// Values for GfxBase->GfxFlags
   GFXF_NEW_DATABASE  = $0001; // New display database
   GFXF_HAVECOMPOSITE = $0002; // CompositeTagList() is available

   GRAPHICSNAME: PChar  = 'graphics.library';

var
  GfxBase: PLibrary = nil;
  IGfx: PInterface = nil;

function GfxObtain(): LongWord; syscall IGfx 60;
function GfxRelease(): LongWord; syscall IGfx 64;
procedure GfxExpunge(); syscall IGfx 68;
function GfxClone(): PInterface; syscall IGfx 72;
function BltBitMap(const SrcBitMap: PBitMap; XSrc, YSrc: LongInt; DestBitMap: PBitMap; XDest, YDest, XSize, YSize: LongInt; MinTerm, Mask: LongWord; TempA: PChar): LongInt; syscall IGfx 76;
procedure BltTemplate(const Source: TPlanePtr; XSrc, SrcMod: LongInt; DestRP: PRastPort; XDest, YDest, XSize, YSize: LongInt); syscall IGfx 80;
procedure ClearEOL(Rp: PRastPort); syscall IGfx 84;
procedure ClearScreen(Rp: PRastPort); syscall IGfx 88;
function TextLength(Rp: PRastPort; const String_: PChar; Count: LongWord): LongInt; syscall IGfx 92;
procedure GfxText(Rp: PRastPort; const String_: PChar; Count: LongWord); syscall IGfx 96;
procedure SetFont(Rp: PRastPort; const TextFont: PTextFont); syscall IGfx 100;
function OpenFont(TextAttr: PTextAttr): PTextFont; syscall IGfx 104;
procedure CloseFont(TextFont: PTextFont); syscall IGfx 108;
function AskSoftStyle(Rp: PRastPort): LongWord; syscall IGfx 112;
function SetSoftStyle(Rp: PRastPort; Style: LongWord; Enable: LongWord): LongWord; syscall IGfx 116;
procedure AddBob(Bob: PBob; Rp: PRastPort); syscall IGfx 120;
procedure AddVSprite(VSprite: PVSprite; Rp: PRastPort); syscall IGfx 124;
procedure DoCollision(Rp: PRastPort); syscall IGfx 128;
procedure DrawGList(Rp: PRastPort; Vp: PViewPort); syscall IGfx 132;
procedure InitGels(Head: PVSprite; Tail: PVSprite; GelsInfo: PGelsInfo); syscall IGfx 136;
procedure InitMasks(VSprite: PVSprite); syscall IGfx 140;
procedure RemIBob(Bob: PBob; Rp: PRastPort; Vp: PViewPort); syscall IGfx 144;
procedure RemVSprite(VSprite: PVSprite); syscall IGfx 148;
procedure SetCollision(Num: LongWord; Routine: Pointer; GelsInfo: PGelsInfo); syscall IGfx 152;
procedure SortGList(Rp: PRastPort); syscall IGfx 156;
procedure AddAnimOb(AnOb: PAnimOb; AnKey: PPAnimOb; Rp: PRastPort); syscall IGfx 160;
procedure Animate(AnKey: PPAnimOb; Rp: PRastPort); syscall IGfx 164;
function GetGBuffers(AnOb: PAnimOb; Rp: PRastPort; Flag: LongInt): LongBool; syscall IGfx 168;
procedure InitGMasks(AnOb: PAnimOb); syscall IGfx 172;
procedure DrawEllipse(Rp: PRastPort; XCenter, YCenter, A, B: LongInt); syscall IGfx 176;
function AreaEllipse(Rp: PRastPort; XCenter, YCenter, A, B: LongInt): LongInt; syscall IGfx 180;
procedure LoadRGB4(Vp: PViewPort; const Colors: PWord; Count: LongWord); syscall IGfx 184;
procedure InitRastPort(Rp: PRastPort); syscall IGfx 188;
procedure InitVPort(Vp: PViewPort); syscall IGfx 192;
function MrgCop(View: PView): LongWord; syscall IGfx 196;
function MakeVPort(View: PView; Vp: PViewPort): LongWord; syscall IGfx 200;
procedure LoadView(View: PView); syscall IGfx 204;
procedure WaitBlit; syscall IGfx 208;
procedure SetRast(Rp: PRastPort; Pen: LongWord); syscall IGfx 212;
procedure GfxMove(Rp: PRastPort; X, Y: LongInt); syscall IGfx 216;
procedure Draw(Rp: PRastPort; X, Y: LongInt); syscall IGfx 220;
function AreaMove(Rp: PRastPort; X, Y: LongInt): LongInt; syscall IGfx 224;
function AreaDraw(Rp: PRastPort; X, Y: LongInt): LongInt; syscall IGfx 228;
function AreaEnd(Rp: PRastPort): LongInt; syscall IGfx 232;
procedure WaitTOF; syscall IGfx 236;
procedure QBlit(blit: PBltNode); syscall IGfx 240;
procedure InitArea(AreaInfo: PAreaInfo; VectorBuffer: APTR; MaxVectors: LongInt); syscall IGfx 244;
procedure SetRGB4(Vp: PViewPort; ColIndex: LongWord; Red, Green, Blue: LongWord); syscall IGfx 248;
procedure QBSBlit(Blit: PBltNode); syscall IGfx 252;
procedure BltClear(MemBlock: TPlanePtr; ByteCount, Flags: LongWord); syscall IGfx 256;
procedure RectFill(Rp: PRastPort; XMin, YMin, XMax, YMax: LongInt); syscall IGfx 260;
procedure BltPattern(Rp: PRastPort; const Mask: TPlanePtr; XMin, YMin, XMax, YMax: LongInt; MaskBPR: LongWord); syscall IGfx 264;
function ReadPixel(Rp: PRastPort; X, Y: LongInt): LongInt; syscall IGfx 268;
function WritePixel(Rp: PRastPort; X, Y: LongInt): LongInt; syscall IGfx 272;
function Flood(Rp: PRastPort; Mode: LongWord; X, Y: LongInt): LongBool; syscall IGfx 276;
procedure PolyDraw(Rp: PRastPort; Count: LongInt; const PolyTable: PSmallInt); syscall IGfx 280;
procedure SetAPen(Rp: PRastPort; Pen: LongWord); syscall IGfx 284;
procedure SetBPen(Rp: PRastPort; Pen: LongWord); syscall IGfx 288;
procedure SetDrMd(Rp: PRastPort; DrawMode: LongWord); syscall IGfx 292;
procedure InitView(View: PView); syscall IGfx 296;
procedure CBump(CopList: PUCopList); syscall IGfx 300;
procedure CMove(CopList: PUCopList; DestOffset: LongInt; data: LongInt); syscall IGfx 304;
procedure CWait(CopList: PUCopList; V, H: LongInt); syscall IGfx 308;
function VBeamPos: LongInt; syscall IGfx 312;
procedure InitBitMap(BitMap: PBitMap; Depth: LongInt; Width, Height: LongWord); syscall IGfx 316;
procedure ScrollRaster(Rp: PRastPort; Dx, Dy, XMin, YMin, XMax, YMax: LongInt); syscall IGfx 320;
procedure WaitBOVP(Vp: PViewPort); syscall IGfx 324;
function GetSprite(Sprite: PSimpleSprite; Num: LongInt): LongInt; syscall IGfx 328;
procedure FreeSprite(Num: LongInt); syscall IGfx 332;
procedure ChangeSprite(Vp: PViewPort; Sprite: PSimpleSprite; NewData: APTR); syscall IGfx 336;
procedure MoveSprite(Vp: PViewPort; Sprite: PSimpleSprite; X, Y: LongInt); syscall IGfx 340;
procedure LockLayerRom(Layer: PLayer); syscall IGfx 344;
procedure UnlockLayerRom(Layer: PLayer); syscall IGfx 348;
procedure SyncSBitMap(Layer: PLayer); syscall IGfx 352;
procedure CopySBitMap(Layer: PLayer); syscall IGfx 356;
procedure OwnBlitter; syscall IGfx 360;
procedure DisownBlitter; syscall IGfx 364;
function InitTmpRas(TmpRas: PTmpRas; Buffer: TPlanePtr; Size: LongInt): PTmpRas; syscall IGfx 368;
procedure AskFont(Rp: PRastPort; TextAttr: PTextAttr); syscall IGfx 372;
procedure AddFont(TextFont: PTextFont); syscall IGfx 376;
procedure RemFont(TextFont: PTextFont); syscall IGfx 380;
function AllocRaster(Width, Height: LongWord): TPlanePtr; syscall IGfx 384;
procedure FreeRaster(P: TPlanePtr; Width, Height: LongWord); syscall IGfx 388;
procedure AndRectRegion(Region: PRegion; const Rectangle: PRectangle); syscall IGfx 392;
function OrRectRegion(Region: PRegion; const Rectangle: PRectangle): LongBool; syscall IGfx 396;
function NewRegion: PRegion; syscall IGfx 400;
function ClearRectRegion(Region: PRegion; const Rectangle: PRectangle): LongBool; syscall IGfx 404;
procedure ClearRegion(Region: PRegion); syscall IGfx 408;
procedure DisposeRegion(Region: PRegion); syscall IGfx 412;
procedure FreeVPortCopLists(Vp: PViewPort); syscall IGfx 416;
procedure FreeCopList(CopList: PCopList); syscall IGfx 420;
procedure ClipBlit(SrcRP: PRastPort; XSrc, ySrc: LongInt; DestRP: PRastPort; XDest, YDest, XSize, YSize: LongWord; MinTerm: LongWord); syscall IGfx 424;
function XorRectRegion(Region: PRegion; const Rectangle: PRectangle): LongBool; syscall IGfx 428;
procedure FreeCprList(CprList: PCprList); syscall IGfx 432;
function GetColorMap(Entries: LongWord): PColorMap; syscall IGfx 436;
procedure FreeColorMap(ColorMap: PColorMap); syscall IGfx 440;
function GetRGB4(ColorMap: PColorMap; Entry: LongWord): LongInt; syscall IGfx 444;
procedure ScrollVPort(Vp: PViewPort); syscall IGfx 448;
function UCopperListInit(UCopList: PUCopList; N: LongInt): PCopList; syscall IGfx 452;
procedure FreeGBuffers(AnOb: PAnimOb; Rp: PRastPort; Flag: LongInt); syscall IGfx 456;
function BltBitMaPRastPort(const SrcBitMap: PBitMap; XSrc, YSrc: LongInt; DestRP: PRastPort; XDest, YDest, XSize, YSize: LongInt; Minterm: LongWord): LongBool; syscall IGfx 460;
function OrRegionRegion(const SrcRegion: PRegion; DestRegion: PRegion): LongBool; syscall IGfx 464;
function XorRegionRegion(const SrcRegion: PRegion; DestRegion: PRegion): LongBool; syscall IGfx 468;
function AndRegionRegion(const SrcRegion: PRegion; DestRegion: PRegion): LongBool; syscall IGfx 472;
procedure SetRGB4CM(ColorMap: PColorMap; ColIndex: LongWord; Red, Green, Blue: LongWord); syscall IGfx 476;
procedure BltMaskBitMapRastPort(const SrcBitMap: PBitMap; XSrc, YSrc: LongInt; DestRP: PRastPort; XDest, YDest, XSize, YSize: LongInt; MinTerm: LongWord; const BltMask: TPlanePtr); syscall IGfx 480;
// 484 private
// 488 private
function AttemptLockLayerRom(Layer: PLayer): LongBool; syscall IGfx 492;
function GfxNew(GfxNodeType: LongWord): APTR; syscall IGfx 496;
procedure GfxFree(GfxNodePtr: PExtendedNode); syscall IGfx 500;
procedure GfxAssociate(const AssociateNode: APTR; GfxNodePtr: PExtendedNode); syscall IGfx 504;
procedure BitMapScale(BitScaleArgs: PBitScaleArgs); syscall IGfx 508;
function ScalerDiv(Factor, Numerator, Denominator: LongWord): LongWord; syscall IGfx 512;
procedure TextExtent(Rp: PRastPort; const String_: STRPTR; Count: LongWord; TextExtent: PTextExtent); syscall IGfx 516;
function TextFit(Rp: PRastPort; const String_: STRPTR; StrLen: LongWord; TextExtent: PTextExtent; ConstrainingExtent: PTextExtent; StrDirection: LongInt; ConstrainingBitWidth, ConstrainingBitHeight: LongWord): LongWord; syscall IGfx 520;
function GfxLookUp(const AssociateNode: APTR): APTR; syscall IGfx 524;
function VideoControl(ColorMap: PColorMap; TagArray: PTagItem): LongWord; syscall IGfx 528;
// 532 VideoControlTags
function OpenMonitor(const MonitorName: STRPTR; DisplayID: LongWord): PMonitorSpec; syscall IGfx 536;
function CloseMonitor(MonitorSpec: PMonitorSpec): LongBool; syscall IGfx 540;
function FindDisplayInfo(DisplayID: LongWord): Pointer; syscall IGfx 544;
function NextDisplayInfo(DisplayID: LongWord): LongWord; syscall IGfx 548;
// 552 private
// 556 private
// 560 private
function GetDisplayInfoData(const Handle: Pointer; Buf: APTR; Size: LongWord; TagID: LongWord; DisplayID: LongWord): LongWord; syscall IGfx 564;
procedure FontExtent(const Font: PTextFont; FontExtent: PTextExtent); syscall IGfx 568;
function ReadPixelLine8(Rp: PRastPort; XStart, YStart, Width: LongWord; Array_: PByte; TempRP: PRastPort): LongInt; syscall IGfx 572;
function WritePixelLine8(Rp: PRastPort; XStart, YStart, Width: LongWord; Array_: PByte; TempRP: PRastPort): LongInt; syscall IGfx 576;
function ReadPixelArray8(Rp: PRastPort; XStart, YStart, XStop, YStop: LongWord; Array_: PByte; TempRP: PRastPort): LongInt; syscall IGfx 580;
function WritePixelArray8(Rp: PRastPort; XStart, YStart, XStop, YStop: LongWord; Array_: PByte; TempRP: PRastPort): LongInt; syscall IGfx 584;
function GetVPModeID(const Vp: PViewPort): LongWord; syscall IGfx 588;
function ModeNotAvailable(ModeID: LongWord): LongWord; syscall IGfx 592;
// 596 private
// 600 private
procedure EraseRect(Rp: PRastPort; XMin, YMin, XMax, YMax: LongInt); syscall IGfx 604;
function ExtendFont(Font: PTextFont; const FontTags: PTagItem): LongWord; syscall IGfx 608;
// 612 ExtendFontTags
procedure StripFont(Font: PTextFont); syscall IGfx 616;
function CalcIVG(V: PView; Cp: PViewPort): LongWord; syscall IGfx 620;
function AttachPalExtra(Cm: PColorMap; Vp: PViewPort): LongInt; syscall IGfx 624;
function ObtainBestPenA(Cm: PColorMap; R, G, B: LongWord;const Tags: PTagItem): LongInt; syscall IGfx 628;
// 632 ObtainBestPen
// 636 private
procedure SetRGB32(Vp: PViewPort; N, R, G, B: LongWord); syscall IGfx 640;
function GetAPen(Rp: PRastPort): LongWord; syscall IGfx 644;
function GetBPen(Rp: PRastPort): LongWord; syscall IGfx 648;
function GetDrMd(Rp: PRastPort): LongWord; syscall IGfx 652;
function GetOutlinePen(Rp: PRastPort): LongWord; syscall IGfx 656;
procedure LoadRGB32(Vp: PViewPort; const Table: PLongWord); syscall IGfx 660;
function SetChipRev(Want: LongWord): LongWord; syscall IGfx 664;
procedure SetABPenDrMd(Rp: PRastPort; APen, BPen, DrawMode: LongWord); syscall IGfx 668;
procedure GetRGB32(const Cm: pColorMap; Firstcolor, NColors: LongWord; Table: PLongWord); syscall IGfx 672;
function BltBitMapTagList(const Tags: PTagItem): LongInt; syscall IGfx 676;
// 680 private
function AllocBitMap(SizeX, SizeY, Depth, Flags: LongWord; const FriendBitmap: PBitMap): PBitMap; syscall IGfx 684;
procedure FreeBitMap(Bm: PBitMap); syscall IGfx 688;
function GetExtSpriteA(Ss: PExtSprite; const Tags: PTagItem): LongInt; syscall IGfx 692;
// 696 GetExtSprite
function CoerceMode(Vp: PViewPort; MonitorID, Flags: LongWord): LongWord; syscall IGfx 700;
procedure ChangeVPBitMap(Vp: PViewPort; Bm: PBitMap; Db: PDBufInfo); syscall IGfx 704;
procedure ReleasePen(Cm: PColorMap; N: LongWord); syscall IGfx 708;
function ObtainPen(Cm: PColorMap; N, R, G, B: LongWord; F: LongInt): LongInt; syscall IGfx 712;
function GetBitMapAttr(const Bm: PBitMap; AttrNum: LongWord): LongWord; syscall IGfx 716;
function AllocDBufInfo(Vp: PViewPort): PDBufInfo; syscall IGfx 720;
procedure FreeDBufInfo(Dbi: PDBufInfo); syscall IGfx 724;
function SetOutlinePen(Rp: PRastPort; Pen: LongWord): LongWord; syscall IGfx 728;
function SetWriteMask(Rp: PRastPort; Msk: LongWord): LongWord; syscall IGfx 732;
procedure SetMaxPen(Rp: PRastPort; MaxPen: LongWord); syscall IGfx 736;
procedure SetRGB32CM(Cm: pColorMap; N, R, G, B: LongWord); syscall IGfx 740;
procedure ScrollRasterBF(Rp: PRastPort; Dx, Dy, XMin, YMin, XMax, yMax: LongWord); syscall IGfx 744;
function FindColor(Cm: pColorMap; R, G, B: LongWord; MaxColor: LongInt): LongWord; syscall IGfx 748;
// 752 BltBitMapTags
function AllocSpriteDataA(const Bm: PBitMap; const Tags: PTagItem): PExtSprite; syscall IGfx 756;
// 758 AllocSpriteData
function ChangeExtSpriteA(Vp: PViewPort; OldSprite: PExtSprite; NewSprite: PExtSprite; const Tags: PTagItem): LongInt; syscall IGfx 764;
// 768 ChangeExtSprite
procedure FreeSpriteData(Sp: PExtSprite); syscall IGfx 772;
function SetRPAttrsA(Rp: PRastPort; const Tags: PTagItem): LongWord; syscall IGfx 776;
// 780 SetRPAttrs
function GetRPAttrsA(const Rp: PRastPort; const Tags: PTagItem): LongWord; syscall IGfx 784;
// 788 GetRPAttrs
function BestModeIDA(const Tags: pTagItem): LongWord; syscall IGfx 1050;
// 796 BestModeID
procedure WriteChunkyPixels(Rp: PRastPort; XStart, YStart, XStop, YStop: LongWord; Array_: PByte; BytesPerRow: LongInt); syscall IGfx 800;
function CompositeTagList(const Operator_: LongWord; const Source: PBitMap; const Destination: PBitMap; const Tags: PTagItem): LongWord; syscall IGfx 804;
// 808 CompositeTags
function AllocBitMapTagList(SizeX, SizeY, Depth: LongWord; const Tags: PTagItem): PBitMap; syscall IGfx 812;
// 812 AllocBitMapTags
function GetMonitorDataTagList(BoardNum, MonitorNum: LongWord; Tags: PTagItem): LongInt; syscall IGfx 820;
// 824 GetMonitorDataTags
function GetBoardDataTagList(BoardNum: LongWord; const Tags: PTagItem): LongInt; syscall IGfx 828;
// 832 GetBoardDataTags
function LockBitMapTagList(BitMap: PBitMap; Tags: PTagItem): APTR; syscall IGfx 836;
// 840 LockBitMapTags
procedure UnlockBitMap(Lock: APTR); syscall IGfx 844;
procedure RectFillColor(Rp: PRastPort; XMin, YMin, XMax, YMax, Color: LongWord); syscall IGfx 848;
function WritePixelColor(Rp: PRastPort; X, Y, Color: LongWord): LongInt; syscall IGfx 852;
function ReadPixelColor(Rp: PRastPort; X, Y: LongWord): LongWord; syscall IGfx 856;
procedure ReadPixelArray(Src: PRastPort; SrcX, SrcY: LongWord; Dst: PByte; DstX, DstY, DstBytesPerRow: LongWord; DstPixelFormat: LongWord; SizeX, SizeY: LongWord); syscall IGfx 860;
procedure WritePixelArray(Src: PByte; SrcX, SrcY, SrcBytesPerRow: LongWord; SrcPixelFormat: LongWord; Dst: PRastPort; DstX, DstY, SizeX, SizeY: LongWord); syscall IGfx 864;
function GraphicsControlTagList(const Tags: PTagItem): LongWord; syscall IGfx 868;
// 872 GraphicsControlTags
function LockBitMapToBoardTagList(BitMap: PBitMap; BoardNum: LongWord; Tags: PTagItem): APTR; syscall IGfx 876;
// 880 LockBitMapToBoardTags
procedure UnlockBitMapFromBoard(Lock: APTR; Modified: LongInt); syscall IGfx 884;
function GetBoardBytesPerRow(BoardNum: LongWord; PixelFormat: LongWord; Width: LongWord): LongWord; syscall IGfx 888;
// 892 private
// 896 private
// 900 private

function AllocSpriteData(bm: PBitMap; const argv: array of PtrUInt): pExtSprite;
function BestModeID(const argv: array of PtrUInt): LongWord;
function ChangeExtSprite(vp: PViewPort; oldsprite: PExtSprite; newsprite: PExtSprite; const argv: array of PtrUInt): LongInt;
function ExtendFontTags(font: PTextFont; const argv: array of PtrUInt): LongWord;
function GetExtSprite(ss: pExtSprite; const argv: array of PtrUInt): LongInt;
procedure GetRPAttrs(rp: PRastPort; const argv: array of PtrUInt);
procedure SetRPAttrs(rp: PRastPort; const argv: array of PtrUInt);
function VideoControlTags(colorMap: PColorMap; const argv: array of PtrUInt): LongWord;

// gfxmacros

procedure BNDRYOFF(w: PRastPort);
procedure InitAnimate(animkey: PPAnimOb);
procedure SetAfPt(w: PRastPort; p: Pointer; n: Byte);
procedure SetDrPt(w: PRastPort; p: Word);
procedure SetOPen(w: PRastPort; c: Byte);
procedure SetWrMsk(w: PRastPort; m: Byte);

procedure SafeSetOutlinePen(w: PRastPort; c: Byte);
procedure SafeSetWriteMask(w: PRastPort; m: SmallInt) ;

{procedure OFF_DISPLAY (cust: pCustom);
procedure ON_DISPLAY (cust: pCustom);
procedure OFF_SPRITE (cust: pCustom);
procedure ON_SPRITE (cust: pCustom);
procedure OFF_VBLANK (cust: pCustom);
procedure ON_VBLANK (cust: pCustom);}

procedure DrawCircle(Rp: PRastPort; xCenter, yCenter, r: LongInt); inline;
function AreaCircle(Rp: PRastPort; xCenter, yCenter, r: SmallInt): LongWord; inline;

function RasSize(w, h: Word): Integer;

implementation

function AllocSpriteData(bm: PBitMap; const argv: array of PtrUInt): PExtSprite;
begin
  AllocSpriteData := AllocSpriteDataA(bm, @argv);
end;

function BestModeID(const argv: array of PtrUInt): LongWord;
begin
    BestModeID := BestModeIDA(@argv);
end;

function ChangeExtSprite(vp: PViewPort; oldsprite: pExtSprite; newsprite: pExtSprite; const argv: array of PtrUInt): LongInt;
begin
    ChangeExtSprite := ChangeExtSpriteA(vp, oldsprite, newsprite, @argv);
end;

function ExtendFontTags(font: PTextFont; const argv: array of PtrUInt): LongWord;
begin
    ExtendFontTags := ExtendFont(font, @argv);
end;

function GetExtSprite(ss: pExtSprite; const argv: array of PtrUInt): LongInt;
begin
    GetExtSprite := GetExtSpriteA(ss, @argv);
end;

procedure GetRPAttrs(rp: PRastPort; const argv: array of PtrUInt);
begin
    GetRPAttrsA(rp, @argv);
end;

function ObtainBestPen(cm: pColorMap; r: LongWord; g: LongWord; b: LongWord; const argv: array of PtrUInt): LongInt;
begin
    ObtainBestPen := ObtainBestPenA(cm, r, g, b, @argv);
end;

procedure SetRPAttrs(rp: PRastPort; const argv: array of PtrUInt);
begin
    SetRPAttrsA(rp, @argv);
end;

function VideoControlTags(colorMap: pColorMap; Const argv: array of PtrUInt): LongWord;
begin
    VideoControlTags := VideoControl(colorMap, @argv);
end;

procedure BNDRYOFF(w: PRastPort);
begin
  with w^ do
    Flags := Flags and (not AREAOUTLINE);
end;

procedure InitAnimate(animkey: PPAnimOb);
begin
  animkey^ := nil;
end;

procedure SetAfPt(w: PRastPort;p: Pointer; n: Byte);
begin
  with w^ do
  begin
    AreaPtrn := p;
    AreaPtSz := n;
  end;
end;

procedure SetDrPt(w: PRastPort; p: Word);
begin
  with w^ do
  begin
    LinePtrn  := p;
    Flags     := Flags or FRST_DOT;
    linpatcnt := 15;
  end;
end;

procedure SetOPen(w: PRastPort; c: Byte);
begin
  WITH w^ DO
  begin
    AOlPen := c;
    Flags  := Flags or AREAOUTLINE;
  end;
end;

procedure SetWrMsk(w: PRastPort; m: Byte);
begin
  w^.Mask := m;
end;

procedure SafeSetOutlinePen(w: PRastPort; c: byte);
begin
  if pGfxBase(GfxBase)^.LibNode.Lib_Version < 39 then
  begin
    w^.AOlPen := c;
    w^.Flags := w^.Flags or AREAOUTLINE;
  end
  else
    c := SetOutlinePen(w,c);
end;

procedure SafeSetWriteMask(w: PRastPort; m: SmallInt) ;
begin
  if PGfxBase(GfxBase)^.LibNode.Lib_Version < 39 then
    w^.Mask := Byte(m)
  else
    SetWriteMask(w, m);
end;
{
procedure OFF_DISPLAY (cust: pCustom);
begin
    cust^.dmacon := BITCLR OR DMAF_RASTER;
end;

procedure ON_DISPLAY (cust: pCustom);
begin
    cust^.dmacon := BITSET OR DMAF_RASTER;
end;

procedure OFF_SPRITE (cust: pCustom);
begin
    cust^.dmacon := BITCLR OR DMAF_SPRITE;
end;

procedure ON_SPRITE (cust: pCustom);
begin
    cust^.dmacon := BITSET OR DMAF_SPRITE;
end;

procedure OFF_VBLANK (cust: pCustom);
begin
    cust^.intena := BITCLR OR INTF_VERTB;
end;

procedure ON_VBLANK (cust: pCustom);
begin
    cust^.intena := BITSET OR INTF_VERTB;
end;}

function RasSize(w, h: Word): Integer; inline;
begin
  RasSize := h * (((w + 15) shr 3) and $FFFE);
end;

procedure DrawCircle(Rp: PRastPort; xCenter, yCenter, r: LongInt); inline;
begin
  DrawEllipse(Rp, xCenter, yCenter, r, r);
end;

function AreaCircle(Rp: PRastPort; xCenter, yCenter, r: SmallInt): LongWord; inline;
begin
  AreaCircle := AreaEllipse(Rp, xCenter, yCenter, r, r);
end;

const
  // Change VERSION and LIBVERSION to proper values
  VERSION: string[2] = '0';
  LIBVERSION: LongWord = 0;

initialization
  GfxBase := OpenLibrary(GRAPHICSNAME,LIBVERSION);
  if Assigned(GfxBase) then
    IGfx := GetInterface(GfxBase, 'main', 1, nil);
finalization
  if Assigned(IGfx) then
    DropInterface(IGfx);
  if Assigned(GfxBase) then
    CloseLibrary(GfxBase);
end.






