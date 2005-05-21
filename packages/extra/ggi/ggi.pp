{******************************************************************************

   Free Pascal conversion (c) 1999 Sebastian Guenther

   LibGGI API interface

   Copyright (C) 1997 Jason McMullan            [jmcc@ggi-project.org]
   Copyright (C) 1997 Steffen Seeger            [seeger@ggi-project.org]
   Copyright (C) 1998 Andrew Apted              [andrew@ggi-project.org]
   Copyright (C) 1998 Andreas Beck              [becka@ggi-project.org]
   Copyright (C) 1998-1999 Marcus Sundberg      [marcus@ggi-project.org]

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
   THE AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

******************************************************************************
*}


{$MODE objfpc}
{$PACKRECORDS C}
{$LINKLIB c}

unit GGI;

interface

uses GII;

const

  libggi = 'ggi';


{******************************************************************************
 LibGGI datatypes and structures
 ******************************************************************************}

  GGI_AUTO      = 0;

type

  TGGICoord = record
    x, y: SmallInt;
  end;

  TGGIPixel = LongWord;

  TGGIAttr = LongWord;

const

  ATTR_FGCOLOR          = $0000FF00;    // fgcolor clut index
  ATTR_BGCOLOR          = $000000FF;    // bgcolor clut index
  ATTR_NORMAL           = $00000000;    // normal style
  ATTR_HALF             = $00010000;    // half intensity
  ATTR_BRIGHT           = $00020000;    // high intensity
  ATTR_INTENSITY        = $00030000;    // mask to get intensity
  ATTR_UNDERLINE        = $00040000;    // underline attribute
  ATTR_BOLD             = $00080000;    // bold style
  ATTR_ITALIC           = $00100000;    // italic style
  ATTR_REVERSE          = $00200000;    // reverse fg/bg
  ATTR_BLINK            = $00800000;    // enable blinking
  ATTR_FONT             = $FF000000;    // font table

function ATTR_COLOR(fg, bg: Integer): Integer;

type

  PGGIColor = ^TGGIColor;

  TGGIColor = record
    r, g, b, a: Word;
  end;

  PGGIClut = ^TGGIClut;

  TGGIClut = record
    size: Word;
    data: PGGIColor;
  end;

const GGI_COLOR_PRECISION = 16;         // 16 bit per R,G, B value

// Graphtypes

type TGGIGraphType = LongWord;

const

  GT_DEPTH_SHIFT        = 0;
  GT_SIZE_SHIFT         = 8;
  GT_SUBSCHEME_SHIFT    = 16;
  GT_SCHEME_SHIFT       = 24;

  GT_DEPTH_MASK         = $ff shl GT_DEPTH_SHIFT;
  GT_SIZE_MASK          = $ff shl GT_SIZE_SHIFT;
  GT_SUBSCHEME_MASK     = $ff shl GT_SUBSCHEME_SHIFT;
  GT_SCHEME_MASK        = $ff shl GT_SCHEME_SHIFT;

// Macros to extract info from a ggi_graphtype.

function GT_DEPTH(x: Integer): Integer;
function GT_SIZE(x: Integer): Integer;
function GT_SUBSCHEME(x: Integer): Integer;
function GT_SCHEME(x: Integer): Integer;

{procedure GT_SETDEPTH(gt, x: Integer);
procedure GT_SETSIZE(gt, x: Integer);
procedure GT_SETSUBSCHEME(gt, x: Integer);
procedure GT_SETSCHEME(gt, x: Integer);}


const

// Enumerated schemes

  GT_TEXT               = 1 shl GT_SCHEME_SHIFT;
  GT_TRUECOLOR          = 2 shl GT_SCHEME_SHIFT;
  GT_GREYSCALE          = 3 shl GT_SCHEME_SHIFT;
  GT_PALETTE            = 4 shl GT_SCHEME_SHIFT;
  GT_STATIC_PALETTE     = 5 shl GT_SCHEME_SHIFT;

// Subschemes
  GT_SUB_REVERSE_ENDIAN = 1 shl GT_SUBSCHEME_SHIFT;
  GT_SUB_HIGHBIT_RIGHT  = 2 shl GT_SUBSCHEME_SHIFT;
  GT_SUB_PACKED_GETPUT  = 4 shl GT_SUBSCHEME_SHIFT;

// Macro that constructs a graphtype
function GT_CONSTRUCT(depth, scheme, size: Integer): Integer;


const

// Common graphtypes

  GT_TEXT16             = 4 or GT_TEXT or (16 shl GT_SIZE_SHIFT);
  GT_TEXT32             = 8 or GT_TEXT or (32 shl GT_SIZE_SHIFT);
  GT_1BIT               = 1 or GT_PALETTE or (1 shl GT_SIZE_SHIFT);
  GT_2BIT               = 2 or GT_PALETTE or (2 shl GT_SIZE_SHIFT);
  GT_4BIT               = 4 or GT_PALETTE or (4 shl GT_SIZE_SHIFT);
  GT_8BIT               = 8 or GT_PALETTE or (8 shl GT_SIZE_SHIFT);
  GT_15BIT              = 15 or GT_TRUECOLOR or (16 shl GT_SIZE_SHIFT);
  GT_16BIT              = 16 or GT_TRUECOLOR or (16 shl GT_SIZE_SHIFT);
  GT_24BIT              = 24 or GT_TRUECOLOR or (24 shl GT_SIZE_SHIFT);
  GT_32BIT              = 24 or GT_TRUECOLOR or (32 shl GT_SIZE_SHIFT);
  GT_AUTO               = 0;
  GT_INVALID            = $ffffffff;

// ggi_mode structure

type

  TGGIMode = record                     // requested by user and changed by driver
    Frames: LongInt;                    // frames needed
    Visible: TGGICoord;                 // vis. pixels, may change slightly
    Virt: TGGICoord;                    // virtual pixels, may change
    Size: TGGICoord;                    // size of visible in mm
    GraphType: TGGIGraphType;           // which mode ?
    dpp: TGGICoord;                     // dots per pixel
  end;


{******************************************************************************
 LibGGI specific events
 ******************************************************************************}

const

  GGI_CMDFLAG_LIBGGI    = GII_CMDFLAG_EXTERNAL shr 1;

  { Tell target that the application should not/should be halted when the
    display is unmapped.        The default is to halt the application.}

 GGICMD_NOHALT_ON_UNMAP = GII_CMDFLAG_EXTERNAL or GGI_CMDFLAG_LIBGGI or GII_CMDFLAG_NODATA or 1;
 GGICMD_HALT_ON_UNMAP   = GII_CMDFLAG_EXTERNAL or GGI_CMDFLAG_LIBGGI or GII_CMDFLAG_NODATA or 2;

  { Requests the application to switch target/mode, or to stop drawing on
    the visual.
    The latter is only sent if the application has explicitly requested
    GGICMD_NOHALT_ON_UNMAP. When a GGI_REQSW_UNMAP request is sent the
    application should respond by sending a GGICMD_ACKNOWLEDGE_SWITCH event
    as quickly as possible. After the acknowledge event is sent the
    application must not draw onto the visual until it recieves an evExpose
     event, which tells the application that the visual is mapped back again.
  }

  GGICMD_REQUEST_SWITCH = GII_CMDFLAG_EXTERNAL or GGI_CMDFLAG_LIBGGI or 1;

  // Used for 'request' field in ggi_cmddata_switchrequest
  GGI_REQSW_UNMAP       = 1;
  GGI_REQSW_MODE        = 2;
  GGI_REQSW_TARGET      = 4;

type

  TGGICmdDataSwitchRequest = record
    Request: LongWord;
    Mode: TGGIMode;
    target: array[0..63] of Char;
  end;

const

  GGICMD_ACKNOWLEDGE_SWITCH = GII_CMDFLAG_EXTERNAL or GGI_CMDFLAG_LIBGGI or GII_CMDFLAG_NODATA or 3;


type

  TGGIVisual = Pointer;
  TGGIResource = Pointer;


// Flags and frames

  TGGIFlags = LongWord;

const

  GGIFLAG_ASYNC = 1;

{******************************************************************************
 Misc macros
 ******************************************************************************}

// Swap the bytes in a 16 respective 32 bit unsigned number
function GGI_BYTEREV16(x: Integer): Integer;
function GGI_BYTEREV32(x: LongWord): LongWord;

// Swap the bitgroups in an 8 bit unsigned number
function GGI_BITREV4(x: Integer): Integer;
function GGI_BITREV2(x: Integer): Integer;
function GGI_BITREV1(x: Integer): Integer;


{******************************************************************************
 Information that can be returned to user apps
 ******************************************************************************}

// Bitmeaning defines

const

  GGI_BM_TYPE_NONE      = 0;            // This bit is not in use


  // Bit influences color of displayed pixel

  GGI_BM_TYPE_COLOR     = $010000;

  GGI_BM_SUB_RED        = $0100;
  GGI_BM_SUB_GREEN      = $0200;
  GGI_BM_SUB_BLUE       = $0300;

  GGI_BM_SUB_CYAN       = $1000;
  GGI_BM_SUB_MAGENTA    = $1100;
  GGI_BM_SUB_YELLOW     = $1200;
  GGI_BM_SUB_K          = $1300;

  GGI_BM_SUB_Y          = $2000;
  GGI_BM_SUB_U          = $2100;
  GGI_BM_SUB_V          = $2200;

  GGI_BM_SUB_CLUT       = $f000;        // This bit Color or attrib ?


  // Bit changes appearance of pixel/glyph

  GGI_BM_TYPE_ATTRIB    = $020000;

  GGI_BM_SUB_ALPHA      = $0100;

  GGI_BM_SUB_BLINK      = $1000;
  GGI_BM_SUB_INTENSITY  = $1100;
  GGI_BM_SUB_UNDERLINE  = $1200;
  GGI_BM_SUB_BOLD       = $1300;
  GGI_BM_SUB_ITALIC     = $1400;

  GGI_BM_SUB_FGCOL      = $2000;
  GGI_BM_SUB_BGCOL      = $2100;

  GGI_BM_SUB_TEXNUM     = $3000;
  GGI_BM_SUB_FONTSEL    = $3100;        // select different font banks
  GGI_BM_SUB_PALSEL     = $3200;        // select different palettes
  GGI_BM_SUB_MODESEL    = $3300;        // select different palettes

  // Bit that influence drawing logic

  GGI_BM_TYPE_LOGIC     = $030000;

  GGI_BM_SUB_ZBUFFER    = $0100;
  GGI_BM_SUB_WRITEPROT  = $1000;
  GGI_BM_SUB_WINDOWID   = $2000;




// Pixelformat for ggiGet/Put* buffers and pixellinearbuffers */

type

  PGGIPixelFormat = ^TGGIPixelFormat;

  TGGIPixelFormat = record
    depth: Integer;                     // Number of significant bits
    size: Integer;                      // Physical size in bits


    {* Simple and common things first :
     *
     * Usage of the mask/shift pairs:
     * If new_value is the _sizeof(ggi_pixel)*8bit_ value of the thing
     * you want to set, you do
     *
     * *pointer &= ~???_mask;           // Mask out old bits
     * *pointer |= (new_value>>shift) & ???_mask;
     *
     * The reason to use 32 bit and "downshifting" is alignment
     * and extensibility. You can easily adjust to other datasizes
     * with a simple addition ...
     *}

    // Simple colors:
    red_mask: TGGIPixel;                // Bitmask of red bits
    red_shift: Integer;                 // Shift for red bits

    green_mask: TGGIPixel;              // Bitmask of green bits
    green_shift: Integer;               // Shift for green bits

    blue_mask: TGGIPixel;               // Bitmask of blue bits
    blue_shift: Integer;                // Shift for blue bits

    // A few common attributes:
    alpha_mask: TGGIPixel;              // Bitmask of alphachannel bits
    alpha_shift: Integer;               // Shift for alpha bits

    clut_mask: TGGIPixel;               // Bitmask of bits for the clut
    clut_shift: Integer;                // Shift for bits for the clut

    fg_mask: TGGIPixel;                 // Bitmask of foreground color
    fg_shift: Integer;                  // Shift for foreground color

    bg_mask: TGGIPixel;                 // Bitmask of background color
    bg_shift: Integer;                  // Shift for background color

    texture_mask: TGGIPixel;            // Bitmask of the texture (for
                                        // textmodes - the actual character)
    texture_shift: Integer;             // Shift for texture

    // Now if this does not suffice you might want to parse the following
    // to find out what each bit does:
    bitmeaning: array[0..SizeOf(TGGIPixel) * 8 - 1] of LongWord;

    // Shall we keep those?
    flags: LongWord;                    // Pixelformat flags

    stdformat: LongWord;                // Standard format identifier

    {* This one has only one use for the usermode application:
     * To quickly check, if two buffers are identical. If both
     * stdformats are the same and _NOT_ 0 (which means "WEIRD"),
     * you may use things like memcpy between them which will have
     * the desired effect ...
     *}
end;

const

  // Pixelformat flags
  GGI_PF_REVERSE_ENDIAN = 1;
  GGI_PF_HIGHBIT_RIGHT  = 2;
  GGI_PF_HAM            = 4;
  GGI_PF_EXTENDED       = 8;


{******************************************************************************
 DirectBuffer
 ******************************************************************************}

type

  TGGIBufferLayout = (
        blPixelLinearBuffer,
        blPixelPlanarBuffer,
        blExtended,

        blLastBufferLayout
  );


  PGGIPixelLinearBuffer = ^TGGIPixelLinearBuffer;

  TGGIPixelLinearBuffer = record
    stride: Integer;                    // bytes per row
    pixelformat: PGGIPixelFormat;       // format of the pixels
  end;


  PGGIPixelPlanarBuffer = ^TGGIPixelPlanarBuffer;

  TGGIPixelPlanarBuffer = record
    next_line: Integer;                 // bytes until next line
    next_plane: Integer;                // bytes until next plane
    pixelformat: PGGIPixelFormat;       // format of the pixels
  end;

// Buffer types

const

  GGI_DB_NORMAL         = 1;            // "frame" is valid when set
  GGI_DB_EXTENDED       = 2;
  GGI_DB_MULTI_LEFT     = 4;
  GGI_DB_MULTI_RIGHT    = 8;

  // Flags that may be 'or'ed with the buffer type
  GGI_DB_SIMPLE_PLB     = $01000000;
  { GGI_DB_SIMPLE_PLB means that the buffer has the following properties:
    type = GGI_DB_NORMAL
    read = write
    noaccess = 0
    align = 0
    layout = blPixelLinearBuffer
  }

type

  PGGIDirectBuffer = ^TGGIDirectBuffer;

  TGGIDirectBuffer = record
    BufferType: LongWord;               // buffer type
    frame: Integer;                     // framenumber (GGI_DB_NORMAL)

    // access info
    resource: TGGIResource;             // If non-NULL you must acquire the
                                        // buffer before using it
    read: Pointer;                      // buffer address for reads
    write:Pointer;                      // buffer address for writes
    page_size: LongWord;                // zero for true linear buffers

    noaccess: LongWord;
    {bitfield. bit x set means you may _not_ access this DB at the
     width of 2^x bytes. Usually 0, but _check_ it.}

    align: LongWord;
    {bitfield. bit x set means you may only access this DB at the
     width of 2^x bytes, when the access is aligned to a multiple
     of 2^x. Note that bit 0 is a bit bogus here, but it should
     be always 0, as then ((noaccess|align)==0) is a quick check
     for "no restrictions". }

    layout: TGGIBufferLayout;

    // The actual buffer info. Depends on layout.
    buffer: record
      case Integer of
        0: (plb: TGGIPixelLinearBuffer);
        1: (plan: TGGIPixelPlanarBuffer);
        2: (extended: Pointer);
      end;
  end;


{******************************************************************************
 Resource management
 ******************************************************************************}

// Access types

const

  GGI_ACTYPE_READ       = 1 shl 0;
  GGI_ACTYPE_WRITE      = 1 shl 1;


{******************************************************************************
 LibGGI function definitions
******************************************************************************}

// Enter and leave the library

function  ggiInit: Integer; cdecl; external libggi;
procedure ggiExit; cdecl; external libggi;
procedure ggiPanic(format: PChar; args: array of const); cdecl; external libggi;


// Open a new visual - use display 'NULL' for the default visual
function  ggiOpen(display: PChar; args: array of const): TGGIVisual; cdecl; external libggi;
function  ggiClose(vis: TGGIVisual): Integer; cdecl; external libggi;

// Get/Set info
function  ggiSetFlags(vis: TGGIVisual; flags: TGGIFlags): Integer; cdecl; external libggi;
function  ggiGetFlags(vis: TGGIVisual): TGGIFlags; cdecl; external libggi;

function  ggiAddFlags(vis: TGGIVisual; flags: TGGIFlags): Integer;
function  ggiRemoveFlags(vis: TGGIVisual; flags: TGGIFlags): Integer;

function  ggiGetPixelFormat(vis: TGGIVisual): PGGIPixelFormat; cdecl; external libggi;


// DirectBuffer (DB) functions
function  ggiDBGetNumBuffers(vis: TGGIVisual): Integer; cdecl; external libggi;
function  ggiDBGetBuffer(vis: TGGIVisual; bufnum: Integer): PGGIDirectBuffer; cdecl; external libggi;

// Resource functions
function  ggiResourceAcquire(res: TGGIResource; actype: LongWord): Integer;
function  ggiResourceRelease(res: TGGIResource): Integer;
function  ggiResourceFastAcquire(res: TGGIResource; actype: LongWord): Integer; cdecl; external libggi;
function  ggiResourceFastRelease(res: TGGIResource): Integer; cdecl; external libggi;


// Library management

const GGI_MAX_APILEN = 1024;

function  ggiGetAPI(vis: TGGIVisual; num: Integer; APIName, arguments: PChar): Integer; cdecl; external libggi;

const GGI_CHG_APILIST = 1;

function  ggiIndicateChange(vis: TGGIVisual; WhatChanged: Integer): Integer; cdecl; external libggi;


// Mode management

function  ggiSetMode(visual: TGGIVisual; var tm: TGGIMode): Integer; cdecl; external libggi;
function  ggiGetMode(visual: TGGIVisual; var tm: TGGIMode): Integer; cdecl; external libggi;
function  ggiCheckMode(visual: TGGIVisual; var tm: TGGIMode): Integer; cdecl; external libggi;
function  ggiSetTextMode(visual: TGGIVisual; cols, rows, vcols, vrows, fontx, fonty: Integer; AType: TGGIGraphType): Integer; cdecl; external libggi;
function  ggiCheckTextMode(visual: TGGIVisual; cols, rows, vcols, vrows, fontx, fonty: Integer; var SuggestedMode: TGGIMode): Integer; cdecl; external libggi;
function  ggiSetGraphMode(visual: TGGIVisual; x, y, xv, yv: Integer; AType: TGGIGraphType): Integer; cdecl; external libggi;
function  ggiCheckGraphMode(visual: TGGIVisual; x, y, xv, yv: Integer; AType: TGGIGraphType; var SuggestedMode: TGGIMode): Integer; cdecl; external libggi;
function  ggiSetSimpleMode(visual: TGGIVisual; xsize, ysize, frames: Integer; AType: TGGIGraphType): Integer; cdecl; external libggi;
function  ggiCheckSimpleMode(visual: TGGIVisual; xsize, ysize, frames: Integer; AType: TGGIGraphType; var md: TGGIMode): Integer; cdecl; external libggi;


// Print all members of the mode struct

function  ggiSPrintMode(s: PChar; var m: TGGIMode): Integer; cdecl; external libggi;
// function ggiFPrintMode(s: PFile; var m: TGGIMode): Integer; cdecl; external libggi;
// #define ggiPrintMode(m) ggiFPrintMode(stdout,(m))


// Fill a mode struct from the text string s

function  ggiParseMode(s: PChar; var m: TGGIMode): Integer; cdecl; external libggi;


// Flush all pending operations to the display device

// Normal flush
function  ggiFlush(vis: TGGIVisual): Integer; cdecl; external libggi;

// Flush only the specified region if it would improve performance
function  ggiFlushRegion(vis: TGGIVisual; x, y, w, h: Integer): Integer; cdecl; external libggi;


// Graphics context

function  ggiSetGCForeground(vis: TGGIVisual; const Color: TGGIPixel): Integer; cdecl; external libggi;
function  ggiGetGCForeground(vis: TGGIVisual; var Color: TGGIPixel): Integer; cdecl; external libggi;
function  ggiSetGCBackground(vis: TGGIVisual; const Color: TGGIPixel): Integer; cdecl; external libggi;
function  ggiGetGCBackground(vis: TGGIVisual; var Color: TGGIPixel): Integer; cdecl; external libggi;
function  ggiSetGCClipping(vis: TGGIVisual; left, top, right, bottom: Integer): Integer; cdecl; external libggi;
function  ggiGetGCClipping(vis: TGGIVisual; var left, top, right, bottom: Integer): Integer; cdecl; external libggi;


// Color palette manipulation
function  ggiMapColor(vis: TGGIVisual; var Color: TGGIColor): TGGIPixel; cdecl; external libggi;
function  ggiUnmapPixel(vis: TGGIVisual; pixel: TGGIPixel; var Color: TGGIColor): Integer; cdecl; external libggi;
function  ggiPackColors(vis: TGGIVisual; var buf; var cols: TGGIColor; len: Integer): Integer; cdecl; external libggi;
function  ggiUnpackPixels(vis: TGGIVisual; var buf; var cols: TGGIColor; len: Integer): Integer; cdecl; external libggi;
function  ggiGetPalette(vis: TGGIVisual; s, len: Integer; var cmap: TGGIColor): Integer; cdecl; external libggi;
function  ggiSetPalette(vis: TGGIVisual; s, len: Integer; var cmap: TGGIColor): Integer; cdecl; external libggi;
function  ggiSetColorfulPalette(vis: TGGIVisual): Integer; cdecl; external libggi;

const GGI_PALETTE_DONTCARE = -1;


// Gamma map manipulation

function  ggiGetGamma(vis: TGGIVisual; var r, g, b: Double): Integer; cdecl; external libggi;
function  ggiSetGamma(vis: TGGIVisual; r, g, b: Double): Integer; cdecl; external libggi;
function  ggiGetGammaMap(vis: TGGIVisual; s, len: Integer; var gammamap: TGGIColor): Integer; cdecl; external libggi;
function  ggiSetGammaMap(vis: TGGIVisual; s, len: Integer; var gammamap: TGGIColor): Integer; cdecl; external libggi;


// Origin handling

function  ggiSetOrigin(vis: TGGIVisual; x, y: Integer): Integer; cdecl; external libggi;
function  ggiGetOrigin(vis: TGGIVisual; var x, y: Integer): Integer; cdecl; external libggi;


// Frame handling

function  ggiSetDisplayFrame(vis: TGGIVisual; FrameNo: Integer): Integer; cdecl; external libggi;
function  ggiSetReadFrame(vis: TGGIVisual; FrameNo: Integer): Integer; cdecl; external libggi;
function  ggiSetWriteFrame(vis: TGGIVisual; FrameNo: Integer): Integer; cdecl; external libggi;

function  ggiGetDisplayFrame(vis: TGGIVisual): Integer; cdecl; external libggi;
function  ggiGetReadFrame(vis: TGGIVisual): Integer; cdecl; external libggi;
function  ggiGetWriteFrame(vis: TGGIVisual): Integer; cdecl; external libggi;


// Generic drawing routines

function  ggiFillscreen(vis: TGGIVisual): Integer; cdecl; external libggi;

function  ggiDrawPixel(vis: TGGIVisual; x, y: Integer): Integer; cdecl; external libggi;
function  ggiPutPixel(vis: TGGIVisual; x, y: Integer; pixel: TGGIPixel): Integer; cdecl; external libggi;
function  ggiGetPixel(vis: TGGIVisual; x, y: Integer; var pixel: TGGIPixel): Integer; cdecl; external libggi;

function  ggiDrawLine(vis: TGGIVisual; x, y, xe, ye: Integer): Integer; cdecl; external libggi;
function  ggiDrawHLine(vis: TGGIVisual; x, y, w: Integer): Integer; cdecl; external libggi;
function  ggiPutHLine(vis: TGGIVisual; x, y, w: Integer; var buf): Integer; cdecl; external libggi;
function  ggiGetHLine(vis: TGGIVisual; x, y, w: Integer; var buf): Integer; cdecl; external libggi;

function  ggiDrawVLine(vis: TGGIVisual; x, y, h: Integer): Integer; cdecl; external libggi;
function  ggiPutVLine(vis: TGGIVisual; x, y, h: Integer; var buf): Integer; cdecl; external libggi;
function  ggiGetVLine(vis: TGGIVisual; x, y, h: Integer; var buf): Integer; cdecl; external libggi;

function  ggiDrawBox(vis: TGGIVisual; x, y, w, h: Integer): Integer; cdecl; external libggi;
function  ggiPutBox(vis: TGGIVisual; x, y, w, h: Integer; var buffer): Integer; cdecl; external libggi;
function  ggiGetBox(vis: TGGIVisual; x, y, w, h: Integer; var buffer): Integer; cdecl; external libggi;
function  ggiCopyBox(vis: TGGIVisual; x, y, w, h, nx, ny: Integer): Integer; cdecl; external libggi;
function  ggiCrossBlit(src: TGGIVisual; sx, sy, w, h: Integer; dst: TGGIVisual; dx, dy: Integer): Integer; cdecl; external libggi;


// Text drawing routines

function  ggiPutc(vis: TGGIVisual; x, y: Integer; c: Char): Integer; cdecl; external libggi;
function  ggiPuts(vis: TGGIVisual; x, y: Integer; str: PChar): Integer; cdecl; external libggi;
function  ggiGetCharSize(vis: TGGIVisual; var width, height: Integer): Integer; cdecl; external libggi;


// Event handling

//###function  ggiEventPoll(vis: TGGIVisual; mask: TGIIEventMask; var t: TTimeVal): TGIIEventMask; cdecl; external libggi;
function  ggiEventsQueued(vis: TGGIVisual; mask: TGIIEventMask): Integer; cdecl; external libggi;
function  ggiEventRead(vis: TGGIVisual; var Event: TGIIEvent; mask: TGIIEventMask): Integer; cdecl; external libggi;
function  ggiSetEventMask(vis: TGGIVisual; EventMask: TGIIEventMask): Integer; cdecl; external libggi;
function  ggiGetEventMask(vis: TGGIVisual): TGIIEventMask; cdecl; external libggi;
function  ggiEventSend(vis: TGGIVisual; var Event: TGIIEvent): Integer; cdecl; external libggi;
function  ggiJoinInputs(vis: TGGIVisual; Input: TGIIInput): TGIIInput; cdecl; external libggi;

function  ggiAddEventMask(vis: TGGIVisual; Mask: TGIIEventMask): Integer;
function  ggiRemoveEventMask(vis: TGGIVisual; Mask: TGIIEventMask): Integer;


// Convenience functions

function  ggiKbhit(vis: TGGIVisual): Integer; cdecl; external libggi;
function  ggiGetc(vis: TGGIVisual): Integer; cdecl; external libggi;


// Extension handling

type
  TGGILibID = Pointer;
  TGGIExtID = Integer;  {Don't rely on that !}
  TGGIParamChangeProc = function(Visual: TGGIVisual; WhatChanged: Integer): Integer;

function  ggiExtensionRegister(name: PChar; size: Integer;
  ParamChange: TGGIParamChangeProc): TGGIExtID; cdecl; external libggi;
function  ggiExtensionUnregister(id: TGGIExtID): Integer; cdecl; external libggi;
function  ggiExtensionAttach(Visual: TGGIVisual; id: TGGIExtID): Integer; cdecl; external libggi;
function  ggiExtensionDetach(Visual: TGGIVisual; id: TGGIExtID): Integer; cdecl; external libggi;
function  ggiExtensionLoadDL(Visual: TGGIVisual; filename, args: PChar; ArgPtr: Pointer): TGGILibID; cdecl; external libggi;



// ===================================================================
// ===================================================================

implementation


function ATTR_COLOR(fg, bg: Integer): Integer;
begin
  Result := (bg and $ff) or ((fg and $ff) shl 8);
end;

function GT_DEPTH(x: Integer): Integer;
begin
  Result := (x and GT_DEPTH_MASK) shr GT_DEPTH_SHIFT;
end;

function GT_SIZE(x: Integer): Integer;
begin
  Result := (x and GT_SIZE_MASK) shr GT_SIZE_SHIFT;
end;

function GT_SUBSCHEME(x: Integer): Integer;
begin
  Result := x and GT_SUBSCHEME_MASK;
end;

function GT_SCHEME(x: Integer): Integer;
begin
  Result := x and GT_SCHEME_MASK;
end;

function GT_CONSTRUCT(depth, scheme, size: Integer): Integer;
begin
  Result := depth or scheme or (size shl GT_SIZE_SHIFT);
end;



function GGI_BYTEREV16(x: Integer): Integer;
begin
  Result := (x shl 8) or (x shr 8);
end;

function GGI_BYTEREV32(x: LongWord): LongWord;
begin
  Result := (x shl 24) or ((x and $ff00) shl 8) or
    ((x and $ff0000) shr 8) or (x shr 24);
end;

function GGI_BITREV4(x: Integer): Integer;
begin
  Result := (x shr 4) or (x shl 4);
end;

function GGI_BITREV2(x: Integer): Integer;
begin
  Result := (x shr 6) or ((x and $30) shr 2) or ((x and $0c) shl 2) or (x shl 6);
end;

function GGI_BITREV1(x: Integer): Integer;
begin
  Result := (x shr 7) or ((x and $40) shr 5) or ((x and $20) shr 3) or
    ((x and $10) shr 1) or ((x and 8) shl 1) or ((x and 4) shl 3) or
    ((x and 2) shl 4) or (x shl 7);
end;

function ggiAddFlags(vis: TGGIVisual; flags: TGGIFlags): Integer;
begin
  Result := ggiSetFlags(vis, ggiGetFlags(vis) or flags);
end;

function ggiRemoveFlags(vis: TGGIVisual; flags: TGGIFlags): Integer;
begin
  Result := ggiSetFlags(vis, ggiGetFlags(vis) and not flags);
end;

function ggiResourceAcquire(res: TGGIResource; actype: LongWord): Integer;
begin
  if res = nil then Result := 0
  else Result := ggiResourceFastAcquire(res, actype);
end;

function ggiResourceRelease(res: TGGIResource): Integer;
begin
  if res = nil then Result := 0
  else Result := ggiResourceFastRelease(res);
end;

function ggiAddEventMask(vis: TGGIVisual; Mask: TGIIEventMask): Integer;
begin
  Result := ggiSetEventMask(vis, ggiGetEventMask(vis) or mask);
end;

function ggiRemoveEventMask(vis: TGGIVisual; Mask: TGIIEventMask): Integer;
begin
  Result := ggiSetEventMask(vis, ggiGetEventMask(vis) and not mask);
end;


end.
