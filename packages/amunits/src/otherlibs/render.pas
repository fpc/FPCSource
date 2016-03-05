{
  This file is part of the Free Pascal run time library.

  A file in Amiga system run time library.
  Copyright (c) 2003 by Nils Sjöholm.
  member of the Amiga RTL development team.

  This is a unit for render.library

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
{
  History:

  First version of this unit.
  16 Jan 2003.

  Changed cardinal > longword.
  Changed startcode for unit.
  12 Feb 2003

  nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

{$mode objfpc}

UNIT RENDER;

INTERFACE
USES Exec,utility,agraphics;

VAR RenderBase : pLibrary = nil;

type
    pPLANEPTR = ^TPLANEPTR;

const
    RENDERNAME : PChar = 'render.library';

{
        $VER: render.h v40 (19.12.2002)
        render.library definitions
   }

  const
     RND_TAGBASE = TAG_USER + $1000;
  {

        memhandler

                                                                          }
  { type of memhandler, see below  }
     RND_MemType = RND_TAGBASE + 1;
  { ptr to block of memory  }
     RND_MemBlock = RND_TAGBASE + 2;
  { size of memblock [bytes]  }
     RND_MemSize = RND_TAGBASE + 3;
  { memflags (exec/memory.h)  }
     RND_MemFlags = RND_TAGBASE + 18;
  { to pass a memhandler as an argument  }
     RND_RMHandler = RND_TAGBASE + 12;
  {
        memhandler types
    }
  { v39 exec dynamic pool  }
     RMHTYPE_POOL = 1;
  { private memory pool  }
     RMHTYPE_PRIVATE = 2;
  { common public memory  }
     RMHTYPE_PUBLIC = 3;
  {

        palette

                                                                          }
  { palette import/export format  }
     RND_PaletteFormat = RND_TAGBASE + 19;
  { tag to indicate a palette is EHB  }
     RND_EHBPalette = RND_TAGBASE + 22;
  { first palette entry  }
     RND_FirstColor = RND_TAGBASE + 23;
  { dispose the old palette and load a new one  }
     RND_NewPalette = RND_TAGBASE + 24;
  { quantization factors  }
     RND_RGBWeight = RND_TAGBASE + 11;
  {
        palette format types
    }
  { ULONG red,green,blue  }
     PALFMT_RGB32 = 1;
  { ULONG 0x00rrggbb  }
     PALFMT_RGB8 = 2;
  { UWORD 0xrgb  }
     PALFMT_RGB4 = 3;
  { render.library palette  }
     PALFMT_PALETTE = 4;
  {
        palette sort mode types
        for the use with SortPalette()
    }
  { no particular order  }
     PALMODE_NONE = $0000;
  { sort palette entries by brightness  }
     PALMODE_BRIGHTNESS = $0001;
  { sort palette entries by the number of pixels that they represent.
           You must supply the RND_Histogram taglist argument.  }
     PALMODE_POPULARITY = $0002;
  { sort palette entries by the number of histogram entries that they
           represent. You must supply the RND_Histogram taglist argument.  }
     PALMODE_REPRESENTATION = $0003;
  { sort palette entries by their optical significance for the human
           eye. Implementation is unknown to you and may change.
           You must supply the RND_Histogram taglist argument.  }
     PALMODE_SIGNIFICANCE = $0004;
  { sort palette entries by color intensity  }
     PALMODE_SATURATION = $0005;
  { By default, sort direction is descending, i.e. the precedence is
           more-to-less. Combine with this flag to invert the sort direction.  }
     PALMODE_ASCENDING = $0008;
  {

        histogram related

                                                                          }
  { histogram type, see below  }
     RND_HSType = RND_TAGBASE + 4;
  { a histogram as an argument  }
     RND_Histogram = RND_TAGBASE + 9;
  {
        Histogram / Palette types
        to be specified with RND_HSType
    }
  { 12bit dynamic histogram  }
     HSTYPE_12BIT = 4;
  { 15bit dynamic histogram  }
     HSTYPE_15BIT = 5;
  { 18bit dynamic histogram  }
     HSTYPE_18BIT = 6;
  { 21bit dynamic histogram  }
     HSTYPE_21BIT = 7;
  { 24bit dynamic histogram  }
     HSTYPE_24BIT = 8;
  { 12bit tabular histogram  }
     HSTYPE_12BIT_TURBO = 20;
  { 15bit tabular histogram  }
     HSTYPE_15BIT_TURBO = 21;
  { 18bit tabular histogram  }
     HSTYPE_18BIT_TURBO = 22;
  {
        tags that can be queried via QueryHistogram()
    }
  { # pixels in a histogram  }
     RND_NumPixels = RND_TAGBASE + 5;
  { # colors in a histogram  }
     RND_NumColors = RND_TAGBASE + 6;
  {

        rendering and conversions

                                                                          }
  { color mode, see below  }
     RND_ColorMode = RND_TAGBASE + 7;
  { dither mode, see below  }
     RND_DitherMode = RND_TAGBASE + 8;
  { dither amount  }
     RND_DitherAmount = RND_TAGBASE + 26;
  { first color index to be output  }
     RND_OffsetColorZero = RND_TAGBASE + 10;
  {
        color mode types
        to be specified with RND_ColorMode
    }
  { normal palette lookup  }
     COLORMODE_CLUT = $0000;
  { HAM8 mode  }
     COLORMODE_HAM8 = $0001;
  { HAM6 mode  }
     COLORMODE_HAM6 = $0002;
  { mask to determine COLORMODE  }
     COLORMODE_MASK = $0003;
  {
        dither mode types
        to be specified with RND_DitherMode
    }
  { no dither  }
     DITHERMODE_NONE = $0000;
  { Floyd-Steinberg dither  }
     DITHERMODE_FS = $0001;
  { random dither. amount required.  }
     DITHERMODE_RANDOM = $0002;
  { EDD dither  }
     DITHERMODE_EDD = $0003;
  {

        miscellaneous

                                                                          }
  { progress callback hook  }
     RND_ProgressHook = RND_TAGBASE + 13;
  { total input width [pixels]  }
     RND_SourceWidth = RND_TAGBASE + 14;
  { total output width [pixels]  }
     RND_DestWidth = RND_TAGBASE + 15;
  { ptr to a chunky conversion table  }
     RND_PenTable = RND_TAGBASE + 16;
  { chunky data left edge [pixels]  }
     RND_LeftEdge = RND_TAGBASE + 17;
  { line callback hook  }
     RND_LineHook = RND_TAGBASE + 20;
  { Mapping-Engine  }
     RND_MapEngine = RND_TAGBASE + 27;
  { Interleave  }
     RND_Interleave = RND_TAGBASE + 28;
  { Palette  }
     RND_Palette = RND_TAGBASE + 29;
  { Weight factor  }
     RND_Weight = RND_TAGBASE + 30;
  { ScaleEngine  }
     RND_ScaleEngine = RND_TAGBASE + 31;
  { Texture coordinates  }
     RND_DestCoordinates = RND_TAGBASE + 42;
  { backcolor for filling  }
     RND_BGColor = RND_TAGBASE + 43;
  { backpen for filling  }
     RND_BGPen = RND_TAGBASE + 44;
  {

        alpha-channel and masking

                                                                          }
  { custom alpha-channel  }
     RND_AlphaChannel = RND_TAGBASE + 32;
  { bytes between alpha-channel pixels  }
     RND_AlphaModulo = RND_TAGBASE + 33;
  { width of alpha-channel array  }
     RND_AlphaWidth = RND_TAGBASE + 34;
  { masking RGB for CreateAlphaArray  }
     RND_MaskRGB = RND_TAGBASE + 35;
  { mask value for outside color range  }
     RND_MaskFalse = RND_TAGBASE + 36;
  { mask value for inside color range  }
     RND_MaskTrue = RND_TAGBASE + 37;
  { total source width for 3channel operations  }
     RND_SourceWidth2 = RND_TAGBASE + 38;
  { second custom alpha-channel  }
     RND_AlphaChannel2 = RND_TAGBASE + 39;
  { pixel modulo for a second alpha-channel  }
     RND_AlphaModulo2 = RND_TAGBASE + 40;
  { width of a second alpha-channel array  }
     RND_AlphaWidth2 = RND_TAGBASE + 41;
  {

        PixelFormat

                                                                          }
  { pixel format, see below  }
     RND_PixelFormat = RND_TAGBASE + 25;
     PIXFMTB_CHUNKY = 3;
     PIXFMTB_BITMAP = 4;
     PIXFMTB_RGB = 5;
     PIXFMT_CHUNKY_CLUT = (1 shl PIXFMTB_CHUNKY) + COLORMODE_CLUT;
     PIXFMT_0RGB_32 = (1 shl PIXFMTB_RGB) + 0;
  {
        these types are currently not used by render.library, but
        some of them are applicable for guigfx.library functions:
    }
     PIXFMT_CHUNKY_HAM8 = (1 shl PIXFMTB_CHUNKY) + COLORMODE_HAM8;
     PIXFMT_CHUNKY_HAM6 = (1 shl PIXFMTB_CHUNKY) + COLORMODE_HAM6;
     PIXFMT_BITMAP_CLUT = (1 shl PIXFMTB_BITMAP) + COLORMODE_CLUT;
     PIXFMT_BITMAP_HAM8 = (1 shl PIXFMTB_BITMAP) + COLORMODE_HAM8;
     PIXFMT_BITMAP_HAM6 = (1 shl PIXFMTB_BITMAP) + COLORMODE_HAM6;
     PIXFMT_RGB_24 = (1 shl PIXFMTB_RGB) + 1;
  {
        strictly internal:
    }
     PIXFMT_BITMAP_RGB = (1 shl PIXFMTB_BITMAP) + (1 shl PIXFMTB_RGB);
  {

        ExtractPalette return codes

        You must at least check for EXTP_SUCCESS.
        EXTP_NO_DATA indicates that there were no colors
        in the histogram.

                                                                          }
     EXTP_SUCCESS = 0;
     EXTP_NOT_ENOUGH_MEMORY = 1;
     EXTP_CALLBACK_ABORTED = 2;
     EXTP_NO_DATA = 3;
  {

        AddRGB, AddRGBImage and AddChunkyImage return codes

        You must at least check for ADDH_SUCCESS.
        If not delivered, the histogram might be
        inaccurate.

                                                                          }
     ADDH_SUCCESS = 0;
     ADDH_NOT_ENOUGH_MEMORY = 1;
     ADDH_CALLBACK_ABORTED = 2;
     ADDH_NO_DATA = 3;
  {

        Render return codes

        You must at least check for REND_SUCCESS.
        If not delivered, the image has not been
        rendered completely.

                                                                          }
     REND_SUCCESS = 0;
     REND_NOT_ENOUGH_MEMORY = 1;
     REND_CALLBACK_ABORTED = 2;
     REND_NO_VALID_PALETTE = 3;
     REND_NO_DATA = 3;
  {

        SortPalette return codes

        You must at least check for SORTP_SUCCESS.
        SORTP_NO_DATA indicates that there were data missing,
        e.g. you specified no histogram or the histogram was empty.

                                                                          }
     SORTP_SUCCESS = 0;
     SORTP_NO_DATA = 1;
     SORTP_NOT_ENOUGH_MEMORY = 2;
     SORTP_NOT_IMPLEMENTED = 3;
  {

        conversion return codes

        These return codes apply to conversion functions
        such as Chunky2RGB and ConvertChunky.

                                                                          }
     CONV_SUCCESS = 0;
     CONV_CALLBACK_ABORTED = 1;
     CONV_NOT_ENOUGH_MEMORY = 2;
     CONV_NO_DATA = 3;





FUNCTION AddChunkyImageA(histogram : POINTER location 'a0'; chunky : pByte location 'a1'; width : WORD location 'd0'; height : WORD location 'd1'; palette : POINTER location 'a2'; taglist : pTagItem location 'a3') : longword; syscall RenderBase 108;
FUNCTION AddHistogramA(histogram1 : POINTER location 'a0'; histogram2 : POINTER location 'a1'; taglist : pTagItem location 'a2') : longword; syscall RenderBase 222;
FUNCTION AddRGB(histogram : POINTER location 'a0'; RGB : longword location 'd0'; count : longword location 'd1') : longword; syscall RenderBase 96;
FUNCTION AddRGBImageA(histogram : POINTER location 'a0'; rgb : pULONG location 'a1'; width : WORD location 'd0'; height : WORD location 'd1'; taglist : pTagItem location 'a2') : longword; syscall RenderBase 102;
FUNCTION AllocRenderMem(rendermemhandler : POINTER location 'a0'; size : longword location 'd0') : POINTER; syscall RenderBase 54;
FUNCTION AllocRenderVec(rendermemhandler : POINTER location 'a0'; size : longword location 'd0') : POINTER; syscall RenderBase 66;
FUNCTION AllocRenderVecClear(rendermemhandler : POINTER location 'a0'; size : longword location 'd0') : POINTER; syscall RenderBase 306;
PROCEDURE ApplyAlphaChannelA(sourcearray : pULONG location 'a0'; width : WORD location 'd0'; height : WORD location 'd1'; destarray : pULONG location 'a1'; taglist : pTagItem location 'a2'); syscall RenderBase 294;
FUNCTION BestPen(palette : POINTER location 'a0'; rgb : longword location 'd0') : LONGINT; syscall RenderBase 204;
PROCEDURE Chunky2BitMapA(chunky : pByte location 'a0'; sx : WORD location 'd0'; sy : WORD location 'd1'; width : WORD location 'd2'; height : WORD location 'd3'; bitmap : pBitMap location 'a1'; dx : WORD location 'd4'; dy : WORD location 'd5'; taglist : pTagItem location 'a2'); syscall RenderBase 138;
FUNCTION Chunky2RGBA(chunky : pByte location 'a0'; width : WORD location 'd0'; height : WORD location 'd1'; rgb : pULONG location 'a1'; palette : POINTER location 'a2'; taglist : pTagItem location 'a3') : longword; syscall RenderBase 132;
FUNCTION ChunkyArrayDiversityA(chunky : pByte location 'a0'; palette : POINTER location 'a1'; width : WORD location 'd0'; height : WORD location 'd1'; taglist : pTagItem location 'a2') : LONGINT; syscall RenderBase 270;
FUNCTION ConvertChunkyA(source : pByte location 'a0'; oldpalette : POINTER location 'a1'; width : WORD location 'd0'; height : WORD location 'd1'; dest : pByte location 'a2'; newpalette : POINTER location 'a3'; taglist : pTagItem location 'a4') : longword; syscall RenderBase 162;
FUNCTION CountRGB(histogram : POINTER location 'a0'; rgb : longword location 'd0') : longword; syscall RenderBase 198;
PROCEDURE CreateAlphaArrayA(rgbarray : pULONG location 'a0'; width : WORD location 'd0'; height : WORD location 'd1'; taglist : pTagItem location 'a1'); syscall RenderBase 312;
FUNCTION CreateHistogramA(taglist : pTagItem location 'a1') : POINTER; syscall RenderBase 78;
FUNCTION CreateMapEngineA(palette : POINTER location 'a0'; taglist : pTagItem location 'a1') : POINTER; syscall RenderBase 246;
FUNCTION CreatePaletteA(taglist : pTagItem location 'a1') : POINTER; syscall RenderBase 174;
PROCEDURE CreatePenTableA(chunky : pByte location 'a0'; oldpalette : POINTER location 'a1'; width : WORD location 'd0'; height : WORD location 'd1'; newpalette : POINTER location 'a2'; convtab : pByte location 'a3'; taglist : pTagItem location 'a4'); syscall RenderBase 168;
FUNCTION CreateRMHandlerA(taglist : pTagItem location 'a1') : POINTER; syscall RenderBase 42;
FUNCTION CreateScaleEngineA(sourcewidth : WORD location 'd0'; sourceheight : WORD location 'd1'; destwidth : WORD location 'd2'; destheight : WORD location 'd3'; taglist : pTagItem location 'a1') : POINTER; syscall RenderBase 144;
PROCEDURE DeleteHistogram(histogram : POINTER location 'a0'); syscall RenderBase 84;
PROCEDURE DeleteMapEngine(engine : POINTER location 'a0'); syscall RenderBase 252;
PROCEDURE DeletePalette(palette : POINTER location 'a0'); syscall RenderBase 180;
PROCEDURE DeleteRMHandler(rmh : POINTER location 'a0'); syscall RenderBase 48;
PROCEDURE DeleteScaleEngine(engine : POINTER location 'a0'); syscall RenderBase 150;
PROCEDURE ExportPaletteA(palette : POINTER location 'a0'; coltab : POINTER location 'a1'; taglist : pTagItem location 'a2'); syscall RenderBase 192;
PROCEDURE ExtractAlphaChannelA(rgbarray : pULONG location 'a0'; width : WORD location 'd0'; height : WORD location 'd1'; chunkyarray : pByte location 'a1'; taglist : pTagItem location 'a2'); syscall RenderBase 288;
FUNCTION ExtractPaletteA(histogram : POINTER location 'a0'; palette : pULONG location 'a1'; numcolors : WORD location 'd0'; taglist : pTagItem location 'a2') : longword; syscall RenderBase 114;
PROCEDURE FlushPalette(palette : POINTER location 'a0'); syscall RenderBase 210;
PROCEDURE FreeRenderMem(rendermemhandler : POINTER location 'a0'; mem : POINTER location 'a1'; size : longword location 'd0'); syscall RenderBase 60;
PROCEDURE FreeRenderVec(mem : POINTER location 'a0'); syscall RenderBase 72;
PROCEDURE ImportPaletteA(palette : POINTER location 'a0'; coltab : POINTER location 'a1'; numcols : WORD location 'd0'; taglist : pTagItem location 'a2'); syscall RenderBase 186;
PROCEDURE InsertAlphaChannelA(maskarray : pByte location 'a0'; width : WORD location 'd0'; height : WORD location 'd1'; rgbarray : pULONG location 'a1'; taglist : pTagItem location 'a2'); syscall RenderBase 282;
FUNCTION MapChunkyArrayA(engine : POINTER location 'a0'; source : pByte location 'a1'; palette : POINTER location 'a2'; width : WORD location 'd0'; height : WORD location 'd1'; dest : pByte location 'a3'; taglist : pTagItem location 'a4') : longword; syscall RenderBase 276;
FUNCTION MapRGBArrayA(engine : POINTER location 'a0'; rgb : pULONG location 'a1'; width : WORD location 'd0'; height : WORD location 'd1'; chunky : pByte location 'a2'; taglist : pTagItem location 'a3') : longword; syscall RenderBase 258;
PROCEDURE MixAlphaChannelA(source1 : pULONG location 'a0'; source2 : pULONG location 'a1'; width : WORD location 'd0'; height : WORD location 'd1'; dest : pULONG location 'a2'; taglist : pTagItem location 'a3'); syscall RenderBase 318;
PROCEDURE MixRGBArrayA(sourcearray : pULONG location 'a0'; width : WORD location 'd0'; height : WORD location 'd1'; destarray : pULONG location 'a1'; ratio : WORD location 'd2'; taglist : pTagItem location 'a2'); syscall RenderBase 300;
PROCEDURE Planar2ChunkyA(planetab : pPLANEPTR location 'a0'; bytewidth : WORD location 'd0'; height : WORD location 'd1'; depth : WORD location 'd2'; bytesperrow : WORD location 'd3'; chunky : pByte location 'a1'; taglist : pTagItem location 'a2'); syscall RenderBase 126;
FUNCTION QueryHistogram(histogram : POINTER location 'a0'; d0arg : longword location 'd0') : longword; syscall RenderBase 90;
FUNCTION RenderA(rgb : pULONG location 'a0'; width : WORD location 'd0'; height : WORD location 'd1'; chunky : pByte location 'a1'; palette : POINTER location 'a2'; taglist : pTagItem location 'a3') : longword; syscall RenderBase 120;
FUNCTION RGBArrayDiversityA(rgb : pULONG location 'a0'; width : WORD location 'd0'; height : WORD location 'd1'; taglist : pTagItem location 'a1') : LONGINT; syscall RenderBase 264;
FUNCTION ScaleA(engine : POINTER location 'a0'; source : POINTER location 'a1'; dest : POINTER location 'a2'; taglist : pTagItem location 'a3') : longword; syscall RenderBase 156;
FUNCTION ScaleOrdinate(source : WORD location 'd0'; dest : WORD location 'd1'; ordinate : WORD location 'd2') : WORD; syscall RenderBase 228;
FUNCTION SortPaletteA(palette : POINTER location 'a0'; mode : longword location 'd0'; taglist : pTagItem location 'a1') : longword; syscall RenderBase 216;
PROCEDURE TintRGBArrayA(source : pULONG location 'a0'; width : WORD location 'd0'; height : WORD location 'd1'; RGB : longword location 'd2'; ratio : WORD location 'd3'; dest : pULONG location 'a1'; taglist : pTagItem location 'a2'); syscall RenderBase 324;
{
 Functions and procedures with array of const go here
}
FUNCTION AddChunkyImage(histogram : POINTER; chunky : pByte; width : WORD; height : WORD; palette : POINTER; const taglist : Array Of Const) : longword;
FUNCTION AddHistogram(histogram1 : POINTER; histogram2 : POINTER; const taglist : Array Of Const) : longword;
FUNCTION AddRGBImage(histogram : POINTER; rgb : pULONG; width : WORD; height : WORD; const taglist : Array Of Const) : longword;
PROCEDURE ApplyAlphaChannel(sourcearray : pULONG; width : WORD; height : WORD; destarray : pULONG; const taglist : Array Of Const);
PROCEDURE Chunky2BitMap(chunky : pByte; sx : WORD; sy : WORD; width : WORD; height : WORD; bitmap : pBitMap; dx : WORD; dy : WORD; const taglist : Array Of Const);
FUNCTION Chunky2RGB(chunky : pByte; width : WORD; height : WORD; rgb : pULONG; palette : POINTER; const taglist : Array Of Const) : longword;
FUNCTION ChunkyArrayDiversity(chunky : pByte; palette : POINTER; width : WORD; height : WORD; const taglist : Array Of Const) : LONGINT;
FUNCTION ConvertChunky(source : pByte; oldpalette : POINTER; width : WORD; height : WORD; dest : pByte; newpalette : POINTER; const taglist : Array Of Const) : longword;
PROCEDURE CreateAlphaArray(rgbarray : pULONG; width : WORD; height : WORD; const taglist : Array Of Const);
FUNCTION CreateHistogram(const taglist : Array Of Const) : POINTER;
FUNCTION CreateMapEngine(palette : POINTER; const taglist : Array Of Const) : POINTER;
FUNCTION CreatePalette(const taglist : Array Of Const) : POINTER;
PROCEDURE CreatePenTable(chunky : pByte; oldpalette : POINTER; width : WORD; height : WORD; newpalette : POINTER; convtab : pByte; const taglist : Array Of Const);
FUNCTION CreateRMHandler(const taglist : Array Of Const) : POINTER;
FUNCTION CreateScaleEngine(sourcewidth : WORD; sourceheight : WORD; destwidth : WORD; destheight : WORD; const taglist : Array Of Const) : POINTER;
PROCEDURE ExportPalette(palette : POINTER; coltab : POINTER; const taglist : Array Of Const);
PROCEDURE ExtractAlphaChannel(rgbarray : pULONG; width : WORD; height : WORD; chunkyarray : pByte; const taglist : Array Of Const);
FUNCTION ExtractPalette(histogram : POINTER; palette : pULONG; numcolors : WORD; const taglist : Array Of Const) : longword;
PROCEDURE ImportPalette(palette : POINTER; coltab : POINTER; numcols : WORD; const taglist : Array Of Const);
PROCEDURE InsertAlphaChannel(maskarray : pByte; width : WORD; height : WORD; rgbarray : pULONG; const taglist : Array Of Const);
FUNCTION MapChunkyArray(engine : POINTER; source : pByte; palette : POINTER; width : WORD; height : WORD; dest : pByte; const taglist : Array Of Const) : longword;
FUNCTION MapRGBArray(engine : POINTER; rgb : pULONG; width : WORD; height : WORD; chunky : pByte; const taglist : Array Of Const) : longword;
PROCEDURE MixAlphaChannel(source1 : pULONG; source2 : pULONG; width : WORD; height : WORD; dest : pULONG; const taglist : Array Of Const);
PROCEDURE MixRGBArray(sourcearray : pULONG; width : WORD; height : WORD; destarray : pULONG; ratio : WORD; const taglist : Array Of Const);
PROCEDURE Planar2Chunky(planetab : pPLANEPTR; bytewidth : WORD; height : WORD; depth : WORD; bytesperrow : WORD; chunky : pByte; const taglist : Array Of Const);
FUNCTION RenderTags(rgb : pULONG; width : WORD; height : WORD; chunky : pByte; palette : POINTER; const taglist : Array Of Const) : longword;
FUNCTION RGBArrayDiversity(rgb : pULONG; width : WORD; height : WORD; const taglist : Array Of Const) : LONGINT;
FUNCTION Scale(engine : POINTER; source : POINTER; dest : POINTER; const taglist : Array Of Const) : longword;
FUNCTION SortPalette(palette : POINTER; mode : longword; const taglist : Array Of Const) : longword;
PROCEDURE TintRGBArray(source : pULONG; width : WORD; height : WORD; RGB : longword; ratio : WORD; dest : pULONG; const taglist : Array Of Const);

IMPLEMENTATION

uses
  tagsarray;

{
 Functions and procedures with array of const go here
}
FUNCTION AddChunkyImage(histogram : POINTER; chunky : pByte; width : WORD; height : WORD; palette : POINTER; const taglist : Array Of Const) : longword;
begin
    AddChunkyImage := AddChunkyImageA(histogram , chunky , width , height , palette , readintags(taglist));
end;

FUNCTION AddHistogram(histogram1 : POINTER; histogram2 : POINTER; const taglist : Array Of Const) : longword;
begin
    AddHistogram := AddHistogramA(histogram1 , histogram2 , readintags(taglist));
end;

FUNCTION AddRGBImage(histogram : POINTER; rgb : pULONG; width : WORD; height : WORD; const taglist : Array Of Const) : longword;
begin
    AddRGBImage := AddRGBImageA(histogram , rgb , width , height , readintags(taglist));
end;

PROCEDURE ApplyAlphaChannel(sourcearray : pULONG; width : WORD; height : WORD; destarray : pULONG; const taglist : Array Of Const);
begin
    ApplyAlphaChannelA(sourcearray , width , height , destarray , readintags(taglist));
end;

PROCEDURE Chunky2BitMap(chunky : pByte; sx : WORD; sy : WORD; width : WORD; height : WORD; bitmap : pBitMap; dx : WORD; dy : WORD; const taglist : Array Of Const);
begin
    Chunky2BitMapA(chunky , sx , sy , width , height , bitmap , dx , dy , readintags(taglist));
end;

FUNCTION Chunky2RGB(chunky : pByte; width : WORD; height : WORD; rgb : pULONG; palette : POINTER; const taglist : Array Of Const) : longword;
begin
    Chunky2RGB := Chunky2RGBA(chunky , width , height , rgb , palette , readintags(taglist));
end;

FUNCTION ChunkyArrayDiversity(chunky : pByte; palette : POINTER; width : WORD; height : WORD; const taglist : Array Of Const) : LONGINT;
begin
    ChunkyArrayDiversity := ChunkyArrayDiversityA(chunky , palette , width , height , readintags(taglist));
end;

FUNCTION ConvertChunky(source : pByte; oldpalette : POINTER; width : WORD; height : WORD; dest : pByte; newpalette : POINTER; const taglist : Array Of Const) : longword;
begin
    ConvertChunky := ConvertChunkyA(source , oldpalette , width , height , dest , newpalette , readintags(taglist));
end;

PROCEDURE CreateAlphaArray(rgbarray : pULONG; width : WORD; height : WORD; const taglist : Array Of Const);
begin
    CreateAlphaArrayA(rgbarray , width , height , readintags(taglist));
end;

FUNCTION CreateHistogram(const taglist : Array Of Const) : POINTER;
begin
    CreateHistogram := CreateHistogramA(readintags(taglist));
end;

FUNCTION CreateMapEngine(palette : POINTER; const taglist : Array Of Const) : POINTER;
begin
    CreateMapEngine := CreateMapEngineA(palette , readintags(taglist));
end;

FUNCTION CreatePalette(const taglist : Array Of Const) : POINTER;
begin
    CreatePalette := CreatePaletteA(readintags(taglist));
end;

PROCEDURE CreatePenTable(chunky : pByte; oldpalette : POINTER; width : WORD; height : WORD; newpalette : POINTER; convtab : pByte; const taglist : Array Of Const);
begin
    CreatePenTableA(chunky , oldpalette , width , height , newpalette , convtab , readintags(taglist));
end;

FUNCTION CreateRMHandler(const taglist : Array Of Const) : POINTER;
begin
    CreateRMHandler := CreateRMHandlerA(readintags(taglist));
end;

FUNCTION CreateScaleEngine(sourcewidth : WORD; sourceheight : WORD; destwidth : WORD; destheight : WORD; const taglist : Array Of Const) : POINTER;
begin
    CreateScaleEngine := CreateScaleEngineA(sourcewidth , sourceheight , destwidth , destheight , readintags(taglist));
end;

PROCEDURE ExportPalette(palette : POINTER; coltab : POINTER; const taglist : Array Of Const);
begin
    ExportPaletteA(palette , coltab , readintags(taglist));
end;

PROCEDURE ExtractAlphaChannel(rgbarray : pULONG; width : WORD; height : WORD; chunkyarray : pByte; const taglist : Array Of Const);
begin
    ExtractAlphaChannelA(rgbarray , width , height , chunkyarray , readintags(taglist));
end;

FUNCTION ExtractPalette(histogram : POINTER; palette : pULONG; numcolors : WORD; const taglist : Array Of Const) : longword;
begin
    ExtractPalette := ExtractPaletteA(histogram , palette , numcolors , readintags(taglist));
end;

PROCEDURE ImportPalette(palette : POINTER; coltab : POINTER; numcols : WORD; const taglist : Array Of Const);
begin
    ImportPaletteA(palette , coltab , numcols , readintags(taglist));
end;

PROCEDURE InsertAlphaChannel(maskarray : pByte; width : WORD; height : WORD; rgbarray : pULONG; const taglist : Array Of Const);
begin
    InsertAlphaChannelA(maskarray , width , height , rgbarray , readintags(taglist));
end;

FUNCTION MapChunkyArray(engine : POINTER; source : pByte; palette : POINTER; width : WORD; height : WORD; dest : pByte; const taglist : Array Of Const) : longword;
begin
    MapChunkyArray := MapChunkyArrayA(engine , source , palette , width , height , dest , readintags(taglist));
end;

FUNCTION MapRGBArray(engine : POINTER; rgb : pULONG; width : WORD; height : WORD; chunky : pByte; const taglist : Array Of Const) : longword;
begin
    MapRGBArray := MapRGBArrayA(engine , rgb , width , height , chunky , readintags(taglist));
end;

PROCEDURE MixAlphaChannel(source1 : pULONG; source2 : pULONG; width : WORD; height : WORD; dest : pULONG; const taglist : Array Of Const);
begin
    MixAlphaChannelA(source1 , source2 , width , height , dest , readintags(taglist));
end;

PROCEDURE MixRGBArray(sourcearray : pULONG; width : WORD; height : WORD; destarray : pULONG; ratio : WORD; const taglist : Array Of Const);
begin
    MixRGBArrayA(sourcearray , width , height , destarray , ratio , readintags(taglist));
end;

PROCEDURE Planar2Chunky(planetab : pPLANEPTR; bytewidth : WORD; height : WORD; depth : WORD; bytesperrow : WORD; chunky : pByte; const taglist : Array Of Const);
begin
    Planar2ChunkyA(planetab , bytewidth , height , depth , bytesperrow , chunky , readintags(taglist));
end;

FUNCTION RenderTags(rgb : pULONG; width : WORD; height : WORD; chunky : pByte; palette : POINTER; const taglist : Array Of Const) : longword;
begin
    RenderTags := RenderA(rgb , width , height , chunky , palette , readintags(taglist));
end;

FUNCTION RGBArrayDiversity(rgb : pULONG; width : WORD; height : WORD; const taglist : Array Of Const) : LONGINT;
begin
    RGBArrayDiversity := RGBArrayDiversityA(rgb , width , height , readintags(taglist));
end;

FUNCTION Scale(engine : POINTER; source : POINTER; dest : POINTER; const taglist : Array Of Const) : longword;
begin
    Scale := ScaleA(engine , source , dest , readintags(taglist));
end;

FUNCTION SortPalette(palette : POINTER; mode : longword; const taglist : Array Of Const) : longword;
begin
    SortPalette := SortPaletteA(palette , mode , readintags(taglist));
end;

PROCEDURE TintRGBArray(source : pULONG; width : WORD; height : WORD; RGB : longword; ratio : WORD; dest : pULONG; const taglist : Array Of Const);
begin
    TintRGBArrayA(source , width , height , RGB , ratio , dest , readintags(taglist));
end;

const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

initialization
  RenderBase := OpenLibrary(RENDERNAME,LIBVERSION);
finalization
  if Assigned(RenderBase) then
    CloseLibrary(RenderBase);
END. (* UNIT RENDER *)



