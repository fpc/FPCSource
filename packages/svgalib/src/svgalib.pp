unit SVGALib;

{*************************************************************************
  VGAlib version 1.2 - (c) 1993 Tommy Frandsen

  This library is free software; you can redistribute it and/or
  modify it without any restrictions. This library is distributed
  in the hope that it will be useful, but without any warranty.

  Extended for svgalib by Harm Hanemaayer and Hartmut Schirmer
 ************************************************************************

  Original conversion to FPK-Pascal done by Balazs Scheidler (bazsi@tas.vein.hu)
  08/29/1997

  Modified by Matthias K"oppe <mkoeppe@csmd.cs.uni-magdeburg.de>, 21-Sep-1997
  The compiler (FPK 0.9.1) seems to have a problem with external procedures
  having void argument list (wrong number of arguments is taken from stack.)
  Therefore, I added dummy arguments to all these procedures. Moreover, I
  added `vgagl' support.

  Modified by Michael Van Canneyt (michael@tfdec1.fys.kuleuven.ac.be) to
  support version 0.99.5 of the compiler. (March 1998) Removed the void replacements.
  Added makefile and 2 testprograms. Tested with svgalib 1.2.12.

  ************************************************************************}

interface

{ As of version 0.99.0, you can specify libs to link to in the source file }
{ Make sure you have those libs. MVC }

{$linklib vga}
{$linklib vgagl}
{$linklib c}

const
  GTEXT             = 0;                { Compatible with VGAlib v1.2 }
  G320x200x16       = 1;
  G640x200x16       = 2;
  G640x350x16       = 3;
  G640x480x16       = 4;
  G320x200x256      = 5;
  G320x240x256      = 6;
  G320x400x256      = 7;
  G360x480x256      = 8;
  G640x480x2        = 9;

  G640x480x256      = 10;
  G800x600x256      = 11;
  G1024x768x256     = 12;

  G1280x1024x256    = 13;   { Additional modes. }

  G320x200x32K      = 14;
  G320x200x64K      = 15;
  G320x200x16M      = 16;
  G640x480x32K      = 17;
  G640x480x64K      = 18;
  G640x480x16M      = 19;
  G800x600x32K      = 20;
  G800x600x64K      = 21;
  G800x600x16M      = 22;
  G1024x768x32K     = 23;
  G1024x768x64K     = 24;
  G1024x768x16M     = 25;
  G1280x1024x32K    = 26;
  G1280x1024x64K    = 27;
  G1280x1024x16M    = 28;

  G800x600x16       = 29;
  G1024x768x16      = 30;
  G1280x1024x16     = 31;

  G720x348x2        = 32;               { Hercules emulation mode }

  G320x200x16M32    = 33;       { 32-bit per pixel modes. }
  G640x480x16M32    = 34;
  G800x600x16M32    = 35;
  G1024x768x16M32   = 36;
  G1280x1024x16M32  = 37;

{ additional resolutions }
  G1152x864x16      = 38;
  G1152x864x256     = 39;
  G1152x864x32K     = 40;
  G1152x864x64K     = 41;
  G1152x864x16M     = 42;
  G1152x864x16M32   = 43;

  G1600x1200x16     = 44;
  G1600x1200x256    = 45;
  G1600x1200x32K    = 46;
  G1600x1200x64K    = 47;
  G1600x1200x16M    = 48;
  G1600x1200x16M32  = 49;

  GLASTMODE         = 49;

{ Extensions to VGAlib v1.2: }

{ blit flags }
const
  HAVE_BITBLIT          = 1;
  HAVE_FILLBLIT         = 2;
  HAVE_IMAGEBLIT        = 4;
  HAVE_HLINELISTBLIT    = 8;
  HAVE_BLITWAIT         = 16;

{ other flags }
  HAVE_RWPAGE           = 1;            { vga_setreadpage() / vga_setwritepage() available }
  IS_INTERLACED         = 2;            { mode is interlaced }
  IS_MODEX              = 4;            { ModeX style 256 colors }
  IS_DYNAMICMODE        = 8;            { Dynamic defined mode }
  CAPABLE_LINEAR        = 16;           { Can go to linear addressing mode. }
  IS_LINEAR             = 32;           { Linear addressing enabled. }
  EXT_INFO_AVAILABLE    = 64;           { Returned modeinfo contains valid extended fields }
  RGB_MISORDERED        = 128;          { Mach32 32bpp uses 0BGR instead of BGR0. }
    { As of this version 1.25 also used to signal if real RGB
       (red first in memory) is used instead of BGR (Mach32 DAC 4) }
  HAVE_EXT_SET          = 256;          { vga_ext_set() available }

{ Valid values for what in vga_ext_set: }
const
  VGA_EXT_AVAILABLE   = 0;      { supported flags }
  VGA_EXT_SET         = 1;      { set flag(s) }
  VGA_EXT_CLEAR       = 2;      { clear flag(s) }
  VGA_EXT_RESET       = 3;      { set/clear flag(s) }
  VGA_EXT_PAGE_OFFSET = 4;      { set an offset for all subsequent vga_set*page() calls }
    { Like: vga_ext_set(VGA_EXT_PAGE_OFFSET, 42);           }
    { returns the previous offset value.                    }

 { Valid params for VGA_EXT_AVAILABLE: }
  VGA_AVAIL_SET        = 0;     { vga_ext_set sub funcs }
  VGA_AVAIL_ACCEL      = 1;     { vga_accel sub funcs }
  VGA_AVAIL_FLAGS      = 2;     { known flags for VGA_EXT_SET }

 { Known flags to vga_ext_set() }
  VGA_CLUT8            = 1;     { 8 bit DAC entries }

  { Acceleration interface. }

  { Accel operations. }
  ACCEL_FILLBOX            = 1; { Simple solid fill. }
  ACCEL_SCREENCOPY         = 2; { Simple screen-to-screen BLT. }
  ACCEL_PUTIMAGE           = 3; { Straight image transfer. }
  ACCEL_DRAWLINE           = 4; { General line draw. }
  ACCEL_SETFGCOLOR         = 5; { Set foreground color. }
  ACCEL_SETBGCOLOR         = 6; { Set background color. }
  ACCEL_SETTRANSPARENCY    = 7; { Set transparency mode. }
  ACCEL_SETRASTEROP        = 8; { Set raster-operation. }
  ACCEL_PUTBITMAP          = 9; { Color-expand bitmap. }
  ACCEL_SCREENCOPYBITMAP   = 10;        { Color-exand from screen. }
  ACCEL_DRAWHLINELIST      = 11;        { Draw horizontal spans. }
  ACCEL_SETMODE            = 12;        { Set blit strategy. }
  ACCEL_SYNC               = 13;        { Wait for blits to finish. }

  { Corresponding bitmask. }
  ACCELFLAG_FILLBOX           = $1;     { Simple solid fill. }
  ACCELFLAG_SCREENCOPY        = $2;     { Simple screen-to-screen BLT. }
  ACCELFLAG_PUTIMAGE          = $4;     { Straight image transfer. }
  ACCELFLAG_DRAWLINE          = $8;     { General line draw. }
  ACCELFLAG_SETFGCOLOR        = $10;    { Set foreground color. }
  ACCELFLAG_SETBGCOLOR        = $20;    { Set background color. }
  ACCELFLAG_SETTRANSPARENCY   = $40;    { Set transparency mode. }
  ACCELFLAG_SETRASTEROP       = $80;    { Set raster-operation. }
  ACCELFLAG_PUTBITMAP         = $100;   { Color-expand bitmap. }
  ACCELFLAG_SCREENCOPYBITMAP  = $200;   { Color-exand from screen. }
  ACCELFLAG_DRAWHLINELIST     = $400;   { Draw horizontal spans. }
  ACCELFLAG_SETMODE           = $800;   { Set blit strategy. }
  ACCELFLAG_SYNC              = $1000;  { Wait for blits to finish. }

  { Mode for SetTransparency. }
  DISABLE_TRANSPARENCY_COLOR  =  0;
  ENABLE_TRANSPARENCY_COLOR   =  1;
  DISABLE_BITMAP_TRANSPARENCY =  2;
  ENABLE_BITMAP_TRANSPARENCY  =  3;

  { Flags for SetMode (accelerator interface). }
  BLITS_SYNC                  =  0;
  BLITS_IN_BACKGROUND         =  $1;

  { Raster ops. }
  ROP_COPY                    =  0;     { Straight copy. }
  ROP_OR                      =  1;     { Source OR destination. }
  ROP_AND                     =  2;     { Source AND destination. }
  ROP_XOR                     =  3;     { Source XOR destination. }
  ROP_INVERT                  =  4;     { Invert destination. }

const
  { chipsets }
  UNDEFINED             = 0;
  VGA                   = 1;
  ET4000                = 2;
  CIRRUS                = 3;
  TVGA8900              = 4;
  OAK                   = 5;
  EGA                   = 6;
  S3                    = 7;
  ET3000                = 8;
  MACH32                = 9;
  GVGA6400              = 10;
  ARK                   = 11;
  ATI                   = 12;
  ALI                   = 13;
  MACH64                = 14;

    { Hor. sync: }
  MON640_60             = 0;    { 31.5 KHz (standard VGA) }
  MON800_56             = 1;    { 35.1 KHz (old SVGA) }
  MON1024_43I           = 2;    { 35.5 KHz (low-end SVGA, 8514) }
  MON800_60             = 3;    { 37.9 KHz (SVGA) }
  MON1024_60            = 4;    { 48.3 KHz (SVGA non-interlaced) }
  MON1024_70            = 5;    { 56.0 KHz (SVGA high frequency) }
  MON1024_72            = 6;

{
 * valid values for what ( | is valid to combine them )
   (vga_waitevent values)
 }
  VGA_MOUSEEVENT        = 1;
  VGA_KEYEVENT          = 2;


type
  pvga_modeinfo = ^vga_modeinfo;
  vga_modeinfo = record
    width,
    height,
    bytesperpixel,
    colors,
    linewidth,          { scanline width in bytes }
    maxlogicalwidth,    { maximum logical scanline width }
    startaddressrange,  { changeable bits set }
    maxpixels,          { video memory / bytesperpixel }
    haveblit,           { mask of blit functions available }
    flags: Longint;     { other flags }

    { Extended fields: }

    chiptype,           { Chiptype detected }
    memory,             { videomemory in KB }
    linewidth_unit: Longint;    { Use only a multiple of this as parameter for set_logicalwidth and
                                  set_displaystart }
    linear_aperture: PChar;     { points to mmap secondary mem aperture of card (NULL if unavailable) }
    aperture_size: Longint;     { size of aperture in KB if size>=videomemory. 0 if unavail }
    set_aperture_page: procedure (page: Longint);
    { if aperture_size<videomemory select a memory page }
    extensions: Pointer;        { points to copy of eeprom for mach32 }
                                { depends from actual driver/chiptype.. etc. }
  end;
  shutdown_routine_ptr = procedure;

var
  graph_mem: PChar;

Function vga_setmode(mode: Longint): Longint; cdecl; external;
Function vga_hasmode(mode: Longint): Boolean; cdecl; external;
Function vga_setflipchar(c: Longint): Longint; cdecl; external;

Function vga_clear: Longint; cdecl; external;
Function vga_flip: Longint; cdecl; external;

Function vga_getxdim: Longint; cdecl; external;
Function vga_getydim: Longint; cdecl; external;
Function vga_getcolors: Longint; cdecl; external;

Function vga_setpalette(index: Longint; red: Longint; green: Longint; blue: Longint): Longint; cdecl; external;
Function vga_getpalette(index: Longint; var red: Longint; var green: Longint; var blue: Longint): Longint; cdecl; external;
Function vga_setpalvec(start: Longint; num: Longint; var pal): Longint; cdecl; external;
Function vga_getpalvec(start: Longint; num: Longint; var pal): Longint; cdecl; external;

Function vga_screenoff: Longint; cdecl; external;
Function vga_screenon: Longint; cdecl; external;

Function vga_setcolor(color: Longint): Longint; cdecl; external;
Function vga_drawpixel(x, y: Longint): Longint; cdecl; external;
Function vga_drawline(x1, y1, x2, y2: Longint): Longint; cdecl; external;
Function vga_drawscanline(line: Longint; var colors): Longint; cdecl; external;
Function vga_drawscansegment(var colors; x, y: Longint; length: Longint): Longint; cdecl; external;
Function vga_getpixel(x, y: Longint): Longint; cdecl; external;
Function vga_getscansegment(var colors; x, y: Longint; length: Longint): Longint; cdecl; external;

Function vga_getch: Longint; cdecl; external;

Function vga_dumpregs: Longint; cdecl; external;


Function vga_getmodeinfo(mode: Longint): pvga_modeinfo; cdecl; external;
Function vga_getdefaultmode: Longint; cdecl; external;
Function vga_getcurrentmode: Longint; cdecl; external;
Function vga_getcurrentchipset: Longint; cdecl; external;
Function vga_getmodename(mode: Longint): PChar; cdecl; external;
Function vga_getmodenumber(name: PChar): Longint; cdecl; external;
Function vga_lastmodenumber: Longint; cdecl; external;

Function vga_getgraphmem: PChar; cdecl; external;

Procedure vga_setpage(p: Longint); cdecl; external;
Procedure vga_setreadpage(p: Longint); cdecl; external;
Procedure vga_setwritepage(p: Longint); cdecl; external;
Procedure vga_setlogicalwidth(w: Longint); cdecl; external;
Procedure vga_setdisplaystart(a: Longint); cdecl; external;
Procedure vga_waitretrace; cdecl; external;
Function vga_claimvideomemory(n: Longint): Longint; cdecl; external;
Procedure vga_disabledriverreport; cdecl; external;
Function vga_setmodeX: Longint; cdecl; external;
Function vga_init: Longint;     { Used to return void in svgalib <= 1.12. } cdecl; external;
Function vga_getmousetype: Longint; cdecl; external;
Function vga_getmonitortype: Longint; cdecl; external;
Procedure vga_setmousesupport(s: Longint); cdecl; external;
Procedure vga_lockvc; cdecl; external;
Procedure vga_unlockvc; cdecl; external;
Function vga_getkey: Longint; cdecl; external;
Procedure vga_runinbackground(s: Longint); cdecl; external;
Function vga_oktowrite: Longint; cdecl; external;
Procedure vga_copytoplanar256(virtualp: PChar; pitch: Longint;
                                  voffset: Longint; vpitch: Longint; w: Longint; h: Longint);cdecl; external;
Procedure vga_copytoplanar16(virtualp: PChar; pitch, voffset, vpitch, w, h: Longint); cdecl; external;
Procedure vga_copytoplane(virtualp: PChar; pitch, voffset, vpitch, w, h, plane: Longint); cdecl; external;
Function vga_setlinearaddressing: Longint; cdecl; external;
Procedure vga_safety_fork(shutdown_routine: shutdown_routine_ptr); cdecl; external;

Procedure vga_setchipset(c: Longint); cdecl; external;
Procedure vga_setchipsetandfeatures(c: Longint; par1: Longint; par2: Longint); cdecl; external;
Procedure vga_gettextfont(font: Pointer); cdecl; external;
Procedure vga_puttextfont(font: Pointer); cdecl; external;
Procedure vga_settextmoderegs(regs: Pointer); cdecl; external;
Procedure vga_gettextmoderegs(regs: Pointer); cdecl; external;

Function vga_white: Longint; cdecl; external;
Function vga_setegacolor(c: Longint): Longint; cdecl; external;
Function vga_setrgbcolor(r, g, b: Longint): Longint; cdecl; external;

Procedure vga_bitblt(srcaddr, destaddr: Longint; w, h: Longint; pitch: Longint); cdecl; external;
Procedure vga_imageblt(srcaddr: Pointer; destaddr: Longint; w, h, pitch: Longint); cdecl; external;
Procedure vga_fillblt(destaddr: Longint; w, h, pitch, c: Longint); cdecl; external;
Procedure vga_hlinelistblt(ymin, n: Longint; var xmin, xmax: Longint; pitch, c: Longint); cdecl; external;
Procedure vga_blitwait; cdecl; external;

{
  The following Functions are not implemented, because they use variable number cdecl; external;
  of arguments
int vga_ext_set(unsigned what,...);
int vga_accel(unsigned operation,...);
}

{
 * wait for keypress, mousemove, I/O, timeout. cf. select (3) for details on
 * all parameters execept which.
 * NULL is a valid argument for any of the ptrs.
 }

type
  fd_set = array [1..32] of Longint;
  timeval = record
    tv_sec, tv_usec: Longint;
  end;

Function vga_waitevent(which: Longint; var inp, outp, excp: fd_set;
                             timeout: timeval): Longint;cdecl; external;

{
 * return value >= has bits set for mouse/keyboard events detected.
 * mouse and raw keyboard events are already handled and their bits removed
 * from *in when vga_waitevent returns.
 * VGA_KEYEVENT relates to vga_getch NOT vga_getkey.
 * return values < 0 signal errors. In this case check errno.
 }

{ This part has been added by Matthias K"oppe <mkoeppe@csmd.cs.uni-magdeburg.de>.
 It reflects the content of `vgagl.h'.
 }

const
  CONTEXT_VIRTUAL  = 0;
  CONTEXT_PAGED    = 1;
  CONTEXT_LINEAR   = 2;
  CONTEXT_MODEX    = 3;
  CONTEXT_PLANAR16 = 4;

const
  MODEFLAG_PAGEFLIPPING_CAPABLE    = $01;
  MODEFLAG_TRIPLEBUFFERING_CAPABLE = $02;
  MODEFLAG_PAGEFLIPPING_ENABLED    = $04;
  MODEFLAG_TRIPLEBUFFERING_ENABLED = $08;
  MODEFLAG_FLIPPAGE_BANKALIGNED    = $10;
  MODEFLAG_32BPP_SHIFT8            = $20;
  MODEFLAG_24BPP_REVERSED          = $20;

type
  PGraphicsContext = ^TGraphicsContext;
  TGraphicsContext = record
                       ModeType: Byte;
                       ModeFlags: Byte;
                       Dummy: Byte;
                       FlipPage: Byte;
                       Width: LongInt;
                       Height: LongInt;
                       BytesPerPixel: LongInt;
                       Colors: LongInt;
                       BitsPerPixel: LongInt;
                       ByteWidth: LongInt;
                       VBuf: pointer;
                       Clip: LongInt;
                       ClipX1: LongInt;
                       ClipY1: LongInt;
                       ClipX2: LongInt;
                       ClipY2: LongInt;
                       ff: pointer;
                     end;

{ Configuration
}
Function gl_setcontextvga(m: LongInt): LongInt; cdecl; external;
Function gl_setcontextvgavirtual(m: LongInt): LongInt; cdecl; external;
Procedure gl_setcontextvirtual(w, h, bpp, bitspp: LongInt; vbuf: pointer); cdecl; external;
Procedure gl_setcontextwidth(w: LongInt); cdecl; external;
Procedure gl_setcontextheight(h: LongInt); cdecl; external;
Function gl_allocatecontext: PGraphicsContext; cdecl; external;
Procedure gl_setcontext(gc: PGraphicsContext); cdecl; external;
Procedure gl_getcontext(gc: PGraphicsContext); cdecl; external;
Procedure gl_freecontext(gc: PGraphicsContext); cdecl; external;

{ Line drawing
}
Procedure gl_setpixel(x, y, c: LongInt); cdecl; external;
Procedure gl_setpixelrgb(x, y, r, g, b: LongInt); cdecl; external;
Function gl_getpixel(x, y: LongInt): LongInt; cdecl; external;
Procedure gl_getpixelrgb(x, y: LongInt; var r, g, b: LongInt); cdecl; external;
Function gl_rgbcolor(r, g, b: LongInt): LongInt; cdecl; external;
Procedure gl_hline(x1, y, x2, c: LongInt); cdecl; external;
Procedure gl_line(x1, y1, x2, y2, c: LongInt); cdecl; external;
Procedure gl_circle(x, y, r, c: LongInt ); cdecl; external;

{ Box (bitmap) Functions cdecl; external;
}
Procedure gl_fillbox(x, y, w, h, c: LongInt); cdecl; external;
Procedure gl_getbox(x, y, w, h: LongInt; dp: pointer); cdecl; external;
Procedure gl_putbox(x, y, w, h: LongInt; dp: pointer); cdecl; external;
Procedure gl_putboxpart(x, y, w, h, bw, bh: LongInt; b: pointer; xo, yo: LongInt); cdecl; external;
Procedure gl_putboxmask(x, y, w, h: LongInt; dp: pointer); cdecl; external;
Procedure gl_copybox(x1, y1, w, h, x2, y2: LongInt); cdecl; external;
Procedure gl_copyboxtocontext(x1, y1, w, h: LongInt; var gc: TGraphicsContext; x2, y2: LongInt); cdecl; external;
Procedure gl_copyboxfromcontext(var gc: TGraphicsContext; x1, y1, w, h, x2, y2: LongInt); cdecl; external;
{ The following Functions only work in 256-color modes: }
Procedure gl_compileboxmask(w, h: LongInt; sdp, dpp: pointer); cdecl; external;
Function gl_compiledboxmasksize(w, h: LongInt; sdp: pointer): LongInt; cdecl; external;
Procedure gl_putboxmaskcompiled(x, y, w, h: LongInt; dp: pointer); cdecl; external;

{ Miscellaneous
 }

Procedure gl_clearscreen(c: LongInt); cdecl; external;
Procedure gl_scalebox(w1, h1: LongInt; sb: pointer; w2, h2: LongInt; db: pointer); cdecl; external;
Procedure gl_setdisplaystart(x, y: LongInt); cdecl; external;
Procedure gl_enableclipping; cdecl; external;
Procedure gl_setclippingwindow(x1, y1, x2, y2: LongInt); cdecl; external;
Procedure gl_disableclipping; cdecl; external;

{ Screen buffering
}

Procedure gl_copyscreen(var gc: TGraphicsContext); cdecl; external;
Procedure gl_setscreenoffset(o: LongInt); cdecl; external;
Function gl_enablepageflipping(var gc: TGraphicsContext): LongInt; cdecl; external;

{ Text
 }

{ Writemode flags.
 }
const
  WRITEMODE_OVERWRITE = 0;
  WRITEMODE_MASKED    = 1;
  FONT_EXPANDED       = 0;
  FONT_COMPRESSED     = 2;

Procedure gl_expandfont(fw, fh, c: LongInt; sfdp, dfdp: pointer); cdecl; external;
Procedure gl_setfont(fw, fh: LongInt; fdp: pointer); cdecl; external;
Procedure gl_colorfont(fw, fh, c: LongInt; fdp: pointer); cdecl; external;
Procedure gl_setwritemode(wm: LongInt); cdecl; external;
Procedure gl_write(x, y: LongInt; s: PChar); cdecl; external;
Procedure gl_writen(x, y, n: LongInt; s: PChar); cdecl; external;
Procedure gl_setfontcolors(bg, fg: LongInt); cdecl; external;

{extern unsigned char *gl_font8x8;      /* compressed 8x8 font */}

type
  TRGB = record
           red, green, blue: Byte
         end;
type
  TRGBPalette = array[0..255] of TRGB;

Procedure gl_setpalettecolor(c, r, b, g: LongInt); cdecl; external;
Procedure gl_getpalettecolor(c: LongInt; var r, b, g: LongInt); cdecl; external;
Procedure gl_setpalettecolors(s, n: LongInt; dp: pointer); cdecl; external;
Procedure gl_getpalettecolors(s, n: LongInt; dp: pointer); cdecl; external;
Procedure gl_setpalette(p: pointer); cdecl; external;
Procedure gl_getpalette(p: pointer); cdecl; external;
Procedure gl_setrgbpalette; cdecl; external;

Procedure gl_font8x8; { ACTUALLY A DATA POINTER } cdecl; external;

implementation


end.
