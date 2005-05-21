{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of
    the Free Pascal development team

    This file implements an interface to the gd library.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit gd;

  interface

{$linklib c}
{$linklib m}
{$linklib png}
{$linklib gd}
{$ifdef hasttf}
{$linklib ttf}
{$endif hasttf}

{$PACKRECORDS C}

const

  libgd = 'gd';
  libc = 'c';

  GD_H = 1;
  DEFAULT_FONTPATH = '/usr/share/fonts/truetype';
  PATHSEPARATOR = ':';
  gdMaxColors = 256;

type
  PByte = ^Byte;
  PPByte = ^PByte;
  PVoid = Pointer;
  PFile = Pointer;

  PGDIMAGESTRUCT = ^gdImage;
  gdImage = record
    pixels : PPbyte;
    sx : longint;
    sy : longint;
    colorsTotal : longint;
    red : array[0..(gdMaxColors)-1] of longint;
    green : array[0..(gdMaxColors)-1] of longint;
    blue : array[0..(gdMaxColors)-1] of longint;
    open : array[0..(gdMaxColors)-1] of longint;
    transparent : longint;
    polyInts : Plongint;
    polyAllocated : longint;
    brush : PgdImageStruct;
    tile : PgdImageStruct;
    brushColorMap : array[0..(gdMaxColors)-1] of longint;
    tileColorMap : array[0..(gdMaxColors)-1] of longint;
    styleLength : longint;
    stylePos : longint;
    style : Plongint;
    interlace : longint;
  end;

  gdImageStruct = gdImage;
  pgdImage = ^gdImage;
  gdImagePtr = PgdImage;

  gdFont = record
       nchars : longint;
       offset : longint;
       w : longint;
       h : longint;
       data : Pchar;
    end;
  pgdFont = ^gdFont;
  gdFontPtr = PgdFont;

var
{$ifndef darwin}
  gdFontLarge      : gdFontPtr; cvar; external;
  gdFontSmall      : gdFontPtr; cvar; external;
  gdFontGiant      : gdFontPtr; cvar; external;
  gdFontMediumBold : gdFontPtr; cvar; external;
  gdFontTiny       : gdFontPtr; cvar; external;
{$else darwin}
  gdFontLarge      : gdFontPtr; external libgd name 'gdFontLarge';
  gdFontSmall      : gdFontPtr; external libgd name 'gdFontSmall';
  gdFontGiant      : gdFontPtr; external libgd name 'gdFontGiant';
  gdFontMediumBold : gdFontPtr; external libgd name 'gdFontMediumBold';
  gdFontTiny       : gdFontPtr; external libgd name 'gdFontTiny';
{$endif darwin}

const
  gdDashSize = 4;
  gdStyled = -(2);
  gdBrushed = -(3);
  gdStyledBrushed = -(4);
  gdTiled = -(5);
  gdTransparent = -(6);

type

  gdSource = record
    source : function (context:Pointer; buffer:Pchar; len:longint):longint;cdecl;
    context : pointer;
    end;
  pgdSource = ^gdSource;
  gdSourcePtr = PgdSource;

  gdSink = record
    sink : function (context:Pvoid; buffer:Pchar; len:longint):longint;cdecl;
    context : pointer;
  end;
  pgdSink = ^gdSink;
  gdSinkPtr = PgdSink;

  gdPoint = record
    x : longint;
    y : longint;
  end;
  pgdPoint = ^gdPoint;
  gdPointPtr = PgdPoint;

  pgdIOCtx = ^gdIOCtx;
  gdIOCtx = record
    getC : function (_para1:PgdIOCtx):longint;cdecl;
    getBuf : function (_para1:PgdIOCtx; _para2:pointer; _para3:longint):longint;
    putC : procedure (_para1:PgdIOCtx; _para2:longint);
    putBuf : function (_para1:PgdIOCtx; _para2:pointer; _para3:longint):longint;
    seek : function (_para1:PgdIOCtx; _para2:longint):longint;
    tell : function (_para1:PgdIOCtx):longint;
    free : procedure (_para1:PgdIOCtx);
  end;
  GDIOCTXPTR = pgdIOCtx;

{ Translated from gd_io.h}
function fopen(a,b:pchar):pFile; cdecl;external libc;
procedure fclose(a:pFile); cdecl;external libc;

procedure Putword(w:longint; ctx:PgdIOCtx); cdecl; external libgd;
procedure Putchar(c:longint; ctx:PgdIOCtx); cdecl; external libgd;
procedure gdPutC(c:byte; ctx:PgdIOCtx); cdecl; external libgd;
function gdPutBuf(_para1:pointer; _para2:longint; _para3:PgdIOCtx):longint; cdecl; external libgd;
procedure gdPutWord(w:longint; ctx:PgdIOCtx); cdecl; external libgd;
procedure gdPutInt(w:longint; ctx:PgdIOCtx); cdecl; external libgd;
function gdGetC(ctx:PgdIOCtx):longint; cdecl; external libgd;
function gdGetBuf(_para1:pointer; _para2:longint; _para3:PgdIOCtx):longint; cdecl; external libgd;
function gdGetByte(result:Plongint; ctx:PgdIOCtx):longint; cdecl; external libgd;
function gdGetWord(result:Plongint; ctx:PgdIOCtx):longint; cdecl; external libgd;
function gdGetInt(result:Plongint; ctx:PgdIOCtx):longint; cdecl; external libgd;
function gdSeek(ctx:PgdIOCtx; _para2:longint):longint; cdecl; external libgd;
function gdTell(ctx:PgdIOCtx):longint; cdecl; external libgd;

function gdImageCreate(sx:longint; sy:longint):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromPng(fd:PFILE):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromPngCtx(inIO:gdIOCtxPtr):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromWBMP(inFile:PFILE):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromWBMPCtx(infile:PgdIOCtx):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromJpeg(infile:PFILE):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromJpegCtx(infile:PgdIOCtx):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromPngSource(infile:gdSourcePtr):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromGd(infile:PFILE):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromGdCtx(infile:gdIOCtxPtr):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromGd2(infile:PFILE):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromGd2Ctx(infile:gdIOCtxPtr):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromGd2Part(infile:PFILE; srcx:longint; srcy:longint; w:longint; h:longint):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromGd2PartCtx(infile:gdIOCtxPtr; srcx:longint; srcy:longint; w:longint; h:longint):gdImagePtr; cdecl; external libgd;
function gdImageCreateFromXbm(fd:PFILE):gdImagePtr; cdecl; external libgd;
procedure gdImageDestroy(im:gdImagePtr); cdecl; external libgd;
procedure gdImageSetPixel(im:gdImagePtr; x:longint; y:longint; color:longint); cdecl; external libgd;
function gdImageGetPixel(im:gdImagePtr; x:longint; y:longint):longint; cdecl; external libgd;
procedure gdImageLine(im:gdImagePtr; x1:longint; y1:longint; x2:longint; y2:longint;  color:longint); cdecl; external libgd;
procedure gdImageDashedLine(im:gdImagePtr; x1:longint; y1:longint; x2:longint; y2:longint;               color:longint); cdecl; external libgd;
procedure gdImageRectangle(im:gdImagePtr; x1:longint; y1:longint; x2:longint; y2:longint;              color:longint); cdecl; external libgd;
procedure gdImageFilledRectangle(im:gdImagePtr; x1:longint; y1:longint; x2:longint; y2:longint;               color:longint); cdecl; external libgd;
function gdImageBoundsSafe(im:gdImagePtr; x:longint; y:longint):longint; cdecl; external libgd;
procedure gdImageChar(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; c:longint; color:longint); cdecl; external libgd;
procedure gdImageCharUp(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; c:longint; color:longint); cdecl; external libgd;
procedure gdImageString(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:Pbyte;  color:longint); cdecl; external libgd;
procedure gdImageStringUp(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:Pbyte; color:longint); cdecl; external libgd;
procedure gdImageString16(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:Pword; color:longint); cdecl; external libgd;
procedure gdImageStringUp16(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:Pword; color:longint); cdecl; external libgd;
{$ifdef HasTTF}
function gdImageStringTTF(im:PgdImage; brect:Plongint; fg:longint; fontlist:Pchar; ptsize:double; angle:double; x:longint; y:longint; astring:Pchar):Pchar; cdecl; external libgd;
function gdImageStringFT(im:PgdImage; brect:Plongint; fg:longint; fontlist:Pchar; ptsize:double; angle:double; x:longint; y:longint; astring:Pchar):Pchar; cdecl; external libgd;
{$endif hasTTF}
procedure gdImagePolygon(im:gdImagePtr; p:gdPointPtr; n:longint; c:longint); cdecl; external libgd;
procedure gdImageFilledPolygon(im:gdImagePtr; p:gdPointPtr; n:longint; c:longint); cdecl; external libgd;
function gdImageColorAllocate(im:gdImagePtr; r:longint; g:longint; b:longint):longint; cdecl; external libgd;
function gdImageColorClosest(im:gdImagePtr; r:longint; g:longint; b:longint):longint; cdecl; external libgd;
function gdImageColorExact(im:gdImagePtr; r:longint; g:longint; b:longint):longint; cdecl; external libgd;
function gdImageColorResolve(im:gdImagePtr; r:longint; g:longint; b:longint):longint; cdecl; external libgd;
procedure gdImageColorDeallocate(im:gdImagePtr; color:longint); cdecl; external libgd;
procedure gdImageColorTransparent(im:gdImagePtr; color:longint); cdecl; external libgd;
procedure gdImagePaletteCopy(dst:gdImagePtr; src:gdImagePtr); cdecl; external libgd;
procedure gdImagePng(im:gdImagePtr; _out:PFILE); cdecl; external libgd;
procedure gdImagePngCtx(im:gdImagePtr; _out:PgdIOCtx); cdecl; external libgd;
procedure gdImageWBMP(image:gdImagePtr; fg:longint; _out:PFILE); cdecl; external libgd;
procedure gdImageWBMPCtx(image:gdImagePtr; fg:longint; _out:PgdIOCtx); cdecl; external libgd;
procedure gdFree(m:Pvoid); cdecl; external libgd;
function gdImageWBMPPtr(im:gdImagePtr; size:Plongint; fg:longint):pointer; cdecl; external libgd;
procedure gdImageJpeg(im:gdImagePtr; _out:PFILE; quality:longint); cdecl; external libgd;
procedure gdImageJpegCtx(im:gdImagePtr; _out:PgdIOCtx; quality:longint); cdecl; external libgd;
function gdImageJpegPtr(im:gdImagePtr; size:Plongint; quality:longint):pointer; cdecl; external libgd;
procedure gdImagePngToSink(im:gdImagePtr; _out:gdSinkPtr); cdecl; external libgd;
procedure gdImageGd(im:gdImagePtr; _out:PFILE); cdecl; external libgd;
procedure gdImageGd2(im:gdImagePtr; _out:PFILE; cs:longint; fmt:longint); cdecl; external libgd;
function gdImagePngPtr(im:gdImagePtr; size:Plongint):pointer; cdecl; external libgd;
function gdImageGdPtr(im:gdImagePtr; size:Plongint):pointer; cdecl; external libgd;
function gdImageGd2Ptr(im:gdImagePtr; cs:longint; fmt:longint; size:Plongint):pointer; cdecl; external libgd;
procedure gdImageArc(im:gdImagePtr; cx:longint; cy:longint; w:longint; h:longint;  s:longint; e:longint; color:longint); cdecl; external libgd;
procedure gdImageFillToBorder(im:gdImagePtr; x:longint; y:longint; border:longint; color:longint); cdecl; external libgd;
procedure gdImageFill(im:gdImagePtr; x:longint; y:longint; color:longint); cdecl; external libgd;
procedure gdImageCopy(dst:gdImagePtr; src:gdImagePtr; dstX:longint; dstY:longint; srcX:longint;             srcY:longint; w:longint; h:longint); cdecl; external libgd;
procedure gdImageCopyMerge(dst:gdImagePtr; src:gdImagePtr; dstX:longint; dstY:longint; srcX:longint;              srcY:longint; w:longint; h:longint; pct:longint); cdecl; external libgd;
procedure gdImageCopyMergeGray(dst:gdImagePtr; src:gdImagePtr; dstX:longint; dstY:longint; srcX:longint;              srcY:longint; w:longint; h:longint; pct:longint); cdecl; external libgd;
procedure gdImageCopyResized(dst:gdImagePtr; src:gdImagePtr; dstX:longint; dstY:longint; srcX:longint;               srcY:longint; dstW:longint; dstH:longint; srcW:longint; srcH:longint); cdecl; external libgd;
procedure gdImageSetBrush(im:gdImagePtr; brush:gdImagePtr); cdecl; external libgd;
procedure gdImageSetTile(im:gdImagePtr; tile:gdImagePtr); cdecl; external libgd;
procedure gdImageSetStyle(im:gdImagePtr; style:Plongint; noOfPixels:longint); cdecl; external libgd;
procedure gdImageInterlace(im:gdImagePtr; interlaceArg:longint); cdecl; external libgd;

{ Translated macros }
function gdImageSX(im : pgdimage) : longint;
function gdImageSY(im : pgdimage) : longint;
function gdImageColorsTotal(im : pgdimage) : longint;
function gdImageRed(im : pgdimage; c : longint): longint;
function gdImageGreen(im : pgdimage; c : longint): longint;
function gdImageBlue(im : pgdimage; c : longint): longint;
function gdImageGetTransparent(im : pgdimage) : longint;
function gdImageGetInterlaced(im : pgdimage) : longint;

function gdNewFileCtx(_para1:PFILE):PgdIOCtx; cdecl; external libgd;
function gdNewDynamicCtx(_para1:longint; _para2:pointer):PgdIOCtx; cdecl; external libgd;
function gdNewSSCtx(infile:gdSourcePtr; _out:gdSinkPtr):PgdIOCtx; cdecl; external libgd;
function gdDPExtractData(ctx:pointer; size:Plongint):pointer; cdecl; external libgd;

{overloaded pascal functions}
function fopen(a,b:string):pFile;
procedure gdImageChar(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; c:char; color:longint);
procedure gdImageCharUp(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; c:char; color:longint);
procedure gdImageString(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:string;  color:longint);
procedure gdImageStringUp(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:string; color:longint);
procedure gdImageString16(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:string; color:longint);
procedure gdImageStringUp16(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:string; color:longint);
{$ifdef hasttf}
function  gdImageStringTTF(im:PgdImage; brect:Plongint; fg:longint; fontlist:string; ptsize:double; angle:double; x:longint; y:longint; astring:string): string;
function  gdImageStringFT(im:PgdImage; brect:Plongint; fg:longint; fontlist:string; ptsize:double; angle:double; x:longint; y:longint; astring:string):string;
{$endif}


const
  GD2_CHUNKSIZE = 128;
  GD2_CHUNKSIZE_MIN = 64;
  GD2_CHUNKSIZE_MAX = 4096;
  GD2_VERS = 1;
  GD2_ID = 'gd2';
  GD2_FMT_RAW = 1;
  GD2_FMT_COMPRESSED = 2;

function gdImageCompare(im1:gdImagePtr; im2:gdImagePtr):longint;cdecl; external libgd;

const
  GD_CMP_IMAGE = 1;
  GD_CMP_NUM_COLORS = 2;
  GD_CMP_COLOR = 4;
  GD_CMP_SIZE_X = 8;
  GD_CMP_SIZE_Y = 16;
  GD_CMP_TRANSPARENT = 32;
  GD_CMP_BACKGROUND = 64;
  GD_CMP_INTERLACE = 128;
  GD_RESOLUTION = 96;

implementation

  function gdImageSX(im : pgdimage) : longint;
    begin
       gdImageSX:=im^.sx;
    end;
  function gdImageSY(im : pgdimage) : longint;
    begin
       gdImageSY:=im^.sy;
    end;
  function gdImageColorsTotal(im : pgdimage) : longint;
    begin
       gdImageColorsTotal:=im^.colorsTotal;
    end;
  function gdImageGetTransparent(im : pgdimage) : longint;
    begin
       gdImageGetTransparent:=im^.transparent;
    end;
  function gdImageGetInterlaced(im : pgdimage) : longint;
    begin
       gdImageGetInterlaced:=im^.interlace;
    end;

  function gdImageRed(im : pgdimage; c : longint): longint;

  begin
    gdImageRed:=im^.red[c];
  end;

  function gdImageGreen(im : pgdimage; c : longint): longint;

  begin
    gdImageGreen := im^.green[c];
  end;

  function gdImageBlue(im : pgdimage; c : longint): longint;

  begin
    gdImageBlue := im^.blue[c];
  end;

function fopen(a,b:string):pFile;

begin
{$ifopt h+}
  fopen:=fopen(pchar(a),pchar(b));
{$else}
  a:=a+#0;
  b:=b+#0;
  fopen:=fopen(@a[1],@b[1]);
{$endif}
end;

procedure gdImageChar(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; c:char; color:longint);

begin
  gdimagechar(im,f,x,y,ord(c),color);
end;

procedure gdImageCharUp(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; c:char; color:longint);

begin
  gdimagecharup(im,f,x,y,ord(c),color);
end;

procedure gdImageString(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:string;  color:longint);

begin
{$ifopt h+}
  gdImageString(im,f,x,y,pbyte(pchar(s)),color);
{$else}
  s:=s+#0;
  gdImageString(im,f,x,y,@s[1],color);
{$endif}
end;

procedure gdImageStringUp(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:string; color:longint);

begin
{$ifopt h+}
  gdImageStringUp(im,f,x,y,pbyte(pchar(s)),color);
{$else}
  s:=s+#0;
  gdImageStringUp(im,f,x,y,pbyte(@s[1]),color);
{$endif}
end;

procedure gdImageString16(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:string; color:longint);

begin
{$ifopt h+}
  gdImageString16(im,f,x,y,pword(pchar(s)),color);
{$else}
  s:=s+#0;
  gdImageString16(im,f,x,y,pword(@s[1]),color);
{$endif}
end;

procedure gdImageStringUp16(im:gdImagePtr; f:gdFontPtr; x:longint; y:longint; s:string; color:longint);

begin
{$ifopt h+}
  gdImageStringUp16(im,f,x,y,pword(pchar(s)),color);
{$else}
  s:=s+#0;
  gdImageStringUp16(im,f,x,y,pword(@s[1]),color);
{$endif}
end;

{$ifdef hasttf}

function gdImageStringTTF(im:PgdImage; brect:Plongint; fg:longint; fontlist:string; ptsize:double; angle:double; x:longint; y:longint; astring:string): string;

begin
{$ifopt h+}
  gdImageStringTTF:=strpas(gdImageStringTTF(im,brect,fg,pchar(fontlist),ptsize,angle,x,y,pchar(astring)));
{$else}
  fontlist:=fornlist+#0;
  astring:=astring+#0;
  gdImageStringTTF:=strpas(gdImageStringTTF(im,brect,fg,@fontlist[1],ptsize,angle,x,y,@astring[1]));
{$endif}
end;

function gdImageStringFT(im:PgdImage; brect:Plongint; fg:longint; fontlist:string; ptsize:double; angle:double; x:longint; y:longint; astring:string):string;

begin
{$ifopt h+}
  gdImageStringFT:=strpas(gdImageStringFT(im,brect,fg,pchar(fontlist),ptsize,angle,x,y,pchar(astring)));
{$else}
  fontlist:=fornlist+#0;
  astring:=astring+#0;
  gdImageStringFT:=strpas(gdImageStringFT(im,brect,fg,@fontlist[1],ptsize,angle,x,y,@astring[1]));

{$endif}
end;
{$endif}

end.
