{*
 *  Free Pascal conversion (c) 1999 Sebastian Guenther
 *
 *  GGI/2D interface
 *
 *  Copyright (C) 1998 by Thomas Tanner. See CREDITS for details.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *}

{$MODE objfpc}
{$PACKRECORDS C}
{$LINKLIB c}

unit GGI2D;

interface

uses GGI;

const

  libggi2d = 'ggi2d';

type

  TGGIAlpha = Word;

const

  GGI_MAX_ALPHA = 255;


// library initialization and exit

function  ggi2dInit: Integer; cdecl; external libggi2d;
function  ggi2dExit: Integer; cdecl; external libggi2d;


// visual

function  ggi2dOpen(Visual: TGGIVisual): Integer; cdecl; external libggi2d;
function  ggi2dClose(Visual: TGGIVisual): Integer; cdecl; external libggi2d;


// images

type
  TGGI2dImage = Pointer;

function  ggi2dCreateImage(Image: TGGI2dImage; Visual, Source: TGGIVisual; x, y, Width, Height: LongWord): Integer;  cdecl; external libggi2d;
function  ggi2dDestroyImage(Image: TGGI2dImage): Integer; cdecl; external libggi2d;
function  ggi2dCompatibleImage(Visual: TGGIVisual; Image: TGGI2dImage): Integer; cdecl; external libggi2d;


// graphics context

type
  TGGI2dArcmode = (GGI2D_ARC_SECTOR, GGI2D_ARC_SEGMENT);
  TGGI2dPolymode = (GGI2D_POLY_EVENODD, GGI2D_POLY_WINDING);

  TGGI2DOperator = (
    GGI2D_NOOP,                 // dest = dest
    GGI2D_INVERT,               // dest = ~dest
    GGI2D_SET,                  // dest = color
    GGI2D_SET_INVERTED,         // dest = ~color
    GGI2D_AND,                  // dest = (dest & color)
    GGI2D_NAND,                 // dest = ~(dest & color)
    GGI2D_AND_REVERSE,          // dest = ~dest & color
    GGI2D_AND_INVERTED,         // dest = dest & ~color
    GGI2D_OR,                   // dest = (dest | color)
    GGI2D_NOR,                  // dest = ~(dest | color)
    GGI2D_OR_REVERSE,           // dest = ~dest & color
    GGI2D_OR_INVERTED,          // dest = dest & ~color
    GGI2D_XOR,                  // dest = (dest ^ color)
    GGI2D_EQUIV,                // dest = ~(dest ^ color
    GGI2D_ADD,                  // dest = dest + color
    GGI2D_SUB);                 // dest = dest - color

  TGGI2dCoord = record
    x, y: SmallInt;
  end;

function  ggi2dSetClip(Visual: TGGIVisual; x1, y1, x2, y2: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dPointVisible(Visual: TGGIVisual; x, y: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dRectVisible(Visual: TGGIVisual; x1, y1, x2, y2: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dSetArcMode(Visual: TGGIVisual; Mode: TGGI2dArcmode): Integer; cdecl; external libggi2d;
function  ggi2dGetArcMode(Visual: TGGIVisual): TGGI2dArcmode; cdecl; external libggi2d;
function  ggi2dSetPolyMode(Visual: TGGIVisual; Mode: TGGI2dPolymode): Integer; cdecl; external libggi2d;
function  ggi2dGetPolyMode(Visual: TGGIVisual): TGGI2dPolymode; cdecl; external libggi2d;
function  ggi2dSetLineDash(Visual: TGGIVisual; var Dash: LongWord; Size: LongWord): Integer; cdecl; external libggi2d;
function  ggi2dGetLineDash(Visual: TGGIVisual; var Dash: LongWord; var Size: LongWord): Integer; cdecl; external libggi2d;
function  ggi2dSetAppendMode(Visual: TGGIVisual; Append: Integer): Integer; cdecl; external libggi2d;
function  ggi2dGetAppendMode(Visual: TGGIVisual): Integer; cdecl; external libggi2d;
function  ggi2dSetDrawColor(Visual: TGGIVisual; Color: TGGIPixel): Integer; cdecl; external libggi2d;
function  ggi2dGetDrawColor(Visual: TGGIVisual): TGGIPixel; cdecl; external libggi2d;
function  ggi2dSetFillColor(Visual: TGGIVisual; Color: TGGIPixel): Integer; cdecl; external libggi2d;
function  ggi2dGetFillColor(Visual: TGGIVisual): TGGIPixel; cdecl; external libggi2d;
function  ggi2dSetFillTexture(Visual: TGGIVisual; RefPoint: TGGI2dCoord; Texture: TGGI2dImage): Integer; cdecl; external libggi2d;
function  ggi2dGetFillTexture(Visual: TGGIVisual; var RefPoint: TGGI2dCoord; var Texture: TGGI2dImage): Integer; cdecl; external libggi2d;
function  ggi2dSetOperator(Visual: TGGIVisual; Oper: TGGI2dOperator): Integer; cdecl; external libggi2d;
function  ggi2dGetOperator(Visual: TGGIVisual): TGGI2dOperator; cdecl; external libggi2d;


// drawing

type
  TGGI2dScanline = record
    x1, x2, y: SmallInt;
  end;

  TGGI2dLine = record
    x1, y1, x2, y2: SmallInt;
  end;


// primitives

function  ggi2dPutPixel(Visual: TGGIVisual; x, y: SmallInt; Color: TGGIPixel): Integer; cdecl; external libggi2d;
function  ggi2dDrawPixel(Visual: TGGIVisual; x, y: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dFillPixel(Visual: TGGIVisual; x, y: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dDrawPixels(Visual: TGGIVisual; var Coords: TGGI2dCoord; Count: LongWord): Integer; cdecl; external libggi2d;
function  ggi2dScanLine(Visual: TGGIVisual; x1, x2, y: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dScanLines(Visual: TGGIVisual; var Scanlines: TGGI2dScanline; Count: LongWord): Integer; cdecl; external libggi2d;
function  ggi2dHLine(Visual: TGGIVisual; x1, x2, y: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dVLine(Visual: TGGIVisual; x, y1, y2: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dDrawRect(Visual: TGGIVisual; x1, y1, x2, y2: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dFillRect(Visual: TGGIVisual; x1, y1, x2, y2: SmallInt): Integer; cdecl; external libggi2d;


// curves

function  ggi2dLine(Visual: TGGIVisual; x1, y1, x2, y2: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dDrawLines(Visual: TGGIVisual; var Lines: TGGI2dLine; Count: LongWord): Integer; cdecl; external libggi2d;
function  ggi2dDrawCircle(Visual: TGGIVisual; x, y, r: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dFillCircle(Visual: TGGIVisual; x, y, r: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dDrawEllipse(Visual: TGGIVisual; x, y, rx, ry: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dFillEllipse(Visual: TGGIVisual; x, y, rx, ry: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dDrawArc(Visual: TGGIVisual; x, y, rx, ry: SmallInt; Start, AEnd: Single; Close: Integer): Integer; cdecl; external libggi2d;
function  ggi2dFillArc(Visual: TGGIVisual; x, y, rx, ry: SmallInt; Start, AEnd: Single): Integer; cdecl; external libggi2d;
function  ggi2dBezier(Visual: TGGIVisual; x1, y1, x2, y2, x3, y3, x4, y4: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dTrapezoid(Visual: TGGIVisual; xl1, xr1, y1, xl2, xr2, y2: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dTriangle(Visual: TGGIVisual; x1, y1, x2, y2, x3, y3: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dDrawPoly(Visual: TGGIVisual; var Coords: TGGI2dCoord; Count: LongWord): Integer; cdecl; external libggi2d;
function  ggi2dFillPoly(Visual: TGGIVisual; var Coords: TGGI2dCoord; Count: LongWord): Integer; cdecl; external libggi2d;
function  ggi2dFillPolys(Visual: TGGIVisual; var Coords: TGGI2dCoord; var Counts: LongWord; Count: LongWord): Integer; cdecl; external libggi2d;


// blitting

function  ggi2dCopyBox(Visual: TGGIVisual; x, y, w, h, nx, ny: SmallInt): Integer; cdecl; external libggi2d;
function  ggi2dCrossBlit(Src: TGGIVisual; sx, sy, w, h: SmallInt; Dest: TGGIVisual;  dx, dy: SmallInt): Integer; cdecl; external libggi2d;

function  ggi2dBlit(
            Visual: TGGIVisual; dx, dy: SmallInt;
            Src: TGGI2dImage; sx, sy, Width, Height: SmallInt): Integer; cdecl; external libggi2d;

function  ggi2dStretchBlit(
            Visual: TGGIVisual; dx, dy, DWith, DHeight: SmallInt;
            Src: TGGI2dImage; sx, sy, SWidth, SHeight: SmallInt): Integer; cdecl; external libggi2d;

function  ggi2dBlitOp(
            Visual: TGGIVisual; dx, dy: SmallInt;
            Src1: TGGI2dImage; s1x, s1y: SmallInt;
            Src2: TGGI2dImage; s2x, s2y: SmallInt;
            Width, Height: SmallInt; Oper: TGGI2dOperator): Integer; cdecl; external libggi2d;

function  ggi2dStretchBlitOp(
            Visual: TGGIVisual; dx, dy, DWidth, DHeight: SmallInt;
            Src1: TGGI2dImage; s1x, s1y: SmallInt;
            Src2: TGGI2dImage; s2x, s2y: SmallInt;
            SWidth, SHeight: SmallInt; Oper: TGGI2dOperator): Integer; cdecl; external libggi2d;



implementation

end.
