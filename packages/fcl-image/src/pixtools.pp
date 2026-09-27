{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Pixel drawing routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit PixTools;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.Classes, FpImage.Canvas, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses classes, FPCanvas, FPimage;
{$ENDIF FPC_DOTTEDUNITS}

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
// Draws a line of brush pixels, both end points included: DrawingMode applies, the pen mode does not.
procedure DrawBrushLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern; const color:TFPColor);
procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern; const color:TFPColor);
procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer; const c:TFPColor);
procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer; const c:TFPColor);
procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer; const c:TFPColor);
procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer; const c:TFPColor);
procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern; const color:TFPColor);
procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern);
procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern);
procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer);
procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer);
procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer);
procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer);
procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer);
procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern);
procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);

procedure FillRectangleImage (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
procedure FillRectangleImageRel (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
procedure FillFloodImage (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);
procedure FillFloodImageRel (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);

// True when (x, y) lies on a line of the hatch aStyle, lines aWidth apart, counted from (aOriginX, aOriginY).
function HatchPixel (aStyle:TFPBrushStyle; x,y, aWidth, aOriginX,aOriginY:integer) : boolean;
// Fills the rectangle (corners included) with the hatch aStyle counted from (aOriginX, aOriginY).
procedure FillRectangleHatch (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; aStyle:TFPBrushStyle;
  aWidth, aOriginX,aOriginY:integer; const c:TFPColor);
// Flood fills from (x, y) with the hatch aStyle counted from (aOriginX, aOriginY).
procedure FillFloodHatch (Canv:TFPCustomCanvas; x,y:integer; aStyle:TFPBrushStyle;
  aWidth, aOriginX,aOriginY:integer; const c:TFPColor);
// Makes the flood fills of the calling thread fill up to pixels of aColor instead of the colour at their start.
procedure BeginFloodFillBorder (const aColor:TFPColor);
// Makes the flood fills of the calling thread fill the colour at their start again.
procedure EndFloodFillBorder;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage.Clipping;
{$ELSE FPC_DOTTEDUNITS}
uses clipping;
{$ENDIF FPC_DOTTEDUNITS}

procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
begin
  FillRectangleColor (Canv, x1,y1, x2,y2, Canv.Brush.FPColor);
end;

procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
var x,y : integer;
begin
  SortRect (x1,y1, x2,y2);
  with Canv do
    begin
      for y := y1 to y2 do
        for x := x1 to x2 do
          DrawPixel(x,y,color);
    end;
end;

{procedure DrawSolidPolyLine (Canv : TFPCustomCanvas; points:array of TPoint; close:boolean);
var i,a, r : integer;
    p : TPoint;
begin
  i := low(points);
  a := high(points);
  p := points[i];
  with Canv do
    begin
    for r := i+1 to a do
      begin
      Line (p.x, p.y, points[r].x, points[r].y);
      p := points[r];
      end;
    if close then
      Line (p.x,p.y, points[i].x,points[i].y);
    end;
end;
}
type
  TPutPixelProc = procedure (Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);

// Draws color at (x, y) with DrawPenPixel: combined with the pixel there by Pen.Mode, and by
// DrawingMode when Pen.Mode is pmCopy.
procedure PutPixelPen(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  Canv.DrawPenPixel(x,y,color);
end;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
begin
  DrawSolidLine (Canv, x1,y1, x2,y2, Canv.Pen.FPColor);
end;

// Draws color at (x, y) with DrawPixel: combined with the pixel there by DrawingMode; Pen.Mode is ignored.
procedure PutPixelBrush(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  Canv.DrawPixel(x,y,color);
end;

// Draws the line from (x1,y1) to (x2,y2), both included, with PutPixelProc.
procedure LineWith (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor; PutPixelProc : TPutPixelProc);
  procedure HorizontalLine (x1,x2,y:integer);
    var x : integer;
    begin
      for x := x1 to x2 do
        PutPixelProc (Canv, x,y, color);
    end;
  procedure VerticalLine (x,y1,y2:integer);
    var y : integer;
    begin
      for y := y1 to y2 do
        PutPixelProc (Canv, x,y, color);
    end;
  procedure SlopedLine;
    var npixels,xinc1,yinc1,xinc2,yinc2,dx,dy,d,dinc1,dinc2 : integer;
    procedure initialize;
      begin // precalculations
      dx := abs(x2-x1);
      dy := abs(y2-y1);
      if dx > dy then  // determining independent variable
        begin  // x is independent
        npixels := dx + 1;
        d := (2 * dy) - dx;
        dinc1 := dy * 2;
        dinc2:= (dy - dx) * 2;
        xinc1 := 1;
        xinc2 := 1;
        yinc1 := 0;
        yinc2 := 1;
        end
      else
        begin  // y is independent
        npixels := dy + 1;
        d := (2 * dx) - dy;
        dinc1 := dx * 2;
        dinc2:= (dx - dy) * 2;
        xinc1 := 0;
        xinc2 := 1;
        yinc1 := 1;
        yinc2 := 1;
        end;
      // going into the correct direction
      if x1 > x2 then
        begin
        xinc1 := - xinc1;
        xinc2 := - xinc2;
        end;
      if y1 > y2 then
        begin
        yinc1 := - yinc1;
        yinc2 := - yinc2;
        end;
      end;
    var r,x,y : integer;
    begin
    initialize;
    x := x1;
    y := y1;
    for r := 1 to nPixels do
      begin
      PutPixelProc (Canv, x,y, color);
      if d < 0 then
        begin
        d := d + dinc1;
        x := x + xinc1;
        y := y + yinc1;
        end
      else
        begin
        d := d + dinc2;
        x := x + xinc2;
        y := y + yinc2;
        end;
      end;
    end;
begin
  if x1 = x2 then  // vertical line
    if y1 < y2 then
      VerticalLine (x1, y1, y2)
    else
      VerticalLine (x1, y2, y1)
  else if y1 = y2 then
    if x1 < x2 then
      HorizontalLine (x1, x2, y1)
    else
      HorizontalLine (x2, x1, y1)
  else  // sloped line
    SlopedLine;
end;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
begin
  LineWith (Canv, x1,y1, x2,y2, color, @PutPixelPen);
end;

procedure DrawBrushLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
begin
  LineWith (Canv, x1,y1, x2,y2, color, @PutPixelBrush);
end;

type
  TLinePoints = array[0..PatternBitCount-1] of boolean;
  PLinePoints = ^TLinePoints;

procedure PatternToPoints (const APattern:TPenPattern; LinePoints:PLinePoints);
var r : integer;
    i : longword;
begin
  i := 1;
  for r := PatternBitCount-1 downto 1 do
    begin
    LinePoints^[r] := (APattern and i) <> 0;
    i := i shl 1;
    end;
  LinePoints^[0] := (APattern and i) <> 0;
end;

procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern);
begin
  DrawPatternLine (Canv, x1,y1, x2,y2, pattern, Canv.Pen.FPColor);
end;

procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern; const color:TFPColor);
// Is copy of DrawSolidLine with patterns added. Not the same procedure for faster solid lines
var LinePoints : TLinePoints;
    PutPixelProc : TPutPixelProc;
  procedure HorizontalLine (x1,x2,y:integer);
    var x : integer;
    begin
      for x := x1 to x2 do
        if LinePoints[x and (PatternBitCount-1)] then
          PutPixelProc (Canv, x,y, color);
    end;
  procedure VerticalLine (x,y1,y2:integer);
    var y : integer;
    begin
      for y := y1 to y2 do
        if LinePoints[y and (PatternBitCount-1)] then
          PutPixelProc (Canv, x,y, color);
    end;
  procedure SlopedLine;
    var npixels,xinc1,yinc1,xinc2,yinc2,dx,dy,d,dinc1,dinc2 : integer;
    procedure initialize;
      begin // precalculations
      dx := abs(x2-x1);
      dy := abs(y2-y1);
      if dx > dy then  // determining independent variable
        begin  // x is independent
        npixels := dx + 1;
        d := (2 * dy) - dx;
        dinc1 := dy * 2;
        dinc2:= (dy - dx) * 2;
        xinc1 := 1;
        xinc2 := 1;
        yinc1 := 0;
        yinc2 := 1;
        end
      else
        begin  // y is independent
        npixels := dy + 1;
        d := (2 * dx) - dy;
        dinc1 := dx * 2;
        dinc2:= (dx - dy) * 2;
        xinc1 := 0;
        xinc2 := 1;
        yinc1 := 1;
        yinc2 := 1;
        end;
      // going into the correct direction
      if x1 > x2 then
        begin
        xinc1 := - xinc1;
        xinc2 := - xinc2;
        end;
      if y1 > y2 then
        begin
        yinc1 := - yinc1;
        yinc2 := - yinc2;
        end;
      end;
    var r,x,y,idx : integer;
    begin
    initialize;
    x := x1;
    y := y1;
    for r := 1 to nPixels do
      begin
      if dx >= dy then
        idx := x
      else
        idx := y;
      if LinePoints[idx and (PatternBitCount-1)] then
        PutPixelProc (Canv, x,y, color);
      if d < 0 then
        begin
        d := d + dinc1;
        x := x + xinc1;
        y := y + yinc1;
        end
      else
        begin
        d := d + dinc2;
        x := x + xinc2;
        y := y + yinc2;
        end;
      end;
    end;
begin
  PatternToPoints (pattern, @LinePoints);
  PutPixelProc := @PutPixelPen;
  if x1 = x2 then  // vertical line
    if y1 < y2 then
      VerticalLine (x1, y1, y2)
    else
      VerticalLine (x1, y2, y1)
  else if y1 = y2 then
    if x1 < x2 then
      HorizontalLine (x1, x2, y1)
    else
      HorizontalLine (x2, x1, y1)
  else  // sloped line
    SlopedLine;
end;

procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer);
begin
  FillRectangleHashHorizontal (Canv, rect, AWidth, Canv.Brush.FPColor);
end;

procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer; const c:TFPColor);
var y : integer;
begin
  with rect do
    begin
    y := AWidth + top;
    while y <= bottom do
      begin
      DrawBrushLine (Canv, left,y, right,y, c);
      inc (y,AWidth);
      end
    end;
end;

procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer);
begin
  FillRectangleHashVertical (Canv, rect, AWidth, Canv.Brush.FPColor);
end;

procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer; const c:TFPColor);
var x : integer;
begin
  with rect do
    begin
    x := AWidth + left;
    while x <= right do
      begin
      DrawBrushLine (Canv, x,top, x,bottom, c);
      inc (x, AWidth);
      end;
    end;
end;

procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer);
begin
  FillRectangleHashDiagonal (Canv, rect, AWidth, Canv.Brush.FPColor);
end;

procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer; const c:TFPColor);
function CheckCorner (Current, max, start : integer) : integer;
  begin
    if Current > max then
      result := Start + current - max
    else
      result := Start;
  end;
var r, rx, ry : integer;
begin
  with rect do
    begin
    // draw from bottom-left corner away
    ry := top + AWidth;
    rx := left + AWidth;
    while (rx < right) and (ry < bottom) do
      begin
      DrawBrushLine (Canv, left,ry, rx,top, c);
      inc (rx, AWidth);
      inc (ry, AWidth);
      end;
    // check which turn need to be taken: left-bottom, right-top, or both
    if (rx >= right) then
      begin
      if (ry >= bottom) then
        begin // Both corners reached
        r := CheckCorner (rx, right, top);
        rx := CheckCorner (ry, bottom, left);
        ry := r;
        end
      else
        begin  // fill vertical
        r := CheckCorner (rx, right, top);
        while (ry < bottom) do
          begin
          DrawBrushLine (Canv, left,ry, right,r, c);
          inc (r, AWidth);
          inc (ry, AWidth);
          end;
        rx := CheckCorner (ry, bottom, left);
        ry := r;
        end
      end
    else
      if (ry >= bottom) then
        begin  // fill horizontal
        r := checkCorner (ry, bottom, left);
        while (rx <= right) do
          begin
          DrawBrushLine (Canv, r,bottom, rx,top, c);
          inc (r, AWidth);
          inc (rx, AWidth);
          end;
        ry := CheckCorner (rx, right, top);
        rx := r;
        end;
    while (rx < right) do  // fill lower right corner
      begin
      DrawBrushLine (Canv, rx,bottom, right,ry, c);
      inc (rx, AWidth);
      inc (ry, AWidth);
      end;
    end;
end;

procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer);
begin
  FillRectangleHashBackDiagonal (Canv, rect, AWidth, Canv.Brush.FPColor);
end;

procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; AWidth:integer; const c:TFPColor);
  function CheckInversCorner (Current, min, start : integer) : integer;
  begin
    if Current < min then
      result := Start - current + min
    else
      result := Start;
  end;
  function CheckCorner (Current, max, start : integer) : integer;
  begin
    if Current > max then
      result := Start - current + max
    else
      result := Start;
  end;
var r, rx, ry : integer;
begin
  with rect do
    begin
    // draw from bottom-left corner away
    ry := bottom - AWidth;
    rx := left + AWidth;
    while (rx < right) and (ry > top) do
      begin
      DrawBrushLine (Canv, left,ry, rx,bottom, c);
      inc (rx, AWidth);
      dec (ry, AWidth);
      end;
    // check which turn need to be taken: left-top, right-bottom, or both
    if (rx >= right) then
      begin
      if (ry <= top) then
        begin // Both corners reached
        r := CheckCorner (rx, right, bottom);
        rx := CheckInversCorner (ry, top, left);
        ry := r;
        end
      else
        begin  // fill vertical
        r := CheckCorner (rx, right, bottom);
        while (ry > top) do
          begin
          DrawBrushLine (Canv, left,ry, right,r, c);
          dec (r, AWidth);
          dec (ry, AWidth);
          end;
        rx := CheckInversCorner (ry, top, left);
        ry := r;
        end
      end
    else
      if (ry <= top) then
        begin  // fill horizontal
        r := checkInversCorner (ry, top, left);
        while (rx < right) do
          begin
          DrawBrushLine (Canv, r,top, rx,bottom, c);
          inc (r, AWidth);
          inc (rx, AWidth);
          end;
        ry := CheckCorner (rx, right, bottom);
        rx := r;
        end;
    while (rx < right) do  // fill upper right corner
      begin
      DrawBrushLine (Canv, rx,top, right,ry, c);
      inc (rx, AWidth);
      dec (ry, AWidth);
      end;
    end;
end;

procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern);
begin
  FillRectanglePattern (Canv, x1,y1, x2,y2, pattern, Canv.Brush.FPColor);
end;

procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern; const color:TFPColor);
var x, y : integer;
    row : TPenPattern;
begin
  SortRect (x1,y1, x2,y2);
  for y := y1 to y2 do
    begin
    row := pattern[y and (PatternBitCount-1)];
    for x := x1 to x2 do
      if (row shr (PatternBitCount - 1 - (x and (PatternBitCount-1)))) and 1 <> 0 then
        Canv.DrawPixel (x,y, color);
    end;
end;

procedure FillRectangleImage (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
var x,y : integer;
begin
  with image do
    for y := y1 to y2 do
      for x := x1 to x2 do
        Canv.DrawPixel(x,y, colors[x mod width, y mod height]);
end;

procedure FillRectangleImageRel (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
var x,y : integer;
begin
  with image do
    for y := y1 to y2 do
      for x := x1 to x2 do
        Canv.DrawPixel(x,y, colors[(x-x1) mod width, (y-y1) mod height]);
end;

type
  TFuncSetColor = procedure (Canv:TFPCustomCanvas; x,y:integer; data:pointer);

  PFloodFillData = ^TFloodFillData;
  TFloodFillData = record
    Canv : TFPCustomCanvas;
    ReplColor : TFPColor;
    Border : boolean;
    SetColor : TFuncSetColor;
    ExtraData : pointer;
  end;

{$IFDEF FPC_HAS_FEATURE_THREADING}
threadvar
{$ELSE}
var
{$ENDIF}
  FloodBorderActive : boolean;
  FloodBorderColor : TFPColor;

procedure BeginFloodFillBorder (const aColor:TFPColor);
begin
  FloodBorderActive := true;
  FloodBorderColor := aColor;
end;

procedure EndFloodFillBorder;
begin
  FloodBorderActive := false;
end;

// Sets the colour data^ replaces, or in a border fill the colour it stops at, for a fill from (x, y).
procedure InitFloodColor (var d:TFloodFillData; x,y:integer);
begin
  d.Border := FloodBorderActive;
  if d.Border then
    d.ReplColor := FloodBorderColor
  else
    d.ReplColor := d.Canv.colors[x,y];
end;

// Calls data^.SetColor once for each pixel of colour data^.ReplColor that is connected to (x, y)
// horizontally or vertically through pixels of that colour.
procedure FloodFill (data:PFloodFillData; x,y:integer);
var
  w, h, lx, rx, cx, ny, i, count : integer;
  done : array of byte;
  stack : array of TPoint;
  p : TPoint;

  function Fillable (ax, ay : integer) : boolean;
  var idx : Int64;
  begin
    idx := Int64(ay) * w + ax;
    Result := ((done[idx shr 3] and (1 shl (idx and 7))) = 0)
              and ((data^.Canv.Colors[ax, ay] = data^.ReplColor) <> data^.Border);
  end;

  procedure SetDone (ax, ay : integer);
  var idx : Int64;
  begin
    idx := Int64(ay) * w + ax;
    done[idx shr 3] := done[idx shr 3] or (1 shl (idx and 7));
  end;

  procedure Push (ax, ay : integer);
  begin
    if count = Length(stack) then
      SetLength(stack, 2 * count + 64);
    stack[count] := Point(ax, ay);
    inc(count);
  end;

begin
  w := data^.Canv.Width;
  h := data^.Canv.Height;
  if (x < 0) or (y < 0) or (x >= w) or (y >= h) then
    exit;
  SetLength(done, (Int64(w) * h + 7) div 8);
  FillChar(done[0], Length(done), 0);
  stack := nil;
  count := 0;
  Push(x, y);
  while count > 0 do
  begin
    dec(count);
    p := stack[count];
    if not Fillable(p.X, p.Y) then
      continue;
    lx := p.X;
    while (lx > 0) and Fillable(lx - 1, p.Y) do
      dec(lx);
    rx := p.X;
    while (rx < w - 1) and Fillable(rx + 1, p.Y) do
      inc(rx);
    for cx := lx to rx do
    begin
      SetDone(cx, p.Y);
      data^.SetColor(data^.Canv, cx, p.Y, data^.ExtraData);
    end;
    for i := 0 to 1 do
    begin
      ny := p.Y - 1 + 2 * i;
      if (ny < 0) or (ny >= h) then
        continue;
      cx := lx;
      while cx <= rx do
        if Fillable(cx, ny) then
        begin
          Push(cx, ny);
          while (cx <= rx) and Fillable(cx, ny) do
            inc(cx);
        end
        else
          inc(cx);
    end;
  end;
end;

procedure SetFloodColor (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
begin
  Canv.DrawPixel(x,y, PFPColor(data)^);
end;

procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer; const color:TFPColor);
var d : TFloodFillData;
begin
  d.Canv := canv;
  InitFloodColor (d, x, y);
  d.SetColor := @SetFloodColor;
  d.ExtraData := @color;
  FloodFill (@d, x, y);
end;

procedure FillFloodColor (Canv:TFPCustomCanvas; x,y:integer);
begin
  FillFloodColor (Canv, x, y, Canv.Brush.FPColor);
end;

type
  TBoolPlane = array[0..PatternBitCount-1] of TLinePoints;
  TFloodPatternRec = record
    plane : TBoolPlane;
    color : TFPColor;
  end;
  PFloodPatternRec = ^TFloodPatternRec;

procedure SetFloodPattern (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var p : PFloodPatternRec;
begin
  p := PFloodPatternRec(data);
  if p^.plane[y and (PatternBitCount-1), x and (PatternBitCount-1)] then
    Canv.DrawPixel(x,y,p^.color);
end;

procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern; const color:TFPColor);
var rec : TFloodPatternRec;
    d : TFloodFillData;

  procedure FillPattern;
  var r : integer;
  begin
    for r := 0 to PatternBitCount-1 do
      PatternToPoints (pattern[r], @rec.plane[r]);
  end;

begin
  d.Canv := canv;
  InitFloodColor (d, x, y);
  d.SetColor := @SetFloodPattern;
  d.ExtraData := @rec;
  FillPattern;
  rec.color := Color;
  FloodFill (@d, x, y);
end;

procedure FillFloodPattern (Canv:TFPCustomCanvas; x,y:integer; const pattern:TBrushPattern);
begin
  FillFloodPattern (Canv, x, y, pattern, Canv.Brush.FPColor);
end;

type
  TFloodHashRec = record
    color : TFPColor;
    width : integer;
  end;
  PFloodHashRec = ^TFloodHashRec;

procedure SetFloodHashHor(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
begin
  r := PFloodHashRec(data);
  if (y mod r^.width) = 0 then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashVer(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
begin
  r := PFloodHashRec(data);
  if (x mod r^.width) = 0 then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashDiag(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : integer;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if ((x mod w) + (y mod w)) = (w - 1) then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashBDiag(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : integer;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if (x mod w) = (y mod w) then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashCross(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : integer;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if ((x mod w) = 0) or ((y mod w) = 0) then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure SetFloodHashDiagCross(Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodHashRec;
    w : integer;
begin
  r := PFloodHashRec(data);
  w := r^.width;
  if ( (x mod w) = (y mod w) ) or
     ( ((x mod w) + (y mod w)) = (w - 1) ) then
    Canv.DrawPixel(x,y,r^.color);
end;

procedure FillFloodHash (Canv:TFPCustomCanvas; x,y:integer; width:integer; SetHashColor:TFuncSetColor; const c:TFPColor);
var rec : TFloodHashRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  InitFloodColor (d, x, y);
  d.SetColor := SetHashColor;
  d.ExtraData := @rec;
  rec.color := c;
  if Width < 1 then
    Width := 1;
  rec.width := Width;
  FloodFill (@d, x, y);
end;

procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashHor, c);
end;

procedure FillFloodHashHorizontal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashHorizontal (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashVer, c);
end;

procedure FillFloodHashVertical (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashVertical (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashDiag, c);
end;

procedure FillFloodHashDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashDiagonal (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashBDiag, c);
end;

procedure FillFloodHashBackDiagonal (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashBackDiagonal (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashDiagCross, c);
end;

procedure FillFloodHashDiagCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashDiagCross (Canv, x, y, width, Canv.Brush.FPColor);
end;

procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer; const c:TFPColor);
begin
  FillFloodHash (canv, x, y, width, @SetFloodHashCross, c);
end;

procedure FillFloodHashCross (Canv:TFPCustomCanvas; x,y:integer; width:integer);
begin
  FillFloodHashCross (Canv, x, y, width, Canv.Brush.FPColor);
end;

type
  TFloodImageRec = record
    xo,yo : integer;
    image : TFPCustomImage;
  end;
  PFloodImageRec = ^TFloodImageRec;

procedure SetFloodImage (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodImageRec;
begin
  r := PFloodImageRec(data);
  with r^.image do
    Canv.DrawPixel(x,y,colors[x mod width, y mod height]);
end;

procedure FillFloodImage (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);
var rec : TFloodImageRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  InitFloodColor (d, x, y);
  d.SetColor := @SetFloodImage;
  d.ExtraData := @rec;
  rec.image := image;
  FloodFill (@d, x, y);
end;

procedure SetFloodImageRel (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
var r : PFloodImageRec;
    xi, yi : integer;
begin
  r := PFloodImageRec(data);
  with r^, image do
    begin
    xi := (x - xo) mod width;
    if xi < 0 then
      xi := width + xi;
    yi := (y - yo) mod height;
    if yi < 0 then
      yi := height + yi;
    Canv.DrawPixel(x,y,colors[xi,yi]);
    end;
end;

procedure FillFloodImageRel (Canv:TFPCustomCanvas; x,y :integer; const Image:TFPCustomImage);
var rec : TFloodImageRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  InitFloodColor (d, x, y);
  d.SetColor := @SetFloodImageRel;
  d.ExtraData := @rec;
  rec.image := image;
  rec.xo := x;
  rec.yo := y;
  FloodFill (@d, x, y);
end;


function HatchPixel (aStyle:TFPBrushStyle; x,y, aWidth, aOriginX,aOriginY:integer) : boolean;
var mx, my : integer;
begin
  if aWidth < 1 then
    aWidth := 1;
  mx := ((x - aOriginX) mod aWidth + aWidth) mod aWidth;
  my := ((y - aOriginY) mod aWidth + aWidth) mod aWidth;
  case aStyle of
    bsHorizontal : Result := my = 0;
    bsVertical : Result := mx = 0;
    bsFDiagonal : Result := mx = my;
    bsBDiagonal : Result := (mx + my) mod aWidth = aWidth - 1;
    bsCross : Result := (mx = 0) or (my = 0);
    bsDiagCross : Result := (mx = my) or ((mx + my) mod aWidth = aWidth - 1);
  else
    Result := false;
  end;
end;

procedure FillRectangleHatch (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; aStyle:TFPBrushStyle;
  aWidth, aOriginX,aOriginY:integer; const c:TFPColor);
var x,y : integer;
begin
  SortRect (x1,y1, x2,y2);
  for y := y1 to y2 do
    for x := x1 to x2 do
      if HatchPixel (aStyle, x,y, aWidth, aOriginX,aOriginY) then
        Canv.DrawPixel (x,y, c);
end;

type
  TFloodHatchRec = record
    color : TFPColor;
    style : TFPBrushStyle;
    width, ox, oy : integer;
  end;
  PFloodHatchRec = ^TFloodHatchRec;

// Paints a flood pixel when it lies on the hatch.
procedure SetFloodHatch (Canv:TFPCustomCanvas; x,y:integer; data:pointer);
begin
  with PFloodHatchRec(data)^ do
    if HatchPixel (style, x,y, width, ox,oy) then
      Canv.DrawPixel (x,y, color);
end;

procedure FillFloodHatch (Canv:TFPCustomCanvas; x,y:integer; aStyle:TFPBrushStyle;
  aWidth, aOriginX,aOriginY:integer; const c:TFPColor);
var rec : TFloodHatchRec;
    d : TFloodFillData;
begin
  d.Canv := canv;
  InitFloodColor (d, x, y);
  d.SetColor := @SetFloodHatch;
  d.ExtraData := @rec;
  rec.color := c;
  rec.style := aStyle;
  rec.width := aWidth;
  rec.ox := aOriginX;
  rec.oy := aOriginY;
  FloodFill (@d, x, y);
end;

end.
