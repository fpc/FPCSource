{
    $Id$
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
{$mode objfpc}{$h+}
unit PixTools;

interface

uses classes, FPCanvas, FPimage;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern; const color:TFPColor);
procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern; const color:TFPColor);
procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern);
procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern);
procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);

procedure FillRectangleImage (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
procedure FillRectangleImageRel (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);

implementation

uses clipping;

procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
begin
  FillRectangleColor (Canv, x1,y1, x2,y2, canv.brush.color);
end;

procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
var x,y : integer;
begin
  SortRect (x1,y1, x2,y2);
  with Canv do
    begin
    for x := x1 to x2 do
      for y := y1 to y2 do
        colors[x,y] := color;
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

procedure PutPixelCopy(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := color;
end;

procedure PutPixelXor(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] xor color;
end;

procedure PutPixelOr(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] or color;
end;

procedure PutPixelAnd(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] and color;
end;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
begin
  DrawSolidLine (Canv, x1,y1, x2,y2, Canv.pen.color);
end;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer; const color:TFPColor);
var PutPixelProc : TPutPixelProc;
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
  with canv.pen do
    case mode of
      pmAnd : PutPixelProc := @PutPixelAnd;
      pmOr : PutPixelProc := @PutPixelOr;
      pmXor : PutPixelProc := @PutPixelXor;
      else PutPixelProc := @PutPixelCopy;
    end;
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

type
  TLinePoints = array[0..PatternBitCount] of boolean;
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
  DrawPatternLine (Canv, x1,y1, x2,y2, pattern, canv.pen.color);
end;

procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; Pattern:TPenPattern; const color:TFPColor);
// Is copy of DrawSolidLine with paterns added. Not the same procedure for faster solid lines
var LinePoints : TLinePoints;
    PutPixelProc : TPutPixelProc;
  procedure HorizontalLine (x1,x2,y:integer);
    var x : integer;
    begin
      for x := x1 to x2 do
        if LinePoints[x mod PatternBitCount] then
          PutPixelProc (Canv, x,y, color);
    end;
  procedure VerticalLine (x,y1,y2:integer);
    var y : integer;
    begin
      for y := y1 to y2 do
        if LinePoints[y mod PatternBitCount] then
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
      if LinePoints[r mod PatternBitCount] then
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
var r : integer;
begin
  PatternToPoints (pattern, @LinePoints);
  with canv.pen do
    case mode of
      pmAnd : PutPixelProc := @PutPixelAnd;
      pmOr : PutPixelProc := @PutPixelOr;
      pmXor : PutPixelProc := @PutPixelXor;
      else PutPixelProc := @PutPixelCopy;
    end;
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

procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashHorizontal (Canv, rect, width, canv.brush.color);
end;

procedure FillRectangleHashHorizontal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
var y : integer;
begin
  with rect do
    begin
    y := Width + top;
    while y <= bottom do
      begin
      DrawSolidLine (Canv, left,y, right,y, c);
      inc (y,Width);
      end
    end;
end;

procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashVertical (Canv, rect, width, canv.brush.color);
end;

procedure FillRectangleHashVertical (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
var x : integer;
begin
  with rect do
    begin
    x := Width + left;
    while x <= right do
      begin
      DrawSolidLine (Canv, x,top, x,bottom, c);
      inc (x, Width);
      end;
    end;
end;

procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashDiagonal (Canv, rect, width, canv.brush.color);
end;

procedure FillRectangleHashDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
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
    ry := top + Width;
    rx := left + Width;
    while (rx < right) and (ry < bottom) do
      begin
      DrawSolidLine (Canv, left,ry, rx,top, c);
      inc (rx, Width);
      inc (ry, Width);
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
          DrawSolidLine (Canv, left,ry, right,r, c);
          inc (r, Width);
          inc (ry, Width);
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
          DrawSolidLine (Canv, r,bottom, rx,top, c);
          inc (r, Width);
          inc (rx, Width);
          end;
        ry := CheckCorner (rx, right, top);
        rx := r;
        end;
    while (rx < right) do  // fill lower right corner
      begin
      DrawSolidLine (Canv, rx,bottom, right,ry, c);
      inc (rx, Width);
      inc (ry, Width);
      end;
    end;
end;

procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer);
begin
  FillRectangleHashBackDiagonal (Canv, rect, width, canv.brush.color);
end;

procedure FillRectangleHashBackDiagonal (Canv:TFPCustomCanvas; const rect:TRect; width:integer; const c:TFPColor);
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
    ry := bottom - Width;
    rx := left + Width;
    while (rx < right) and (ry > top) do
      begin
      DrawSolidLine (Canv, left,ry, rx,bottom, c);
      inc (rx, Width);
      dec (ry, Width);
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
          DrawSolidLine (Canv, left,ry, right,r, c);
          dec (r, Width);
          dec (ry, Width);
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
          DrawSolidLine (Canv, r,top, rx,bottom, c);
          inc (r, Width);
          inc (rx, Width);
          end;
        ry := CheckCorner (rx, right, bottom);
        rx := r;
        end;
    while (rx < right) do  // fill upper right corner
      begin
      DrawSolidLine (Canv, rx,top, right,ry, c);
      inc (rx, Width);
      dec (ry, Width);
      end;
    end;
end;

procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern);
begin
  FillRectanglePattern (Canv, x1,y1, x2,y2, pattern, canv.brush.color);
end;

procedure FillRectanglePattern (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const pattern:TBrushPattern; const color:TFPColor);
var r : integer;
begin
  for r := y1 to y2 do
    DrawPatternLine (Canv, x1,r, x2,r, pattern[r mod PatternBitCount], color);
end;

procedure FillRectangleImage (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
var x,y : integer;
begin
  with image do
    for x := x1 to x2 do
      for y := y1 to y2 do
        Canv.colors[x,y] := colors[x mod width, y mod height];
end;

procedure FillRectangleImageRel (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; const Image:TFPCustomImage);
var x,y : integer;
begin
  with image do
    for x := x1 to x2 do
      for y := y1 to y2 do
        Canv.colors[x,y] := colors[(x-x1) mod width, (y-y1) mod height];
end;

end.
