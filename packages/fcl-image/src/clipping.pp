{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Clipping support.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit Clipping;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses classes;
{$ENDIF FPC_DOTTEDUNITS}

procedure SortRect (var rect : TRect);
procedure SortRect (var left,top, right,bottom : integer);
function PointInside (const x,y:integer; bounds:TRect) : boolean;

Function CheckRectClipping (ClipRect:TRect; var Rect:Trect) : Boolean;
Function CheckRectClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer) : Boolean;
procedure CheckLineClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer);
// Moves the endpoints (x1, y1) and (x2, y2) to the ends of the part of the line inside ClipRect
// (Right and Bottom included) and returns True; returns False with the endpoints unchanged
// when the whole line lies outside ClipRect.
function ClipLine (ClipRect:TRect; var x1,y1, x2,y2 : integer) : boolean;

implementation

procedure SortRect (var rect : TRect);
begin
  with rect do
    SortRect (left,top, right,bottom);
end;

procedure SortRect (var left,top, right,bottom : integer);
var r : integer;
begin
  if left > right then
    begin
    r := left;
    left := right;
    right := r;
    end;
  if top > bottom then
    begin
    r := top;
    top := bottom;
    bottom := r;
    end;
end;

function PointInside (const x,y:integer; bounds:TRect) : boolean;
begin
  SortRect (bounds);
  with Bounds do
    result := (x >= left) and (x <= right) and
              (y >= top) and (y <= bottom);
end;

Function CheckRectClipping (ClipRect:TRect; var Rect:Trect) : Boolean;
begin
  with Rect do
    Result:=CheckRectClipping (ClipRect, left,top,right,bottom);
end;

Function CheckRectClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer) : boolean;

  procedure ClearRect;
  begin
    x1 := -1;
    x2 := -1;
    y1 := -1;
    y2 := -1;
  end;
begin
  Result:=true;
  SortRect (ClipRect);
  SortRect (x1,y1, x2,y2);

  with ClipRect do
    begin
    if ( x1 < Left ) then // left side needs to be clipped
      x1 := left;
    if ( x2 > right ) then // right side needs to be clipped
      x2 := right;
    if ( y1 < top ) then // top side needs to be clipped
      y1 := top;
    if ( y2 > bottom ) then // bottom side needs to be clipped
      y2 := bottom;
    if (x1 > x2) or (y1 > y2) then
      begin
      ClearRect;
      Result:=False;
      end;
    end;
end;

function ClipLine (ClipRect:TRect; var x1,y1, x2,y2 : integer) : boolean;
const
  cLeft = 1;
  cRight = 2;
  cTop = 4;
  cBottom = 8;
var
  fx1, fy1, fx2, fy2, x, y : double;
  c1, c2, c : integer;

  function Code (ax, ay : double) : integer;
  begin
    Result := 0;
    if ax < ClipRect.Left then
      Result := cLeft
    else if ax > ClipRect.Right then
      Result := cRight;
    if ay < ClipRect.Top then
      Result := Result or cTop
    else if ay > ClipRect.Bottom then
      Result := Result or cBottom;
  end;

begin
  SortRect (ClipRect);
  fx1 := x1;
  fy1 := y1;
  fx2 := x2;
  fy2 := y2;
  c1 := Code (fx1, fy1);
  c2 := Code (fx2, fy2);
  while (c1 or c2) <> 0 do
  begin
    if (c1 and c2) <> 0 then
      exit(false);
    if c1 <> 0 then
      c := c1
    else
      c := c2;
    if (c and cTop) <> 0 then
    begin
      x := fx1 + (fx2 - fx1) * (ClipRect.Top - fy1) / (fy2 - fy1);
      y := ClipRect.Top;
    end
    else if (c and cBottom) <> 0 then
    begin
      x := fx1 + (fx2 - fx1) * (ClipRect.Bottom - fy1) / (fy2 - fy1);
      y := ClipRect.Bottom;
    end
    else if (c and cRight) <> 0 then
    begin
      y := fy1 + (fy2 - fy1) * (ClipRect.Right - fx1) / (fx2 - fx1);
      x := ClipRect.Right;
    end
    else
    begin
      y := fy1 + (fy2 - fy1) * (ClipRect.Left - fx1) / (fx2 - fx1);
      x := ClipRect.Left;
    end;
    if c = c1 then
    begin
      fx1 := x;
      fy1 := y;
      c1 := Code (fx1, fy1);
    end
    else
    begin
      fx2 := x;
      fy2 := y;
      c2 := Code (fx2, fy2);
    end;
  end;
  x1 := Round (fx1);
  y1 := Round (fy1);
  x2 := Round (fx2);
  y2 := Round (fy2);
  Result := true;
end;

procedure CheckLineClipping (ClipRect:TRect; var x1,y1, x2,y2 : integer);
begin
  if not ClipLine (ClipRect, x1,y1, x2,y2) then
  begin
    x1 := -1;
    y1 := -1;
    x2 := -1;
    y2 := -1;
  end;
end;

end.
