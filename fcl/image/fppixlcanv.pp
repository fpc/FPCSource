{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TPixelCanvas class.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPPixlCanv;

interface

uses classes, FPImage, FPCanvas, PixTools;

type

  { need still to be implemented :
    GetColor / SetColor
    Get/Set Width/Height
  }

  PixelCanvasException = class (TFPCanvasException);

  TFPPixelCanvas = class (TFPCustomCanvas)
  protected
    function DoCreateDefaultFont : TFPCustomFont; override;
    function DoCreateDefaultPen : TFPCustomPen; override;
    function DoCreateDefaultBrush : TFPCustomBrush; override;
    procedure DoTextOut (x,y:integer;text:string); override;
    procedure DoGetTextSize (text:string; var w,h:integer); override;
    function  DoGetTextHeight (text:string) : integer; override;
    function  DoGetTextWidth (text:string) : integer; override;
    procedure DoRectangle (Bounds:TRect); override;
    procedure DoRectangleFill (Bounds:TRect); override;
    procedure DoEllipseFill (Bounds:TRect); override;
    procedure DoEllipse (Bounds:TRect); override;
    procedure DoPolygonFill (points:array of TPoint); override;
    procedure DoPolygon (points:array of TPoint); override;
    procedure DoPolyline (points:array of TPoint); override;
    procedure DoFloodFill (x,y:integer); override;
    procedure DoLine (x1,y1,x2,y2:integer); override;
  end;

implementation

uses Clipping;

const
  ErrNotAvailable = 'Not availlable';

procedure NotImplemented;
begin
  raise PixelCanvasException.Create(ErrNotAvailable);
end;

function TFPPixelCanvas.DoCreateDefaultFont : TFPCustomFont;
begin
  result := TFPEmptyFont.Create;
  with result do
    begin
    Size := 10;
    Color := colBlack;
    end;
end;

function TFPPixelCanvas.DoCreateDefaultPen : TFPCustomPen;
begin
  result := TFPEmptyPen.Create;
  with result do
    begin
    Color := colBlack;
    width := 1;
    pattern := 0;
    Style := psSolid;
    Mode := pmCopy;
    end;
end;

function TFPPixelCanvas.DoCreateDefaultBrush : TFPCustomBrush;
begin
  result := TFPEmptyBrush.Create;
  with result do
    begin
    Style := bsClear;
    end;
end;

procedure TFPPixelCanvas.DoTextOut (x,y:integer;text:string);
begin
  NotImplemented;
end;

procedure TFPPixelCanvas.DoGetTextSize (text:string; var w,h:integer);
begin
  NotImplemented;
end;

function  TFPPixelCanvas.DoGetTextHeight (text:string) : integer;
begin
  result := -1;
  NotImplemented;
end;

function  TFPPixelCanvas.DoGetTextWidth (text:string) : integer;
begin
  result := -1;
  NotImplemented;
end;

procedure TFPPixelCanvas.DoRectangle (Bounds:TRect);
begin
  if clipping then
    CheckRectClipping (ClipRect, Bounds);
  with Bounds do
    begin
    DoLine (left,top,left,bottom);
    DoLine (left,bottom,right,bottom);
    DoLine (right,bottom,right,top);
    DoLine (right,top,left,top);
    end;
end;

procedure TFPPixelCanvas.DoRectangleFill (Bounds:TRect);
begin
  writeln ('Rectangle Fill, sorting bounds');
  SortRect (bounds);
  writeln ('Checking clipping');
  if clipping then
    CheckRectClipping (ClipRect, Bounds);
  writeln ('Choosing what to do');
  with bounds do
    case Brush.style of  //TODO: patterns and image
      bsSolid : FillRectangleColor (self, left,top, right,bottom);
      bsPattern : ;
      bsImage : ;
      bsDiagonal : ;
      bsFDiagonal : ;
      bsCross : ;
      bsDiagCross : ;
      bsHorizontal : ;
      bsVertical : ;
    end;
  writeln ('Rectangle finished');
end;

procedure TFPPixelCanvas.DoEllipseFill (Bounds:TRect);
begin  //TODO
end;

procedure TFPPixelCanvas.DoEllipse (Bounds:TRect);
begin  //TODO
end;

procedure TFPPixelCanvas.DoPolygonFill (points:array of TPoint);
begin  //TODO: how to find a point inside the polygon ?
end;

procedure TFPPixelCanvas.DoPolygon (points:array of TPoint);
var i,a, r : integer;
    p : TPoint;
begin
  i := low(points);
  a := high(points);
  p := points[i];
  for r := i+1 to a do
    begin
    DoLine (p.x, p.y, points[r].x, points[r].y);
    p := points[r];
    end;
  DoLine (p.x,p.y, points[i].x,points[i].y);
end;

procedure TFPPixelCanvas.DoPolyline (points:array of TPoint);
var i,a, r : integer;
    p : TPoint;
begin
  i := low(points);
  a := high(points);
  p := points[i];
  for r := i+1 to a do
    begin
    DoLine (p.x, p.y, points[r].x, points[r].y);
    p := points[r];
    end;
end;

procedure TFPPixelCanvas.DoFloodFill (x,y:integer);
begin    //TODO
end;

const
  PenPatterns : array[psDash..psDashDotDot] of word =
    ($EEEE, $AAAA, $E4E4, $EAEA);

procedure TFPPixelCanvas.DoLine (x1,y1,x2,y2:integer);

  procedure DrawOneLine (xx1,yy1, xx2,yy2:integer);
  begin
    if Clipping then
      CheckLineClipping (ClipRect, xx1,yy1, xx2,yy2);
    DrawSolidLine (self, xx1,yy1, xx2,yy2);
  end;

  procedure SolidThickLine;
  var w1, w2, r : integer;
      MoreHor : boolean;
  begin
    // determine lines above and under
    w1 := pen.width div 2;
    w2 := w1;
    if w1+w2 = pen.width then
      dec (w1);
    // determine slanting
    MoreHor := (abs(x2-x1) < abs(y2-y1));
    if MoreHor then
      begin  // add lines left/right
      for r := 1 to w1 do
        DrawOneLine (x1-r,y1, x2-r,y2);
      for r := 1 to w2 do
        DrawOneLine (x1+r,y1, x2+r,y2);
      end
    else
      begin  // add lines above/under
      for r := 1 to w1 do
        DrawOneLine (x1,y1-r, x2,y2-r);
      for r := 1 to w2 do
        DrawOneLine (x1,y1+r, x2,y2+r);
      end;
  end;

begin
  if Clipping then
    CheckLineClipping (ClipRect, x1,y1, x2,y2);
  case Pen.style of
    psSolid :
      begin
      DrawSolidLine (self, x1,y1, x2,y2);
      if pen.width > 1 then
        SolidThickLine;
      end;
    psPattern: ;
      // DrawPatternLine (self, x1,y1, x2,y2, pattern);
      // Patterned lines have width always at 1
    psDash, psDot, psDashDot, psDashDotDot : ;
      //DrawPatternLine (self, x1,y1, x2,y2, PenPattern[Style]);
  end;
end;

end.
