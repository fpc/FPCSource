{
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
{$IFNDEF FPC_DOTTEDUNITS}
unit FPPixlCanv;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpImage, FpImage.Canvas, FpImage.PixelTools, FpImage.Ellipses, FpImage.PolygonFillTools;
{$ELSE FPC_DOTTEDUNITS}
uses Sysutils, classes, FpImage, FPCanvas, PixTools, ellipses, PolygonFillTools;
{$ENDIF FPC_DOTTEDUNITS}

type

  { need still to be implemented in descendants :
    GetColor / SetColor
    Get/Set Width/Height
  }

  PixelCanvasException = class (TFPCanvasException);

  { TFPPixelCanvas }

  TFPPixelCanvas = class (TFPCustomCanvas)
  private
    // True when the brush is a hatch and HatchOrigin is not hoDefault.
    function HatchByOrigin : boolean;
    procedure PenRectangle (const Bounds:TRect);
    procedure PenEllipse (const Bounds:TRect);
    procedure PenPolygon (const points:array of TPoint);
    procedure PenPolyline (const points:array of TPoint);
    procedure PenLine (x1,y1,x2,y2:integer);
    procedure StrokeThick (const points:array of TPoint; aClosed:boolean);
  protected
    function DoCreateDefaultFont : TFPCustomFont; override;
    function DoCreateDefaultPen : TFPCustomPen; override;
    function DoCreateDefaultBrush : TFPCustomBrush; override;
    procedure DoTextOut (x,y:integer;text:AnsiString); override;
    procedure DoGetTextSize (text:AnsiString; var w,h:integer); override;
    function  DoGetTextHeight (text:AnsiString) : integer; override;
    function  DoGetTextWidth (text:AnsiString) : integer; override;
    procedure DoRectangle (const Bounds:TRect); override;
    procedure DoRectangleFill (const Bounds:TRect); override;
    procedure DoEllipseFill (const Bounds:TRect); override;
    procedure DoEllipse (const Bounds:TRect); override;
    procedure DoPolygonFill (const points:array of TPoint); override;
    procedure DoPolygon (const points:array of TPoint); override;
    procedure DoPolyline (const points:array of TPoint); override;
    procedure DoFloodFill (x,y:integer); override;
    procedure DoLine (x1,y1,x2,y2:integer); override;
    function GetNativeTextOrigin : TFPTextOrigin; override;
    function GetNativeTextMeasure : TFPTextMeasure; override;
    procedure DoFloodFillStyle (x, y: integer; const FillColor: TFPColor; FillStyle: TFPFloodFillStyle); override;
  public
  end;

const
  PenPatterns : array[psDash..psDashDotDot] of TPenPattern =
    ($EEEEEEEE, $AAAAAAAA, $E4E4E4E4, $EAEAEAEA);
  sErrNoImage:AnsiString = 'No brush image specified';
  sErrNotAvailable:AnsiString = 'Not available';

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Math, FpImage.Clipping;
{$ELSE FPC_DOTTEDUNITS}
uses Math, Clipping;
{$ENDIF FPC_DOTTEDUNITS}

procedure NotImplemented;
begin
  raise ENotImplemented.Create(sErrNotAvailable);
end;

function TFPPixelCanvas.GetNativeTextOrigin : TFPTextOrigin;
begin
  Result := toBaseline;
end;

function TFPPixelCanvas.GetNativeTextMeasure : TFPTextMeasure;
begin
  Result := tmInk;
end;

procedure TFPPixelCanvas.DoFloodFillStyle (x, y: integer; const FillColor: TFPColor; FillStyle: TFPFloodFillStyle);
begin
  if FillStyle = ffSurface then
    inherited DoFloodFillStyle(x, y, FillColor, FillStyle)
  else
    begin
    BeginFloodFillBorder(FillColor);
    try
      DoFloodFill(x, y);
    finally
      EndFloodFillBorder;
    end;
    end;
end;

function TFPPixelCanvas.DoCreateDefaultFont : TFPCustomFont;
begin
  result := TFPEmptyFont.Create;
  with result do
    begin
    Size := 10;
    FPColor := colBlack;
    end;
end;

function TFPPixelCanvas.DoCreateDefaultPen : TFPCustomPen;
begin
  result := TFPEmptyPen.Create;
  with result do
    begin
    FPColor := colBlack;
    width := 1;
    pattern := 0;
    Style := psSolid;
    Mode := pmCopy;
    end;
end;

function TFPPixelCanvas.DoCreateDefaultBrush : TFPCustomBrush;
begin
  result := TFPEmptyBrush.Create;
  result.Style := bsSolid;
end;

procedure TFPPixelCanvas.DoTextOut (x,y:integer;text:AnsiString);
begin
  NotImplemented;
end;

procedure TFPPixelCanvas.DoGetTextSize (text:AnsiString; var w,h:integer);
begin
  NotImplemented;
end;

function  TFPPixelCanvas.DoGetTextHeight (text:AnsiString) : integer;
begin
  result := -1;
  NotImplemented;
end;

function  TFPPixelCanvas.DoGetTextWidth (text:AnsiString) : integer;
begin
  result := -1;
  NotImplemented;
end;

procedure TFPPixelCanvas.DoRectangle (const Bounds:TRect);
begin
  BeginPenShape;
  try
    PenRectangle (Bounds);
  finally
    EndPenShape;
  end;
end;

// Draws the outline of a rectangle with the pen.
procedure TFPPixelCanvas.PenRectangle (const Bounds:TRect);
var pattern : longword;

  procedure CheckLine (x1,y1, x2,y2 : integer);
  begin
    if not clipping or ClipLine (DeviceClipRect, x1,y1, x2,y2) then
      DrawSolidLine (self, x1,y1, x2,y2, Pen.FPColor)
  end;

  procedure CheckPLine (x1,y1, x2,y2 : integer);
  begin
    if not clipping or ClipLine (DeviceClipRect, x1,y1, x2,y2) then
      DrawPatternLine (self, x1,y1, x2,y2, pattern, Pen.FPColor)
  end;

var b : TRect;
    r : integer;

begin
  b := bounds;
  if pen.style = psSolid then
    for r := 1 to pen.width do
      begin
      with b do
        begin
        CheckLine (left,top,left,bottom);
        CheckLine (left,bottom,right,bottom);
        CheckLine (right,bottom,right,top);
        CheckLine (right,top,left,top);
        end;
      DecRect (b);
      end
  else if pen.style <> psClear then
    begin
    if pen.style = psPattern then
      pattern := Pen.pattern
    else
      pattern := PenPatterns[pen.style];
    with b do
      begin
      CheckPLine (left,top,left,bottom);
      CheckPLine (left,bottom,right,bottom);
      CheckPLine (right,bottom,right,top);
      CheckPLine (right,top,left,top);
      end;
    end;
end;

function TFPPixelCanvas.HatchByOrigin : boolean;
begin
  Result := (HatchOrigin <> hoDefault)
            and (Brush.Style in [bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross]);
end;

procedure TFPPixelCanvas.DoRectangleFill (const Bounds:TRect);
var b : TRect;
    ox, oy : integer;
begin
  b := Bounds;
  SortRect (b);
  ox := 0;
  oy := 0;
  if HatchOrigin = hoShape then
    begin
    ox := b.Left;
    oy := b.Top;
    end;
  if clipping then
    CheckRectClipping (DeviceClipRect, B);
  if HatchByOrigin then
    begin
    with b do
      FillRectangleHatch (self, left,top, right,bottom, Brush.Style, HashWidth, ox,oy, Brush.FPColor);
    exit;
    end;
  with b do
    case Brush.style of
      bsSolid : FillRectangleColor (self, left,top, right,bottom);
      bsPattern : FillRectanglePattern (self, left,top, right,bottom, brush.pattern);
      bsImage :
        if assigned (brush.image) then
          if RelativeBrushImage then
            FillRectangleImageRel (self, left,top, right,bottom, brush.image)
          else
            FillRectangleImage (self, left,top, right,bottom, brush.image)
        else
          raise PixelCanvasException.Create (sErrNoImage);
      bsBDiagonal : FillRectangleHashDiagonal (self, b, HashWidth);
      bsFDiagonal : FillRectangleHashBackDiagonal (self, b, HashWidth);
      bsCross :
        begin
        FillRectangleHashHorizontal (self, b, HashWidth);
        FillRectangleHashVertical (self, b, HashWidth);
        end;
      bsDiagCross :
        begin
        FillRectangleHashDiagonal (self, b, HashWidth);
        FillRectangleHashBackDiagonal (self, b, HashWidth);
        end;
      bsHorizontal : FillRectangleHashHorizontal (self, b, HashWidth);
      bsVertical : FillRectangleHashVertical (self, b, HashWidth);
    end;
end;

procedure TFPPixelCanvas.DoEllipseFill (const Bounds:TRect);
begin
  if HatchByOrigin then
    begin
    if HatchOrigin = hoShape then
      FillEllipseHatch (self, Bounds, Brush.Style, HashWidth, Min(Bounds.Left, Bounds.Right),
                        Min(Bounds.Top, Bounds.Bottom), Brush.FPColor)
    else
      FillEllipseHatch (self, Bounds, Brush.Style, HashWidth, 0, 0, Brush.FPColor);
    exit;
    end;
  case Brush.style of
    bsSolid : FillEllipseColor (self, Bounds, Brush.FPColor);
    bsPattern : FillEllipsePattern (self, Bounds, brush.pattern, Brush.FPColor);
    bsImage :
      if assigned (brush.image) then
        if RelativeBrushImage then
          FillEllipseImageRel (self, Bounds, brush.image)
        else
          FillEllipseImage (self, Bounds, brush.image)
      else
        raise PixelCanvasException.Create (sErrNoImage);
    bsBDiagonal : FillEllipseHashDiagonal (self, Bounds, HashWidth, Brush.FPColor);
    bsFDiagonal : FillEllipseHashBackDiagonal (self, Bounds, HashWidth, Brush.FPColor);
    bsCross : FillEllipseHashCross (self, Bounds, HashWidth, Brush.FPColor);
    bsDiagCross : FillEllipseHashDiagCross (self, Bounds, HashWidth, Brush.FPColor);
    bsHorizontal : FillEllipseHashHorizontal (self, Bounds, HashWidth, Brush.FPColor);
    bsVertical : FillEllipseHashVertical (self, Bounds, HashWidth, Brush.FPColor);
  end;
end;

procedure TFPPixelCanvas.DoEllipse (const Bounds:TRect);
begin
  BeginPenShape;
  try
    PenEllipse (Bounds);
  finally
    EndPenShape;
  end;
end;

// Draws the outline of an ellipse with the pen.
procedure TFPPixelCanvas.PenEllipse (const Bounds:TRect);
begin
  with pen do
    case style of
      psSolid :
        if pen.width > 1 then
          DrawSolidEllipse (self, Bounds, width, FPColor)
        else
          DrawSolidEllipse (self, Bounds, FPColor);
      psPattern:
        DrawPatternEllipse (self, Bounds, pattern, FPColor);
      psDash, psDot, psDashDot, psDashDotDot :
        DrawPatternEllipse (self, Bounds, PenPatterns[Style], FPColor);
    end;
end;

procedure TFPPixelCanvas.DoPolygonFill (const points:array of TPoint);
var ox, oy, i : integer;
begin
  if HatchByOrigin then
    begin
    ox := 0;
    oy := 0;
    if (HatchOrigin = hoShape) and (Length(points) > 0) then
      begin
      ox := points[0].X;
      oy := points[0].Y;
      for i := 1 to High(points) do
        begin
        ox := Min(ox, points[i].X);
        oy := Min(oy, points[i].Y);
        end;
      end;
    FillPolygonHatch (self, points, PolygonNonZeroWindingRule, Brush.Style, HashWidth, ox, oy, Brush.FPColor);
    exit;
    end;
  case Brush.Style of
    bsSolid:
      FillPolygonSolid(self, points, PolygonNonZeroWindingRule, Brush.FPColor);
    bsHorizontal:
      FillPolygonHorizontal(self, points, PolygonNonZeroWindingRule, Brush.FPColor, HashWidth);
    bsVertical:
      FillPolygonVertical(self, points, PolygonNonZeroWindingRule, Brush.FPColor, HashWidth);
    bsCross:
      begin
        FillPolygonHorizontal(self, points, PolygonNonZeroWindingRule, Brush.FPColor, HashWidth);
        FillPolygonVertical(self, points, PolygonNonZeroWindingRule, Brush.FPColor, HashWidth);
      end;
    bsFDiagonal:
      FillPolygonDiagonal(self, points, PolygonNonZeroWindingRule, Brush.FPColor, HashWidth);
    bsBDiagonal:
      FillPolygonBackDiagonal(self, points, PolygonNonZeroWindingRule, Brush.FPColor, HashWidth);
    bsDiagCross:
      begin
        FillPolygonDiagonal(self, points, PolygonNonZeroWindingRule, Brush.FPColor, HashWidth);
        FillPolygonBackDiagonal(self, points, PolygonNonZeroWindingRule, Brush.FPColor, HashWidth);
      end;
    bsPattern:
      FillPolygonPattern(self, points, PolygonNonZeroWindingRule, Brush.FPColor, Brush.Pattern);
    bsImage:
      FillPolygonImage(self, points, PolygonNonZeroWindingRule, Brush.Image, RelativeBrushImage);
  end;
end;

procedure TFPPixelCanvas.DoFloodFill (x,y:integer);
begin
  if HatchByOrigin then
    begin
    if HatchOrigin = hoShape then
      FillFloodHatch (self, x,y, Brush.Style, HashWidth, x,y, Brush.FPColor)
    else
      FillFloodHatch (self, x,y, Brush.Style, HashWidth, 0,0, Brush.FPColor);
    exit;
    end;
  case Brush.style of
    bsSolid : FillFloodColor (self, x,y);
    bsPattern : FillFloodPattern (self, x,y, brush.pattern);
    bsImage :
      if assigned (brush.image) then
        if RelativeBrushImage then
          FillFloodImageRel (self, x,y, brush.image)
        else
          FillFloodImage (self, x,y, brush.image)
      else
        raise PixelCanvasException.Create (sErrNoImage);
    bsBDiagonal : FillFloodHashDiagonal (self, x,y, HashWidth);
    bsFDiagonal : FillFloodHashBackDiagonal (self, x,y, HashWidth);
    bsCross : FillFloodHashCross (self, x,y, HashWidth);
    bsDiagCross : FillFloodHashDiagCross (self, x,y, HashWidth);
    bsHorizontal : FillFloodHashHorizontal (self, x,y, HashWidth);
    bsVertical : FillFloodHashVertical (self, x,y, HashWidth);
  end;
end;

procedure TFPPixelCanvas.DoPolygon (const points:array of TPoint);
begin
  BeginPenShape;
  try
    PenPolygon (points);
  finally
    EndPenShape;
  end;
end;

// Draws the closed outline of a polygon with the pen.
procedure TFPPixelCanvas.PenPolygon (const points:array of TPoint);
var i,a, r : integer;
    p : TPoint;
begin
  if (Pen.Style = psSolid) and (Pen.Width > 1) then
    begin
    StrokeThick (points, true);
    exit;
    end;
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

procedure TFPPixelCanvas.DoPolyline (const points:array of TPoint);
begin
  BeginPenShape;
  try
    PenPolyline (points);
  finally
    EndPenShape;
  end;
end;

// Draws connected lines through points with the pen.
procedure TFPPixelCanvas.PenPolyline (const points:array of TPoint);
var i,a, r : integer;
    p : TPoint;
begin
  if (Pen.Style = psSolid) and (Pen.Width > 1) then
    begin
    StrokeThick (points, false);
    exit;
    end;
  i := low(points);
  a := high(points);
  p := points[i];
  for r := i+1 to a do
    begin
    DoLine (p.x, p.y, points[r].x, points[r].y);
    p := points[r];
    end;
end;

procedure TFPPixelCanvas.DoLine (x1,y1,x2,y2:integer);
begin
  BeginPenShape;
  try
    PenLine (x1,y1, x2,y2);
  finally
    EndPenShape;
  end;
end;

// Draws a line with the pen, both end points included.
procedure TFPPixelCanvas.PenLine (x1,y1,x2,y2:integer);
var
  cx1, cy1, cx2, cy2 : integer;
begin
  case Pen.style of
    psSolid :
      if pen.width > 1 then
        StrokeThick ([Point(x1,y1), Point(x2,y2)], false)
      else if not Clipping or ClipLine (DeviceClipRect, x1,y1, x2,y2) then
        DrawSolidLine (self, x1,y1, x2,y2, Pen.FPColor);
    psPattern, psDash, psDot, psDashDot, psDashDotDot :
      begin
      // Patterned lines have width always at 1
      cx1 := x1; cy1 := y1; cx2 := x2; cy2 := y2;
      if not Clipping or ClipLine (DeviceClipRect, cx1,cy1, cx2,cy2) then
        if Pen.Style = psPattern then
          DrawPatternLine (self, cx1,cy1, cx2,cy2, pen.pattern)
        else
          DrawPatternLine (self, cx1,cy1, cx2,cy2, PenPatterns[Pen.Style]);
      end;
  end;
end;

{ Strokes the lines through points with a pen wider than one pixel: the width
  is measured across each segment, ends follow Pen.EndCap and corners
  Pen.JoinStyle. Pixel (x, y) has its centre at (x+0.5, y+0.5). }
procedure TFPPixelCanvas.StrokeThick (const points:array of TPoint; aClosed:boolean);
const
  MiterLimit = 10;
var
  h : double;
  clip : TRect;
  pts : array of TPolygonPointF;
  count, i, first, last : integer;

  function PF (ax, ay : double) : TPolygonPointF;
  begin
    Result.X := ax;
    Result.Y := ay;
  end;

  procedure Direction (const a, b : TPolygonPointF; out dx, dy : double);
  var len : double;
  begin
    dx := b.X - a.X;
    dy := b.Y - a.Y;
    len := Sqrt(dx * dx + dy * dy);
    dx := dx / len;
    dy := dy / len;
  end;

  procedure Fill (const p : array of TPolygonPointF);
  begin
    FillPolygonPenF (self, p, clip, Pen.FPColor);
  end;

  procedure Circle (const c : TPolygonPointF);
  var n, k : integer;
      p : array of TPolygonPointF;
  begin
    n := Max(8, Ceil(2 * Pi * h));
    SetLength(p, n);
    for k := 0 to n - 1 do
      p[k] := PF(c.X + h * Cos(2 * Pi * k / n), c.Y + h * Sin(2 * Pi * k / n));
    Fill (p);
  end;

  procedure Body (const a, b : TPolygonPointF);
  var dx, dy : double;
  begin
    Direction (a, b, dx, dy);
    Fill ([PF(a.X - dy * h, a.Y + dx * h), PF(b.X - dy * h, b.Y + dx * h),
           PF(b.X + dy * h, b.Y - dx * h), PF(a.X + dy * h, a.Y - dx * h)]);
  end;

  // Caps the end c of a segment; (dx, dy) points away from the segment.
  procedure Cap (const c : TPolygonPointF; dx, dy : double);
  var e : double;
  begin
    if Pen.EndCap = pecRound then
    begin
      Circle (c);
      exit;
    end;
    if Pen.EndCap = pecSquare then
      e := h
    else
      e := 0.5;
    Fill ([PF(c.X - dy * h, c.Y + dx * h), PF(c.X - dy * h + dx * e, c.Y + dx * h + dy * e),
           PF(c.X + dy * h + dx * e, c.Y - dx * h + dy * e), PF(c.X + dy * h, c.Y - dx * h)]);
  end;

  // Joins the segments a-p and p-b at p.
  procedure Join (const a, p, b : TPolygonPointF);
  var d1x, d1y, d2x, d2y, cross, side, n1x, n1y, n2x, n2y, dot, ratio : double;
      o1, o2, m : TPolygonPointF;
  begin
    if Pen.JoinStyle = pjsRound then
    begin
      Circle (p);
      exit;
    end;
    Direction (a, p, d1x, d1y);
    Direction (p, b, d2x, d2y);
    cross := d1x * d2y - d1y * d2x;
    if Abs(cross) < 1e-9 then
      exit;
    if cross > 0 then
      side := -1
    else
      side := 1;
    n1x := -d1y * side; n1y := d1x * side;
    n2x := -d2y * side; n2y := d2x * side;
    o1 := PF(p.X + n1x * h, p.Y + n1y * h);
    o2 := PF(p.X + n2x * h, p.Y + n2y * h);
    dot := n1x * n2x + n1y * n2y;
    ratio := Sqrt(2 / (1 + dot));
    if (Pen.JoinStyle = pjsMiter) and (dot > -1 + 1e-9) and (ratio <= MiterLimit) then
    begin
      m := PF(p.X + (n1x + n2x) * h / (1 + dot), p.Y + (n1y + n2y) * h / (1 + dot));
      Fill ([p, o1, m, o2]);
    end
    else
      Fill ([p, o1, o2]);
  end;

var dx, dy : double;
begin
  if Clipping then
    clip := DeviceClipRect
  else
    clip := Rect(0, 0, Width - 1, Height - 1);
  h := Pen.Width / 2;
  SetLength(pts, Length(points));
  count := 0;
  for i := 0 to High(points) do
    if (count = 0) or (points[i].X + 0.5 <> pts[count - 1].X) or (points[i].Y + 0.5 <> pts[count - 1].Y) then
    begin
      pts[count] := PF(points[i].X + 0.5, points[i].Y + 0.5);
      inc(count);
    end;
  if aClosed and (count > 1) and (pts[0].X = pts[count - 1].X) and (pts[0].Y = pts[count - 1].Y) then
    dec(count);
  if count = 0 then
    exit;
  if count = 1 then
  begin
    if Pen.EndCap = pecRound then
      Circle (pts[0])
    else
      Fill ([PF(pts[0].X - h, pts[0].Y - h), PF(pts[0].X + h, pts[0].Y - h),
             PF(pts[0].X + h, pts[0].Y + h), PF(pts[0].X - h, pts[0].Y + h)]);
    exit;
  end;
  for i := 0 to count - 2 do
    Body (pts[i], pts[i + 1]);
  if aClosed and (count > 2) then
  begin
    Body (pts[count - 1], pts[0]);
    for i := 0 to count - 1 do
      Join (pts[(i + count - 1) mod count], pts[i], pts[(i + 1) mod count]);
  end
  else
  begin
    for i := 1 to count - 2 do
      Join (pts[i - 1], pts[i], pts[i + 1]);
    first := 0;
    last := count - 1;
    Direction (pts[first + 1], pts[first], dx, dy);
    Cap (pts[first], dx, dy);
    Direction (pts[last - 1], pts[last], dx, dy);
    Cap (pts[last], dx, dy);
  end;
end;

end.
