{%MainUnit fpcanvas.pp}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TFPCustomCanvas implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TFPCustomCanvas }

constructor TFPCustomCanvas.Create;
begin
  inherited create;
  FMatrix := TFPCanvasMatrix.Identity;
  FHashWidth := 15;
  FTextOrigin := GetNativeTextOrigin;
  FTextMeasure := GetNativeTextMeasure;
  FTextStyle := Default(TFPTextStyle);
  FTextStyle.Alignment := taLeftJustify;
  FTextStyle.Layout := ftlTop;
  FTextStyle.Wordbreak := True;
  FTextStyle.SingleLine := True;
  FTextStyle.Clipping := True;
  FClipping := false;
  FRemovingHelpers := false;
  FHelpers := TList.Create;
  FDefaultFont := CreateDefaultFont;
  FDefaultPen := CreateDefaultPen;
  FDefaultBrush := CreateDefaultBrush;
end;

destructor TFPCustomCanvas.Destroy;
begin
  FreeAndNil(FClipRegion);
  FRemovingHelpers := True;
  // first remove all helper references
  RemoveHelpers;
  // then free helpers
  FDefaultFont.Free;
  FDefaultBrush.Free;
  FDefaultPen.Free;
  FHelpers.Free;
  FRemovingHelpers := False;
  inherited;
end;

procedure TFPCustomCanvas.CheckHelper (AHelper:TFPCanvasHelper);
// remove references to AHelper
begin
  if AHelper = FPen then
    FPen := nil
  else if AHelper = FFont then
    FFont := nil
  else if AHelper = FBrush then
    FBrush := nil;
  if not FRemovingHelpers then
    begin
    if AHelper = FDefaultFont then
      FDefaultFont := CreateDefaultFont
    else if AHelper = FDefaultPen then
      FDefaultPen := CreateDefaultPen
    else if AHelper = FDefaultBrush then
      FDefaultBrush := CreateDefaultBrush;
    end;
  FHelpers.Remove (AHelper);
end;

procedure TFPCustomCanvas.RemoveHelpers;
var r : integer;
begin
  for r := FHelpers.count-1 downto 0 do
    with TFPCanvasHelper(FHelpers[r]) do
      if FCanvas = self then
        if FFixedCanvas then
          DeallocateResources
        else
          FCanvas := nil;
  FHelpers.Clear;
end;

procedure TFPCustomCanvas.AddHelper (AHelper : TFPCanvasHelper);
var r : integer;
begin
  r := FHelpers.IndexOf (AHelper);
  if r < 0 then
    FHelpers.Add (AHelper);
end;

function TFPCustomCanvas.CreateDefaultFont : TFPCustomFont;
begin
  result := DoCreateDefaultFont;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EFont])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.CreateDefaultPen : TFPCustomPen;
begin
  result := DoCreateDefaultPen;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EPen])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.CreateDefaultBrush : TFPCustomBrush;
begin
  result := DoCreateDefaultBrush;
  if not assigned (result) then
    raise TFPCanvasException.CreateFmt (ErrCouldNotCreate, [EBrush])
  else
    begin
    result.AllocateResources (self);
    FHelpers.Add (result);
    end;
end;

function TFPCustomCanvas.GetClipping: boolean;
begin
  Result:=FClipping;
end;

function TFPCustomCanvas.GetClipRect: TRect;
begin
  if FClipRegion = nil then Result := Bounds(0, 0, 0, 0)
  else
    begin
    Result:=FClipRegion.GetBoundingRect();
    if FRectangleMode=rmExclude then
      begin
      Inc(Result.Right);
      Inc(Result.Bottom);
      end;
    end;
end;

function TFPCustomCanvas.GetDeviceClipRect: TRect;
begin
  if FClipRegion = nil then Result := Bounds(0, 0, 0, 0)
  else Result:=FClipRegion.GetBoundingRect();
end;

function TFPCustomCanvas.UserRect(const aRect: TRect; out aResult: TRect): Boolean;

begin
  aResult:=aRect;
  SortRect(aResult);
  if FRectangleMode=rmExclude then
    begin
    if (aResult.Right<=aResult.Left) or (aResult.Bottom<=aResult.Top) then
      Exit(False);
    Dec(aResult.Right);
    Dec(aResult.Bottom);
    end;
  Result:=True;
end;

function TFPCustomCanvas.DeviceRect(const aRect: TRect; out aResult: TRect): Boolean;

var
  lExtent: TRect;

begin
  Result:=UserRect(aRect,aResult);
  if Result and HasTransform then
    begin
    lExtent:=TransformRect(Rect(aResult.Left,aResult.Top,aResult.Right+1,aResult.Bottom+1));
    aResult:=Rect(lExtent.Left,lExtent.Top,lExtent.Right-1,lExtent.Bottom-1);
    Result:=(aResult.Right>=aResult.Left) and (aResult.Bottom>=aResult.Top);
    end;
end;

function TFPCustomCanvas.CreateFont : TFPCustomFont;
begin
  result := DoCreateDefaultFont;
end;

function TFPCustomCanvas.CreatePen : TFPCustomPen;
begin
  result := DoCreateDefaultPen;
end;

function TFPCustomCanvas.CreateBrush : TFPCustomBrush;
begin
  result := DoCreateDefaultBrush;
end;

function TFPCustomCanvas.AllowFont (AFont : TFPCustomFont) : boolean;
begin
  if AFont is TFPCustomDrawFont then
    result := true
  else
    result := DoAllowFont (AFont);
end;

procedure TFPCustomCanvas.SetFont (AValue:TFPCustomFont);
begin
  if (AValue <> FFont) and AllowFont(AValue) then
    begin
      if FManageResources then
        FFont.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FFont := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetFont : TFPCustomFont;
begin
  if assigned (FFont) then
    result := FFont
  else
    result := FDefaultFont;
end;

function TFPCustomCanvas.DoAllowFont (AFont : TFPCustomFont) : boolean;
begin
  result := false;
end;

function TFPCustomCanvas.AllowBrush (ABrush : TFPCustomBrush) : boolean;
begin
  if ABrush is TFPCustomDrawBrush then
    result := true
  else
    result := DoAllowBrush (ABrush);
end;

procedure TFPCustomCanvas.SetBrush (AValue:TFPCustomBrush);
begin
  if (AValue <> FBrush) and AllowBrush(AValue) then
    begin
      if FManageResources then
        FBrush.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FBrush := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetBrush : TFPCustomBrush;
begin
  if assigned (FBrush) then
    result := FBrush
  else
    result := FDefaultBrush
end;

function TFPCustomCanvas.DoAllowBrush (ABrush : TFPCustomBrush) : boolean;
begin
  result := false;
end;

function TFPCustomCanvas.AllowPen (APen : TFPCustomPen) : boolean;
begin
  if APen is TFPCustomDrawPen then
    result := true
  else
    result := DoAllowPen (APen);
end;

procedure TFPCustomCanvas.SetPen (AValue:TFPCustomPen);
begin
  if (AValue <> FPen) and AllowPen (AValue) then
    begin
      if FManageResources then
        FPen.Assign(AValue)
      else
        begin
          AValue.AllocateResources (self);
          FPen := AValue;
          AddHelper (AValue);
        end;
    end;
end;

function TFPCustomCanvas.GetPen : TFPCustomPen;
begin
  if assigned (FPen) then
    result := FPen
  else
    result := FDefaultPen;
end;

procedure TFPCustomCanvas.SetClipping(const AValue: boolean);
begin
  FClipping:=AValue;
end;

procedure TFPCustomCanvas.SetClipRect(const AValue: TRect);
var
  lNewRegion: TFPRectRegion;
begin
  lNewRegion := TFPRectRegion.Create;
  lNewRegion.Rect := AValue;
  SortRect(lNewRegion.Rect);
  if FRectangleMode=rmExclude then
    begin
    Dec(lNewRegion.Rect.Right);
    Dec(lNewRegion.Rect.Bottom);
    end;
  if FClipRegion <> nil then FClipRegion.Free;
  FClipRegion := lNewRegion;
end;

procedure TFPCustomCanvas.SetPenPos(const AValue: TPoint);
begin
  FPenPos:=AValue;
end;

procedure TFPCustomCanvas.SetClipRegion(const AValue: TFPCustomRegion);
begin
  if AValue = FClipRegion then exit;
  FClipRegion.Free;
  FClipRegion := AValue;
end;

function TFPCustomCanvas.DoAllowPen (APen : TFPCustomPen) : boolean;
begin
  result := false;
end;

procedure TFPCustomCanvas.DoLockCanvas;
begin
end;

procedure TFPCustomCanvas.DoUnlockCanvas;
begin
end;

procedure TFPCustomCanvas.LockCanvas;
begin
  if FLocks = 0 then
    DoLockCanvas;
  inc (FLocks);
end;

procedure TFPCustomCanvas.UnlockCanvas;
begin
  if FLocks > 0 then
    begin
    dec (FLocks);
    if FLocks = 0 then
      DoUnlockCanvas;
    end
  else
    raise TFPCanvasException.Create (ErrNoLock);
end;

function TFPCustomCanvas.Locked: boolean;
begin
  Result:=FLocks>0;
end;

{ Coordinate transformation support }

function TFPCustomCanvas.HasTransform: Boolean;
begin
  Result := (FMatrix._00 <> 1.0) or (FMatrix._01 <> 0.0) or
            (FMatrix._10 <> 0.0) or (FMatrix._11 <> 1.0) or
            (FMatrix._20 <> 0.0) or (FMatrix._21 <> 0.0);
end;

function TFPCustomCanvas.HasRotation: Boolean;
begin
  Result := (FMatrix._01 <> 0.0) or (FMatrix._10 <> 0.0);
end;

function TFPCustomCanvas.TransformPoint(X, Y: Integer): TPoint;
begin
  Result := FMatrix.Transform(X, Y);
end;

function TFPCustomCanvas.TransformRect(const R: TRect): TRect;
var
  P1, P2, P3, P4: TPoint;
begin
  P1 := FMatrix.Transform(R.Left, R.Top);
  P2 := FMatrix.Transform(R.Right, R.Top);
  P3 := FMatrix.Transform(R.Right, R.Bottom);
  P4 := FMatrix.Transform(R.Left, R.Bottom);
  Result.Left := Min(Min(P1.X, P2.X), Min(P3.X, P4.X));
  Result.Top := Min(Min(P1.Y, P2.Y), Min(P3.Y, P4.Y));
  Result.Right := Max(Max(P1.X, P2.X), Max(P3.X, P4.X));
  Result.Bottom := Max(Max(P1.Y, P2.Y), Max(P3.Y, P4.Y));
end;

function TFPCustomCanvas.TransformPoints(const Points: array of TPoint): TFPCanvasPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Points));
  for I := 0 to High(Points) do
    Result[I] := FMatrix.Transform(Points[I]);
end;

procedure TFPCustomCanvas.Translate(DX, DY: Double);
begin
  FMatrix := FMatrix.Multiply(TFPCanvasMatrix.CreateTranslation(DX, DY));
end;

procedure TFPCustomCanvas.Scale(SX, SY: Double);
begin
  FMatrix := FMatrix.Multiply(TFPCanvasMatrix.CreateScale(SX, SY));
end;

procedure TFPCustomCanvas.Rotate(ARadians: Double);
begin
  FMatrix := FMatrix.Multiply(TFPCanvasMatrix.CreateRotation(ARadians));
end;

procedure TFPCustomCanvas.ResetTransform;
begin
  FMatrix := TFPCanvasMatrix.Identity;
end;

function TFPCustomCanvas.GetNativeTextOrigin : TFPTextOrigin;
begin
  Result := toTop;
end;

function TFPCustomCanvas.GetNativeTextMeasure : TFPTextMeasure;
begin
  Result := tmFont;
end;

function TFPCustomCanvas.DoGetTextMetrics (out aMetrics: TFPTextMetric) : boolean;
begin
  aMetrics := Default(TFPTextMetric);
  Result := False;
end;

function TFPCustomCanvas.DoGetTextAdvance (text:ansistring) : integer;
begin
  Result := DoGetTextWidth(text);
end;

function TFPCustomCanvas.GetTextMetrics (out aMetrics: TFPTextMetric) : boolean;
begin
  if Font is TFPCustomDrawFont then
    Result := TFPCustomDrawFont(Font).DoGetTextMetrics(aMetrics)
  else
    Result := DoGetTextMetrics(aMetrics);
end;

{ The ascender is applied along the downward direction of the text, which turns with Font.Orientation. }
procedure TFPCustomCanvas.MoveToTextOrigin(var x, y: integer);
var
  lMetrics: TFPTextMetric;
  lShift, lAngle: Double;
begin
  if (FTextOrigin = GetNativeTextOrigin) or not GetTextMetrics(lMetrics) then
    exit;
  if FTextOrigin = toTop then
    lShift := lMetrics.Ascender
  else
    lShift := -lMetrics.Ascender;
  lAngle := DegToRad(Font.Orientation / 10);
  x := x + Round(lShift * Sin(lAngle));
  y := y + Round(lShift * Cos(lAngle));
end;

function TFPCustomCanvas.MeasuresByFont: boolean;
begin
  Result := (FTextMeasure = tmFont) and (GetNativeTextMeasure = tmInk);
end;

procedure TFPCustomCanvas.TextOut (x,y:integer;text:ansistring);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  MoveToTextOrigin(x, y);
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).DrawText(x,y, text)
  else
    DoTextOut (x,y, text);
end;

procedure TFPCustomCanvas.GetTextSize (text:ansistring; var w,h:integer);
begin
  if MeasuresByFont then
    begin
    w := TextWidth(text);
    h := TextHeight(text);
    end
  else if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).GetTextSize (text, w, h)
  else
    DoGetTextSize (Text, w, h);
end;

function TFPCustomCanvas.GetTextHeight (text:ansistring) : integer;
begin
  Result := TextHeight(Text);
end;

function TFPCustomCanvas.GetTextWidth (text:ansistring) : integer;
begin
  Result := TextWidth(Text);
end;

function TFPCustomCanvas.TextExtent(const Text: ansistring): TSize;
begin
  Result:=Default(TSize);
  GetTextSize(Text, Result.cx, Result.cy);
end;

function TFPCustomCanvas.TextHeight(const Text: ansistring): Integer;
var
  lMetrics: TFPTextMetric;
begin
  if MeasuresByFont and GetTextMetrics(lMetrics) then
    result := lMetrics.Height
  else if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextHeight (text)
  else
    result := DoGetTextHeight (Text);
end;

function TFPCustomCanvas.TextWidth(const Text: ansistring): Integer;
begin
  if MeasuresByFont then
    begin
    if Font is TFPCustomDrawFont then
      result := TFPCustomDrawFont(Font).DoGetTextAdvance (text)
    else
      result := DoGetTextAdvance (Text);
    end
  else if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextWidth (text)
  else
    result := DoGetTextWidth (Text);
end;

procedure TFPCustomCanvas.TextOut (x,y:integer;text:unicodestring);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  MoveToTextOrigin(x, y);
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).DrawText(x,y, text)
  else
    DoTextOut (x,y, text);
end;

procedure TFPCustomCanvas.GetTextSize (text:unicodestring; var w,h:integer);
begin
  if MeasuresByFont then
    begin
    w := TextWidth(text);
    h := TextHeight(text);
    end
  else if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).GetTextSize (text, w, h)
  else
    DoGetTextSize (Text, w, h);
end;

function TFPCustomCanvas.GetTextHeight (text:unicodestring) : integer;
begin
  Result := TextHeight(Text);
end;

function TFPCustomCanvas.GetTextWidth (text:unicodestring) : integer;
begin
  Result := TextWidth(Text);
end;

function TFPCustomCanvas.TextExtent(const Text: unicodestring): TSize;
begin
  Result:=Default(TSize);
  GetTextSize(Text, Result.cx, Result.cy);
end;

function TFPCustomCanvas.TextHeight(const Text: unicodestring): Integer;
var
  lMetrics: TFPTextMetric;
begin
  if MeasuresByFont and GetTextMetrics(lMetrics) then
    result := lMetrics.Height
  else if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextHeight (text)
  else
    result := DoGetTextHeight (Text);
end;

function TFPCustomCanvas.TextWidth(const Text: unicodestring): Integer;
begin
  if MeasuresByFont then
    begin
    if Font is TFPCustomDrawFont then
      result := TFPCustomDrawFont(Font).DoGetTextAdvance (text)
    else
      result := DoGetTextAdvance (UTF8Encode(Text));
    end
  else if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextWidth (text)
  else
    result := DoGetTextWidth (Text);
end;

procedure TFPCustomCanvas.DoTextOut (x,y:integer;text:unicodestring);

begin
  DoTextOut(x,y,UTF8Encode(text));
end;

procedure TFPCustomCanvas.DoGetTextSize (text:unicodestring; var w,h:integer);

begin
  DoGetTextSize(UTF8Encode(Text),w,h);
end;

function  TFPCustomCanvas.DoGetTextHeight (text:unicodestring) : integer;

begin
  Result:=DoGetTextHeight(UTF8Encode(text));
end;

function  TFPCustomCanvas.DoGetTextWidth (text:unicodestring) : integer;

begin
  Result:=DoGetTextWidth(UTF8Encode(text));
end;

{ Returns the points of the arc of the ellipse in aBounds (Right and Bottom
  included), from the ray at aStart16 over aLength16, in 1/16 degree
  counter-clockwise from 3 o'clock. }
function ArcPoints(const aBounds: TRect; aStart16, aLength16: Integer): TFPCanvasPointArray;
var
  cx, cy, rx, ry, a0, da, a, r: Double;
  n, i: Integer;
begin
  cx := (aBounds.Left + aBounds.Right) / 2;
  cy := (aBounds.Top + aBounds.Bottom) / 2;
  rx := (aBounds.Right - aBounds.Left) / 2;
  ry := (aBounds.Bottom - aBounds.Top) / 2;
  if aLength16 > 360*16 then
    aLength16 := 360*16
  else if aLength16 < -360*16 then
    aLength16 := -360*16;
  a0 := DegToRad(aStart16 / 16);
  da := DegToRad(aLength16 / 16);
  n := Ceil(Abs(da) * Max(rx, ry));
  if n < 1 then
    n := 1
  else if n > 100000 then
    n := 100000;
  SetLength(Result, n + 1);
  for i := 0 to n do
  begin
    a := a0 + da * i / n;
    if (rx = 0) or (ry = 0) then
      Result[i] := Point(Round(cx + rx * Cos(a)), Round(cy - ry * Sin(a)))
    else
    begin
      r := rx * ry / Sqrt(Sqr(ry * Cos(a)) + Sqr(rx * Sin(a)));
      Result[i] := Point(Round(cx + r * Cos(a)), Round(cy - r * Sin(a)));
    end;
  end;
end;

procedure TFPCustomCanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg,
  Angle16DegLength: Integer);
var
  R: TRect;
  Pts: TFPCanvasPointArray;
begin
  if (Pen.Style = psClear) or not DeviceRect(Rect(ALeft, ATop, ARight, ABottom), R) then
    exit;
  Pts := ArcPoints(R, Angle16Deg, Angle16DegLength);
  if Pen is TFPCustomDrawPen then
    TFPCustomDrawPen(Pen).Polyline(Pts, false)
  else
    DoPolyline(Pts);
end;

// Returns the angles in 1/16 degree of the rays from the centre of the bounds through (SX, SY) and on, counter-clockwise, to (EX, EY).
procedure RadialAngles(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer; out aStart16, aLength16: Integer);
var
  cx, cy: Double;
  lEnd: Integer;
begin
  cx := (ALeft + ARight) / 2;
  cy := (ATop + ABottom) / 2;
  aStart16 := Round(RadToDeg(ArcTan2(cy - SY, SX - cx)) * 16);
  lEnd := Round(RadToDeg(ArcTan2(cy - EY, EX - cx)) * 16);
  aLength16 := lEnd - aStart16;
  while aLength16 <= 0 do
    Inc(aLength16, 360*16);
end;

procedure TFPCustomCanvas.Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX,
  EY: Integer);
var
  lStart, lLength: Integer;
begin
  RadialAngles(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY, lStart, lLength);
  Arc(ALeft, ATop, ARight, ABottom, lStart, lLength);
end;

procedure TFPCustomCanvas.DoMoveTo (x,y:integer);
begin
end;

procedure TFPCustomCanvas.DoLineTo (x,y:integer);
begin
  DoLine (FPenPos.X,FPenPos.y, x,y);
end;

procedure TFPCustomCanvas.MoveTo (x,y:integer);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  FPenPos.x := x;
  FPenPos.y := y;
  DoMoveTo (x,y);
end;

procedure TFPCustomCanvas.MoveTo (p:TPoint);
begin
  if HasTransform then
    p := FMatrix.Transform(p);
  FPenPos := p;
  DoMoveTo (p.x,p.y);
end;

procedure TFPCustomCanvas.LineTo (x,y:integer);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  if Pen.Style <> psClear then
    if Pen is TFPCustomDrawPen then
      TFPCustomDrawPen(Pen).DrawLine (FPenPos.x, FPenPos.y, x, y)
    else
      DoLineTo (x,y);
  FPenPos.x := x;
  FPenPos.y := y;
end;

procedure TFPCustomCanvas.LineTo (p:TPoint);
begin
  LineTo (p.x, p.y);
end;

procedure TFPCustomCanvas.Line (x1,y1,x2,y2:integer);
var
  P1, P2: TPoint;
begin
  if HasTransform then
    begin
    P1 := TransformPoint(x1, y1);
    P2 := TransformPoint(x2, y2);
    x1 := P1.X; y1 := P1.Y;
    x2 := P2.X; y2 := P2.Y;
    end;
  if Pen.Style <> psClear then
    if Pen is TFPCustomDrawPen then
      TFPCustomDrawPen(Pen).DrawLine (x1,y1, x2,y2)
    else
      DoLine (x1,y1, x2,y2);
  FPenPos.x := x2;
  FPenPos.y := y2;
end;

procedure TFPCustomCanvas.Line (const p1,p2:TPoint);
begin
  Line (p1.x,p1.y,p2.x,p2.y);
end;

procedure TFPCustomCanvas.Line (const points:TRect);
begin
  with points do
    Line (left,top, right,bottom);
end;

procedure TFPCustomCanvas.Polyline (Const points:array of TPoint);
var
  TransPts: TFPCanvasPointArray;
begin
  if HasTransform then
    begin
    TransPts := TransformPoints(points);
    if Pen.Style <> psClear then
      if Pen is TFPCustomDrawPen then
        TFPCustomDrawPen(Pen).Polyline (TransPts, false)
      else
        DoPolyline (TransPts);
    FPenPos := TransPts[High(TransPts)];
    end
  else
    begin
    if Pen.Style <> psClear then
      if Pen is TFPCustomDrawPen then
        TFPCustomDrawPen(Pen).Polyline (points, false)
      else
        DoPolyline (points);
    FPenPos := points[high(points)];
    end;
end;

procedure TFPCustomCanvas.RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);
var
  R: TRect;
begin
  if FRectangleMode=rmExclude then
    begin
    if DeviceRect(Rect(x1, y1, x2, y2), R) then
      DoRadialPie(R.Left, R.Top, R.Right, R.Bottom, StartAngle16Deg, Angle16DegLength);
    end
  else if HasTransform then
    begin
    R := TransformRect(Rect(x1, y1, x2, y2));
    DoRadialPie(R.Left, R.Top, R.Right, R.Bottom, StartAngle16Deg, Angle16DegLength);
    end
  else
    DoRadialPie(X1, y1, x2, y2, StartAngle16Deg, Angle16DegLength);
end;

procedure TFPCustomCanvas.DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);
var
  ArcPts, Pts: TFPCanvasPointArray;
  i: Integer;
begin
  ArcPts := ArcPoints(Rect(x1, y1, x2, y2), StartAngle16Deg, Angle16DegLength);
  SetLength(Pts, Length(ArcPts) + 1);
  Pts[0] := Point(Round((x1 + x2) / 2), Round((y1 + y2) / 2));
  for i := 0 to High(ArcPts) do
    Pts[i + 1] := ArcPts[i];
  if Brush.Style <> bsClear then
    if Brush is TFPCustomDrawBrush then
      TFPCustomDrawBrush(Brush).Polygon(Pts)
    else
      DoPolygonFill(Pts);
  if Pen.Style <> psClear then
    if Pen is TFPCustomDrawPen then
      TFPCustomDrawPen(Pen).Polyline(Pts, true)
    else
      DoPolygon(Pts);
end;

procedure TFPCustomCanvas.DoPolyBezier(Points: PPoint; NumPts: Integer;
                           Filled: boolean = False;
                           Continuous: boolean = False);
{ Default implementation: flatten cubic Bezier curves into line segments
  using recursive De Casteljau subdivision.

  Point layout (following Windows GDI PolyBezier convention):
  - Non-continuous: groups of 4 points each (start, cp1, cp2, end).
    Total points must be 4*N for N segments.
  - Continuous: first segment uses points[0..3], subsequent segments
    use 3 points each (cp1, cp2, end) with start = previous end.
    Total points must be 4 + 3*(N-1) = 1 + 3*N for N segments.

  When Filled=false, draws the outline using DoLine.
  When Filled=true, collects all points and calls DoPolygonFill. }
const
  FlatnessTolerance = 0.5; { max pixel deviation before subdividing }

var
  PolyPts: array of TPoint;
  PolyCount: Integer;

  function PointAt(Index: Integer): TPoint; inline;
  begin
    Result := PPoint(Points + Index)^;
  end;

  procedure AddPolyPoint(const P: TPoint);
  begin
    if PolyCount >= Length(PolyPts) then
      SetLength(PolyPts, PolyCount * 2 + 16);
    PolyPts[PolyCount] := P;
    Inc(PolyCount);
  end;

  function IsFlat(const P0, P1, P2, P3: TPoint): Boolean;
  { Check if the control points are close enough to the line P0-P3 }
  var
    ux, uy, vx, vy: Double;
  begin
    ux := 3.0 * P1.X - 2.0 * P0.X - P3.X;
    uy := 3.0 * P1.Y - 2.0 * P0.Y - P3.Y;
    vx := 3.0 * P2.X - 2.0 * P3.X - P0.X;
    vy := 3.0 * P2.Y - 2.0 * P3.Y - P0.Y;
    if ux * ux < vx * vx then ux := vx;
    if uy * uy < vy * vy then uy := vy;
    Result := (ux * ux + uy * uy) <= 16.0 * FlatnessTolerance * FlatnessTolerance;
  end;

  procedure SubdivideCubic(const P0, P1, P2, P3: TPoint; Depth: Integer);
  { Recursively subdivide a cubic Bezier using De Casteljau at t=0.5 }
  var
    M01, M12, M23, M012, M123, Mid: TPoint;
  begin
    if (Depth > 12) or IsFlat(P0, P1, P2, P3) then
      begin
      if Filled then
        AddPolyPoint(P3)
      else
        DoLine(P0.X, P0.Y, P3.X, P3.Y);
      exit;
      end;
    { Split at t=0.5 }
    M01.X := (P0.X + P1.X) div 2;  M01.Y := (P0.Y + P1.Y) div 2;
    M12.X := (P1.X + P2.X) div 2;  M12.Y := (P1.Y + P2.Y) div 2;
    M23.X := (P2.X + P3.X) div 2;  M23.Y := (P2.Y + P3.Y) div 2;
    M012.X := (M01.X + M12.X) div 2;  M012.Y := (M01.Y + M12.Y) div 2;
    M123.X := (M12.X + M23.X) div 2;  M123.Y := (M12.Y + M23.Y) div 2;
    Mid.X := (M012.X + M123.X) div 2;  Mid.Y := (M012.Y + M123.Y) div 2;
    SubdivideCubic(P0, M01, M012, Mid, Depth + 1);
    SubdivideCubic(Mid, M123, M23, P3, Depth + 1);
  end;

var
  I, NumSegs, Idx: Integer;
  P0, P1, P2, P3: TPoint;
begin
  if NumPts < 4 then
    exit;
  { Calculate number of cubic segments }
  if Continuous then
    NumSegs := (NumPts - 1) div 3
  else
    NumSegs := NumPts div 4;
  if NumSegs < 1 then
    exit;
  { Initialize polygon point collection for filled mode }
  PolyCount := 0;
  PolyPts := nil;
  if Filled then
    AddPolyPoint(PointAt(0));
  { Process each cubic Bezier segment }
  Idx := 0;
  for I := 0 to NumSegs - 1 do
    begin
    if (I = 0) or (not Continuous) then
      begin
      P0 := PointAt(Idx);
      P1 := PointAt(Idx + 1);
      P2 := PointAt(Idx + 2);
      P3 := PointAt(Idx + 3);
      Inc(Idx, 4);
      if Filled and (I > 0) then
        AddPolyPoint(P0);
      end
    else
      begin
      { Continuous: start from previous endpoint }
      P0 := P3;
      P1 := PointAt(Idx);
      P2 := PointAt(Idx + 1);
      P3 := PointAt(Idx + 2);
      Inc(Idx, 3);
      end;
    SubdivideCubic(P0, P1, P2, P3, 0);
    end;
  if Filled and (PolyCount > 2) then
    begin
    SetLength(PolyPts, PolyCount);
    DoPolygonFill(PolyPts);
    DoPolygon(PolyPts);
    end;
end;

procedure TFPCustomCanvas.PolyBezier(Points: PPoint; NumPts: Integer;
                     Filled: boolean = False;
                     Continuous: boolean = False);
var
  TransPts: TFPCanvasPointArray;
  I: Integer;
begin
  if HasTransform then
    begin
    SetLength(TransPts, NumPts);
    for I := 0 to NumPts - 1 do
      TransPts[I] := FMatrix.Transform(PPoint(Points + I)^);
    DoPolyBezier(@TransPts[0], NumPts, Filled, Continuous);
    end
  else
    DoPolyBezier(Points, NumPts, Filled, Continuous);
end;

procedure TFPCustomCanvas.PolyBezier(const Points: array of TPoint;
                     Filled: boolean = False;
                     Continuous: boolean = False);
var
  NPoints: integer;
  TransPts: TFPCanvasPointArray;
begin
  NPoints:=High(Points)-Low(Points)+1;
  if NPoints>0 then
    begin
    if HasTransform then
      begin
      TransPts := TransformPoints(Points);
      DoPolyBezier(@TransPts[0], NPoints, Filled, Continuous);
      end
    else
      DoPolyBezier(@Points[Low(Points)], NPoints, Filled, Continuous);
    end;
end;

procedure TFPCustomCanvas.Clear;
var r : TRect;
begin
  if Brush.Style <> bsClear then
    begin
    if Brush is TFPCustomDrawBrush then
      TFPCustomDrawBrush(Brush).Rectangle(0,0, width-1, height-1)
    else
      begin
      r := Rect(0,0, width-1, height-1);
      DoRectangleFill (r);
      end;
    end;
end;

procedure TFPCustomCanvas.DrawPixel(const x, y: integer;
  const newcolor: TFPColor);
begin
  case FDrawingMode of
    dmOpaque: Colors[x,y] := newcolor;
    dmAlphaBlend: Colors[x,y] := AlphaBlend(Colors[x,y], newcolor);
    dmCustom: Colors[x,y] := FOnCombineColors(Colors[x,y], newcolor);
  end;
end;

procedure TFPCustomCanvas.BeginPenShape;
begin
  inc(FPenShapeLevel);
  if (FPenShapeLevel = 1) and ((Pen.Mode <> pmCopy) or (FDrawingMode <> dmOpaque)) then
  begin
    FPenShapeWidth := Width;
    FPenShapeHeight := Height;
    SetLength(FPenShapeDone, (Int64(FPenShapeWidth) * FPenShapeHeight + 7) div 8);
    if Length(FPenShapeDone) > 0 then
      FillChar(FPenShapeDone[0], Length(FPenShapeDone), 0);
  end;
end;

procedure TFPCustomCanvas.EndPenShape;
begin
  if FPenShapeLevel > 0 then
    dec(FPenShapeLevel);
  if FPenShapeLevel = 0 then
    FPenShapeDone := nil;
end;

procedure TFPCustomCanvas.DrawPenPixel(const x, y: integer; const newcolor: TFPColor);
var
  lIndex: Int64;
begin
  if FPenShapeDone <> nil then
  begin
    if (x < 0) or (y < 0) or (x >= FPenShapeWidth) or (y >= FPenShapeHeight) then
      exit;
    lIndex := Int64(y) * FPenShapeWidth + x;
    if (FPenShapeDone[lIndex shr 3] and (1 shl (lIndex and 7))) <> 0 then
      exit;
    FPenShapeDone[lIndex shr 3] := FPenShapeDone[lIndex shr 3] or (1 shl (lIndex and 7));
  end;
  if Pen.Mode = pmCopy then
    DrawPixel(x, y, newcolor)
  else
    Colors[x,y] := PenModeColor(Pen.Mode, newcolor, Colors[x,y]);
end;

procedure TFPCustomCanvas.Erase;
var
  x,y:Integer;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do
      Colors[x,y]:=colTransparent;
end;

function InterpolateColor(AStartColor, AEndColor: TFPColor; x, Total: Integer): TFPColor;
var
  f1, f2: Double;
begin
  f2 := x / Total;
  f1 := 1.0 - f2;
  Result.Red := round(AStartColor.Red * f1 + AEndColor.Red * f2);
  Result.Green := round(AStartColor.Green * f1 + AEndColor.Green * f2);
  Result.Blue := round(AStartColor.Blue * f1 + AEndColor.Blue * f2);
  Result.Alpha := round(AStartColor.Alpha * f1 + AEndColor.Alpha * f2);
end;

procedure TFPCustomCanvas.GradientFill(const ARect: TRect;
  AStartColor, AEndColor: TFPColor; ADirection: TFPGradientDirection);
var
  x, y, i, n: Integer;
  oldPenStyle: TFPPenStyle;
  oldPenWidth: Integer;
  oldPenColor: TFPColor;
  lRect: TRect;
begin
  if not UserRect(ARect, lRect) then
    exit;
  oldPenStyle := Pen.Style;
  oldPenWidth := Pen.Width;
  oldPenColor := Pen.FPColor;
  Pen.Style := psSolid;
  Pen.Width := 1;
  try
    if ADirection = gdVertical then
    begin
      n := lRect.Bottom - lRect.Top + 1;
      i := 0;
      for y := lRect.Top to lRect.Bottom do
      begin
        Pen.FPColor := InterpolateColor(AStartColor, AEndColor, i, n);
        Line(lRect.Left, y, lRect.Right, y);
        inc(i);
      end;
    end else
    begin
      n := lRect.Right - lRect.Left + 1;
      i := 0;
      for x := lRect.Left to lRect.Right do
      begin
        Pen.FPColor := InterpolateColor(AStartColor, AEndColor, i, n);
        Line(x, lRect.Top, x, lRect.Bottom);
        inc(i);
      end;
    end;
  finally
    Pen.Style := oldPenStyle;
    Pen.Width := oldPenWidth;
    Pen.FPColor := oldPenColor;
  end;
end;

procedure TFPCustomCanvas.DoRectangleAndFill (const Bounds:TRect);
begin
  DoRectangleFill (Bounds);
  DoRectangle (Bounds);
end;

procedure TFPCustomCanvas.DoEllipseAndFill (const Bounds:TRect);
begin
  DoEllipseFill (Bounds);
  DoEllipse (Bounds);
end;

procedure TFPCustomCanvas.DoPolygonAndFill (const points:array of TPoint);
begin
  DoPolygonFill (points);
  DoPolygon (points);
end;

procedure TFPCustomCanvas.Ellipse (const Bounds:TRect);
var
  p,b,dp,db,pb : boolean;
  TB: TRect;
begin
  if not DeviceRect(Bounds, TB) then
    exit;
  p := Pen.style <> psClear;
  b := Brush.style <> bsClear;
  pb := false;
  dp:=False;
  db:=False;
  if p and (Pen is TFPCustomDrawPen) then
      begin
      p := false;
      dp := true;
      end;
  if b and (Brush is TFPCustomDrawBrush) then
      begin
      b := false;
      db := true;
      end;
  if p and b then
    begin
    p := false;
    b := false;
    pb := true;
    end;
  if pb then
    DoEllipseAndFill (TB)
  else
    begin
    if p then
      DoEllipse (TB)
    else if dp then
      with TB do
        TFPCustomDrawPen(Pen).Ellipse (left,top,right,bottom);
    if b then
      DoEllipseFill (TB)
    else if db then
      with TB do
        TFPCustomDrawBrush(Brush).Ellipse (left,top,right,bottom);
    end;
end;

procedure TFPCustomCanvas.Ellipse (left,top,right,bottom:integer);
begin
  Ellipse (Rect(left,top,right,bottom));
end;

procedure TFPCustomCanvas.EllipseC (x,y:integer; rx,ry:longword);
begin
  if FRectangleMode=rmExclude then
    Ellipse (Rect(x-rx,y-ry,x+rx+1,y+ry+1))
  else
    Ellipse (Rect(x-rx,y-ry,x+rx,y+ry));
end;

procedure TFPCustomCanvas.Rectangle (left,top,right,bottom:integer);
begin
  Rectangle (Rect(left,top,right,bottom));
end;

procedure TFPCustomCanvas.FillRect(const ARect: TRect);
var
  TR: TRect;
begin
  if (Brush.style <> bsClear) and DeviceRect(ARect, TR) then
    begin
    if not (brush is TFPCustomDrawBrush) then
      DoRectangleFill (TR)
    else
      with TR do
        TFPCustomDrawBrush(Brush).Rectangle (left,top,right,bottom);
    end;
end;

procedure TFPCustomCanvas.FillRect(X1,Y1,X2,Y2: Integer);

begin
  FillRect (Rect(X1,Y1,X2,Y2));
end;

procedure TFPCustomCanvas.Rectangle (const Bounds:TRect);
var
  np,nb,dp,db: boolean;
  TB: TRect;
  Corners: array[0..3] of TPoint;
begin
  if HasTransform and HasRotation then
    begin
    // When rotation is active, convert rectangle to polygon with pre-transformed corners
    if not UserRect(Bounds, TB) then
      exit;
    Corners[0] := FMatrix.Transform(TB.Left, TB.Top);
    Corners[1] := FMatrix.Transform(TB.Right+1, TB.Top);
    Corners[2] := FMatrix.Transform(TB.Right+1, TB.Bottom+1);
    Corners[3] := FMatrix.Transform(TB.Left, TB.Bottom+1);
    np := Pen.style <> psClear;
    nb := Brush.style <> bsClear;
    if np and nb then
      DoPolygonAndFill(Corners)
    else
      begin
      if np then
        DoPolygon(Corners);
      if nb then
        DoPolygonFill(Corners);
      end;
    exit;
    end;
  if not DeviceRect(Bounds, TB) then
    exit;
  np:= Pen.style <> psClear; // Need pen ?
  nb:= Brush.style <> bsClear;  // Need brush ?
  dp:=(pen is TFPCustomDrawPen); // Pen draws ?
  db:=(brush is TFPCustomDrawBrush); // Brush draws ?
  if (np and nb) and not (db or dp) then
    DoRectangleAndFill (TB)
  else
    begin
    if np then
      begin
      If not dp then
        DoRectangle (TB)
      else
        with TB do
          TFPCustomDrawPen(Pen).Rectangle (left,top,right,bottom);
      end;
    if Nb then
      begin
      if not db then
        DoRectangleFill (TB)
      else
        with TB do
          TFPCustomDrawBrush(Brush).Rectangle (left,top,right,bottom);
      end;
    end;
end;

procedure TFPCustomCanvas.FloodFill (x,y:integer);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  if Brush.Style <> bsClear then
    begin
    if Brush is TFPCustomDrawBrush then
      TFPCustomDrawBrush (Brush).FloodFill (x,y)
    else
      DoFloodFill (x,y);
    end;
end;

procedure TFPCustomCanvas.Polygon (const points:array of TPoint);
var
  p,b,dp,db,pb : boolean;
  TransPts: TFPCanvasPointArray;

  procedure DoPolygonWork(const pts: array of TPoint);
  begin
    if pb then
      DoPolygonAndFill (pts)
    else
      begin
      if p then
        DoPolygon (pts)
      else if dp then
        TFPCustomDrawPen(Pen).Polyline (pts, true);
      if b then
        DoPolygonFill (pts)
      else if db then
        TFPCustomDrawBrush(Brush).Polygon (pts);
      end;
  end;

begin
  p := Pen.style <> psClear;
  b := Brush.style <> bsClear;
  dp:=false;
  db:=false;
  pb:=False;
  if p and (pen is TFPCustomDrawPen) then
      begin
      p := false;
      dp := true;
      end;
  if b and (brush is TFPCustomDrawBrush) then
      begin
      b := false;
      db := true;
      end;
  if p and b then
    begin
    p := false;
    b := false;
    pb := true;
    end;
  if HasTransform then
    begin
    TransPts := TransformPoints(points);
    DoPolygonWork(TransPts);
    end
  else
    DoPolygonWork(points);
end;

procedure TFPCustomCanvas.CopyRect (x,y:integer; canvas:TFPCustomCanvas;
  SourceRect:TRect);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  if UserRect (SourceRect, SourceRect) then
    DoCopyRect (x,y, canvas, SourceRect);
end;

procedure TFPCustomCanvas.DoCopyRect (x,y:integer; canvas:TFPCustomCanvas; Const SourceRect:TRect);
var
  yy,r,t : integer;
begin
  with SourceRect do begin
    for t := top to bottom do begin
      yy := t - top + y;
      for r := left to right do
        colors[r - left + x,yy] := canvas.colors[r,t];
    end;
  end;
end;

procedure TFPCustomCanvas.Draw (x,y:integer; image:TFPCustomImage);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  DoDraw (x,y, image);
end;

procedure TFPCustomCanvas.DoDraw (x,y:integer; Const image:TFPCustomImage);
var
  xx,xi,yi,xm,ym,r,t : integer;
begin
  xm := x + image.width-1;
  if (xm<0) or (x>width) then exit;
  if xm >= width then
    xm := width - 1;
  ym := y + image.height-1;
  if (ym<0) or (y>height) then exit;
  if ym >= height then
    ym := height - 1;
  xi := x;
  yi := y;
  if clipping then
    if not CheckRectClipping (DeviceClipRect, xi,yi, xm,ym) then
      exit;
  for r := xi to xm do
    begin
    xx := r - x;
    for t := yi to ym do
      DrawPixel(r,t, image.colors[xx,t-y]);
    end;
end;

procedure TFPCustomCanvas.StretchDraw(x, y, w, h: integer; source: TFPCustomImage);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  DoStretchDraw (x,y,w,h, source);
end;

procedure TFPCustomCanvas.DoStretchDraw(x, y, w, h: integer; source: TFPCustomImage);
var
    FreeInterpolation : boolean;
    IP : TFPCustomInterpolation;
begin
  FreeInterpolation := not assigned (FInterpolation);
  if FreeInterpolation then
    IP := TMitchellInterpolation.Create
  else
    IP := FInterpolation;
  try
    with IP do
      begin
      Initialize (source, self);
      Execute (x,y,w,h);
      end;
  finally
    if FreeInterpolation then
      IP.Free;
  end;
end;



// Returns the outline of the rectangle Bounds (Right and Bottom included) with corners rounded by RX x RY ellipses.
function RoundRectPoints(const Bounds: TRect; RX, RY: Integer): TFPCanvasPointArray;

  procedure Add(const aPoints: TFPCanvasPointArray);
  var
    i, n: Integer;
  begin
    n := Length(Result);
    SetLength(Result, n + Length(aPoints));
    for i := 0 to High(aPoints) do
      Result[n + i] := aPoints[i];
  end;

begin
  Result := nil;
  with Bounds do
    begin
    RX := Max(1, Min(RX, Right - Left + 1));
    RY := Max(1, Min(RY, Bottom - Top + 1));
    Add(ArcPoints(Rect(Right - RX + 1, Top, Right, Top + RY - 1), 0, 90*16));
    Add(ArcPoints(Rect(Left, Top, Left + RX - 1, Top + RY - 1), 90*16, 90*16));
    Add(ArcPoints(Rect(Left, Bottom - RY + 1, Left + RX - 1, Bottom), 180*16, 90*16));
    Add(ArcPoints(Rect(Right - RX + 1, Bottom - RY + 1, Right, Bottom), 270*16, 90*16));
    end;
end;

// Returns aText with each tab replaced by spaces up to the next multiple of 8 characters.
function ExpandTextTabs(const aText: string): string;
var
  i, lColumn: Integer;
begin
  Result := '';
  lColumn := 0;
  for i := 1 to Length(aText) do
    if aText[i] = #9 then
      repeat
        Result := Result + ' ';
        Inc(lColumn);
      until lColumn mod 8 = 0
    else
      begin
      Result := Result + aText[i];
      if (Ord(aText[i]) and $C0) <> $80 then
        Inc(lColumn);
      end;
end;

// Returns aLine with '&&' as '&' and its first single '&' removed; aIndex is the byte index of the character after it, or 0.
function StripTextPrefix(const aLine: string; out aIndex: Integer): string;
var
  i: Integer;
begin
  Result := '';
  aIndex := 0;
  i := 1;
  while i <= Length(aLine) do
    begin
    if aLine[i] = '&' then
      begin
      if (i < Length(aLine)) and (aLine[i + 1] = '&') then
        begin
        Result := Result + '&';
        Inc(i, 2);
        continue;
        end;
      if aIndex = 0 then
        begin
        aIndex := Length(Result) + 1;
        Inc(i);
        continue;
        end;
      end;
    Result := Result + aLine[i];
    Inc(i);
    end;
  if aIndex > Length(Result) then
    aIndex := 0;
end;

// Returns the byte length of the UTF-8 character at aText[aIndex].
function UTF8CharBytes(const aText: string; aIndex: Integer): Integer;
begin
  Result := 1;
  while (aIndex + Result <= Length(aText)) and ((Ord(aText[aIndex + Result]) and $C0) = $80) do
    Inc(Result);
end;

procedure TFPCustomCanvas.ShapePolygon(const Pts: array of TPoint);
begin
  if Length(Pts) = 0 then
    exit;
  if Brush.Style <> bsClear then
    DoPolygonFill(Pts);
  if Pen.Style <> psClear then
    DoPolygon(Pts);
end;

procedure TFPCustomCanvas.DoChord(const Bounds: TRect; aStart16, aLength16: integer);
begin
  ShapePolygon(ArcPoints(Bounds, aStart16, aLength16));
end;

procedure TFPCustomCanvas.DoRoundRect(const Bounds: TRect; RX, RY: integer);
begin
  if (RX < 2) or (RY < 2) then
    begin
    if Brush.Style <> bsClear then
      DoRectangleFill(Bounds);
    if Pen.Style <> psClear then
      DoRectangle(Bounds);
    end
  else
    ShapePolygon(RoundRectPoints(Bounds, RX, RY));
end;

{ The border area is filled run by run; hoDefault hatches it from the canvas origin, as the other flood fills. }
procedure TFPCustomCanvas.DoFloodFillStyle(x, y: integer; const FillColor: TFPColor; FillStyle: TFPFloodFillStyle);
var
  lDone: array of Boolean;
  lStack: array of TPoint;
  lRuns: array of TRect;
  lCount, lRunCount, lLeft, lRight, cx, ny, i: Integer;
  p: TPoint;
  lOrigin: THatchOrigin;

  function Fillable(ax, ay: Integer): Boolean;
  begin
    Result := not lDone[ay * Width + ax] and (Colors[ax, ay] <> FillColor);
  end;

  procedure Push(ax, ay: Integer);
  begin
    if lCount = Length(lStack) then
      SetLength(lStack, 2 * lCount + 64);
    lStack[lCount] := Point(ax, ay);
    Inc(lCount);
  end;

begin
  if (x < 0) or (y < 0) or (x >= Width) or (y >= Height) then
    exit;
  if FillStyle = ffSurface then
    begin
    if Colors[x, y] = FillColor then
      DoFloodFill(x, y);
    exit;
    end;
  SetLength(lDone, Width * Height);
  lStack := nil;
  lRuns := nil;
  lCount := 0;
  lRunCount := 0;
  Push(x, y);
  while lCount > 0 do
    begin
    Dec(lCount);
    p := lStack[lCount];
    if not Fillable(p.X, p.Y) then
      continue;
    lLeft := p.X;
    while (lLeft > 0) and Fillable(lLeft - 1, p.Y) do
      Dec(lLeft);
    lRight := p.X;
    while (lRight < Width - 1) and Fillable(lRight + 1, p.Y) do
      Inc(lRight);
    for cx := lLeft to lRight do
      lDone[p.Y * Width + cx] := True;
    if lRunCount = Length(lRuns) then
      SetLength(lRuns, 2 * lRunCount + 64);
    lRuns[lRunCount] := Rect(lLeft, p.Y, lRight, p.Y);
    Inc(lRunCount);
    for i := 0 to 1 do
      begin
      ny := p.Y - 1 + 2 * i;
      if (ny >= 0) and (ny < Height) then
        for cx := lLeft to lRight do
          if Fillable(cx, ny) then
            Push(cx, ny);
      end;
    end;
  lOrigin := FHatchOrigin;
  if FHatchOrigin = hoDefault then
    FHatchOrigin := hoCanvas;
  try
    for i := 0 to lRunCount - 1 do
      DoRectangleFill(lRuns[i]);
  finally
    FHatchOrigin := lOrigin;
  end;
end;

procedure TFPCustomCanvas.StretchDraw(const DestRect: TRect; source: TFPCustomImage);
var
  R: TRect;
begin
  if UserRect(DestRect, R) then
    StretchDraw(R.Left, R.Top, R.Right - R.Left + 1, R.Bottom - R.Top + 1, source);
end;

procedure TFPCustomCanvas.CopyRect(const Dest: TRect; SrcCanvas: TFPCustomCanvas; const Source: TRect);
var
  lDest, lSource: TRect;
  lImage: TFPMemoryImage;
  lX, lY: Integer;
begin
  if not (UserRect(Dest, lDest) and UserRect(Source, lSource)) then
    exit;
  if (lDest.Right - lDest.Left = lSource.Right - lSource.Left)
     and (lDest.Bottom - lDest.Top = lSource.Bottom - lSource.Top) then
    begin
    CopyRect(lDest.Left, lDest.Top, SrcCanvas, Source);
    exit;
    end;
  lImage := TFPMemoryImage.Create(lSource.Right - lSource.Left + 1, lSource.Bottom - lSource.Top + 1);
  try
    for lY := 0 to lImage.Height - 1 do
      for lX := 0 to lImage.Width - 1 do
        lImage.Colors[lX, lY] := SrcCanvas.Colors[lSource.Left + lX, lSource.Top + lY];
    StretchDraw(lDest.Left, lDest.Top, lDest.Right - lDest.Left + 1, lDest.Bottom - lDest.Top + 1, lImage);
  finally
    lImage.Free;
  end;
end;

procedure TFPCustomCanvas.ArcTo(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);
var
  R: TRect;
  lStart, lLength: Integer;
  P: TPoint;
begin
  if not UserRect(Rect(ALeft, ATop, ARight, ABottom), R) then
    exit;
  RadialAngles(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY, lStart, lLength);
  P := ArcPoints(R, lStart, 0)[0];
  LineTo(P.X, P.Y);
  Arc(ALeft, ATop, ARight, ABottom, lStart, lLength);
  P := ArcPoints(R, lStart + lLength, 0)[0];
  MoveTo(P.X, P.Y);
end;

procedure TFPCustomCanvas.AngleArc(X, Y: Integer; Radius: Longword; StartAngle, SweepAngle: Single);
var
  x1, y1, x2, y2: Integer;
  lSinStart, lCosStart, lSinEnd, lCosEnd: Single;
begin
  SinCos(Pi * StartAngle / 180, lSinStart, lCosStart);
  SinCos(Pi * (StartAngle + SweepAngle) / 180, lSinEnd, lCosEnd);
  x1 := Trunc(X + lCosStart * Radius);
  y1 := Trunc(Y - lSinStart * Radius);
  x2 := Trunc(X + lCosEnd * Radius);
  y2 := Trunc(Y - lSinEnd * Radius);
  LineTo(x1, y1);
  if SweepAngle > 0 then
    Arc(X - Integer(Radius), Y - Integer(Radius), X + Integer(Radius), Y + Integer(Radius), x1, y1, x2, y2)
  else
    Arc(X - Integer(Radius), Y - Integer(Radius), X + Integer(Radius), Y + Integer(Radius), x2, y2, x1, y1);
  MoveTo(x2, y2);
end;

procedure TFPCustomCanvas.Chord(x1, y1, x2, y2, Angle16Deg, Angle16DegLength: Integer);
var
  R: TRect;
begin
  if HasTransform and HasRotation then
    begin
    if UserRect(Rect(x1, y1, x2, y2), R) then
      ShapePolygon(TransformPoints(ArcPoints(R, Angle16Deg, Angle16DegLength)));
    end
  else if DeviceRect(Rect(x1, y1, x2, y2), R) then
    DoChord(R, Angle16Deg, Angle16DegLength);
end;

procedure TFPCustomCanvas.Chord(x1, y1, x2, y2, SX, SY, EX, EY: Integer);
var
  lStart, lLength: Integer;
begin
  RadialAngles(x1, y1, x2, y2, SX, SY, EX, EY, lStart, lLength);
  Chord(x1, y1, x2, y2, lStart, lLength);
end;

procedure TFPCustomCanvas.Pie(EllipseX1, EllipseY1, EllipseX2, EllipseY2, StartX, StartY, EndX, EndY: Integer);
var
  lStart, lLength: Integer;
begin
  RadialAngles(EllipseX1, EllipseY1, EllipseX2, EllipseY2, StartX, StartY, EndX, EndY, lStart, lLength);
  RadialPie(EllipseX1, EllipseY1, EllipseX2, EllipseY2, lStart, lLength);
end;

procedure TFPCustomCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX, RY: Integer);
var
  R: TRect;
begin
  if HasTransform and HasRotation then
    begin
    if UserRect(Rect(X1, Y1, X2, Y2), R) then
      ShapePolygon(TransformPoints(RoundRectPoints(R, RX, RY)));
    end
  else if DeviceRect(Rect(X1, Y1, X2, Y2), R) then
    begin
    if HasTransform then
      begin
      RX := Round(RX * Abs(FMatrix._00));
      RY := Round(RY * Abs(FMatrix._11));
      end;
    DoRoundRect(R, RX, RY);
    end;
end;

procedure TFPCustomCanvas.RoundRect(const Rect: TRect; RX, RY: Integer);
begin
  RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, RX, RY);
end;

procedure TFPCustomCanvas.Frame(const ARect: TRect);
var
  R: TRect;
  lCorners: array[0..3] of TPoint;
begin
  if Pen.Style = psClear then
    exit;
  if HasTransform and HasRotation then
    begin
    if not UserRect(ARect, R) then
      exit;
    lCorners[0] := FMatrix.Transform(R.Left, R.Top);
    lCorners[1] := FMatrix.Transform(R.Right + 1, R.Top);
    lCorners[2] := FMatrix.Transform(R.Right + 1, R.Bottom + 1);
    lCorners[3] := FMatrix.Transform(R.Left, R.Bottom + 1);
    DoPolygon(lCorners);
    end
  else if DeviceRect(ARect, R) then
    DoRectangle(R);
end;

procedure TFPCustomCanvas.Frame(X1, Y1, X2, Y2: Integer);
begin
  Frame(Rect(X1, Y1, X2, Y2));
end;

procedure TFPCustomCanvas.FrameRect(const ARect: TRect);
var
  R: TRect;
begin
  if (Brush.Style = bsClear) or not DeviceRect(ARect, R) then
    exit;
  with R do
    begin
    DoRectangleFill(Rect(Left, Top, Right, Top));
    if Bottom > Top then
      DoRectangleFill(Rect(Left, Bottom, Right, Bottom));
    if Bottom - Top >= 2 then
      begin
      DoRectangleFill(Rect(Left, Top + 1, Left, Bottom - 1));
      if Right > Left then
        DoRectangleFill(Rect(Right, Top + 1, Right, Bottom - 1));
      end;
    end;
end;

procedure TFPCustomCanvas.FrameRect(X1, Y1, X2, Y2: Integer);
begin
  FrameRect(Rect(X1, Y1, X2, Y2));
end;

{ As in the LCL, the bottom-left corner belongs to TopColor and the top-right corner to BottomColor. }
procedure TFPCustomCanvas.Frame3D(var ARect: TRect; const TopColor, BottomColor: TFPColor; const FrameWidth: integer);
var
  R: TRect;
  i, lWidth: Integer;
  lStyle: TFPBrushStyle;
  lColor: TFPColor;

  procedure Strip(aLeft, aTop, aRight, aBottom: Integer; const aColor: TFPColor);
  begin
    if (aRight < aLeft) or (aBottom < aTop) then
      exit;
    Brush.FPColor := aColor;
    DoRectangleFill(Rect(aLeft, aTop, aRight, aBottom));
  end;

begin
  if (FrameWidth <= 0) or not DeviceRect(ARect, R) then
    exit;
  lWidth := Min(FrameWidth, (Min(R.Right - R.Left, R.Bottom - R.Top) + 2) div 2);
  lStyle := Brush.Style;
  lColor := Brush.FPColor;
  Brush.Style := bsSolid;
  try
    for i := 1 to lWidth do
      with R do
        begin
        Strip(Left, Top, Left, Bottom, TopColor);
        Strip(Left + 1, Top, Right - 1, Top, TopColor);
        if Right > Left then
          Strip(Right, Top, Right, Bottom - 1, BottomColor);
        if Bottom > Top then
          Strip(Left + 1, Bottom, Right, Bottom, BottomColor);
        R := Rect(Left + 1, Top + 1, Right - 1, Bottom - 1);
        end;
  finally
    Brush.Style := lStyle;
    Brush.FPColor := lColor;
  end;
  InflateRect(ARect, -lWidth, -lWidth);
end;

procedure TFPCustomCanvas.DrawFocusRect(const ARect: TRect);
var
  lMode: TFPPenMode;
  lStyle: TFPPenStyle;
  lColor: TFPColor;
  lWidth: Integer;
begin
  lMode := Pen.Mode;
  lStyle := Pen.Style;
  lColor := Pen.FPColor;
  lWidth := Pen.Width;
  Pen.Mode := pmXor;
  Pen.Style := psDot;
  Pen.FPColor := colWhite;
  Pen.Width := 1;
  try
    Frame(ARect);
  finally
    Pen.Mode := lMode;
    Pen.Style := lStyle;
    Pen.FPColor := lColor;
    Pen.Width := lWidth;
  end;
end;

procedure TFPCustomCanvas.FloodFill(X, Y: Integer; const FillColor: TFPColor; FillStyle: TFPFloodFillStyle);
var
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(X, Y);
    X := P.X;
    Y := P.Y;
    end;
  if Brush.Style <> bsClear then
    DoFloodFillStyle(X, Y, FillColor, FillStyle);
end;

procedure TFPCustomCanvas.Polygon(const Points: array of TPoint; Winding: Boolean; StartIndex: Integer; NumPts: Integer);
var
  lPoints: TFPCanvasPointArray;
  lWinding: Boolean;
  i: Integer;
begin
  if NumPts < 0 then
    NumPts := Length(Points) - StartIndex;
  NumPts := Min(NumPts, Length(Points) - StartIndex);
  if (StartIndex < 0) or (NumPts <= 0) then
    exit;
  SetLength(lPoints, NumPts);
  for i := 0 to NumPts - 1 do
    lPoints[i] := Points[StartIndex + i];
  lWinding := FNonZeroWindingRule;
  FNonZeroWindingRule := Winding;
  try
    Polygon(lPoints);
  finally
    FNonZeroWindingRule := lWinding;
  end;
end;

procedure TFPCustomCanvas.Polygon(Points: PPoint; NumPts: Integer; Winding: boolean);
var
  lPoints: TFPCanvasPointArray;
  i: Integer;
begin
  if (Points = nil) or (NumPts <= 0) then
    exit;
  SetLength(lPoints, NumPts);
  for i := 0 to NumPts - 1 do
    lPoints[i] := Points[i];
  Polygon(lPoints, Winding);
end;

procedure TFPCustomCanvas.Polyline(const Points: array of TPoint; StartIndex: Integer; NumPts: Integer);
var
  lPoints: TFPCanvasPointArray;
  i: Integer;
begin
  if NumPts < 0 then
    NumPts := Length(Points) - StartIndex;
  NumPts := Min(NumPts, Length(Points) - StartIndex);
  if (StartIndex < 0) or (NumPts <= 0) then
    exit;
  SetLength(lPoints, NumPts);
  for i := 0 to NumPts - 1 do
    lPoints[i] := Points[StartIndex + i];
  Polyline(lPoints);
end;

procedure TFPCustomCanvas.Polyline(Points: PPoint; NumPts: Integer);
var
  lPoints: TFPCanvasPointArray;
  i: Integer;
begin
  if (Points = nil) or (NumPts <= 0) then
    exit;
  SetLength(lPoints, NumPts);
  for i := 0 to NumPts - 1 do
    lPoints[i] := Points[i];
  Polyline(lPoints);
end;

procedure TFPCustomCanvas.WrapTextLine(const aLine: string; aWidth: integer; aLines: TStrings);
var
  lCurrent, lWord, lTry: string;
  lStart, lWordStart, i: Integer;
begin
  lCurrent := '';
  lStart := 1;
  i := 1;
  while i <= Length(aLine) + 1 do
    begin
    lWordStart := i;
    while (i <= Length(aLine)) and (aLine[i] <> ' ') do
      Inc(i);
    lWord := Copy(aLine, lWordStart, i - lWordStart);
    if lCurrent = '' then
      begin
      lCurrent := lWord;
      lStart := lWordStart;
      end
    else
      begin
      lTry := lCurrent + ' ' + lWord;
      if TextWidth(lTry) > aWidth then
        begin
        aLines.AddObject(lCurrent, TObject(PtrInt(lStart)));
        lCurrent := lWord;
        lStart := lWordStart;
        end
      else
        lCurrent := lTry;
      end;
    Inc(i);
    end;
  aLines.AddObject(lCurrent, TObject(PtrInt(lStart)));
end;

function TFPCustomCanvas.EllipsisText(const aLine: string; aWidth: integer): string;
var
  j: Integer;
begin
  Result := aLine;
  if TextWidth(aLine) <= aWidth then
    exit;
  while (Result <> '') and (TextWidth(Result + '...') > aWidth) do
    begin
    j := Length(Result);
    while (j > 1) and ((Ord(Result[j]) and $C0) = $80) do
      Dec(j);
    SetLength(Result, j - 1);
    end;
  Result := Result + '...';
end;

procedure TFPCustomCanvas.TextRect(const ARect: TRect; X, Y: integer; const Text: string);
begin
  TextRect(ARect, X, Y, Text, FTextStyle);
end;

{ Lines are laid out top-down with TextOrigin toTop and font metrics; a clip region that is not a
  rectangle is detached for the duration and put back afterwards. }
procedure TFPCustomCanvas.TextRect(ARect: TRect; X, Y: integer; const Text: string; const Style: TFPTextStyle);
var
  lBox, lClip, lOldClip: TRect;
  lRaw, lLines: TStringList;
  lPrefixes: array of Integer;
  lMetrics: TFPTextMetric;
  lHasMetrics, lClipped, lOldClipping: Boolean;
  lOldRegion: TFPCustomRegion;
  lOldOrigin: TFPTextOrigin;
  lOldMeasure: TFPTextMeasure;
  lLineHeight, lLeft, lRight, lTop, lTotal, lX, lBlock, lPrefix, lStart, i, j, n, lUX, lUY: Integer;
  lLine, s: string;
  lPenColor: TFPColor;
  lPenStyle: TFPPenStyle;
  lPenMode: TFPPenMode;
  lPenWidth: Integer;

  // Returns the x of a line of aWidth pixels, by the alignment.
  function LineX(aWidth: Integer): Integer;
  begin
    case Style.Alignment of
      taRightJustify: Result := lRight - aWidth;
      taCenter: Result := lBox.Left + (lRight - lBox.Left - aWidth) div 2;
    else
      Result := lLeft;
    end;
  end;

begin
  if not UserRect(ARect, lBox) then
    exit;
  lRight := lBox.Right + 1;
  lOldOrigin := FTextOrigin;
  lOldMeasure := FTextMeasure;
  FTextOrigin := toTop;
  FTextMeasure := tmFont;
  lClipped := False;
  lOldRegion := nil;
  lOldClipping := False;
  lRaw := TStringList.Create;
  lLines := TStringList.Create;
  try
    lHasMetrics := GetTextMetrics(lMetrics);
    if lHasMetrics then
      lLineHeight := lMetrics.Height
    else
      lLineHeight := TextHeight('Hg');
    if Style.Alignment = taLeftJustify then
      lLeft := X
    else
      lLeft := lBox.Left;
    s := Text;
    if Style.ExpandTabs then
      s := ExpandTextTabs(s);
    if Style.SingleLine then
      lRaw.Add(s)
    else
      lRaw.Text := s;
    lPrefixes := nil;
    for i := 0 to lRaw.Count - 1 do
      begin
      lLine := lRaw[i];
      lPrefix := 0;
      if Style.ShowPrefix then
        lLine := StripTextPrefix(lLine, lPrefix);
      n := lLines.Count;
      if Style.Wordbreak and not Style.SingleLine then
        WrapTextLine(lLine, lRight - lLeft, lLines)
      else if Style.EndEllipsis then
        lLines.AddObject(EllipsisText(lLine, lRight - lLeft), TObject(PtrInt(1)))
      else
        lLines.AddObject(lLine, TObject(PtrInt(1)));
      SetLength(lPrefixes, lLines.Count);
      for j := n to lLines.Count - 1 do
        begin
        lStart := PtrInt(lLines.Objects[j]);
        lPrefixes[j] := 0;
        if (lPrefix >= lStart) and (lPrefix < lStart + Length(lLines[j])) then
          lPrefixes[j] := lPrefix - lStart + 1;
        end;
      end;
    lTotal := lLines.Count * lLineHeight;
    case Style.Layout of
      ftlCenter: lTop := lBox.Top + (lBox.Bottom + 1 - lBox.Top - lTotal) div 2;
      ftlBottom: lTop := lBox.Bottom + 1 - lTotal;
    else
      lTop := Y;
    end;
    if Style.Clipping then
      begin
      lOldClipping := Clipping;
      lOldClip := ClipRect;
      // ClipRect is in device pixels, in the convention of RectangleMode
      if not DeviceRect(ARect, lClip) then
        exit;
      if FRectangleMode = rmExclude then
        begin
        Inc(lClip.Right);
        Inc(lClip.Bottom);
        end;
      if lOldClipping and Assigned(FClipRegion) then
        begin
        lClip.Left := Max(lClip.Left, lOldClip.Left);
        lClip.Top := Max(lClip.Top, lOldClip.Top);
        lClip.Right := Min(lClip.Right, lOldClip.Right);
        lClip.Bottom := Min(lClip.Bottom, lOldClip.Bottom);
        end;
      lOldRegion := FClipRegion;
      FClipRegion := nil;
      lClipped := True;
      ClipRect := lClip;
      Clipping := True;
      end;
    if Style.Opaque and (lLines.Count > 0) then
      begin
      lBlock := 0;
      for i := 0 to lLines.Count - 1 do
        lBlock := Max(lBlock, TextWidth(lLines[i]));
      lX := LineX(lBlock);
      if FRectangleMode = rmExclude then
        FillRect(Rect(lX, lTop, lX + lBlock, lTop + lTotal))
      else
        FillRect(Rect(lX, lTop, lX + lBlock - 1, lTop + lTotal - 1));
      end;
    for i := 0 to lLines.Count - 1 do
      begin
      lLine := lLines[i];
      lX := LineX(TextWidth(lLine));
      TextOut(lX, lTop + i * lLineHeight, lLine);
      if lPrefixes[i] > 0 then
        begin
        lUX := lX + TextWidth(Copy(lLine, 1, lPrefixes[i] - 1));
        if lHasMetrics then
          lUY := lTop + i * lLineHeight + lMetrics.Ascender + 1
        else
          lUY := lTop + (i + 1) * lLineHeight - 1;
        lPenColor := Pen.FPColor;
        lPenStyle := Pen.Style;
        lPenMode := Pen.Mode;
        lPenWidth := Pen.Width;
        Pen.FPColor := Font.FPColor;
        Pen.Style := psSolid;
        Pen.Mode := pmCopy;
        Pen.Width := 1;
        try
          Line(lUX, lUY, lUX + TextWidth(Copy(lLine, lPrefixes[i], UTF8CharBytes(lLine, lPrefixes[i]))) - 1, lUY);
        finally
          Pen.FPColor := lPenColor;
          Pen.Style := lPenStyle;
          Pen.Mode := lPenMode;
          Pen.Width := lPenWidth;
        end;
        end;
      end;
  finally
    if lClipped then
      begin
      ClipRegion := lOldRegion;
      if Assigned(lOldRegion) and (lOldRegion is TFPRectRegion) then
        ClipRect := lOldClip;
      Clipping := lOldClipping;
      end;
    FTextOrigin := lOldOrigin;
    FTextMeasure := lOldMeasure;
    lLines.Free;
    lRaw.Free;
  end;
end;

function TFPCustomCanvas.TextFitInfo(const Text: string; MaxWidth: Integer): Integer;
var
  i, n: Integer;
begin
  Result := 0;
  i := 1;
  while i <= Length(Text) do
    begin
    n := UTF8CharBytes(Text, i);
    if TextWidth(Copy(Text, 1, i + n - 1)) > MaxWidth then
      exit;
    Inc(Result);
    Inc(i, n);
    end;
end;
