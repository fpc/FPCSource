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
  else Result:=FClipRegion.GetBoundingRect();
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
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).DrawText(x,y, text)
  else
    DoTextOut (x,y, text);
end;

procedure TFPCustomCanvas.GetTextSize (text:ansistring; var w,h:integer);
begin
  if Font is TFPCustomDrawFont then
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
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextHeight (text)
  else
    result := DoGetTextHeight (Text);
end;

function TFPCustomCanvas.TextWidth(const Text: ansistring): Integer;
begin
  if Font is TFPCustomDrawFont then
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
  if Font is TFPCustomDrawFont then
    TFPCustomDrawFont(Font).DrawText(x,y, text)
  else
    DoTextOut (x,y, text);
end;

procedure TFPCustomCanvas.GetTextSize (text:unicodestring; var w,h:integer);
begin
  if Font is TFPCustomDrawFont then
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
begin
  if Font is TFPCustomDrawFont then
    result := TFPCustomDrawFont(Font).GetTextHeight (text)
  else
    result := DoGetTextHeight (Text);
end;

function TFPCustomCanvas.TextWidth(const Text: unicodestring): Integer;
begin
  if Font is TFPCustomDrawFont then
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

procedure TFPCustomCanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg,
  Angle16DegLength: Integer);
begin

end;

procedure TFPCustomCanvas.Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX,
  EY: Integer);
begin

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
  if HasTransform then
    begin
    R := TransformRect(Rect(x1, y1, x2, y2));
    DoRadialPie(R.Left, R.Top, R.Right, R.Bottom, StartAngle16Deg, Angle16DegLength);
    end
  else
    DoRadialPie(X1, y1, x2, y2, StartAngle16Deg, Angle16DegLength);
end;

procedure TFPCustomCanvas.DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);

begin
  // To be implemented
end;

procedure TFPCustomCanvas.DoPolyBezier(Points: PPoint; NumPts: Integer;
                           Filled: boolean = False;
                           Continuous: boolean = False);
{ Default implementation: flatten cubic Bezier curves into line segments
  using recursive De Casteljau subdivision.

  Point layout (following Windows GDI PolyBezier convention):
  - Non-continuous: groups of 4 points each (start, cp1, cp2, end).
    Total points must be 1 + 3*N for N segments.
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
  NumSegs := (NumPts - 1) div 3;
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
      TFPCustomDrawBrush(Brush).Rectangle(0,0, width, height)
    else
      begin
      r := Rect(0,0, width, height);
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
begin
  oldPenStyle := Pen.Style;
  oldPenWidth := Pen.Width;
  oldPenColor := Pen.FPColor;
  Pen.Style := psSolid;
  Pen.Width := 1;

  if ADirection = gdVertical then
  begin
    n := ARect.Bottom - ARect.Top;
    if n = 0 then
      exit;
    i := 0;
    for y := ARect.Top to ARect.Bottom - 1 do
    begin
      Pen.FPColor := InterpolateColor(AStartColor, AEndColor, i, n);
      Line(ARect.Left, y, ARect.Right - 1, y);
      inc(i);
    end;
  end else
  begin
    n := ARect.Right - ARect.Left;
    if n = 0 then
      exit;
    i := 0;
    for x := ARect.Left to ARect.Right - 1 do
    begin
      Pen.FPColor := InterpolateColor(AStartColor, AEndColor, i, n);
      Line(x, ARect.Top, x, ARect.Bottom - 1);
      inc(i);
    end;
  end;

  Pen.Style := oldPenStyle;
  Pen.Width := oldPenWidth;
  Pen.FPColor := oldPenColor;
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
  if HasTransform then
    TB := TransformRect(Bounds)
  else
    TB := Bounds;
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
  if (Brush.style <> bsClear) then
    begin
    if HasTransform then
      TR := TransformRect(ARect)
    else
      TR := ARect;
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
    Corners[0] := FMatrix.Transform(Bounds.Left, Bounds.Top);
    Corners[1] := FMatrix.Transform(Bounds.Right, Bounds.Top);
    Corners[2] := FMatrix.Transform(Bounds.Right, Bounds.Bottom);
    Corners[3] := FMatrix.Transform(Bounds.Left, Bounds.Bottom);
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
  if HasTransform then
    TB := TransformRect(Bounds)
  else
    TB := Bounds;
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
  yy,r,t : integer;
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
  SortRect (SourceRect);
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
  xx,xi,yi,xm,ym,r,t : integer;
  P: TPoint;
begin
  if HasTransform then
    begin
    P := TransformPoint(x, y);
    x := P.X;
    y := P.Y;
    end;
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
    if not CheckRectClipping (ClipRect, xi,yi, xm,ym) then
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
    FreeInterpolation : boolean;
    IP : TFPCustomInterpolation;
begin
  FreeInterpolation := not assigned (FInterpolation);
  if FreeInterpolation then
    IP := TMitchelInterpolation.Create
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


