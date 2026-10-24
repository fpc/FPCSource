{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Some more interpolation filters implementation for TFPCanvas.StretchDraw:

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ TFPCustomInterpolation }

procedure TFPCustomInterpolation.Initialize(aimage: TFPCustomImage; acanvas: TFPCustomCanvas);
begin
  fimage := aimage;
  fcanvas := acanvas;
end;

type
  TInterpolationKernel = function (x: double): double of object;

  // The source pixels of one destination pixel: source pixel Start+i has weight Weights[i].
  TPixelWeights = record
    Start: integer;
    Weights: array of double;
  end;
  TPixelWeightTable = array of TPixelWeights;

{ Returns for each of NewSize destination pixels the weights of the OldSize
  source pixels. Shrinking weights every source pixel by the part of the
  destination pixel it covers. Enlarging samples source position
  (OldSize-1)*(i+0.5)/NewSize with Kernel over Support, the source extended
  by its edge pixels; without Kernel the nearest source pixel is taken. }
function PixelWeightTable(OldSize, NewSize: integer; Kernel: TInterpolationKernel;
  Support: double): TPixelWeightTable;
var
  i, j, k, lo, hi: integer;
  a, b, factor, p, w, sum: double;
  acc: array of double;
begin
  Result := nil;
  SetLength(Result, NewSize);
  SetLength(acc, OldSize);
  for i := 0 to NewSize-1 do
  begin
    for k := 0 to OldSize-1 do
      acc[k] := 0;
    if NewSize = OldSize then
      acc[i] := 1
    else if NewSize < OldSize then
    begin
      factor := OldSize / NewSize;
      a := i * factor;
      b := (i + 1) * factor;
      for j := Floor(a) to Min(Ceil(b), OldSize) - 1 do
        acc[j] := (Min(b, j + 1) - Max(a, j)) / factor;
    end
    else if not Assigned(Kernel) then
      acc[Min(OldSize-1, Floor((i + 0.5) * OldSize / NewSize))] := 1
    else
    begin
      p := (OldSize - 1) * (i + 0.5) / NewSize;
      for j := Floor(p - Support) to Ceil(p + Support) do
        if Abs(p - j) < Support then
        begin
          w := Kernel(Abs(p - j));
          k := Min(Max(j, 0), OldSize - 1);
          acc[k] := acc[k] + w;
        end;
      sum := 0;
      for k := 0 to OldSize-1 do
        sum := sum + acc[k];
      if Abs(sum) < 1e-9 then
      begin
        for k := 0 to OldSize-1 do
          acc[k] := 0;
        acc[Min(OldSize-1, Max(0, Round(p)))] := 1;
      end
      else
        for k := 0 to OldSize-1 do
          acc[k] := acc[k] / sum;
    end;
    lo := 0;
    while (lo < OldSize - 1) and (acc[lo] = 0) do
      inc(lo);
    hi := OldSize - 1;
    while (hi > lo) and (acc[hi] = 0) do
      dec(hi);
    Result[i].Start := lo;
    SetLength(Result[i].Weights, hi - lo + 1);
    for k := lo to hi do
      Result[i].Weights[k - lo] := acc[k];
  end;
end;

{ Draws Image scaled to the w x h pixels at (x, y) of Canvas; each destination
  pixel mixes the source pixels XTable and YTable give it, colours weighted by
  their alpha. }
procedure ResampleImage(Image: TFPCustomImage; Canvas: TFPCustomCanvas; x, y, w, h: integer;
  const XTable, YTable: TPixelWeightTable);
type
  TPremultiplied = record
    r, g, b, a: double;
  end;
var
  rows: array of TPremultiplied;
  sx, sy, dx, dy, k: integer;
  f: double;
  c: TFPColor;
  acc: TPremultiplied;

  function ToWord(v: double): word;
  begin
    if v <= 0 then
      Result := 0
    else if v >= $FFFF then
      Result := $FFFF
    else
      Result := Round(v);
  end;

begin
  // horizontal pass over every source row, premultiplied by alpha
  SetLength(rows, Image.Height * w);
  for sy := 0 to Image.Height - 1 do
    for dx := 0 to w - 1 do
    begin
      acc.r := 0; acc.g := 0; acc.b := 0; acc.a := 0;
      with XTable[dx] do
        for k := 0 to High(Weights) do
        begin
          f := Weights[k];
          c := Image.Colors[Start + k, sy];
          acc.r := acc.r + f * c.Red * c.Alpha;
          acc.g := acc.g + f * c.Green * c.Alpha;
          acc.b := acc.b + f * c.Blue * c.Alpha;
          acc.a := acc.a + f * c.Alpha;
        end;
      rows[sy * w + dx] := acc;
    end;
  // vertical pass
  for dy := 0 to h - 1 do
    for dx := 0 to w - 1 do
    begin
      acc.r := 0; acc.g := 0; acc.b := 0; acc.a := 0;
      with YTable[dy] do
        for k := 0 to High(Weights) do
        begin
          f := Weights[k];
          sx := (Start + k) * w + dx;
          acc.r := acc.r + f * rows[sx].r;
          acc.g := acc.g + f * rows[sx].g;
          acc.b := acc.b + f * rows[sx].b;
          acc.a := acc.a + f * rows[sx].a;
        end;
      if acc.a > 0.5 then
      begin
        c.Red := ToWord(acc.r / acc.a);
        c.Green := ToWord(acc.g / acc.a);
        c.Blue := ToWord(acc.b / acc.a);
      end
      else
      begin
        c.Red := 0;
        c.Green := 0;
        c.Blue := 0;
      end;
      c.Alpha := ToWord(acc.a);
      Canvas.DrawPixel(x + dx, y + dy, c);
    end;
end;

{ TFPBoxInterpolation }

procedure TFPBoxInterpolation.Execute(x,y,w,h:integer);
begin
  if (w<=0) or (h<=0) or (Image.Width=0) or (Image.Height=0) then
    exit;
  ResampleImage(Image, Canvas, x, y, w, h,
    PixelWeightTable(Image.Width, w, nil, 0), PixelWeightTable(Image.Height, h, nil, 0));
end;

{ TFPBaseInterpolation }

procedure TFPBaseInterpolation.Execute(x, y, w, h: integer);
// paint Image on Canvas at x,y,w*h
begin
  if (w<=0) or (h<=0) or (image.Width=0) or (image.Height=0) then
    exit;
  ResampleImage(Image, Canvas, x, y, w, h,
    PixelWeightTable(Image.Width, w, @Filter, MaxSupport),
    PixelWeightTable(Image.Height, h, @Filter, MaxSupport));
end;

function TFPBaseInterpolation.Filter(x: double): double;
begin
  Result := 1.0 - Abs(x);
end;

function TFPBaseInterpolation.MaxSupport: double;
begin
  Result:=1.0;
end;

{ TMitchellInterpolation }

function TMitchellInterpolation.Filter(x: double): double;
const
  B  = (1.0/3.0);
  C  = (1.0/3.0);
  P0 = ((  6.0- 2.0*B       )/6.0);
  P2 = ((-18.0+12.0*B+ 6.0*C)/6.0);
  P3 = (( 12.0- 9.0*B- 6.0*C)/6.0);
  Q0 = ((       8.0*B+24.0*C)/6.0);
  Q1 = ((     -12.0*B-48.0*C)/6.0);
  Q2 = ((       6.0*B+30.0*C)/6.0);
  Q3 = ((     - 1.0*B- 6.0*C)/6.0);
begin
  if (x < -2.0) then
    result := 0.0
  else if (x < -1.0) then
    result := Q0-x*(Q1-x*(Q2-x*Q3))
  else if (x < 0.0) then
    result := P0+x*x*(P2-x*P3)
  else if (x < 1.0) then
    result := P0+x*x*(P2+x*P3)
  else if (x < 2.0) then
    result := Q0+x*(Q1+x*(Q2+x*Q3))
  else
  result := 0.0;
end;

function TMitchellInterpolation.MaxSupport: double;
begin
  result := 2.0;
end;

