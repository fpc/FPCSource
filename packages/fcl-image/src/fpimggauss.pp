{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team

    fpImage Gaussian blur routines by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

}

{$IFNDEF FPC_DOTTEDUNITS}
unit FPImgGauss;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Math, System.Classes, FpImage;
{$ELSE FPC_DOTTEDUNITS}
uses
  Math, Classes, FPimage;
{$ENDIF FPC_DOTTEDUNITS}

{ Fast Gaussian blur to Area (excluding Area.Right and Area.Bottom)
  Pixels outside the image are treated as having the same color as the edge.
  This is a binominal approximation of fourth degree, so it is pretty near the
  real gaussian blur in most cases but much faster for big radius.
  Runtime: O((Area.Width+Radius) * (Area.Height+Radius))  }
procedure GaussianBlurBinominal4(AImg: TFPCustomImage; Radius: integer;
  SrcArea: TRect);
procedure GaussianBlurBinominal4(SrcImg, DestImg: TFPCustomImage; Radius: integer;
  SrcArea: TRect; DestXY: TPoint);

{ Gaussian blur to Area (excluding Area.Right and Area.Bottom)
  Pixels outside the image are treated as having the same color as the edge.
  Runtime: O(Area.Width * Area.Height * Radius)  }
procedure GaussianBlur(Img: TFPCustomImage; Radius: integer; Area: TRect);

{ MatrixBlur1D
  The Matrix1D has a width of Radius*2+1.
  The sum of all entries in the Matrix1D must be <= 65536.
  Create Matrix1D with ComputeGaussianBlurMatrix1D.
  Each pixel x,y in Area is replaced by a pixel computed from all pixels in
  x-Radius..x+Radius, y-Radius..y+Radius
  The new value is the sum of all pixels multiplied by Matrix1D, once
  horizontally and once vertically.

  Pixels outside the image are treated as having the same color as the edge.

  Runtime is O(Area width * Area height * Radius) }
procedure MatrixBlur1D(Img: TFPCustomImage; Radius: integer; Area: TRect; Matrix1D: PWord);

{ MatrixBlur2D
  The Matrix2D is quadratic and has a width of Radius*2+1.
  The sum of all entries in the Matrix2D must be <= 65536.
  Create Matrix2D with ComputeGaussianBlurMatrix2D.
  Each pixel x,y in Area (Left..Right-1,Top..Bottom-1) is replaced by a pixel
  computed from all pixels in x-Radius..x+Radius, y-Radius..y+Radius.
  The new value is the sum of all pixels multiplied by Matrix2D.

  Pixels outside the image are treated as having the same color as the edge.

  Runtime is O(Area width * Area height * Radius * Radius) }
procedure MatrixBlur2D(Img: TFPCustomImage; Radius: integer; Area: TRect; Matrix2D: PWord);

{ ComputeGaussianBlurMatrix1D creates a one dimensional matrix of size
  Width = Radius*2+1

  Deviation := Radius / 3
  G(x) := (1 / SQRT( 2 * pi * Deviation^2)) * e^( - (x^2) / (2 * Deviation^2) )

  Each word is a factor [0,1) multiplied by 65536.
  The total sum of the matrix is 65536. }
function ComputeGaussianBlurMatrix1D(Radius: integer): PWord;

{ ComputeGaussianBlurMatrix2D creates a two dimensional matrix of quadratic size
  Width = Radius*2+1

  Deviation := Radius / 3
  G(x,y) := (1 / (2 * pi * Deviation^2)) * e^( - (x^2 + y^2) / (2 * Deviation^2) )

  Each word is a factor [0,1) multiplied by 65536.
  The total sum of the matrix is 65536. }
function ComputeGaussianBlurMatrix2D(Radius: integer): PWord;

implementation

type
  // A colour with real channels for the running sums of the blurs.
  TRealColor = record
    red, green, blue, alpha: double;
  end;
  TRealColorArray = array of TRealColor;

// Returns aColor with real channels.
function ToRealColor(const aColor: TFPColor): TRealColor;
begin
  Result.red := aColor.red;
  Result.green := aColor.green;
  Result.blue := aColor.blue;
  Result.alpha := aColor.alpha;
end;

// Returns aColor rounded and limited to 0..$FFFF.
function ToFPColor(const aColor: TRealColor): TFPColor;

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
  Result.red := ToWord(aColor.red);
  Result.green := ToWord(aColor.green);
  Result.blue := ToWord(aColor.blue);
  Result.alpha := ToWord(aColor.alpha);
end;

{ Replaces each of the n values a[i] by the average of a[i-aLeft]..a[i+aRight];
  positions before the first or after the last value use that value. tmp is
  work space for n values. }
procedure BoxPass(var a, tmp: TRealColorArray; n, aLeft, aRight: integer);
var
  i, j, w: integer;
  sum: TRealColor;

  procedure Add(const c: TRealColor; f: double);
  begin
    sum.red := sum.red + f * c.red;
    sum.green := sum.green + f * c.green;
    sum.blue := sum.blue + f * c.blue;
    sum.alpha := sum.alpha + f * c.alpha;
  end;

begin
  w := aLeft + aRight + 1;
  sum.red := 0; sum.green := 0; sum.blue := 0; sum.alpha := 0;
  for j := -aLeft to aRight do
    Add(a[Min(Max(j, 0), n - 1)], 1);
  for i := 0 to n - 1 do
  begin
    tmp[i].red := sum.red / w;
    tmp[i].green := sum.green / w;
    tmp[i].blue := sum.blue / w;
    tmp[i].alpha := sum.alpha / w;
    Add(a[Min(i + aRight + 1, n - 1)], 1);
    Add(a[Max(i - aLeft, 0)], -1);
  end;
  for i := 0 to n - 1 do
    a[i] := tmp[i];
end;

{ Blurs the n values of a with four box averages of width Radius; for an even
  Radius two lean left and two right of each value, so the blur stays centred. }
procedure BinominalBlur1D(var a, tmp: TRealColorArray; n, Radius: integer);
var
  l, r: integer;
begin
  l := (Radius - 1) div 2;
  r := Radius - 1 - l;
  BoxPass(a, tmp, n, l, r);
  BoxPass(a, tmp, n, r, l);
  BoxPass(a, tmp, n, l, r);
  BoxPass(a, tmp, n, r, l);
end;

procedure GaussianBlurBinominal4(AImg: TFPCustomImage; Radius: integer;
  SrcArea: TRect);
begin
  GaussianBlurBinominal4(AImg,AImg,Radius,SrcArea,SrcArea.TopLeft);
end;

procedure GaussianBlurBinominal4(SrcImg, DestImg: TFPCustomImage;
  Radius: integer; SrcArea: TRect; DestXY: TPoint);
var
  x, y, margin, colLeft, colRight, cols, rows, n, i: integer;
  line, tmp, vert: TRealColorArray;
begin
  // clip
  if SrcArea.Left<0 then begin
    dec(DestXY.X,SrcArea.Left);
    SrcArea.Left:=0;
  end;
  if SrcArea.Top<0 then begin
    dec(DestXY.Y,SrcArea.Top);
    SrcArea.Top:=0;
  end;
  if DestXY.X<0 then begin
    dec(SrcArea.Left,DestXY.X);
    DestXY.X:=0;
  end;
  if DestXY.Y<0 then begin
    dec(SrcArea.Top,DestXY.Y);
    DestXY.Y:=0;
  end;
  SrcArea.Right:=Min(SrcImg.Width,SrcArea.Right);
  SrcArea.Bottom:=Min(SrcImg.Height,SrcArea.Bottom);
  SrcArea.Right:=Min(SrcArea.Right,DestImg.Width-DestXY.X+SrcArea.Left);
  SrcArea.Bottom:=Min(SrcArea.Bottom,DestImg.Height-DestXY.Y+SrcArea.Top);
  if SrcArea.Left>=SrcArea.Right then exit;
  if SrcArea.Top>=SrcArea.Bottom then exit;

  if Radius<1 then begin
    if (SrcImg<>DestImg) or (DestXY.X<>SrcArea.Left) or (DestXY.Y<>SrcArea.Top) then
      for y:=SrcArea.Top to SrcArea.Bottom-1 do
        for x:=SrcArea.Left to SrcArea.Right-1 do
          DestImg.Colors[DestXY.X+x-SrcArea.Left,DestXY.Y+y-SrcArea.Top]:=SrcImg.Colors[x,y];
    exit;
  end;

  // positions beyond the image edge take the edge pixel; the blur reads up to 2*Radius pixels away
  margin:=2*Radius;
  colLeft:=Max(0,SrcArea.Left-margin);
  colRight:=Min(SrcImg.Width-1,SrcArea.Right-1+margin);
  cols:=colRight-colLeft+1;
  rows:=SrcArea.Bottom-SrcArea.Top;
  SetLength(vert,cols*rows);

  // vertical pass over every column the horizontal pass needs, margin columns included
  n:=rows+2*margin;
  SetLength(line,n);
  SetLength(tmp,n);
  for x:=colLeft to colRight do begin
    for i:=0 to n-1 do
      line[i]:=ToRealColor(SrcImg.Colors[x,Min(Max(SrcArea.Top-margin+i,0),SrcImg.Height-1)]);
    BinominalBlur1D(line,tmp,n,Radius);
    for y:=0 to rows-1 do
      vert[y*cols+x-colLeft]:=line[y+margin];
  end;

  // horizontal pass
  n:=(SrcArea.Right-SrcArea.Left)+2*margin;
  SetLength(line,n);
  SetLength(tmp,n);
  for y:=0 to rows-1 do begin
    for i:=0 to n-1 do
      line[i]:=vert[y*cols+Min(Max(SrcArea.Left-margin+i,colLeft),colRight)-colLeft];
    BinominalBlur1D(line,tmp,n,Radius);
    for x:=SrcArea.Left to SrcArea.Right-1 do
      DestImg.Colors[DestXY.X+x-SrcArea.Left,DestXY.Y+y]:=ToFPColor(line[x-SrcArea.Left+margin]);
  end;
end;

procedure GaussianBlur(Img: TFPCustomImage; Radius: integer; Area: TRect);
var
  Matrix: PWord;
begin
  // check input
  if (Radius<1) then exit;
  Area.Left:=Max(0,Area.Left);
  Area.Top:=Max(0,Area.Top);
  Area.Right:=Min(Area.Right,Img.Width);
  Area.Bottom:=Min(Area.Bottom,Img.Height);
  if (Area.Left>=Area.Right) or (Area.Top>=Area.Bottom) then exit;

  // compute gaussian matrix
  Matrix:=ComputeGaussianBlurMatrix1D(Radius);
  try
    MatrixBlur1D(Img,Radius,Area,Matrix);
  finally
    FreeMem(Matrix);
  end;
end;

// Returns the colour whose channels are aRed, aGreen, aBlue and aAlpha divided by 65536, each limited to $FFFF.
function WeightedColor(const aRed, aGreen, aBlue, aAlpha: QWord): TFPColor;
begin
  Result.red:=Min(aRed shr 16,$FFFF);
  Result.green:=Min(aGreen shr 16,$FFFF);
  Result.blue:=Min(aBlue shr 16,$FFFF);
  Result.alpha:=Min(aAlpha shr 16,$FFFF);
end;

procedure MatrixBlur1D(Img: TFPCustomImage; Radius: integer; Area: TRect;
  Matrix1D: PWord);
{ The vertical sums of every column the horizontal pass needs are computed
  from the original pixels before any pixel is replaced, and are not rounded. }
type
  TColorSums = record
    red, green, blue, alpha: QWord;
  end;
var
  x, y, xd, yd, StartX, EndX, SumWidth: Integer;
  VertSums: array of TColorSums;
  NewRed, NewGreen, NewBlue, NewAlpha: QWord;
  Col: TFPColor;
  Sums: TColorSums;
  Multiplier: Word;
begin
  // check input
  if (Radius<1) then exit;
  Area.Left:=Max(0,Area.Left);
  Area.Top:=Max(0,Area.Top);
  Area.Right:=Min(Area.Right,Img.Width);
  Area.Bottom:=Min(Area.Bottom,Img.Height);
  if (Area.Left>=Area.Right) or (Area.Top>=Area.Bottom) then exit;

  StartX:=Area.Left-Radius;
  EndX:=Area.Right-1+Radius;
  SumWidth:=EndX-StartX+1;
  SetLength(VertSums,SumWidth*(Area.Bottom-Area.Top));
  // vertical sums (coordinates out of the image are mapped to the edges)
  for y:=Area.Top to Area.Bottom-1 do
    for x:=StartX to EndX do begin
      NewRed:=0; NewGreen:=0; NewBlue:=0; NewAlpha:=0;
      for yd:=-Radius to Radius do begin
        Col:=Img.Colors[Min(Max(0,x),Img.Width-1),Min(Max(0,y+yd),Img.Height-1)];
        Multiplier:=Matrix1D[yd+Radius];
        inc(NewRed,QWord(Col.red)*Multiplier);
        inc(NewGreen,QWord(Col.green)*Multiplier);
        inc(NewBlue,QWord(Col.blue)*Multiplier);
        inc(NewAlpha,QWord(Col.alpha)*Multiplier);
      end;
      Sums.red:=NewRed;
      Sums.green:=NewGreen;
      Sums.blue:=NewBlue;
      Sums.alpha:=NewAlpha;
      VertSums[(y-Area.Top)*SumWidth+x-StartX]:=Sums;
    end;
  // horizontal sums
  for y:=Area.Top to Area.Bottom-1 do
    for x:=Area.Left to Area.Right-1 do begin
      NewRed:=0; NewGreen:=0; NewBlue:=0; NewAlpha:=0;
      for xd:=-Radius to Radius do begin
        Sums:=VertSums[(y-Area.Top)*SumWidth+x+xd-StartX];
        Multiplier:=Matrix1D[xd+Radius];
        inc(NewRed,Sums.red*Multiplier);
        inc(NewGreen,Sums.green*Multiplier);
        inc(NewBlue,Sums.blue*Multiplier);
        inc(NewAlpha,Sums.alpha*Multiplier);
      end;
      Img.Colors[x,y]:=WeightedColor(NewRed shr 16,NewGreen shr 16,NewBlue shr 16,NewAlpha shr 16);
    end;
end;

procedure MatrixBlur2D(Img: TFPCustomImage; Radius: integer; Area: TRect;
  Matrix2D: PWord);
{ Before any pixel is replaced, the original pixels of the area and of a border
  of Radius around it are copied; border positions outside the image take the
  nearest edge pixel. }
var
  x, y, xd, yd, MatrixWidth, OrigLeft, OrigTop, OrigWidth, OrigHeight: Integer;
  OrigPixels: array of TFPColor;
  NewRed, NewGreen, NewBlue, NewAlpha: QWord;
  Col: TFPColor;
  Multiplier: Word;
begin
  // check input
  if (Radius<1) then exit;
  Area.Left:=Max(0,Area.Left);
  Area.Top:=Max(0,Area.Top);
  Area.Right:=Min(Area.Right,Img.Width);
  Area.Bottom:=Min(Area.Bottom,Img.Height);
  if (Area.Left>=Area.Right) or (Area.Top>=Area.Bottom) then exit;

  MatrixWidth:=Radius*2+1;
  OrigLeft:=Area.Left-Radius;
  OrigTop:=Area.Top-Radius;
  OrigWidth:=Area.Right-Area.Left+2*Radius;
  OrigHeight:=Area.Bottom-Area.Top+2*Radius;
  SetLength(OrigPixels,OrigWidth*OrigHeight);
  for y:=0 to OrigHeight-1 do
    for x:=0 to OrigWidth-1 do
      OrigPixels[y*OrigWidth+x]:=Img.Colors[Min(Max(0,OrigLeft+x),Img.Width-1),
                                            Min(Max(0,OrigTop+y),Img.Height-1)];
  for y:=Area.Top to Area.Bottom-1 do
    for x:=Area.Left to Area.Right-1 do begin
      NewRed:=0; NewGreen:=0; NewBlue:=0; NewAlpha:=0;
      for yd:=-Radius to Radius do
        for xd:=-Radius to Radius do begin
          Col:=OrigPixels[(y+yd-OrigTop)*OrigWidth+x+xd-OrigLeft];
          Multiplier:=Matrix2D[xd+Radius+(yd+Radius)*MatrixWidth];
          inc(NewRed,QWord(Col.red)*Multiplier);
          inc(NewGreen,QWord(Col.green)*Multiplier);
          inc(NewBlue,QWord(Col.blue)*Multiplier);
          inc(NewAlpha,QWord(Col.alpha)*Multiplier);
        end;
      Img.Colors[x,y]:=WeightedColor(NewRed,NewGreen,NewBlue,NewAlpha);
    end;
end;

function ComputeGaussianBlurMatrix1D(Radius: integer): PWord;
// returns a 1dim matrix of Words for the gaussian blur.
// Each word is a factor [0,1) multiplied by 65536.
// The total sum of the matrix is 65536.
const
  StandardDeviationToRadius = 3; // Pixels more far away as 3*Deviation are too small
var
  Width: Integer;
  Matrix: PWord;
  Deviation, p, total: double;
  g: array of double;
  x: Integer;
  MatrixSum: Integer;
begin
  Width:=Radius*2+1;
  GetMem(Matrix,SizeOf(Word)*Width);
  Result:=Matrix;
  // G(x) = e^(-x^2 / (2 * Deviation^2)), normalized to a sum of 1
  Deviation:=Radius/StandardDeviationToRadius;
  if Deviation<=0 then
    Deviation:=1;
  p:=-1/(2*Deviation*Deviation);
  SetLength(g,Radius+1);
  total:=0;
  for x:=0 to Radius do begin
    g[x]:=exp(x*x*p);
    if x=0 then
      total:=total+g[x]
    else
      total:=total+2*g[x];
  end;
  for x:=0 to Radius do begin
    Matrix[Radius+x]:=Floor(g[x]/total*65536);
    Matrix[Radius-x]:=Matrix[Radius+x];
  end;
  // fix sum to 65536
  MatrixSum:=0;
  for x:=0 to Width-1 do
    inc(MatrixSum,Matrix[x]);
  Matrix[Radius]:=Min(High(Word),65536-MatrixSum+Matrix[Radius]);
end;

function ComputeGaussianBlurMatrix2D(Radius: integer): PWord;
// returns a 2dim matrix of Words for the gaussian blur.
// Each word is a factor [0,1) multiplied by 65536.
// The total sum of the matrix is 65536.
const
  StandardDeviationToRadius = 3; // Pixels more far away as 3*Deviation are too small
var
  Matrix: PWord;
  Width, x, y, MatrixSum: Integer;
  Deviation, p, total: double;
  g: array of double;
begin
  Width:=Radius*2+1;
  GetMem(Matrix,SizeOf(Word)*Width*Width);
  Result:=Matrix;
  // G(x,y) = e^(-(x^2 + y^2) / (2 * Deviation^2)), normalized to a sum of 1
  Deviation:=Radius/StandardDeviationToRadius;
  if Deviation<=0 then
    Deviation:=1;
  p:=-1/(2*Deviation*Deviation);
  SetLength(g,Width*Width);
  total:=0;
  for y:=0 to Width-1 do
    for x:=0 to Width-1 do begin
      g[y*Width+x]:=exp((Sqr(x-Radius)+Sqr(y-Radius))*p);
      total:=total+g[y*Width+x];
    end;
  MatrixSum:=0;
  for y:=0 to Width-1 do
    for x:=0 to Width-1 do begin
      Matrix[y*Width+x]:=Floor(g[y*Width+x]/total*65536);
      inc(MatrixSum,Matrix[y*Width+x]);
    end;
  // fix sum to 65536
  Matrix[Radius+Radius*Width]:=Min(High(Word),65536-MatrixSum+Matrix[Radius+Radius*Width]);
end;

end.
