{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by the Free Pascal development team

    Polygon filling routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit PolygonFillTools;

interface

uses
  Math, Classes, FPImage, FPCanvas, PixTools;

procedure FillPolygonSolid(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Color: TFPColor);

procedure FillPolygonHorizontal(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Color: TFPColor; Width: Integer);

procedure FillPolygonVertical(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Color: TFPColor; Width: Integer);

procedure FillPolygonDiagonal(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Color: TFPColor; Width: Integer);

procedure FillPolygonBackDiagonal(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Color: TFPColor; Width: Integer);

procedure FillPolygonPattern(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Color: TFPColor; Pattern: TBrushPattern);

procedure FillPolygonImage(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Image: TFPCustomImage; Relative: Boolean);

type
  TPolygonInfoData = record
    P: TPoint;               // Intersection point
    Index1, Index2: Integer; // Indices of adjacent polygon vertices
  end;
  PPolygonInfoData = ^TPolygonInfoData;

  TPolygonInfo = class
  private
    FAngle: Double;
    FInfoList: TFPList;
    FPoints: array of TPoint;
    FOrigBounds: TRect;
    FRotBounds: TRect;
    procedure ClearList;
    procedure FreeList;
    function GetCount: Integer;
    function GetInfoData(AIndex: Integer): PPolygonInfoData;
  protected
    procedure AddInfoData(X, Y, Index1, Index2: Integer);
    function CalcBounds(const APoints: Array of TPoint): TRect;
    procedure DeleteInfoAt(AIndex: Integer);
    procedure GetIntersections(Position: Integer);
    procedure RespectWindingRule(Position: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GatherPolygonInfos(Position: Integer; Winding: Boolean);
    procedure UsePoints(const APoints: Array of TPoint; Angle: Double);

    property OrigBounds: TRect read FOrigBounds;
    property RotBounds: TRect read FRotBounds;
    property Count: Integer read GetCount;
    property Data[AIndex: Integer]: PPolygonInfoData read GetInfoData;
  end;


implementation

{ Helper routines }

function CrossProduct(P, P1, P2: TPoint): Integer;
var
  a, b: TPoint;
begin
  a := P - P1;
  b := P2 - P1;
  Result := a.X * b.Y - b.X * a.Y;
end;

procedure CalcWindingNumber(const P, P1, P2: TPoint; var WindingNumber: Integer);
begin
  if CrossProduct(P, P1, P2) > 0 then
    inc(windingNumber)
  else
    dec(windingNumber);
end;

function RotatePoint(const APoint: TPoint; Angle: Double): TPoint;
var
  sa, ca: Double;
begin
  sincos(Angle,sa,ca);
  Result.X := Round( ca * APoint.X + sa * APoint.Y);
  Result.Y := Round(-sa * APoint.X + ca * APoint.Y);
end;

function CompareX(Item1, Item2: Pointer): Integer;
begin
  Result := PPolygonInfoData(Item1)^.P.X - PPolygonInfoData(Item2)^.P.X;
end;


{ TPolygonInfo }

constructor TPolygonInfo.Create;
begin
  inherited Create;
  FInfoList := TFPList.Create;
end;

destructor TPolygonInfo.Destroy;
begin
  FreeList;
  inherited;
end;

procedure TPolygonInfo.AddInfoData(X, Y, Index1, Index2: Integer);
var
  d: PPolygonInfoData;
begin
  New(d);
  d^.P := Point(X, Y);
  d^.Index1 := Index1;
  d^.Index2 := Index2;
  FInfoList.Add(d);
end;

{ Determines the bounding box of the (rotated) polygon points. }
function TPolygonInfo.CalcBounds(const APoints: array of TPoint): TRect;
var
  i: Integer;
begin
  Result := Rect(MaxInt, MaxInt, -MaxInt-1, -MaxInt-1);
  for i := low(APoints) to High(APoints) do
  begin
    if APoints[i].X < Result.Left then Result.Left := APoints[i].X;
    if APoints[i].Y < Result.Top then Result.Top := APoints[i].Y;
    if APoints[i].X > Result.Right then Result.Right := APoints[i].X;
    if APoints[i].Y > Result.Bottom then Result.Bottom := APoints[i].Y;
  end;
end;

procedure TPolygonInfo.ClearList;
var
  i: integer;
  infoData: PPolygonInfoData;
begin
  if Assigned(FInfoList) then
  begin
    for i := 0 to FInfoList.Count-1 do
    begin
      infoData := PPolygonInfoData(FInfoList[i]);
      Dispose(infoData);
    end;
    FInfoList.Clear;
  end;
end;

procedure TPolygonInfo.DeleteInfoAt(AIndex: Integer);
var
  infoData: PPolygonInfoData;
begin
  infoData := PPolygonInfoData(FInfoList[AIndex]);
  Dispose(infoData);
  FInfoList.Delete(AIndex);
end;

procedure TPolygonInfo.FreeList;
begin
  if Assigned(FInfoList) then
  begin
    ClearList;
    FInfoList.Free;
    FInfoList := nil;
  end;
end;

procedure TPolygonInfo.GatherPolygonInfos(Position: Integer; Winding: Boolean);
var
  i: Integer;
  infoData: PPolygonInfoData;
  P: TPoint;
begin
  ClearList;

  // Calculate intersection points
  GetIntersections(Position);

  // Sort nodes from left to right
  FInfoList.Sort(@CompareX);

  // Remove intersection points so that winding rule is obeyed.
  if Winding and (Length(FPoints) > 3) then
    RespectWindingRule(Position);

  // Rotate intersection points back
  if FAngle <> 0.0 then
    for i := 0 to FInfoList.Count-1 do
    begin
      infoData := GetInfoData(i);
      P := RotatePoint(infoData^.P, -FAngle);
      infoData^.P := P;
    end;
end;

function TPolygonInfo.GetCount: Integer;
begin
  Result := FInfoList.Count;
end;

function TPolygonInfo.GetInfoData(AIndex: Integer): PPolygonInfoData;
begin
  Result := PPolygonInfoData(FInfoList[AIndex]);
end;

{ Calculates the intersection points of a horizontal line at the specified
  y position with the (rotated) polygon edges. Stores the intersection points
  in the FInfoList. }
procedure TPolygonInfo.GetIntersections(Position: Integer);
var
  numPoints: Integer;
  i, j: Integer;
  x: Integer;
begin
  numPoints := Length(FPoints);
  j := numPoints - 1;
  for i := 0 to numPoints-1 do
  begin
    if ((FPoints[i].Y < Position) and (FPoints[j].Y >= Position)) or
       ((FPoints[j].Y < Position) and (FPoints[i].Y >= Position)) then
    begin
      if FPoints[j].Y = FPoints[i].Y then
        x := FPoints[i].X
      else
        x := Round(FPoints[i].X + (Position - FPoints[i].Y) / (FPoints[j].Y - FPoints[i].Y) * (FPoints[j].X - FPoints[i].X));
      AddInfoData(x, Position, j, i);
    end;
    j := i;
  end;
end;

{ Removes intersection points from FInfoList which are "inside" the polygon at
  a given y position. The decision whether a point is "inside" is made based
  on the non-zero winding rule. }
procedure TPolygonInfo.RespectWindingRule(Position: Integer);
var
  i: Integer;
  windingNumber, prevWindingNumber: Integer;
  d: PPolygonInfoData;
begin
  windingNumber := 0;
  prevWindingNumber := 0;
  i := 0;
  while i < FInfoList.Count do
  begin
    d := GetInfoData(i);
    CalcWindingNumber(Point(FRotBounds.Left-10, Position), FPoints[d^.Index1], FPoints[d^.Index2], windingNumber);
    if (prevWindingNumber <> 0) and (windingNumber <> 0) then
      // This crossing point is inside
      DeleteInfoAt(i)
    else
      inc(i);
    prevWindingNumber := windingNumber;
  end;
end;

{ Gets the polygon points to the handled.
  If Angle is not zero the points will be rotated so that internally only
  intersections with horizontal lines must be calculated.
  The found intersection points are rotated back by -Angle before the user can
  access them. }
procedure TPolygonInfo.UsePoints(const APoints: array of TPoint; Angle: Double);
var
  i: Integer;
begin
  FAngle := Angle;

  // Determine the bounding box of the original polygon
  FOrigBounds := CalcBounds(APoints);

  // Rotate points by hatch direction (if needed)
  SetLength(FPoints, Length(APoints));
  if Angle = 0 then
  begin
    for i := Low(APoints) to High(APoints) do
      FPoints[i] := APoints[i];
    FRotBounds := FOrigBounds;
  end else
  begin
    for i := Low(APoints) to High(APoints) do
      FPoints[i] := RotatePoint(APoints[i], Angle);
    // Determine the bounding box of the rotated polygon
    FRotBounds := CalcBounds(FPoints);
  end;
end;


{ General-purpose fill procedure }

procedure InternalFillColor(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Color: TFPColor; Angle: Double; Step: Integer);
var
  info: TPolygonInfo;
  i, y: Integer;
  P1, P2: TPoint;
begin
  info := TPolygonInfo.Create;
  try
    info.UsePoints(Points, Angle);
    // Note: y and RotBounds refer to the ROTATED polygon
    y := info.RotBounds.Top + Step;
    while y <= info.RotBounds.Bottom do
    begin
      info.GatherPolygonInfos(y, Winding);
      i := 0;
      // Fill the pixels between pairs of intersection points
      while i < info.Count do
      begin
        // Note: P1 and P2 are in the original coordinate system again
        P1 := info.Data[i]^.P;
        P2 := info.Data[i+1]^.P;
        DrawSolidLine(Canv, P1.X, P1.Y, P2.X, P2.Y, Color);
        inc(i, 2);
      end;
      inc(y, Step);
    end;
  finally
    info.Free;
  end;
end;


{ Public procedures }

procedure FillPolygonSolid(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Color: TFPColor);
begin
  InternalFillColor(Canv, Points, Winding, Color, 0.0, 1);
end;

procedure FillPolygonHorizontal(Canv: TFPCustomCanvas; const Points: Array of TPoint;
  Winding: Boolean; Color: TFPColor; Width: Integer);
begin
  InternalFillColor(Canv, Points, Winding, Color, 0.0, Width);
end;

procedure FillPolygonVertical(Canv: TFPCustomCanvas; const Points: Array of TPoint;
  Winding: Boolean; Color: TFPColor; Width: Integer);
begin
  InternalFillColor(Canv, Points, Winding, Color, pi/2, Width);
end;

procedure FillPolygonDiagonal(Canv: TFPCustomCanvas; const Points: Array of TPoint;
  Winding: Boolean; Color: TFPColor; Width: Integer);
begin
  InternalFillColor(Canv, Points, Winding, Color, pi/4, Width);
end;

procedure FillPolygonBackDiagonal(Canv: TFPCustomCanvas; const Points: Array of TPoint;
  Winding: Boolean; Color: TFPColor; Width: Integer);
begin
  InternalFillColor(Canv, Points, Winding, Color, -pi/4, Width);
end;

procedure FillPolygonPattern(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Color: TFPColor; Pattern: TBrushPattern);

  procedure DrawPatternLine(x1, x2, y: Integer; PatternRow: TPenPattern);
  var
    x: Integer;
    pixNo: Byte;
    pixValue: TPenPattern;
  begin
    pixNo := x1 mod patternBitCount;
    pixValue := 1 shl pixNo;
    for x := x1 to x2 do
    begin
      if pixValue and PatternRow <> 0 then
        Canv.DrawPixel(x, y, Color);
      if pixNo = patternBitCount-1 then
      begin
        pixNo := 0;
        pixValue := 1;
      end else
      begin
        inc(pixNo);
        pixValue := pixvalue shl 1;
      end;
    end;
  end;

var
  info: TPolygonInfo;
  i, x1, x2, y: Integer;
  patternHeight: Integer;
  patternRow: TPenPattern;
begin
  patternHeight := Length(Pattern);
  info := TPolygonInfo.Create;
  try
    info.UsePoints(Points, 0.0);
    for y := info.OrigBounds.Top to info.OrigBounds.Bottom do
    begin
      info.GatherPolygonInfos(y, Winding);
      i := 0;
      // Fill the pixels between pairs of intersection points
      while i < info.Count do
      begin
        x1 := info.Data[i]^.P.X;
        x2 := info.Data[i+1]^.P.X;
        patternRow := Pattern[y mod patternHeight];
        DrawPatternLine(x1, x2, y, patternRow);
        inc(i, 2);
      end;
    end;
  finally
    info.Free;
  end;
end;

procedure FillPolygonImage(Canv: TFPCustomCanvas; const Points: array of TPoint;
  Winding: Boolean; Image: TFPCustomImage; Relative: Boolean);
var
  x0, y0: Integer;

  procedure DrawImgLine(x1, x2, y: Integer);
  var
    x, imgX, imgY: Integer;
  begin
    imgX := (x1 - x0) mod Image.Width;
    ImgY := (y - y0) mod Image.Height;
    for x := x1 to x2 do
    begin
      Canv.DrawPixel(x, y, Image.Colors[imgX, imgY]);
      inc(imgX);
      if imgX >= Image.Width then imgX := 0;
    end;
  end;

var
  info: TPolygonInfo;
  i, x1, x2, y: Integer;
begin
  info := TPolygonInfo.Create;
  try
    info.UsePoints(Points, 0.0);
    if Relative then
    begin
      x0 := info.OrigBounds.Left;
      y0 := info.OrigBounds.Top;
    end else
    begin
      x0 := 0;
      y0 := 0;
    end;
    for y := info.OrigBounds.Top to info.OrigBounds.Bottom do
    begin
      info.GatherPolygonInfos(y, Winding);
      i := 0;
      // Fill the pixels between pairs of intersection points
      while i < info.Count do
      begin
        x1 := info.Data[i]^.P.X;
        x2 := info.Data[i+1]^.P.X;
        DrawImgLine(x1, x2, y);
        inc(i, 2);
      end;
    end;
  finally
    info.Free;
  end;
end;

end.
