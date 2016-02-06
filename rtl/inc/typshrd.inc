{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Marco van de Voort
    member of the Free Pascal development team.

    Types that are in unit types on all platforms but also in 
    unit Windows on win<x> 

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ TSize }
{$ifdef VER3}
constructor TSize.Create(ax,ay:Longint);
begin
  cx:=ax; cy:=ay;
end;

constructor TSize.Create(asz :TSize);
begin
  cx:=asz.cx; cy:=asz.cy;
  // vector:=TSize(asz.vector); ??
end;
{$endif}

function TSize.IsZero : Boolean;
begin
  result:=(cx=0) and (cy=0);
end;

function TSize.Distance(const asz : TSize) : Double;
begin
  result:=sqrt(sqr(cx-asz.cx)+sqr(cy-asz.cy));
end;

function TSize.Add(const asz : TSize): TSize;
begin
  result.cx:=cx+asz.cx;
  result.cy:=cy+asz.cy;
end;

function TSize.Subtract(const asz : TSize): TSize;
begin
  result.cx:=cx-asz.cx;
  result.cy:=cy-asz.cy;
end;

class operator TSize.=(const asz1, asz2 : TSize) : Boolean;
begin
  result:=(asz1.cx=asz2.cx) and (asz1.cy=asz2.cy);
end;

class operator TSize.<> (const asz1, asz2 : TSize): Boolean;
begin
  result:=(asz1.cx<>asz2.cx) or (asz1.cy<>asz2.cy);
end;

class operator TSize.+(const asz1, asz2 : TSize): TSize;
begin
  result.cx:=asz1.cx+asz2.cx;
  result.cy:=asz1.cy+asz2.cy;
end;

class operator TSize.-(const asz1, asz2 : TSize): TSize;
begin
  result.cx:=asz1.cx-asz2.cx;
  result.cy:=asz1.cy-asz2.cy;
end;

{ TPoint }
{$ifdef VER3}
constructor TPoint.Create(ax,ay:Longint);
begin
  x:=ax; y:=ay;
end;

constructor TPoint.Create(apt :TPoint);
begin
  x:=apt.x; y:=apt.y;
end;

{$endif}
function TPoint.Add(const apt: TPoint): TPoint;
begin
  result.x:=x+apt.x;
  result.y:=y+apt.y;
end;

function TPoint.Distance(const apt: TPoint): ValReal;
begin
  result:=sqrt(sqr(ValReal(apt.x)-ValReal(x))+sqr(ValReal(apt.y)-ValReal(y))); // convert to ValReal to prevent integer overflows
end;

function TPoint.IsZero : Boolean;
begin
 result:=(x=0) and (y=0);
end;

function TPoint.Subtract(const apt : TPoint): TPoint;
begin
  result.x:=x-apt.x;
  result.y:=y-apt.y;
end;

class function TPoint.Zero: TPoint;
begin
  Result.x := 0;
  Result.y := 0;
end;

procedure TPoint.SetLocation(const apt :TPoint);
begin
 x:=apt.x; y:=apt.y;
end;
procedure TPoint.SetLocation(ax,ay : Longint);
begin
  x:=ax; y:=ay;
end;

procedure TPoint.Offset(const apt :TPoint);
begin
 x:=x+apt.x;
 y:=y+apt.y;
end;

class function TPoint.PointInCircle(const apt, acenter: TPoint;
  const aradius: Integer): Boolean;
begin
  Result := apt.Distance(acenter) <= aradius;
end;

procedure TPoint.Offset(dx,dy : Longint);
begin
  x:=x+dx;
  y:=y+dy;
end;

class operator TPoint.= (const apt1, apt2 : TPoint) : Boolean;
begin
  result:=(apt1.x=apt2.x) and (apt1.y=apt2.y);
end;

class operator TPoint.<> (const apt1, apt2 : TPoint): Boolean;
begin
  result:=(apt1.x<>apt2.x) or (apt1.y<>apt2.y);
end;

class operator TPoint.+ (const apt1, apt2 : TPoint): TPoint;
begin
  result.x:=apt1.x+apt2.x;
  result.y:=apt1.y+apt2.y;
end;

class operator TPoint.- (const apt1, apt2 : TPoint): TPoint;
begin
  result.x:=apt1.x-apt2.x;
  result.y:=apt1.y-apt2.y;
end;

// warning suppression for the next ones?
class operator TPoint.:= (const aspt : TSmallPoint): TPoint;
begin
  result.x:=aspt.x;
  result.y:=aspt.y;
end;

class operator TPoint.Explicit (const apt: TPoint): TSmallPoint;
begin
  result.x:=apt.x;
  result.y:=apt.y;
end;

{ TRect }

class operator TRect. * (L, R: TRect): TRect;
begin
  Result := TRect.Intersect(L, R);
end;

class operator TRect. + (L, R: TRect): TRect;
begin
  Result := TRect.Union(L, R);
end;

class operator TRect. <> (L, R: TRect): Boolean;
begin
  Result := not(L=R);
end;

class operator TRect. = (L, R: TRect): Boolean;
begin
  Result :=
    (L.Left = R.Left) and (L.Right = R.Right) and
    (L.Top = R.Top) and (L.Bottom = R.Bottom);
end;

{$IFDEF VER3_0_0}
class function TRect.Create(ALeft, ATop, ARight, ABottom: Longint): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

class function TRect.Create(P1, P2: TPoint; Normalize: Boolean): TRect;
begin
  Result.TopLeft := P1;
  Result.BottomRight := P2;
  if Normalize then
    Result.NormalizeRect;
end;

class function TRect.Create(Origin: TPoint): TRect;
begin
  Result.TopLeft := Origin;
  Result.BottomRight := Origin;
end;

class function TRect.Create(Origin: TPoint; AWidth, AHeight: Longint): TRect;
begin
  Result.TopLeft := Origin;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

class function TRect.Create(R: TRect; Normalize: Boolean): TRect;
begin
  Result := R;
  if Normalize then
    Result.NormalizeRect;
end;

{$ELSE}

constructor TRect.Create(ALeft, ATop, ARight, ABottom: Longint);
begin
  Left := ALeft;
  Top := ATop;
  Right := ARight;
  Bottom := ABottom;
end;

constructor TRect.Create(P1, P2: TPoint; Normalize: Boolean);
begin
  TopLeft := P1;
  BottomRight := P2;
  if Normalize then
    NormalizeRect;
end;

constructor TRect.Create(Origin: TPoint);
begin
  TopLeft := Origin;
  BottomRight := Origin;
end;

constructor TRect.Create(Origin: TPoint; AWidth, AHeight: Longint);
begin
  TopLeft := Origin;
  Width := AWidth;
  Height := AHeight;
end;

constructor TRect.Create(R: TRect; Normalize: Boolean);
begin
  Self := R;
  if Normalize then
    NormalizeRect;
end;
{$ENDIF}

function TRect.CenterPoint: TPoint;
begin
  Result.X := (Right-Left) div 2 + Left;
  Result.Y := (Bottom-Top) div 2 + Top;
end;

function TRect.Contains(Pt: TPoint): Boolean;
begin
  Result := (Left <= Pt.X) and (Pt.X <= Right) and (Top <= Pt.Y) and (Pt.Y <= Bottom);
end;

function TRect.Contains(R: TRect): Boolean;
begin
  Result := (Left <= R.Left) and (R.Right <= Right) and (Top <= R.Top) and (R.Bottom <= Bottom);
end;

class function TRect.Empty: TRect;
begin
  Result := TRect.Create(0,0,0,0);
end;

function TRect.getHeight: Longint;
begin
  result:=bottom-top;
end;

function TRect.getLocation: TPoint;
begin
  result.x:=Left; result.y:=top;
end;

function TRect.getSize: TSize;
begin
  result.cx:=width; result.cy:=height;
end;

function TRect.getWidth: Longint;
begin
  result:=right-left;
end;

procedure TRect.Inflate(DX, DY: Longint);
begin
  InflateRect(Self, DX, DY);
end;

procedure TRect.Intersect(R: TRect);
begin
  Self := Intersect(Self, R);
end;

class function TRect.Intersect(R1: TRect; R2: TRect): TRect;
begin
  IntersectRect(Result, R1, R2);
end;

function TRect.IntersectsWith(R: TRect): Boolean;
begin
  Result := (Left < R.Right) and (R.Left < Right) and (Top < R.Bottom) and (R.Top < Bottom);
end;

function TRect.IsEmpty: Boolean;
begin
  Result := (Right <= Left) or (Bottom <= Top);
end;

procedure TRect.NormalizeRect;
var
  x: LongInt;
begin
  if Top>Bottom then
  begin
    x := Top;
    Top := Bottom;
    Bottom := x;
  end;
  if Left>Right then
  begin
    x := Left;
    Left := Right;
    Right := x;
  end
end;

procedure TRect.Inflate(DL, DT, DR, DB: Longint);
begin
  Dec(Left, DL);
  Dec(Top, DT);
  Inc(Right, DR);
  Inc(Bottom, DB);
end;

procedure TRect.Offset(DX, DY: Longint);
begin
  OffsetRect(Self, DX, DY);
end;

procedure TRect.Offset(DP: TPoint);
begin
  OffsetRect(Self, DP.X, DP.Y);
end;

procedure TRect.setHeight(AValue: Longint);
begin
  bottom:=top+avalue;
end;

procedure TRect.SetLocation(X, Y: Longint);
begin
  Offset(X-Left, Y-Top);
end;

procedure TRect.SetLocation(P: TPoint);
begin
  SetLocation(P.X, P.Y);
end;

procedure TRect.setSize(AValue: TSize);
begin
  bottom:=top+avalue.cy;
  right:=left+avalue.cx;
end;

procedure TRect.setWidth(AValue: Longint);
begin
  right:=left+avalue;
end;

function TRect.SplitRect(SplitType: TSplitRectType; Percent: Double): TRect;
begin
  Result := Self;
  case SplitType of
    srLeft: Result.Right := Left + Trunc(Percent*Width);
    srRight: Result.Left := Right - Trunc(Percent*Width);
    srTop: Result.Bottom := Top + Trunc(Percent*Height);
    srBottom: Result.Top := Bottom - Trunc(Percent*Height);
  end;
end;

function TRect.SplitRect(SplitType: TSplitRectType; ASize: Longint): TRect;
begin
  Result := Self;
  case SplitType of
    srLeft: Result.Right := Left + ASize;
    srRight: Result.Left := Right - ASize;
    srTop: Result.Bottom := Top + ASize;
    srBottom: Result.Top := Bottom - ASize;
  end;
end;

class function TRect.Union(const Points: array of TPoint): TRect;
var
  i: Integer;
begin
  if Length(Points) > 0 then
  begin
    Result.TopLeft := Points[Low(Points)];
    Result.BottomRight := Points[Low(Points)];

    for i := Low(Points)+1 to High(Points) do
    begin
      if Points[i].X < Result.Left then Result.Left := Points[i].X;
      if Points[i].X > Result.Right then Result.Right := Points[i].X;
      if Points[i].Y < Result.Top then Result.Top := Points[i].Y;
      if Points[i].Y > Result.Bottom then Result.Bottom := Points[i].Y;
    end;
  end else
    Result := Empty;
end;

procedure TRect.Union(R: TRect);
begin
  Self := Union(Self, R);
end;

class function TRect.Union(R1, R2: TRect): TRect;
begin
  UnionRect(Result, R1, R2);
end;


