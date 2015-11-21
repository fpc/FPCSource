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
end;

function TPoint.Distance(const apt : TPoint) : Double;
begin
  result:=sqrt(sqr(apt.x-x)+sqr(apt.y-y));
end;

function TPoint.IsZero : Boolean;
begin
 result:=(x=0) and (y=0);
end;

function TPoint.Subtract(const apt : TPoint): TPoint;
begin
  result.x:=x-apt.x;
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
 x:=x-apt.x;
 y:=y-apt.y;
end;

procedure TPoint.Offset(dx,dy : Longint);
begin
  x:=x-dx;
  y:=y-dy;
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

procedure TRect.setHeight(AValue: Longint);
begin
  right:=left+avalue;
end;

procedure TRect.setLocation(AValue: TPoint);
begin
  top:=avalue.x; left:=avalue.y;
end;

procedure TRect.setSize(AValue: TSize);
begin
  bottom:=top+avalue.cy;
  right:=left+avalue.cx;
end;

procedure TRect.setWidth(AValue: Longint);
begin
  bottom:=top+avalue;
end;


