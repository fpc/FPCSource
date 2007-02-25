{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TDrawObjects implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TFPCustomDrawPen }

procedure TFPCustomDrawPen.DrawLine (x1,y1,x2,y2:integer);
begin
  DoDrawLine (x1,y1,x2,y2);
end;

procedure TFPCustomDrawPen.Polyline (const points:array of TPoint; close:boolean);
begin
  DoPolyLine (points, false);
end;

procedure TFPCustomDrawPen.Ellipse (left,top, right,bottom:integer);
begin
  DoEllipse (left,top,right,bottom);
end;

procedure TFPCustomDrawPen.Rectangle (left,top, right,bottom:integer);
begin
  DoRectangle (left,top,right,bottom);
end;

{ TFPCustomDrawBrush }

procedure TFPCustomDrawBrush.Rectangle (left,top,right,bottom:integer);
begin
  DoRectangle (left,top,right,bottom);
end;

procedure TFPCustomDrawBrush.FloodFill (x,y:integer);
begin
  DoFloodFill (x,y);
end;

procedure TFPCustomDrawBrush.Ellipse (left,top, right,bottom:integer);
begin
  DoEllipse (left,top,right,bottom);
end;

procedure TFPCustomDrawBrush.Polygon (const points:array of TPoint);
begin
  DoPolygon (points);
end;

{ TFPCustomDrawFont }

procedure TFPCustomDrawFont.DrawText (x,y:integer; text:string);
begin
  DoDrawText (x,y, text);
end;

procedure TFPCustomDrawFont.GetTextSize (text:string; var w,h:integer);
begin
  DoGetTextSize (text, w,h);
end;

function TFPCustomDrawFont.GetTextHeight (text:string) : integer;
begin
  result := DoGetTextHeight (Text);
end;

function TFPCustomDrawFont.GetTextWidth (text:string) : integer;
begin
  result := DoGetTextWidth (Text);
end;
