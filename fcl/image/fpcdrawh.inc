{ TFPCustomDrawPen }

procedure TFPCustomDrawPen.DrawLine (x1,y1,x2,y2:integer);
begin
  DoDrawLine (x1,y1,x2,y2);
end;

procedure TFPCustomDrawPen.Polyline (points:array of TPoint; close:boolean);
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

procedure TFPCustomDrawBrush.Polygon (points:array of TPoint);
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
