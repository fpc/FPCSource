{$mode objfpc}{$h+}
unit PixTools;

interface

uses classes, FPCanvas, clipping, FPimage;

procedure DrawSolidPolyline (Canv : TFPCustomCanvas; points:array of TPoint; close:boolean);
procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; pattern:word);

implementation

procedure FillRectangleColor (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer);
var x,y : integer;
    c : TFPColor;
begin
  writeln ('FillREctangleColor, sorting rec');
  SortRect (x1,y1, x2,y2);
  with Canv do
    begin
    writeln ('FillRectangleColor(',x1,',',y1,', ',x2,',',y2);
    c := brush.color;
    for x := x1 to x2 do
      for y := y1 to y2 do
        colors[x,y] := c;
    end;
end;

procedure DrawSolidPolyLine (Canv : TFPCustomCanvas; points:array of TPoint; close:boolean);
var i,a, r : integer;
    p : TPoint;
begin
  i := low(points);
  a := high(points);
  p := points[i];
  with Canv do
    begin
    for r := i+1 to a do
      begin
      Line (p.x, p.y, points[r].x, points[r].y);
      p := points[r];
      end;
    if close then
      Line (p.x,p.y, points[i].x,points[i].y);
    end;
end;

type
  TPutPixelProc = procedure (Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);

procedure PutPixelCopy(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := color;
end;

procedure PutPixelXor(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] xor color;
end;

procedure PutPixelOr(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] or color;
end;

procedure PutPixelAnd(Canv:TFPCustomCanvas; x,y:integer; color:TFPColor);
begin
  with Canv do
    Colors[x,y] := Colors[x,y] and color;
end;

procedure DrawSolidLine (Canv : TFPCustomCanvas; x1,y1, x2,y2:integer);
var PutPixelProc : TPutPixelProc;
  procedure HorizontalLine (x1,x2,y:integer);
    var x : integer;
        c : TFPColor;
    begin
      c := Canv.pen.color;
      for x := x1 to x2 do
        PutPixelProc (Canv, x,y, c);
    end;
  procedure VerticalLine (x,y1,y2:integer);
    var y : integer;
        c : TFPColor;
    begin
      c := Canv.pen.color;
      for y := y1 to y2 do
        PutPixelProc (Canv, x,y, c);
    end;
  procedure SlopedLine;
    var npixels,xinc1,yinc1,xinc2,yinc2,dx,dy,d,dinc1,dinc2 : integer;
    procedure initialize;
      begin // precalculations
      dx := abs(x2-x1);
      dy := abs(y2-y1);
      if dx > dy then  // determining independent variable
        begin  // x is independent
        npixels := dx + 1;
        d := (2 * dy) - dx;
        dinc1 := dy * 2;
        dinc2:= (dy - dx) * 2;
        xinc1 := 1;
        xinc2 := 1;
        yinc1 := 0;
        yinc2 := 1;
        end
      else
        begin  // y is independent
        npixels := dy + 1;
        d := (2 * dx) - dy;
        dinc1 := dx * 2;
        dinc2:= (dx - dy) * 2;
        xinc1 := 0;
        xinc2 := 1;
        yinc1 := 1;
        yinc2 := 1;
        end;
      // going into the correct direction
      if x1 > x2 then
        begin
        xinc1 := - xinc1;
        xinc2 := - xinc2;
        end;
      if y1 > y2 then
        begin
        yinc1 := - yinc1;
        yinc2 := - yinc2;
        end;
      end;
    var r,x,y : integer;
        c : TFPColor;
    begin
    initialize;
    x := x1;
    y := y1;
    c := canv.pen.color;
    for r := 1 to nPixels do
      begin
      PutPixelProc (Canv, x,y, c);
      if d < 0 then
        begin
        d := d + dinc1;
        x := x + xinc1;
        y := y + yinc1;
        end
      else
        begin
        d := d + dinc2;
        x := x + xinc2;
        y := y + yinc2;
        end;
      end;
    end;
begin
  with canv.pen do
    case mode of
      pmAnd : PutPixelProc := @PutPixelAnd;
      pmOr : PutPixelProc := @PutPixelOr;
      pmXor : PutPixelProc := @PutPixelXor;
      else PutPixelProc := @PutPixelCopy;
    end;
  if x1 = x2 then  // vertical line
    if y1 < y2 then
      VerticalLine (x1, y1, y2)
    else
      VerticalLine (x1, y2, y1)
  else if y1 = y2 then
    if x1 < x2 then
      HorizontalLine (x1, x2, y1)
    else
      HorizontalLine (x2, x1, y1)
  else  // sloped line
    SlopedLine;
end;

procedure DrawPatternLine (Canv:TFPCustomCanvas; x1,y1, x2,y2:integer; pattern:word);
begin
end;

end.
