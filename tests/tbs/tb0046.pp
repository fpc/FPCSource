{ %GRAPH }
{ %TARGET=go32v2,win32,linux}

{ Old file: tbs0052.pp }
{  Graph, collects missing graph unit routines          OK 0.99.9 (PM) }

uses
  crt,graph;

const
  Triangle: array[1..3] of PointType = ((X: 50; Y: 100), (X: 100; Y:100),
    (X: 150; Y: 150));
  Rect : array[1..4] of PointType = ((X: 50; Y: 100), (X: 100; Y:100),
    (X: 75; Y: 150), (X: 80; Y : 50));
  Penta : array[1..5] of PointType = ((X: 250; Y: 100), (X: 300; Y:100),
    (X: 275; Y: 150), (X: 280; Y : 50), (X:295; Y : 80) );

var Gd, Gm: Integer;
begin
  Gd := Detect;
  InitGraph(Gd, Gm, 'c:\bp\bgi');
  if GraphResult <> grOk then
    Halt(1);
  drawpoly(SizeOf(Triangle) div SizeOf(PointType), Triangle);
  {readln;}delay(1000);
  setcolor(red);
  fillpoly(SizeOf(Triangle) div SizeOf(PointType), Triangle);
  {readln;}delay(1000);
  SetFillStyle(SolidFill,blue);
  Bar(0,0,GetMaxX,GetMaxY);
  Rectangle(25,25,GetMaxX-25,GetMaxY-25);
  setViewPort(25,25,GetMaxX-25,GetMaxY-25,true);
  clearViewPort;
  setcolor(magenta);
  SetFillStyle(SolidFill,red);
  fillpoly(SizeOf(Rect) div SizeOf(PointType), Rect);
  fillpoly(SizeOf(Penta) div SizeOf(PointType), Penta);
  graphdefaults;
  {readln;}delay(1000);
  CloseGraph;
end.
