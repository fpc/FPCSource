
uses 
  graph;

const
  Triangle: array[1..4] of PointType = ((X: 50; Y: 100), (X: 100; Y:100),
    (X: 150; Y: 150), (X:  50; Y: 100));

var Gd, Gm: Integer;
begin
  Gd := Detect;
  InitGraph(Gd, Gm, '');
  if GraphResult <> grOk then
    Halt(1);
  drawpoly(SizeOf(Triangle) div SizeOf(PointType), Triangle);
  readln;
  fillpoly(SizeOf(Triangle) div SizeOf(PointType), Triangle);
  graphdefaults;
  readln;
  CloseGraph;
end.
