{
 test for graph unit's DrawPoly and FillPoly procedures
 compiles with Turbo Pascal 7 and Free Pascal
 used for TP7 compatibily testing
}

program PolyTest;

uses
  graph;

const
  MaxPoints = 1000;

var
  InF: Text;
  NumPoints: Integer;
  Poly: array [1..MaxPoints] of PointType;

procedure ReadPoly;
var
  I: Integer;
begin
  Readln(InF, NumPoints);
  for I := 1 to NumPoints do
    Readln(InF, Poly[I].X, Poly[I].Y);
end;

procedure CheckGraphResult;
var
  ErrorCode: Integer;
begin
  ErrorCode := GraphResult;
  if ErrorCode <> grOk then
  begin
    CloseGraph;
    Writeln(ErrorCode, ': ', GraphErrorMsg(ErrorCode));
    Readln;
    Halt(1);
  end;
end;

procedure Tralala;
var
  I: Integer;
  IStr: string;
begin
  if ParamStr(1) <> '' then
    Assign(InF, ParamStr(1))
  else
    Assign(InF, 'polytest.txt');
  Reset(InF);
  I := 1;
  while not Eof(InF) do
  begin
    ReadPoly;
    ClearDevice;
    Str(I, IStr);
    OutTextXY(0, 0, IStr);
    DrawPoly(NumPoints, Poly);
    CheckGraphResult;
    Readln;

    ClearDevice;
    OutTextXY(0, 0, IStr + ' fill');
    FillPoly(NumPoints, Poly);
    CheckGraphResult;
    Readln;
    Inc(I);
  end;
  Close(InF);
end;

var
  GraphDriver, GraphMode: Integer;
begin
  GraphDriver := VGA;
  GraphMode := VGAHi;
  InitGraph(GraphDriver, GraphMode, '');
  SetFillStyle(SolidFill, 9);
  Tralala;
  CloseGraph;
end.
