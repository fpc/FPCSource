{
  Test for the basic graph operations - PutPixel, GetPixel and HLine/VLine
  drawing with different colours and write modes

  Test draws random pixels and H/V lines with the graph unit and performs the
  same operations in memory. Finally it reads the whole resulting image, pixel
  by pixel, via GetPixel and compares the result with the expected value from
  the PixArray

  Useful for testing the platform-specific parts of the FPC graph unit (in
  various modes and operating systems)

  This test works also with TP7
}

program DrawTest;

uses
  Graph;

type
  TTestParams = record
    Driver: Integer;
    Mode: Integer;
    NumberOfObjectsToDraw: Integer;
    ProbabilityPixel: Integer;
    ProbabilityHLine: Integer;
    ProbabilityVLine: Integer;
  end;

  TPixelColor = Word;
  PRow = ^TRow;
  TRow = array [0..1279] of TPixelColor;

var
  XRes, YRes: Integer;
  PixArray: array [0..1023] of PRow;

procedure InitPixArray(AXRes, AYRes: Integer);
var
  Y: Integer;
begin
  XRes := AXRes;
  YRes := AYRes;
  for Y := 0 to AYRes - 1 do
  begin
    GetMem(PixArray[Y], AXRes * SizeOf(TPixelColor));
    FillChar(PixArray[Y]^, AXRes * SizeOf(TPixelColor), 0);
  end;
end;

procedure FreePixArray;
var
  Y: Integer;
begin
  for Y := 0 to YRes - 1 do
    FreeMem(PixArray[Y], XRes * SizeOf(TPixelColor));
end;

procedure TestFinalResult;
var
  X, Y: Integer;
begin
  for Y := 0 to YRes - 1 do
    for X := 0 to XRes - 1 do
      if GetPixel(X, Y) <> PixArray[Y]^[X] then
      begin
        CloseGraph;
        Writeln('Error at X = ', X, ', Y = ', Y);
        Halt(1);
      end;
end;

procedure TestPutPixel(X, Y: Integer; Color: TPixelColor);
begin
  PutPixel(X, Y, Color);

  PixArray[Y]^[X] := Color;
end;

procedure DirectPutPixel(X, Y: Integer; Color: TPixelColor; WriteMode: Integer);
begin
  case WriteMode of
    NormalPut, OrPut, NotPut: PixArray[Y]^[X] := Color;
    XORPut, AndPut: PixArray[Y]^[X] := PixArray[Y]^[X] xor Color;

    { TODO: add some sort of SetWriteModeExtended to the FPC graph unit, so
      we can test these as well: }
{    OrPut: PixArray[Y]^[X] := PixArray[Y]^[X] or Color;}
{    AndPut: PixArray[Y]^[X] := PixArray[Y]^[X] and Color;}
{    NotPut: PixArray[Y]^[X] := Color xor GetMaxColor;}
  end;
end;

procedure TestHLine(Y, X1, X2: Integer; Color: TPixelColor; WriteMode: Integer);
var
  tmp, X: Integer;
begin
  SetWriteMode(WriteMode);
  SetColor(Color);
  Line(X1, Y, X2, Y);

  if X1 > X2 then
  begin
    tmp := X1;
    X1 := X2;
    X2 := tmp;
  end;

  for X := X1 to X2 do
  begin
    DirectPutPixel(X, Y, Color, WriteMode);
  end;

  SetWriteMode(NormalPut);
end;

procedure TestVLine(X, Y1, Y2: Integer; Color: TPixelColor; WriteMode: Integer);
var
  tmp, Y: Integer;
begin
  SetWriteMode(WriteMode);
  SetColor(Color);
  Line(X, Y1, X, Y2);

  if Y1 > Y2 then
  begin
    tmp := Y1;
    Y1 := Y2;
    Y2 := tmp;
  end;

  for Y := Y1 to Y2 do
  begin
    DirectPutPixel(X, Y, Color, WriteMode);
  end;

  SetWriteMode(NormalPut);
end;

procedure TestDraw(const TestParams: TTestParams);
var
  I: Integer;
  R: Integer;
begin
  for I := 1 to TestParams.NumberOfObjectsToDraw do
  begin
    R := Random(TestParams.ProbabilityPixel + TestParams.ProbabilityHLine + TestParams.ProbabilityVLine);
    if R < TestParams.ProbabilityPixel then
      TestPutPixel(Random(XRes), Random(YRes), Random(GetMaxColor + 1))
    else
      if (R >= TestParams.ProbabilityPixel) and (R < TestParams.ProbabilityPixel + TestParams.ProbabilityHLine) then
        TestHLine(Random(YRes), Random(XRes), Random(XRes), Random(GetMaxColor + 1), Random(NotPut + 1))
      else
        TestVLine(Random(XRes), Random(YRes), Random(YRes), Random(GetMaxColor + 1), Random(NotPut + 1));
  end;
end;

procedure PerformTest(const TestParams: TTestParams);
var
  GraphDriver, GraphMode: Integer;
begin
  GraphDriver := TestParams.Driver;
  GraphMode := TestParams.Mode;
  InitGraph(GraphDriver, GraphMode, 'C:\TP\BGI');

  InitPixArray(GetMaxX + 1, GetMaxY + 1);

  TestDraw(TestParams);

  TestFinalResult;

  FreePixArray;

  CloseGraph;
  Writeln('Ok');
end;

var
  TestsCount: Integer;
  TestParams: TTestParams;
  Code: Integer;
  I: Integer;
begin
  if ParamCount <> 3 then
  begin
    Writeln('Usage: ', ParamStr(0), ' <driver number> <mode number> <tests count>');
    Writeln;
    Writeln('For example: ', ParamStr(0), ' 9 2 20');
    Writeln('performs 20 tests in 640x480x16 VGA mode (VGA = 9, VGAHi = 2)');
    Halt;
  end;
  Val(ParamStr(1), TestParams.Driver, Code);
  Val(ParamStr(2), TestParams.Mode, Code);
  Val(ParamStr(3), TestsCount, Code);

  Randomize;

  for I := 1 to TestsCount do
  begin
    TestParams.NumberOfObjectsToDraw := Random(30000);
    TestParams.ProbabilityPixel := Random(10);
    TestParams.ProbabilityHLine := Random(2);
    TestParams.ProbabilityVLine := Random(2);
    PerformTest(TestParams);
  end;
end.
