{
    Tests for fpimgbarcode: the bars drawn into an image follow the EAN module
    patterns of ISO/IEC 15420 and the bar widths of fpbarcode, inside the rectangle.
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcbarcodedraw;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, types, fpcunit, testregistry, fpimage, fpimgtests, fpbarcode, fpimgbarcode;

type
  TTestBarcodeDraw = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FDraw: TFPDrawBarCode;
    // The EAN-8 module pattern of ISO/IEC 15420 for 8 digits, '1' for a dark module.
    function EAN8Pattern(const aDigits: String): String;
    // The EAN-13 module pattern of ISO/IEC 15420 for 13 digits, '1' for a dark module.
    function EAN13Pattern(const aDigits: String): String;
    // The pixel row fpbarcode describes for aText: one character per pixel, '1' for dark.
    function EncodingRow(const aText: String; aEncoding: TBarcodeEncoding; aUnit: Integer; aWeight: Double): String;
    // Fails unless columns aLeft.. of rows aTop..aBottom follow aPattern, aPixel pixels per character.
    procedure CheckPattern(const aMessage, aPattern: String; aLeft, aTop, aBottom, aPixel: Integer);
    // Fails unless the rectangle has aColor on every pixel.
    procedure CheckArea(const aMessage: String; aLeft, aTop, aRight, aBottom: Integer; const aColor: TFPColor);
    // Draws aText with DrawBarCode into a new image of its exact width and checks it against EncodingRow.
    procedure CheckEncoding(const aText: String; aEncoding: TBarcodeEncoding; aUnit: Integer; aWeight: Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEAN8Bars;
    procedure TestEAN8BarsOneUnit;
    procedure TestEAN8BarsThreeUnits;
    procedure TestEAN13Bars;
    procedure TestDrawingFollowsTheEncoding;
    procedure TestCode128BTextWithASpace;
    procedure TestCalcWidth;
    procedure TestInvalidTextIsNotDrawn;
    procedure TestBarsStartAtTheRectangle;
    procedure TestBackgroundBeyondTheBarsIsWhite;
    procedure TestClippingKeepsABarcodeThatFits;
    procedure TestClippingCutsTheBars;
    procedure TestPostNetShortBarsStandOnTheBaseline;
  end;

implementation

const
  cRedColor: TFPColor = (Red: $FFFF; Green: 0; Blue: 0; Alpha: $FFFF);
  cCodeL: array[0..9] of String = ('0001101', '0011001', '0010011', '0111101', '0100011',
    '0110001', '0101111', '0111011', '0110111', '0001011');
  cCodeG: array[0..9] of String = ('0100111', '0110011', '0011011', '0100001', '0011101',
    '0111001', '0000101', '0010001', '0001001', '0010111');
  cCodeR: array[0..9] of String = ('1110010', '1100110', '1101100', '1000010', '1011100',
    '1001110', '1010000', '1000100', '1001000', '1110100');
  cParity: array[0..9] of String = ('LLLLLL', 'LLGLGG', 'LLGGLG', 'LLGGGL', 'LGLLGG',
    'LGGLLG', 'LGGGLL', 'LGLGLG', 'LGLGGL', 'LGGLGL');


procedure TTestBarcodeDraw.SetUp;

begin
  inherited SetUp;
  FDraw := TFPDrawBarCode.Create;
end;


procedure TTestBarcodeDraw.TearDown;

begin
  FreeAndNil(FDraw);
  FreeAndNil(FImage);
  inherited TearDown;
end;


function TTestBarcodeDraw.EAN8Pattern(const aDigits: String): String;

var
  I: Integer;

begin
  Result := '101';
  for I := 1 to 4 do
    Result := Result + cCodeL[Ord(aDigits[I]) - Ord('0')];
  Result := Result + '01010';
  for I := 5 to 8 do
    Result := Result + cCodeR[Ord(aDigits[I]) - Ord('0')];
  Result := Result + '101';
end;


function TTestBarcodeDraw.EAN13Pattern(const aDigits: String): String;

var
  I: Integer;
  lParity: String;

begin
  lParity := cParity[Ord(aDigits[1]) - Ord('0')];
  Result := '101';
  for I := 2 to 7 do
    if lParity[I - 1] = 'L' then
      Result := Result + cCodeL[Ord(aDigits[I]) - Ord('0')]
    else
      Result := Result + cCodeG[Ord(aDigits[I]) - Ord('0')];
  Result := Result + '01010';
  for I := 8 to 13 do
    Result := Result + cCodeR[Ord(aDigits[I]) - Ord('0')];
  Result := Result + '101';
end;


function TTestBarcodeDraw.EncodingRow(const aText: String; aEncoding: TBarcodeEncoding; aUnit: Integer; aWeight: Double): String;

var
  lParams: TBarParamsArray;
  lWidths: TBarWidthArray;
  I: Integer;

begin
  Result := '';
  lParams := StringToBarcodeParams(aText, aEncoding);
  lWidths := CalcBarWidths(aEncoding, aUnit, aWeight);
  for I := 0 to High(lParams) do
    if lParams[I].c = bcBlack then
      Result := Result + StringOfChar('1', lWidths[lParams[I].w])
    else
      Result := Result + StringOfChar('0', lWidths[lParams[I].w]);
end;


procedure TTestBarcodeDraw.CheckPattern(const aMessage, aPattern: String; aLeft, aTop, aBottom, aPixel: Integer);

var
  I, J, lX: Integer;
  lExpected: TFPColor;

begin
  for I := 1 to Length(aPattern) do
    begin
    if aPattern[I] = '1' then
      lExpected := colBlack
    else
      lExpected := colWhite;
    for lX := 0 to aPixel - 1 do
      for J := aTop to aBottom do
        AssertColorsEqual(Format('%s: pixel (%d,%d) of module %d', [aMessage, aLeft + (I - 1) * aPixel + lX, J, I - 1]),
          lExpected, FImage.Colors[aLeft + (I - 1) * aPixel + lX, J]);
    end;
end;


procedure TTestBarcodeDraw.CheckArea(const aMessage: String; aLeft, aTop, aRight, aBottom: Integer; const aColor: TFPColor);

var
  I, J: Integer;

begin
  for J := aTop to aBottom do
    for I := aLeft to aRight do
      AssertColorsEqual(Format('%s: pixel (%d,%d)', [aMessage, I, J]), aColor, FImage.Colors[I, J]);
end;


procedure TTestBarcodeDraw.CheckEncoding(const aText: String; aEncoding: TBarcodeEncoding; aUnit: Integer; aWeight: Double);

var
  lRow: String;

begin
  lRow := EncodingRow(aText, aEncoding, aUnit, aWeight);
  FreeAndNil(FImage);
  FImage := CreateSolidImage(Length(lRow), 12, cRedColor);
  AssertTrue(Format('"%s" can be drawn as %s', [aText, BarcodeEncodingNames[aEncoding]]),
    DrawBarCode(FImage, aText, aEncoding, aUnit, aWeight));
  CheckPattern(Format('"%s" as %s, unit %d, weight %g', [aText, BarcodeEncodingNames[aEncoding], aUnit, aWeight]),
    lRow, 0, 0, 11, 1);
end;


procedure TTestBarcodeDraw.TestEAN8Bars;

begin
  FImage := CreateSolidImage(67 * 2, 30, cRedColor);
  AssertTrue('the EAN-8 code 12345670 is drawn', DrawBarCode(FImage, '12345670', beEAN8, 2, 2.0));
  CheckPattern('EAN-8 12345670 at 2 pixels per module', EAN8Pattern('12345670'), 0, 0, 29, 2);
end;


procedure TTestBarcodeDraw.TestEAN8BarsOneUnit;

begin
  FImage := CreateSolidImage(67, 20, cRedColor);
  AssertTrue('the EAN-8 code 96385074 is drawn', DrawBarCode(FImage, '96385074', beEAN8, 1, 2.0));
  CheckPattern('EAN-8 96385074 at 1 pixel per module', EAN8Pattern('96385074'), 0, 0, 19, 1);
end;


procedure TTestBarcodeDraw.TestEAN8BarsThreeUnits;

begin
  FImage := CreateSolidImage(67 * 3, 20, cRedColor);
  AssertTrue('the EAN-8 code 55123457 is drawn', DrawBarCode(FImage, '55123457', beEAN8, 3, 2.0));
  CheckPattern('EAN-8 55123457 at 3 pixels per module', EAN8Pattern('55123457'), 0, 0, 19, 3);
end;


procedure TTestBarcodeDraw.TestEAN13Bars;

begin
  FImage := CreateSolidImage(95 * 2, 25, cRedColor);
  AssertTrue('the EAN-13 code 5901234123457 is drawn', DrawBarCode(FImage, '5901234123457', beEAN13, 2, 2.0));
  CheckPattern('EAN-13 5901234123457 at 2 pixels per module', EAN13Pattern('5901234123457'), 0, 0, 24, 2);
end;


procedure TTestBarcodeDraw.TestDrawingFollowsTheEncoding;

begin
  CheckEncoding('CODE39', be39, 1, 2.0);
  CheckEncoding('CODE39', be39, 2, 3.0);
  CheckEncoding('Barcode128', be128B, 2, 2.0);
  CheckEncoding('123456', be128C, 1, 2.0);
  CheckEncoding('12345678', be2of5interleaved, 2, 2.5);
  CheckEncoding('A1234B', beCodabar, 1, 3.0);
  CheckEncoding('HELLO', be93, 3, 2.0);
  CheckEncoding('1234', beMSI, 2, 2.0);
end;


procedure TTestBarcodeDraw.TestCode128BTextWithASpace;

begin
  CheckEncoding('A B', be128B, 1, 2.0);
end;


procedure TTestBarcodeDraw.TestCalcWidth;

begin
  FDraw.Encoding := beEAN8;
  FDraw.UnitWidth := 2;
  FDraw.Text := '12345670';
  AssertEquals('an EAN-8 code at unit width 2 is 67 modules of 2 pixels', 134, FDraw.CalcWidth);
  FDraw.Text := 'ABC';
  AssertEquals('a text that EAN-8 cannot encode has width -1', -1, FDraw.CalcWidth);
end;


procedure TTestBarcodeDraw.TestInvalidTextIsNotDrawn;

begin
  FImage := CreateSolidImage(80, 20, cRedColor);
  AssertFalse('EAN-8 cannot draw letters', DrawBarCode(FImage, 'ABC', beEAN8));
  CheckArea('an EAN-8 code of letters leaves the image untouched', 0, 0, 79, 19, cRedColor);
end;


procedure TTestBarcodeDraw.TestBarsStartAtTheRectangle;

begin
  FImage := CreateSolidImage(160, 40, cRedColor);
  FDraw.Image := FImage;
  FDraw.Rect := Rect(10, 5, 10 + 134, 30);
  FDraw.UnitWidth := 2;
  FDraw.Encoding := beEAN8;
  FDraw.Text := '12345670';
  AssertTrue('the EAN-8 code is drawn in the rectangle', FDraw.Draw);
  CheckPattern('EAN-8 12345670 drawn from the top left corner (10,5) of the rectangle', EAN8Pattern('12345670'), 10, 5, 29, 2);
  CheckArea('the columns left of the rectangle are untouched', 0, 0, 9, 39, cRedColor);
  CheckArea('the rows above the rectangle are untouched', 0, 0, 159, 4, cRedColor);
  CheckArea('the rows below the rectangle are untouched', 0, 32, 159, 39, cRedColor);
end;


procedure TTestBarcodeDraw.TestBackgroundBeyondTheBarsIsWhite;

begin
  FImage := CreateSolidImage(200, 20, cRedColor);
  FDraw.Image := FImage;
  FDraw.Rect := Rect(0, 0, 199, 19);
  FDraw.UnitWidth := 1;
  FDraw.Encoding := beEAN8;
  FDraw.Text := '12345670';
  AssertTrue('the EAN-8 code is drawn', FDraw.Draw);
  CheckPattern('EAN-8 12345670 at the left of a wide rectangle', EAN8Pattern('12345670'), 0, 0, 18, 1);
  CheckArea('the rectangle right of the bars is white', 67, 0, 198, 18, colWhite);
end;


procedure TTestBarcodeDraw.TestClippingKeepsABarcodeThatFits;

begin
  FImage := CreateSolidImage(160, 30, cRedColor);
  FDraw.Image := FImage;
  FDraw.Rect := Rect(10, 0, 10 + 134, 29);
  FDraw.UnitWidth := 2;
  FDraw.Encoding := beEAN8;
  FDraw.Text := '12345670';
  FDraw.Clipping := True;
  AssertTrue('the EAN-8 code is drawn with clipping', FDraw.Draw);
  CheckPattern('with clipping, a barcode as wide as its rectangle at x=10 is drawn whole', EAN8Pattern('12345670'), 10, 0, 28, 2);
end;


procedure TTestBarcodeDraw.TestClippingCutsTheBars;

var
  lPattern: String;

begin
  FImage := CreateSolidImage(160, 30, cRedColor);
  FDraw.Image := FImage;
  FDraw.Rect := Rect(0, 0, 60, 29);
  FDraw.UnitWidth := 2;
  FDraw.Encoding := beEAN8;
  FDraw.Text := '12345670';
  FDraw.Clipping := True;
  AssertTrue('the EAN-8 code is drawn with clipping', FDraw.Draw);
  lPattern := EAN8Pattern('12345670');
  CheckPattern('with clipping, the bars inside the rectangle are drawn', Copy(lPattern, 1, 25), 0, 0, 28, 2);
  CheckArea('with clipping, nothing is drawn right of the rectangle', 62, 0, 159, 29, cRedColor);
end;


procedure TTestBarcodeDraw.TestPostNetShortBarsStandOnTheBaseline;

var
  lRow: String;
  lX: Integer;

begin
  lRow := EncodingRow('1', bePostNet, 2, 2.0);
  FImage := CreateSolidImage(Length(lRow), 50, cRedColor);
  AssertTrue('the PostNet code 1 is drawn', DrawBarCode(FImage, '1', bePostNet, 2, 2.0));
  AssertTrue('the frame bar is dark at the top', FImage.Colors[0, 0] = colBlack);
  AssertTrue('the frame bar is dark at the bottom', FImage.Colors[0, 49] = colBlack);
  lX := Pos('0', lRow) - 1;
  lX := lX + Pos('1', Copy(lRow, lX + 1, MaxInt)) - 1;
  AssertTrue('the first bar of the digit 1 (00011) is a short bar, dark at the bottom row', FImage.Colors[lX, 49] = colBlack);
  AssertTrue('the first bar of the digit 1 (00011) is a short bar, white at the top row', FImage.Colors[lX, 0] = colWhite);
end;


initialization
  RegisterTest('barcodedraw', TTestBarcodeDraw);
end.
