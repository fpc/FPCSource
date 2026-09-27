{
    Tests for the small helper units: clipping, fpimgcmn (Swap, CRC-32),
    fpunitofmeasure (conversions) and fppapers (paper sizes).
    See the file COPYING.FPC, included in this distribution, for details.
}
unit tcmisc;

{$mode objfpc}{$H+}

interface

uses sysutils, classes, types, fpcunit, testregistry, fpimage, fpimgtests,
     clipping, fpimgcmn, fpunitofmeasure, fppapers;

type
  // Tests of clipping.pp, whose rectangles and results are inclusive corner coordinates.
  TTestClipping = class(TTestCase)
  private
    // Fails unless the rectangle has the expected corners.
    procedure CheckRect(const aMessage: String; aLeft, aTop, aRight, aBottom: Integer; const aRect: TRect);
    // Clips the line (aX1,aY1)-(aX2,aY2) to the clip rect (0,0)-(10,10) and checks the result.
    procedure CheckLine(const aMessage: String; aX1, aY1, aX2, aY2, aEX1, aEY1, aEX2, aEY2: Integer);
    // Clips the line (aX1,aY1)-(aX2,aY2) to the clip rect (0,0)-(10,10) and checks that it is rejected.
    procedure CheckLineRejected(const aMessage: String; aX1, aY1, aX2, aY2: Integer);
  published
    procedure TestSortRect;
    procedure TestSortRectCoordinates;
    procedure TestPointInside;
    procedure TestPointInsideUnsortedBounds;
    procedure TestRectInsideIsUnchanged;
    procedure TestRectPartlyOutsideTopLeft;
    procedure TestRectPartlyOutsideBottomRight;
    procedure TestRectCoveringTheClipRect;
    procedure TestRectOutsideOnEachSide;
    procedure TestRectTouchingTheEdge;
    procedure TestRectUnsorted;
    procedure TestRectWithUnsortedClipRect;
    procedure TestLineInsideIsUnchanged;
    procedure TestDiagonalLineCrossingTheRect;
    procedure TestLineLeavingOnTheRight;
    procedure TestLineCrossingLeftAndRight;
    procedure TestLineCuttingACorner;
    procedure TestLineKeepsItsDirection;
    procedure TestDiagonalLineOutsideOnEachSide;
    procedure TestDiagonalLinePassingACorner;
    procedure TestVerticalLineCrossing;
    procedure TestVerticalLineOnTheEdge;
    procedure TestVerticalLineAbove;
    procedure TestVerticalLineBelow;
    procedure TestVerticalLineLeft;
    procedure TestVerticalLineRight;
    procedure TestHorizontalLineCrossing;
    procedure TestHorizontalLineRight;
    procedure TestHorizontalLineLeft;
    procedure TestHorizontalLineAbove;
    procedure TestHorizontalLineBelow;
    procedure TestPointLineInside;
    procedure TestPointLineOutside;
    procedure TestLineWithUnsortedClipRect;
  end;

  TTestImgCmn = class(TTestCase)
  private
    // The CRC-32 of a string with CalculateCRC.
    function CRCOf(const aText: AnsiString): LongWord;
    // The CRC-32 register after the bytes of aText, starting from aCRC.
    function CRCUpdate(aCRC: LongWord; const aText: AnsiString): LongWord;
  published
    procedure TestSwapWord;
    procedure TestSwapLongWord;
    procedure TestSwapInteger;
    procedure TestSwapQWord;
    procedure TestSwapInt64;
    procedure TestSwapTwiceIsIdentity;
    procedure TestCRCOfCheckString;
    procedure TestCRCOfIEND;
    procedure TestCRCOfNothing;
    procedure TestCRCRegisterAsUsedByPNG;
    procedure TestCRCInPieces;
  end;

  TTestUnitOfMeasure = class(TTestCase)
  published
    procedure TestInchToOtherUnits;
    procedure TestMetricUnits;
    procedure TestTypographicUnits;
    procedure TestSameUnit;
    procedure TestPhysicalSizeToPixels;
    procedure TestPhysicalSizeToPixelsPerCentimeter;
    procedure TestIllDefinedResolution;
    procedure TestPixelsToPhysicalSize;
    procedure TestConvertThroughPixels;
    procedure TestRoundTrips;
    procedure TestRectFToPixels;
    procedure TestRectToPixels;
    procedure TestRectToPixelsPreservingData;
    procedure TestConvertRectF;
    procedure TestConvertRectFToPixelsUsesTheResolution;
    procedure TestConvertRectFFromPixelsUsesTheResolution;
    procedure TestNames;
  end;

  TTestPapers = class(TTestCase)
  private
    // Fails unless the paper aName in aPapers is aWidth x aHeight within aTolerance.
    procedure CheckPaper(const aMessage: String; const aPapers: TPaperSizes; const aName: String; aWidth, aHeight, aTolerance: Single);
    // Fails unless both tables list the same papers with sizes agreeing to within aTolerance cm.
    procedure CheckTables(const aName: String; const aCm, aInch: TPaperSizes; aTolerance: Single);
  published
    procedure TestISO216A;
    procedure TestISO216B;
    procedure TestISO216C;
    procedure TestASeriesHalves;
    procedure TestUSSizes;
    procedure TestANSISizes;
    procedure TestJISB;
    procedure TestCmAndInchTablesAgree;
    procedure TestInchToCm;
    procedure TestCmToInch;
    procedure TestConversionLeavesTheSourceAlone;
    procedure TestGetPaperSize;
    procedure TestGetPaperSizeExactFit;
    procedure TestGetPaperSizeTooLarge;
    procedure TestGetPaperSizeFromSeveralTables;
  end;

implementation

{ TTestClipping }

procedure TTestClipping.CheckRect(const aMessage: String; aLeft, aTop, aRight, aBottom: Integer; const aRect: TRect);

begin
  AssertEquals(aMessage + ': left', aLeft, aRect.Left);
  AssertEquals(aMessage + ': top', aTop, aRect.Top);
  AssertEquals(aMessage + ': right', aRight, aRect.Right);
  AssertEquals(aMessage + ': bottom', aBottom, aRect.Bottom);
end;


procedure TTestClipping.CheckLine(const aMessage: String; aX1, aY1, aX2, aY2, aEX1, aEY1, aEX2, aEY2: Integer);

var
  lX1, lY1, lX2, lY2: Integer;
  lText: String;

begin
  lX1 := aX1;
  lY1 := aY1;
  lX2 := aX2;
  lY2 := aY2;
  CheckLineClipping(Rect(0, 0, 10, 10), lX1, lY1, lX2, lY2);
  lText := Format('%s: (%d,%d)-(%d,%d) clipped to the corners (0,0)-(10,10) is (%d,%d)-(%d,%d)',
    [aMessage, aX1, aY1, aX2, aY2, aEX1, aEY1, aEX2, aEY2]);
  if (lX1 <> aEX1) or (lY1 <> aEY1) or (lX2 <> aEX2) or (lY2 <> aEY2) then
    Fail(Format('%s, got (%d,%d)-(%d,%d)', [lText, lX1, lY1, lX2, lY2]));
end;


procedure TTestClipping.CheckLineRejected(const aMessage: String; aX1, aY1, aX2, aY2: Integer);

var
  lX1, lY1, lX2, lY2: Integer;

begin
  lX1 := aX1;
  lY1 := aY1;
  lX2 := aX2;
  lY2 := aY2;
  CheckLineClipping(Rect(0, 0, 10, 10), lX1, lY1, lX2, lY2);
  if (lX1 <> -1) or (lY1 <> -1) or (lX2 <> -1) or (lY2 <> -1) then
    Fail(Format('%s: (%d,%d)-(%d,%d) lies outside the corners (0,0)-(10,10) and is rejected '
      + '(all coordinates -1), got (%d,%d)-(%d,%d)', [aMessage, aX1, aY1, aX2, aY2, lX1, lY1, lX2, lY2]));
end;


procedure TTestClipping.TestSortRect;

var
  lRect: TRect;

begin
  lRect := Rect(10, 20, 0, 5);
  SortRect(lRect);
  CheckRect('SortRect puts the smaller coordinates in Left and Top', 0, 5, 10, 20, lRect);
  lRect := Rect(1, 2, 3, 4);
  SortRect(lRect);
  CheckRect('SortRect leaves a sorted rectangle alone', 1, 2, 3, 4, lRect);
end;


procedure TTestClipping.TestSortRectCoordinates;

var
  lLeft, lTop, lRight, lBottom: Integer;

begin
  lLeft := 7;
  lTop := -3;
  lRight := 2;
  lBottom := -8;
  SortRect(lLeft, lTop, lRight, lBottom);
  AssertEquals('SortRect on coordinates sorts left', 2, lLeft);
  AssertEquals('SortRect on coordinates sorts top', -8, lTop);
  AssertEquals('SortRect on coordinates sorts right', 7, lRight);
  AssertEquals('SortRect on coordinates sorts bottom', -3, lBottom);
end;


procedure TTestClipping.TestPointInside;

begin
  AssertTrue('a point in the middle is inside', PointInside(5, 5, Rect(0, 0, 10, 10)));
  AssertTrue('the top left corner is inside', PointInside(0, 0, Rect(0, 0, 10, 10)));
  AssertTrue('the bottom right corner is inside, the bounds being inclusive corners', PointInside(10, 10, Rect(0, 0, 10, 10)));
  AssertFalse('a point right of the bounds is outside', PointInside(11, 5, Rect(0, 0, 10, 10)));
  AssertFalse('a point above the bounds is outside', PointInside(5, -1, Rect(0, 0, 10, 10)));
  AssertFalse('a point left of the bounds is outside', PointInside(-1, 5, Rect(0, 0, 10, 10)));
  AssertFalse('a point below the bounds is outside', PointInside(5, 11, Rect(0, 0, 10, 10)));
end;


procedure TTestClipping.TestPointInsideUnsortedBounds;

begin
  AssertTrue('PointInside sorts the bounds first', PointInside(5, 5, Rect(10, 10, 0, 0)));
  AssertFalse('PointInside with unsorted bounds still rejects outside points', PointInside(12, 5, Rect(10, 10, 0, 0)));
end;


procedure TTestClipping.TestRectInsideIsUnchanged;

var
  lRect: TRect;

begin
  lRect := Rect(2, 3, 7, 8);
  AssertTrue('a rectangle inside the clip rect is visible', CheckRectClipping(Rect(0, 0, 10, 10), lRect));
  CheckRect('a rectangle inside the clip rect is unchanged', 2, 3, 7, 8, lRect);
end;


procedure TTestClipping.TestRectPartlyOutsideTopLeft;

var
  lRect: TRect;

begin
  lRect := Rect(-5, -5, 5, 5);
  AssertTrue('a rectangle partly outside is visible', CheckRectClipping(Rect(0, 0, 10, 10), lRect));
  CheckRect('a rectangle over the top left corner is cut at the clip rect', 0, 0, 5, 5, lRect);
end;


procedure TTestClipping.TestRectPartlyOutsideBottomRight;

var
  lX1, lY1, lX2, lY2: Integer;

begin
  lX1 := 5;
  lY1 := 5;
  lX2 := 15;
  lY2 := 15;
  AssertTrue('a rectangle partly outside is visible', CheckRectClipping(Rect(0, 0, 10, 10), lX1, lY1, lX2, lY2));
  CheckRect('a rectangle over the bottom right corner is cut at the inclusive corner (10,10)', 5, 5, 10, 10,
    Rect(lX1, lY1, lX2, lY2));
end;


procedure TTestClipping.TestRectCoveringTheClipRect;

var
  lRect: TRect;

begin
  lRect := Rect(-100, -100, 100, 100);
  AssertTrue('a rectangle covering the clip rect is visible', CheckRectClipping(Rect(0, 0, 10, 10), lRect));
  CheckRect('a rectangle covering the clip rect becomes the clip rect', 0, 0, 10, 10, lRect);
end;


procedure TTestClipping.TestRectOutsideOnEachSide;

const
  cRects: array[0..3, 0..3] of Integer = ((-10, 2, -1, 8), (11, 2, 20, 8), (2, -10, 8, -1), (2, 11, 8, 20));
  cSides: array[0..3] of String = ('left', 'right', 'above', 'below');

var
  I: Integer;
  lRect: TRect;

begin
  for I := 0 to 3 do
    begin
    lRect := Rect(cRects[I, 0], cRects[I, 1], cRects[I, 2], cRects[I, 3]);
    AssertFalse('a rectangle ' + cSides[I] + ' of the clip rect is not visible',
      CheckRectClipping(Rect(0, 0, 10, 10), lRect));
    CheckRect('a rectangle ' + cSides[I] + ' of the clip rect is cleared to -1', -1, -1, -1, -1, lRect);
    end;
end;


procedure TTestClipping.TestRectTouchingTheEdge;

var
  lRect: TRect;

begin
  lRect := Rect(10, 2, 20, 8);
  AssertTrue('a rectangle sharing the column 10 with the clip rect (0,0)-(10,10) is visible',
    CheckRectClipping(Rect(0, 0, 10, 10), lRect));
  CheckRect('a rectangle sharing the column 10 keeps that column only', 10, 2, 10, 8, lRect);
end;


procedure TTestClipping.TestRectUnsorted;

var
  lRect: TRect;

begin
  lRect := Rect(5, 5, -5, -5);
  AssertTrue('an unsorted rectangle partly inside is visible', CheckRectClipping(Rect(0, 0, 10, 10), lRect));
  CheckRect('an unsorted rectangle is sorted and clipped', 0, 0, 5, 5, lRect);
end;


procedure TTestClipping.TestRectWithUnsortedClipRect;

var
  lRect: TRect;

begin
  lRect := Rect(5, 5, 15, 15);
  AssertTrue('clipping to an unsorted clip rect works', CheckRectClipping(Rect(10, 10, 0, 0), lRect));
  CheckRect('clipping to an unsorted clip rect sorts it first', 5, 5, 10, 10, lRect);
end;


procedure TTestClipping.TestLineInsideIsUnchanged;

begin
  CheckLine('a line inside is unchanged', 1, 2, 8, 9, 1, 2, 8, 9);
  CheckLine('a line between the corners is unchanged', 0, 0, 10, 10, 0, 0, 10, 10);
end;


procedure TTestClipping.TestDiagonalLineCrossingTheRect;

begin
  CheckLine('a diagonal through the whole rect is cut at both corners', -10, -10, 20, 20, 0, 0, 10, 10);
  CheckLine('an anti-diagonal through the whole rect is cut at both corners', -5, 15, 15, -5, 0, 10, 10, 0);
end;


procedure TTestClipping.TestLineLeavingOnTheRight;

begin
  CheckLine('a line leaving on the right is cut at x=10', 4, 4, 16, 10, 4, 4, 10, 7);
  CheckLine('a line leaving at the bottom is cut at y=10', 4, 4, 10, 16, 4, 4, 7, 10);
end;


procedure TTestClipping.TestLineCrossingLeftAndRight;

begin
  CheckLine('a line crossing both sides is cut at x=0 and x=10', -4, 0, 16, 10, 0, 2, 10, 7);
end;


procedure TTestClipping.TestLineCuttingACorner;

begin
  CheckLine('a line cutting the top left corner is cut at both edges', -2, 5, 5, -2, 0, 3, 3, 0);
end;


procedure TTestClipping.TestLineKeepsItsDirection;

begin
  CheckLine('a clipped line keeps its first point first', 16, 10, 4, 4, 10, 7, 4, 4);
end;


procedure TTestClipping.TestDiagonalLineOutsideOnEachSide;

begin
  CheckLineRejected('a line left of the rect', -10, 0, -2, 8);
  CheckLineRejected('a line right of the rect', 12, 0, 20, 8);
  CheckLineRejected('a line above the rect', 0, -10, 8, -2);
  CheckLineRejected('a line below the rect', 0, 12, 8, 20);
end;


procedure TTestClipping.TestDiagonalLinePassingACorner;

begin
  CheckLineRejected('a line passing outside the top left corner', -10, 5, 5, -10);
  CheckLineRejected('a line passing outside the bottom right corner', 20, 5, 5, 20);
end;


procedure TTestClipping.TestVerticalLineCrossing;

begin
  CheckLine('a vertical line through the rect is cut at y=0 and y=10', 5, -5, 5, 15, 5, 0, 5, 10);
  CheckLine('a vertical line inside is unchanged', 5, 2, 5, 8, 5, 2, 5, 8);
end;


procedure TTestClipping.TestVerticalLineOnTheEdge;

begin
  CheckLine('a vertical line on the inclusive left edge x=0 is kept', 0, -5, 0, 15, 0, 0, 0, 10);
  CheckLine('a vertical line on the inclusive right edge x=10 is kept', 10, -5, 10, 15, 10, 0, 10, 10);
end;


procedure TTestClipping.TestVerticalLineAbove;

begin
  CheckLineRejected('a vertical line above the rect, not collapsed to a point on the edge', 5, -10, 5, -2);
end;


procedure TTestClipping.TestVerticalLineBelow;

begin
  CheckLineRejected('a vertical line below the rect, not collapsed to a point on the edge', 5, 12, 5, 20);
end;


procedure TTestClipping.TestVerticalLineLeft;

begin
  CheckLineRejected('a vertical line left of the rect', -5, 2, -5, 8);
end;


procedure TTestClipping.TestVerticalLineRight;

begin
  CheckLineRejected('a vertical line right of the rect', 11, -5, 11, 15);
end;


procedure TTestClipping.TestHorizontalLineCrossing;

begin
  CheckLine('a horizontal line through the rect is cut at x=0 and x=10', -5, 5, 15, 5, 0, 5, 10, 5);
  CheckLine('a horizontal line inside is unchanged', 2, 5, 8, 5, 2, 5, 8, 5);
end;


procedure TTestClipping.TestHorizontalLineRight;

begin
  CheckLineRejected('a horizontal line right of the rect, not collapsed to a point on the edge', 12, 5, 20, 5);
end;


procedure TTestClipping.TestHorizontalLineLeft;

begin
  CheckLineRejected('a horizontal line left of the rect, not collapsed to a point on the edge', -20, 5, -12, 5);
end;


procedure TTestClipping.TestHorizontalLineAbove;

begin
  CheckLineRejected('a horizontal line above the rect', 2, -3, 8, -3);
end;


procedure TTestClipping.TestHorizontalLineBelow;

begin
  CheckLineRejected('a horizontal line below the rect', -5, 12, 15, 12);
end;


procedure TTestClipping.TestPointLineInside;

begin
  CheckLine('a line of one point inside is unchanged', 3, 3, 3, 3, 3, 3, 3, 3);
end;


procedure TTestClipping.TestPointLineOutside;

begin
  CheckLineRejected('a line of one point outside', 20, 20, 20, 20);
end;


procedure TTestClipping.TestLineWithUnsortedClipRect;

var
  lX1, lY1, lX2, lY2: Integer;

begin
  lX1 := -10;
  lY1 := -10;
  lX2 := 20;
  lY2 := 20;
  CheckLineClipping(Rect(10, 10, 0, 0), lX1, lY1, lX2, lY2);
  CheckRect('a line clipped to an unsorted clip rect', 0, 0, 10, 10, Rect(lX1, lY1, lX2, lY2));
end;


{ TTestImgCmn }

function TTestImgCmn.CRCOf(const aText: AnsiString): LongWord;

var
  lBytes: TBytes;

begin
  lBytes := TEncoding.ASCII.GetBytes(UnicodeString(aText));
  if Length(lBytes) = 0 then
    SetLength(lBytes, 1);
  Result := CalculateCRC(lBytes[0], Length(aText));
end;


function TTestImgCmn.CRCUpdate(aCRC: LongWord; const aText: AnsiString): LongWord;

var
  lBytes: TBytes;

begin
  lBytes := TEncoding.ASCII.GetBytes(UnicodeString(aText));
  if Length(lBytes) = 0 then
    SetLength(lBytes, 1);
  Result := CalculateCRC(aCRC, lBytes[0], Length(aText));
end;


procedure TTestImgCmn.TestSwapWord;

begin
  AssertEquals('Swap of the word $1234 is $3412', $3412, fpimgcmn.Swap(Word($1234)));
  AssertEquals('Swap of the word $00FF is $FF00', $FF00, fpimgcmn.Swap(Word($00FF)));
end;


procedure TTestImgCmn.TestSwapLongWord;

begin
  AssertEquals('Swap of the longword $12345678 is $78563412', Int64($78563412), Int64(fpimgcmn.Swap(LongWord($12345678))));
  AssertEquals('Swap of the longword $000000FF is $FF000000', Int64($FF000000), Int64(fpimgcmn.Swap(LongWord($000000FF))));
end;


procedure TTestImgCmn.TestSwapInteger;

begin
  AssertEquals('Swap of the integer $12345678 is $78563412', Integer($78563412), fpimgcmn.Swap(Integer($12345678)));
  AssertEquals('Swap of the integer -2 ($FFFFFFFE) is $FEFFFFFF', Integer($FEFFFFFF), fpimgcmn.Swap(Integer(-2)));
end;


procedure TTestImgCmn.TestSwapQWord;

begin
  AssertTrue('Swap of the qword $0102030405060708 is $0807060504030201',
    fpimgcmn.Swap(QWord($0102030405060708)) = QWord($0807060504030201));
  AssertTrue('Swap of the qword $00000000000000FF is $FF00000000000000',
    fpimgcmn.Swap(QWord($00000000000000FF)) = QWord($FF00000000000000));
end;


procedure TTestImgCmn.TestSwapInt64;

begin
  AssertEquals('Swap of the int64 $0102030405060708 is $0807060504030201', Int64($0807060504030201),
    fpimgcmn.Swap(Int64($0102030405060708)));
  AssertEquals('Swap of the int64 -2 is $FEFFFFFFFFFFFFFF', Int64(QWord($FEFFFFFFFFFFFFFF)), fpimgcmn.Swap(Int64(-2)));
end;


procedure TTestImgCmn.TestSwapTwiceIsIdentity;

var
  I: Integer;
  lValue: LongWord;

begin
  for I := 0 to 1000 do
    begin
    lValue := (Int64(I) * 2654435761) and $FFFFFFFF;
    AssertEquals('swapping a longword twice gives it back', Int64(lValue), Int64(fpimgcmn.Swap(fpimgcmn.Swap(lValue))));
    AssertEquals('swapping a word twice gives it back', Word(lValue), fpimgcmn.Swap(fpimgcmn.Swap(Word(lValue))));
    end;
end;


procedure TTestImgCmn.TestCRCOfCheckString;

begin
  AssertEquals('the CRC-32 of "123456789" is the standard check value $CBF43926', Int64($CBF43926), Int64(CRCOf('123456789')));
end;


procedure TTestImgCmn.TestCRCOfIEND;

begin
  AssertEquals('the CRC-32 of "IEND" is $AE426082, the CRC of every PNG IEND chunk', Int64($AE426082), Int64(CRCOf('IEND')));
end;


procedure TTestImgCmn.TestCRCOfNothing;

begin
  AssertEquals('the CRC-32 of no bytes is 0', 0, Int64(CRCOf('')));
end;


procedure TTestImgCmn.TestCRCRegisterAsUsedByPNG;

var
  lCRC: LongWord;

begin
  lCRC := CRCUpdate($FFFFFFFF, 'IEND');
  AssertEquals('the register started at $FFFFFFFF and inverted at the end gives the CRC-32 of "IEND"',
    Int64($AE426082), Int64(lCRC xor $FFFFFFFF));
  AssertEquals('the register form without data returns the start value', Int64($12345678), Int64(CRCUpdate($12345678, '')));
end;


procedure TTestImgCmn.TestCRCInPieces;

var
  lCRC: LongWord;

begin
  lCRC := CRCUpdate($FFFFFFFF, '1234');
  lCRC := CRCUpdate(lCRC, '56789');
  AssertEquals('the register updated in two pieces gives the CRC-32 of the whole', Int64($CBF43926), Int64(lCRC xor $FFFFFFFF));
end;


{ TTestUnitOfMeasure }

procedure TTestUnitOfMeasure.TestInchToOtherUnits;

begin
  AssertEquals('1 inch is 2.54 cm', 2.54, PhysicalSizeConvert(uomInches, 1, uomCentimeters), 1e-5);
  AssertEquals('1 inch is 25.4 mm', 25.4, PhysicalSizeConvert(uomInches, 1, uomMillimeters), 1e-4);
  AssertEquals('1 inch is 72 points', 72, PhysicalSizeConvert(uomInches, 1, uomPoints), 1e-4);
  AssertEquals('1 inch is 6 picas', 6, PhysicalSizeConvert(uomInches, 1, uomPicas), 1e-5);
  AssertEquals('2.54 cm is 1 inch', 1, PhysicalSizeConvert(uomCentimeters, 2.54, uomInches), 1e-5);
end;


procedure TTestUnitOfMeasure.TestMetricUnits;

begin
  AssertEquals('1 cm is 10 mm', 10, PhysicalSizeConvert(uomCentimeters, 1, uomMillimeters), 1e-5);
  AssertEquals('25 mm is 2.5 cm', 2.5, PhysicalSizeConvert(uomMillimeters, 25, uomCentimeters), 1e-5);
end;


procedure TTestUnitOfMeasure.TestTypographicUnits;

begin
  AssertEquals('1 pica is 12 points', 12, PhysicalSizeConvert(uomPicas, 1, uomPoints), 1e-5);
  AssertEquals('36 points are half an inch', 0.5, PhysicalSizeConvert(uomPoints, 36, uomInches), 1e-6);
  AssertEquals('1 point is 0.3528 mm', 25.4 / 72, PhysicalSizeConvert(uomPoints, 1, uomMillimeters), 1e-5);
end;


procedure TTestUnitOfMeasure.TestSameUnit;

begin
  AssertEquals('converting to the same unit keeps the size', 3.25, PhysicalSizeConvert(uomPicas, 3.25, uomPicas), 1e-6);
  AssertEquals('pixels to pixels keeps the size', 17, PhysicalSizeConvert(uomPixels, 17, uomPixels), 1e-6);
end;


procedure TTestUnitOfMeasure.TestPhysicalSizeToPixels;

begin
  AssertEquals('1 inch at the default 96 DPI is 96 pixels', 96, PhysicalSizeToPixels(uomInches, 1), 1e-4);
  AssertEquals('2.54 cm at 96 DPI is 96 pixels', 96, PhysicalSizeToPixels(uomCentimeters, 2.54), 1e-3);
  AssertEquals('72 points at 300 DPI are 300 pixels', 300, PhysicalSizeToPixels(uomPoints, 72, ruPixelsPerInch, 300), 1e-3);
  AssertEquals('pixels stay pixels', 42, PhysicalSizeToPixels(uomPixels, 42, ruPixelsPerInch, 300), 1e-6);
end;


procedure TTestUnitOfMeasure.TestPhysicalSizeToPixelsPerCentimeter;

begin
  AssertEquals('2 cm at 40 pixels/cm are 80 pixels', 80, PhysicalSizeToPixels(uomCentimeters, 2, ruPixelsPerCentimeter, 40), 1e-4);
  AssertEquals('25.4 mm at 40 pixels/cm are 101.6 pixels', 101.6,
    PhysicalSizeToPixels(uomMillimeters, 25.4, ruPixelsPerCentimeter, 40), 1e-3);
  AssertEquals('1 inch at 40 pixels/cm is 101.6 pixels', 101.6, PhysicalSizeToPixels(uomInches, 1, ruPixelsPerCentimeter, 40), 1e-3);
end;


procedure TTestUnitOfMeasure.TestIllDefinedResolution;

begin
  AssertEquals('a resolution of 0 is taken as 96 DPI', 96, PhysicalSizeToPixels(uomInches, 1, ruPixelsPerInch, 0), 1e-4);
  AssertEquals('the resolution unit ruNone is taken as 96 DPI', 96, PhysicalSizeToPixels(uomInches, 1, ruNone, 300), 1e-4);
  AssertEquals('pixels to inches with a resolution of 0 uses 96 DPI', 1, PixelsToPhysicalSize(96, uomInches, ruPixelsPerInch, 0), 1e-5);
end;


procedure TTestUnitOfMeasure.TestPixelsToPhysicalSize;

begin
  AssertEquals('96 pixels at 96 DPI are 1 inch', 1, PixelsToPhysicalSize(96), 1e-6);
  AssertEquals('300 pixels at 300 DPI are 72 points', 72, PixelsToPhysicalSize(300, uomPoints, ruPixelsPerInch, 300), 1e-3);
  AssertEquals('96 pixels at 96 DPI are 2.54 cm', 2.54, PixelsToPhysicalSize(96, uomCentimeters), 1e-5);
  AssertEquals('80 pixels at 40 pixels/cm are 2 cm', 2, PixelsToPhysicalSize(80, uomCentimeters, ruPixelsPerCentimeter, 40), 1e-5);
  AssertEquals('pixels to pixels keeps the size', 42, PixelsToPhysicalSize(42, uomPixels), 1e-6);
end;


procedure TTestUnitOfMeasure.TestConvertThroughPixels;

begin
  AssertEquals('1 inch to pixels at 150 DPI is 150', 150, PhysicalSizeConvert(uomInches, 1, uomPixels, ruPixelsPerInch, 150), 1e-3);
  AssertEquals('150 pixels to inches at 150 DPI is 1', 1, PhysicalSizeConvert(uomPixels, 150, uomInches, ruPixelsPerInch, 150), 1e-5);
end;


procedure TTestUnitOfMeasure.TestRoundTrips;

var
  lFrom, lTo: TUnitOfMeasure;
  lBack: Single;

begin
  for lFrom := Low(TUnitOfMeasure) to High(TUnitOfMeasure) do
    for lTo := Low(TUnitOfMeasure) to High(TUnitOfMeasure) do
      begin
      lBack := PhysicalSizeConvert(lTo, PhysicalSizeConvert(lFrom, 12.5, lTo, ruPixelsPerInch, 300), lFrom, ruPixelsPerInch, 300);
      AssertEquals(Format('12.5 %s to %s and back', [UnitOfMeasureShortNames[lFrom], UnitOfMeasureShortNames[lTo]]),
        12.5, lBack, 1e-4);
      end;
end;


procedure TTestUnitOfMeasure.TestRectFToPixels;

var
  lRect: TRectF;

begin
  lRect := PhysicalSizeToPixels(uomInches, RectF(1, 2, 3, 4), ruPixelsPerInch, 100, 200);
  AssertEquals('the left of a rectangle uses the horizontal resolution', 100, lRect.Left, 1e-3);
  AssertEquals('the top of a rectangle uses the vertical resolution', 400, lRect.Top, 1e-3);
  AssertEquals('the right of a rectangle uses the horizontal resolution', 300, lRect.Right, 1e-3);
  AssertEquals('the bottom of a rectangle uses the vertical resolution', 800, lRect.Bottom, 1e-3);
  lRect := PixelsToPhysicalSize(RectF(100, 400, 300, 800), uomInches, ruPixelsPerInch, 100, 200);
  AssertEquals('back to inches: left', 1, lRect.Left, 1e-5);
  AssertEquals('back to inches: top', 2, lRect.Top, 1e-5);
  AssertEquals('back to inches: right', 3, lRect.Right, 1e-5);
  AssertEquals('back to inches: bottom', 4, lRect.Bottom, 1e-5);
end;


procedure TTestUnitOfMeasure.TestRectToPixels;

var
  lRect: TRect;

begin
  lRect := PhysicalSizeToPixels(uomInches, Rect(0, 0, 1, 2));
  AssertEquals('an integer rectangle in inches: right at 96 DPI', 96, lRect.Right);
  AssertEquals('an integer rectangle in inches: bottom at 96 DPI', 192, lRect.Bottom);
  lRect := PhysicalSizeToPixels(uomMillimeters, Rect(1, 1, 3, 3));
  AssertEquals('1 mm at 96 DPI (3.78 pixels) rounds to 4', 4, lRect.Left);
  AssertEquals('3 mm at 96 DPI (11.34 pixels) rounds to 11', 11, lRect.Right);
end;


procedure TTestUnitOfMeasure.TestRectToPixelsPreservingData;

var
  lRect: TRect;

begin
  lRect := PhysicalSizeToPixels(uomMillimeters, Rect(1, 1, 3, 3), True);
  AssertEquals('preserving data, 1 mm (3.78 pixels) as left floors to 3', 3, lRect.Left);
  AssertEquals('preserving data, 1 mm (3.78 pixels) as top floors to 3', 3, lRect.Top);
  AssertEquals('preserving data, 3 mm (11.34 pixels) as right ceils to 12', 12, lRect.Right);
  AssertEquals('preserving data, 3 mm (11.34 pixels) as bottom ceils to 12', 12, lRect.Bottom);
end;


procedure TTestUnitOfMeasure.TestConvertRectF;

var
  lRect: TRectF;

begin
  lRect := PhysicalSizeConvert(uomInches, RectF(1, 2, 3, 4), uomCentimeters);
  AssertEquals('a rectangle in inches to cm: left', 2.54, lRect.Left, 1e-4);
  AssertEquals('a rectangle in inches to cm: top', 5.08, lRect.Top, 1e-4);
  AssertEquals('a rectangle in inches to cm: right', 7.62, lRect.Right, 1e-4);
  AssertEquals('a rectangle in inches to cm: bottom', 10.16, lRect.Bottom, 1e-4);
end;


procedure TTestUnitOfMeasure.TestConvertRectFToPixelsUsesTheResolution;

var
  lRect: TRectF;

begin
  lRect := PhysicalSizeConvert(uomInches, RectF(1, 1, 2, 2), uomPixels, ruPixelsPerInch, 300);
  AssertEquals('a rectangle in inches to pixels at 300 DPI: left', 300, lRect.Left, 1e-3);
  AssertEquals('a rectangle in inches to pixels at 300 DPI: right', 600, lRect.Right, 1e-3);
  AssertEquals('a rectangle in inches to pixels at 300 DPI: top', 300, lRect.Top, 1e-3);
  AssertEquals('a rectangle in inches to pixels at 300 DPI: bottom', 600, lRect.Bottom, 1e-3);
end;


procedure TTestUnitOfMeasure.TestConvertRectFFromPixelsUsesTheResolution;

var
  lRect: TRectF;

begin
  lRect := PhysicalSizeConvert(uomPixels, RectF(300, 300, 600, 600), uomInches, ruPixelsPerInch, 300);
  AssertEquals('a rectangle in pixels to inches at 300 DPI: left', 1, lRect.Left, 1e-5);
  AssertEquals('a rectangle in pixels to inches at 300 DPI: right', 2, lRect.Right, 1e-5);
  AssertEquals('a rectangle in pixels to inches at 300 DPI: top', 1, lRect.Top, 1e-5);
  AssertEquals('a rectangle in pixels to inches at 300 DPI: bottom', 2, lRect.Bottom, 1e-5);
end;


procedure TTestUnitOfMeasure.TestNames;

begin
  AssertEquals('the short name of millimeters', 'mm', UnitOfMeasureShortNames[uomMillimeters]);
  AssertEquals('the name of points', 'Points', UnitOfMeasureNames[uomPoints]);
  AssertTrue('pixels per inch have inches as denominator', ResolutionDenominatorUnit[ruPixelsPerInch] = uomInches);
  AssertTrue('pixels per centimeter have centimeters as denominator', ResolutionDenominatorUnit[ruPixelsPerCentimeter] = uomCentimeters);
end;


{ TTestPapers }

procedure TTestPapers.CheckPaper(const aMessage: String; const aPapers: TPaperSizes; const aName: String; aWidth, aHeight, aTolerance: Single);

var
  I: Integer;

begin
  for I := 0 to High(aPapers) do
    if aPapers[I].name = aName then
      begin
      AssertEquals(aMessage + ': width', aWidth, aPapers[I].w, aTolerance);
      AssertEquals(aMessage + ': height', aHeight, aPapers[I].h, aTolerance);
      Exit;
      end;
  Fail(aMessage + ': the paper ' + aName + ' is listed');
end;


procedure TTestPapers.CheckTables(const aName: String; const aCm, aInch: TPaperSizes; aTolerance: Single);

var
  I: Integer;

begin
  AssertEquals(aName + ': both tables list the same number of papers', Length(aCm), Length(aInch));
  for I := 0 to High(aCm) do
    begin
    AssertEquals(Format('%s: paper %d has the same name in both tables', [aName, I]), aCm[I].name, aInch[I].name);
    AssertEquals(Format('%s %s: the width in inches times 2.54 is the width in cm (to %g cm)', [aName, aCm[I].name, aTolerance]),
      aCm[I].w, aInch[I].w * 2.54, aTolerance);
    AssertEquals(Format('%s %s: the height in inches times 2.54 is the height in cm (to %g cm)', [aName, aCm[I].name, aTolerance]),
      aCm[I].h, aInch[I].h * 2.54, aTolerance);
    end;
end;


procedure TTestPapers.TestISO216A;

begin
  CheckPaper('A0 is 841 x 1189 mm (ISO 216)', Paper_A_cm, 'A0', 84.1, 118.9, 0.001);
  CheckPaper('A3 is 297 x 420 mm (ISO 216)', Paper_A_cm, 'A3', 29.7, 42.0, 0.001);
  CheckPaper('A4 is 210 x 297 mm (ISO 216)', Paper_A_cm, 'A4', 21.0, 29.7, 0.001);
  CheckPaper('A5 is 148 x 210 mm (ISO 216)', Paper_A_cm, 'A5', 14.8, 21.0, 0.001);
  CheckPaper('A10 is 26 x 37 mm (ISO 216)', Paper_A_cm, 'A10', 2.6, 3.7, 0.001);
  CheckPaper('A4 is 8.268 x 11.693 inches', Paper_A_inch, 'A4', 210 / 25.4, 297 / 25.4, 0.002);
end;


procedure TTestPapers.TestISO216B;

begin
  CheckPaper('B0 is 1000 x 1414 mm (ISO 216)', Paper_B_cm, 'B0', 100.0, 141.4, 0.001);
  CheckPaper('B5 is 176 x 250 mm (ISO 216)', Paper_B_cm, 'B5', 17.6, 25.0, 0.001);
end;


procedure TTestPapers.TestISO216C;

begin
  CheckPaper('C4 is 229 x 324 mm (ISO 269)', Paper_C_cm, 'C4', 22.9, 32.4, 0.001);
  CheckPaper('C5 is 162 x 229 mm (ISO 269)', Paper_C_cm, 'C5', 16.2, 22.9, 0.001);
end;


procedure TTestPapers.TestASeriesHalves;

var
  I: Integer;

begin
  for I := 1 to High(Paper_A_cm) do
    AssertEquals(Format('the height of %s is the width of %s (halving)', [Paper_A_cm[I].name, Paper_A_cm[I - 1].name]),
      Paper_A_cm[I - 1].w, Paper_A_cm[I].h, 0.001);
end;


procedure TTestPapers.TestUSSizes;

begin
  CheckPaper('Letter is 8.5 x 11 inches', Paper_US_inch, 'Letter', 8.5, 11, 0.001);
  CheckPaper('Legal is 8.5 x 14 inches', Paper_US_inch, 'Legal', 8.5, 14, 0.001);
  CheckPaper('Tabloid is 11 x 17 inches', Paper_US_inch, 'Tabloid', 11, 17, 0.001);
  CheckPaper('Executive is 7.25 x 10.5 inches', Paper_US_inch, 'Executive', 7.25, 10.5, 0.001);
  CheckPaper('Letter is 21.59 x 27.94 cm, to the mm', Paper_US_cm, 'Letter', 21.59, 27.94, 0.05);
end;


procedure TTestPapers.TestANSISizes;

begin
  CheckPaper('ANSI A is 8.5 x 11 inches', Paper_ANSI_inch, 'A', 8.5, 11, 0.001);
  CheckPaper('ANSI C is 17 x 22 inches', Paper_ANSI_inch, 'C', 17, 22, 0.001);
  CheckPaper('ANSI E is 34 x 44 inches', Paper_ANSI_inch, 'E', 34, 44, 0.001);
end;


procedure TTestPapers.TestJISB;

begin
  CheckPaper('JIS B4 is 257 x 364 mm', Paper_JIS_cm, 'B4', 25.7, 36.4, 0.001);
  CheckPaper('JIS B5 is 182 x 257 mm', Paper_JIS_cm, 'B5', 18.2, 25.7, 0.001);
end;


procedure TTestPapers.TestCmAndInchTablesAgree;

begin
  CheckTables('ISO A', Paper_A_cm, Paper_A_inch, 0.05);
  CheckTables('ISO B', Paper_B_cm, Paper_B_inch, 0.05);
  CheckTables('ISO C', Paper_C_cm, Paper_C_inch, 0.05);
  CheckTables('DIN 476', Paper_DIN_476_cm, Paper_DIN_476_inch, 0.05);
  CheckTables('JIS B', Paper_JIS_cm, Paper_JIS_inch, 0.05);
  CheckTables('Shiroku ban', Paper_Shiroku_ban_cm, Paper_Shiroku_ban_inch, 0.05);
  CheckTables('Kiku', Paper_Kiku_cm, Paper_Kiku_inch, 0.05);
  CheckTables('US', Paper_US_cm, Paper_US_inch, 0.05);
  CheckTables('ANSI', Paper_ANSI_cm, Paper_ANSI_inch, 0.05);
  CheckTables('Photo', Photo_cm, Photo_inch, 0.05);
  CheckTables('Business card', Paper_BUSINESS_CARD_cm, Paper_BUSINESS_CARD_inch, 0.05);
end;


procedure TTestPapers.TestInchToCm;

var
  lPapers: TPaperSizes;

begin
  lPapers := Sizes_InchToCm(Paper_US_inch);
  AssertEquals('converting to cm keeps the number of papers', Length(Paper_US_inch), Length(lPapers));
  CheckPaper('Letter converted to cm is 21.59 x 27.94', lPapers, 'Letter', 21.59, 27.94, 0.001);
end;


procedure TTestPapers.TestCmToInch;

var
  lPapers: TPaperSizes;

begin
  lPapers := Sizes_CmToInch(Paper_A_cm);
  AssertEquals('converting to inches keeps the number of papers', Length(Paper_A_cm), Length(lPapers));
  CheckPaper('A4 converted to inches is 8.268 x 11.693', lPapers, 'A4', 21.0 / 2.54, 29.7 / 2.54, 0.001);
end;


procedure TTestPapers.TestConversionLeavesTheSourceAlone;

var
  lPapers: TPaperSizes;

begin
  lPapers := Sizes_CmToInch(Paper_A_cm);
  AssertEquals('the converted copy is in inches', 21.0 / 2.54, lPapers[4].w, 0.001);
  CheckPaper('the source table stays in cm after a conversion', Paper_A_cm, 'A4', 21.0, 29.7, 0.001);
end;


procedure TTestPapers.TestGetPaperSize;

var
  lPaper: TPaperSize;

begin
  lPaper := GetPaperSize(20, 28, [Paper_A_cm]);
  AssertEquals('the smallest A paper holding 20 x 28 cm is A4', 'A4', lPaper.name);
  lPaper := GetPaperSize(15, 20, [Paper_A_cm]);
  AssertEquals('the smallest A paper holding 15 x 20 cm is A4, A5 being 14.8 wide', 'A4', lPaper.name);
  lPaper := GetPaperSize(1, 1, [Paper_A_cm]);
  AssertEquals('the smallest A paper holding 1 x 1 cm is A10', 'A10', lPaper.name);
end;


procedure TTestPapers.TestGetPaperSizeExactFit;

begin
  AssertEquals('A4 holds exactly 21 x 29.7 cm', 'A4', GetPaperSize(21.0, 29.7, [Paper_A_cm]).name);
end;


procedure TTestPapers.TestGetPaperSizeTooLarge;

var
  lPaper: TPaperSize;

begin
  lPaper := GetPaperSize(200, 200, [Paper_A_cm]);
  AssertEquals('no paper holds 200 x 200 cm: the name is empty', '', lPaper.name);
  AssertEquals('no paper holds 200 x 200 cm: the width is 0', 0, lPaper.w, 0);
end;


procedure TTestPapers.TestGetPaperSizeFromSeveralTables;

begin
  AssertEquals('Letter is the smallest of A and US papers holding 21.5 x 27.5 cm', 'Letter',
    GetPaperSize(21.5, 27.5, [Paper_A_cm, Paper_US_cm]).name);
  AssertEquals('A4 is the smallest of A and US papers holding 20 x 28 cm', 'A4',
    GetPaperSize(20, 28, [Paper_US_cm, Paper_A_cm]).name);
end;


initialization
  RegisterTests('misc', [TTestClipping, TTestImgCmn, TTestUnitOfMeasure, TTestPapers]);
end.
