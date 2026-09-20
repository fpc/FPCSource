{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Unit tests for the fpsvg.read attribute grammars.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgread;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Hash.Base64,
     System.ZLib.Zstream, FpcUnit.Test, FpcUnit.Registry,
     fpsvg.types, fpsvg.dom, fpsvg.read;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, base64, zstream, fpcunit, testregistry,
     fpsvg.types, fpsvg.dom, fpsvg.read;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Reading the document inside a data URI, gzipped or not. }
  TTestSVGDataURI = class(TTestCase)
  published
    procedure TestAPlainDataURIIsReadAsADocument;
    procedure TestAGzippedDataURIIsReadAsADocument;
    procedure TestAGzipMemberWithoutANameIsReadToo;
    procedure TestADataURIWrittenWithEscapesIsReadToo;
    procedure TestADataURIOfAnotherTypeHasNoDocument;
    procedure TestWhatIsNotAURIIsNotOne;
    procedure TestTextOutsideASCIIComesBackByteForByte;
    procedure TestAGzippedDocumentKeepsTextOutsideASCII;
  end;

  { The views a document declares, and the fragment that selects one. }
  TTestSVGViews = class(TTestCase)
  private
    FDocument: TSVGDocument;
    procedure LoadSource(const aText: String);
  protected
    procedure TearDown; override;
  published
    procedure TestADocumentWithoutViewsDeclaresNone;
    procedure TestTheViewsComeInDocumentOrder;
    procedure TestAViewKeepsItsViewBoxAndRatio;
    procedure TestAViewWithoutARatioLeavesItToTheRoot;
    procedure TestAFragmentFindsTheViewOfThatName;
    procedure TestAFragmentNamingAShapeFindsNoView;
    procedure TestAFragmentNamingNothingFindsNoView;
    procedure TestAnSVGViewSpecificationGivesItsViewBox;
    procedure TestAnSVGViewSpecificationGivesItsRatio;
    procedure TestAnSVGViewSpecificationGivesItsTransform;
    procedure TestAnSVGViewSpecificationReadsAnEscapedSeparator;
    procedure TestAnEmptySVGViewSpecificationGivesNothing;
  end;

  TTestSVGNumberGrammar = class(TTestCase)
  published
    procedure TestPlainInteger;
    procedure TestSignedFraction;
    procedure TestLeadingDot;
    procedure TestExponent;
    procedure TestExponentWithoutDigitsStopsBeforeE;
    procedure TestCommaIsNotADecimalSeparator;
    procedure TestEmptyIsRejected;
    procedure TestTrailingGarbageIsRejected;
  end;

  TTestSVGLengthGrammar = class(TTestCase)
  published
    procedure TestUnitlessIsANumber;
    procedure TestPercent;
    procedure TestAbsoluteUnits;
    procedure TestUnknownUnitIsRejected;
    procedure TestResolvePercentAgainstBase;
    procedure TestResolveAbsoluteUnits;
  end;

  TTestSVGListGrammar = class(TTestCase)
  published
    procedure TestCommaSeparated;
    procedure TestWhitespaceSeparated;
    procedure TestSignAsSeparator;
    procedure TestEmptyListIsEmpty;
    procedure TestALoneCoordinateEndsThePointList;
    procedure TestRubbishEndsThePointList;
    procedure TestAPointListOfNoWholePairIsRejected;
    procedure TestPointList;
  end;

  TTestSVGTransformGrammar = class(TTestCase)
  private
    procedure AssertMaps(const aMessage, aTransform: String;
      aInX, aInY, aOutX, aOutY: Double);
  published
    procedure TestTranslate;
    procedure TestScaleWithOneArgument;
    procedure TestMatrix;
    procedure TestRotateAboutOrigin;
    procedure TestRotateAboutPoint;
    procedure TestSkewX;
    procedure TestRightmostAppliesFirst;
    procedure TestCommaSeparatedList;
    procedure TestWrongArgumentCountIsRejected;
    procedure TestUnknownFunctionIsRejected;
    procedure TestMissingParenthesisIsRejected;
    procedure TestEmptyIsIdentity;
  end;

  TTestSVGViewBoxGrammar = class(TTestCase)
  published
    procedure TestFourNumbers;
    procedure TestNegativeWidthIsRejected;
    procedure TestThreeNumbersAreRejected;
    procedure TestDefaultAspectRatio;
    procedure TestAlignAndSlice;
    procedure TestDeferIsAccepted;
    procedure TestUnknownAlignIsRejected;
  end;

  TTestSVGViewBoxTransform = class(TTestCase)
  private
    procedure AssertMaps(const aMessage: String; const aMatrix: TSVGMatrix;
      aInX, aInY, aOutX, aOutY: Double);
  published
    procedure TestIdentityMapping;
    procedure TestMeetLetterboxesVertically;
    procedure TestSliceFillsAndOverflows;
    procedure TestNoneStretches;
    procedure TestXMinYMinAnchorsAtOrigin;
    procedure TestDegenerateViewBoxIsIdentity;
  end;

implementation

const
  Delta = 1e-9;

{ TTestSVGNumberGrammar }

procedure TTestSVGNumberGrammar.TestPlainInteger;

var
  lValue: Double;

begin
  AssertTrue('an integer parses', TryStrToSVGNumber('42', lValue));
  AssertEquals('the integer keeps its value', 42, lValue, Delta);
end;


procedure TTestSVGNumberGrammar.TestSignedFraction;

var
  lValue: Double;

begin
  AssertTrue('a signed fraction parses', TryStrToSVGNumber('-2.5', lValue));
  AssertEquals('the sign and fraction are kept', -2.5, lValue, Delta);
end;


procedure TTestSVGNumberGrammar.TestLeadingDot;

var
  lValue: Double;

begin
  AssertTrue('a number may start with a dot', TryStrToSVGNumber('.5', lValue));
  AssertEquals('the leading dot means a fraction', 0.5, lValue, Delta);
end;


procedure TTestSVGNumberGrammar.TestExponent;

var
  lValue: Double;

begin
  AssertTrue('an exponent parses', TryStrToSVGNumber('1.5e2', lValue));
  AssertEquals('the exponent scales the value', 150, lValue, Delta);
  AssertTrue('a negative exponent parses', TryStrToSVGNumber('2E-2', lValue));
  AssertEquals('the negative exponent divides', 0.02, lValue, Delta);
end;


procedure TTestSVGNumberGrammar.TestExponentWithoutDigitsStopsBeforeE;

var
  lValue: Double;

begin
  AssertFalse('a bare e after a number is not a valid whole number',
    TryStrToSVGNumber('5e', lValue));
end;


procedure TTestSVGNumberGrammar.TestCommaIsNotADecimalSeparator;

var
  lValue: Double;

begin
  AssertFalse('a comma does not separate a decimal fraction',
    TryStrToSVGNumber('1,5', lValue));
end;


procedure TTestSVGNumberGrammar.TestEmptyIsRejected;

var
  lValue: Double;

begin
  AssertFalse('an empty string is not a number', TryStrToSVGNumber('', lValue));
  AssertFalse('whitespace alone is not a number', TryStrToSVGNumber('   ', lValue));
end;


procedure TTestSVGNumberGrammar.TestTrailingGarbageIsRejected;

var
  lValue: Double;

begin
  AssertFalse('trailing text is not part of a number',
    TryStrToSVGNumber('12abc', lValue));
end;


{ TTestSVGLengthGrammar }

procedure TTestSVGLengthGrammar.TestUnitlessIsANumber;

var
  lLength: TSVGLength;

begin
  AssertTrue('a bare number is a length', lLength.TryParse('10'));
  AssertEquals('the value is kept', 10, lLength.Value, Delta);
  AssertTrue('no unit means user units', lLength.LengthUnit = luNumber);
end;


procedure TTestSVGLengthGrammar.TestPercent;

var
  lLength: TSVGLength;

begin
  AssertTrue('a percentage is a length', lLength.TryParse('50%'));
  AssertEquals('the percentage keeps its value', 50, lLength.Value, Delta);
  AssertTrue('the unit is percent', lLength.LengthUnit = luPercent);
end;


procedure TTestSVGLengthGrammar.TestAbsoluteUnits;

var
  lLength: TSVGLength;

begin
  AssertTrue('millimetres parse', lLength.TryParse('3mm'));
  AssertTrue('the unit is millimetres', lLength.LengthUnit = luMm);
  AssertTrue('points parse', lLength.TryParse('12pt'));
  AssertTrue('the unit is points', lLength.LengthUnit = luPt);
  AssertTrue('an absolute unit needs no viewport',
    lLength.IsAbsolute);
  AssertTrue('em parses', lLength.TryParse('1.5em'));
  AssertFalse('em depends on the font size', lLength.IsAbsolute);
end;


procedure TTestSVGLengthGrammar.TestUnknownUnitIsRejected;

var
  lLength: TSVGLength;

begin
  AssertFalse('an unknown unit is rejected', lLength.TryParse('10qq'));
end;


procedure TTestSVGLengthGrammar.TestResolvePercentAgainstBase;

begin
  AssertEquals('a percentage resolves against the given base', 25,
    TSVGLength.Create(50, luPercent).Resolve(50, 16, 8, 96), Delta);
end;


procedure TTestSVGLengthGrammar.TestResolveAbsoluteUnits;

begin
  AssertEquals('an inch is the DPI in user units', 96,
    TSVGLength.Create(1, luIn).Resolve(0, 16, 8, 96), Delta);
  AssertEquals('a point is a seventy-second of an inch', 1,
    TSVGLength.Create(1, luPt).Resolve(0, 16, 8, 72), Delta);
  AssertEquals('an em is the font size', 24,
    TSVGLength.Create(1.5, luEm).Resolve(0, 16, 8, 96), Delta);
end;


{ TTestSVGListGrammar }

procedure TTestSVGListGrammar.TestCommaSeparated;

var
  lValues: TSVGDoubleArray;

begin
  AssertTrue('a comma-separated list parses',
    TryStrToSVGNumberList('1,2,3', lValues));
  AssertEquals('all three numbers are read', 3, Length(lValues));
  AssertEquals('the last number is read', 3, lValues[2], Delta);
end;


procedure TTestSVGListGrammar.TestWhitespaceSeparated;

var
  lValues: TSVGDoubleArray;

begin
  AssertTrue('whitespace separates list items',
    TryStrToSVGNumberList('  1   2 '#9'3'#10, lValues));
  AssertEquals('all three numbers are read', 3, Length(lValues));
end;


procedure TTestSVGListGrammar.TestSignAsSeparator;

var
  lValues: TSVGDoubleArray;

begin
  AssertTrue('a sign starts a new number without a separator',
    TryStrToSVGNumberList('1-2', lValues));
  AssertEquals('two numbers are read', 2, Length(lValues));
  AssertEquals('the second number keeps its sign', -2, lValues[1], Delta);
end;


procedure TTestSVGListGrammar.TestEmptyListIsEmpty;

var
  lValues: TSVGDoubleArray;

begin
  AssertTrue('an empty list parses', TryStrToSVGNumberList('   ', lValues));
  AssertEquals('an empty list holds no numbers', 0, Length(lValues));
end;


procedure TTestSVGListGrammar.TestALoneCoordinateEndsThePointList;

var
  lPoints: TSVGPointArray;

begin
  AssertTrue('the pairs before a lone coordinate are read',
    TryStrToSVGPointList('1 2 3', lPoints));
  AssertEquals('and the lone one is not a point of its own', 1,
    Length(lPoints));
  AssertEquals('the pair keeps its x', 1, lPoints[0].X, Delta);
  AssertEquals('the pair keeps its y', 2, lPoints[0].Y, Delta);
end;


procedure TTestSVGListGrammar.TestRubbishEndsThePointList;

var
  lPoints: TSVGPointArray;

begin
  AssertTrue('the pairs before the unreadable one are kept',
    TryStrToSVGPointList('1 2 3 4 nonsense 5 6', lPoints));
  AssertEquals('and nothing past it is', 2, Length(lPoints));
  AssertEquals('the last pair read keeps its x', 3, lPoints[1].X, Delta);
  AssertEquals('the last pair read keeps its y', 4, lPoints[1].Y, Delta);
end;


procedure TTestSVGListGrammar.TestAPointListOfNoWholePairIsRejected;

var
  lPoints: TSVGPointArray;

begin
  AssertFalse('one coordinate is no point at all',
    TryStrToSVGPointList('1', lPoints));
  AssertFalse('and neither is nothing', TryStrToSVGPointList('   ', lPoints));
  AssertFalse('nor a list that starts with an unreadable pair',
    TryStrToSVGPointList('nonsense 1 2', lPoints));
end;


procedure TTestSVGListGrammar.TestPointList;

var
  lPoints: TSVGPointArray;

begin
  AssertTrue('a point list parses', TryStrToSVGPointList('0,0 10,20', lPoints));
  AssertEquals('two points are read', 2, Length(lPoints));
  AssertEquals('the second point keeps its x', 10, lPoints[1].X, Delta);
  AssertEquals('the second point keeps its y', 20, lPoints[1].Y, Delta);
end;


{ TTestSVGTransformGrammar }

procedure TTestSVGTransformGrammar.AssertMaps(const aMessage, aTransform: String;
  aInX, aInY, aOutX, aOutY: Double);

var
  lMatrix: TSVGMatrix;
  lPoint: TSVGPoint;

begin
  AssertTrue(aMessage + ' (parses)', lMatrix.TryParse(aTransform));
  lPoint := lMatrix.Transform(TSVGPoint.Create(aInX, aInY));
  AssertEquals(aMessage + ' (x)', aOutX, lPoint.X, 1e-6);
  AssertEquals(aMessage + ' (y)', aOutY, lPoint.Y, 1e-6);
end;


procedure TTestSVGTransformGrammar.TestTranslate;

begin
  AssertMaps('translate shifts the point', 'translate(10,20)', 1, 2, 11, 22);
  AssertMaps('translate with one argument leaves y alone', 'translate(10)',
    1, 2, 11, 2);
end;


procedure TTestSVGTransformGrammar.TestScaleWithOneArgument;

begin
  AssertMaps('one scale argument scales both axes', 'scale(3)', 1, 2, 3, 6);
end;


procedure TTestSVGTransformGrammar.TestMatrix;

begin
  AssertMaps('matrix applies its six coefficients', 'matrix(2,0,0,3,4,5)',
    1, 1, 6, 8);
end;


procedure TTestSVGTransformGrammar.TestRotateAboutOrigin;

begin
  AssertMaps('rotate turns the x axis towards the y axis', 'rotate(90)',
    1, 0, 0, 1);
end;


procedure TTestSVGTransformGrammar.TestRotateAboutPoint;

begin
  AssertMaps('rotate about a centre leaves the centre fixed',
    'rotate(90,5,5)', 5, 5, 5, 5);
  AssertMaps('rotate about a centre turns around it',
    'rotate(90,5,5)', 6, 5, 5, 6);
end;


procedure TTestSVGTransformGrammar.TestSkewX;

begin
  AssertMaps('skewX shifts x in proportion to y', 'skewX(45)', 0, 2, 2, 2);
end;


procedure TTestSVGTransformGrammar.TestRightmostAppliesFirst;

begin
  AssertMaps('the rightmost transform applies to the point first',
    'translate(10,0) scale(2)', 1, 0, 12, 0);
  AssertMaps('reversing the list gives a different result',
    'scale(2) translate(10,0)', 1, 0, 22, 0);
end;


procedure TTestSVGTransformGrammar.TestCommaSeparatedList;

begin
  AssertMaps('transforms may be separated by commas',
    'translate(1,1),translate(2,2)', 0, 0, 3, 3);
end;


procedure TTestSVGTransformGrammar.TestWrongArgumentCountIsRejected;

var
  lMatrix: TSVGMatrix;

begin
  AssertFalse('translate takes at most two arguments',
    lMatrix.TryParse('translate(1,2,3)'));
  AssertFalse('matrix takes exactly six arguments',
    lMatrix.TryParse('matrix(1,2,3)'));
  AssertFalse('rotate takes one or three arguments',
    lMatrix.TryParse('rotate(1,2)'));
end;


procedure TTestSVGTransformGrammar.TestUnknownFunctionIsRejected;

var
  lMatrix: TSVGMatrix;

begin
  AssertFalse('an unknown transform function is rejected',
    lMatrix.TryParse('warp(2)'));
end;


procedure TTestSVGTransformGrammar.TestMissingParenthesisIsRejected;

var
  lMatrix: TSVGMatrix;

begin
  AssertFalse('an unclosed argument list is rejected',
    lMatrix.TryParse('translate(1,2'));
  AssertFalse('a missing argument list is rejected',
    lMatrix.TryParse('translate'));
end;


procedure TTestSVGTransformGrammar.TestEmptyIsIdentity;

var
  lMatrix: TSVGMatrix;

begin
  AssertTrue('an empty transform parses', lMatrix.TryParse(''));
  AssertTrue('an empty transform is the identity',
    lMatrix.IsIdentity);
end;


{ TTestSVGViewBoxGrammar }

procedure TTestSVGViewBoxGrammar.TestFourNumbers;

var
  lRect: TSVGRect;

begin
  AssertTrue('a viewBox of four numbers parses',
    lRect.TryParseViewBox('0 0 100 50'));
  AssertEquals('the width comes from the third number', 100,
    lRect.Width, Delta);
  AssertEquals('the height comes from the fourth number', 50,
    lRect.Height, Delta);
end;


procedure TTestSVGViewBoxGrammar.TestNegativeWidthIsRejected;

var
  lRect: TSVGRect;

begin
  AssertFalse('a negative width is an error',
    lRect.TryParseViewBox('0 0 -1 10'));
end;


procedure TTestSVGViewBoxGrammar.TestThreeNumbersAreRejected;

var
  lRect: TSVGRect;

begin
  AssertFalse('a viewBox needs four numbers',
    lRect.TryParseViewBox('0 0 100'));
end;


procedure TTestSVGViewBoxGrammar.TestDefaultAspectRatio;

var
  lAspect: TSVGPreserveAspectRatio;

begin
  lAspect := TSVGPreserveAspectRatio.Default;
  AssertTrue('the initial alignment is xMidYMid', lAspect.Align = paXMidYMid);
  AssertTrue('the initial mode is meet', lAspect.MeetOrSlice = msMeet);
end;


procedure TTestSVGViewBoxGrammar.TestAlignAndSlice;

var
  lAspect: TSVGPreserveAspectRatio;

begin
  AssertTrue('an alignment with slice parses',
    lAspect.TryParse('xMinYMax slice'));
  AssertTrue('the alignment is read', lAspect.Align = paXMinYMax);
  AssertTrue('slice is read', lAspect.MeetOrSlice = msSlice);
  AssertTrue('none parses', lAspect.TryParse('none'));
  AssertTrue('none disables uniform scaling', lAspect.Align = paNone);
  AssertTrue('meet is the default mode', lAspect.MeetOrSlice = msMeet);
end;


procedure TTestSVGViewBoxGrammar.TestDeferIsAccepted;

var
  lAspect: TSVGPreserveAspectRatio;

begin
  AssertTrue('a leading defer is accepted',
    lAspect.TryParse('defer xMaxYMid meet'));
  AssertTrue('the alignment after defer is read', lAspect.Align = paXMaxYMid);
end;


procedure TTestSVGViewBoxGrammar.TestUnknownAlignIsRejected;

var
  lAspect: TSVGPreserveAspectRatio;

begin
  AssertFalse('an unknown alignment is rejected',
    lAspect.TryParse('xMiddle'));
  AssertFalse('an unknown mode is rejected',
    lAspect.TryParse('xMidYMid squeeze'));
  AssertFalse('a case-mismatched alignment is rejected',
    lAspect.TryParse('xmidymid'));
end;


{ TTestSVGViewBoxTransform }

procedure TTestSVGViewBoxTransform.AssertMaps(const aMessage: String;
  const aMatrix: TSVGMatrix; aInX, aInY, aOutX, aOutY: Double);

var
  lPoint: TSVGPoint;

begin
  lPoint := aMatrix.Transform(TSVGPoint.Create(aInX, aInY));
  AssertEquals(aMessage + ' (x)', aOutX, lPoint.X, 1e-6);
  AssertEquals(aMessage + ' (y)', aOutY, lPoint.Y, 1e-6);
end;

procedure TTestSVGViewBoxTransform.TestIdentityMapping;

begin
  AssertMaps('a viewBox matching the viewport maps one to one',
    TSVGPreserveAspectRatio.Default.ViewBoxTransform(
      TSVGRect.CreateSize(0, 0, 100, 100), TSVGRect.CreateSize(0, 0, 100, 100)),
    50, 50, 50, 50);
end;


procedure TTestSVGViewBoxTransform.TestMeetLetterboxesVertically;

var
  lMatrix: TSVGMatrix;

begin
  lMatrix := TSVGPreserveAspectRatio.Default.ViewBoxTransform(
    TSVGRect.CreateSize(0, 0, 100, 100), TSVGRect.CreateSize(0, 0, 100, 200));
  AssertMaps('meet scales by the smaller factor', lMatrix, 100, 100, 100, 150);
  AssertMaps('meet centres the letterboxed content', lMatrix, 0, 0, 0, 50);
end;


procedure TTestSVGViewBoxTransform.TestSliceFillsAndOverflows;

var
  lAspect: TSVGPreserveAspectRatio;
  lMatrix: TSVGMatrix;

begin
  lAspect := TSVGPreserveAspectRatio.Create(paXMidYMid, msSlice);
  lMatrix := lAspect.ViewBoxTransform(TSVGRect.CreateSize(0, 0, 100, 100),
    TSVGRect.CreateSize(0, 0, 100, 200));
  AssertMaps('slice scales by the larger factor', lMatrix, 100, 100, 150, 200);
end;


procedure TTestSVGViewBoxTransform.TestNoneStretches;

var
  lAspect: TSVGPreserveAspectRatio;

begin
  lAspect := TSVGPreserveAspectRatio.Create(paNone, msMeet);
  AssertMaps('none scales each axis independently',
    lAspect.ViewBoxTransform(TSVGRect.CreateSize(0, 0, 100, 100),
      TSVGRect.CreateSize(0, 0, 100, 200)), 100, 100, 100, 200);
end;


procedure TTestSVGViewBoxTransform.TestXMinYMinAnchorsAtOrigin;

var
  lAspect: TSVGPreserveAspectRatio;

begin
  lAspect := TSVGPreserveAspectRatio.Create(paXMinYMin, msMeet);
  AssertMaps('xMinYMin puts the viewBox origin at the viewport origin',
    lAspect.ViewBoxTransform(TSVGRect.CreateSize(0, 0, 100, 100),
      TSVGRect.CreateSize(0, 0, 100, 200)), 0, 0, 0, 0);
end;


procedure TTestSVGViewBoxTransform.TestDegenerateViewBoxIsIdentity;

begin
  AssertTrue('a zero-width viewBox yields the identity',
    TSVGPreserveAspectRatio.Default.ViewBoxTransform(
      TSVGRect.CreateSize(0, 0, 0, 100),
      TSVGRect.CreateSize(0, 0, 100, 100)).IsIdentity);
end;


{ TTestSVGDataURI }

// The bytes a gzip member holds for the text, written the way RFC 1952
// lays one out: ten bytes of header, the deflate data with no header of
// its own, and eight bytes of check that nothing here reads.
function GzipOf(const aText: RawByteString;
  const aName: String): RawByteString;

const
  Header: array[0..9] of Byte = ($1F, $8B, 8, 0, 0, 0, 0, 0, 0, 3);

var
  lOut: TMemoryStream;
  lZip: Tcompressionstream;
  lName: RawByteString;
  lByte: Byte;
  I: Integer;

begin
  Result := '';
  lOut := TMemoryStream.Create;
  try
    for I := Low(Header) to High(Header) do
      begin
      lByte := Header[I];
      if (I = 3) and (aName <> '') then
        // The flag that says a name follows the fixed header.
        lByte := $08;
      lOut.Write(lByte, 1);
      end;
    if aName <> '' then
      begin
      lName := RawByteString(aName);
      lOut.Write(lName[1], Length(lName));
      lByte := 0;
      lOut.Write(lByte, 1);
      end;
    lZip := Tcompressionstream.create(clDefault, lOut, True);
    try
      if aText <> '' then
        lZip.WriteBuffer(aText[1], Length(aText));
    finally
      lZip.Free;
    end;
    lByte := 0;
    for I := 1 to 8 do
      lOut.Write(lByte, 1);
    SetLength(Result, lOut.Size);
    if lOut.Size > 0 then
      Move(lOut.Memory^, Result[1], lOut.Size);
  finally
    lOut.Free;
  end;
end;


function Base64Of(const aBytes: RawByteString): String;

var
  lPlain: TMemoryStream;
  lText: TStringStream;
  lEncoder: TBase64EncodingStream;

begin
  Result := '';
  lPlain := TMemoryStream.Create;
  lText := TStringStream.Create('');
  lEncoder := nil;
  try
    if aBytes <> '' then
      lPlain.Write(aBytes[1], Length(aBytes));
    lPlain.Position := 0;
    lEncoder := TBase64EncodingStream.Create(lText);
    lEncoder.CopyFrom(lPlain, lPlain.Size);
    lEncoder.Flush;
    Result := lText.DataString;
  finally
    lEncoder.Free;
    lText.Free;
    lPlain.Free;
  end;
end;


function StarSource: RawByteString;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg" width="40" '
    + 'height="40" viewBox="0 0 40 40"><rect width="10" height="10"/></svg>';
end;


procedure TTestSVGDataURI.TestAPlainDataURIIsReadAsADocument;

var
  lDocument: TSVGDocument;

begin
  lDocument := ReadSVGDataURI('data:image/svg+xml;base64,'
    + Base64Of(StarSource));
  try
    AssertNotNull('the document inside the uri is read', lDocument);
    AssertEquals('and is the one that was written', 'svg',
      lDocument.Root.TagName);
  finally
    lDocument.Free;
  end;
end;


procedure TTestSVGDataURI.TestAGzippedDataURIIsReadAsADocument;

var
  lDocument: TSVGDocument;

begin
  lDocument := ReadSVGDataURI('data:image/svg+xml;base64,'
    + Base64Of(GzipOf(StarSource, 'star.svg')));
  try
    AssertNotNull('a document written out with gzip is read', lDocument);
    AssertEquals('and is the one that was written', 'svg',
      lDocument.Root.TagName);
    AssertEquals('with its one child', 1, lDocument.Root.ChildCount);
  finally
    lDocument.Free;
  end;
end;


procedure TTestSVGDataURI.TestAGzipMemberWithoutANameIsReadToo;

var
  lDocument: TSVGDocument;

begin
  // A member may include the name of the file it was made from, and the
  // deflate data after the header moves by that much.
  lDocument := ReadSVGDataURI('data:image/svg+xml;base64,'
    + Base64Of(GzipOf(StarSource, '')));
  try
    AssertNotNull('a member without a name is read', lDocument);
    AssertEquals('and is the one that was written', 'svg',
      lDocument.Root.TagName);
  finally
    lDocument.Free;
  end;
end;


procedure TTestSVGDataURI.TestADataURIWrittenWithEscapesIsReadToo;

var
  lDocument: TSVGDocument;

begin
  lDocument := ReadSVGDataURI('data:image/svg+xml,'
    + '%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%3E'
    + '%3C%2Fsvg%3E');
  try
    AssertNotNull('a uri written with escapes is read', lDocument);
  finally
    lDocument.Free;
  end;
end;


procedure TTestSVGDataURI.TestADataURIOfAnotherTypeHasNoDocument;

begin
  AssertFalse('a png contains no document',
    SVGDataURIHoldsSVG('data:image/png;base64,iVBOR'));
  AssertTrue('and svg contains one',
    SVGDataURIHoldsSVG('data:image/svg+xml;base64,PHN2Zz4='));
  AssertNull('so nothing is read from the one that does not',
    ReadSVGDataURI('data:image/png;base64,iVBOR'));
end;


procedure TTestSVGDataURI.TestWhatIsNotAURIIsNotOne;

begin
  AssertFalse('a file name is no data uri', SVGIsDataURI('star.svg'));
  AssertTrue('and a data uri is', SVGIsDataURI('data:image/png,x'));
end;


// The three characters of the tests below, as the bytes UTF-8 writes
// them: e acute, the ideograph for sun, and the euro sign.
function AccentedBytes: RawByteString;

begin
  Result := #$C3#$A9 + ' ' + #$E6#$97#$A5 + ' ' + #$E2#$82#$AC;
end;


// A document whose text is written in UTF-8 and reaches past ASCII.
function AccentedSource: RawByteString;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg" width="40" '
    + 'height="40"><text x="1" y="2">' + AccentedBytes
    + '</text></svg>';
end;


procedure TTestSVGDataURI.TestTextOutsideASCIIComesBackByteForByte;

var
  lDocument: TSVGDocument;

begin
  lDocument := ReadSVGString(AccentedSource);
  try
    AssertNotNull('the document is read', lDocument);
    AssertTrue('and its text is the bytes that were written',
      lDocument.Root.FindChildElement('text').TextContent
      = TSVGString(AccentedBytes));
  finally
    lDocument.Free;
  end;
end;


procedure TTestSVGDataURI.TestAGzippedDocumentKeepsTextOutsideASCII;

var
  lDocument: TSVGDocument;
  lStream: TMemoryStream;
  lBytes: RawByteString;

begin
  lBytes := GzipOf(AccentedSource, 'accented.svg');
  lStream := TMemoryStream.Create;
  try
    lStream.Write(lBytes[1], Length(lBytes));
    lStream.Position := 0;
    lDocument := ReadSVGStream(lStream);
    try
      AssertNotNull('the gzipped document is read', lDocument);
      AssertTrue('and its text came through the inflate unchanged',
        lDocument.Root.FindChildElement('text').TextContent
        = TSVGString(AccentedBytes));
    finally
      lDocument.Free;
    end;
  finally
    lStream.Free;
  end;
end;


procedure TTestSVGViews.LoadSource(const aText: String);

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(aText);
end;


procedure TTestSVGViews.TearDown;

begin
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGViews.TestADocumentWithoutViewsDeclaresNone;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"><rect id="r"/></svg>');
  AssertEquals('no view is declared', 0, Length(SVGViewsOf(FDocument)));
end;


procedure TTestSVGViews.TestTheViewsComeInDocumentOrder;

var
  lViews: TSVGViewArray;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<view id="first" viewBox="0 0 10 10"/>'
    + '<g><view id="second" viewBox="10 0 10 10"/></g></svg>');
  lViews := SVGViewsOf(FDocument);
  AssertEquals('both views are found', 2, Length(lViews));
  AssertEquals('the first one comes first', 'first', lViews[0].Name);
  AssertEquals('and the nested one after it', 'second', lViews[1].Name);
end;


procedure TTestSVGViews.TestAViewKeepsItsViewBoxAndRatio;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<view id="corner" viewBox="10 20 30 40" '
    + 'preserveAspectRatio="xMinYMin slice"/></svg>');
  AssertTrue('the view is found', SVGFindView(FDocument, 'corner', lView));
  AssertTrue('it has a viewBox', lView.HasViewBox);
  AssertEquals('at the x it gives', 10.0, lView.ViewBox.Left, 0.001);
  AssertEquals('and the width it gives', 30.0, lView.ViewBox.Width, 0.001);
  AssertTrue('it has an aspect ratio', lView.HasRatio);
  AssertEquals('the one it gives', 'xMinYMin slice', lView.Ratio.ToString);
end;


procedure TTestSVGViews.TestAViewWithoutARatioLeavesItToTheRoot;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<view id="plain" viewBox="0 0 10 10"/></svg>');
  AssertTrue('the view is found', SVGFindView(FDocument, 'plain', lView));
  AssertTrue('it has a viewBox', lView.HasViewBox);
  AssertFalse('and no aspect ratio of its own', lView.HasRatio);
end;


procedure TTestSVGViews.TestAFragmentFindsTheViewOfThatName;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<view id="half" viewBox="0 0 50 50"/></svg>');
  AssertTrue('the fragment finds it',
    SVGViewOfFragment(FDocument, '#half', lView));
  AssertEquals('under its own name', 'half', lView.Name);
  AssertEquals('with the viewBox it gives', 50.0, lView.ViewBox.Width, 0.001);
end;


procedure TTestSVGViews.TestAFragmentNamingAShapeFindsNoView;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<rect id="box" width="10" height="10"/></svg>');
  AssertFalse('a shape is not a view',
    SVGViewOfFragment(FDocument, '#box', lView));
  AssertFalse('and nothing is reframed', lView.HasViewBox);
end;


procedure TTestSVGViews.TestAFragmentNamingNothingFindsNoView;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"><rect id="r"/></svg>');
  AssertFalse('a name nothing answers to finds no view',
    SVGViewOfFragment(FDocument, '#gone', lView));
end;


procedure TTestSVGViews.TestAnSVGViewSpecificationGivesItsViewBox;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"><rect id="r"/></svg>');
  AssertTrue('the specification is read',
    SVGViewOfFragment(FDocument, '#svgView(viewBox(5,10,20,40))', lView));
  AssertTrue('it has a viewBox', lView.HasViewBox);
  AssertEquals('at the x it gives', 5.0, lView.ViewBox.Left, 0.001);
  AssertEquals('the y it gives', 10.0, lView.ViewBox.Top, 0.001);
  AssertEquals('the width it gives', 20.0, lView.ViewBox.Width, 0.001);
  AssertEquals('and the height it gives', 40.0, lView.ViewBox.Height, 0.001);
end;


procedure TTestSVGViews.TestAnSVGViewSpecificationGivesItsRatio;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"><rect id="r"/></svg>');
  AssertTrue('the specification is read',
    SVGViewOfFragment(FDocument,
      '#svgView(viewBox(0,0,10,10);preserveAspectRatio(xMaxYMax))', lView));
  AssertTrue('it has an aspect ratio', lView.HasRatio);
  AssertEquals('the one it gives', 'xMaxYMax', lView.Ratio.ToString);
end;


procedure TTestSVGViews.TestAnSVGViewSpecificationGivesItsTransform;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"><rect id="r"/></svg>');
  AssertTrue('the specification is read',
    SVGViewOfFragment(FDocument,
      '#svgView(viewBox(64,227,72,72);transform(translate(36,36)))', lView));
  AssertTrue('it has a transform', lView.HasTransform);
  AssertEquals('which moves along x', 36.0, lView.Transform.e, 0.001);
  AssertEquals('and along y', 36.0, lView.Transform.f, 0.001);
  AssertTrue('the viewBox beside it is read as well', lView.HasViewBox);
end;


procedure TTestSVGViews.TestAnSVGViewSpecificationReadsAnEscapedSeparator;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"><rect id="r"/></svg>');
  AssertTrue('the specification is read',
    SVGViewOfFragment(FDocument,
      '#svgView(viewBox(64,227,72,72)%3Btransform(scale(2)))', lView));
  AssertTrue('the part after the escaped semicolon is a part of its own',
    lView.HasTransform);
  AssertEquals('and holds the scale it gives', 2.0, lView.Transform.a, 0.001);
  AssertEquals('the viewBox before it keeps its width', 72.0,
    lView.ViewBox.Width, 0.001);
end;


procedure TTestSVGViews.TestAnEmptySVGViewSpecificationGivesNothing;

var
  lView: TSVGView;

begin
  LoadSource('<svg xmlns="http://www.w3.org/2000/svg"><rect id="r"/></svg>');
  AssertFalse('a specification asking for nothing reframes nothing',
    SVGViewOfFragment(FDocument, '#svgView(zoomAndPan(disable))', lView));
end;


initialization
  RegisterTest('read', TTestSVGViews);
  RegisterTest('read', TTestSVGDataURI);
  RegisterTest('read', TTestSVGNumberGrammar);
  RegisterTest('read', TTestSVGLengthGrammar);
  RegisterTest('read', TTestSVGListGrammar);
  RegisterTest('read', TTestSVGTransformGrammar);
  RegisterTest('read', TTestSVGViewBoxGrammar);
  RegisterTest('read', TTestSVGViewBoxTransform);
end.
