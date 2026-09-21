{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the PDF backend.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgpdf;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.StrUtils, FpcUnit.Test,
     FpcUnit.Registry,
     FpPdf.Pdf, svgstubfont, fpsvg.types, fpsvg.dom, fpsvg.read,
     fpsvg.render, fpsvg.pdf;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, strutils, fpcunit, testregistry, fppdf, svgstubfont,
     fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.render, fpsvg.pdf;
{$ENDIF FPC_DOTTEDUNITS}

type

  { One image of a fixed size, every pixel of it red. }
  TTestPDFImage = class(TObject, ISVGImageSource, ISVGImageResolver)
  private
    FWidth, FHeight: Integer;
  public
    constructor Create(aWidth, aHeight: Integer);
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPixel(aX, aY: Integer): TSVGColor;
    function GetRow(aY, aX, aCount: Integer; aDest: PSVGColor): Boolean;
    function ResolveImage(const aHRef, aBaseURI: String): ISVGImageSource;
  end;

  { TTestSVGPDF }

  TTestSVGPDF = class(TTestCase)
  private
    FFonts: TSVGStubFontProvider;
    FDocument: TSVGDocument;
    FRenderer: TSVGRenderer;
    FBackend: TSVGPDFBackend;
    FFile: RawByteString;
    // Renders the markup and keeps the file the backend wrote.
    procedure Render(const aText: TSVGString);
    // Fails unless the file holds the text, whitespace runs counting as one.
    procedure AssertHas(const aMessage, aText: String);
    // Fails when the file holds the text.
    procedure AssertLacks(const aMessage, aText: String);
    // How often the file holds the text.
    function CountOf(const aText: String): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPageTakesTheSizeOfTheDocument;
    procedure TestScaleMultipliesThePageSize;
    procedure TestFillWritesTheColourAndTheOperator;
    procedure TestFillRuleWritesTheStarOperator;
    procedure TestStrokeWritesTheWidthAndTheOperator;
    procedure TestDashesAreWrittenWithTheirOffset;
    procedure TestTransformBecomesAMatrix;
    procedure TestClipPathBecomesAClipOperator;
    procedure TestFillOpacityBecomesAGraphicsState;
    procedure TestGroupOpacityBecomesAFormAndAState;
    procedure TestLinearGradientBecomesAnAxialShading;
    procedure TestRadialGradientBecomesARadialShading;
    procedure TestStopOpacityBecomesASoftMask;
    procedure TestMaskBecomesALuminositySoftMask;
    procedure TestImageBecomesAnXObject;
    procedure TestTextBecomesOutlines;
    procedure TestTwoFramesBecomeTwoPages;
    procedure TestEveryPageIsValidPDF;
  end;

implementation

// Wraps markup in a root element of a hundred units square.
function Doc(const aBody: TSVGString; const aRoot: TSVGString = ''): TSVGString;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100"'
    + aRoot + '>' + aBody + '</svg>';
end;


// The text with every run of whitespace as a single space, so that the
// lines a writer breaks do not matter.
function Flattened(const aText: RawByteString): RawByteString;

var
  I: Integer;
  lSpace: Boolean;

begin
  Result := '';
  lSpace := False;
  for I := 1 to Length(aText) do
    if aText[I] in [' ', #9, #10, #13] then
      lSpace := True
    else
      begin
      if lSpace and (Result <> '') then
        Result := Result + ' ';
      lSpace := False;
      Result := Result + aText[I];
      end;
end;


{ TTestPDFImage }

constructor TTestPDFImage.Create(aWidth, aHeight: Integer);

begin
  inherited Create;
  FWidth := aWidth;
  FHeight := aHeight;
end;


function TTestPDFImage.GetWidth: Integer;

begin
  Result := FWidth;
end;


function TTestPDFImage.GetHeight: Integer;

begin
  Result := FHeight;
end;


function TTestPDFImage.GetPixel(aX, aY: Integer): TSVGColor;

begin
  if aX + aY >= 0 then ;
  Result := TSVGColor.FromBytes(255, 0, 0, 255);
end;


function TTestPDFImage.GetRow(aY, aX, aCount: Integer;
  aDest: PSVGColor): Boolean;

begin
  if (aY >= 0) and (aX >= 0) and (aCount >= 0) and (aDest <> nil) then ;
  Result := False;
end;


function TTestPDFImage.ResolveImage(
  const aHRef, aBaseURI: String): ISVGImageSource;

begin
  if (aHRef <> '') or (aBaseURI <> '') then ;
  Result := Self;
end;


{ TTestSVGPDF }

procedure TTestSVGPDF.SetUp;

begin
  inherited SetUp;
  FFonts := TSVGStubFontProvider.Create;
  FRenderer := TSVGRenderer.Create;
  FRenderer.Fonts := FFonts;
end;


procedure TTestSVGPDF.TearDown;

begin
  FreeAndNil(FBackend);
  FreeAndNil(FRenderer);
  FreeAndNil(FDocument);
  FreeAndNil(FFonts);
  FFile := '';
  inherited TearDown;
end;


procedure TTestSVGPDF.Render(const aText: TSVGString);

var
  lStream: TMemoryStream;

begin
  FreeAndNil(FDocument);
  if FBackend = nil then
    FBackend := TSVGPDFBackend.Create;
  // The content is read back as text, so it is not compressed.
  FBackend.Document.Options := FBackend.Document.Options - [poCompressText];
  FDocument := ReadSVGString(aText);
  FRenderer.Render(FDocument, FBackend);
  lStream := TMemoryStream.Create;
  try
    FBackend.SaveToStream(lStream);
    SetLength(FFile, lStream.Size);
    lStream.Position := 0;
    if lStream.Size > 0 then
      lStream.ReadBuffer(FFile[1], lStream.Size);
  finally
    lStream.Free;
  end;
  FFile := Flattened(FFile);
end;


procedure TTestSVGPDF.AssertHas(const aMessage, aText: String);

begin
  AssertTrue(aMessage + ' (' + aText + ')', Pos(RawByteString(aText), FFile) > 0);
end;


procedure TTestSVGPDF.AssertLacks(const aMessage, aText: String);

begin
  AssertTrue(aMessage + ' (' + aText + ')', Pos(RawByteString(aText), FFile) = 0);
end;


function TTestSVGPDF.CountOf(const aText: String): Integer;

var
  lAt: Integer;

begin
  Result := 0;
  lAt := Pos(RawByteString(aText), FFile);
  while lAt > 0 do
    begin
    Inc(Result);
    lAt := PosEx(RawByteString(aText), FFile, lAt + 1);
    end;
end;


procedure TTestSVGPDF.TestPageTakesTheSizeOfTheDocument;

begin
  Render(Doc('<rect width="10" height="10"/>'));
  AssertHas('the page is as large as the document', '/MediaBox [0 0 100 100]');
end;


procedure TTestSVGPDF.TestScaleMultipliesThePageSize;

begin
  FBackend := TSVGPDFBackend.Create;
  FBackend.Scale := 0.75;
  Render(Doc('<rect width="10" height="10"/>'));
  AssertHas('the page is in points of three quarters of a unit',
    '/MediaBox [0 0 75 75]');
end;


procedure TTestSVGPDF.TestFillWritesTheColourAndTheOperator;

begin
  Render(Doc('<rect x="10" y="20" width="30" height="40" fill="#ff0000"/>'));
  AssertHas('the fill colour is written', '1 0 0 rg');
  AssertHas('the shape is filled', 'h f');
  AssertHas('the corner of the rectangle is written', '10 20 m');
end;


procedure TTestSVGPDF.TestFillRuleWritesTheStarOperator;

begin
  Render(Doc('<path d="M 0 0 h 50 v 50 h -50 z M 10 10 h 30 v 30 h -30 z"'
    + ' fill-rule="evenodd" fill="#000080"/>'));
  AssertHas('the even-odd rule is written', 'f*');
end;


procedure TTestSVGPDF.TestStrokeWritesTheWidthAndTheOperator;

begin
  Render(Doc('<line x1="0" y1="0" x2="50" y2="50" stroke="#008000"'
    + ' stroke-width="4" stroke-linecap="round"/>'));
  AssertHas('the pen width is written', '4 w');
  AssertHas('the round cap is written', '1 J');
  AssertHas('the line is stroked', 'S');
end;


procedure TTestSVGPDF.TestDashesAreWrittenWithTheirOffset;

begin
  Render(Doc('<line x1="0" y1="0" x2="50" y2="0" stroke="#000000"'
    + ' stroke-dasharray="8 4" stroke-dashoffset="3"/>'));
  AssertHas('the dashes are written', '[8 4] 3 d');
end;


procedure TTestSVGPDF.TestTransformBecomesAMatrix;

begin
  Render(Doc('<g transform="translate(10,20)">'
    + '<rect width="10" height="10"/></g>'));
  // The matrix of the document turns the y axis over; the translation of
  // the group comes before it.
  AssertHas('the transform reaches the matrix', '1 0 0 -1 10 80 cm');
end;


procedure TTestSVGPDF.TestClipPathBecomesAClipOperator;

begin
  Render(Doc('<defs><clipPath id="c"><rect x="0" y="0" width="20"'
    + ' height="20"/></clipPath></defs>'
    + '<rect width="100" height="100" fill="#ff0000" clip-path="url(#c)"/>'));
  AssertTrue('the clip of the shape is written beside the one of the page',
    CountOf('W n') >= 2);
end;


procedure TTestSVGPDF.TestFillOpacityBecomesAGraphicsState;

begin
  Render(Doc('<rect width="50" height="50" fill="#ff0000"'
    + ' fill-opacity="0.5"/>'));
  AssertHas('the alpha of the fill is written', '/ca 0.5');
  AssertHas('the state is put into effect', '/GS0 gs');
end;


procedure TTestSVGPDF.TestGroupOpacityBecomesAFormAndAState;

begin
  Render(Doc('<g opacity="0.25"><rect width="50" height="50" fill="#ff0000"/>'
    + '<rect x="20" y="20" width="50" height="50" fill="#0000ff"/></g>'));
  AssertHas('the group becomes a transparency group', '/S /Transparency');
  AssertHas('the group is drawn as a form', '/Fm0 Do');
  AssertHas('the opacity of the group is written', '/ca 0.25');
end;


procedure TTestSVGPDF.TestLinearGradientBecomesAnAxialShading;

begin
  Render(Doc('<defs><linearGradient id="g"><stop offset="0"'
    + ' stop-color="#ff0000"/><stop offset="1" stop-color="#0000ff"/>'
    + '</linearGradient></defs>'
    + '<rect width="80" height="40" fill="url(#g)"/>'));
  AssertHas('the gradient becomes an axial shading', '/ShadingType 2');
  AssertHas('the shading is painted through a pattern', '/PatternType 2');
  AssertHas('the pattern becomes the fill colour', '/Pattern cs /P0 scn');
  AssertHas('the stops become a stitching function', '/FunctionType 2');
end;


procedure TTestSVGPDF.TestRadialGradientBecomesARadialShading;

begin
  Render(Doc('<defs><radialGradient id="g" cx="0.5" cy="0.5" r="0.5"'
    + ' fx="0.2" fy="0.3"><stop offset="0" stop-color="#ffffff"/>'
    + '<stop offset="1" stop-color="#000080"/></radialGradient></defs>'
    + '<circle cx="50" cy="50" r="40" fill="url(#g)"/>'));
  AssertHas('the gradient becomes a radial shading', '/ShadingType 3');
end;


procedure TTestSVGPDF.TestStopOpacityBecomesASoftMask;

begin
  Render(Doc('<defs><linearGradient id="g"><stop offset="0"'
    + ' stop-color="#008000" stop-opacity="1"/><stop offset="1"'
    + ' stop-color="#008000" stop-opacity="0"/></linearGradient></defs>'
    + '<rect width="80" height="40" fill="url(#g)"/>'));
  AssertHas('the opacity of the stops becomes a soft mask',
    '/S /Luminosity');
  AssertHas('the mask is painted in grey', '/DeviceGray');
end;


procedure TTestSVGPDF.TestMaskBecomesALuminositySoftMask;

begin
  Render(Doc('<defs><mask id="m"><rect width="100" height="100"'
    + ' fill="#808080"/></mask></defs>'
    + '<rect width="80" height="40" fill="#ff0000" mask="url(#m)"/>'));
  AssertHas('the mask becomes a soft mask', '/SMask');
  AssertHas('the mask reads the luminosity of its form', '/S /Luminosity');
end;


procedure TTestSVGPDF.TestImageBecomesAnXObject;

var
  lImages: TTestPDFImage;

begin
  lImages := TTestPDFImage.Create(4, 4);
  try
    FRenderer.Images := lImages;
    Render(Doc('<image x="10" y="10" width="40" height="40"'
      + ' xlink:href="red.png"/>'));
    AssertHas('the image becomes an image object', '/Subtype /Image');
    AssertHas('the image is drawn', '/I0 Do');
    AssertHas('the image is as wide as it was given', '/Width 4');
  finally
    FRenderer.Images := nil;
    lImages.Free;
  end;
end;


procedure TTestSVGPDF.TestTextBecomesOutlines;

begin
  Render(Doc('<text x="10" y="50" font-family="stub" font-size="20"'
    + ' fill="#000000">ab</text>'));
  // The stub font draws a square for every glyph, so the run becomes two
  // squares and no font of the document.
  AssertLacks('no font is embedded', '/Subtype /TrueType');
  AssertHas('the glyphs are filled as a path', 'h f');
  AssertHas('the first glyph starts at the pen', '10 50 m');
end;


procedure TTestSVGPDF.TestTwoFramesBecomeTwoPages;

begin
  FBackend := TSVGPDFBackend.Create;
  Render(Doc('<rect width="10" height="10" fill="#ff0000"/>'));
  Render(Doc('<rect width="10" height="10" fill="#0000ff"/>'));
  AssertHas('the document holds both pages', '/Count 2');
end;


procedure TTestSVGPDF.TestEveryPageIsValidPDF;

begin
  Render(Doc('<rect width="10" height="10" fill="#ff0000"/>'));
  AssertTrue('the file starts with the header',
    Copy(FFile, 1, 5) = '%PDF-');
  AssertHas('the file holds a catalogue', '/Type /Catalog');
  AssertHas('the file ends with the table', 'startxref');
end;


initialization
  RegisterTest('pdf', TTestSVGPDF);
end.
