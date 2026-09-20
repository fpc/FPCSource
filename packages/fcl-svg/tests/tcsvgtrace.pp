{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Operation-log golden tests for the fpsvg.trace backend.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgtrace;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$INTERFACES CORBA}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpcUnit.Test, FpcUnit.Registry,
     svggoldens, svgstubfont, fpsvg.types, fpsvg.backend,
     fpsvg.trace;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpcunit, testregistry, svggoldens, svgstubfont,
     fpsvg.types, fpsvg.backend, fpsvg.trace;
{$ENDIF FPC_DOTTEDUNITS}

type
  TStubPaintServer = class(TObject, ISVGPaintServer)
  private
    FKind: TSVGPaintServerKind;
    FID: TSVGString;
  public
    constructor Create(aKind: TSVGPaintServerKind; const aID: TSVGString);
    function GetPaintServerKind: TSVGPaintServerKind;
    function GetPaintServerID: TSVGString;
    function GetGradient(out aGradient: TSVGGradient): Boolean;
  end;

  TStubImageSource = class(TObject, ISVGImageSource)
  private
    FWidth, FHeight: Integer;
  public
    constructor Create(aWidth, aHeight: Integer);
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPixel(aX, aY: Integer): TSVGColor;
    function GetRow(aY, aX, aCount: Integer; aDest: PSVGColor): Boolean;
  end;

  TTestSVGTraceFormat = class(TTestCase)
  published
    procedure TestIntegerHasNoDecimals;
    procedure TestFractionKeepsSignificantDigits;
    procedure TestNegativeZeroNormalises;
    procedure TestIdentityMatrixIsNamed;
    procedure TestEightBitColourIsShort;
    procedure TestSixteenBitColourIsLong;
  end;

  TTestSVGTrace = class(TTestCase)
  private
    FBackend: TSVGTraceBackend;
    FPath: TSVGPath;
    procedure CheckGolden(const aName: String);
    procedure BuildRectPath(aLeft, aTop, aRight, aBottom: Double);
    procedure PopClipWithoutPush;
    procedure EndFrameWithOpenClip;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyFrame;
    procedure TestFillAndStroke;
    procedure TestPaintServerFill;
    procedure TestClipNesting;
    procedure TestLayerComposite;
    procedure TestLayerAsMask;
    procedure TestGlyphRun;
    procedure TestDrawImage;
    procedure TestPopClipWithoutPushRaises;
    procedure TestEndFrameWithOpenClipRaises;
    procedure TestBackendIsRegistered;
  end;

implementation

{ TStubPaintServer }

constructor TStubPaintServer.Create(aKind: TSVGPaintServerKind; const aID: TSVGString);

begin
  inherited Create;
  FKind := aKind;
  FID := aID;
end;


function TStubPaintServer.GetPaintServerKind: TSVGPaintServerKind;

begin
  Result := FKind;
end;


function TStubPaintServer.GetPaintServerID: TSVGString;

begin
  Result := FID;
end;


function TStubPaintServer.GetGradient(out aGradient: TSVGGradient): Boolean;

begin
  aGradient := TSVGGradient.CreateLinear(TSVGPoint.Create(0, 0),
    TSVGPoint.Create(1, 0));
  Result := FKind <> pkPattern;
end;


{ TStubImageSource }

constructor TStubImageSource.Create(aWidth, aHeight: Integer);

begin
  inherited Create;
  FWidth := aWidth;
  FHeight := aHeight;
end;


function TStubImageSource.GetWidth: Integer;

begin
  Result := FWidth;
end;


function TStubImageSource.GetHeight: Integer;

begin
  Result := FHeight;
end;


function TStubImageSource.GetPixel(aX, aY: Integer): TSVGColor;

begin
  Result := TSVGColor.FromBytes(aX and 255, aY and 255, 0, 255);
end;


function TStubImageSource.GetRow(aY, aX, aCount: Integer; aDest: PSVGColor): Boolean;

var
  I: Integer;

begin
  Result := True;
  for I := 0 to aCount - 1 do
    aDest[I] := GetPixel(aX + I, aY);
end;


{ TTestSVGTraceFormat }

procedure TTestSVGTraceFormat.TestIntegerHasNoDecimals;

begin
  AssertEquals('a whole number loses its decimal part', '10', SVGFormatFloat(10));
end;


procedure TTestSVGTraceFormat.TestFractionKeepsSignificantDigits;

begin
  AssertEquals('a half keeps one decimal', '0.5', SVGFormatFloat(0.5));
  AssertEquals('trailing zeros are trimmed', '-2.25', SVGFormatFloat(-2.25));
end;


procedure TTestSVGTraceFormat.TestNegativeZeroNormalises;

begin
  AssertEquals('negative zero prints as zero', '0', SVGFormatFloat(-0.0));
  AssertEquals('a value rounding to zero prints as zero', '0',
    SVGFormatFloat(-1e-12));
end;


procedure TTestSVGTraceFormat.TestIdentityMatrixIsNamed;

begin
  AssertEquals('the identity matrix is written as a word, not as numbers',
    'identity', TSVGMatrix.Identity.ToString);
  AssertEquals('a translation is spelled out', '[1 0 0 1 5 6]',
    TSVGMatrix.Translation(5, 6).ToString);
end;


procedure TTestSVGTraceFormat.TestEightBitColourIsShort;

begin
  AssertEquals('a colour from 8-bit components prints as 8-bit hex',
    '#ff8000ff', TSVGColor.FromBytes(255, 128, 0, 255).ToString);
end;


procedure TTestSVGTraceFormat.TestSixteenBitColourIsLong;

var
  lColor: TSVGColor;

begin
  lColor := TSVGColor.FromBytes(255, 0, 0, 255);
  lColor.Green := 1;
  AssertEquals('a channel outside the 8-bit grid prints as 16-bit hex',
    '#ffff00010000ffff', lColor.ToString);
end;


{ TTestSVGTrace }

procedure TTestSVGTrace.SetUp;

begin
  inherited SetUp;
  FBackend := TSVGTraceBackend.Create;
  FPath := TSVGPath.Create;
end;


procedure TTestSVGTrace.TearDown;

begin
  FreeAndNil(FPath);
  FreeAndNil(FBackend);
  inherited TearDown;
end;


procedure TTestSVGTrace.BuildRectPath(aLeft, aTop, aRight, aBottom: Double);

begin
  FPath.Clear;
  FPath.MoveTo(aLeft, aTop);
  FPath.LineTo(aRight, aTop);
  FPath.LineTo(aRight, aBottom);
  FPath.LineTo(aLeft, aBottom);
  FPath.Close;
end;


procedure TTestSVGTrace.CheckGolden(const aName: String);

begin
  AssertGolden(Self, aName, FBackend.Log);
end;


procedure TTestSVGTrace.PopClipWithoutPush;

begin
  FBackend.PopClip;
end;


procedure TTestSVGTrace.EndFrameWithOpenClip;

begin
  FBackend.EndFrame;
end;


procedure TTestSVGTrace.TestEmptyFrame;

begin
  FBackend.BeginFrame(320, 240);
  FBackend.EndFrame;
  CheckGolden('empty-frame');
end;


procedure TTestSVGTrace.TestFillAndStroke;

var
  lPen: TSVGPen;

begin
  lPen := TSVGPen.Default;
  lPen.Width := 2.5;
  lPen.Cap := lcRound;
  lPen.Join := ljBevel;
  SetLength(lPen.Dashes, 2);
  lPen.Dashes[0] := 4;
  lPen.Dashes[1] := 1.5;
  lPen.DashOffset := 0.25;
  FBackend.BeginFrame(100, 50);
  BuildRectPath(10, 10, 90, 40);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(255, 0, 0, 255)), frNonZero, 1);
  FBackend.StrokePath(FPath, TSVGMatrix.Translation(1, 2),
    TSVGPaint.CreateColor(TSVGColor.FromBytes(0, 0, 255, 128)), lPen, 0.5);
  FBackend.EndFrame;
  CheckGolden('fill-and-stroke');
end;


procedure TTestSVGTrace.TestPaintServerFill;

var
  lServer: TStubPaintServer;

begin
  lServer := TStubPaintServer.Create(pkLinearGradient, 'grad1');
  try
    FBackend.BeginFrame(64, 64);
    BuildRectPath(0, 0, 64, 64);
    FBackend.FillPath(FPath, TSVGMatrix.Identity, TSVGPaint.CreateServer(lServer),
      frEvenOdd, 1);
    FBackend.FillPath(FPath, TSVGMatrix.Identity, TSVGPaint.None, frNonZero, 1);
    FBackend.EndFrame;
    CheckGolden('paint-server');
  finally
    lServer.Free;
  end;
end;


procedure TTestSVGTrace.TestClipNesting;

begin
  FBackend.BeginFrame(100, 100);
  BuildRectPath(0, 0, 50, 50);
  FBackend.PushClip(FPath, TSVGMatrix.Identity, frNonZero);
  BuildRectPath(25, 25, 75, 75);
  FBackend.PushClip(FPath, TSVGMatrix.Scaling(2, 2), frEvenOdd);
  BuildRectPath(0, 0, 100, 100);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(0, 255, 0, 255)), frNonZero, 1);
  FBackend.PopClip;
  FBackend.PopClip;
  FBackend.EndFrame;
  CheckGolden('clip-nesting');
end;


procedure TTestSVGTrace.TestLayerComposite;

begin
  FBackend.BeginFrame(80, 80);
  FBackend.PushLayer(TSVGRect.CreateSize(0, 0, 80, 80), 0.75, True);
  BuildRectPath(10, 10, 70, 70);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(0, 0, 0, 255)), frNonZero, 1);
  FBackend.PopLayer;
  FBackend.EndFrame;
  CheckGolden('layer-composite');
end;


procedure TTestSVGTrace.TestLayerAsMask;

begin
  FBackend.BeginFrame(80, 80);
  FBackend.PushLayer(TSVGRect.Empty, 1, False);
  BuildRectPath(0, 0, 40, 80);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(255, 255, 255, 255)), frNonZero, 1);
  FBackend.PopLayerAsMask(mmLuminance);
  FBackend.EndFrame;
  CheckGolden('layer-as-mask');
end;


procedure TTestSVGTrace.TestGlyphRun;

var
  lFont: TSVGStubFont;
  lGlyphs: TSVGGlyphArray;

begin
  lFont := TSVGStubFont.Create('DejaVu Sans', 10);
  try
    SetLength(lGlyphs, 3);
    lGlyphs[0].GlyphID := 36; lGlyphs[0].X := 0; lGlyphs[0].Y := 0;
    lGlyphs[1].GlyphID := 37; lGlyphs[1].X := 7.5; lGlyphs[1].Y := 0;
    lGlyphs[2].GlyphID := 38; lGlyphs[2].X := 15; lGlyphs[2].Y := -1.25;
    FBackend.BeginFrame(200, 40);
    FBackend.DrawGlyphRun(lFont, lGlyphs, TSVGMatrix.Translation(10, 30),
      TSVGPaint.CreateColor(TSVGColor.FromBytes(0, 0, 0, 255)), 1);
    FBackend.EndFrame;
    CheckGolden('glyph-run');
  finally
    lFont.Free;
  end;
end;


procedure TTestSVGTrace.TestDrawImage;

var
  lImage: TStubImageSource;

begin
  lImage := TStubImageSource.Create(16, 8);
  try
    FBackend.BeginFrame(64, 64);
    FBackend.DrawImage(lImage, TSVGRect.CreateSize(4, 4, 32, 16),
      TSVGMatrix.Scaling(2, 2), 0.9);
    FBackend.DrawImage(nil, TSVGRect.Empty, TSVGMatrix.Identity, 1);
    FBackend.EndFrame;
    CheckGolden('draw-image');
  finally
    lImage.Free;
  end;
end;


procedure TTestSVGTrace.TestPopClipWithoutPushRaises;

begin
  FBackend.BeginFrame(10, 10);
  AssertException('a pop without a matching push is rejected',
    ESVGBackend, @PopClipWithoutPush);
end;


procedure TTestSVGTrace.TestEndFrameWithOpenClipRaises;

begin
  FBackend.BeginFrame(10, 10);
  BuildRectPath(0, 0, 5, 5);
  FBackend.PushClip(FPath, TSVGMatrix.Identity, frNonZero);
  AssertException('ending a frame with an open clip is rejected',
    ESVGBackend, @EndFrameWithOpenClip);
end;


procedure TTestSVGTrace.TestBackendIsRegistered;

var
  lBackend: TSVGRenderBackend;

begin
  AssertNotNull('the trace backend registers itself',
    SVGBackends.FindBackend('trace'));
  lBackend := SVGBackends.CreateBackend('trace');
  try
    AssertTrue('the registry builds a trace backend',
      lBackend is TSVGTraceBackend);
    AssertTrue('the trace backend claims clip support',
      bcClipPath in TSVGTraceBackend.Capabilities);
  finally
    lBackend.Free;
  end;
end;


initialization
  RegisterTest('trace', TTestSVGTraceFormat);
  RegisterTest('trace', TTestSVGTrace);
end.
