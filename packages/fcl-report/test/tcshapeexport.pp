{ Tests rendering of report shapes by the canvas helper and the PDF exporter. }
unit tcshapeexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpimage, fpcanvas, fpimgcanv,
  fpreport, fpreportcanvashelper, fppdf, fpreportpdfexport;

type

  { TTestCanvasShapeRender }

  TTestCanvasShapeRender = class(TTestCase)
  private
    FImage: TFPMemoryImage;
    FCanvas: TFPImageCanvas;
    FHelper: TFPReportCanvasHelper;
    FShape: TFPReportShape;
    procedure Render(aRadius: TFPReportUnits);
    function IsDrawn(aX, aY: Integer): Boolean;
    function IsDrawnNear(aX, aY: Integer): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundedRectEdges;
    procedure TestRoundedRectCorners;
    procedure TestRoundedRectNotFilled;
    procedure TestRoundedRectRadiusLimited;
    procedure TestRoundedRectZeroRadius;
  end;

  { TTestPDFShapeRender }

  TTestPDFShapeRender = class(TTestCase)
  private
    FDocument: TPDFDocument;
    FPage: TPDFPage;
    FExporter: TFPReportExportPDF;
    FShape: TFPReportShape;
    FMinX, FMinY, FMaxX, FMaxY, FStartX, FStartY: Double;
    FCurves: Integer;
    procedure Render(aRadius: TFPReportUnits);
    procedure AssertPt(const aMsg: String; aExpectedMM, aActualPt: Double);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRoundedRectIsDrawn;
    procedure TestRoundedRectBounds;
    procedure TestRoundedRectRadius;
    procedure TestRoundedRectRadiusLimited;
  end;

implementation

type
  TShapeAccess = class(TFPReportShape);
  TExporterAccess = class(TFPReportExportPDF);
  TPDFObjectAccess = class(TPDFObject);

const
  { 10 pixels per mm }
  TestDPI = 254;
  ShapeLeft = 1;
  ShapeTop = 1;
  ShapeWidth = 40;
  ShapeHeight = 25;
  PDFOriginX = 10;
  PDFOriginY = 40;
  { Points per mm }
  PtPerMM = 72 / 25.4;


// Create a blue rounded rectangle shape of ShapeWidth x ShapeHeight mm with a runtime layout
function CreateShape(aRadius: TFPReportUnits): TFPReportShape;

begin
  Result:=TFPReportShape.Create(nil);
  Result.Layout.Left:=ShapeLeft;
  Result.Layout.Top:=ShapeTop;
  Result.Layout.Width:=ShapeWidth;
  Result.Layout.Height:=ShapeHeight;
  Result.ShapeType:=stRoundedRect;
  Result.Color:=clBlue;
  Result.CornerRadius:=aRadius;
  TShapeAccess(Result).CreateRTLayout;
end;


{ TTestCanvasShapeRender }

procedure TTestCanvasShapeRender.SetUp;

begin
  FImage:=TFPMemoryImage.Create(ShapeWidth*10+40, ShapeHeight*10+40);
  FCanvas:=TFPImageCanvas.Create(FImage);
  FCanvas.Brush.Style:=bsSolid;
  FCanvas.Brush.FPColor:=colWhite;
  FCanvas.FillRect(0, 0, FImage.Width-1, FImage.Height-1);
  FHelper:=TFPReportCanvasHelper.Create(FCanvas, TestDPI);
  FShape:=nil;
end;


procedure TTestCanvasShapeRender.TearDown;

begin
  FreeAndNil(FShape);
  FreeAndNil(FHelper);
  FreeAndNil(FCanvas);
  FreeAndNil(FImage);
end;


procedure TTestCanvasShapeRender.Render(aRadius: TFPReportUnits);

var
  lPt: TFPReportPoint;

begin
  FShape:=CreateShape(aRadius);
  lPt.Left:=ShapeLeft;
  lPt.Top:=ShapeTop;
  FCanvas.Brush.Style:=bsSolid;
  FHelper.RenderShape(lPt, FShape);
end;


function TTestCanvasShapeRender.IsDrawn(aX, aY: Integer): Boolean;

begin
  Result:=FImage.Colors[aX, aY]<>colWhite;
end;


function TTestCanvasShapeRender.IsDrawnNear(aX, aY: Integer): Boolean;

var
  lX, lY: Integer;

begin
  Result:=False;
  for lX:=aX-1 to aX+1 do
    for lY:=aY-1 to aY+1 do
      if IsDrawn(lX, lY) then
        Exit(True);
end;


procedure TTestCanvasShapeRender.TestRoundedRectEdges;

begin
  Render(5);
  AssertTrue('top edge is drawn', IsDrawn(210, 10));
  AssertTrue('bottom edge is drawn', IsDrawn(210, 259));
  AssertTrue('left edge is drawn', IsDrawn(10, 135));
  AssertTrue('right edge is drawn', IsDrawn(409, 135));
end;


procedure TTestCanvasShapeRender.TestRoundedRectCorners;

begin
  Render(5);
  AssertFalse('top-left corner point is cut off', IsDrawn(10, 10));
  AssertFalse('bottom-right corner point is cut off', IsDrawn(409, 259));
  AssertTrue('top-left arc passes 45 degrees', IsDrawnNear(25, 25));
  AssertTrue('bottom-right arc passes 45 degrees', IsDrawnNear(394, 244));
  AssertTrue('top edge is straight from the end of the arc', IsDrawn(61, 10));
end;


procedure TTestCanvasShapeRender.TestRoundedRectNotFilled;

begin
  Render(5);
  AssertFalse('inside is not filled', IsDrawn(210, 135));
end;


procedure TTestCanvasShapeRender.TestRoundedRectRadiusLimited;

begin
  Render(100);
  AssertTrue('top edge midpoint is drawn', IsDrawn(210, 10));
  AssertTrue('left side is a half circle touching the left border', IsDrawn(10, 135));
  AssertFalse('left side is rounded above its midpoint', IsDrawn(10, 60));
end;


procedure TTestCanvasShapeRender.TestRoundedRectZeroRadius;

begin
  Render(0);
  AssertTrue('top-left corner point is drawn', IsDrawn(10, 10));
  AssertTrue('bottom-right corner point is drawn', IsDrawn(409, 259));
end;


{ TTestPDFShapeRender }

procedure TTestPDFShapeRender.SetUp;

begin
  FDocument:=TPDFDocument.Create(nil);
  FDocument.Options:=[];
  FDocument.StartDocument;
  FDocument.Sections.AddSection;
  FPage:=FDocument.Pages.AddPage;
  FPage.UnitOfMeasure:=uomMillimeters;
  FExporter:=TFPReportExportPDF.Create(nil);
  FShape:=nil;
end;


procedure TTestPDFShapeRender.TearDown;

begin
  FreeAndNil(FShape);
  FreeAndNil(FExporter);
  FreeAndNil(FDocument);
end;


procedure TTestPDFShapeRender.Render(aRadius: TFPReportUnits);

var
  lOrigin: TPDFCoord;
  lStream: TStringStream;
  lLines, lTokens: TStringList;
  lObj, lLine, lToken: Integer;
  lValue: Double;
  lFirst: Boolean;
  lFS: TFormatSettings;

begin
  lFS:=DefaultFormatSettings;
  lFS.DecimalSeparator:='.';
  FShape:=CreateShape(aRadius);
  lOrigin.X:=PDFOriginX;
  lOrigin.Y:=PDFOriginY;
  TExporterAccess(FExporter).RenderShape(FPage, lOrigin, FShape);
  FCurves:=0;
  FMinX:=MaxInt;
  FMinY:=MaxInt;
  FMaxX:=-MaxInt;
  FMaxY:=-MaxInt;
  lFirst:=True;
  lStream:=TStringStream.Create('');
  lLines:=TStringList.Create;
  lTokens:=TStringList.Create;
  try
    for lObj:=0 to FPage.ObjectCount-1 do
      if FPage.Objects[lObj] is TPDFRoundedRectangle then
        TPDFObjectAccess(FPage.Objects[lObj]).Write(lStream);
    lLines.Text:=lStream.DataString;
    lTokens.Delimiter:=' ';
    lTokens.StrictDelimiter:=True;
    for lLine:=0 to lLines.Count-1 do
      begin
      lTokens.DelimitedText:=Trim(lLines[lLine]);
      if lTokens.Count<3 then
        Continue;
      if lTokens[lTokens.Count-1]='c' then
        Inc(FCurves)
      else if (lTokens[lTokens.Count-1]<>'m') and (lTokens[lTokens.Count-1]<>'l') then
        Continue;
      for lToken:=0 to lTokens.Count-2 do
        begin
        lValue:=StrToFloat(lTokens[lToken], lFS);
        if Odd(lToken) then
          begin
          if lValue<FMinY then FMinY:=lValue;
          if lValue>FMaxY then FMaxY:=lValue;
          end
        else
          begin
          if lValue<FMinX then FMinX:=lValue;
          if lValue>FMaxX then FMaxX:=lValue;
          end;
        end;
      if lFirst then
        begin
        FStartX:=StrToFloat(lTokens[0], lFS);
        FStartY:=StrToFloat(lTokens[1], lFS);
        lFirst:=False;
        end;
      end;
  finally
    lTokens.Free;
    lLines.Free;
    lStream.Free;
  end;
end;


procedure TTestPDFShapeRender.AssertPt(const aMsg: String; aExpectedMM, aActualPt: Double);

begin
  AssertEquals(aMsg, aExpectedMM*PtPerMM, aActualPt, 0.05);
end;


procedure TTestPDFShapeRender.TestRoundedRectIsDrawn;

begin
  Render(5);
  AssertEquals('path has 4 corner curves', 4, FCurves);
end;


procedure TTestPDFShapeRender.TestRoundedRectBounds;

begin
  Render(5);
  AssertPt('left side', PDFOriginX, FMinX);
  AssertPt('right side', PDFOriginX+ShapeWidth, FMaxX);
  AssertPt('lower side', PDFOriginY, FMinY);
  AssertPt('upper side', PDFOriginY+ShapeHeight, FMaxY);
end;


procedure TTestPDFShapeRender.TestRoundedRectRadius;

begin
  Render(5);
  AssertPt('path starts after the corner radius', PDFOriginX+5, FStartX);
  AssertPt('path starts on the lower side', PDFOriginY, FStartY);
end;


procedure TTestPDFShapeRender.TestRoundedRectRadiusLimited;

begin
  Render(100);
  AssertPt('radius is limited to half the height', PDFOriginX+ShapeHeight/2, FStartX);
  AssertPt('right side with limited radius', PDFOriginX+ShapeWidth, FMaxX);
end;


initialization
  RegisterTests([TTestCanvasShapeRender, TTestPDFShapeRender]);
end.
