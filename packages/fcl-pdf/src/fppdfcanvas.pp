{
    TPDFCanvas: a TFPCustomCanvas that draws on a page of a TPDFDocument.
    This file is part of the Free Pascal run time library.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$IFNDEF FPC_DOTTEDUNITS}
unit fppdfcanvas;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Types, System.Math, FpImage, FpImage.Canvas, FpImage.ImageCanvas,
  FpPdf.Pdf, FpPdf.Ttf, FpPdf.Ttf.Parser;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, Types, Math, fpimage, fpcanvas, fpimgcanv, fppdf, fpttf, fpparsettf;
{$ENDIF FPC_DOTTEDUNITS}

type
  EPDFCanvas = class(Exception);

  // Appends a path to the current path of the page.
  TPDFCanvasPath = procedure is nested;

  { A font of the canvas: the font of the document it is written in and its metrics. }
  TPDFCanvasFont = class
  public
    FontIndex: Integer;
    CoreIndex: Integer;
    TrueType: TFPFontCacheItem;
    destructor Destroy; override;
  end;

  { Draws on Page, one canvas pixel per PDF point with y counted from the top of the page.
    Destination-dependent operations need Shadow, a raster copy of the page. }
  TPDFCanvas = class(TFPCustomCanvas)
  private
    FDocument: TPDFDocument;
    FPage: TPDFPage;
    FWidth, FHeight: Integer;
    FFonts: TStringList;
    FStates: array of record
      FillAlpha, StrokeAlpha: TPDFFloat;
      Blend: TPDFBlendMode;
      Index: Integer;
    end;
    FShadow: Boolean;
    FShadowImage, FSnapshot: TFPMemoryImage;
    FShadowCanvas: TFPImageCanvas;
    FNopPenStyle: TFPPenStyle;
    FNopPen: Boolean;
    procedure SetPage(aValue: TPDFPage);
    procedure SetShadow(aValue: Boolean);
    // Returns the y of the page for the canvas y aY, for the methods of TPDFPage.
    function PageY(aY: Double): Double;
    // Returns the index of the graphics state with these alphas and blend mode, adding it when new.
    function StateIndex(aFillAlpha, aStrokeAlpha: TPDFFloat; aBlend: TPDFBlendMode): Integer;
    // Returns the canvas font of Font, adding it to the document when new.
    function CanvasFont: TPDFCanvasFont;
    // Returns Font.Size, or 10 when it is not set.
    function FontSize: Integer;
    // Returns the width of aText in points for the canvas font aFont.
    function TextPoints(aFont: TPDFCanvasFont; const aText: String): Double;
    // Starts the graphics state of one shape: q, the clip rectangle and the alpha of aFill and aStroke.
    procedure BeginShapeState(const aFill, aStroke: TFPColor; aUseFill, aUseStroke: Boolean);
    // Sets the stroke colour, width, caps, joins and dashes of the pen.
    procedure SetPenState;
    // Appends the rectangle from (aLeft, aTop) to (aRight, aBottom) to the path.
    procedure RectPath(aLeft, aTop, aRight, aBottom: Double);
    // Appends the arc of the ellipse at (acx, acy) with radii arx, ary from parametric angle aStart over aSweep radians.
    procedure ArcPath(acx, acy, arx, ary, aStart, aSweep: Double; aMove: Boolean);
    // Appends the arc from the ray at aStart16 over aLength16 in 1/16 degree of the ellipse at (acx, acy).
    procedure RayArcPath(acx, acy, arx, ary: Double; aStart16, aLength16: Integer; aMove: Boolean);
    // Fills the path aPath with the brush; aBounds are the device pixels it covers, aOrigin where its hatch or image starts.
    procedure FillShape(aPath: TPDFCanvasPath; const aBounds: TRect; const aOrigin: TPoint; aEvenOdd, aEllipse: Boolean);
    // Strokes the path aPath with the pen.
    procedure StrokeShape(aPath: TPDFCanvasPath);
    // Adds aImage to the document; with aAlpha its alpha becomes a soft mask.
    function AddImage(aImage: TFPCustomImage; aAlpha: Boolean): Integer;
    // Draws the image aNumber over the aWidth x aHeight device pixels from (aX, aY).
    procedure PlaceImage(aNumber, aX, aY, aWidth, aHeight: Integer);
    // Creates or frees the shadow raster to follow Shadow and the canvas size.
    procedure UpdateShadow;
    // Copies the drawing state to the shadow canvas.
    procedure SyncShadow;
    // Returns whether the drawing of the pen, the brush or both depends on the pixels under it.
    function StartShape(aPen, aFill: Boolean): Boolean;
    // Ends the shape started by StartShape, writing the patch when aPatch.
    procedure EndShape(aPatch: Boolean);
    // Copies the shadow raster to FSnapshot.
    procedure TakeSnapshot;
    // Writes the shadow pixels that differ from FSnapshot as an image with a soft mask.
    procedure WritePatch;
    // Fills the region from (x, y) found in the shadow raster, up to aBorderColor when aBorder.
    procedure FloodRegion(x, y: Integer; aBorder: Boolean; const aBorderColor: TFPColor);
    procedure PDFLineTo(X1, Y1: Integer);
    procedure PDFLine(X1, Y1, X2, Y2: Integer);
    procedure PDFPolyline(const Points: array of TPoint; aClosed: Boolean);
    procedure PDFPolygonFill(const Points: array of TPoint);
    procedure PDFPolyBezier(Points: PPoint; NumPts: Integer; Filled, Continuous: Boolean);
    procedure PDFRectangle(const Bounds: TRect; DoFill: Boolean);
    procedure PDFEllipse(const Bounds: TRect; DoFill: Boolean);
    procedure PDFPie(const Bounds: TRect; aStart16, aLength16: Integer; aChord: Boolean);
    procedure PDFRoundRect(const Bounds: TRect; RX, RY: Integer);
    procedure PDFArc(const Bounds: TRect; aStart16, aLength16: Integer);
    procedure PDFImage(x, y, w, h: Integer; aImage: TFPCustomImage; aInterpolate: Boolean = False);
  protected
    procedure SetWidth(AValue: Integer); override;
    function GetWidth: Integer; override;
    procedure SetHeight(AValue: Integer); override;
    function GetHeight: Integer; override;
    // Fills the device pixel (x, y) with Value.
    procedure SetColor(x, y: Integer; const Value: TFPColor); override;
    // Returns the shadow raster pixel at (x, y); raises EPDFCanvas without Shadow.
    function GetColor(x, y: Integer): TFPColor; override;
    function DoCreateDefaultFont: TFPCustomFont; override;
    function DoCreateDefaultPen: TFPCustomPen; override;
    function DoCreateDefaultBrush: TFPCustomBrush; override;
    procedure DoTextOut(x, y: Integer; Text: AnsiString); override;
    procedure DoGetTextSize(text: AnsiString; var w, h: Integer); override;
    function DoGetTextHeight(text: AnsiString): Integer; override;
    function DoGetTextWidth(text: AnsiString): Integer; override;
    function DoGetTextMetrics(out aMetrics: TFPTextMetric): Boolean; override;
    function GetNativeTextOrigin: TFPTextOrigin; override;
    procedure DoMoveTo(x, y: Integer); override;
    procedure DoLineTo(x, y: Integer); override;
    procedure DoLine(x1, y1, x2, y2: Integer); override;
    procedure DoRectangle(const Bounds: TRect); override;
    procedure DoRectangleFill(const Bounds: TRect); override;
    procedure DoEllipse(const Bounds: TRect); override;
    procedure DoEllipseFill(const Bounds: TRect); override;
    procedure DoPolygon(const Points: array of TPoint); override;
    procedure DoPolygonFill(const Points: array of TPoint); override;
    procedure DoPolyline(const Points: array of TPoint); override;
    procedure DoPolyBezier(Points: PPoint; NumPts: Integer; Filled: Boolean = False;
                           Continuous: Boolean = False); override;
    procedure DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer); override;
    procedure DoChord(const Bounds: TRect; aStart16, aLength16: Integer); override;
    procedure DoRoundRect(const Bounds: TRect; RX, RY: Integer); override;
    procedure DoFloodFill(x, y: Integer); override;
    procedure DoFloodFillStyle(x, y: Integer; const FillColor: TFPColor; FillStyle: TFPFloodFillStyle); override;
    procedure DoCopyRect(x, y: Integer; canvas: TFPCustomCanvas; const SourceRect: TRect); override;
    procedure DoDraw(x, y: Integer; const image: TFPCustomImage); override;
    procedure DoStretchDraw(x, y, w, h: Integer; source: TFPCustomImage); override;
  public
    // Creates a canvas on aPage of aDocument; the page measures in points from then on.
    constructor Create(aDocument: TPDFDocument; aPage: TPDFPage);
    destructor Destroy; override;
    // Strokes the arc of the ellipse in the bounds, angles in 1/16 degree counter-clockwise from 3 o'clock.
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); overload; override;
    // Fills ARect with an axial shading from AStartColor to AEndColor.
    procedure GradientFill(const ARect: TRect; AStartColor, AEndColor: TFPColor;
                           ADirection: TFPGradientDirection); override;
    // Clears the shadow raster to a white page.
    procedure ClearShadow;
    // The document the canvas writes in.
    property Document: TPDFDocument read FDocument;
    // The page the canvas draws on; setting it takes the size of the page and clears the shadow.
    property Page: TPDFPage read FPage write SetPage;
    // Whether a raster copy of the page is kept for pixel reads, flood fills and destination-dependent modes.
    property Shadow: Boolean read FShadow write SetShadow;
  end;

implementation

resourcestring
  SErrCannotReadPixels = 'The PDF canvas cannot read pixels without Shadow.';
  SErrNeedsShadow = 'The PDF canvas needs Shadow for flood fills, pen modes and drawing modes that read the page.';
  SErrNoPage = 'The PDF canvas has no page.';

type
  TCoreFont = record
    Name: String;
    Ascender, Descender, UnderlinePosition, UnderlineThickness, XHeight: SmallInt;
  end;

const
  // Metrics of the Latin standard fonts in 1/1000 em, from the Adobe AFM files.
  CoreFonts: array[0..11] of TCoreFont = (
    (Name: 'Helvetica'; Ascender: 718; Descender: -207; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 523),
    (Name: 'Helvetica-Bold'; Ascender: 718; Descender: -207; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 532),
    (Name: 'Helvetica-Oblique'; Ascender: 718; Descender: -207; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 523),
    (Name: 'Helvetica-BoldOblique'; Ascender: 718; Descender: -207; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 532),
    (Name: 'Times-Roman'; Ascender: 683; Descender: -217; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 450),
    (Name: 'Times-Bold'; Ascender: 683; Descender: -217; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 461),
    (Name: 'Times-Italic'; Ascender: 683; Descender: -217; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 441),
    (Name: 'Times-BoldItalic'; Ascender: 683; Descender: -217; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 462),
    (Name: 'Courier'; Ascender: 629; Descender: -157; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 426),
    (Name: 'Courier-Bold'; Ascender: 629; Descender: -157; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 439),
    (Name: 'Courier-Oblique'; Ascender: 629; Descender: -157; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 426),
    (Name: 'Courier-BoldOblique'; Ascender: 629; Descender: -157; UnderlinePosition: -100; UnderlineThickness: 50; XHeight: 439));

  PenPatterns: array[psDash..psDashDotDot] of LongWord = ($EEEEEEEE, $AAAAAAAA, $E4E4E4E4, $EAEAEAEA);

type
  TCP1252String = type AnsiString(1252);

var
  // Real numbers in PDF always use a '.' as decimal separator.
  NumberFormat: TFormatSettings;

// Returns aColor as an opaque TARGBColor.
function ARGB(const aColor: TFPColor): TARGBColor;

begin
  Result := $FF000000 or (LongWord(aColor.Red shr 8) shl 16) or (LongWord(aColor.Green shr 8) shl 8)
            or LongWord(aColor.Blue shr 8);
end;


// Returns aValue formatted as a PDF number.
function Num(aValue: Double): String;

begin
  if Abs(aValue) < 0.0005 then
    aValue := 0;
  Result := FormatFloat('0.###', aValue, NumberFormat);
end;


// Returns whether a pixel of aImage is not fully opaque.
function HasTransparency(aImage: TFPCustomImage): Boolean;

var
  x, y: Integer;
begin
  Result := True;
  for y := 0 to aImage.Height - 1 do
    for x := 0 to aImage.Width - 1 do
      if aImage.Colors[x, y].Alpha <> alphaOpaque then
        exit;
  Result := False;
end;


// Sets aDash and aPhase to the dash array of the 32-bit pen pattern aPattern, set bits drawn from the most significant.
procedure PatternDash(aPattern: LongWord; out aDash: TDashArray; out aPhase: TPDFFloat);

  function Bit(i: Integer): Boolean;
  begin
    Result := ((aPattern shr (31 - (i mod 32))) and 1) <> 0;
  end;

var
  i, lStart, lRun, lPeriod: Integer;
  lOn: Boolean;
begin
  aDash := nil;
  aPhase := 0;
  if aPattern = $FFFFFFFF then
    exit;
  if aPattern = 0 then
    begin
    aDash := [0, 1];
    exit;
    end;
  lPeriod := 1;
  while (lPeriod < 32) and (RolDWord(aPattern, lPeriod) <> aPattern) do
    lPeriod := lPeriod * 2;
  lStart := 0;
  while not (Bit(lStart) and not Bit(lStart + 31)) do
    Inc(lStart);
  lOn := True;
  lRun := 0;
  for i := lStart to lStart + lPeriod - 1 do
    begin
    if Bit(i) <> lOn then
      begin
      SetLength(aDash, Length(aDash) + 1);
      aDash[High(aDash)] := lRun;
      lOn := not lOn;
      lRun := 0;
      end;
    Inc(lRun);
    end;
  SetLength(aDash, Length(aDash) + 1);
  aDash[High(aDash)] := lRun;
  aPhase := (lPeriod - lStart mod lPeriod) mod lPeriod;
end;


{ TPDFCanvasFont }

destructor TPDFCanvasFont.Destroy;

begin
  TrueType.Free;
  inherited Destroy;
end;


{ TPDFCanvas }

constructor TPDFCanvas.Create(aDocument: TPDFDocument; aPage: TPDFPage);

begin
  inherited Create;
  FDocument := aDocument;
  FFonts := TStringList.Create;
  FFonts.OwnsObjects := True;
  FFonts.Sorted := True;
  Pen.Mode := pmCopy;
  Page := aPage;
end;


destructor TPDFCanvas.Destroy;

begin
  FShadowCanvas.Free;
  FShadowImage.Free;
  FSnapshot.Free;
  FFonts.Free;
  inherited Destroy;
end;


procedure TPDFCanvas.SetPage(aValue: TPDFPage);

begin
  FPage := aValue;
  if Assigned(FPage) then
    begin
    FPage.UnitOfMeasure := uomPixels;
    FWidth := Round(FPage.Paper.W);
    FHeight := Round(FPage.Paper.H);
    end;
  UpdateShadow;
  ClearShadow;
end;


procedure TPDFCanvas.SetWidth(AValue: Integer);

begin
  FWidth := AValue;
  UpdateShadow;
end;


function TPDFCanvas.GetWidth: Integer;

begin
  Result := FWidth;
end;


procedure TPDFCanvas.SetHeight(AValue: Integer);

begin
  FHeight := AValue;
  UpdateShadow;
end;


function TPDFCanvas.GetHeight: Integer;

begin
  Result := FHeight;
end;


function TPDFCanvas.PageY(aY: Double): Double;

begin
  if poPageOriginAtTop in FDocument.Options then
    Result := aY
  else
    Result := FHeight - aY;
end;


function TPDFCanvas.DoCreateDefaultFont: TFPCustomFont;

begin
  Result := TFPEmptyFont.Create;
  Result.Size := 10;
  Result.FPColor := colBlack;
end;


function TPDFCanvas.DoCreateDefaultPen: TFPCustomPen;

begin
  Result := TFPEmptyPen.Create;
  Result.FPColor := colBlack;
  Result.Width := 1;
  Result.Mode := pmCopy;
end;


function TPDFCanvas.DoCreateDefaultBrush: TFPCustomBrush;

begin
  Result := TFPEmptyBrush.Create;
  Result.Style := bsSolid;
  Result.FPColor := colWhite;
end;


function TPDFCanvas.StateIndex(aFillAlpha, aStrokeAlpha: TPDFFloat; aBlend: TPDFBlendMode): Integer;

var
  i: Integer;
  lState: TPDFGraphicsStateItem;
begin
  for i := 0 to High(FStates) do
    if SameValue(FStates[i].FillAlpha, aFillAlpha) and SameValue(FStates[i].StrokeAlpha, aStrokeAlpha)
       and (FStates[i].Blend = aBlend) then
      exit(FStates[i].Index);
  lState := FDocument.GraphicsStates.AddState;
  lState.FillAlpha := aFillAlpha;
  lState.StrokeAlpha := aStrokeAlpha;
  lState.BlendMode := aBlend;
  Result := lState.Index;
  SetLength(FStates, Length(FStates) + 1);
  FStates[High(FStates)].FillAlpha := aFillAlpha;
  FStates[High(FStates)].StrokeAlpha := aStrokeAlpha;
  FStates[High(FStates)].Blend := aBlend;
  FStates[High(FStates)].Index := Result;
end;


{ Alpha applies under dmAlphaBlend; pmNot inverts with the difference blend of white. }
procedure TPDFCanvas.BeginShapeState(const aFill, aStroke: TFPColor; aUseFill, aUseStroke: Boolean);

var
  R: TRect;
  lFillAlpha, lStrokeAlpha: TPDFFloat;
  lBlend: TPDFBlendMode;
begin
  if not Assigned(FPage) then
    raise EPDFCanvas.Create(SErrNoPage);
  FPage.PushGraphicsStack;
  if Clipping then
    begin
    R := DeviceClipRect;
    RectPath(R.Left - 0.5, R.Top - 0.5, R.Right + 0.5, R.Bottom + 0.5);
    FPage.ClipPath;
    end;
  lFillAlpha := 1;
  lStrokeAlpha := 1;
  lBlend := pbmNormal;
  if DrawingMode = dmAlphaBlend then
    begin
    if aUseFill then
      lFillAlpha := aFill.Alpha / $FFFF;
    if aUseStroke then
      lStrokeAlpha := aStroke.Alpha / $FFFF;
    end;
  if aUseStroke and (Pen.Mode = pmNot) then
    lBlend := pbmDifference;
  if (lFillAlpha < 1) or (lStrokeAlpha < 1) or (lBlend <> pbmNormal) then
    FPage.SetGraphicsState(StateIndex(lFillAlpha, lStrokeAlpha, lBlend));
end;


procedure TPDFCanvas.SetPenState;

const
  Caps: array[TFPPenEndCap] of TPDFLineCapStyle = (plcsRoundCap, plcsProjectingSquareCap, plcsButtCap);
  Joins: array[TFPPenJoinStyle] of TPDFLineJoinStyle = (pljsRoundJoin, pljsBevelJoin, pljsMiterJoin);
var
  lDash: TDashArray;
  lPhase: TPDFFloat;
  lColor: TFPColor;
begin
  if Pen.Mode = pmNot then
    lColor := colWhite
  else
    lColor := PenModeColor(Pen.Mode, Pen.FPColor, colBlack);
  FPage.SetColor(ARGB(lColor), True);
  FPage.SetLineWidth(Max(0, Pen.Width));
  FPage.SetLineJoinStyle(Joins[Pen.JoinStyle]);
  case Pen.Style of
    psDash, psDot, psDashDot, psDashDotDot:
      PatternDash(PenPatterns[Pen.Style], lDash, lPhase);
    psPattern:
      PatternDash(Pen.Pattern, lDash, lPhase);
  else
    lDash := nil;
    lPhase := 0;
  end;
  if lDash <> nil then
    begin
    FPage.SetLineCapStyle(plcsButtCap);
    FPage.SetDashPattern(lDash, lPhase);
    end
  else
    FPage.SetLineCapStyle(Caps[Pen.EndCap]);
end;


procedure TPDFCanvas.RectPath(aLeft, aTop, aRight, aBottom: Double);

begin
  FPage.MoveTo(aLeft, PageY(aTop));
  FPage.LineTo(aRight, PageY(aTop));
  FPage.LineTo(aRight, PageY(aBottom));
  FPage.LineTo(aLeft, PageY(aBottom));
  FPage.ClosePath;
end;


{ Each Bezier segment spans at most a quarter turn; on the canvas the y of the ellipse runs down. }
procedure TPDFCanvas.ArcPath(acx, acy, arx, ary, aStart, aSweep: Double; aMove: Boolean);

var
  i, n: Integer;
  a0, a1, k, x0, y0, x1, y1: Double;
begin
  n := Max(1, Ceil(Abs(aSweep) / (Pi / 2) - 1e-9));
  a0 := aStart;
  x0 := acx + arx * Cos(a0);
  y0 := acy - ary * Sin(a0);
  if aMove then
    FPage.MoveTo(x0, PageY(y0))
  else
    FPage.LineTo(x0, PageY(y0));
  for i := 1 to n do
    begin
    a1 := aStart + aSweep * i / n;
    k := 4 / 3 * Tan((a1 - a0) / 4);
    x1 := acx + arx * Cos(a1);
    y1 := acy - ary * Sin(a1);
    FPage.CubicCurveTo(x0 - k * arx * Sin(a0), PageY(y0 - k * ary * Cos(a0)),
                       x1 + k * arx * Sin(a1), PageY(y1 + k * ary * Cos(a1)),
                       x1, PageY(y1), 0, False);
    a0 := a1;
    x0 := x1;
    y0 := y1;
    end;
end;


{ The rays are at geometric angles; the ellipse takes the matching parametric angles. }
procedure TPDFCanvas.RayArcPath(acx, acy, arx, ary: Double; aStart16, aLength16: Integer; aMove: Boolean);

  function Parametric(aDegrees: Double): Double;
  begin
    Result := ArcTan2(arx * Sin(DegToRad(aDegrees)), ary * Cos(DegToRad(aDegrees)));
  end;

var
  t0, dt: Double;
begin
  aLength16 := EnsureRange(aLength16, -360*16, 360*16);
  t0 := Parametric(aStart16 / 16);
  if (aLength16 = 0) or (Abs(aLength16) = 360*16) then
    dt := DegToRad(aLength16 / 16)
  else
    begin
    dt := Parametric((aStart16 + aLength16) / 16) - t0;
    if aLength16 > 0 then
      while dt <= 0 do
        dt := dt + 2 * Pi
    else
      while dt >= 0 do
        dt := dt - 2 * Pi;
    end;
  ArcPath(acx, acy, arx, ary, t0, dt, aMove);
end;


{ Hatch lines are placed like on the pixel canvases: every HashWidth from the origin, diagonals
  where x - y or x + y is a multiple; the tiles of pattern and image brushes start at the origin. }
procedure TPDFCanvas.FillShape(aPath: TPDFCanvasPath; const aBounds: TRect; const aOrigin: TPoint; aEvenOdd, aEllipse: Boolean);

var
  lOrigin: TPoint;
  lShape: Boolean;
  lWidth, k, lNumber, x, y, lTileW, lTileH: Integer;
  lTile: TFPMemoryImage;
  lBit: Boolean;
  L, T, R, B: Double;

  procedure Clip;
  begin
    aPath();
    if aEvenOdd then
      FPage.ClipPathEvenOdd
    else
      FPage.ClipPath;
  end;

  procedure Segment(x1, y1, x2, y2: Double);
  begin
    FPage.MoveTo(x1, PageY(y1));
    FPage.LineTo(x2, PageY(y2));
  end;

begin
  if Brush.Style = bsClear then
    exit;
  L := aBounds.Left - 1;
  T := aBounds.Top - 1;
  R := aBounds.Right + 1;
  B := aBounds.Bottom + 1;
  BeginShapeState(Brush.FPColor, Brush.FPColor, True, True);
  try
    case Brush.Style of
      bsSolid:
        begin
        FPage.SetColor(ARGB(Brush.FPColor), False);
        aPath();
        if aEvenOdd then
          FPage.FillEvenOddPath
        else
          FPage.FillPath;
        end;
      bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross:
        begin
        case HatchOrigin of
          hoShape: lShape := True;
          hoCanvas: lShape := False;
        else
          lShape := not aEllipse;
        end;
        if lShape then
          lOrigin := aOrigin
        else
          lOrigin := Point(0, 0);
        lWidth := Max(1, HashWidth);
        Clip;
        FPage.SetColor(ARGB(Brush.FPColor), True);
        FPage.SetLineWidth(1);
        FPage.SetLineCapStyle(plcsButtCap);
        if Brush.Style in [bsHorizontal, bsCross] then
          for k := Ceil((T - lOrigin.Y) / lWidth) to Floor((B - lOrigin.Y) / lWidth) do
            Segment(L, lOrigin.Y + k * lWidth, R, lOrigin.Y + k * lWidth);
        if Brush.Style in [bsVertical, bsCross] then
          for k := Ceil((L - lOrigin.X) / lWidth) to Floor((R - lOrigin.X) / lWidth) do
            Segment(lOrigin.X + k * lWidth, T, lOrigin.X + k * lWidth, B);
        // x - y = lOrigin.X - lOrigin.Y + k * lWidth
        if Brush.Style in [bsFDiagonal, bsDiagCross] then
          for k := Floor((L - B - lOrigin.X + lOrigin.Y) / lWidth) to Ceil((R - T - lOrigin.X + lOrigin.Y) / lWidth) do
            Segment(L, L - (lOrigin.X - lOrigin.Y + k * lWidth), R, R - (lOrigin.X - lOrigin.Y + k * lWidth));
        // x + y = lOrigin.X + lOrigin.Y - 1 + k * lWidth
        if Brush.Style in [bsBDiagonal, bsDiagCross] then
          for k := Floor((L + T - lOrigin.X - lOrigin.Y + 1) / lWidth) to Ceil((R + B - lOrigin.X - lOrigin.Y + 1) / lWidth) do
            Segment(L, lOrigin.X + lOrigin.Y - 1 + k * lWidth - L, R, lOrigin.X + lOrigin.Y - 1 + k * lWidth - R);
        FPage.StrokePath;
        end;
      bsPattern, bsImage:
        begin
        if (Brush.Style = bsImage) and not Assigned(Brush.Image) then
          exit;
        if Brush.Style = bsPattern then
          begin
          lTile := TFPMemoryImage.Create(32, 32);
          try
            for y := 0 to 31 do
              for x := 0 to 31 do
                begin
                lBit := ((Brush.Pattern[y] shr (31 - x)) and 1) <> 0;
                if lBit then
                  lTile.Colors[x, y] := Brush.FPColor
                else
                  lTile.Colors[x, y] := colTransparent;
                end;
            lNumber := AddImage(lTile, True);
          finally
            lTile.Free;
          end;
          lTileW := 32;
          lTileH := 32;
          lOrigin := Point(0, 0);
          end
        else
          begin
          lNumber := AddImage(Brush.Image, DrawingMode = dmAlphaBlend);
          lTileW := Brush.Image.Width;
          lTileH := Brush.Image.Height;
          if RelativeBrushImage then
            lOrigin := aOrigin
          else
            lOrigin := Point(0, 0);
          end;
        Clip;
        for y := Floor((T - lOrigin.Y) / lTileH) to Floor((B - lOrigin.Y) / lTileH) do
          for x := Floor((L - lOrigin.X) / lTileW) to Floor((R - lOrigin.X) / lTileW) do
            PlaceImage(lNumber, lOrigin.X + x * lTileW, lOrigin.Y + y * lTileH, lTileW, lTileH);
        end;
      bsClear: ;
    end;
  finally
    FPage.PopGraphicsStack;
  end;
end;


procedure TPDFCanvas.StrokeShape(aPath: TPDFCanvasPath);

begin
  if (Pen.Style = psClear) or (Pen.Mode = pmNop) then
    exit;
  BeginShapeState(Pen.FPColor, Pen.FPColor, False, True);
  try
    SetPenState;
    aPath();
    FPage.StrokePath;
  finally
    FPage.PopGraphicsStack;
  end;
end;


function TPDFCanvas.AddImage(aImage: TFPCustomImage; aAlpha: Boolean): Integer;

var
  lRGB, lAlpha: TBytes;
  x, y, i: Integer;
  c: TFPColor;
begin
  SetLength(lRGB, aImage.Width * aImage.Height * 3);
  lAlpha := nil;
  aAlpha := aAlpha and HasTransparency(aImage);
  if aAlpha then
    begin
    SetLength(lAlpha, aImage.Width * aImage.Height);
    FDocument.Options := FDocument.Options + [poUseImageTransparency];
    end;
  i := 0;
  for y := 0 to aImage.Height - 1 do
    for x := 0 to aImage.Width - 1 do
      begin
      c := aImage.Colors[x, y];
      lRGB[3 * i] := c.Red shr 8;
      lRGB[3 * i + 1] := c.Green shr 8;
      lRGB[3 * i + 2] := c.Blue shr 8;
      if aAlpha then
        lAlpha[i] := c.Alpha shr 8;
      Inc(i);
      end;
  Result := FDocument.Images.AddRawImage(aImage.Width, aImage.Height, lRGB, lAlpha);
end;


{ The image covers its pixels: from the left edge of pixel aX to the right edge of pixel aX + aWidth - 1. }
procedure TPDFCanvas.PlaceImage(aNumber, aX, aY, aWidth, aHeight: Integer);

begin
  FPage.DrawImage(aX - 0.5, PageY(aY + aHeight - 0.5), aWidth, aHeight, aNumber);
end;


{ ---- fonts and text ---- }

{ Font.Name is a TrueType file, a standard font name, or a family mapped to Helvetica, Times or Courier. }
function TPDFCanvas.CanvasFont: TPDFCanvasFont;

var
  lName, lKey: String;
  lIndex, i: Integer;
begin
  lName := Font.Name;
  if FileExists(lName) and (Pos(LowerCase(ExtractFileExt(lName)), '.ttf.otf') > 0) then
    lKey := lName
  else
    begin
    lIndex := -1;
    for i := 0 to High(CoreFonts) do
      if SameText(lName, CoreFonts[i].Name) then
        lIndex := i;
    if lIndex < 0 then
      begin
      lName := LowerCase(lName);
      if (Pos('courier', lName) > 0) or (Pos('mono', lName) > 0) then
        lIndex := 8
      else if (Pos('times', lName) > 0) or (Pos('roman', lName) > 0)
              or ((Pos('serif', lName) > 0) and (Pos('sans', lName) = 0)) then
        lIndex := 4
      else
        lIndex := 0;
      if Font.Bold then
        Inc(lIndex);
      if Font.Italic then
        Inc(lIndex, 2);
      end;
    lKey := CoreFonts[lIndex].Name;
    end;
  i := FFonts.IndexOf(lKey);
  if i >= 0 then
    exit(TPDFCanvasFont(FFonts.Objects[i]));
  Result := TPDFCanvasFont.Create;
  if lKey = Font.Name then
    begin
    Result.CoreIndex := -1;
    Result.TrueType := TFPFontCacheItem.Create(lKey);
    Result.FontIndex := FDocument.AddFont(lKey, ChangeFileExt(ExtractFileName(lKey), ''));
    end
  else
    begin
    Result.CoreIndex := lIndex;
    Result.FontIndex := FDocument.AddFont(lKey);
    end;
  FFonts.AddObject(lKey, Result);
end;


function TPDFCanvas.FontSize: Integer;

begin
  Result := Font.Size;
  if Result <= 0 then
    Result := 10;
end;


function TPDFCanvas.TextPoints(aFont: TPDFCanvasFont; const aText: String): Double;

var
  lWidths: TPDFFontWidthArray;
  lText: TCP1252String;
  lUnicode: UnicodeString;
  lInfo: TTFFileInfo;
  i: Integer;
begin
  Result := 0;
  if aFont.CoreIndex >= 0 then
    begin
    lWidths := FDocument.GetStdFontCharWidthsArray(CoreFonts[aFont.CoreIndex].Name);
    lText := TCP1252String(UTF8Decode(aText));
    for i := 1 to Length(lText) do
      Result := Result + lWidths[Ord(lText[i])];
    Result := Result * FontSize / 2048;
    end
  else
    begin
    lInfo := aFont.TrueType.FontData;
    lUnicode := UTF8Decode(aText);
    for i := 1 to Length(lUnicode) do
      Result := Result + lInfo.GetAdvanceWidth(lInfo.GetGlyphIndex(Ord(lUnicode[i])));
    Result := Result * FontSize / lInfo.Head.UnitsPerEm;
    end;
end;


function TPDFCanvas.DoGetTextMetrics(out aMetrics: TFPTextMetric): Boolean;

var
  lFont: TPDFCanvasFont;
  lInfo: TTFFileInfo;
begin
  lFont := CanvasFont;
  if lFont.CoreIndex >= 0 then
    with CoreFonts[lFont.CoreIndex] do
      begin
      aMetrics.Ascender := Round(Ascender * FontSize / 1000);
      aMetrics.Descender := Round(-Descender * FontSize / 1000);
      aMetrics.Height := Round((Ascender - Descender) * FontSize / 1000);
      end
  else
    begin
    lInfo := lFont.TrueType.FontData;
    aMetrics.Ascender := Round(lInfo.Ascender * FontSize / lInfo.Head.UnitsPerEm);
    aMetrics.Descender := Round(-lInfo.Descender * FontSize / lInfo.Head.UnitsPerEm);
    aMetrics.Height := Round((lInfo.Ascender - lInfo.Descender) * FontSize / lInfo.Head.UnitsPerEm);
    end;
  Result := True;
end;


function TPDFCanvas.GetNativeTextOrigin: TFPTextOrigin;

begin
  Result := toBaseline;
end;


procedure TPDFCanvas.DoGetTextSize(text: AnsiString; var w, h: Integer);

begin
  w := DoGetTextWidth(text);
  h := DoGetTextHeight(text);
end;


function TPDFCanvas.DoGetTextHeight(text: AnsiString): Integer;

var
  lMetrics: TFPTextMetric;
begin
  DoGetTextMetrics(lMetrics);
  Result := lMetrics.Height;
end;


function TPDFCanvas.DoGetTextWidth(text: AnsiString): Integer;

begin
  Result := Round(TextPoints(CanvasFont, text));
end;


{ Underline and strike-through bars are drawn in the rotated space of the text, with y up. }
procedure TPDFCanvas.DoTextOut(x, y: Integer; Text: AnsiString);

var
  lFont: TPDFCanvasFont;
  lWidth, lScale, lAngle, lPosition, lThickness, lXHeight: Double;
begin
  lFont := CanvasFont;
  BeginShapeState(Font.FPColor, Font.FPColor, True, False);
  try
    FPage.SetColor(ARGB(Font.FPColor), False);
    FPage.SetFont(lFont.FontIndex, FontSize);
    FPage.WriteText(x, PageY(y), Text, Font.Orientation / 10);
    if Font.Underline or Font.StrikeThrough then
      begin
      lWidth := TextPoints(lFont, Text);
      lScale := FontSize / 1000;
      if lFont.CoreIndex >= 0 then
        begin
        lPosition := CoreFonts[lFont.CoreIndex].UnderlinePosition;
        lThickness := CoreFonts[lFont.CoreIndex].UnderlineThickness;
        lXHeight := CoreFonts[lFont.CoreIndex].XHeight;
        end
      else
        begin
        lPosition := -100;
        lThickness := 50;
        lXHeight := 500;
        end;
      lAngle := DegToRad(Font.Orientation / 10);
      FPage.WriteRawContent(Format('q %s %s %s %s %s %s cm' + CRLF,
        [Num(Cos(lAngle)), Num(Sin(lAngle)), Num(-Sin(lAngle)), Num(Cos(lAngle)), Num(x), Num(FHeight - y)]));
      if Font.Underline then
        FPage.WriteRawContent(Format('0 %s %s %s re f' + CRLF,
          [Num((lPosition - lThickness / 2) * lScale), Num(lWidth), Num(lThickness * lScale)]));
      if Font.StrikeThrough then
        FPage.WriteRawContent(Format('0 %s %s %s re f' + CRLF,
          [Num((lXHeight - lThickness) / 2 * lScale), Num(lWidth), Num(lThickness * lScale)]));
      FPage.WriteRawContent('Q' + CRLF);
      end;
  finally
    FPage.PopGraphicsStack;
  end;
end;


{ ---- the native shapes ---- }

procedure TPDFCanvas.PDFLineTo(X1, Y1: Integer);

var
  lFrom: TPoint;

  procedure Path;
  begin
    FPage.MoveTo(lFrom.X, PageY(lFrom.Y));
    FPage.LineTo(X1, PageY(Y1));
  end;

begin
  lFrom := PenPos;
  StrokeShape(@Path);
end;


procedure TPDFCanvas.PDFLine(X1, Y1, X2, Y2: Integer);

  procedure Path;
  begin
    FPage.MoveTo(X1, PageY(Y1));
    FPage.LineTo(X2, PageY(Y2));
  end;

begin
  StrokeShape(@Path);
end;


procedure TPDFCanvas.PDFPolyline(const Points: array of TPoint; aClosed: Boolean);

var
  lPoints: array of TPoint;
  i: Integer;

  procedure Path;
  var
    j: Integer;
  begin
    FPage.MoveTo(lPoints[0].X, PageY(lPoints[0].Y));
    for j := 1 to High(lPoints) do
      FPage.LineTo(lPoints[j].X, PageY(lPoints[j].Y));
    if aClosed then
      FPage.ClosePath;
  end;

begin
  if Length(Points) = 0 then
    exit;
  SetLength(lPoints, Length(Points));
  for i := 0 to High(Points) do
    lPoints[i] := Points[i];
  StrokeShape(@Path);
end;


procedure TPDFCanvas.PDFPolygonFill(const Points: array of TPoint);

var
  lPoints: array of TPoint;
  lBounds: TRect;
  i: Integer;

  procedure Path;
  var
    j: Integer;
  begin
    FPage.MoveTo(lPoints[0].X, PageY(lPoints[0].Y));
    for j := 1 to High(lPoints) do
      FPage.LineTo(lPoints[j].X, PageY(lPoints[j].Y));
    FPage.ClosePath;
  end;

begin
  if Length(Points) = 0 then
    exit;
  SetLength(lPoints, Length(Points));
  lBounds := Rect(Points[0].X, Points[0].Y, Points[0].X, Points[0].Y);
  for i := 0 to High(Points) do
    begin
    lPoints[i] := Points[i];
    lBounds.Left := Min(lBounds.Left, Points[i].X);
    lBounds.Top := Min(lBounds.Top, Points[i].Y);
    lBounds.Right := Max(lBounds.Right, Points[i].X);
    lBounds.Bottom := Max(lBounds.Bottom, Points[i].Y);
    end;
  FillShape(@Path, lBounds, lBounds.TopLeft, not PolygonNonZeroWindingRule, False);
end;


procedure TPDFCanvas.PDFPolyBezier(Points: PPoint; NumPts: Integer; Filled, Continuous: Boolean);

var
  lSegments: Integer;
  lBounds: TRect;
  i: Integer;

  procedure Path;
  var
    j, lIdx: Integer;
  begin
    lIdx := 0;
    for j := 0 to lSegments - 1 do
      begin
      if (j = 0) or not Continuous then
        begin
        if Filled and (j > 0) then
          FPage.ClosePath;
        FPage.MoveTo(Points[lIdx].X, PageY(Points[lIdx].Y));
        Inc(lIdx);
        end;
      FPage.CubicCurveTo(Points[lIdx].X, PageY(Points[lIdx].Y), Points[lIdx + 1].X, PageY(Points[lIdx + 1].Y),
                         Points[lIdx + 2].X, PageY(Points[lIdx + 2].Y), 0, False);
      Inc(lIdx, 3);
      end;
    if Filled then
      FPage.ClosePath;
  end;

begin
  if Continuous then
    lSegments := (NumPts - 1) div 3
  else
    lSegments := NumPts div 4;
  if lSegments < 1 then
    exit;
  if Filled then
    begin
    lBounds := Rect(Points[0].X, Points[0].Y, Points[0].X, Points[0].Y);
    for i := 1 to NumPts - 1 do
      begin
      lBounds.Left := Min(lBounds.Left, Points[i].X);
      lBounds.Top := Min(lBounds.Top, Points[i].Y);
      lBounds.Right := Max(lBounds.Right, Points[i].X);
      lBounds.Bottom := Max(lBounds.Bottom, Points[i].Y);
      end;
    FillShape(@Path, lBounds, lBounds.TopLeft, not PolygonNonZeroWindingRule, False);
    end;
  StrokeShape(@Path);
end;


{ A fill covers the pixels of the bounds; a thick outline stays inside them. }
procedure TPDFCanvas.PDFRectangle(const Bounds: TRect; DoFill: Boolean);

var
  R: TRect;
  lInset: Double;

  procedure FillPath;
  begin
    RectPath(R.Left - 0.5, R.Top - 0.5, R.Right + 0.5, R.Bottom + 0.5);
  end;

  procedure OutlinePath;
  begin
    RectPath(R.Left + lInset, R.Top + lInset, R.Right - lInset, R.Bottom - lInset);
  end;

begin
  R := Bounds;
  if R.Left > R.Right then
    begin
    R.Left := Bounds.Right;
    R.Right := Bounds.Left;
    end;
  if R.Top > R.Bottom then
    begin
    R.Top := Bounds.Bottom;
    R.Bottom := Bounds.Top;
    end;
  if DoFill then
    FillShape(@FillPath, R, R.TopLeft, False, False)
  else
    begin
    lInset := Max(0, Min((Pen.Width - 1) / 2, Min(R.Right - R.Left, R.Bottom - R.Top) / 2));
    StrokeShape(@OutlinePath);
    end;
end;


procedure TPDFCanvas.PDFEllipse(const Bounds: TRect; DoFill: Boolean);

var
  R: TRect;
  rx, ry, cx, cy: Double;

  procedure Path;
  begin
    ArcPath(cx, cy, rx, ry, 0, 2 * Pi, True);
    FPage.ClosePath;
  end;

begin
  R := Rect(Min(Bounds.Left, Bounds.Right), Min(Bounds.Top, Bounds.Bottom),
            Max(Bounds.Left, Bounds.Right), Max(Bounds.Top, Bounds.Bottom));
  cx := (R.Left + R.Right) / 2;
  cy := (R.Top + R.Bottom) / 2;
  rx := (R.Right - R.Left) / 2;
  ry := (R.Bottom - R.Top) / 2;
  if DoFill then
    begin
    rx := rx + 0.5;
    ry := ry + 0.5;
    FillShape(@Path, R, R.TopLeft, False, True);
    end
  else
    begin
    // emInside: the outer edge of the stroke lies on the outer edge of the bounds
    if (EllipseMode = emInside) and (Pen.Width > 1) then
      begin
      rx := rx - (Pen.Width - 1) / 2;
      ry := ry - (Pen.Width - 1) / 2;
      end;
    if (rx > 0) and (ry > 0) then
      StrokeShape(@Path);
    end;
end;


procedure TPDFCanvas.PDFPie(const Bounds: TRect; aStart16, aLength16: Integer; aChord: Boolean);

var
  cx, cy, rx, ry: Double;

  procedure Path;
  begin
    if aChord then
      RayArcPath(cx, cy, rx, ry, aStart16, aLength16, True)
    else
      begin
      FPage.MoveTo(cx, PageY(cy));
      RayArcPath(cx, cy, rx, ry, aStart16, aLength16, False);
      end;
    FPage.ClosePath;
  end;

begin
  cx := (Bounds.Left + Bounds.Right) / 2;
  cy := (Bounds.Top + Bounds.Bottom) / 2;
  rx := Abs(Bounds.Right - Bounds.Left) / 2;
  ry := Abs(Bounds.Bottom - Bounds.Top) / 2;
  if (rx <= 0) or (ry <= 0) then
    exit;
  FillShape(@Path, Rect(Min(Bounds.Left, Bounds.Right), Min(Bounds.Top, Bounds.Bottom),
                        Max(Bounds.Left, Bounds.Right), Max(Bounds.Top, Bounds.Bottom)),
            Point(Min(Bounds.Left, Bounds.Right), Min(Bounds.Top, Bounds.Bottom)), False, True);
  StrokeShape(@Path);
end;


{ The corner radii follow the ellipse convention: RX pixels wide is a radius of (RX - 1) / 2. }
procedure TPDFCanvas.PDFRoundRect(const Bounds: TRect; RX, RY: Integer);

var
  L, T, R, B, lRX, lRY: Double;

  procedure Path;
  begin
    ArcPath(R - lRX, T + lRY, lRX, lRY, 0, Pi / 2, True);
    ArcPath(L + lRX, T + lRY, lRX, lRY, Pi / 2, Pi / 2, False);
    ArcPath(L + lRX, B - lRY, lRX, lRY, Pi, Pi / 2, False);
    ArcPath(R - lRX, B - lRY, lRX, lRY, 3 * Pi / 2, Pi / 2, False);
    FPage.ClosePath;
  end;

begin
  L := Min(Bounds.Left, Bounds.Right);
  R := Max(Bounds.Left, Bounds.Right);
  T := Min(Bounds.Top, Bounds.Bottom);
  B := Max(Bounds.Top, Bounds.Bottom);
  lRX := Min((RX - 1) / 2, (R - L) / 2);
  lRY := Min((RY - 1) / 2, (B - T) / 2);
  if (lRX <= 0) or (lRY <= 0) then
    begin
    PDFRectangle(Bounds, True);
    PDFRectangle(Bounds, False);
    exit;
    end;
  FillShape(@Path, Rect(Round(L), Round(T), Round(R), Round(B)), Point(Round(L), Round(T)), False, False);
  StrokeShape(@Path);
end;


procedure TPDFCanvas.PDFArc(const Bounds: TRect; aStart16, aLength16: Integer);

  procedure Path;
  begin
    RayArcPath((Bounds.Left + Bounds.Right) / 2, (Bounds.Top + Bounds.Bottom) / 2,
               Abs(Bounds.Right - Bounds.Left) / 2, Abs(Bounds.Bottom - Bounds.Top) / 2, aStart16, aLength16, True);
  end;

begin
  StrokeShape(@Path);
end;


procedure TPDFCanvas.PDFImage(x, y, w, h: Integer; aImage: TFPCustomImage; aInterpolate: Boolean);

var
  lNumber: Integer;
begin
  if (w <= 0) or (h <= 0) or (aImage.Width = 0) or (aImage.Height = 0) then
    exit;
  BeginShapeState(colWhite, colWhite, False, False);
  try
    lNumber := AddImage(aImage, DrawingMode = dmAlphaBlend);
    FDocument.Images[lNumber].Interpolate := aInterpolate;
    PlaceImage(lNumber, x, y, w, h);
  finally
    FPage.PopGraphicsStack;
  end;
end;


{ ---- the hooks of TFPCustomCanvas, with the shadow raster ---- }

procedure TPDFCanvas.DoMoveTo(x, y: Integer);

begin
end;


procedure TPDFCanvas.DoLineTo(x, y: Integer);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if FShadow then
    FShadowCanvas.Line(PenPos.X, PenPos.Y, x, y);
  if not lPatch then
    PDFLineTo(x, y);
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoLine(x1, y1, x2, y2: Integer);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch then
    PDFLine(x1, y1, x2, y2);
  if FShadow then
    FShadowCanvas.Line(x1, y1, x2, y2);
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoRectangle(const Bounds: TRect);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch then
    PDFRectangle(Bounds, False);
  if FShadow then
    begin
    FShadowCanvas.Brush.Style := bsClear;
    FShadowCanvas.Rectangle(Bounds);
    end;
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoRectangleFill(const Bounds: TRect);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(False, True);
  if not lPatch then
    PDFRectangle(Bounds, True);
  if FShadow then
    FShadowCanvas.FillRect(Bounds);
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoEllipse(const Bounds: TRect);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch then
    PDFEllipse(Bounds, False);
  if FShadow then
    begin
    FShadowCanvas.Brush.Style := bsClear;
    FShadowCanvas.Ellipse(Bounds);
    end;
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoEllipseFill(const Bounds: TRect);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(False, True);
  if not lPatch then
    PDFEllipse(Bounds, True);
  if FShadow then
    begin
    FShadowCanvas.Pen.Style := psClear;
    FShadowCanvas.Ellipse(Bounds);
    end;
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoPolygon(const Points: array of TPoint);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch then
    PDFPolyline(Points, True);
  if FShadow then
    begin
    FShadowCanvas.Brush.Style := bsClear;
    FShadowCanvas.Polygon(Points);
    end;
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoPolygonFill(const Points: array of TPoint);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(False, True);
  if not lPatch then
    PDFPolygonFill(Points);
  if FShadow then
    begin
    FShadowCanvas.Pen.Style := psClear;
    FShadowCanvas.Polygon(Points);
    end;
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoPolyline(const Points: array of TPoint);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, False);
  if not lPatch then
    PDFPolyline(Points, False);
  if FShadow then
    FShadowCanvas.Polyline(Points);
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoPolyBezier(Points: PPoint; NumPts: Integer; Filled: Boolean; Continuous: Boolean);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, Filled);
  if not lPatch then
    PDFPolyBezier(Points, NumPts, Filled, Continuous);
  if FShadow then
    FShadowCanvas.PolyBezier(Points, NumPts, Filled, Continuous);
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoRadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength: Integer);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, True);
  if not lPatch then
    PDFPie(Rect(x1, y1, x2, y2), StartAngle16Deg, Angle16DegLength, False);
  if FShadow then
    FShadowCanvas.RadialPie(x1, y1, x2, y2, StartAngle16Deg, Angle16DegLength);
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoChord(const Bounds: TRect; aStart16, aLength16: Integer);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, True);
  if not lPatch then
    PDFPie(Bounds, aStart16, aLength16, True);
  if FShadow then
    FShadowCanvas.Chord(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom, aStart16, aLength16);
  EndShape(lPatch);
end;


procedure TPDFCanvas.DoRoundRect(const Bounds: TRect; RX, RY: Integer);

var
  lPatch: Boolean;
begin
  lPatch := StartShape(True, True);
  if not lPatch then
    PDFRoundRect(Bounds, RX, RY);
  if FShadow then
    FShadowCanvas.RoundRect(Bounds, RX, RY);
  EndShape(lPatch);
end;


procedure TPDFCanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer);

var
  R: TRect;
  lPatch: Boolean;
begin
  if (Pen.Style = psClear) or not DeviceRect(Rect(ALeft, ATop, ARight, ABottom), R) then
    exit;
  if (R.Right = R.Left) or (R.Bottom = R.Top) then
    begin
    inherited Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength);
    exit;
    end;
  lPatch := StartShape(True, False);
  if not lPatch then
    PDFArc(R, Angle16Deg, Angle16DegLength);
  if FShadow then
    FShadowCanvas.Arc(R.Left, R.Top, R.Right, R.Bottom, Angle16Deg, Angle16DegLength);
  EndShape(lPatch);
end;


{ The shading covers the pixels of R; the colours run between the centres of the first and last pixel. }
procedure TPDFCanvas.GradientFill(const ARect: TRect; AStartColor, AEndColor: TFPColor;
  ADirection: TFPGradientDirection);

var
  R: TRect;
  lShading: TPDFShadingItem;
  lPatch: Boolean;
begin
  if not DeviceRect(ARect, R) then
    exit;
  lPatch := (DrawingMode = dmCustom);
  if lPatch and not FShadow then
    raise EPDFCanvas.Create(SErrNeedsShadow);
  if FShadow then
    SyncShadow;
  if lPatch then
    TakeSnapshot
  else
    begin
    lShading := FDocument.Shadings.AddShading;
    lShading.Kind := pshAxial;
    lShading.Coords[0] := R.Left;
    lShading.Coords[1] := FHeight - R.Top;
    if ADirection = gdVertical then
      begin
      lShading.Coords[2] := R.Left;
      lShading.Coords[3] := FHeight - R.Bottom;
      end
    else
      begin
      lShading.Coords[2] := R.Right;
      lShading.Coords[3] := FHeight - R.Top;
      end;
    lShading.ExtendStart := True;
    lShading.ExtendEnd := True;
    lShading.AddStop(0, ARGB(AStartColor));
    lShading.AddStop(1, ARGB(AEndColor));
    BeginShapeState(AStartColor, AStartColor, True, False);
    try
      RectPath(R.Left - 0.5, R.Top - 0.5, R.Right + 0.5, R.Bottom + 0.5);
      FPage.ClipPath;
      FPage.PaintShading(lShading.Index);
    finally
      FPage.PopGraphicsStack;
    end;
    end;
  if FShadow then
    FShadowCanvas.GradientFill(R, AStartColor, AEndColor, ADirection);
  if lPatch then
    WritePatch;
end;


procedure TPDFCanvas.SetColor(x, y: Integer; const Value: TFPColor);

var
  lBrush: TFPColor;
  lStyle: TFPBrushStyle;
begin
  lBrush := Brush.FPColor;
  lStyle := Brush.Style;
  Brush.FPColor := Value;
  Brush.Style := bsSolid;
  try
    PDFRectangle(Rect(x, y, x, y), True);
  finally
    Brush.FPColor := lBrush;
    Brush.Style := lStyle;
  end;
  if FShadow then
    FShadowImage.Colors[x, y] := Value;
end;


function TPDFCanvas.GetColor(x, y: Integer): TFPColor;

begin
  Result := colTransparent;
  if not FShadow then
    raise EPDFCanvas.Create(SErrCannotReadPixels);
  if (x >= 0) and (y >= 0) and (x < FShadowImage.Width) and (y < FShadowImage.Height) then
    Result := FShadowImage.Colors[x, y];
end;


procedure TPDFCanvas.DoDraw(x, y: Integer; const image: TFPCustomImage);

var
  lPatch: Boolean;
begin
  lPatch := DrawingMode = dmCustom;
  if lPatch and not FShadow then
    raise EPDFCanvas.Create(SErrNeedsShadow);
  if FShadow then
    SyncShadow;
  if lPatch then
    TakeSnapshot
  else
    PDFImage(x, y, image.Width, image.Height, image);
  if FShadow then
    FShadowCanvas.Draw(x, y, image);
  if lPatch then
    WritePatch;
end;


procedure TPDFCanvas.DoCopyRect(x, y: Integer; canvas: TFPCustomCanvas; const SourceRect: TRect);

var
  lImage: TFPMemoryImage;
  lX, lY: Integer;
begin
  with SourceRect do
    begin
    if (Right < Left) or (Bottom < Top) then
      exit;
    lImage := TFPMemoryImage.Create(Right - Left + 1, Bottom - Top + 1);
    end;
  try
    for lY := 0 to lImage.Height - 1 do
      for lX := 0 to lImage.Width - 1 do
        lImage.Colors[lX, lY] := canvas.Colors[SourceRect.Left + lX, SourceRect.Top + lY];
    BeginShapeState(colWhite, colWhite, False, False);
    try
      PlaceImage(AddImage(lImage, False), x, y, lImage.Width, lImage.Height);
    finally
      FPage.PopGraphicsStack;
    end;
    if FShadow then
      begin
      SyncShadow;
      FShadowCanvas.DrawingMode := dmOpaque;
      FShadowCanvas.Draw(x, y, lImage);
      end;
  finally
    lImage.Free;
  end;
end;


{ Without an Interpolation the viewer scales the source; with one it is resampled first. }
procedure TPDFCanvas.DoStretchDraw(x, y, w, h: Integer; source: TFPCustomImage);

var
  lPatch: Boolean;
  lImage: TFPMemoryImage;
  lCanvas: TFPImageCanvas;
begin
  if (w <= 0) or (h <= 0) then
    exit;
  lPatch := DrawingMode = dmCustom;
  if lPatch and not FShadow then
    raise EPDFCanvas.Create(SErrNeedsShadow);
  if FShadow then
    begin
    SyncShadow;
    if lPatch then
      TakeSnapshot;
    FShadowCanvas.StretchDraw(x, y, w, h, source);
    FShadowCanvas.Interpolation := nil;
    if lPatch then
      begin
      WritePatch;
      exit;
      end;
    end;
  if Interpolation = nil then
    begin
    PDFImage(x, y, w, h, source, True);
    exit;
    end;
  lImage := TFPMemoryImage.Create(w, h);
  try
    lCanvas := TFPImageCanvas.Create(lImage);
    try
      lCanvas.Interpolation := Interpolation;
      lCanvas.StretchDraw(0, 0, w, h, source);
    finally
      lCanvas.Interpolation := nil;
      lCanvas.Free;
    end;
    PDFImage(x, y, w, h, lImage);
  finally
    lImage.Free;
  end;
end;


procedure TPDFCanvas.DoFloodFill(x, y: Integer);

begin
  FloodRegion(x, y, False, colTransparent);
end;


procedure TPDFCanvas.DoFloodFillStyle(x, y: Integer; const FillColor: TFPColor; FillStyle: TFPFloodFillStyle);

begin
  if FillStyle = ffSurface then
    inherited DoFloodFillStyle(x, y, FillColor, FillStyle)
  else
    FloodRegion(x, y, True, FillColor);
end;


{ ---- the shadow raster ---- }

procedure TPDFCanvas.SetShadow(aValue: Boolean);

begin
  if FShadow = aValue then
    exit;
  FShadow := aValue;
  UpdateShadow;
end;


procedure TPDFCanvas.UpdateShadow;

begin
  if FShadow and (FWidth > 0) and (FHeight > 0) then
    begin
    if Assigned(FShadowImage) and (FShadowImage.Width = FWidth) and (FShadowImage.Height = FHeight) then
      exit;
    FreeAndNil(FShadowCanvas);
    FreeAndNil(FShadowImage);
    FShadowImage := TFPMemoryImage.Create(FWidth, FHeight);
    FShadowCanvas := TFPImageCanvas.Create(FShadowImage);
    FShadowCanvas.RectangleMode := rmInclude;
    ClearShadow;
    end
  else
    begin
    FreeAndNil(FShadowCanvas);
    FreeAndNil(FShadowImage);
    FShadow := FShadow and ((FWidth <= 0) or (FHeight <= 0));
    end;
end;


procedure TPDFCanvas.ClearShadow;

var
  x, y: Integer;
begin
  if not Assigned(FShadowImage) then
    exit;
  for y := 0 to FShadowImage.Height - 1 do
    for x := 0 to FShadowImage.Width - 1 do
      FShadowImage.Colors[x, y] := colWhite;
end;


procedure TPDFCanvas.SyncShadow;

begin
  UpdateShadow;
  with FShadowCanvas do
    begin
    Pen.Style := Self.Pen.Style;
    Pen.Width := Max(1, Self.Pen.Width);
    Pen.Mode := Self.Pen.Mode;
    Pen.FPColor := Self.Pen.FPColor;
    Pen.EndCap := Self.Pen.EndCap;
    Pen.JoinStyle := Self.Pen.JoinStyle;
    Pen.Pattern := Self.Pen.Pattern;
    Brush.Style := Self.Brush.Style;
    Brush.FPColor := Self.Brush.FPColor;
    Brush.Image := Self.Brush.Image;
    Brush.Pattern := Self.Brush.Pattern;
    DrawingMode := Self.DrawingMode;
    OnCombineColors := Self.OnCombineColors;
    EllipseMode := Self.EllipseMode;
    HashWidth := Self.HashWidth;
    HatchOrigin := Self.HatchOrigin;
    RelativeBrushImage := Self.RelativeBrushImage;
    PolygonNonZeroWindingRule := Self.PolygonNonZeroWindingRule;
    Interpolation := Self.Interpolation;
    Clipping := Self.Clipping;
    if Self.Clipping then
      ClipRect := Self.DeviceClipRect;
    end;
end;


{ Native are the pen modes that ignore the page, pmNot, alpha blending and pmNop, which strokes nothing. }
function TPDFCanvas.StartShape(aPen, aFill: Boolean): Boolean;

begin
  aPen := aPen and (Pen.Style <> psClear);
  aFill := aFill and (Brush.Style <> bsClear);
  Result := (aPen and not (Pen.Mode in [pmCopy, pmBlack, pmWhite, pmNop, pmNotCopy, pmNot]))
            or ((aPen or aFill) and (DrawingMode = dmCustom));
  if Result and not FShadow then
    raise EPDFCanvas.Create(SErrNeedsShadow);
  if FShadow then
    SyncShadow;
  if Result then
    TakeSnapshot;
  FNopPen := not Result and aPen and (Pen.Mode = pmNop);
  if FNopPen then
    begin
    FNopPenStyle := Pen.Style;
    Pen.Style := psClear;
    end;
end;


procedure TPDFCanvas.EndShape(aPatch: Boolean);

begin
  if FNopPen then
    begin
    Pen.Style := FNopPenStyle;
    FNopPen := False;
    end;
  if aPatch then
    WritePatch;
end;


procedure TPDFCanvas.TakeSnapshot;

var
  x, y: Integer;
begin
  if not Assigned(FSnapshot) then
    FSnapshot := TFPMemoryImage.Create(FShadowImage.Width, FShadowImage.Height)
  else if (FSnapshot.Width <> FShadowImage.Width) or (FSnapshot.Height <> FShadowImage.Height) then
    FSnapshot.SetSize(FShadowImage.Width, FShadowImage.Height);
  for y := 0 to FShadowImage.Height - 1 do
    for x := 0 to FShadowImage.Width - 1 do
      FSnapshot.Colors[x, y] := FShadowImage.Colors[x, y];
end;


procedure TPDFCanvas.WritePatch;

var
  x, y, lLeft, lTop, lRight, lBottom: Integer;
  lPatch: TFPMemoryImage;
  c: TFPColor;
begin
  lLeft := FShadowImage.Width;
  lTop := FShadowImage.Height;
  lRight := -1;
  lBottom := -1;
  for y := 0 to FShadowImage.Height - 1 do
    for x := 0 to FShadowImage.Width - 1 do
      if FShadowImage.Colors[x, y] <> FSnapshot.Colors[x, y] then
        begin
        lLeft := Min(lLeft, x);
        lRight := Max(lRight, x);
        lTop := Min(lTop, y);
        lBottom := Max(lBottom, y);
        end;
  if lRight < 0 then
    exit;
  lPatch := TFPMemoryImage.Create(lRight - lLeft + 1, lBottom - lTop + 1);
  try
    for y := lTop to lBottom do
      for x := lLeft to lRight do
        begin
        c := FShadowImage.Colors[x, y];
        if c <> FSnapshot.Colors[x, y] then
          c.Alpha := alphaOpaque
        else
          c.Alpha := alphaTransparent;
        lPatch.Colors[x - lLeft, y - lTop] := c;
        end;
    FPage.PushGraphicsStack;
    try
      PlaceImage(AddImage(lPatch, True), lLeft, lTop, lPatch.Width, lPatch.Height);
    finally
      FPage.PopGraphicsStack;
    end;
  finally
    lPatch.Free;
  end;
end;


{ The region of a surface fill is what a sentinel fill of a copy of the shadow changes; the region
  of a border fill is searched in the shadow, limited to the clip rectangle. Its pixel outline is
  filled as a path. }
procedure TPDFCanvas.FloodRegion(x, y: Integer; aBorder: Boolean; const aBorderColor: TFPColor);

var
  lScratch: TFPImageCanvas;
  lSentinel: TFPColor;
  lRegion: array of Boolean;
  lStack: array of TPoint;
  lClip, lBounds: TRect;
  lX, lY, lCount, i: Integer;
  p: TPoint;
  lPatch: Boolean;

  function Fillable(ax, ay: Integer): Boolean;
  begin
    Result := (ax >= lClip.Left) and (ax <= lClip.Right) and (ay >= lClip.Top) and (ay <= lClip.Bottom)
              and not lRegion[ay * FWidth + ax] and (FShadowImage.Colors[ax, ay] <> aBorderColor);
  end;

  procedure Push(ax, ay: Integer);
  begin
    if lCount = Length(lStack) then
      SetLength(lStack, 2 * lCount + 64);
    lStack[lCount] := Point(ax, ay);
    Inc(lCount);
  end;

  function Inside(ax, ay: Integer): Boolean;
  begin
    Result := (ax >= 0) and (ay >= 0) and (ax < FWidth) and (ay < FHeight) and lRegion[ay * FWidth + ax];
  end;

  // Appends the region to the path as a rectangle per run of pixels in a row.
  procedure Path;
  var
    ax, ay: Integer;

    procedure Run(ax1, ax2, ay1: Integer);
    begin
      FPage.MoveTo(ax1 - 0.5, PageY(ay1 - 0.5));
      FPage.LineTo(ax2 + 0.5, PageY(ay1 - 0.5));
      FPage.LineTo(ax2 + 0.5, PageY(ay1 + 0.5));
      FPage.LineTo(ax1 - 0.5, PageY(ay1 + 0.5));
      FPage.ClosePath;
    end;

  var
    lStart: Integer;
  begin
    for ay := lBounds.Top to lBounds.Bottom do
      begin
      ax := lBounds.Left;
      while ax <= lBounds.Right do
        if Inside(ax, ay) then
          begin
          lStart := ax;
          while (ax <= lBounds.Right) and Inside(ax, ay) do
            Inc(ax);
          Run(lStart, ax - 1, ay);
          end
        else
          Inc(ax);
      end;
  end;

begin
  if not FShadow then
    raise EPDFCanvas.Create(SErrNeedsShadow);
  if (Brush.Style = bsClear) or (x < 0) or (y < 0) or (x >= FWidth) or (y >= FHeight) then
    exit;
  SyncShadow;
  SetLength(lRegion, FWidth * FHeight);
  if aBorder then
    begin
    lClip := Rect(0, 0, FWidth - 1, FHeight - 1);
    if Clipping then
      begin
      lClip.Left := Max(lClip.Left, DeviceClipRect.Left);
      lClip.Top := Max(lClip.Top, DeviceClipRect.Top);
      lClip.Right := Min(lClip.Right, DeviceClipRect.Right);
      lClip.Bottom := Min(lClip.Bottom, DeviceClipRect.Bottom);
      end;
    lStack := nil;
    lCount := 0;
    Push(x, y);
    while lCount > 0 do
      begin
      Dec(lCount);
      p := lStack[lCount];
      if not Fillable(p.X, p.Y) then
        continue;
      lRegion[p.Y * FWidth + p.X] := True;
      if p.X > 0 then
        Push(p.X - 1, p.Y);
      if p.X < FWidth - 1 then
        Push(p.X + 1, p.Y);
      if p.Y > 0 then
        Push(p.X, p.Y - 1);
      if p.Y < FHeight - 1 then
        Push(p.X, p.Y + 1);
      end;
    end
  else
    begin
    TakeSnapshot;
    lSentinel := FSnapshot.Colors[x, y];
    lSentinel.Red := lSentinel.Red xor $FFFF;
    lScratch := TFPImageCanvas.Create(FSnapshot);
    try
      lScratch.Clipping := Clipping;
      if Clipping then
        lScratch.ClipRect := DeviceClipRect;
      lScratch.Brush.Style := bsSolid;
      lScratch.Brush.FPColor := lSentinel;
      lScratch.FloodFill(x, y);
    finally
      lScratch.Free;
    end;
    for i := 0 to FWidth * FHeight - 1 do
      lRegion[i] := FSnapshot.Colors[i mod FWidth, i div FWidth] <> FShadowImage.Colors[i mod FWidth, i div FWidth];
    end;
  lBounds := Rect(FWidth, FHeight, -1, -1);
  for lY := 0 to FHeight - 1 do
    for lX := 0 to FWidth - 1 do
      if lRegion[lY * FWidth + lX] then
        begin
        lBounds.Left := Min(lBounds.Left, lX);
        lBounds.Right := Max(lBounds.Right, lX);
        lBounds.Top := Min(lBounds.Top, lY);
        lBounds.Bottom := Max(lBounds.Bottom, lY);
        end;
  if lBounds.Right < 0 then
    exit;
  lPatch := DrawingMode = dmCustom;
  if lPatch then
    TakeSnapshot
  else
    FillShape(@Path, lBounds, Point(x, y), False, True);
  if aBorder then
    FShadowCanvas.FloodFill(x, y, aBorderColor, ffBorder)
  else
    FShadowCanvas.FloodFill(x, y);
  if lPatch then
    WritePatch;
end;

initialization
  NumberFormat := DefaultFormatSettings;
  NumberFormat.DecimalSeparator := '.';
end.
