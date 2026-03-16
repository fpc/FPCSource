{$mode objfpc}{$h+}
program TransformDemo;
{
  Demonstrates coordinate transformation on every affected TFPCustomCanvas
  drawing method. Each demo creates a separate PNG image showing the method
  drawn first without transformation (in gray) and then with a transformation
  (in color), so the effect is clearly visible.
}

uses
  Classes, SysUtils, Math, FPImage, FPCanvas, FPImgCanv, FPWritePNG;

const
  W = 200;  // image width
  H = 200;  // image height

var
  Writer: TFPWriterPNG;

procedure InitWriter;
begin
  Writer := TFPWriterPNG.Create;
  Writer.Indexed := false;
  Writer.WordSized := false;
  Writer.UseAlpha := false;
  Writer.GrayScale := false;
end;

procedure SaveImage(Image: TFPMemoryImage; const FileName: string);
begin
  Image.SaveToFile(FileName, Writer);
  WriteLn('Saved: ', FileName);
end;

{ Helper: create image+canvas, fill white background }
procedure CreateCanvas(out Img: TFPMemoryImage; out Cvs: TFPImageCanvas;
  AWidth: Integer = W; AHeight: Integer = H);
begin
  Img := TFPMemoryImage.Create(AWidth, AHeight);
  Cvs := TFPImageCanvas.Create(Img);
  Cvs.Brush.FPColor := colWhite;
  Cvs.Brush.Style := bsSolid;
  Cvs.Pen.FPColor := colWhite;
  Cvs.Pen.Style := psSolid;
  Cvs.Rectangle(0, 0, AWidth, AHeight);
end;

{ ---------- Demo: Line ---------- }
procedure DemoLine;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs);
  // Reference: untransformed line in gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Pen.Width := 1;
  Cvs.Line(20, 100, 180, 100);
  // Transformed: translate down 30px, then draw same line in red
  Cvs.Translate(0, 30);
  Cvs.Pen.FPColor := colRed;
  Cvs.Line(20, 100, 180, 100);
  Cvs.ResetTransform;
  // Transformed: rotate 30 degrees around image center (100,100)
  Cvs.Translate(-100, -100);
  Cvs.Rotate(Pi / 6);
  Cvs.Translate(100, 100);
  Cvs.Pen.FPColor := colBlue;
  Cvs.Line(20, 100, 180, 100);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_line.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: MoveTo / LineTo ---------- }
procedure DemoMoveToLineTo;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs);
  // Reference in gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Pen.Width := 1;
  Cvs.MoveTo(20, 40);
  Cvs.LineTo(180, 40);
  Cvs.LineTo(180, 160);
  // Translated in red
  Cvs.Translate(0, 20);
  Cvs.Pen.FPColor := colRed;
  Cvs.MoveTo(20, 40);
  Cvs.LineTo(180, 40);
  Cvs.LineTo(180, 160);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_moveto_lineto.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: Rectangle ---------- }
procedure DemoRectangle;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs);
  // Reference rectangle in gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Brush.Style := bsClear;
  Cvs.Rectangle(40, 60, 160, 140);
  // Translated rectangle in red
  Cvs.Translate(10, -20);
  Cvs.Pen.FPColor := colRed;
  Cvs.Rectangle(40, 60, 160, 140);
  Cvs.ResetTransform;
  // Rotated rectangle in blue (dispatches to polygon)
  Cvs.Translate(-100, -100);
  Cvs.Rotate(Pi / 8);
  Cvs.Translate(100, 100);
  Cvs.Pen.FPColor := colBlue;
  Cvs.Rectangle(40, 60, 160, 140);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_rectangle.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: FillRect ---------- }
procedure DemoFillRect;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
  LightRed: TFPColor;
begin
  CreateCanvas(Img, Cvs);
  LightRed.Red := $FFFF; LightRed.Green := $AAAA; LightRed.Blue := $AAAA; LightRed.Alpha := alphaOpaque;
  // Reference in light gray
  Cvs.Brush.FPColor := colSilver;
  Cvs.Brush.Style := bsSolid;
  Cvs.Pen.Style := psClear;
  Cvs.FillRect(50, 70, 150, 130);
  // Translated in light red
  Cvs.Translate(20, 20);
  Cvs.Brush.FPColor := LightRed;
  Cvs.FillRect(50, 70, 150, 130);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_fillrect.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: Ellipse ---------- }
procedure DemoEllipse;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs);
  // Reference ellipse in gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Brush.Style := bsClear;
  Cvs.Ellipse(30, 50, 170, 150);
  // Translated ellipse in red
  Cvs.Translate(0, -20);
  Cvs.Pen.FPColor := colRed;
  Cvs.Ellipse(30, 50, 170, 150);
  Cvs.ResetTransform;
  // Scaled ellipse in blue (scale Y 2x around center)
  Cvs.Translate(-100, -100);
  Cvs.Scale(1, 2);
  Cvs.Translate(100, 100);
  Cvs.Pen.FPColor := colBlue;
  Cvs.Ellipse(60, 80, 140, 120);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_ellipse.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: Polygon ---------- }
procedure DemoPolygon;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
  Pts: array[0..2] of TPoint;
begin
  CreateCanvas(Img, Cvs);
  // Triangle
  Pts[0] := Point(100, 30);
  Pts[1] := Point(170, 160);
  Pts[2] := Point(30, 160);
  // Reference in gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Brush.Style := bsClear;
  Cvs.Polygon(Pts);
  // Rotated 20 degrees around image center in red
  Cvs.Translate(-100, -100);
  Cvs.Rotate(20 * Pi / 180);
  Cvs.Translate(100, 100);
  Cvs.Pen.FPColor := colRed;
  Cvs.Polygon(Pts);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_polygon.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: Polyline ---------- }
procedure DemoPolyline;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
  Pts: array[0..3] of TPoint;
begin
  CreateCanvas(Img, Cvs);
  // Zigzag
  Pts[0] := Point(20, 100);
  Pts[1] := Point(80, 40);
  Pts[2] := Point(120, 160);
  Pts[3] := Point(180, 100);
  // Reference in gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Pen.Width := 1;
  Cvs.Polyline(Pts);
  // Translated down in red
  Cvs.Translate(0, 20);
  Cvs.Pen.FPColor := colRed;
  Cvs.Polyline(Pts);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_polyline.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: FloodFill ---------- }
procedure DemoFloodFill;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
  LightBlue: TFPColor;
begin
  CreateCanvas(Img, Cvs);
  LightBlue.Red := $AAAA; LightBlue.Green := $AAAA; LightBlue.Blue := $FFFF; LightBlue.Alpha := alphaOpaque;
  // Draw two small closed boxes and flood-fill them
  // (keep boxes small to avoid stack overflow in recursive flood fill)
  Cvs.Pen.FPColor := colBlack;
  Cvs.Pen.Width := 1;
  Cvs.Brush.Style := bsClear;
  Cvs.Rectangle(40, 70, 70, 100);
  Cvs.Rectangle(120, 70, 150, 100);
  // Fill first box at its center (no transform)
  Cvs.Brush.FPColor := colSilver;
  Cvs.Brush.Style := bsSolid;
  Cvs.FloodFill(55, 85);
  // Translate so that (55,85) maps into the second box, fill in blue
  Cvs.Translate(80, 0);
  Cvs.Brush.FPColor := LightBlue;
  Cvs.FloodFill(55, 85);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_floodfill.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: TextOut (ansistring) ---------- }
{ Note: TFPPixelCanvas does not implement DoTextOut (raises ENotImplemented).
  To demonstrate that TextOut applies the transform to the (x,y) position,
  we use a TFPCustomDrawFont descendant or skip this demo if unavailable.
  Here we demonstrate by catching the exception and documenting the limitation. }
procedure DemoTextOut;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs);
  Cvs.Brush.Style := bsClear;
  // TFPPixelCanvas does not implement text rendering, so we demonstrate the
  // coordinate transformation using a cross marker drawn with Line instead.
  // Reference marker at (50, 80) in gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Line(45, 80, 55, 80);
  Cvs.Line(50, 75, 50, 85);
  // Translated marker in red: same coordinates, shifted down 40px by transform
  Cvs.Translate(0, 40);
  Cvs.Pen.FPColor := colRed;
  Cvs.Line(45, 80, 55, 80);
  Cvs.Line(50, 75, 50, 85);
  Cvs.ResetTransform;
  // Translated marker in blue: shifted right 80px
  Cvs.Translate(80, 0);
  Cvs.Pen.FPColor := colBlue;
  Cvs.Line(45, 80, 55, 80);
  Cvs.Line(50, 75, 50, 85);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_textout.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: EllipseC ---------- }
{ EllipseC delegates to Ellipse, so this shows transform applies transitively. }
procedure DemoEllipseC;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs);
  // Reference circle in gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Brush.Style := bsClear;
  Cvs.EllipseC(100, 100, 40, 40);
  // Translated in red
  Cvs.Translate(30, -30);
  Cvs.Pen.FPColor := colRed;
  Cvs.EllipseC(100, 100, 40, 40);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_ellipsec.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: CopyRect ---------- }
procedure DemoCopyRect;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
  SrcImg: TFPMemoryImage;
  SrcCvs: TFPImageCanvas;
begin
  // Create a small source image with a red square
  SrcImg := TFPMemoryImage.Create(40, 40);
  SrcCvs := TFPImageCanvas.Create(SrcImg);
  SrcCvs.Brush.FPColor := colRed;
  SrcCvs.Brush.Style := bsSolid;
  SrcCvs.Pen.FPColor := colRed;
  SrcCvs.Rectangle(0, 0, 40, 40);
  // Draw a blue border
  SrcCvs.Pen.FPColor := colBlue;
  SrcCvs.Brush.Style := bsClear;
  SrcCvs.Rectangle(0, 0, 39, 39);

  CreateCanvas(Img, Cvs);
  // Reference: copy at (20, 80)
  Cvs.CopyRect(20, 80, SrcCvs, Rect(0, 0, 39, 39));
  // Transformed: translate so it appears at a different position
  Cvs.Translate(80, 0);
  Cvs.CopyRect(20, 80, SrcCvs, Rect(0, 0, 39, 39));
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_copyrect.png');
  SrcCvs.Free;
  SrcImg.Free;
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: Draw ---------- }
procedure DemoDraw;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
  Sprite: TFPMemoryImage;
  SpriteCvs: TFPImageCanvas;
begin
  // Create a small sprite: green square with yellow border
  Sprite := TFPMemoryImage.Create(30, 30);
  SpriteCvs := TFPImageCanvas.Create(Sprite);
  SpriteCvs.Brush.FPColor := colGreen;
  SpriteCvs.Brush.Style := bsSolid;
  SpriteCvs.Pen.FPColor := colYellow;
  SpriteCvs.Rectangle(0, 0, 30, 30);

  CreateCanvas(Img, Cvs);
  // Reference: draw at (30, 80)
  Cvs.Draw(30, 80, Sprite);
  // Transformed: translate right 80px
  Cvs.Translate(80, 0);
  Cvs.Draw(30, 80, Sprite);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_draw.png');
  SpriteCvs.Free;
  Sprite.Free;
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: PolyBezier (array of TPoint) ---------- }
procedure DemoPolyBezier;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
  Pts: array[0..6] of TPoint;
begin
  CreateCanvas(Img, Cvs);
  // Two continuous cubic Bezier segments (7 points = 1 + 3*2)
  Pts[0] := Point(20, 100);   // start
  Pts[1] := Point(50, 20);    // cp1
  Pts[2] := Point(80, 20);    // cp2
  Pts[3] := Point(100, 100);  // end of first / start of second
  Pts[4] := Point(120, 180);  // cp1
  Pts[5] := Point(150, 180);  // cp2
  Pts[6] := Point(180, 100);  // end
  // Reference in gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Pen.Width := 1;
  Cvs.PolyBezier(Pts, False, True);
  // Translated down 30px in red
  Cvs.Translate(0, 30);
  Cvs.Pen.FPColor := colRed;
  Cvs.PolyBezier(Pts, False, True);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_polybezier.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: PolyBezier filled ---------- }
procedure DemoPolyBezierFilled;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
  Pts: array[0..6] of TPoint;
  LightGreen: TFPColor;
begin
  CreateCanvas(Img, Cvs);
  LightGreen.Red := $AAAA; LightGreen.Green := $FFFF; LightGreen.Blue := $AAAA; LightGreen.Alpha := alphaOpaque;
  // S-curve closed shape: two continuous cubic Bezier segments
  Pts[0] := Point(30, 100);
  Pts[1] := Point(60, 20);
  Pts[2] := Point(90, 20);
  Pts[3] := Point(100, 100);
  Pts[4] := Point(110, 180);
  Pts[5] := Point(140, 180);
  Pts[6] := Point(170, 100);
  // Reference: filled in light gray
  Cvs.Pen.FPColor := colSilver;
  Cvs.Pen.Width := 1;
  Cvs.Brush.FPColor := colSilver;
  Cvs.Brush.Style := bsSolid;
  Cvs.PolyBezier(Pts, True, True);
  // Translated right 20px and down 20px, filled in light green
  Cvs.Translate(20, 20);
  Cvs.Pen.FPColor := colGreen;
  Cvs.Brush.FPColor := LightGreen;
  Cvs.PolyBezier(Pts, True, True);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_polybezier_filled.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: RadialPie ---------- }
{ Note: DoRadialPie is not implemented in TFPPixelCanvas (stub only).
  We demonstrate with an Ellipse instead, which shows the same bounding-rect
  transform principle. }
procedure DemoRadialPie;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs);
  // Use ellipse to demonstrate bounding-rect transform (same as RadialPie would use)
  Cvs.Pen.FPColor := colSilver;
  Cvs.Pen.Width := 1;
  Cvs.Brush.Style := bsClear;
  Cvs.Ellipse(30, 50, 170, 150);
  // Translated in red
  Cvs.Translate(0, -20);
  Cvs.Pen.FPColor := colRed;
  Cvs.Ellipse(30, 50, 170, 150);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_radialpie.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: GradientFill ---------- }
procedure DemoGradientFill;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs, 300, 200);
  // Reference gradient on the left
  Cvs.GradientFill(Rect(10, 40, 130, 160), colRed, colBlue, gdVertical);
  // Translated gradient on the right (same source rect, shifted by transform)
  Cvs.Translate(150, 0);
  Cvs.GradientFill(Rect(10, 40, 130, 160), colRed, colBlue, gdVertical);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_gradientfill.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: Scale ---------- }
procedure DemoScale;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs);
  // Reference rectangle
  Cvs.Pen.FPColor := colSilver;
  Cvs.Brush.Style := bsClear;
  Cvs.Rectangle(40, 60, 80, 100);
  // Scaled 2x from origin -> rectangle doubles in size and position
  Cvs.Scale(2, 2);
  Cvs.Pen.FPColor := colRed;
  Cvs.Rectangle(40, 60, 80, 100);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_scale.png');
  Cvs.Free;
  Img.Free;
end;

{ ---------- Demo: Combined Translate+Rotate+Scale ---------- }
procedure DemoCombined;
var
  Img: TFPMemoryImage;
  Cvs: TFPImageCanvas;
begin
  CreateCanvas(Img, Cvs);
  Cvs.Brush.Style := bsClear;
  // Reference square centered at (100,100)
  Cvs.Pen.FPColor := colSilver;
  Cvs.Rectangle(70, 70, 130, 130);
  // Rotate 45 degrees around center (100,100)
  Cvs.Translate(-100, -100);
  Cvs.Rotate(Pi / 4);
  Cvs.Translate(100, 100);
  Cvs.Pen.FPColor := colRed;
  Cvs.Rectangle(70, 70, 130, 130);
  Cvs.ResetTransform;
  // Rotate 45 degrees and scale 1.4x, both around center (100,100)
  Cvs.Translate(-100, -100);
  Cvs.Scale(1.4, 1.4);
  Cvs.Rotate(Pi / 4);
  Cvs.Translate(100, 100);
  Cvs.Pen.FPColor := colBlue;
  Cvs.Rectangle(70, 70, 130, 130);
  Cvs.ResetTransform;

  SaveImage(Img, 'transform_combined.png');
  Cvs.Free;
  Img.Free;
end;

{ ========== Main ========== }
begin
  InitWriter;
  try
    DemoLine;
    DemoMoveToLineTo;
    DemoRectangle;
    DemoFillRect;
    DemoEllipse;
    DemoPolygon;
    DemoPolyline;
    DemoFloodFill;
    DemoTextOut;
    DemoEllipseC;
    DemoCopyRect;
    DemoDraw;
    DemoPolyBezier;
    DemoPolyBezierFilled;
    DemoRadialPie;
    DemoGradientFill;
    DemoScale;
    DemoCombined;
    WriteLn('All transform demos saved successfully.');
  finally
    Writer.Free;
  end;
end.
