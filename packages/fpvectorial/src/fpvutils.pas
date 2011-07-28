{
fpvutils.pas

Vector graphics document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
         Pedro Sol Pegorini L de Lima
}
unit fpvutils;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Math,
  fpvectorial, fpimage;

type
  T10Strings = array[0..9] of shortstring;

// Color Conversion routines
function FPColorToRGBHexString(AColor: TFPColor): string;
function RGBToFPColor(AR, AG, AB: byte): TFPColor; inline;
// Other routine
function CanvasCoordsToFPVectorial(AY: Integer; AHeight: Integer): Integer; inline;
function CanvasTextPosToFPVectorial(AY: Integer; ACanvasHeight, ATextHeight: Integer): Integer;
function SeparateString(AString: string; ASeparator: char): T10Strings;
// Mathematical routines
procedure EllipticalArcToBezier(Xc, Yc, Rx, Ry, startAngle, endAngle: Double; var P1, P2, P3, P4: T3DPoint);
procedure CircularArcToBezier(Xc, Yc, R, startAngle, endAngle: Double; var P1, P2, P3, P4: T3DPoint);

implementation

{@@ This function is utilized by the SVG writer and some other places, so
    it shouldn't be changed.
}
function FPColorToRGBHexString(AColor: TFPColor): string;
begin
  Result := Format('%.2x%.2x%.2x', [AColor.Red shr 8, AColor.Green shr 8, AColor.Blue shr 8]);
end;

function RGBToFPColor(AR, AG, AB: byte): TFPColor; inline;
begin
  Result.Red := (AR shl 8) + AR;
  Result.Green := (AG shl 8) + AG;
  Result.Blue := (AB shl 8) + AB;
  Result.Alpha := $FFFF;
end;

{@@ Converts the coordinate system from a TCanvas to FPVectorial
    The basic difference is that the Y axis is positioned differently and
    points upwards in FPVectorial and downwards in TCanvas.
    The X axis doesn't change. The fix is trivial and requires only the Height of
    the Canvas as extra info.

    @param AHeight Should receive TCanvas.Height
}
function CanvasCoordsToFPVectorial(AY: Integer; AHeight: Integer): Integer; inline;
begin
  Result := AHeight - AY;
end;

{@@
  LCL Text is positioned based on the top-left corner of the text.
  Besides that, one also needs to take the general coordinate change into account too.

  @param ACanvasHeight Should receive TCanvas.Height
  @param ATextHeight   Should receive TFont.Size
}
function CanvasTextPosToFPVectorial(AY: Integer; ACanvasHeight, ATextHeight: Integer): Integer;
begin
  Result := CanvasCoordsToFPVectorial(AY, ACanvasHeight) - ATextHeight;
end;

{@@
  Reads a string and separates it in substring
  using ASeparator to delimite them.

  Limits:

  Number of substrings: 10 (indexed 0 to 9)
  Length of each substring: 255 (they are shortstrings)
}
function SeparateString(AString: string; ASeparator: char): T10Strings;
var
  i, CurrentPart: integer;
begin
  CurrentPart := 0;

  { Clears the result }
  for i := 0 to 9 do
    Result[i] := '';

  { Iterates througth the string, filling strings }
  for i := 1 to Length(AString) do
  begin
    if Copy(AString, i, 1) = ASeparator then
    begin
      Inc(CurrentPart);

      { Verifies if the string capacity wasn't exceeded }
      if CurrentPart > 9 then
        Exit;
    end
    else
      Result[CurrentPart] := Result[CurrentPart] + Copy(AString, i, 1);
  end;
end;

{ Considering a counter-clockwise arc, elliptical and alligned to the axises

  An elliptical Arc can be converted to
  the following Cubic Bezier control points:

  P1 = E(startAngle)            <- start point
  P2 = P1+alfa * dE(startAngle) <- control point
  P3 = P4−alfa * dE(endAngle)   <- control point
  P4 = E(endAngle)              <- end point

  source: http://www.spaceroots.org/documents/ellipse/elliptical-arc.pdf

  The equation of an elliptical arc is:

  X(t) = Xc + Rx * cos(t)
  Y(t) = Yc + Ry * sin(t)

  dX(t)/dt = - Rx * sin(t)
  dY(t)/dt = + Ry * cos(t)
}
procedure EllipticalArcToBezier(Xc, Yc, Rx, Ry, startAngle, endAngle: Double;
  var P1, P2, P3, P4: T3DPoint);
var
  halfLength, arcLength, alfa: Double;
begin
  arcLength := endAngle - startAngle;
  halfLength := (endAngle - startAngle) / 2;
  alfa := sin(arcLength) * (Sqrt(4 + 3*sqr(tan(halfLength))) - 1) / 3;

  // Start point
  P1.X := Xc + Rx * cos(startAngle);
  P1.Y := Yc + Ry * sin(startAngle);

  // End point
  P4.X := Xc + Rx * cos(endAngle);
  P4.Y := Yc + Ry * sin(endAngle);

  // Control points
  P2.X := P1.X + alfa * -1 * Rx * sin(startAngle);
  P2.Y := P1.Y + alfa * Ry * cos(startAngle);

  P3.X := P4.X - alfa * -1 * Rx * sin(endAngle);
  P3.Y := P4.Y - alfa * Ry * cos(endAngle);
end;

procedure CircularArcToBezier(Xc, Yc, R, startAngle, endAngle: Double; var P1,
  P2, P3, P4: T3DPoint);
begin
  EllipticalArcToBezier(Xc, Yc, R, R, startAngle, endAngle, P1, P2, P3, P4);
end;

end.

