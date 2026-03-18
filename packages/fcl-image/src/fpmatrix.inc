{%MainUnit fpcanvas.pp}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TFPCanvasMatrix implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ TFPCanvasMatrix }

class function TFPCanvasMatrix.Identity: TFPCanvasMatrix; static;
begin
  Result._00 := 1.0;
  Result._01 := 0.0;
  Result._10 := 0.0;
  Result._11 := 1.0;
  Result._20 := 0.0;
  Result._21 := 0.0;
end;

class function TFPCanvasMatrix.CreateTranslation(DX, DY: Double): TFPCanvasMatrix; static;
begin
  Result := Identity;
  Result._20 := DX;
  Result._21 := DY;
end;

class function TFPCanvasMatrix.CreateScale(SX, SY: Double): TFPCanvasMatrix; static;
begin
  Result := Identity;
  Result._00 := SX;
  Result._11 := SY;
end;

class function TFPCanvasMatrix.CreateRotation(ARadians: Double): TFPCanvasMatrix; static;
var
  S, C: Double;
begin
  S := Sin(ARadians);
  C := Cos(ARadians);
  Result._00 := C;
  Result._01 := S;
  Result._10 := -S;
  Result._11 := C;
  Result._20 := 0.0;
  Result._21 := 0.0;
end;

function TFPCanvasMatrix.Transform(const APoint: TPoint): TPoint;
begin
  Result.X := Round(APoint.X * _00 + APoint.Y * _10 + _20);
  Result.Y := Round(APoint.X * _01 + APoint.Y * _11 + _21);
end;

function TFPCanvasMatrix.Transform(X, Y: Integer): TPoint;
begin
  Result.X := Round(X * _00 + Y * _10 + _20);
  Result.Y := Round(X * _01 + Y * _11 + _21);
end;

function TFPCanvasMatrix.Multiply(const Other: TFPCanvasMatrix): TFPCanvasMatrix;
begin
  Result._00 := _00 * Other._00 + _01 * Other._10;
  Result._01 := _00 * Other._01 + _01 * Other._11;
  Result._10 := _10 * Other._00 + _11 * Other._10;
  Result._11 := _10 * Other._01 + _11 * Other._11;
  Result._20 := _20 * Other._00 + _21 * Other._10 + Other._20;
  Result._21 := _20 * Other._01 + _21 * Other._11 + Other._21;
end;
