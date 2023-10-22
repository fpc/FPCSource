unit utmathvectorbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Types, system.math.vectors;

const
  c45 = Sqrt(2)/2;  // cosine/sine 45°
  c60 = Sqrt(3)/2;  // cosine 60°
  c30 = 1/2;        // cosine 30°

  s30 = C60; // sine 30°
  s45 = c45; // sine 45°
  s60 = c30; // sine 60°



type
  { TCMathVectorsBase }

  TCMathVectorsBase = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    class Procedure AssertEquals(const Msg : String; aExpected,aActual : TVector); overload;
    class Procedure AssertEquals(const Msg : String; aExpected,aActual : TPoint3D); overload;
    class Procedure AssertEquals(const Msg : String; aExpected,aActual : TPointF); overload;
    class Procedure AssertEquals(const Msg : String; aExpected,aActual : TVector3D); overload;
    class Procedure AssertVector3D(const Msg : String; aExpectedX,aExpectedY,aExpectedZ,aExpectedW: Single; aActual : TVector3D);
    class Procedure AssertVector(const Msg : String; aExpectedX,aExpectedY,aExpectedW: Single; aActual : TVector);
    class Procedure AssertPoint3D(const Msg : String; aExpectedX,aExpectedY,aExpectedZ: Single; aActual : TPoint3D);
    class procedure AssertMatrix(Const Msg : String; m11, m12, m13,  m21, m22, m23,  m31, m32, m33 : Single; aActual : TMatrix);
    class procedure AssertMatrix(Const Msg : String; aExpected : Array of single; aActual : TMatrix);
    class procedure AssertMatrix(Const Msg : String; aExpected, aActual : TMatrix);
    class procedure AssertMatrix3D(Const Msg : String; m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44 : Single; aActual : TMatrix3D);
    class procedure AssertMatrix3D(Const Msg : String; aExpected : Array of single; aActual : TMatrix3D);
    class procedure AssertMatrix3D(Const Msg : String; aExpected, aActual : TMatrix3D);
    class Procedure AssertPointF(const Msg : String; aExpectedX,aExpectedY: Single; aActual : TPointF);
    class Procedure AssertQuaternion(const Msg : String; aExpectedReal,aExpectedImagX,aExpectedImagY,aExpectedImagZ: Single; aActual : TQuaternion3D);
    class Procedure AssertQuaternion(const Msg : String; aExpectedReal: Single; aExpectedImag : TPoint3D; aActual : TQuaternion3D);
  end;



implementation

procedure TCMathVectorsBase.SetUp;
begin

end;

procedure TCMathVectorsBase.TearDown;
begin

end;

class procedure TCMathVectorsBase.AssertEquals(const Msg: String; aExpected,
  aActual: TVector);
begin
  AssertEquals(Msg+' X',aExpected.X,aActual.X,TEpsilon.Vector);
  AssertEquals(Msg+' Y',aExpected.Y,aActual.Y,TEpsilon.Vector);
  AssertEquals(Msg+' W',aExpected.W,aActual.W,TEpsilon.Vector);
end;

class procedure TCMathVectorsBase.AssertEquals(const Msg: String; aExpected,
  aActual: TPoint3D);
begin
  AssertEquals(Msg+' X',aExpected.X,aActual.X,TEpsilon.Vector);
  AssertEquals(Msg+' Y',aExpected.Y,aActual.Y,TEpsilon.Vector);
  AssertEquals(Msg+' Z',aExpected.Z,aActual.Z,TEpsilon.Vector);
end;

class procedure TCMathVectorsBase.AssertEquals(const Msg: String; aExpected,
  aActual: TPointF);
begin
  AssertEquals(Msg+' X',aExpected.X,aActual.X,TEpsilon.Vector);
  AssertEquals(Msg+' Y',aExpected.Y,aActual.Y,TEpsilon.Vector);

end;

class procedure TCMathVectorsBase.AssertEquals(const Msg: String; aExpected,
  aActual: TVector3D);
begin
  AssertEquals(Msg+' X',aExpected.X,aActual.X,TEpsilon.Vector);
  AssertEquals(Msg+' Y',aExpected.Y,aActual.Y,TEpsilon.Vector);
  AssertEquals(Msg+' Z',aExpected.Z,aActual.Z,TEpsilon.Vector);
  AssertEquals(Msg+' W',aExpected.W,aActual.W,TEpsilon.Vector);
end;

class procedure TCMathVectorsBase.AssertVector3D(const Msg: String; aExpectedX,
  aExpectedY, aExpectedZ, aExpectedW: Single; aActual: TVector3D);
begin
  AssertEquals(Msg+' X',aExpectedX,aActual.X,TEpsilon.Vector);
  AssertEquals(Msg+' Y',aExpectedY,aActual.Y,TEpsilon.Vector);
  AssertEquals(Msg+' Z',aExpectedZ,aActual.Z,TEpsilon.Vector);
  AssertEquals(Msg+' W',aExpectedW,aActual.W,TEpsilon.Vector);
end;

class procedure TCMathVectorsBase.AssertVector(const Msg: String; aExpectedX,
  aExpectedY, aExpectedW: Single; aActual: TVector);
begin
  AssertEquals(Msg+' X',aExpectedX,aActual.X,TEpsilon.Vector);
  AssertEquals(Msg+' Y',aExpectedY,aActual.Y,TEpsilon.Vector);
  AssertEquals(Msg+' W',aExpectedW,aActual.W,TEpsilon.Vector);
end;

class procedure TCMathVectorsBase.AssertPoint3D(const Msg: String; aExpectedX, aExpectedY, aExpectedZ: Single; aActual: TPoint3D);
begin
  AssertEquals(Msg+' X',aExpectedX,aActual.X,TEpsilon.Vector);
  AssertEquals(Msg+' Y',aExpectedY,aActual.Y,TEpsilon.Vector);
  AssertEquals(Msg+' Z',aExpectedZ,aActual.Z,TEpsilon.Vector);
end;

class procedure TCMathVectorsBase.AssertMatrix(const Msg: String; m11, m12, m13, m21, m22, m23, m31, m32, m33: Single;
  aActual: TMatrix);
begin
  AssertEquals(Msg+' m11',m11,aActual.m11,TEpsilon.Vector);
  AssertEquals(Msg+' m12',m12,aActual.m12,TEpsilon.Vector);
  AssertEquals(Msg+' m13',m13,aActual.m13,TEpsilon.Vector);

  AssertEquals(Msg+' m21',m21,aActual.m21,TEpsilon.Vector);
  AssertEquals(Msg+' m22',m22,aActual.m22,TEpsilon.Vector);
  AssertEquals(Msg+' m23',m23,aActual.m23,TEpsilon.Vector);

  AssertEquals(Msg+' m31',m31,aActual.m31,TEpsilon.Vector);
  AssertEquals(Msg+' m32',m32,aActual.m32,TEpsilon.Vector);
  AssertEquals(Msg+' m33',m33,aActual.m33,TEpsilon.Vector);
end;

class procedure TCMathVectorsBase.AssertMatrix(const Msg: String; aExpected: array of single; aActual: TMatrix);
begin
  AssertEquals(Msg+' number of elements',9,Length(aExpected));
  AssertMatrix(Msg,aExpected[0],aExpected[1],aExpected[2],
                   aExpected[3],aExpected[4],aExpected[5],
                   aExpected[6],aExpected[7],aExpected[8],aActual);
end;

class procedure TCMathVectorsBase.AssertMatrix(const Msg: String; aExpected, aActual: TMatrix);
begin
  With aExpected do
    AssertMatrix(Msg,m11, m12, m13, m21, m22, m23, m31, m32, m33,aActual);
end;

class procedure TCMathVectorsBase.AssertMatrix3D(const Msg: String; m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33,
  m34, m41, m42, m43, m44 : Single; aActual: TMatrix3D);
begin
  AssertEquals(Msg+' m11',m11,aActual.m11,TEpsilon.Vector);
  AssertEquals(Msg+' m12',m12,aActual.m12,TEpsilon.Vector);
  AssertEquals(Msg+' m13',m13,aActual.m13,TEpsilon.Vector);
  AssertEquals(Msg+' m14',m14,aActual.m14,TEpsilon.Vector);

  AssertEquals(Msg+' m21',m21,aActual.m21,TEpsilon.Vector);
  AssertEquals(Msg+' m22',m22,aActual.m22,TEpsilon.Vector);
  AssertEquals(Msg+' m23',m23,aActual.m23,TEpsilon.Vector);
  AssertEquals(Msg+' m24',m24,aActual.m24,TEpsilon.Vector);

  AssertEquals(Msg+' m31',m31,aActual.m31,TEpsilon.Vector);
  AssertEquals(Msg+' m32',m32,aActual.m32,TEpsilon.Vector);
  AssertEquals(Msg+' m33',m33,aActual.m33,TEpsilon.Vector);
  AssertEquals(Msg+' m34',m34,aActual.m34,TEpsilon.Vector);

  AssertEquals(Msg+' m41',m41,aActual.m41,TEpsilon.Vector);
  AssertEquals(Msg+' m42',m42,aActual.m42,TEpsilon.Vector);
  AssertEquals(Msg+' m43',m43,aActual.m43,TEpsilon.Vector);
  AssertEquals(Msg+' m44',m44,aActual.m44,TEpsilon.Vector);

end;

class procedure TCMathVectorsBase.AssertMatrix3D(const Msg: String; aExpected: array of single; aActual: TMatrix3D);
begin
  AssertEquals(Msg+' number of elements',16,Length(aExpected));
  AssertMatrix3D(Msg,aExpected[0],aExpected[1],aExpected[2],aExpected[3],
                   aExpected[4],aExpected[5],aExpected[6],aExpected[7],
                   aExpected[8],aExpected[9], aExpected[10], aExpected[11],
                   aExpected[12], aExpected[13], aExpected[14], aExpected[15], aActual);
end;

class procedure TCMathVectorsBase.AssertMatrix3D(const Msg: String; aExpected, aActual: TMatrix3D);
begin
  With aExpected do
    AssertMatrix3D(Msg,m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44, aActual);
end;

class procedure TCMathVectorsBase.AssertPointF(const Msg: String; aExpectedX, aExpectedY: Single; aActual: TPointF);
begin
  AssertEquals(Msg+' X',aExpectedX,aActual.X,TEpsilon.Vector);
  AssertEquals(Msg+' Y',aExpectedY,aActual.Y,TEpsilon.Vector);
end;

class procedure TCMathVectorsBase.AssertQuaternion(const Msg: String; aExpectedReal, aExpectedImagX, aExpectedImagY,
  aExpectedImagZ: Single; aActual: TQuaternion3D);
begin
  AssertEquals(Msg+' RealPart',aExpectedReal,aActual.RealPart,TEpsilon.Vector);
  AssertEquals(Msg+' ImagPart.X',aExpectedImagX,aActual.ImagPart.X,TEpsilon.Vector);
  AssertEquals(Msg+' ImagPart.Y',aExpectedImagY,aActual.ImagPart.Y,TEpsilon.Vector);
  AssertEquals(Msg+' ImagPart.Z',aExpectedImagZ,aActual.ImagPart.Z,TEpsilon.Vector);
end;

class procedure TCMathVectorsBase.AssertQuaternion(const Msg: String; aExpectedReal: Single; aExpectedImag: TPoint3D;
  aActual: TQuaternion3D);
begin
  AssertEquals(Msg+' RealPart',aExpectedReal,aActual.RealPart,TEpsilon.Vector);
  AssertEquals(Msg+' ImagPart.X',aExpectedImag.X,aActual.ImagPart.X,TEpsilon.Vector);
  AssertEquals(Msg+' ImagPart.Y',aExpectedImag.Y,aActual.ImagPart.Y,TEpsilon.Vector);
  AssertEquals(Msg+' ImagPart.Z',aExpectedImag.Z,aActual.ImagPart.Z,TEpsilon.Vector);
end;

end.

