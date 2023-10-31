{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by Michael Van Canneyt
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This unit contains modified version of some algorithms from the GLScene
  GLS.VectorGeometry.pas.
  The GLScene unit is covered under the MPL 1.0. license.
}

unit System.Math.Vectors;

{$mode objfpc}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.Math;
{$ELSE}
  Types, Math;
{$ENDIF}


type
  TEpsilon = record
  const
    Matrix = 1E-5;
    Vector = 1E-4;
    Scale = 1E-4;
    FontSize = 1E-2;
    Position = 1E-3;
    Angle = 1E-4;
  end;

  TVector3DType = array [0..3] of Single;

  TVectorArray = array [0..2] of Single;

  TPolygon = array of TPointF;

  TCubicBezier = array [0..3] of TPointF;

  tagVECTOR = record
    case Integer of
      0: (V: TVectorArray;);
      1: (X: Single;
          Y: Single;
          W: Single;);
  end;

  PVector = ^TVector;

  { TVector }

  TVector = record
    class function Create(const aX, aY: Single; const aW: Single = 1.0): TVector; overload; static; inline;
    class function Create(const aPoint: TPointF): TVector; overload; static; inline;
    class function Zero: TVector; inline; static;

    class operator +(const aVector1, aVector2: TVector): TVector;
    class operator /(const aVector: TVector; const aFactor: Single): TVector;
    class operator =(const aVector1, aVector2: TVector): Boolean; inline;
    class operator Explicit(const aPoint: TVector): TPointF;
    class operator :=(const aPoint: TPointF): TVector; inline;
    class operator :=(const aPoint: TVector): TPointF; inline;
    class operator :=(const aSize: TSizeF): TVector; inline;
    class operator *(const aFactor: Single; const aVector: TVector): TVector; inline;
    class operator *(const aVector: TVector; const aFactor: Single): TVector;
    class operator <>(const aVector1, aVector2: TVector): Boolean; inline;
    class operator -(const aVector1, aVector2: TVector): TVector;


    function Length: Single;
    function Normalize: TVector;
    function CrossProduct(const aVector: TVector): TVector;
    function DotProduct(const aVector: TVector): Single;
    function MidVector(const aVector: TVector): TVector;

    function ToPointF: TPointF; inline;
    Function ToString: String;

    case Integer of
      0: (V: TVectorArray;);
      1: (X: Single;
          Y: Single;
          W: Single;);
  end;

  { TPoint3D }

  TPoint3D = record
  Public
    type
      TPoint3DArray = array [0..2] of Single;
    class function Create(const aX, aY, aZ: Single): TPoint3D; overload; static; inline;
    class function Create(const P: TVector3DType): TPoint3D; overload; static; inline;
    class function Create(const aPoint: TPointF; const aZ: Single = 0.0): TPoint3D; overload; static; inline;
    class function Zero: TPoint3D; inline; static;

    class operator +(const aPoint1, aPoint2: TPoint3D): TPoint3D;
    class operator /(const aPoint: TPoint3D; const aFactor: Single): TPoint3D;
    class operator =(const aPoint1, aPoint2: TPoint3D): Boolean; inline;
    class operator *(const aFactor: Single; const aPoint: TPoint3D): TPoint3D; inline;
    class operator *(const aPoint: TPoint3D; const aFactor: Single): TPoint3D; inline;
    class operator *(const aPoint1, aPoint2: TPoint3D): TPoint3D;
    class operator -(const aPoint: TPoint3D): TPoint3D;
    class operator <>(const aPoint1, aPoint2: TPoint3D): Boolean; inline;
    class operator -(const aPoint1, aPoint2: TPoint3D): TPoint3D;

    function AngleCosine(const aPoint: TPoint3D): Single;
    function CrossProduct(const aPoint: TPoint3D): TPoint3D;
    function Distance(const aPoint: TPoint3D): Single;
    function DotProduct(const aPoint: TPoint3D): Single;
    function EqualsTo(const aPoint: TPoint3D; const aEpsilon: Single = 0): Boolean; inline;
    function Length: Single; inline;
    function MidPoint(const aPoint: TPoint3D): TPoint3D; inline;
    function Normalize: TPoint3D;
    procedure Offset(const aDelta: TPoint3D); overload; inline;
    procedure Offset(const aDeltaX, aDeltaY, aDeltaZ: Single); overload; inline;
    function Reflect(const aPoint: TPoint3D): TPoint3D; inline;
    function Rotate(const aAxis: TPoint3D; const aAngle: Single): TPoint3D; inline;
    Function ToString: String;
  Public
    case Integer of
      0: (V: TPoint3DArray;);
      1: (X: Single;
          Y: Single;
          Z: Single;);
  end;
  PPoint3D = ^TPoint3D;

  tagVECTOR3D = record
    case Integer of
      0: (V: TVector3DType;);
      1: (X: Single;
          Y: Single;
          Z: Single;
          W: Single;);
  end;


  TMatrixArray = array [0..2] of TVector;
  TMaxtrixArrayBase = array[0..2] of tagVECTOR;

  { TMatrix }

  TMatrix = record
  public
    class function CreateRotation(const aAngle: Single): TMatrix; static;
    class function CreateScaling(const aScaleX, aScaleY: Single): TMatrix; static;
    class function CreateTranslation(const aDeltaX, aDeltaY: Single): TMatrix; static;

    class operator =(const RightMatrix, LeftMatrix: TMatrix): Boolean;
    class operator *(const aMatrix1, aMatrix2: TMatrix): TMatrix;
    class operator *(const aPoint: TPointF; const aMatrix: TMatrix): TPointF;
    class operator *(const aVector: TVector; const aMatrix: TMatrix): TVector;
    class operator *(const aVector: TPoint3D; const aMatrix: TMatrix): TPoint3D;
    class operator *(const aFactor: Single; const aMatrix: TMatrix): TMatrix;
    class operator *(const aMatrix: TMatrix; const aFactor: Single): TMatrix;
    class operator /(const aMatrix: TMatrix; const aFactor: Single): TMatrix;

    function Adjoint: TMatrix;
    function Determinant: Single;
    function EqualsTo(const aMatrix: TMatrix; const Epsilon: Single = TEpsilon.Matrix): Boolean;
    function ExtractScale: TPointF;
    function Inverse: TMatrix;
    function Scale(const aFactor: Single): TMatrix;
    Function ToString(Multiline : Boolean = true) : String;
  Public
    case Integer of
      0: (M: TMatrixArray;);
      1: (m11, m12, m13: Single;
          m21, m22, m23: Single;
          m31, m32, m33: Single);
  end;

  TMatrixConstants = record helper for TMatrix
    const Identity: TMatrix = (m11: 1; m12: 0; m13: 0;
                               m21: 0; m22: 1; m23: 0;
                               m31: 0; m32: 0; m33: 1);
  end;

  PVector3D = ^TVector3D;

  { TVector3D }

  TVector3D = record
  Public
    class function Create(const aX, aY, aZ: Single; const aW: Single = 1.0): TVector3D; overload; static; inline;
    class function Create(const aPoint: TPoint3D; const aW: Single = 1.0): TVector3D; overload; static; inline;
    class function Zero: TVector3D; inline; static;

    class operator +(const aVector1, aVector2: TVector3D): TVector3D;
    class operator /(const aVector: TVector3D; const aFactor: Single): TVector3D;
    class operator =(const aVector1, aVector2: TVector3D): Boolean;
    class operator explicit(const aVector: TVector3D): TPoint3D;
    class operator :=(const aPoint: TPoint3D): TVector3D;
    class operator :=(const aVector: TVector3D): TPoint3D; inline;
    class operator *(const aFactor: Single; const aVector: TVector3D): TVector3D; inline;
    class operator *(const aVector: TVector3D; const aFactor: Single): TVector3D; inline;
    class operator *(const aVector1, aVector2: TVector3D): TVector3D;
    class operator -(const aVector: TVector3D): TVector3D;
    class operator <>(const aVector1, aVector2: TVector3D): Boolean;
    class operator -(const aVector1, aVector2: TVector3D): TVector3D;


    function AngleCosine(const aVector: TVector3D): Single;
    function CrossProduct(const aVector: TVector3D): TVector3D;
    function Distance(const aVector: TVector3D): Single;
    function DotProduct(const aVector: TVector3D): Single;
    function EqualsTo(const aVector: TVector3D; const Epsilon: Single = 0): Boolean; inline;
    function Length: Single;
    function MidVector(const aVector: TVector3D): TVector3D;
    function Normalize: TVector3D;
    procedure Offset(const aDelta: TPoint3D); overload; inline;
    procedure Offset(const aDeltaX, aDeltaY, aDeltaZ: Single); overload; inline;
    function Reflect(const aVector: TVector3D): TVector3D; inline;
    function Rotate(const aAxis: TPoint3D; const aAngle: Single): TVector3D; inline;
    function ToPoint3D(const aTransform: Boolean = False): TPoint3D;
  Public
    case Integer of
      0: (V: TVector3DType;);
      1: (X: Single;
          Y: Single;
          Z: Single;
          W: Single;);
  end;

  TVector3DArray = array [0..2] of TVector3D;
  TVector3DArrayBase = array[0..2] of tagVECTOR3D;

  TMatrix3DType = array [0..3] of TVector3D;
  TMatrix3DTypeBase = array[0..3] of tagVECTOR3D;

  { TMatrix3D }

  TMatrix3D = record
  public
    constructor Create(const aM11, aM12, aM13, aM14,
                             aM21, aM22, aM23, aM24,
                             aM31, aM32, aM33, aM34,
                             aM41, aM42, aM43, aM44: Single); overload;
     { Transposed:
       0 4 8  12
       1 5 9  13
       2 6 10 14
       3 7 11 15 }
    constructor Create(const aArray: TSingleDynArray); overload;

    class Function Zero : TMatrix3D; static;

    class function CreateLookAtDirLH(const aSource, aDirection, aCeiling: TPoint3D): TMatrix3D; static;
    class function CreateLookAtDirRH(const aSource, aDirection, aCeiling: TPoint3D): TMatrix3D; static;
    class function CreateLookAtLH(const aSource, aTarget, aCeiling: TPoint3D): TMatrix3D; static;
    class function CreateLookAtRH(const aSource, aTarget, aCeiling: TPoint3D): TMatrix3D; static;

    class function CreateOrthoLH(const aWidth, aHeight, aZNear, aZFar: Single): TMatrix3D; static;
    class function CreateOrthoOffCenterLH(const aLeft, aTop, aRight, aBottom, aZNear, aZFar: Single): TMatrix3D; static;
    class function CreateOrthoOffCenterRH(const aLeft, aTop, aRight, aBottom, aZNear, aZFar: Single): TMatrix3D; static;
    class function CreateOrthoRH(const aWidth, aHeight, aZNear, aZFar: Single): TMatrix3D; static;

    class function CreatePerspectiveFovLH(const aFOV, aAspect, aZNear, aZFar: Single; const aHorizontalFOV: Boolean = False): TMatrix3D; static;
    class function CreatePerspectiveFovRH(const aFOV, aAspect, aZNear, aZFar: Single; const aHorizontalFOV: Boolean = False): TMatrix3D; static;

    class function CreateRotation(const aAxis: TPoint3D; const aAngle: Single): TMatrix3D; static;
    class function CreateRotationHeadingPitchBank(const aHeading, aPitch, aBank: Single): TMatrix3D; static;
    class function CreateRotationX(const aAngle: Single): TMatrix3D; static;
    class function CreateRotationY(const aAngle: Single): TMatrix3D; static;
    class function CreateRotationYawPitchRoll(const aYaw, aPitch, aRoll: Single): TMatrix3D; static;
    class function CreateRotationZ(const aAngle: Single): TMatrix3D; static;
    class function CreateScaling(const aScale: TPoint3D): TMatrix3D; static;
    class function CreateTranslation(const aTranslation: TPoint3D): TMatrix3D; static;

    class operator *(const aPoint: TPoint3D; const aMatrix: TMatrix3D): TPoint3D;
    class operator *(const aMatrix1, aMatrix2: TMatrix3D): TMatrix3D;
    class operator *(const aVector: TVector3D; const aMatrix: TMatrix3D): TVector3D;
    class operator *(const aFactor: single; const aMatrix: TMatrix3D) : TMatrix3D;
    class operator *(const aMatrix: TMatrix3D; const aFactor: single) : TMatrix3D;
    class operator /(const aMatrix: TMatrix3D; const aFactor: single) : TMatrix3D;

    function Adjoint: TMatrix3D;
    function Determinant: Single;
    function EyePosition: TPoint3D;
    function Inverse: TMatrix3D;
    function Scale(const aFactor : Single) : TMatrix3D;
    function ToMatrix: TMatrix;
    function Transpose: TMatrix3D;
    Function ToString(Multiline : Boolean = true) : String;

    case Integer of
      0: (M: TMatrix3DType;);
      // m[row,col]
      1: (m11, m12, m13, m14: Single;
          m21, m22, m23, m24: Single;
          m31, m32, m33, m34: Single;
          m41, m42, m43, m44: Single);
  end;

  TMatrix3DConstants = record helper for TMatrix3D
    const Identity: TMatrix3D = (m11: 1; m12: 0; m13: 0; m14: 0;
                                 m21: 0; m22: 1; m23: 0; m24: 0;
                                 m31: 0; m32: 0; m33: 1; m34: 0;
                                 m41: 0; m42: 0; m43: 0; m44: 1;);
  end;

  { TQuaternion3D }

  TQuaternion3D = record
  Public
    constructor Create(const aAxis: TPoint3D; const aAngle: Single); overload;
    constructor Create(const P1,P2: TPoint3D); overload;
    constructor Create(const aYaw, aPitch, aRoll: Single); overload;
    constructor Create(const aMatrix: TMatrix3D); overload;

    class operator :=(const aQuaternion: TQuaternion3D): TMatrix3D;
    class operator *(const aQuaternion1, aQuaternion2: TQuaternion3D): TQuaternion3D;

    function Length: Single;
    function Normalize: TQuaternion3D;
    function ToString : string;
  Public
    case Integer of
      0: (V: TVector3DType;);
      1: (ImagPart: TPoint3D;
          RealPart: Single;);
  end;

  TQuaternion3DConstants = record helper for TQuaternion3D
    const Identity: TQuaternion3D = (ImagPart: (X: 0; Y: 0; Z: 0);
                                     RealPart: 1);
  end;

const
  DefaultVectorWidth = Single(1.0);
  NullVector3D: TVector3D = (X: 0; Y: 0; Z: 0; W: DefaultVectorWidth);
  NullPoint3D: TPoint3D = (X: 0; Y: 0; Z: 0);

function Vector(const X, Y: Single; const W: Single = DefaultVectorWidth): TVector; overload;
function Vector(const P: TPointF; const W: Single = DefaultVectorWidth): TVector; overload;
function Vector3D(const X, Y, Z: Single; const W: Single = DefaultVectorWidth): TVector3D; overload;
function Vector3D(const P: TPoint3D; const W: Single = DefaultVectorWidth): TVector3D; overload;
function Point3D(const X, Y, Z: Single): TPoint3D; overload;
function Point3D(const aVector3D: TVector3D; const aTransform: Boolean = False): TPoint3D; overload;
function PointF(const V: TVector): TPointF; inline; overload;

implementation

Function Display(D : Single) : String;

begin
  WriteStr(Result,D:7:4);
end;

{ ---------------------------------------------------------------------
  TVector
  ---------------------------------------------------------------------}

class function TVector.Create(const aX, aY: Single; const aW: Single): TVector;

begin
  Result.X:=aX;
  Result.Y:=aY;
  Result.W:=aW;
end;


class function TVector.Create(const aPoint: TPointF): TVector;

begin
  Result.X:=aPoint.X;
  Result.Y:=aPoint.Y;
  Result.W:=DefaultVectorWidth;
end;


class function TVector.Zero: TVector;

begin
  Result.X:=0;
  Result.Y:=0;
  Result.W:=0;
end;


class operator TVector.+(const aVector1, aVector2: TVector): TVector;

begin
  Result.X:=aVector1.X+aVector2.X;
  Result.Y:=aVector1.Y+aVector2.Y;
  Result.W:=aVector1.W+aVector2.W;
end;


class operator TVector./(const aVector: TVector; const aFactor: Single): TVector;

begin
  Result.X:=aVector.X/aFactor;
  Result.Y:=aVector.Y/aFactor;
  Result.W:=aVector.W/aFactor;
end;


class operator TVector.=(const aVector1, aVector2: TVector): Boolean;

begin
  Result:=SameValue(aVector1.X,aVector2.X, TEpsilon.Vector)
          and SameValue(aVector1.Y,aVector2.Y, TEpsilon.Vector)
          and SameValue(aVector1.W,aVector2.W, TEpsilon.Vector);
end;


class operator TVector.Explicit(const aPoint: TVector): TPointF;

var
  S : Single;

begin
  S:=1;
  if not SameValue(APoint.W,0,TEpsilon.Vector) then
    S:=1/aPoint.W;
  Result.X := APoint.X * S;
  Result.Y := APoint.Y * S;
end;

class operator TVector.:=(const aPoint: TPointF): TVector;

begin
  Result:=TVector.Create(aPoint);
end;


class operator TVector.:=(const aPoint: TVector): TPointF;

begin
  Result:=TPointF(aPoint);
end;

class operator TVector.:=(const aSize: TSizeF): TVector;

begin
  Result:=TVector.Create(TPointF(aSize));
end;


class operator TVector.*(const aFactor: Single; const aVector: TVector): TVector;

begin
  Result.X:=aFactor*aVector.X;
  Result.Y:=aFactor*aVector.Y;
  Result.W:=aFactor*aVector.W;
end;


class operator TVector.*(const aVector: TVector; const aFactor: Single): TVector;
begin
  Result.X:=aVector.X*aFactor;
  Result.Y:=aVector.Y*aFactor;
  Result.W:=aVector.W*aFactor;
end;


class operator TVector.<>(const aVector1, aVector2: TVector): Boolean;

begin
  Result:=Not (aVector1=aVector2);
end;


class operator TVector.-(const aVector1, aVector2: TVector): TVector;

begin
  Result.X:=aVector1.X-aVector2.X;
  Result.Y:=aVector1.Y-aVector2.Y;
  Result.W:=aVector1.W-aVector2.W; // [Spock:] Highly illogical...
end;


function TVector.Length: Single;
begin
  Result:=Sqrt(DotProduct(Self));
end;


function TVector.Normalize: TVector;

var
  S: Single;

begin
  S:=Length;
  if SameValue(S,0,TEpsilon.Vector) then
    S:=1
  else
    S:=1/S;
  Result:=S*Self;
end;


function TVector.CrossProduct(const aVector: TVector): TVector;

begin
  Result.X:=(Y*aVector.W)-(W*aVector.Y);
  Result.Y:=(W*aVector.X)-(X*aVector.W);
  Result.W:=(X*aVector.Y)-(Y*aVector.X);
end;


function TVector.DotProduct(const aVector: TVector): Single;

begin
  Result:=(X*aVector.X)+(Y*aVector.Y)+(W*aVector.W);
end;


function TVector.MidVector(const aVector: TVector): TVector;

begin
  Result:=(Self+aVector)/2;
end;


function TVector.ToPointF: TPointF;

begin
  Result:=TPointF(Self);
end;


Function TVector.ToString: String;

begin
  WriteStr(Result,'(',X:5:2,',',Y:5:2,' W:',W:5:2,')');
end;


{ ---------------------------------------------------------------------
  TPoint3D
  ---------------------------------------------------------------------}


class function TPoint3D.Create(const aX, aY, aZ: Single): TPoint3D;

begin
  Result.X:=aX;
  Result.Y:=aY;
  Result.Z:=aZ;
end;


class function TPoint3D.Create(const P: TVector3DType): TPoint3D;

begin
  Result.X:=P[0];
  Result.Y:=P[1];
  Result.Z:=P[2];
end;


class function TPoint3D.Create(const aPoint: TPointF; const aZ: Single): TPoint3D;

begin
  Result.X:=aPoint.X;
  Result.Y:=aPoint.Y;
  Result.Z:=aZ;
end;


class function TPoint3D.Zero: TPoint3D;

begin
  Result.X:=0;
  Result.Y:=0;
  Result.Z:=0;
end;


class operator TPoint3D.+(const aPoint1, aPoint2: TPoint3D): TPoint3D;

begin
  Result.X:=aPoint1.X+aPoint2.X;
  Result.Y:=aPoint1.Y+aPoint2.Y;
  Result.Z:=aPoint1.Z+aPoint2.Z;
end;


class operator TPoint3D./(const aPoint: TPoint3D; const aFactor: Single): TPoint3D;

begin
  Result.X:=aPoint.X/aFactor;
  Result.Y:=aPoint.Y/aFactor;
  Result.Z:=aPoint.Z/aFactor;
end;


class operator TPoint3D.=(const aPoint1, aPoint2: TPoint3D): Boolean;

begin
  Result:=SameValue(aPoint1.X,aPoint2.X, TEpsilon.Vector)
          and SameValue(aPoint1.Y,aPoint2.Y, TEpsilon.Vector)
          and SameValue(aPoint1.Z,aPoint2.Z, TEpsilon.Vector);
end;


class operator TPoint3D.*(const aFactor: Single; const aPoint: TPoint3D): TPoint3D;

begin
  Result.X:=aFactor*aPoint.X;
  Result.Y:=aFactor*aPoint.Y;
  Result.Z:=aFactor*aPoint.Z;
end;


class operator TPoint3D.*(const aPoint: TPoint3D; const aFactor: Single): TPoint3D;

begin
  Result.X:=aPoint.X*aFactor;
  Result.Y:=aPoint.Y*aFactor;
  Result.Z:=aPoint.Z*aFactor;
end;


class operator TPoint3D.*(const aPoint1, aPoint2: TPoint3D): TPoint3D;

begin
  Result.X:=aPoint1.X*aPoint2.X;
  Result.Y:=aPoint1.Y*aPoint2.Y;
  Result.Z:=aPoint1.Z*aPoint2.Z;
end;


class operator TPoint3D.-(const aPoint: TPoint3D): TPoint3D;

begin
  Result.X:=-aPoint.X;
  Result.Y:=-aPoint.Y;
  Result.Z:=-aPoint.Z;
end;


class operator TPoint3D.<>(const aPoint1, aPoint2: TPoint3D): Boolean;

begin
  Result:=Not(aPoint1=aPoint2);
end;


class operator TPoint3D.-(const aPoint1, aPoint2: TPoint3D): TPoint3D;

begin
  Result.X:=aPoint1.X-aPoint2.X;
  Result.Y:=aPoint1.Y-aPoint2.Y;
  Result.Z:=aPoint1.Z-aPoint2.Z;
end;


function TPoint3D.DotProduct(const aPoint: TPoint3D): Single;

begin
  Result:=(X*aPoint.X)+(Y*aPoint.Y)+(Z*aPoint.Z);
//  Writeln('Dot(',X,',',Y,',',Z,': ',Result);
end;


function TPoint3D.Length: Single;

begin
  Result:=Sqrt(DotProduct(Self));
//  Writeln('Len:',Result);
end;


function TPoint3D.AngleCosine(const aPoint: TPoint3D): Single;

var
  Fact : Single;

begin
  Fact:=Length*APoint.Length;
  if Abs(Fact)<=Epsilon then
    Fact:=Epsilon;
  Result:=DotProduct(APoint)/Fact;
  Result:=Max(Result,-1);
  Result:=Min(Result,1);
end;


function TPoint3D.CrossProduct(const aPoint: TPoint3D): TPoint3D;

// https://en.wikipedia.org/wiki/Cross_product
begin
  Result.X:=(Y*aPoint.Z)-(Z*aPoint.Y);
  Result.Y:=(Z*aPoint.X)-(X*aPoint.Z);
  Result.Z:=(X*aPoint.Y)-(Y*aPoint.X);
end;


function TPoint3D.Distance(const aPoint: TPoint3D): Single;

begin
   Result:=(aPoint-Self).Length;
end;


function TPoint3D.EqualsTo(const aPoint: TPoint3D; const aEpsilon: Single): Boolean;

begin
  Result:=SameValue(X,aPoint.X,aEpsilon)
          and SameValue(Y,aPoint.Y,aEpsilon)
          and SameValue(Z,aPoint.Z,aEpsilon)
end;


function TPoint3D.MidPoint(const aPoint: TPoint3D): TPoint3D;

begin
  Result:=(aPoint+Self)/2;
end;


function TPoint3D.Normalize: TPoint3D;

var
  S : Single;

begin
  S:=Length;
  if SameValue(S,0,TEpsilon.Vector) then
    S:=1
  else
    S:=1/S;
  Result:=Self*S;
end;


procedure TPoint3D.Offset(const aDelta: TPoint3D);

begin
  X:=X+aDelta.X;
  Y:=Y+aDelta.Y;
  Z:=Z+aDelta.Z;
end;


procedure TPoint3D.Offset(const aDeltaX, aDeltaY, aDeltaZ: Single);

begin
  X:=X+aDeltaX;
  Y:=Y+aDeltaY;
  Z:=Z+aDeltaZ;
end;


function TPoint3D.Reflect(const aPoint: TPoint3D): TPoint3D;

begin
  // Seems to be this, with aPoint the normal line:
  // https://math.stackexchange.com/questions/13261/how-to-get-a-reflection-vector
  Result:=Self-(aPoint*Self.DotProduct(APoint)*2);
end;


function TPoint3D.Rotate(const aAxis: TPoint3D; const aAngle: Single): TPoint3D;

begin
  Result:=Self*TMatrix3D.CreateRotation(aAxis,aAngle);
end;


function TPoint3D.ToString: String;

begin
  WriteStr(Result,'(',X:5:2,',',Y:5:2,',',Z:5:2,')');
end;


{ ---------------------------------------------------------------------
  TMatrix
  ---------------------------------------------------------------------}


class function TMatrix.CreateRotation(const aAngle: Single): TMatrix;

var
  cA,sA : Single;

begin
  SinCos(aAngle,sA,cA);
  Result:=TMatrix.Identity;
  Result.m11 := cA;
  Result.m12 := sA;
  Result.m21 := -sA;
  Result.m22 := cA;
end;


class function TMatrix.CreateScaling(const aScaleX, aScaleY: Single): TMatrix;

begin
  Result:=TMatrix.Identity;
  Result.m11:=aScaleX;
  Result.m22:=aScaleY;
end;


class function TMatrix.CreateTranslation(const aDeltaX, aDeltaY: Single): TMatrix;

begin
  Result:=TMatrix.Identity;
  Result.m31:=aDeltaX;
  Result.m32:=aDeltaY;
end;


class operator TMatrix.=(const RightMatrix, LeftMatrix: TMatrix): Boolean;

begin
  Result:=RightMatrix.EqualsTo(LeftMatrix,TEpsilon.Matrix);
end;


class operator TMatrix.*(const aMatrix1, aMatrix2: TMatrix): TMatrix;

var
  M1 : TMatrix absolute aMatrix1;
  M2 : TMatrix absolute aMatrix2;
  R : TMatrix;

begin
  With R do
    begin
    m11:=M1.m11*M2.m11 + M1.m12*M2.m21 + M1.m13*M2.m31;
    m12:=M1.m11*M2.m12 + M1.m12*M2.m22 + M1.m13*M2.m32;
    m13:=M1.m11*M2.m13 + M1.m12*M2.m23 + M1.m13*M2.m33;

    m21:=M1.m21*M2.m11 + M1.m22*M2.m21 + M1.m23*M2.m31;
    m22:=M1.m21*M2.m12 + M1.m22*M2.m22 + M1.m23*M2.m32;
    m23:=M1.m21*M2.m13 + M1.m22*M2.m23 + M1.m23*M2.m33;

    m31:=M1.m31*M2.m11 + M1.m32*M2.m21 + M1.m33*M2.m31;
    m32:=M1.m31*M2.m12 + M1.m32*M2.m22 + M1.m33*M2.m32;
    m33:=M1.m31*M2.m13 + M1.m32*M2.m23 + M1.m33*M2.m33;
    end;
  Result:=R;
end;


class operator TMatrix.*(const aPoint: TPointF; const aMatrix: TMatrix): TPointF;

var
  P : TPointF absolute aPoint;
  N : TMatrix absolute aMatrix;

begin
  Result.X:=P.X*N.M11 + P.Y*N.M21 + N.M31;
  Result.Y:=P.X*N.M12 + P.Y*N.M22 + N.M32
end;


class operator TMatrix.*(const aVector: TVector; const aMatrix: TMatrix): TVector;

var
  V : TVector absolute aVector;
  N : TMatrix absolute aMatrix;

begin
  Result.X:=V.X*N.M11 + V.Y*N.M21 + V.W*N.M31;
  Result.Y:=V.X*N.M12 + V.Y*N.M22 + V.W*N.M32;
  Result.W:=V.X*N.M13 + V.Y*N.M23 + V.W*N.M33;
end;


class operator TMatrix.*(const aVector: TPoint3D; const aMatrix: TMatrix): TPoint3D;
var
  V : TPoint3D absolute aVector;
  N : TMatrix absolute aMatrix;

begin
  Result.X:=V.X*N.M11 + V.Y*N.M21 + V.Z*N.M31;
  Result.Y:=V.X*N.M12 + V.Y*N.M22 + V.Z*N.M32;
  Result.Z:=V.X*N.M13 + V.Y*N.M23 + V.Z*N.M33;
end;

class operator TMatrix.*(const aFactor: Single; const aMatrix: TMatrix): TMatrix;

begin
  Result:=aMatrix.Scale(aFactor);
end;


class operator TMatrix.*(const aMatrix: TMatrix; const aFactor: Single): TMatrix;

begin
  Result:=aMatrix.Scale(aFactor);
end;


class operator TMatrix./(const aMatrix: TMatrix; const aFactor: Single): TMatrix;

begin
  Result:=aMatrix.Scale(1/aFactor);
end;


function TMatrix.Adjoint: TMatrix;
// https://en.wikipedia.org/wiki/Adjugate_matrix
var
  a1, a2, a3, b1, b2, b3, c1, c2, c3: Single;
  R : TMatrix;

begin
  a1:=Self.M11;
  a2:=Self.M12;
  a3:=Self.M13;
  b1:=Self.M21;
  b2:=Self.M22;
  b3:=Self.M23;
  c1:=Self.M31;
  c2:=Self.M32;
  c3:=Self.M33;
  With R do
    begin
    M11:= (b2 * c3 - c2 * b3);
    M12:=-(a2 * c3 - c2 * a3);
    M13:= (a2 * b3 - b2 * a3);

    M21:=-(b1 * c3 - c1 * b3);
    M22:= (a1 * c3 - c1 * a3);
    M23:=-(a1 * b3 - b1 * a3);

    M31:= (b1 * c2 - c1 * b2);
    M32:=-(a1 * c2 - c1 * a2);
    M33:= (a1 * b2 - b1 * a2);
    end;
  Result:=R;
end;


// Determinant is calculated recursively.
Function Det2x2(const a1, a2, b1, b2 : single) : Single; inline;

begin
  Result:=a1*b2-b1*a2;
end;


function Det3x3(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;

begin
  Result:=a1*Det2x2(b2,b3,c2,c3)-b1*Det2x2(a2,a3,c2,c3)+c1*Det2x2(a2,a3,b2,b3);
end;


function TMatrix.Determinant: Single;

begin
  Result:=Det3x3(m11,m12,m13,m21,m22,m23,m31,m32,m33);
end;


function TMatrix.EqualsTo(const aMatrix: TMatrix; const Epsilon: Single
  ): Boolean;

  Function Eq(s1,S2 : single) : Boolean; inline;
  begin
    Result:=SameValue(S1,S2,Epsilon);
  end;

Var
  N : TMatrix absolute aMatrix;

begin
  Result:=Eq(m11,N.m11) and Eq(m12,N.m12) and Eq(m13,N.m13)
          and Eq(m21,N.m21) and Eq(m22,N.m22) and Eq(m23,N.m23)
          and Eq(m31,N.m31) and Eq(m32,N.m32) and Eq(m33,N.m33);
end;

function TMatrix.ExtractScale: TPointF;

const
  Origin : TPointF = (X:0;Y:0);

begin
  Result.X:=(PointF(1,0)*Self).Distance(Origin*Self);
  Result.Y:=(PointF(0,1)*Self).Distance(Origin*Self);
end;


function TMatrix.Inverse: TMatrix;

var
  D: Single;

begin
  D:=Self.Determinant;
  if SameValue(D,0,Epsilon) then
    Result:=TMatrix.Identity
  else
    Result:=(Self.Adjoint/D);
end;


function TMatrix.Scale(const aFactor: Single): TMatrix;

begin
  Result.M11:=M11*aFactor;
  Result.M12:=M12*aFactor;
  Result.M13:=M13*aFactor;

  Result.M21:=M21*aFactor;
  Result.M22:=M22*aFactor;
  Result.M23:=M23*aFactor;

  Result.M31:=M31*aFactor;
  Result.M32:=M32*aFactor;
  Result.M33:=M33*aFactor;
end;


function TMatrix.ToString(Multiline: Boolean): String;

var
  S,Sep : String;

begin
  Sep:='';
  if MultiLine then
    Sep:=sLineBreak;
  WriteStr(S,'[',Display(M11),',',Display(M12),',',Display(M13));
  Result:=S+','+Sep;
  WriteStr(S,Display(M21),',',Display(M22),',',Display(M23));
  Result:=Result+S+Sep;
  WriteStr(S,Display(M31),',',Display(M32),',',Display(M33));
  Result:=Result+S+']';
end;


{ ---------------------------------------------------------------------
  TVector3D
  ---------------------------------------------------------------------}

class function TVector3D.Create(const aX, aY, aZ: Single; const aW: Single): TVector3D;

begin
  Result.X:=aX;
  Result.Y:=aY;
  Result.Z:=aZ;
  Result.W:=aW;
end;


class function TVector3D.Create(const aPoint: TPoint3D; const aW: Single): TVector3D;

begin
  Result.X:=aPoint.X;
  Result.Y:=aPoint.Y;
  Result.Z:=aPoint.Z;
  Result.W:=aW;
end;


class function TVector3D.Zero: TVector3D;

begin
  Result.X:=0;
  Result.Y:=0;
  Result.Z:=0;
  Result.W:=0;
end;


function TVector3D.EqualsTo(const aVector: TVector3D; const Epsilon: Single): Boolean;

begin
  Result:=SameValue(X,aVector.X,Epsilon)
          and SameValue(Y,aVector.Y,Epsilon)
          and SameValue(Z,aVector.Z,Epsilon)
          and SameValue(W,aVector.W,Epsilon);
end;


class operator TVector3D.+(const aVector1, aVector2: TVector3D): TVector3D;

var
  V1 : TVector3D absolute aVector1;
  V2 : TVector3D absolute aVector2;

begin
  Result.X:=V1.X+V2.X;
  Result.Y:=V1.Y+V2.Y;
  Result.Z:=V1.Z+V2.Z;
  Result.W:=V1.W+V2.W;
end;


class operator TVector3D./(const aVector: TVector3D; const aFactor: Single): TVector3D;

begin
  With aVector do
    begin
    Result.X:=X/aFactor;
    Result.Y:=Y/aFactor;
    Result.Z:=Z/aFactor;
    Result.W:=W/aFactor;
    end;
end;


class operator TVector3D.=(const aVector1, aVector2: TVector3D): Boolean;

begin
  Result:=aVector1.EqualsTo(aVector2,TEpsilon.Vector);
end;


class operator TVector3D.explicit(const aVector: TVector3D): TPoint3D;

begin
  Result:=aVector.ToPoint3D;
end;


class operator TVector3D.:=(const aPoint: TPoint3D): TVector3D;

begin
  Result:=TVector3D.Create(APoint);
end;


class operator TVector3D.:=(const aVector: TVector3D): TPoint3D;

begin
  Result:=TPoint3D(AVector);
end;


class operator TVector3D.*(const aVector: TVector3D; const aFactor: Single): TVector3D;
begin
  With aVector do
    begin
    Result.X:=X*aFactor;
    Result.Y:=Y*aFactor;
    Result.Z:=Z*aFactor;
    Result.W:=W*aFactor;
    end;
end;


class operator TVector3D.*(const aFactor: Single; const aVector: TVector3D): TVector3D;

begin
  Result:=aVector*aFactor;
end;


class operator TVector3D.*(const aVector1, aVector2: TVector3D): TVector3D;

var
  V1 : TVector3D absolute aVector1;
  V2 : TVector3D absolute aVector2;

begin
  Result.X:=V1.X*V2.X;
  Result.Y:=V1.Y*V2.Y;
  Result.Z:=V1.Z*V2.Z;
  Result.W:=V1.W*V2.W;
end;


class operator TVector3D.-(const aVector: TVector3D): TVector3D;

var
  aV : TVector3D absolute aVector;

begin
  With Result do
    begin
    X:=-aV.X;
    Y:=-aV.Y;
    Z:=-aV.Z;
    W:=-aV.W;
    end;
end;


class operator TVector3D.<>(const aVector1, aVector2: TVector3D): Boolean;

begin
  Result:=Not (aVector1=aVector2)
end;


class operator TVector3D.-(const aVector1, aVector2: TVector3D): TVector3D;

var
  V1 : TVector3D absolute aVector1;
  V2 : TVector3D absolute aVector2;

begin
  Result.X:=V1.X-V2.X;
  Result.Y:=V1.Y-V2.Y;
  Result.Z:=V1.Z-V2.Z;
  Result.W:=V1.W-V2.W;
end;


function TVector3D.AngleCosine(const aVector: TVector3D): Single;

var
  P1,P2 : TPoint3D;

begin
  P1:=Self;
  P2:=aVector;
  Result:=P1.AngleCosine(P2);
end;


function TVector3D.CrossProduct(const aVector: TVector3D): TVector3D;

var
  aV : TVector3D absolute aVector;

begin
  Result.X:=Y*aV.Z-Z*aV.Y;
  Result.Y:=Z*aV.X-X*aV.Z;
  Result.Z:=X*aV.Y-Y*aV.X;
  Result.W:=1;
end;


function TVector3D.Distance(const aVector: TVector3D): Single;

begin
  Result:=(AVector-Self).Length;
end;


function TVector3D.DotProduct(const aVector: TVector3D): Single;

begin
  Result:=X*aVector.X
          + Y*aVector.Y
          + Z*aVector.Z;
end;


function TVector3D.Length: Single;
begin
  Result:=Sqrt(Sqr(X)+Sqr(Y)+Sqr(Z)+Sqr(W));
end;


function TVector3D.MidVector(const aVector: TVector3D): TVector3D;
begin
  Result:=(Self+aVector)/2;
end;


function TVector3D.Normalize: TVector3D;

var
  Len: Single;
begin
  Len:=Self.Length;
  if SameValue(0,Len,TEpsilon.Vector)  then
    Result:=Self
  else
    Result:=Result/Len;
end;


function TVector3D.Reflect(const aVector: TVector3D): TVector3D;

var
  P1,P2 : TPoint3D;

begin
  P1:=Self;
  P2:=aVector;
  Result:=P1.Reflect(P2);
end;


function TVector3D.Rotate(const aAxis: TPoint3D; const aAngle: Single
  ): TVector3D;
begin
  Result:=Self*TMatrix3D.CreateRotation(aAxis,aAngle);
end;


function TVector3D.ToPoint3D(const aTransform: Boolean): TPoint3D;

var
  Scale: Single;

begin
  Scale:=1;
  if ATransform and not SameValue(W,0,TEpsilon.Vector) then
    Scale:=1/W;
  Result.X:=X*Scale;
  Result.Y:=Y*Scale;
  Result.Z:=Z*Scale;
end;


procedure TVector3D.Offset(const aDelta: TPoint3D);
begin
  X:=X+aDelta.X;
  Y:=Y+aDelta.Y;
  Z:=Z+aDelta.Z;
end;


procedure TVector3D.Offset(const aDeltaX, aDeltaY, aDeltaZ: Single);

begin
  X:=X+aDeltaX;
  Y:=Y+aDeltaY;
  Z:=Z+aDeltaZ;
end;


{ ---------------------------------------------------------------------
  TMatrix3D
  ---------------------------------------------------------------------}

constructor TMatrix3D.Create(const aM11, aM12, aM13, aM14,
                                   aM21, aM22, aM23, aM24,
                                   aM31, aM32, aM33, aM34,
                                   aM41, aM42, aM43, aM44: Single);

begin
  m11:=aM11;
  m12:=aM12;
  m13:=aM13;
  m14:=aM14;
  m21:=aM21;
  m22:=aM22;
  m23:=aM23;
  m24:=aM24;
  m31:=aM31;
  m32:=aM32;
  m33:=aM33;
  m34:=aM34;
  m41:=aM41;
  m42:=aM42;
  m43:=aM43;
  m44:=aM44;
end;


constructor TMatrix3D.Create(const aArray: TSingleDynArray);
begin
 Self:=TMatrix3D.Create(aArray[0],aArray[4],aArray[8], aArray[12],
                        aArray[1],aArray[5],aArray[9], aArray[13],
                        aArray[2],aArray[6],aArray[10],aArray[14],
                        aArray[3],aArray[7],aArray[11],aArray[15]);
end;


class function TMatrix3D.CreateLookAtDirLH(const aSource, aDirection, aCeiling: TPoint3D): TMatrix3D;

// See e.g.
// https://www.gamedev.net/tutorials/programming/graphics/perspective-projections-in-lh-and-rh-systems-r3598/
// or See glscene https://github.com/glscene
// GLS.VectorGeometry.pas : CreateLookAtMatrix

var
  Z,X,Y : TPoint3D;
  R : TMatrix3D;

begin
  // Create reference axes X,Y,Z
  Z:=-aDirection.Normalize;
  X:=aCeiling.CrossProduct(Z).Normalize;
  Y:=Z.CrossProduct(X); // Is normalized because X,Z normalized
  R:=TMatrix3D.Identity;

  // Translate
  R.m41:= X.DotProduct(aSource);
  R.m42:=-Y.DotProduct(aSource);
  R.m43:=-Z.DotProduct(aSource);

  // Rotate
  R.m11 := X.X;
  R.m12 := Y.X;
  R.m13 := Z.X;

  R.m21 := X.Y;
  R.m22 := Y.Y;
  R.m23 := Z.Y;

  R.m31 := X.Z;
  R.m32 := Y.Z;
  R.m33 := Z.Z;

  Result:=R;
end;


class function TMatrix3D.CreateLookAtDirRH(const aSource, aDirection,
  aCeiling: TPoint3D): TMatrix3D;
// See e.g.
// https://www.gamedev.net/tutorials/programming/graphics/perspective-projections-in-lh-and-rh-systems-r3598/
// or See glscene https://github.com/glscene
// GLS.VectorGeometry.pas : CreateLookAtMatrix

var
  Z,X,Y : TPoint3D;
  R : TMatrix3D;

begin
  // Create reference axes X,Y,Z
  Z:=aDirection.Normalize;
  X:=aCeiling.CrossProduct(Z).Normalize;
  Y:=Z.CrossProduct(X); // Is normalized because X,Z normalized
  R:=TMatrix3D.Identity;

  // Translate
  R.m41:=-X.DotProduct(aSource);
  R.m42:=-Y.DotProduct(aSource);
  R.m43:=-Z.DotProduct(aSource);

  // Rotate
  R.m11 := X.X;
  R.m12 := Y.X;
  R.m13 := Z.X;

  R.m21 := X.Y;
  R.m22 := Y.Y;
  R.m23 := Z.Y;

  R.m31 := X.Z;
  R.m32 := Y.Z;
  R.m33 := Z.Z;

  Result:=R;
end;


class function TMatrix3D.CreateLookAtLH(const aSource, aTarget, aCeiling: TPoint3D): TMatrix3D;

// See e.g.
// https://www.gamedev.net/tutorials/programming/graphics/perspective-projections-in-lh-and-rh-systems-r3598/
// or See glscene https://github.com/glscene
// GLS.VectorGeometry.pas : CreateLookAtMatrix

var
  Z,X,Y : TPoint3D;
  R : TMatrix3D;

begin
  // Create reference axes X,Y,Z
  Z:=(aTarget-aSource).Normalize;
  X:=aCeiling.CrossProduct(Z).Normalize;
  Y:=Z.CrossProduct(X); // Is normalized because X,Z normalized
  R:=TMatrix3D.Identity;

  // Translate
  R.m41:=-X.DotProduct(aSource);
  R.m42:=-Y.DotProduct(aSource);
  R.m43:=-Z.DotProduct(aSource);

  // Rotate
  // row 1
  R.m11 := X.X;
  R.m12 := Y.X;
  R.m13 := Z.X;

  // row 2
  R.m21 := X.Y;
  R.m22 := Y.Y;
  R.m23 := Z.Y;

  // row 3
  R.m31 := X.Z;
  R.m32 := Y.Z;
  R.m33 := Z.Z;

  Result:=R;
end;


class function TMatrix3D.CreateLookAtRH(const aSource, aTarget, aCeiling: TPoint3D): TMatrix3D;

var
  Z,X,Y : TPoint3D;
  R : TMatrix3D;

begin
  // Create reference axes X,Y,Z
  Z:=(aSource-aTarget).Normalize;
  X:=aCeiling.CrossProduct(Z).Normalize;
  Y:=Z.CrossProduct(X); // Is normalized because X,Z normalized
  R:=TMatrix3D.Identity;

  // Translate
  R.m41:=-X.DotProduct(aSource);
  R.m42:=-Y.DotProduct(aSource);
  R.m43:=-Z.DotProduct(aSource);

  // Rotate
  // Row 1
  R.m11 := X.X;
  R.m12 := Y.X;
  R.m13 := Z.X;

  // Row 2
  R.m21 := X.Y;
  R.m22 := Y.Y;
  R.m23 := Z.Y;

  // Row 3
  R.m31 := X.Z;
  R.m32 := Y.Z;
  R.m33 := Z.Z;

  Result:=R;
end;


class function TMatrix3D.CreateOrthoLH(const aWidth, aHeight, aZNear, aZFar: Single): TMatrix3D;

// See glscene https://github.com/glscene
// GLS.VectorGeometry.pas, CreateMatrixFromFrustum and friends.

var
  R : TMatrix3D;
  aLen : Single;

begin
  R:=TMatrix3D.Identity;
  aLen:=(aZFar-aZNear);
  R.m11:=2/aWidth;
  R.m22:=2/aHeight;
  R.m33:=1/aLen;
  R.m42:=aZNear/(-alen);
  Result:=R;
end;


class function TMatrix3D.CreateOrthoOffCenterLH(const aLeft, aTop, aRight, aBottom, aZNear, aZFar: Single): TMatrix3D;
// See glscene https://github.com/glscene
// GLS.VectorGeometry.pas, CreateMatrixFromFrustum and friends.

var
  R : TMatrix3D;
  aWidth,aHeight,aLen : Single;

begin
  // Same as TMatrix3D.CreateOrthoLH, but offset
  R:=TMatrix3D.Identity;
  aWidth:=aRight-aleft;
  aHeight:=aTop-aBottom;
  aLen:=(aZFar-aZNear);
  R.m11:=2/aWidth;
  R.m22:=2/aHeight;
  R.m33:=1/aLen;
  R.m41:=(aLeft+aRight)/(-aWidth);
  R.m42:=(aTop+aBottom)/(-aHeight);
  R.m43:=aZNear/(-alen);
  Result:=R;
end;


class function TMatrix3D.CreateOrthoOffCenterRH(const aLeft, aTop, aRight, aBottom, aZNear, aZFar: Single): TMatrix3D;
// See glscene https://github.com/glscene
// GLS.VectorGeometry.pas, CreateMatrixFromFrustum and friends.

var
  R : TMatrix3D;
  aWidth,aHeight,aLen : Single;

begin
  // Same as TMatrix3D.CreateOrthoRH, but offset
  R:=TMatrix3D.Identity;
  aWidth:=aRight-aleft;
  aHeight:=aTop-aBottom;
  aLen:=(aZFar-aZNear);
  R.m11:=2/aWidth;
  R.m22:=2/aHeight;
  R.m33:=1/-aLen;
  R.m41:=(aLeft+aRight)/(-aWidth);
  R.m42:=(aTop+aBottom)/(-aHeight);
  R.m43:=aZNear/(-alen);
  Result:=R;
end;


class function TMatrix3D.CreateOrthoRH(const aWidth, aHeight, aZNear, aZFar: Single): TMatrix3D;

// See glscene https://github.com/glscene
// GLS.VectorGeometry.pas, CreateMatrixFromFrustum and friends.

var
  R : TMatrix3D;

begin
  R:=TMatrix3D.Identity;
  R.m11:=2/aWidth;
  R.m22:=2/aHeight;
  R.m33:=1/(aZNear-aZFar);
  R.m42:=AZNear/(AZNear-AZFar);
  Result:=R;
end;


class function TMatrix3D.CreatePerspectiveFovLH(const aFOV, aAspect, aZNear,
  aZFar: Single; const aHorizontalFOV: Boolean): TMatrix3D;

// see https//www.gamedev.net/tutorials/programming/graphics/perspective-projections-in-lh-and-rh-systems-r3598/
// Perspective projection for A,B,C,D,E, DirectX calculation.

var
  aDist,sX,SY: Single;
  R: TMatrix3D;

begin
  if aHorizontalFOV then
    begin
    sX:=1/Tan(aFOV/2);
    sY:=sX/aAspect;
    end
  else
    begin
    sY:=1/Tan(aFOV/2);
    sX:=sY/aAspect;
    end;
  aDist:=aZFar-aZNear;

  R:=TMatrix3D.Identity;
  R.m11:=sY; // A
  R.m22:=sY; // B
  R.m33:=aZFar/aDist; // C
  R.m34:=1; // D
  R.m43:=-aZNear*aZFar/aDist; // E
  R.m44:=0;
  Result:=R;
end;


class function TMatrix3D.CreatePerspectiveFovRH(const aFOV, aAspect, aZNear,
  aZFar: Single; const aHorizontalFOV: Boolean): TMatrix3D;

// see https//www.gamedev.net/tutorials/programming/graphics/perspective-projections-in-lh-and-rh-systems-r3598/
// Perspective projection for A,B,C,D,E, DirectX calculation.

var
  aDist,sX,SY: Single;
  R: TMatrix3D;

begin
  if aHorizontalFOV then
    begin
    sX:=1/Tan(aFOV/2);
    sY:=sX/aAspect;
    end
  else
    begin
    sY:=1/Tan(aFOV/2);
    sX:=sY/aAspect;
    end;
  aDist:=aZNear-aZFar;

  R:=TMatrix3D.Identity;
  R.m11:=sY; // A
  R.m22:=sY; // B
  R.m33:=aZFar/aDist; // C
  R.m34:=-1; // D
  R.m43:=aZNear*aZFar/aDist; // E
  R.m44:=0;
  Result:=R;
end;


class function TMatrix3D.CreateRotation(const aAxis: TPoint3D; const aAngle: Single): TMatrix3D;

// See 'Rotation matrix from axis and angle' on
// https://en.wikipedia.org/wiki/Rotation_matrix
// Y-axis reversed

var
  U: TPoint3D;
  C,S,Cos1: Single;
  R : TMatrix3D;

begin
  SinCos(aAngle,S,C);
  Cos1:=1-C;
  U:=aAxis.Normalize;
  R:=TMatrix3D.Identity;
  With U do
    begin
    // Row 1
    R.m11:=(X*X*Cos1) + C;
    R.m12:=(X*Y*Cos1) + (Z*S);
    R.m13:=(Z*X*Cos1) - (Y*S);

    // Row 2
    R.m21:=(X*Y*Cos1) - (Z*S);
    R.m22:=(Y*Y*Cos1) + C;
    R.m23:=(Y*Z*Cos1) + (X*S);

    // Row 3
    R.m31:=(Z*X*Cos1) + (Y*S);
    R.m32:=(Y*Z*Cos1) - (X*S);
    R.m33:=(Z*Z*Cos1) + C;
    end;
  Result:=R;
end;


class function TMatrix3D.CreateRotationHeadingPitchBank(const aHeading, aPitch,
  aBank: Single): TMatrix3D;

// Header-Pitch-Bank uses Tait-Brian angles
// See https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
// Y-axis reversed

var
  cHeading, sHeading, cPitch, sPitch, cBank, sBank : Single;
  R : TMatrix3D;

begin
  SinCos(aHeading,sHeading,cHeading);  // Z axis (psi)
  SinCos(aPitch,sPitch,cPitch);        // Y axis (Theta)
  SinCos(aBank,sBank,cBank);           // X axis (phi)
  R:=TMatrix3D.Identity;
  // Row 1
  R.m11:= cHeading*cBank + sHeading*sPitch*sBank;
  R.m12:=-cHeading*sBank + sBank*sHeading*cBank;
  R.m13:= sHeading*cPitch;

  // Row 2
  R.m21:=sBank*cPitch;
  R.m22:=cBank*cPitch;
  R.m23:=-sPitch;

  // Row 3
  R.m31:=-sHeading*cBank + cHeading*sPitch*sBank;
  R.m32:=(sBank*sHeading) + cHeading*sPitch*cBank;
  R.m33:=cHeading*cPitch;

  Result:=R;
end;


class function TMatrix3D.CreateRotationX(const aAngle: Single): TMatrix3D;

var
  cA,sA: Single;
  R: TMatrix3D;

begin
  SinCos(aAngle,sA,cA);
  R:=TMatrix3D.Identity;
  (*
  [1 0   0 ]
  [0 ca sa]
  [0 -sa  ca]
  *)
  R.m11:=1;
  R.m22:=cA;
  R.m23:=sA;
  R.m32:=-sA;
  R.m33:=cA;
  Result:=R;
end;


class function TMatrix3D.CreateRotationY(const aAngle: Single): TMatrix3D;
var
  cA,sA: Single;
  R: TMatrix3D;

begin
  SinCos(aAngle,sA,cA);
  R:=TMatrix3D.Identity;
  (*
  [ca 0  -sa]
  [0  1   0 ]
  [sa 0   ca]
  *)
  R.m11:=cA;
  R.m13:=-sA;
  R.m31:=sA;
  R.m33:=cA;
  Result:=R;
end;


class function TMatrix3D.CreateRotationYawPitchRoll(const aYaw, aPitch,
  aRoll: Single): TMatrix3D;

// See general 3D rotations in
// https://en.wikipedia.org/wiki/Rotation_matrix
// aYaw = Alpha = A, aPitch = Beta = B, aRoll = Gamma = G

var
  sYaw, cYaw, sPitch, cPitch, sRoll, cRoll: Single;
  R : TMatrix3D;

begin
  SinCos(aYaw,SYaw,CYaw);
  SinCos(aPitch,sPitch,CPitch);
  SinCos(aRoll,SRoll,CRoll);
  R:=TMatrix3D.Identity;
  // Row 1
  R.m11:=cRoll*cYaw        + sPitch*sRoll*sYaw;
  R.m12:=cYaw*sPitch*sRoll - cRoll*sYaw;
  R.m13:= -cPitch*sRoll;
  // Row 2
  R.m21:=cPitch*sYaw;
  R.m22:=cPitch*cYaw;
  R.m23:=sPitch;
  // Row 3
  R.m31:=cYaw*sRoll         - cRoll*sPitch*sYaw;
  R.m32:=-cRoll*cYaw*sPitch - sRoll*sYaw;
  R.m33:=cPitch*cRoll;

  Result:=R;
end;


class function TMatrix3D.CreateRotationZ(const aAngle: Single): TMatrix3D;

var
  cA,sA: Single;
  R : TMatrix3D;

begin
  SinCos(aAngle,sA,cA);
  R:=TMatrix3D.Identity;
  (*
  [ca sa 0]
  [-sa  ca 0]
  [0   0  1]
  *)

  R.m11:=cA;
  R.m12:=sA;
  R.m21:=-sA;
  R.m22:=cA;
  Result:=R;
end;


class function TMatrix3D.CreateScaling(const aScale: TPoint3D): TMatrix3D;

var
  R : TMatrix3D;

begin
  R:=Zero;
  R.m11:=aScale.X;
  R.m22:=aScale.Y;
  R.m33:=aScale.Z;
  R.m44:=1;
  Result:=R;
end;


class function TMatrix3D.CreateTranslation(const aTranslation: TPoint3D): TMatrix3D;

var
  R : TMatrix3D;

begin
  R:=TMatrix3D.Identity;
  With aTranslation do
    begin
    R.m41:=X;
    R.m42:=Y;
    R.m43:=Z;
    R.m44:=1;
    end;
  Result:=R;
end;


class operator TMatrix3D.*(const aPoint: TPoint3D; const aMatrix: TMatrix3D
  ): TPoint3D;
var
  P : TPoint3D absolute aPoint;
  N : TMatrix3D absolute aMatrix;
begin
  Result.X:=(P.X*N.m11)+(P.Y*N.m21)+(P.Z*N.m31)+N.m41;
  Result.Y:=(P.X*N.m12)+(P.Y*N.m22)+(P.Z*N.m32)+N.m42;
  Result.Z:=(P.X*N.m13)+(P.Y*N.m23)+(P.Z*N.m33)+N.m43;
end;


class operator TMatrix3D.*(const aMatrix1, aMatrix2: TMatrix3D): TMatrix3D;

var
  R,C,K : Integer;
  S : Single;

begin
  For R:=0 to 3 do
    For C:=0 to 3 do
      begin
      S:=0;
      For K:=0 to 3 do
        S:=S+(aMatrix1.M[R].V[K]*aMatrix2.M[K].V[C]);
      Result.M[R].V[C]:=S;
      end;
end;


class operator TMatrix3D.*(const aVector: TVector3D; const aMatrix: TMatrix3D
  ): TVector3D;

var
  V : TVector3D absolute aVector;
  N : TMatrix3D absolute aMatrix;

begin
  Result.X:=(V.X*N.m11)+(V.Y*N.m21)+(V.Z*N.m31)+(V.W*N.m41);
  Result.Y:=(V.X*N.m12)+(V.Y*N.m22)+(V.Z*N.m32)+(V.W*N.m42);
  Result.Z:=(V.X*N.m13)+(V.Y*N.m23)+(V.Z*N.m33)+(V.W*N.m43);
  Result.W:=(V.X*N.m14)+(V.Y*N.m24)+(V.Z*N.m34)+(V.W*N.m44);
end;


class operator TMatrix3D.*(const aFactor: single; const aMatrix: TMatrix3D) : TMatrix3D;

begin
  Result:=aMatrix*aFactor;
end;


class operator TMatrix3D.*(const aMatrix: TMatrix3D; const aFactor: single) : TMatrix3D;

var
  R : TMatrix3D;
  A : TMatrix3D absolute aMatrix;

begin
  R.M11:=A.M11*aFactor;
  R.M12:=A.M12*aFactor;
  R.M13:=A.M13*aFactor;
  R.M14:=A.M14*aFactor;

  R.M21:=A.M21*aFactor;
  R.M22:=A.M22*aFactor;
  R.M23:=A.M23*aFactor;
  R.M24:=A.M24*aFactor;

  R.M31:=A.M31*aFactor;
  R.M32:=A.M32*aFactor;
  R.M33:=A.M33*aFactor;
  R.M34:=A.M34*aFactor;

  R.M41:=A.M41*aFactor;
  R.M42:=A.M42*aFactor;
  R.M43:=A.M43*aFactor;
  R.M44:=A.M44*aFactor;

  Result:=R;
end;


class operator TMatrix3D./(const aMatrix: TMatrix3D; const aFactor: single
  ): TMatrix3D;
var
  R : TMatrix3D;
  A : TMatrix3D absolute aMatrix;

begin
  R.M11:=A.M11/aFactor;
  R.M12:=A.M12/aFactor;
  R.M13:=A.M13/aFactor;
  R.M14:=A.M14/aFactor;

  R.M21:=A.M21/aFactor;
  R.M22:=A.M22/aFactor;
  R.M23:=A.M23/aFactor;
  R.M24:=A.M24/aFactor;

  R.M31:=A.M31/aFactor;
  R.M32:=A.M32/aFactor;
  R.M33:=A.M33/aFactor;
  R.M34:=A.M34/aFactor;

  R.M41:=A.M41/aFactor;
  R.M42:=A.M42/aFactor;
  R.M43:=A.M43/aFactor;
  R.M44:=A.M44/aFactor;

  Result:=R;
end;


function TMatrix3D.Adjoint: TMatrix3D;

var
  a1, a2, a3, a4,
  b1, b2, b3, b4,
  c1, c2, c3, c4,
  d1, d2, d3, d4: Single;
  R : TMatrix3D;

begin
  a1 := M11;
  b1 := M12;
  c1 := M13;
  d1 := M14;

  a2 := M21;
  b2 := M22;
  c2 := M23;
  d2 := M24;

  a3 := M31;
  b3 := M32;
  c3 := M33;
  d3 := M34;

  a4 := M41;
  b4 := M42;
  c4 := M43;
  d4 := M44;

  With R do
    begin
    M11:= Det3x3(b2,b3,b4, c2,c3,c4, d2,d3,d4);
    M21:=-Det3x3(a2,a3,a4, c2,c3,c4, d2,d3,d4);
    M31:= Det3x3(a2,a3,a4, b2,b3,b4, d2,d3,d4);
    M41:=-Det3x3(a2,a3,a4, b2,b3,b4, c2,c3,c4);

    M12:=-Det3x3(b1,b3,b4, c1,c3,c4, d1,d3,d4);
    M22:= Det3x3(a1,a3,a4, c1,c3,c4, d1,d3,d4);
    M32:=-Det3x3(a1,a3,a4, b1,b3,b4, d1,d3,d4);
    M42:= Det3x3(a1,a3,a4, b1,b3,b4, c1,c3,c4);

    M13:= Det3x3(b1,b2,b4, c1,c2,c4, d1,d2,d4);
    M23:=-Det3x3(a1,a2,a4, c1,c2,c4, d1,d2,d4);
    M33:= Det3x3(a1,a2,a4, b1,b2,b4, d1,d2,d4);
    M43:=-Det3x3(a1,a2,a4, b1,b2,b4, c1,c2,c4);

    M14:=-Det3x3(b1,b2,b3, c1,c2,c3, d1,d2,d3);
    M24:= Det3x3(a1,a2,a3, c1,c2,c3, d1,d2,d3);
    M34:=-Det3x3(a1,a2,a3, b1,b2,b3, d1,d2,d3);
    M44:= Det3x3(a1,a2,a3, b1,b2,b3, c1,c2,c3);
    end;
  Result:=R;
end;


function TMatrix3D.Determinant: Single;

begin
  Result:= (M11*Det3x3(m22,m23,m24, m32,m33,m34, m42,m43,m44))
          -(M12*Det3x3(m21,m23,m24, m31,m33,m34, m41,m43,m44))
          +(M13*Det3x3(m21,m22,m24, m31,m32,m34, m41,m42,m44))
          -(M14*Det3x3(m21,m22,m23, m31,m32,m33, m41,m42,m43))
end;


function TMatrix3D.EyePosition: TPoint3D;

begin
  Result.X:=-(M11*M41 + M12*M42 + M13*M43);
  Result.Y:=-(M21*M41 + M22*M42 + M23*M43);
  Result.Z:=-(M31*M41 + M32*M42 + M33*M43);
end;


function TMatrix3D.Inverse: TMatrix3D;

var
  D: Single;

begin
  D:=Self.Determinant;
  if SameValue(D,0,Epsilon) then
    Result:=TMatrix3D.Identity
  else
    Result:=(Self.Adjoint/D);
end;

function TMatrix3D.Scale(const aFactor: Single): TMatrix3D;
begin
  Result:=Self*aFactor;
end;


function TMatrix3D.ToMatrix: TMatrix;

begin
  Result.m11:=m11;
  Result.m12:=m12;
  Result.m13:=m13;
  Result.m21:=m21;
  Result.m22:=m22;
  Result.m23:=m23;
  Result.m31:=m31;
  Result.m32:=m32;
  Result.m33:=m33;
end;


function TMatrix3D.Transpose: TMatrix3D;

begin
  Result.M11:=M11;
  Result.M21:=M12;
  Result.M31:=M13;
  Result.M41:=M14;

  Result.M12:=M21;
  Result.M22:=M22;
  Result.M32:=M23;
  Result.M42:=M24;

  Result.M13:=M31;
  Result.M23:=M32;
  Result.M33:=M33;
  Result.M43:=M34;

  Result.M14:=M41;
  Result.M24:=M42;
  Result.M34:=M43;
  Result.M44:=M44;
end;

function TMatrix3D.ToString(Multiline: Boolean): String;

var
  S,Sep : String;

begin
  Sep:='';
  if MultiLine then
    Sep:=sLineBreak;
  WriteStr(S,Display(m11),',',Display(M12),',',Display(M13),',',Display(M14));
  Result:=S+','+Sep;
  WriteStr(S,Display(M21),',',Display(M22),',',Display(M23),',',Display(M24));
  Result:=Result+S+','+Sep;
  WriteStr(S,Display(M31),',',Display(M32),',',Display(M33),',',Display(M34));
  Result:=Result+S+','+Sep;
  WriteStr(S,Display(M41),',',Display(M42),',',Display(M43),',',Display(M44));
  Result:='['+Result+S+']';
end;


class function TMatrix3D.Zero: TMatrix3D; static;

begin
  Result:=Default(TMatrix3D);
end;


{ ---------------------------------------------------------------------
  TQuaternion3D
  ---------------------------------------------------------------------}

constructor TQuaternion3D.Create(const aAxis: TPoint3D; const aAngle: Single);

var
  L,sA,cA : Single;

begin
  L:=aAxis.Length;
  if SameValue(L,0,Epsilon2) then
    Self:=TQuaternion3D.Identity
  else
    begin
    SinCos(aAngle/2,sA,cA);
    Self.RealPart:=cA;
    Self.ImagPart:=aAxis*(sA/L);
    end;
end;

constructor TQuaternion3D.Create(const P1, P2: TPoint3D);
begin
  ImagPart:=P1.CrossProduct(P2);
  RealPart:=Sqrt((P1.DotProduct(P2)+1)/2)
end;


constructor TQuaternion3D.Create(const aYaw, aPitch, aRoll: Single);

begin
  Self:=TQuaternion3D.Create(Point3D(0,1,0),AYaw) // rotation around Y
        *TQuaternion3D.Create(Point3D(1,0,0),APitch) // rotation around X
        *TQuaternion3D.Create(Point3D(0,0,1),ARoll); // rotation around Z
end;

constructor TQuaternion3D.Create(const aMatrix: TMatrix3D);

// See glscene  QuaternionFromMatrix

var
  Q : TQuaternion3D;
  M : TMatrix3D absolute aMatrix;
  Diag, S : Double;

begin
  Diag:=1+m.m11+m.m22+m.m33;
  if Diag>Epsilon then
    begin
    // All large
    S:=2*Sqrt(Diag);
    Q.RealPart:=S/4;
    S:=1/S;
    Q.ImagPart.X:=(M.m23 - M.m32)*S;
    Q.ImagPart.Y:=(M.m31 - M.m13)*S;
    Q.ImagPart.Z:=(M.m12 - M.m21)*S;
    end
  else if (M.m11>M.m22) and (M.m11>M.m33) then
    begin
    // XX is largest
    S:=2*Sqrt(Max(Epsilon, (1 + M.m11-M.m22-M.m33)));
    Q.ImagPart.X:=(S/4);
    S:=1/S;
    Q.ImagPart.Y:=(M.m12 + M.m21)*S;
    Q.ImagPart.Z:=(M.m31 + M.m13)*S;
    Q.RealPart  :=(M.m23 - M.m32)*S;
    end
  else if (M.m22 > M.m33) then
    begin
    // YY is largest
    S:=2*Sqrt(Max(Epsilon, (1 + M.m22-M.m11-M.m33)));
    Q.ImagPart.Y:=S/4;
    S:=1/S;
    Q.ImagPart.X:=(M.m12 + M.m21)*S;
    Q.ImagPart.Z:=(M.m23 + M.m32)*S;
    Q.RealPart  :=(M.m31 - M.m13)*S;
    end
  else
    begin
    // ZZ is largest
    S:=2*Sqrt(Max(Epsilon,(1+M.m33-M.m11-M.m22)));
    Q.ImagPart.Z:=S/4;
    S:=1/S;
    Q.ImagPart.X:=(M.m31 + M.m13)*S;
    Q.ImagPart.Y:=(M.m23 + M.m32)*S;
    Q.RealPart  :=(M.m12 - M.m21)*S;
    end;
  Self:=Q.Normalize;
end;


class operator TQuaternion3D.:=(const aQuaternion: TQuaternion3D): TMatrix3D;

var
  N: TQuaternion3D;
  xx, xy, xz, xw, yy, yz, yw, zz, zw: Single;
  R : TMatrix3D;

begin
  N:=AQuaternion.Normalize;
  With N do
    begin
    xx:=Sqr(ImagPart.X);
    xy:=ImagPart.X*ImagPart.Y;
    xz:=ImagPart.X*ImagPart.Z;
    xw:=ImagPart.X*RealPart;

    yy:=Sqr(ImagPart.Y);
    yz:=ImagPart.Y*ImagPart.Z;
    yw:=ImagPart.Y*RealPart;

    zz:=Sqr(ImagPart.Z);
    zw:=ImagPart.Z*RealPart;
    end;
  R:=TMatrix3D.Zero;
  With R do
    begin
    M11:=1-2*(yy+zz);
    M21:=2*(xy-zw);
    M31:=2*(xz+yw);

    M12:=2*(xy+zw);
    M22:=1-2*(xx+zz);
    M32:=2*(yz-xw);

    M13:=2*(xz-yw);
    M23:=2*(yz+xw);
    M33:=1-2*(xx+yy);

    M44:=1;
    end;
  Result:=R;
end;


class operator TQuaternion3D.*(const aQuaternion1, aQuaternion2: TQuaternion3D): TQuaternion3D;

var
  Q1 : TQuaternion3D absolute aQuaternion1;
  Q2 : TQuaternion3D absolute aQuaternion2;

begin
  Result.RealPart:= Q1.RealPart*Q2.RealPart - Q1.ImagPart.DotProduct(Q2.ImagPart);
  Result.ImagPart.X:=Q1.RealPart*Q2.ImagPart.X
                     + Q2.RealPart*Q1.ImagPart.X
                     + Q1.ImagPart.Y*Q2.ImagPart.Z
                     - Q1.ImagPart.Z*Q2.ImagPart.Y;
  Result.ImagPart.Y:=Q1.RealPart*Q2.ImagPart.Y
                     + Q2.RealPart*Q1.ImagPart.Y
                     + Q1.ImagPart.Z*Q2.ImagPart.X
                     - Q1.ImagPart.X*Q2.ImagPart.Z;
  Result.ImagPart.Z:=Q1.RealPart*Q2.ImagPart.Z
                     + Q2.RealPart*Q1.ImagPart.Z
                     + Q1.ImagPart.X*Q2.ImagPart.Y
                     - Q1.ImagPart.Y*Q2.ImagPart.X;
end;


function TQuaternion3D.Length: Single;

begin
  Result:=Sqrt(Sqr(ImagPart.X)+Sqr(ImagPart.Y)+Sqr(ImagPart.Z)+Sqr(RealPart));
end;


function TQuaternion3D.Normalize: TQuaternion3D;

var
  Len : Single;

begin
  Len:=Length;
  If SameValue(Len,0,Epsilon) then
    Result:=TQuaternion3D.Identity
  else
    begin
    Len:=1/Len;
    Result.ImagPart:=ImagPart*Len;
    Result.RealPart:=RealPart*Len;
    end;
end;

function TQuaternion3D.ToString: string;
begin
  WriteStr(Result,'(',Display(RealPart),',i ',Display(ImagPart.X),',j ',Display(ImagPart.Y),',k ',Display(ImagPart.Z),')');
end;

{ ---------------------------------------------------------------------
  Auxiliary functions
  ---------------------------------------------------------------------}


function Vector(const X, Y: Single; const W: Single = 1.0): TVector;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.W:=W;
end;


function Vector(const P: TPointF; const W: Single = 1.0): TVector;
begin
  Result.X:=P.X;
  Result.Y:=P.Y;
  Result.W:=W;
end;

function Vector3D(const X, Y, Z: Single; const W: Single = 1.0): TVector3D;

begin
  Result:=TVector3D.Create(X,Y,Z,W);
end;


function Vector3D(const P: TPoint3D; const W: Single = 1.0): TVector3D;

begin
  Result:=TVector3D.Create(P,W);
end;


function Point3D(const X, Y, Z: Single): TPoint3D;

begin
  Result:=TPoint3D.Create(X,Y,Z);
end;


function Point3D(const aVector3D: TVector3D; const aTransform: Boolean): TPoint3D;

begin
  Result:=AVector3D.ToPoint3D(aTransform);
end;


function PointF(const V: TVector): TPointF;

begin
  Result:=TPointF(V);
end;

end.
