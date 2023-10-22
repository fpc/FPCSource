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

unit utcpoint;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, types, utmathvectorbase, system.math.vectors;

type
    { TTestPoint3D }

  TTestPoint3D = Class(TCMathVectorsBase)
  Private
    FP : Array[1..3] of TPoint3D;
    procedure ClearPoints;
    function GetP(aIndex: Integer): TPoint3D;
    procedure SetP(aIndex: Integer; aValue: TPoint3D);
  Protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property P1 : TPoint3D Index 1 Read GetP Write SetP;
    Property P2 : TPoint3D Index 2 Read GetP Write SetP;
    Property P3 : TPoint3D Index 3 Read GetP Write SetP;
  Published
    procedure TestHookUp;
    Procedure TestZero;
    Procedure TestCreate;
    Procedure TestCreate2DPointAndZ;
    Procedure TestCreateVector3D;
    Procedure TestAdd;
    Procedure TestMultiplyFactor;
    Procedure TestMultiply;
    Procedure TestSubtract;
    Procedure TestEqual;
    Procedure TestDivide;
    Procedure TestUnEqual;
    Procedure TestUnaryMinus;
    Procedure TestAngleCosine;
    Procedure TestCrossProduct;
    Procedure TestDistance;
    Procedure TestDotProduct;
    Procedure TestEqualsTo;
    Procedure TestLength;
    Procedure TestMidPoint;
    Procedure TestNormalize;
    Procedure TestOffsetPoint;
    Procedure TestOffsetDelta;
    Procedure TestReflect;
    Procedure TestRotate;
  end;


implementation
{ TTestPoint3D }

procedure TTestPoint3D.ClearPoints;

var
  I : integer;

begin
  For I:=1 to 3 do
    begin
    FP[I].X:=0;
    FP[I].Y:=0;
    FP[I].Z:=0;
    end;
end;

function TTestPoint3D.GetP(aIndex: Integer): TPoint3D;
begin
  Result.X:=FP[AIndex].X;
  Result.Y:=FP[AIndex].Y;
  Result.Z:=FP[AIndex].Z;
end;

procedure TTestPoint3D.SetP(aIndex: Integer; aValue: TPoint3D);
begin
  FP[AIndex].X:=aValue.X;
  FP[AIndex].Y:=aValue.Y;
  FP[AIndex].Z:=aValue.Z;
end;

procedure TTestPoint3D.SetUp;
begin
  inherited SetUp;
  ClearPoints;
end;

procedure TTestPoint3D.TearDown;
begin
  inherited TearDown;
end;

procedure TTestPoint3D.TestHookUp;
var
  I : Integer;
begin
  For I:=1 to 3 do
    begin
    AssertEquals('Point3D '+intTostr(i)+'.X',0.0,FP[I].X);
    AssertEquals('Point3D '+intTostr(i)+'.Y',0.0,FP[I].Y);
    AssertEquals('Point3D '+intTostr(i)+'.Z',0.0,FP[I].Z);
    end;
end;

procedure TTestPoint3D.TestZero;
begin
  P1:=TPoint3D.Zero;
  AssertPoint3D('Zero',0,0,0,P1);
end;

procedure TTestPoint3D.TestCreate;
begin
  P1:=TPoint3D.Create(1,2,3);
  AssertPoint3D('Create',1,2,3,P1);
end;

procedure TTestPoint3D.TestCreate2DPointAndZ;
begin
  P1:=TPoint3D.Create(PointF(1,2),3);
  AssertPoint3D('Create',1,2,3,P1);
end;

procedure TTestPoint3D.TestCreateVector3D;
begin
  P1:=TPoint3D.Create([1,2,3,4]);
  AssertPoint3D('Create',1,2,3,P1);
end;

procedure TTestPoint3D.TestAdd;
begin
  P1:=TPoint3D.Create(1,2,3);
  P2:=TPoint3D.Create(5,4,3);
  P3:=P2+P1;
  AssertPoint3D('Add 1',6,6,6,P3);
  P2:=TPoint3D.Create(7,8,9);
  P3:=P2+P1;
  AssertPoint3D('Add 2',8,10,12,P3);
end;

procedure TTestPoint3D.TestMultiplyFactor;
begin
  P1:=TPoint3D.Create(1,2,3);
  P2:=P1*3;
  AssertPoint3D('Factor 1',3,6,9,P2);
  P2:=2*P1;
  AssertPoint3D('Factor 2',2,4,6,P2);
end;

procedure TTestPoint3D.TestMultiply;
begin
  P1:=TPoint3D.Create(1,2,3);
  P2:=TPoint3D.Create(5,4,3);
  P3:=P2*P1;
  AssertPoint3D('Create',5,8,9,P3);
end;

procedure TTestPoint3D.TestSubtract;
begin
  P1:=TPoint3D.Create(1,2,3);
  P2:=TPoint3D.Create(5,4,3);
  P3:=P2-P1;
  AssertPoint3D('Subtract 1',4,2,0,P3);
  P2:=TPoint3D.Create(7,8,9);
  P3:=P2-P1;
  AssertPoint3D('Subtract 2',6,6,6,P3);
end;

procedure TTestPoint3D.TestEqual;
begin
  P1:=TPoint3D.Create(1,2,3);
  P2:=TPoint3D.Create(5,4,3);
  AssertFalse('Equal NOk',P1=P2);
  P2:=TPoint3D.Create(1,2,3);
  AssertTrue('Equal OK',P1=P2);
end;

procedure TTestPoint3D.TestDivide;
begin
  P1:=TPoint3D.Create(3,6,9);
  P2:=P1/3;
  AssertPoint3D('Factor 1',1,2,3,P2);
end;

procedure TTestPoint3D.TestUnEqual;
begin
  P1:=TPoint3D.Create(1,2,3);
  P2:=TPoint3D.Create(5,4,3);
  AssertTrue('unEqual Ok',P1<>P2);
  P2:=TPoint3D.Create(1,2,3);
  AssertFalse('unEqual OK',P1<>P2);
end;

procedure TTestPoint3D.TestUnaryMinus;
begin
  P1:=TPoint3D.Create(1,2,3);
  P2:=-P1;
  AssertPoint3D('Unary minus',-1,-2,-3,P2);
end;

procedure TTestPoint3D.TestAngleCosine;
begin
  P1:=TPoint3D.Create(1,0,0);
  P2:=TPoint3D.Create(0,1,0);
  AssertEquals('Angle',0, P1.AngleCosine(P2));
  P1:=TPoint3D.Create(1,0,0);
  P2:=TPoint3D.Create(0,0,1);
  AssertEquals('Angle',0, P1.AngleCosine(P2));
  P1:=TPoint3D.Create(0,1,0);
  P2:=TPoint3D.Create(0,0,1);
  AssertEquals('Angle',0, P1.AngleCosine(P2));
  P1:=TPoint3D.Create(0,1,0);
  P2:=TPoint3D.Create(1,1,0);
  AssertEquals('Angle',c45, P1.AngleCosine(P2));
end;

procedure TTestPoint3D.TestCrossProduct;
begin
  P1:=TPoint3D.Create(1,1,0);
  P2:=TPoint3D.Create(2,2,0);
  P3:=P2.CrossProduct(P1);
  AssertPoint3D('T1',0,0,0,P3);
  P1:=TPoint3D.Create(1,1,0);
  P2:=TPoint3D.Create(2,2,0);
  P3:=P2.CrossProduct(P1);
  AssertPoint3D('T2',0,0,0,P3);
end;

procedure TTestPoint3D.TestDistance;
begin
  P1:=TPoint3D.Create(1,1,0);
  P2:=TPoint3D.Create(1,2,0);
  AssertEquals('Dist 1,',1,P1.Distance(P2));
  P1:=TPoint3D.Create(0,1,1);
  P2:=TPoint3D.Create(0,2,1);
  AssertEquals('Dist 2,',1,P1.Distance(P2));
  P1:=TPoint3D.Create(0,0,0);
  P2:=TPoint3D.Create(1,1,0);
  AssertEquals('Dist 3,',c45*2,P1.Distance(P2));
end;

procedure TTestPoint3D.TestDotProduct;
begin
  P1:=TPoint3D.Create(3,4,9);
  P2:=TPoint3D.Create(3,4,9);
  AssertEquals('Test 1',9+16+81,P1.DotProduct(P2));
  P2:=TPoint3D.Create(1,1,0);
  AssertEquals('Test 1',7,P1.DotProduct(P2));
end;

procedure TTestPoint3D.TestEqualsTo;
begin
  P1:=TPoint3D.Create(3,4,9);
  P2:=TPoint3D.Create(3,4,9);
  AssertTrue('Test 1',P1.EqualsTo(P2));
  P2:=TPoint3D.Create(1,1,1);
  AssertFalse('Test 2',P1.EqualsTo(P2));
end;

procedure TTestPoint3D.TestLength;
begin
  P1:=TPoint3D.Create(1,0,0);
  AssertEquals('Dist 1,',1,P1.Length);
  P1:=TPoint3D.Create(0,1,1);
  AssertEquals('Dist 2,',c45*2,P1.Length);
  P1:=TPoint3D.Create(1,1,1);
  AssertEquals('Dist 3,',1.7321,P1.Length);
end;

procedure TTestPoint3D.TestMidPoint;
begin
  P1:=TPoint3D.Create(0,0,1);
  P2:=TPoint3D.Create(0,2,1);
  AssertPoint3D('Midpoint1',0,1,1,P1.MidPoint(P2));
  P1:=TPoint3D.Create(1,0,0);
  P2:=TPoint3D.Create(0,1,0);
  AssertPoint3D('Midpoint2',0.5,0.5,0,P1.MidPoint(P2));
end;

procedure TTestPoint3D.TestNormalize;
begin
  P1:=TPoint3D.Create(1,1,1);
  P2:=P1.Normalize;
  AssertPoint3D('Normalize 1',0.5773,0.5773,0.5773,P2);
  P1:=TPoint3D.Create(0,0,1);
  P2:=P1.Normalize;
  AssertPoint3D('Normalize 2',0,0,1,P2);
  P1:=TPoint3D.Create(1,0,0);
  P2:=P1.Normalize;
  AssertPoint3D('Normalize 3',1,0,0,P2);
end;

procedure TTestPoint3D.TestOffsetPoint;

Var
  P : TPoint3D; // Cannot use property

begin
  P:=TPoint3D.Create(1,2,3);
  P.Offset(Point3D(4,5,6));
  AssertPoint3D('Off',5,7,9,P);
end;

procedure TTestPoint3D.TestOffsetDelta;

Var
  P : TPoint3D; // Cannot use property

begin
  P:=TPoint3D.Create(0,0,0);
  P.Offset(1,2,3);
  AssertPoint3D('Off',1,2,3,P);
end;

procedure TTestPoint3D.TestReflect;

// P2 is normal of reflecting surface: X,Y plane.

begin
  P1:=TPoint3D.Create(0,0,1);
  P2:=TPoint3D.Create(0,0,1);
  P3:=P1.Reflect(P2);
  AssertPoint3D('Reflect 1',0,0,-1,P3);
  P1:=TPoint3D.Create(0,-1,-1);
  P3:=P1.Reflect(P2);
  AssertPoint3D('Reflect 2',0,-1,1,P3);
end;

procedure TTestPoint3D.TestRotate;
begin
  P1:=TPoint3D.Create(1,0,0);
  P2:=TPoint3D.Create(0,0,1); // Z-axis
  P3:=P1.Rotate(P2,Pi/2);
  AssertPoint3D('Rotate 1',0,1,0,P3);
  P2:=TPoint3D.Create(0,1,0); // Y-axis
  P3:=P1.Rotate(P2,Pi/2);
  AssertPoint3D('Rotate 2',0,0,-1,P3); // Z is pointing down!
end;

initialization
  RegisterTests([TTestPoint3D]);
end.

