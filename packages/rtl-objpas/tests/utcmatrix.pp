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
unit utcmatrix;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, utmathvectorbase, types, system.math.vectors;

Type

  { TTestMatrix }

  TTestMatrix = class(TCMathVectorsBase)
  Private
    FM : Array[1..3] of TMatrix;
    procedure ClearMatrices;
    function GetM(AIndex: Integer): TMatrix;
    procedure SetM(AIndex: Integer; AValue: TMatrix);
  Protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property M1 : TMatrix Index 1 Read GetM Write SetM;
    Property M2 : TMatrix Index 2 Read GetM Write SetM;
    Property M3 : TMatrix Index 3 Read GetM Write SetM;
  Published
    procedure TestHookUp;
    Procedure TestCreateRotation;
    Procedure TestCreateScaling;
    Procedure TestCreateTranslation;
    Procedure TTestEqual;
    Procedure TTestMultiply;
    Procedure TTestMultiplyPointf;
    Procedure TTestMultiplyVector;
    Procedure TTestMultiplyPoint3D;
    Procedure TTestMultiplyFactor1;
    Procedure TTestMultiplyFactor2;
    Procedure TTestDiv;
    Procedure TTestAdjoint;
    Procedure TTestDeterminant;
    Procedure TTestEqualsTo;
    Procedure TTestExtractScale;
    Procedure TTestInverse;
    Procedure TTestScale;
  end;


implementation

{ TTestMatrix }

procedure TTestMatrix.ClearMatrices;

var
  I : integer;

begin
  For I:=1 to 3 do
    FM[I]:=Default(TMatrix);
end;

function TTestMatrix.GetM(AIndex: Integer): TMatrix;
begin
  Result:=FM[aIndex];
end;

procedure TTestMatrix.SetM(AIndex: Integer; AValue: TMatrix);
begin
  FM[aIndex]:=aValue;
end;

procedure TTestMatrix.SetUp;
begin
  inherited SetUp;
  ClearMatrices;
end;

procedure TTestMatrix.TearDown;
begin
  inherited TearDown;
  ClearMatrices;
end;

procedure TTestMatrix.TestHookUp;

var
  I,C,R : Integer;
begin
  For I:=1 to 3 do
    For R:=0 to 2 do
      For C:=0 to 2 do
       AssertEquals(Format('M%d[%d,%d]',[I,C,R]),0.0,FM[I].M[R].V[C]);
end;

procedure TTestMatrix.TestCreateRotation;

begin
  M1:=TMatrix.CreateRotation(0);
  AssertMatrix('Create 1',[1,0,0,
                           0,1,0,
                           0,0,1],M1);
  M1:=TMatrix.CreateRotation(Pi/2);
  AssertMatrix('Create 2',[0,1,0,
                           -1,0,0,
                           0,0,1],M1);
  M1:=TMatrix.CreateRotation(Pi);
  AssertMatrix('Create 3',[-1,0,0,
                           0,-1,0,
                           0,0,1],M1);
end;

procedure TTestMatrix.TestCreateScaling;
begin
  M1:=TMatrix.CreateScaling(2,2);
  AssertMatrix('Create 1',[2,0,0,
                           0,2,0,
                           0,0,1],M1);
  M1:=TMatrix.CreateScaling(2,3);
  AssertMatrix('Create 2',[2,0,0,
                           0,3,0,
                           0,0,1],M1);
end;

procedure TTestMatrix.TestCreateTranslation;
begin
  M1:=TMatrix.CreateTranslation(2,3);
  AssertMatrix('Create 1',[1,0,0,
                           0,1,0,
                           2,3,1],M1);
  M1:=TMatrix.CreateTranslation(0,0);
  AssertMatrix('Create 2',[1,0,0,
                           0,1,0,
                           0,0,1],M1);
end;

procedure TTestMatrix.TTestEqual;
begin
  M1:=TMatrix.CreateTranslation(2,3);
  M2:=TMatrix.CreateTranslation(2,3);
  AssertTrue('Equal translation',M1=M2);
  M2:=TMatrix.CreateTranslation(0,0);
  AssertFalse('NEqual translation',M1=M2);
  M1:=TMatrix.CreateScaling(2,3);
  M2:=TMatrix.CreateScaling(2,3);
  AssertTrue('Equal Scaling',M1=M2);
  M2:=TMatrix.CreateScaling(0,0);
  AssertFalse('NEqual Scaling',M1=M2);
end;

procedure TTestMatrix.TTestMultiply;
begin
  M1:=TMatrix.CreateRotation(Pi/4);
  M2:=TMatrix.CreateRotation(Pi/4);
  M3:=M1*M2;
  AssertMatrix('Multiply',[0,1,0,
                           -1,0,0,
                           0,0,1],M3);
  M1:=TMatrix.CreateRotation(Pi/3);
  M2:=TMatrix.CreateRotation(Pi/6);
  M3:=M1*M2;
  AssertMatrix('Multiply',[0,1,0,
                           -1,0,0,
                           0,0,1],M3);
end;

procedure TTestMatrix.TTestMultiplyPointf;

Var
  P1,P2 : TPointF;

begin
  P1:=PointF(1,0);
  M1:=TMatrix.CreateRotation(Pi/2);
  P2:=P1*M1;
  AssertPointF('Multiplied',0,1,P2);
  M1:=TMatrix.CreateRotation(Pi);
  P2:=P1*M1;
  AssertPointF('Multiplied 2',-1,0,P2);
  M1:=TMatrix.CreateTranslation(3,4);
  P2:=P1*M1;
  AssertPointF('Multiplied 2',4,4,P2);
end;

procedure TTestMatrix.TTestMultiplyVector;

Var
  V1,V2 : TVector;

begin
  V1:=Vector(1,0);
  M1:=TMatrix.CreateRotation(Pi/2);
  V2:=V1*M1;
  AssertVector('Multiplied',0,1,1,V2);
  M1:=TMatrix.CreateRotation(Pi);
  V2:=V1*M1;
  AssertVector('Multiplied 2',-1,0,1,V2);
end;

procedure TTestMatrix.TTestMultiplyPoint3D;
Var
  P1,P2 : TPoint3D;

begin
  P1:=Point3D(1,0,0);
  M1:=TMatrix.CreateRotation(Pi/2);
  P2:=P1*M1;
  AssertPoint3D('Multiplied',0,1,0,P2);
  M1:=TMatrix.CreateRotation(Pi);
  P2:=P1*M1;
  AssertPoint3D('Multiplied 2',-1,0,0,P2);
end;

procedure TTestMatrix.TTestMultiplyFactor1;
begin
  M1:=TMatrix.CreateRotation(Pi/2);
  M2:=M1*2;
  AssertMatrix('Multiply',[0,2,0,
                           -2,0,0,
                           0,0,2],M2);
end;

procedure TTestMatrix.TTestMultiplyFactor2;
begin
  M1:=TMatrix.CreateRotation(Pi/2);
  M2:=2*M1;
  AssertMatrix('Multiply',[0,2,0,
                           -2,0,0,
                           0,0,2],M2);
end;

procedure TTestMatrix.TTestDiv;
begin
  M1:=TMatrix.CreateRotation(Pi/2);
  M2:=M1/2;
  AssertMatrix('Divide',[ 0,  1/2,0,
                         -1/2, 0, 0,
                          0,   0, 1/2],M2);
end;

procedure TTestMatrix.TTestAdjoint;
begin
  M1:=TMatrix.CreateRotation(Pi/2);
  M2:=M1.Adjoint;
  AssertMatrix('Adjoint 1',[0,-1,0,
                            1,0,0,
                            0,0,1],M2);
  M1:=TMatrix.CreateRotation(Pi/6);
  M2:=M1.Adjoint;
  AssertMatrix('Adjoint 2',[ 0.866, -0.5, 0.00,
                             0.50, 0.866, 0.00,
                            -0.00, 0.00, 1.00],M2);
end;

procedure TTestMatrix.TTestDeterminant;
begin
  M1:=TMatrix.CreateRotation(Pi/2);
  AssertEquals('Det 1',1,M1.Determinant);
  M1:=TMatrix.CreateRotation(Pi/3);
  AssertEquals('Det 1',1,M1.Determinant);
end;

procedure TTestMatrix.TTestEqualsTo;
begin
  M1:=TMatrix.CreateTranslation(2,3);
  M2:=TMatrix.CreateTranslation(2,3);
  AssertTrue('Equal translation',M1.EqualsTo(M2));
  M2:=TMatrix.CreateTranslation(0,0);
  AssertFalse('NEqual translation',M1.EqualsTo(M2));
  M1:=TMatrix.CreateScaling(2,3);
  M2:=TMatrix.CreateScaling(2,3);
  AssertTrue('Equal Scaling',M1.EqualsTo(M2));
  M2:=TMatrix.CreateScaling(0,0);
  AssertFalse('NEqual Scaling',M1.EqualsTo(M2));

end;

procedure TTestMatrix.TTestExtractScale;
begin
  M1:=TMatrix.CreateRotation(Pi/2);
  AssertPointF('Scale 1',1,1,M1.ExtractScale);
  M1:=M1.Scale(2);
  AssertPointF('Scale 2',2,2,M1.ExtractScale);
end;

procedure TTestMatrix.TTestInverse;
begin
  M1:=TMatrix.CreateRotation(Pi/2);
  M2:=M1.Inverse;
  M3:=TMatrix.CreateRotation(-Pi/2);
  AssertMatrix('Inverse 1',M3,M2);
  M1:=TMatrix.CreateRotation(Pi/6);
  M2:=M1.Inverse;
  M3:=TMatrix.CreateRotation(-Pi/6);
  AssertMatrix('Inverse 2',M3,M2);
end;

procedure TTestMatrix.TTestScale;


begin
  M1:=TMatrix.CreateRotation(Pi/2);
  AssertMatrix('Create',[0,1,0,
                         -1,0,0,
                         0,0,1],M1);
  M2:=M1.Scale(2);
  AssertMatrix('Scaled',[0,2,0,
                         -2,0,0,
                         0,0,2],M2);
end;


initialization
  RegisterTest(TTestMatrix);
end.

