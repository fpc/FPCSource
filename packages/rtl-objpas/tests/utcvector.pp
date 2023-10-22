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
unit utcvector;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, types, utmathvectorbase, system.math.vectors;

Type

  { TTestVector }

  TTestVector = class(TCMathVectorsBase)
  Private
    FV : Array[1..3] of TVector;
    procedure ClearVectors;
    function GetV(AIndex: Integer): TVector;
    procedure SetV(AIndex: Integer; AValue: TVector);
  Protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property V1 : TVector Index 1 Read GetV Write SetV;
    Property V2 : TVector Index 2 Read GetV Write SetV;
    Property V3 : TVector Index 3 Read GetV Write SetV;
  Published
    procedure TestHookUp;
    Procedure TestZero;
    Procedure TestCreate;
    Procedure TestCreateW;
    Procedure TestAssign;
    Procedure TestAssignPointf;
    Procedure TestAssignToPointf;
    Procedure TestAdd;
    Procedure TestMultiplyFactor;
    Procedure TestDivide;
    Procedure TestEqual;
    Procedure TestNotEqual;
    Procedure TestSubtract;
    Procedure TestLength;
    Procedure TestNormalize;
    Procedure TestCrossProduct;
    Procedure TestDotProduct;
    Procedure TestToPointF;
  end;

implementation
{ TTestVector }

function TTestVector.GetV(AIndex: Integer): TVector;
begin
  Result:=FV[aIndex];
end;

procedure TTestVector.SetV(AIndex: Integer; AValue: TVector);
begin
  FV[aIndex]:=aValue;
end;

procedure TTestVector.ClearVectors;

var
  I : integer;

begin
  For I:=1 to 3 do
    begin
    FV[I].X:=0;
    FV[I].Y:=0;
    FV[I].W:=0;
    end;
end;



procedure TTestVector.SetUp;
begin
  inherited SetUp;
  ClearVectors;
end;

procedure TTestVector.TearDown;
begin
  inherited TearDown;
end;

procedure TTestVector.TestHookUp;
var
  I : Integer;
begin
  For I:=1 to 3 do
    begin
    AssertEquals('Vector '+intTostr(i)+'.X',0.0,FV[I].X);
    AssertEquals('Vector '+intTostr(i)+'.Y',0.0,FV[I].Y);
    AssertEquals('Vector '+intTostr(i)+'.W',0.0,FV[I].W);
    end;
end;

procedure TTestVector.TestZero;
begin
  V1:=TVector.Zero;
  AssertEquals('Vector.X',0.0,V1.X);
  AssertEquals('Vector.Y',0.0,V1.Y);
  AssertEquals('Vector.W',0.0,V1.W);
end;

procedure TTestVector.TestCreate;
begin
  V1:=TVector.Create(1,2);
  AssertEquals('Vector.X',1.0,V1.X);
  AssertEquals('Vector.Y',2.0,V1.Y);
  AssertEquals('Vector.W',DefaultVectorWidth,V1.W);
end;

procedure TTestVector.TestCreateW;
begin
  V1:=TVector.Create(1,2,3);
  AssertVector('Vector',1,2,3,V1);
end;

procedure TTestVector.TestAssign;
begin
  V2:=TVector.Create(1,2,3);
  V1:=V2;
  AssertVector('Assign',1,2,3,V1);
end;

procedure TTestVector.TestAssignPointf;

var
  P : TPointF;

begin
  P:=PointF(1,2);
  V1:=P;
  AssertVector('Vector',1,2,DefaultVectorWidth,V1);
end;

procedure TTestVector.TestAssignToPointf;
Var
  P : TPointF;

begin
  V1:=TVector.Create(1,2,3);
  P:=V1;
  AssertEquals('Assign 1',PointF(0.3333,0.6666),P);
  V1:=TVector.Create(1,2,1);
  P:=V1;
  AssertEquals('Assign 2',PointF(1,2),P);
  V1:=TVector.Create(1,2,0);
  P:=V1;
  AssertEquals('Assign 3',PointF(1,2),P);
end;

procedure TTestVector.TestAdd;

begin
  V1:=TVector.Create(1,2,3);
  V2:=TVector.Create(6,5,4);
  V3:=V1+V2;
  AssertVector('Vector',7,7,7,V3);
end;

procedure TTestVector.TestMultiplyFactor;
begin
  V1:=TVector.Create(1,2,3);
  V2:=V1*3;
  AssertVector('Vector 1',3,6,9,V2);
  V2:=3*V1;
  AssertVector('Vector 2',3,6,9,V2);
end;

procedure TTestVector.TestDivide;
begin
  V1:=TVector.Create(1,2,3);
  V2:=V1/3;
  AssertVector('Vector 1',0.3333,0.6666,1,V2);
end;

procedure TTestVector.TestEqual;
begin
  V1:=TVector.Create(1,2,3);
  V2:=TVector.Create(1,2,3);
  AssertTrue('Equal 1',V1=V2);
  V2:=TVector.Create(3,2,1);
  AssertFalse('Equal 2',V1=V2);
  V2:=TVector.Create(1+TEpsilon.Vector*0.99,2,3);
  AssertTrue('Equal within precision',V1=V2);
  V2:=TVector.Create(1+TEpsilon.Vector*1.1,2,3);
  AssertFalse('Unequal outside precision',V1=V2);
end;

procedure TTestVector.TestNotEqual;
begin
  V1:=TVector.Create(1,2,3);
  V2:=TVector.Create(1,2,3);
  AssertFalse('Not Equal',V1<>V2);
  V2:=TVector.Create(3,2,1);
  AssertTrue('Equal',V1<>V2);
  V2:=TVector.Create(1+TEpsilon.Vector*0.99,2,3);
  AssertFalse('Equal within precision',V1<>V2);
  V2:=TVector.Create(1+TEpsilon.Vector*1.1,2,3);
  AssertTrue('Unequal outside precision',V1<>V2);
end;

procedure TTestVector.TestSubtract;
begin
  V1:=TVector.Create(1,2,3);
  V2:=TVector.Create(6,5,4);
  V3:=V2-V1;
  AssertVector('Vector',5,3,1,V3);
end;

procedure TTestVector.TestLength;
begin
  V1:=TVector.Create(3,4,0);
  AssertEquals('Length 1',5,V1.Length);
  V1:=TVector.Create(3,4,1);
  AssertEquals('Length 1',Sqrt(26),V1.Length,TEpsilon.Vector);
end;

procedure TTestVector.TestNormalize;
begin
  V1:=TVector.Create(3,4,0);
  V2:=V1.Normalize;
  AssertVector('No width',3/5,4/5,0,V2);
  AssertEquals('Length 1',1,V2.Length,TEpsilon.Vector);
  V1:=TVector.Create(3,4,1);
  V2:=V1.Normalize;
  AssertVector('No width',3/Sqrt(26),4/Sqrt(26),1/Sqrt(26),V2);
  AssertEquals('Length 1',1,V2.Length,TEpsilon.Vector);
end;

procedure TTestVector.TestCrossProduct;
begin
  V1:=TVector.Create(1,1,0);
  V2:=TVector.Create(2,2,0);
  V3:=V2.CrossProduct(V1);
  AssertVector('T1',0,0,0,V3);
  V1:=TVector.Create(1,1,0);
  V2:=TVector.Create(2,2,0);
  V3:=V2.CrossProduct(V1);
  AssertVector('T1',0,0,0,V3);
end;

procedure TTestVector.TestDotProduct;
begin
  V1:=TVector.Create(3,4,9);
  V2:=TVector.Create(3,4,9);
  AssertEquals('Test 1',9+16+81,V1.DotProduct(V2));
  V2:=TVector.Create(1,1,0);
  AssertEquals('Test 1',7,V1.DotProduct(V2));
end;

procedure TTestVector.TestToPointF;

var
  P : TPointF;

begin
  V1:=TVector.Create(1,2,3);
  P:=V1.ToPointF;
  AssertEquals('ToPointF 1',PointF(0.3333,0.6666),P);
  V1:=TVector.Create(1,2,1);
  P:=V1.ToPointF;
  AssertEquals('ToPointF 2',PointF(1,2),P);
end;


initialization
  RegisterTest(TTestVector);
end.

