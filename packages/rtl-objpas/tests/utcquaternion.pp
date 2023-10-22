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

unit utcquaternion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, utmathvectorbase, types, system.math.vectors;

Type
  { TTestQuaternion3D }

  TTestQuaternion3D = class(TCMathVectorsBase)
  Private
    FQ : Array[1..3] of TQuaternion3D;
    procedure Clear;
    function GetQ(AIndex: Integer): TQuaternion3D;
    procedure SetQ(AIndex: Integer; AValue: TQuaternion3D);
  Protected
    procedure SetUp; override;
    procedure TearDown; override;
    Property Q1 : TQuaternion3D Index 1 Read GetQ Write SetQ;
    Property Q2 : TQuaternion3D Index 2 Read GetQ Write SetQ;
    Property Q3 : TQuaternion3D Index 3 Read GetQ Write SetQ;
  Published
    procedure TestHookUp;
    procedure TestCreateVectorAngle;
    procedure TestCreateVectorVector;
    procedure TestCreateYawPitchRoll;
    procedure TestCreateMatrix;
    procedure TestAssignToMatrix;
    procedure TestMultiply;
  end;

implementation

{ TTestQuaternion3D }

procedure TTestQuaternion3D.Clear;
begin

end;

function TTestQuaternion3D.GetQ(AIndex: Integer): TQuaternion3D;
begin
  Result:=FQ[aIndex];
end;

procedure TTestQuaternion3D.SetQ(AIndex: Integer; AValue: TQuaternion3D);
begin
  FQ[aIndex]:=aValue;
end;

procedure TTestQuaternion3D.SetUp;
begin
  inherited SetUp;
  Clear;
end;

procedure TTestQuaternion3D.TearDown;
begin
  Clear;
  inherited TearDown;
end;

procedure TTestQuaternion3D.TestHookUp;

var
  I,J : Integer;

begin
  For I:=1 to 3 do
    begin
    AssertEquals(Format('Q%d.Real',[I]),0.0,FQ[I].RealPart);
    For J:=0 to 2 do
      AssertEquals(Format('Q%d.ImagPart[%d]',[I,J]),0.0,FQ[I].ImagPart.V[J]);
    end;
end;

Procedure TTestQuaternion3D.TestCreateVectorAngle;
begin
  // Test 202
  Q1:=TQuaternion3D.Create(Point3D(1,0,0),Pi/2);
  AssertQuaternion('Create 2', 0.7071, 0.7071,  0.0000, 0.0000, Q1);
  // Test 203
  Q1:=TQuaternion3D.Create(Point3D(0,1,0),Pi/2);
  AssertQuaternion('Create 3', 0.7071, 0.0000, 0.7071, 0.0000, Q1);
  // Test 204
  Q1:=TQuaternion3D.Create(Point3D(0,0,1),Pi/2);
  AssertQuaternion('Create 4', 0.7071, 0.0000, 0.0000, 0.7071, Q1);
end;

Procedure TTestQuaternion3D.TestCreateVectorVector;

begin
  // Test 201
  Q1:=TQuaternion3D.Create(Point3D(1,0,0),Point3D(0,1,0));
  AssertQuaternion('Create 1', 0.7071,  0.0000,  0.0000, 1.0000, Q1);
end;

Procedure TTestQuaternion3D.TestCreateYawPitchRoll;

begin
  // Test 205
  Q1:=TQuaternion3D.Create(Pi/2,0,0);
  AssertQuaternion('Create 1', 0.7071,  0.0000, 0.7071, 0.0000, Q1);
  // Test 206
  Q1:=TQuaternion3D.Create(0,Pi/2,0);
  AssertQuaternion('Create 2', 0.7071,  0.7071, 0.0000, 0.0000, Q1);
  // Test 207
  Q1:=TQuaternion3D.Create(0,0,Pi/2);
  AssertQuaternion('Create 3', 0.7071,  0.0000,  0.0000, 0.7071, Q1);
end;

procedure TTestQuaternion3D.TestCreateMatrix;

var
  M : TMatrix3D;

begin
  M:=TMatrix3D.CreateRotationX(pi/2);
  Q1:=TQuaternion3D.Create(M);
  AssertQuaternion('Create 1', 0.7071, 0.7071, 0.0000, 0.0000, Q1);
  M:=TMatrix3D.CreateRotationY(pi/2);
  Q1:=TQuaternion3D.Create(M);
  AssertQuaternion('Create 2', 0.7071, 0.0000, 0.7071, 0.0000, Q1);
  M:=TMatrix3D.CreateRotationZ(pi/2);
  Q1:=TQuaternion3D.Create(M);
  AssertQuaternion('Create 2', 0.7071, 0.0000, 0.0000, 0.7071, Q1);
end;

procedure TTestQuaternion3D.TestAssignToMatrix;

var
  M,M2 : TMatrix3D;

begin
  M:=TMatrix3D.CreateRotationX(pi/2);
  Q1:=TQuaternion3D.Create(M);
  M2:=Q1;
  AssertMatrix3D('Assign',M,M2);
end;

procedure TTestQuaternion3D.TestMultiply;
begin
  // test 208
  Q1:=TQuaternion3D.Create(0,0,Pi/2);
  Q2:=TQuaternion3D.Create(0,Pi/2,0);
  Q3:=Q1*Q2;
  AssertQuaternion('Multiply 1', 0.5,0.5,0.5,0.5, Q3);
  // test 209
  Q1:=TQuaternion3D.Create(Point3D(1,0,0),Pi/2);
  Q2:=TQuaternion3D.Create(Point3D(0,1,0),Pi/2);
  Q3:=Q1*Q2;
  AssertQuaternion('Multiply 2', 0.5,0.5,0.5,0.5, Q3);
end;


Initialization
  RegisterTest(TTestQuaternion3D);

end.

