{ Unit matrix: vectors and matrices are records without hidden fields, so their
  size is that of a plain record with the same data, the init procedures fill
  the data and the operators work on them. }
program tmatrix1;

{$mode objfpc}

uses
  matrix;

type
  TPlainV2s = record data: Tvector2_single_data; end;
  TPlainV2d = record data: Tvector2_double_data; end;
  TPlainV2e = record data: Tvector2_extended_data; end;
  TPlainV3s = record data: Tvector3_single_data; end;
  TPlainV3d = record data: Tvector3_double_data; end;
  TPlainV3e = record data: Tvector3_extended_data; end;
  TPlainV4d = record data: Tvector4_double_data; end;
  TPlainV4e = record data: Tvector4_extended_data; end;
  TPlainM2s = record data: Tmatrix2_single_data; end;
  TPlainM2d = record data: Tmatrix2_double_data; end;
  TPlainM2e = record data: Tmatrix2_extended_data; end;
  TPlainM3s = record data: Tmatrix3_single_data; end;
  TPlainM3d = record data: Tmatrix3_double_data; end;
  TPlainM3e = record data: Tmatrix3_extended_data; end;
  TPlainM4s = record data: Tmatrix4_single_data; end;
  TPlainM4e = record data: Tmatrix4_extended_data; end;

var
  Errors: Integer = 0;

// Report error aCode with aMsg when aCondition is false.
procedure Check(aCondition: Boolean; aCode: Integer; const aMsg: string);

begin
  if not aCondition then
    begin
    Writeln('Error ',aCode,': ',aMsg);
    Inc(Errors);
    end;
end;


// True if aValue and aExpected differ by less than 1e-5.
function Same(aValue, aExpected: Double): Boolean;

begin
  Result:=Abs(aValue-aExpected)<1e-5;
end;


// Check that each type has the size of a plain record with its data.
procedure TestSizes;

begin
  Check(SizeOf(Tvector4_single)=16,1,'SizeOf(Tvector4_single)=16');
  Check(SizeOf(Tmatrix4_double)=128,2,'SizeOf(Tmatrix4_double)=128');
  Check(SizeOf(Tvector2_single)=SizeOf(TPlainV2s),3,'Tvector2_single');
  Check(SizeOf(Tvector2_double)=SizeOf(TPlainV2d),4,'Tvector2_double');
  Check(SizeOf(Tvector2_extended)=SizeOf(TPlainV2e),5,'Tvector2_extended');
  Check(SizeOf(Tvector3_single)=SizeOf(TPlainV3s),6,'Tvector3_single');
  Check(SizeOf(Tvector3_double)=SizeOf(TPlainV3d),7,'Tvector3_double');
  Check(SizeOf(Tvector3_extended)=SizeOf(TPlainV3e),8,'Tvector3_extended');
  Check(SizeOf(Tvector4_double)=SizeOf(TPlainV4d),9,'Tvector4_double');
  Check(SizeOf(Tvector4_extended)=SizeOf(TPlainV4e),10,'Tvector4_extended');
  Check(SizeOf(Tmatrix2_single)=SizeOf(TPlainM2s),11,'Tmatrix2_single');
  Check(SizeOf(Tmatrix2_double)=SizeOf(TPlainM2d),12,'Tmatrix2_double');
  Check(SizeOf(Tmatrix2_extended)=SizeOf(TPlainM2e),13,'Tmatrix2_extended');
  Check(SizeOf(Tmatrix3_single)=SizeOf(TPlainM3s),14,'Tmatrix3_single');
  Check(SizeOf(Tmatrix3_double)=SizeOf(TPlainM3d),15,'Tmatrix3_double');
  Check(SizeOf(Tmatrix3_extended)=SizeOf(TPlainM3e),16,'Tmatrix3_extended');
  Check(SizeOf(Tmatrix4_single)=SizeOf(TPlainM4s),17,'Tmatrix4_single');
  Check(SizeOf(Tmatrix4_extended)=SizeOf(TPlainM4e),18,'Tmatrix4_extended');
end;


// Check that an array of vectors can be read as an array of floats.
procedure TestLayout;

var
  lVectors: array[0..1] of Tvector4_single;
  lFloats: PSingle;
  I: Integer;

begin
  lVectors[0].init(1,2,3,4);
  lVectors[1].init(5,6,7,8);
  lFloats:=PSingle(@lVectors);
  for I:=0 to 7 do
    Check(lFloats[I]=I+1,20+I,'Float at index of array of Tvector4_single');
end;


// Check the init procedures of vectors and matrices.
procedure TestInit;

var
  lVec: Tvector3_double;
  lMat: Tmatrix3_single;
  I, J: Integer;

begin
  lVec.init(1,2,3);
  Check((lVec.data[0]=1) and (lVec.data[1]=2) and (lVec.data[2]=3),30,'Tvector3_double.init');
  lVec.init_zero;
  Check((lVec.data[0]=0) and (lVec.data[1]=0) and (lVec.data[2]=0),31,'Tvector3_double.init_zero');
  lVec.init_one;
  Check((lVec.data[0]=1) and (lVec.data[1]=1) and (lVec.data[2]=1),32,'Tvector3_double.init_one');
  lMat.init_identity;
  for I:=0 to 2 do
    for J:=0 to 2 do
      Check(lMat.data[I,J]=Ord(I=J),33,'Tmatrix3_single.init_identity');
  lMat.init(1,2,3,4,5,6,7,8,9);
  Check((lMat.data[0,0]=1) and (lMat.data[1,2]=6) and (lMat.data[2,1]=8),34,'Tmatrix3_single.init');
  lMat.init_zero;
  for I:=0 to 2 do
    for J:=0 to 2 do
      Check(lMat.data[I,J]=0,35,'Tmatrix3_single.init_zero');
end;


// Check a selection of vector and matrix operators.
procedure TestOperators;

var
  lA, lB, lC: Tvector3_single;
  lD: Tvector3_double;
  lV4: Tvector4_single;
  lM, lRot: Tmatrix2_double;
  lV2: Tvector2_double;

begin
  lA.init(1,0,0);
  lB.init(0,1,0);
  lC:=lA><lB;
  Check((lC.data[0]=0) and (lC.data[1]=0) and (lC.data[2]=1),40,'Cross product');
  Check(lA**lB=0,41,'Inner product of orthogonal vectors');
  lC:=(lA+lB)*2;
  Check((lC.data[0]=2) and (lC.data[1]=2) and (lC.data[2]=0),42,'Vector addition and scaling');
  lC:=lA-lB;
  Check((lC.data[0]=1) and (lC.data[1]=-1),43,'Vector subtraction');
  Check(Same(lC.length,Sqrt(2)),44,'Vector length');
  lD:=lC;
  Check((lD.data[0]=1) and (lD.data[1]=-1) and (lD.data[2]=0),45,'Single to double vector conversion');
  lV4:=lC;
  Check((lV4.data[0]=1) and (lV4.data[1]=-1) and (lV4.data[3]=0),46,'Vector3 to vector4 conversion');
  lRot.init(0,-1,1,0);
  lV2.init(1,0);
  lV2:=lRot*lV2;
  Check((lV2.data[0]=0) and (lV2.data[1]=1),47,'Matrix times vector');
  lM:=lRot*lRot.transpose;
  Check((lM.data[0,0]=1) and (lM.data[0,1]=0) and (lM.data[1,0]=0) and (lM.data[1,1]=1),48,
    'Rotation times its transpose is identity');
  Check(lRot.determinant=1,49,'Determinant');
end;


begin
  TestSizes;
  TestLayout;
  TestInit;
  TestOperators;
  if Errors<>0 then
    Halt(1);
  Writeln('ok');
end.
