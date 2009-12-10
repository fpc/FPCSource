(*************************************************************************
AP library
Copyright (c) 2003-2009 Sergey Bochkanov (ALGLIB project).

>>> LICENSE >>>
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation (www.fsf.org); either version 2 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

A copy of the GNU General Public License is available at
http://www.fsf.org/licensing/licenses

>>> END OF LICENSE >>>
*************************************************************************)
unit u_Ap;

interface

uses Math, Sysutils;

/////////////////////////////////////////////////////////////////////////
// constants
/////////////////////////////////////////////////////////////////////////
const
    MachineEpsilon = 5E-16;
    MaxRealNumber = 1E300;
    MinRealNumber = 1E-300;

/////////////////////////////////////////////////////////////////////////
// arrays
/////////////////////////////////////////////////////////////////////////
type
    PDouble = ^Double;

    Complex = record
        X, Y: Double;
    end;

    TInteger1DArray     = array of LongInt;
    TReal1DArray        = array of Double;
    TComplex1DArray     = array of Complex;
    TBoolean1DArray     = array of Boolean;

    TInteger2DArray     = array of array of LongInt;
    TReal2DArray        = array of array of Double;
    TComplex2DArray     = array of array of Complex;
    TBoolean2DArray     = array of array of Boolean;

    RCommState = record
        Stage:  LongInt;
        IA:     TInteger1DArray;
        BA:     TBoolean1DArray;
        RA:     TReal1DArray;
        CA:     TComplex1DArray;
    end;

/////////////////////////////////////////////////////////////////////////
// Functions/procedures
/////////////////////////////////////////////////////////////////////////
function AbsReal(X : Double):Double;
function AbsInt (I : Integer):Integer;
function RandomReal():Double;
function RandomInteger(I : Integer):Integer;
function Sign(X:Double):Integer;
function AP_Sqr(X:Double):Double;

function DynamicArrayCopy(const A: TInteger1DArray):TInteger1DArray;overload;
function DynamicArrayCopy(const A: TReal1DArray):TReal1DArray;overload;
function DynamicArrayCopy(const A: TComplex1DArray):TComplex1DArray;overload;
function DynamicArrayCopy(const A: TBoolean1DArray):TBoolean1DArray;overload;

function DynamicArrayCopy(const A: TInteger2DArray):TInteger2DArray;overload;
function DynamicArrayCopy(const A: TReal2DArray):TReal2DArray;overload;
function DynamicArrayCopy(const A: TComplex2DArray):TComplex2DArray;overload;
function DynamicArrayCopy(const A: TBoolean2DArray):TBoolean2DArray;overload;

function AbsComplex(const Z : Complex):Double;
function Conj(const Z : Complex):Complex;
function CSqr(const Z : Complex):Complex;

function C_Complex(const X : Double):Complex;
function C_Opposite(const Z : Complex):Complex;
function C_Add(const Z1 : Complex; const Z2 : Complex):Complex;
function C_Mul(const Z1 : Complex; const Z2 : Complex):Complex;
function C_AddR(const Z1 : Complex; const R : Double):Complex;
function C_MulR(const Z1 : Complex; const R : Double):Complex;
function C_Sub(const Z1 : Complex; const Z2 : Complex):Complex;
function C_SubR(const Z1 : Complex; const R : Double):Complex;
function C_RSub(const R : Double; const Z1 : Complex):Complex;
function C_Div(const Z1 : Complex; const Z2 : Complex):Complex;
function C_DivR(const Z1 : Complex; const R : Double):Complex;
function C_RDiv(const R : Double; const Z2 : Complex):Complex;
function C_Equal(const Z1 : Complex; const Z2 : Complex):Boolean;
function C_NotEqual(const Z1 : Complex; const Z2 : Complex):Boolean;
function C_EqualR(const Z1 : Complex; const R : Double):Boolean;
function C_NotEqualR(const Z1 : Complex; const R : Double):Boolean;

/////////////////////////////////////////////////////////////////////////
// AP BLAS generic interface
/////////////////////////////////////////////////////////////////////////
//procedure UseAPBLAS(Flag: Boolean);
function APVDotProduct(
   V1: PDouble; I11, I12: Integer;
   V2: PDouble; I21, I22: Integer):Double;
procedure APVMove(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer);overload;
procedure APVMove(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer;
   S: Double);overload;
procedure APVMoveNeg(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer);
procedure APVAdd(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer);overload;
procedure APVAdd(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer;
   S: Real);overload;
procedure APVSub(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer);overload;
procedure APVSub(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer;
   S: Real);overload;
procedure APVMul(
   VOp: PDouble; I1, I2: Integer;
   S: Real);

/////////////////////////////////////////////////////////////////////////
// IEEE-compliant functions, placed at the end, under 'non-optimization'
// compiler switch
/////////////////////////////////////////////////////////////////////////
function AP_Double(X:Double):Double;
function AP_FP_Eq(X:Double; Y:Double):Boolean;
function AP_FP_NEq(X:Double; Y:Double):Boolean;
function AP_FP_Less(X:Double; Y:Double):Boolean;
function AP_FP_Less_Eq(X:Double; Y:Double):Boolean;
function AP_FP_Greater(X:Double; Y:Double):Boolean;
function AP_FP_Greater_Eq(X:Double; Y:Double):Boolean;

{var
    // pointers to AP BLAS functions
    ASMDotProduct1: function(V1: PDouble; V2: PDouble; N: Integer):Double;cdecl;
    ASMMove1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer);cdecl;
    ASMMoveS1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer; S: Double);cdecl;
    ASMMoveNeg1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer);cdecl;
    ASMAdd1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer);cdecl;
    ASMAddS1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer; S: Double);cdecl;
    ASMSub1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer);cdecl;
}

implementation

{var
    // use ablas.dll (ALGLIB BLAS) if found
    UseAPBLASFlag: Boolean = True;
}
    // pointers to AP BLAS functions
{$IFNDEF NOABLAS}
{    ASMDotProduct1: function(V1: PDouble; V2: PDouble; N: Integer):Double;cdecl;
    ASMMove1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer);cdecl;
    ASMMoveS1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer; S: Double);cdecl;
    ASMMoveNeg1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer);cdecl;
    ASMAdd1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer);cdecl;
    ASMAddS1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer; S: Double);cdecl;
    ASMSub1: procedure(VDst: PDouble; VSrc: PDouble; N: Integer);cdecl;}
{$ENDIF}

/////////////////////////////////////////////////////////////////////////
// Functions/procedures
/////////////////////////////////////////////////////////////////////////
function AbsReal(X : Double):Double;
begin
    //Result := Abs(X);
    if X>=0 then
        Result:=X
    else
        Result:=-X;
end;

function AbsInt (I : Integer):Integer;
begin
    //Result := Abs(I);
    if I>=0 then
        Result:=I
    else
        Result:=-I;
end;

function RandomReal():Double;
begin
    Result := Random;
end;

function RandomInteger(I : Integer):Integer;
begin
    Result := Random(I);
end;

function Sign(X:Double):Integer;
begin
    if X>0 then
        Result := 1
    else if X<0 then
        Result := -1
    else
        Result := 0;
end;

function AP_Sqr(X:Double):Double;
begin
    Result := X*X;
end;

/////////////////////////////////////////////////////////////////////////
// dynamical arrays copying
/////////////////////////////////////////////////////////////////////////
function DynamicArrayCopy(const A: TInteger1DArray):TInteger1DArray;overload;
var
    I:  Integer;
begin
    SetLength(Result, High(A)+1);
    for I:=Low(A) to High(A) do
        Result[I]:=A[I];
end;

function DynamicArrayCopy(const A: TReal1DArray):TReal1DArray;overload;
var
    I:  Integer;
begin
    SetLength(Result, High(A)+1);
    for I:=Low(A) to High(A) do
        Result[I]:=A[I];
end;

function DynamicArrayCopy(const A: TComplex1DArray):TComplex1DArray;overload;
var
    I:  Integer;
begin
    SetLength(Result, High(A)+1);
    for I:=Low(A) to High(A) do
        Result[I]:=A[I];
end;

function DynamicArrayCopy(const A: TBoolean1DArray):TBoolean1DArray;overload;
var
    I:  Integer;
begin
    SetLength(Result, High(A)+1);
    for I:=Low(A) to High(A) do
        Result[I]:=A[I];
end;

function DynamicArrayCopy(const A: TInteger2DArray):TInteger2DArray;overload;
var
    I,J:    Integer;
begin
    SetLength(Result, High(A)+1);
    for I:=Low(A) to High(A) do
    begin
        SetLength(Result[I], High(A[I])+1);
        for J:=Low(A[I]) to High(A[I]) do
            Result[I,J]:=A[I,J];
    end;
end;

function DynamicArrayCopy(const A: TReal2DArray):TReal2DArray;overload;
var
    I,J:    Integer;
begin
    SetLength(Result, High(A)+1);
    for I:=Low(A) to High(A) do
    begin
        SetLength(Result[I], High(A[I])+1);
        for J:=Low(A[I]) to High(A[I]) do
            Result[I,J]:=A[I,J];
    end;
end;

function DynamicArrayCopy(const A: TComplex2DArray):TComplex2DArray;overload;
var
    I,J:    Integer;
begin
    SetLength(Result, High(A)+1);
    for I:=Low(A) to High(A) do
    begin
        SetLength(Result[I], High(A[I])+1);
        for J:=Low(A[I]) to High(A[I]) do
            Result[I,J]:=A[I,J];
    end;
end;

function DynamicArrayCopy(const A: TBoolean2DArray):TBoolean2DArray;overload;
var
    I,J:    Integer;
begin
    SetLength(Result, High(A)+1);
    for I:=Low(A) to High(A) do
    begin
        SetLength(Result[I], High(A[I])+1);
        for J:=Low(A[I]) to High(A[I]) do
            Result[I,J]:=A[I,J];
    end;
end;

/////////////////////////////////////////////////////////////////////////
// complex numbers
/////////////////////////////////////////////////////////////////////////
function AbsComplex(const Z : Complex):Double;
var
    W : Double;
    XABS : Double;
    YABS : Double;
    V : Double;
begin
    XABS := AbsReal(Z.X);
    YABS := AbsReal(Z.Y);
    W := Max(XABS, YABS);
    V := Min(XABS, YABS);
    if V=0 then
    begin
        Result := W;
    end
    else
    begin
        Result := W*SQRT(1+Sqr(V/W));
    end;
end;


function Conj(const Z : Complex):Complex;
begin
    Result.X := Z.X;
    Result.Y := -Z.Y;
end;


function CSqr(const Z : Complex):Complex;
begin
    Result.X := Sqr(Z.X)-Sqr(Z.Y);
    Result.Y := 2*Z.X*Z.Y;
end;


function C_Complex(const X : Double):Complex;
begin
    Result.X := X;
    Result.Y := 0;
end;


function C_Opposite(const Z : Complex):Complex;
begin
    Result.X := -Z.X;
    Result.Y := -Z.Y;
end;


function C_Add(const Z1 : Complex; const Z2 : Complex):Complex;
begin
    Result.X := Z1.X+Z2.X;
    Result.Y := Z1.Y+Z2.Y;
end;


function C_Mul(const Z1 : Complex; const Z2 : Complex):Complex;
begin
    Result.X := Z1.X*Z2.X-Z1.Y*Z2.Y;
    Result.Y := Z1.X*Z2.Y+Z1.Y*Z2.X;
end;


function C_AddR(const Z1 : Complex; const R : Double):Complex;
begin
    Result.X := Z1.X+R;
    Result.Y := Z1.Y;
end;


function C_MulR(const Z1 : Complex; const R : Double):Complex;
begin
    Result.X := Z1.X*R;
    Result.Y := Z1.Y*R;
end;


function C_Sub(const Z1 : Complex; const Z2 : Complex):Complex;
begin
    Result.X := Z1.X-Z2.X;
    Result.Y := Z1.Y-Z2.Y;
end;


function C_SubR(const Z1 : Complex; const R : Double):Complex;
begin
    Result.X := Z1.X-R;
    Result.Y := Z1.Y;
end;


function C_RSub(const R : Double; const Z1 : Complex):Complex;
begin
    Result.X := R-Z1.X;
    Result.Y := -Z1.Y;
end;


function C_Div(const Z1 : Complex; const Z2 : Complex):Complex;
var
    A : Double;
    B : Double;
    C : Double;
    D : Double;
    E : Double;
    F : Double;
begin
    A := Z1.X;
    B := Z1.Y;
    C := Z2.X;
    D := Z2.Y;
    if AbsReal(D)<AbsReal(C) then
    begin
        E := D/C;
        F := C+D*E;
        Result.X := (A+B*E)/F;
        Result.Y := (B-A*E)/F;
    end
    else
    begin
        E := C/D;
        F := D+C*E;
        Result.X := (B+A*E)/F;
        Result.Y := (-A+B*E)/F;
    end;
end;


function C_DivR(const Z1 : Complex; const R : Double):Complex;
begin
    Result.X := Z1.X/R;
    Result.Y := Z1.Y/R;
end;


function C_RDiv(const R : Double; const Z2 : Complex):Complex;
var
    A : Double;
    C : Double;
    D : Double;
    E : Double;
    F : Double;
begin
    A := R;
    C := Z2.X;
    D := Z2.Y;
    if AbsReal(D)<AbsReal(C) then
    begin
        E := D/C;
        F := C+D*E;
        Result.X := A/F;
        Result.Y := -A*E/F;
    end
    else
    begin
        E := C/D;
        F := D+C*E;
        Result.X := A*E/F;
        Result.Y := -A/F;
    end;
end;


function C_Equal(const Z1 : Complex; const Z2 : Complex):Boolean;
begin
    Result := (Z1.X=Z2.X) and (Z1.Y=Z2.Y);
end;


function C_NotEqual(const Z1 : Complex; const Z2 : Complex):Boolean;
begin
    Result := (Z1.X<>Z2.X) or (Z1.Y<>Z2.Y);
end;

function C_EqualR(const Z1 : Complex; const R : Double):Boolean;
begin
    Result := (Z1.X=R) and (Z1.Y=0);
end;

function C_NotEqualR(const Z1 : Complex; const R : Double):Boolean;
begin
    Result := (Z1.X<>R) or (Z1.Y<>0);
end;


/////////////////////////////////////////////////////////////////////////
// AP BLAS generic interface
/////////////////////////////////////////////////////////////////////////
{procedure UseAPBLAS(Flag: Boolean);
begin
    UseAPBLASFlag:=Flag;
end;}

function APVDotProduct(
   V1: PDouble; I11, I12: Integer;
   V2: PDouble; I21, I22: Integer):Double;
var
    I, C: LongInt;
begin
    Assert(I12-I11=I22-I21, 'APVDotProduct: arrays of different size!');
    Inc(V1, I11);
    Inc(V2, I21);

    //
    // Generic pascal code
    //
    C:=I12-I11;
    Result:=0;
    for I:=0 to C do
    begin
        Result:=Result+V1^*V2^;
        Inc(V1);
        Inc(V2);
    end;
end;


procedure APVMove(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer);overload;
var
    I, C: LongInt;
begin
    Assert(I12-I11=I22-I21, 'APVMove: arrays of different size!');
    Inc(VDst, I11);
    Inc(VSrc, I21);

    //
    // Generic pascal code
    //
    C:=I12-I11;
    for I:=0 to C do
    begin
        VDst^:=VSrc^;
        Inc(VDst);
        Inc(VSrc);
    end;
end;


procedure APVMove(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer;
   S: Double);overload;
var
    I, C: LongInt;
begin
    Assert(I12-I11=I22-I21, 'APVMove: arrays of different size!');
    Inc(VDst, I11);
    Inc(VSrc, I21);

    //
    // Generic pascal code
    //
    C:=I12-I11;
    for I:=0 to C do
    begin
        VDst^:=S*VSrc^;
        Inc(VDst);
        Inc(VSrc);
    end;
end;


procedure APVMoveNeg(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer);
var
    I, C: LongInt;
begin
    Assert(I12-I11=I22-I21, 'APVMoveNeg: arrays of different size!');
    Inc(VDst, I11);
    Inc(VSrc, I21);

    //
    // Generic pascal code
    //
    C:=I12-I11;
    for I:=0 to C do
    begin
        VDst^:=-VSrc^;
        Inc(VDst);
        Inc(VSrc);
    end;
end;


procedure APVAdd(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer);overload;
var
    I, C: LongInt;
begin
    Assert(I12-I11=I22-I21, 'APVAdd: arrays of different size!');
    Inc(VDst, I11);
    Inc(VSrc, I21);

    //
    // Generic pascal code
    //
    C:=I12-I11;
    for I:=0 to C do
    begin
        VDst^:=VDst^+VSrc^;
        Inc(VDst);
        Inc(VSrc);
    end;
end;


procedure APVAdd(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer;
   S: Real);overload;
var
    I, C: LongInt;
begin
    Assert(I12-I11=I22-I21, 'APVAdd: arrays of different size!');
    Inc(VDst, I11);
    Inc(VSrc, I21);

    //
    // Generic pascal code
    //
    C:=I12-I11;
    for I:=0 to C do
    begin
        VDst^:=VDst^+S*VSrc^;
        Inc(VDst);
        Inc(VSrc);
    end;
end;


procedure APVSub(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer);overload;
var
    I, C: LongInt;
begin
    Assert(I12-I11=I22-I21, 'APVSub arrays of different size!');
    Inc(VDst, I11);
    Inc(VSrc, I21);

    //
    // Generic pascal code
    //
    C:=I12-I11;
    for I:=0 to C do
    begin
        VDst^:=VDst^-VSrc^;
        Inc(VDst);
        Inc(VSrc);
    end;
end;


procedure APVSub(
   VDst: PDouble; I11, I12: Integer;
   VSrc: PDouble; I21, I22: Integer;
   S: Real);overload;
begin
    Assert(I12-I11=I22-I21, 'APVSub: arrays of different size!');
    APVAdd(VDst, I11, I12, VSrc, I21, I22, -S);
end;


procedure APVMul(
   VOp: PDouble; I1, I2: Integer;
   S: Real);
var
    I, C: LongInt;
begin
    Inc(VOp, I1);
    C:=I2-I1;
    for I:=0 to C do
    begin
        VOp^:=S*VOp^;
        Inc(VOp);
    end;
end;

/////////////////////////////////////////////////////////////////////////
// IEEE-compliant functions
/////////////////////////////////////////////////////////////////////////
{$OPTIMIZATION OFF}
function AP_Double(X:Double):Double;
begin
    Result:=X;
end;

function AP_FP_Eq(X:Double; Y:Double):Boolean;
begin
    Result:=X=Y;
end;

function AP_FP_NEq(X:Double; Y:Double):Boolean;
begin
    Result:=X<>Y;
end;

function AP_FP_Less(X:Double; Y:Double):Boolean;
begin
    Result:=X<Y;
end;

function AP_FP_Less_Eq(X:Double; Y:Double):Boolean;
begin
    Result:=X<=Y;
end;

function AP_FP_Greater(X:Double; Y:Double):Boolean;
begin
    Result:=X>Y;
end;

function AP_FP_Greater_Eq(X:Double; Y:Double):Boolean;
begin
    Result:=X>=Y;
end;

end.
