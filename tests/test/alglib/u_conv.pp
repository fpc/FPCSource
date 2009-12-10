(*************************************************************************
Copyright (c) 2009, Sergey Bochkanov (ALGLIB project).

>>> SOURCE LICENSE >>>
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
unit u_conv;
interface
uses Math, Sysutils, u_ap, u_ftbase, u_fft;

procedure ConvC1D(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);
procedure ConvC1DInv(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);
procedure ConvC1DCircular(const S : TComplex1DArray;
     M : Integer;
     const R : TComplex1DArray;
     N : Integer;
     var C : TComplex1DArray);
procedure ConvC1DCircularInv(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);
procedure ConvR1D(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);
procedure ConvR1DInv(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);
procedure ConvR1DCircular(const S : TReal1DArray;
     M : Integer;
     const R : TReal1DArray;
     N : Integer;
     var C : TReal1DArray);
procedure ConvR1DCircularInv(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);
procedure ConvC1DX(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     Circular : Boolean;
     Alg : Integer;
     Q : Integer;
     var R : TComplex1DArray);
procedure ConvR1DX(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     Circular : Boolean;
     Alg : Integer;
     Q : Integer;
     var R : TReal1DArray);

implementation

(*************************************************************************
1-dimensional complex convolution.

For given A/B returns conv(A,B) (non-circular). Subroutine can automatically
choose between three implementations: straightforward O(M*N)  formula  for
very small N (or M), overlap-add algorithm for  cases  where  max(M,N)  is
significantly larger than min(M,N), but O(M*N) algorithm is too slow,  and
general FFT-based formula for cases where two previois algorithms are  too
slow.

Algorithm has max(M,N)*log(max(M,N)) complexity for any M/N.

INPUT PARAMETERS
    A   -   array[0..M-1] - complex function to be transformed
    M   -   problem size
    B   -   array[0..N-1] - complex function to be transformed
    N   -   problem size

OUTPUT PARAMETERS
    R   -   convolution: A*B. array[0..N+M-2].

NOTE:
    It is assumed that A is zero at T<0, B is zero too.  If  one  or  both
functions have non-zero values at negative T's, you  can  still  use  this
subroutine - just shift its result correspondingly.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvC1D(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);
begin
    Assert((N>0) and (M>0), 'ConvC1D: incorrect N or M!');
    
    //
    // normalize task: make M>=N,
    // so A will be longer that B.
    //
    if M<N then
    begin
        ConvC1D(B, N, A, M, R);
        Exit;
    end;
    ConvC1DX(A, M, B, N, False, -1, 0, R);
end;


(*************************************************************************
1-dimensional complex non-circular deconvolution (inverse of ConvC1D()).

Algorithm has M*log(M)) complexity for any M (composite or prime).

INPUT PARAMETERS
    A   -   array[0..M-1] - convolved signal, A = conv(R, B)
    M   -   convolved signal length
    B   -   array[0..N-1] - response
    N   -   response length, N<=M

OUTPUT PARAMETERS
    R   -   deconvolved signal. array[0..M-N].

NOTE:
    deconvolution is unstable process and may result in division  by  zero
(if your response function is degenerate, i.e. has zero Fourier coefficient).

NOTE:
    It is assumed that A is zero at T<0, B is zero too.  If  one  or  both
functions have non-zero values at negative T's, you  can  still  use  this
subroutine - just shift its result correspondingly.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvC1DInv(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);
var
    I : Integer;
    P : Integer;
    Buf : TReal1DArray;
    Buf2 : TReal1DArray;
    Plan : FTPlan;
    C1 : Complex;
    C2 : Complex;
    C3 : Complex;
    T : Double;
begin
    Assert((N>0) and (M>0) and (N<=M), 'ConvC1DInv: incorrect N or M!');
    P := FTBaseFindSmooth(M);
    FTBaseGenerateComplexFFTPlan(P, Plan);
    SetLength(Buf, 2*P);
    I:=0;
    while I<=M-1 do
    begin
        Buf[2*I+0] := A[I].X;
        Buf[2*I+1] := A[I].Y;
        Inc(I);
    end;
    I:=M;
    while I<=P-1 do
    begin
        Buf[2*I+0] := 0;
        Buf[2*I+1] := 0;
        Inc(I);
    end;
    SetLength(Buf2, 2*P);
    I:=0;
    while I<=N-1 do
    begin
        Buf2[2*I+0] := B[I].X;
        Buf2[2*I+1] := B[I].Y;
        Inc(I);
    end;
    I:=N;
    while I<=P-1 do
    begin
        Buf2[2*I+0] := 0;
        Buf2[2*I+1] := 0;
        Inc(I);
    end;
    FTBaseExecutePlan(Buf, 0, P, Plan);
    FTBaseExecutePlan(Buf2, 0, P, Plan);
    I:=0;
    while I<=P-1 do
    begin
        C1.X := Buf[2*I+0];
        C1.Y := Buf[2*I+1];
        C2.X := Buf2[2*I+0];
        C2.Y := Buf2[2*I+1];
        C3 := C_Div(C1,C2);
        Buf[2*I+0] := C3.X;
        Buf[2*I+1] := -C3.Y;
        Inc(I);
    end;
    FTBaseExecutePlan(Buf, 0, P, Plan);
    T := AP_Double(1)/P;
    SetLength(R, M-N+1);
    I:=0;
    while I<=M-N do
    begin
        R[I].X := +T*Buf[2*I+0];
        R[I].Y := -T*Buf[2*I+1];
        Inc(I);
    end;
end;


(*************************************************************************
1-dimensional circular complex convolution.

For given S/R returns conv(S,R) (circular). Algorithm has linearithmic
complexity for any M/N.

IMPORTANT:  normal convolution is commutative,  i.e.   it  is symmetric  -
conv(A,B)=conv(B,A).  Cyclic convolution IS NOT.  One function - S - is  a
signal,  periodic function, and another - R - is a response,  non-periodic
function with limited length.

INPUT PARAMETERS
    S   -   array[0..M-1] - complex periodic signal
    M   -   problem size
    B   -   array[0..N-1] - complex non-periodic response
    N   -   problem size

OUTPUT PARAMETERS
    R   -   convolution: A*B. array[0..M-1].

NOTE:
    It is assumed that A is zero at T<0, B is zero too.  If  one  or  both
functions have non-zero values at negative T's, you  can  still  use  this
subroutine - just shift its result correspondingly.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvC1DCircular(const S : TComplex1DArray;
     M : Integer;
     const R : TComplex1DArray;
     N : Integer;
     var C : TComplex1DArray);
var
    Buf : TComplex1DArray;
    I1 : Integer;
    I2 : Integer;
    J2 : Integer;
    i_ : Integer;
    i1_ : Integer;
begin
    Assert((N>0) and (M>0), 'ConvC1DCircular: incorrect N or M!');
    
    //
    // normalize task: make M>=N,
    // so A will be longer (at least - not shorter) that B.
    //
    if M<N then
    begin
        SetLength(Buf, M);
        I1:=0;
        while I1<=M-1 do
        begin
            Buf[I1] := C_Complex(0);
            Inc(I1);
        end;
        I1 := 0;
        while I1<N do
        begin
            I2 := Min(I1+M-1, N-1);
            J2 := I2-I1;
            i1_ := (I1) - (0);
            for i_ := 0 to J2 do
            begin
                Buf[i_] := C_Add(Buf[i_], R[i_+i1_]);
            end;
            I1 := I1+M;
        end;
        ConvC1DCircular(S, M, Buf, M, C);
        Exit;
    end;
    ConvC1DX(S, M, R, N, True, -1, 0, C);
end;


(*************************************************************************
1-dimensional circular complex deconvolution (inverse of ConvC1DCircular()).

Algorithm has M*log(M)) complexity for any M (composite or prime).

INPUT PARAMETERS
    A   -   array[0..M-1] - convolved periodic signal, A = conv(R, B)
    M   -   convolved signal length
    B   -   array[0..N-1] - non-periodic response
    N   -   response length

OUTPUT PARAMETERS
    R   -   deconvolved signal. array[0..M-1].

NOTE:
    deconvolution is unstable process and may result in division  by  zero
(if your response function is degenerate, i.e. has zero Fourier coefficient).

NOTE:
    It is assumed that A is zero at T<0, B is zero too.  If  one  or  both
functions have non-zero values at negative T's, you  can  still  use  this
subroutine - just shift its result correspondingly.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvC1DCircularInv(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);
var
    I : Integer;
    P : Integer;
    I1 : Integer;
    I2 : Integer;
    J2 : Integer;
    Buf : TReal1DArray;
    Buf2 : TReal1DArray;
    CBuf : TComplex1DArray;
    Plan : FTPlan;
    C1 : Complex;
    C2 : Complex;
    C3 : Complex;
    T : Double;
    i_ : Integer;
    i1_ : Integer;
begin
    Assert((N>0) and (M>0), 'ConvC1DCircularInv: incorrect N or M!');
    
    //
    // normalize task: make M>=N,
    // so A will be longer (at least - not shorter) that B.
    //
    if M<N then
    begin
        SetLength(CBuf, M);
        I:=0;
        while I<=M-1 do
        begin
            CBuf[I] := C_Complex(0);
            Inc(I);
        end;
        I1 := 0;
        while I1<N do
        begin
            I2 := Min(I1+M-1, N-1);
            J2 := I2-I1;
            i1_ := (I1) - (0);
            for i_ := 0 to J2 do
            begin
                CBuf[i_] := C_Add(CBuf[i_], B[i_+i1_]);
            end;
            I1 := I1+M;
        end;
        ConvC1DCircularInv(A, M, CBuf, M, R);
        Exit;
    end;
    
    //
    // Task is normalized
    //
    FTBaseGenerateComplexFFTPlan(M, Plan);
    SetLength(Buf, 2*M);
    I:=0;
    while I<=M-1 do
    begin
        Buf[2*I+0] := A[I].X;
        Buf[2*I+1] := A[I].Y;
        Inc(I);
    end;
    SetLength(Buf2, 2*M);
    I:=0;
    while I<=N-1 do
    begin
        Buf2[2*I+0] := B[I].X;
        Buf2[2*I+1] := B[I].Y;
        Inc(I);
    end;
    I:=N;
    while I<=M-1 do
    begin
        Buf2[2*I+0] := 0;
        Buf2[2*I+1] := 0;
        Inc(I);
    end;
    FTBaseExecutePlan(Buf, 0, M, Plan);
    FTBaseExecutePlan(Buf2, 0, M, Plan);
    I:=0;
    while I<=M-1 do
    begin
        C1.X := Buf[2*I+0];
        C1.Y := Buf[2*I+1];
        C2.X := Buf2[2*I+0];
        C2.Y := Buf2[2*I+1];
        C3 := C_Div(C1,C2);
        Buf[2*I+0] := C3.X;
        Buf[2*I+1] := -C3.Y;
        Inc(I);
    end;
    FTBaseExecutePlan(Buf, 0, M, Plan);
    T := AP_Double(1)/M;
    SetLength(R, M);
    I:=0;
    while I<=M-1 do
    begin
        R[I].X := +T*Buf[2*I+0];
        R[I].Y := -T*Buf[2*I+1];
        Inc(I);
    end;
end;


(*************************************************************************
1-dimensional real convolution.

Analogous to ConvC1D(), see ConvC1D() comments for more details.

INPUT PARAMETERS
    A   -   array[0..M-1] - real function to be transformed
    M   -   problem size
    B   -   array[0..N-1] - real function to be transformed
    N   -   problem size

OUTPUT PARAMETERS
    R   -   convolution: A*B. array[0..N+M-2].

NOTE:
    It is assumed that A is zero at T<0, B is zero too.  If  one  or  both
functions have non-zero values at negative T's, you  can  still  use  this
subroutine - just shift its result correspondingly.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvR1D(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);
var
    I : Integer;
    J : Integer;
    P : Integer;
    Q : Integer;
    ABuf : TComplex1DArray;
    BBuf : TComplex1DArray;
    V : Complex;
    Flop1 : Double;
    Flop2 : Double;
begin
    Assert((N>0) and (M>0), 'ConvR1D: incorrect N or M!');
    
    //
    // normalize task: make M>=N,
    // so A will be longer that B.
    //
    if M<N then
    begin
        ConvR1D(B, N, A, M, R);
        Exit;
    end;
    ConvR1DX(A, M, B, N, False, -1, 0, R);
end;


(*************************************************************************
1-dimensional real deconvolution (inverse of ConvC1D()).

Algorithm has M*log(M)) complexity for any M (composite or prime).

INPUT PARAMETERS
    A   -   array[0..M-1] - convolved signal, A = conv(R, B)
    M   -   convolved signal length
    B   -   array[0..N-1] - response
    N   -   response length, N<=M

OUTPUT PARAMETERS
    R   -   deconvolved signal. array[0..M-N].

NOTE:
    deconvolution is unstable process and may result in division  by  zero
(if your response function is degenerate, i.e. has zero Fourier coefficient).

NOTE:
    It is assumed that A is zero at T<0, B is zero too.  If  one  or  both
functions have non-zero values at negative T's, you  can  still  use  this
subroutine - just shift its result correspondingly.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvR1DInv(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);
var
    I : Integer;
    P : Integer;
    Buf : TReal1DArray;
    Buf2 : TReal1DArray;
    Buf3 : TReal1DArray;
    Plan : FTPlan;
    C1 : Complex;
    C2 : Complex;
    C3 : Complex;
    T : Double;
begin
    Assert((N>0) and (M>0) and (N<=M), 'ConvR1DInv: incorrect N or M!');
    P := FTBaseFindSmoothEven(M);
    SetLength(Buf, P);
    APVMove(@Buf[0], 0, M-1, @A[0], 0, M-1);
    I:=M;
    while I<=P-1 do
    begin
        Buf[I] := 0;
        Inc(I);
    end;
    SetLength(Buf2, P);
    APVMove(@Buf2[0], 0, N-1, @B[0], 0, N-1);
    I:=N;
    while I<=P-1 do
    begin
        Buf2[I] := 0;
        Inc(I);
    end;
    SetLength(Buf3, P);
    FTBaseGenerateComplexFFTPlan(P div 2, Plan);
    FFTR1DInternalEven(Buf, P, Buf3, Plan);
    FFTR1DInternalEven(Buf2, P, Buf3, Plan);
    Buf[0] := Buf[0]/Buf2[0];
    Buf[1] := Buf[1]/Buf2[1];
    I:=1;
    while I<=P div 2-1 do
    begin
        C1.X := Buf[2*I+0];
        C1.Y := Buf[2*I+1];
        C2.X := Buf2[2*I+0];
        C2.Y := Buf2[2*I+1];
        C3 := C_Div(C1,C2);
        Buf[2*I+0] := C3.X;
        Buf[2*I+1] := C3.Y;
        Inc(I);
    end;
    FFTR1DInvInternalEven(Buf, P, Buf3, Plan);
    SetLength(R, M-N+1);
    APVMove(@R[0], 0, M-N, @Buf[0], 0, M-N);
end;


(*************************************************************************
1-dimensional circular real convolution.

Analogous to ConvC1DCircular(), see ConvC1DCircular() comments for more details.

INPUT PARAMETERS
    S   -   array[0..M-1] - real signal
    M   -   problem size
    B   -   array[0..N-1] - real response
    N   -   problem size

OUTPUT PARAMETERS
    R   -   convolution: A*B. array[0..M-1].

NOTE:
    It is assumed that A is zero at T<0, B is zero too.  If  one  or  both
functions have non-zero values at negative T's, you  can  still  use  this
subroutine - just shift its result correspondingly.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvR1DCircular(const S : TReal1DArray;
     M : Integer;
     const R : TReal1DArray;
     N : Integer;
     var C : TReal1DArray);
var
    Buf : TReal1DArray;
    I1 : Integer;
    I2 : Integer;
    J2 : Integer;
begin
    Assert((N>0) and (M>0), 'ConvC1DCircular: incorrect N or M!');
    
    //
    // normalize task: make M>=N,
    // so A will be longer (at least - not shorter) that B.
    //
    if M<N then
    begin
        SetLength(Buf, M);
        I1:=0;
        while I1<=M-1 do
        begin
            Buf[I1] := 0;
            Inc(I1);
        end;
        I1 := 0;
        while I1<N do
        begin
            I2 := Min(I1+M-1, N-1);
            J2 := I2-I1;
            APVAdd(@Buf[0], 0, J2, @R[0], I1, I2);
            I1 := I1+M;
        end;
        ConvR1DCircular(S, M, Buf, M, C);
        Exit;
    end;
    
    //
    // reduce to usual convolution
    //
    ConvR1DX(S, M, R, N, True, -1, 0, C);
end;


(*************************************************************************
1-dimensional complex deconvolution (inverse of ConvC1D()).

Algorithm has M*log(M)) complexity for any M (composite or prime).

INPUT PARAMETERS
    A   -   array[0..M-1] - convolved signal, A = conv(R, B)
    M   -   convolved signal length
    B   -   array[0..N-1] - response
    N   -   response length

OUTPUT PARAMETERS
    R   -   deconvolved signal. array[0..M-N].

NOTE:
    deconvolution is unstable process and may result in division  by  zero
(if your response function is degenerate, i.e. has zero Fourier coefficient).

NOTE:
    It is assumed that A is zero at T<0, B is zero too.  If  one  or  both
functions have non-zero values at negative T's, you  can  still  use  this
subroutine - just shift its result correspondingly.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvR1DCircularInv(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);
var
    I : Integer;
    I1 : Integer;
    I2 : Integer;
    J2 : Integer;
    Buf : TReal1DArray;
    Buf2 : TReal1DArray;
    Buf3 : TReal1DArray;
    CBuf : TComplex1DArray;
    CBuf2 : TComplex1DArray;
    Plan : FTPlan;
    C1 : Complex;
    C2 : Complex;
    C3 : Complex;
    T : Double;
begin
    Assert((N>0) and (M>0), 'ConvR1DCircularInv: incorrect N or M!');
    
    //
    // normalize task: make M>=N,
    // so A will be longer (at least - not shorter) that B.
    //
    if M<N then
    begin
        SetLength(Buf, M);
        I:=0;
        while I<=M-1 do
        begin
            Buf[I] := 0;
            Inc(I);
        end;
        I1 := 0;
        while I1<N do
        begin
            I2 := Min(I1+M-1, N-1);
            J2 := I2-I1;
            APVAdd(@Buf[0], 0, J2, @B[0], I1, I2);
            I1 := I1+M;
        end;
        ConvR1DCircularInv(A, M, Buf, M, R);
        Exit;
    end;
    
    //
    // Task is normalized
    //
    if M mod 2=0 then
    begin
        
        //
        // size is even, use fast even-size FFT
        //
        SetLength(Buf, M);
        APVMove(@Buf[0], 0, M-1, @A[0], 0, M-1);
        SetLength(Buf2, M);
        APVMove(@Buf2[0], 0, N-1, @B[0], 0, N-1);
        I:=N;
        while I<=M-1 do
        begin
            Buf2[I] := 0;
            Inc(I);
        end;
        SetLength(Buf3, M);
        FTBaseGenerateComplexFFTPlan(M div 2, Plan);
        FFTR1DInternalEven(Buf, M, Buf3, Plan);
        FFTR1DInternalEven(Buf2, M, Buf3, Plan);
        Buf[0] := Buf[0]/Buf2[0];
        Buf[1] := Buf[1]/Buf2[1];
        I:=1;
        while I<=M div 2-1 do
        begin
            C1.X := Buf[2*I+0];
            C1.Y := Buf[2*I+1];
            C2.X := Buf2[2*I+0];
            C2.Y := Buf2[2*I+1];
            C3 := C_Div(C1,C2);
            Buf[2*I+0] := C3.X;
            Buf[2*I+1] := C3.Y;
            Inc(I);
        end;
        FFTR1DInvInternalEven(Buf, M, Buf3, Plan);
        SetLength(R, M);
        APVMove(@R[0], 0, M-1, @Buf[0], 0, M-1);
    end
    else
    begin
        
        //
        // odd-size, use general real FFT
        //
        FFTR1D(A, M, CBuf);
        SetLength(Buf2, M);
        APVMove(@Buf2[0], 0, N-1, @B[0], 0, N-1);
        I:=N;
        while I<=M-1 do
        begin
            Buf2[I] := 0;
            Inc(I);
        end;
        FFTR1D(Buf2, M, CBuf2);
        I:=0;
        while I<=Floor(AP_Double(M)/2) do
        begin
            CBuf[I] := C_Div(CBuf[I],CBuf2[I]);
            Inc(I);
        end;
        FFTR1DInv(CBuf, M, R);
    end;
end;


(*************************************************************************
1-dimensional complex convolution.

Extended subroutine which allows to choose convolution algorithm.
Intended for internal use, ALGLIB users should call ConvC1D()/ConvC1DCircular().

INPUT PARAMETERS
    A   -   array[0..M-1] - complex function to be transformed
    M   -   problem size
    B   -   array[0..N-1] - complex function to be transformed
    N   -   problem size, N<=M
    Alg -   algorithm type:
            *-2     auto-select Q for overlap-add
            *-1     auto-select algorithm and parameters
            * 0     straightforward formula for small N's
            * 1     general FFT-based code
            * 2     overlap-add with length Q
    Q   -   length for overlap-add

OUTPUT PARAMETERS
    R   -   convolution: A*B. array[0..N+M-1].

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvC1DX(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     Circular : Boolean;
     Alg : Integer;
     Q : Integer;
     var R : TComplex1DArray);
var
    I : Integer;
    J : Integer;
    P : Integer;
    PTotal : Integer;
    I1 : Integer;
    I2 : Integer;
    J1 : Integer;
    J2 : Integer;
    BBuf : TComplex1DArray;
    V : Complex;
    AX : Double;
    AY : Double;
    BX : Double;
    BY : Double;
    T : Double;
    TX : Double;
    TY : Double;
    FlopCand : Double;
    FlopBest : Double;
    AlgBest : Integer;
    Plan : FTPlan;
    Buf : TReal1DArray;
    Buf2 : TReal1DArray;
    i_ : Integer;
    i1_ : Integer;
begin
    Assert((N>0) and (M>0), 'ConvC1DX: incorrect N or M!');
    Assert(N<=M, 'ConvC1DX: N<M assumption is false!');
    
    //
    // Auto-select
    //
    if (Alg=-1) or (Alg=-2) then
    begin
        
        //
        // Initial candidate: straightforward implementation.
        //
        // If we want to use auto-fitted overlap-add,
        // flop count is initialized by large real number - to force
        // another algorithm selection
        //
        AlgBest := 0;
        if Alg=-1 then
        begin
            FlopBest := 2*M*N;
        end
        else
        begin
            FlopBest := MaxRealNumber;
        end;
        
        //
        // Another candidate - generic FFT code
        //
        if Alg=-1 then
        begin
            if Circular and FTBaseIsSmooth(M) then
            begin
                
                //
                // special code for circular convolution of a sequence with a smooth length
                //
                FlopCand := 3*FTBaseGetFLOPEstimate(M)+6*M;
                if AP_FP_Less(FlopCand,FlopBest) then
                begin
                    AlgBest := 1;
                    FlopBest := FlopCand;
                end;
            end
            else
            begin
                
                //
                // general cyclic/non-cyclic convolution
                //
                P := FTBaseFindSmooth(M+N-1);
                FlopCand := 3*FTBaseGetFLOPEstimate(P)+6*P;
                if AP_FP_Less(FlopCand,FlopBest) then
                begin
                    AlgBest := 1;
                    FlopBest := FlopCand;
                end;
            end;
        end;
        
        //
        // Another candidate - overlap-add
        //
        Q := 1;
        PTotal := 1;
        while PTotal<N do
        begin
            PTotal := PTotal*2;
        end;
        while PTotal<=M+N-1 do
        begin
            P := PTotal-N+1;
            FlopCand := Ceil(AP_Double(M)/P)*(2*FTBaseGetFLOPEstimate(PTotal)+8*PTotal);
            if AP_FP_Less(FlopCand,FlopBest) then
            begin
                FlopBest := FlopCand;
                AlgBest := 2;
                Q := P;
            end;
            PTotal := PTotal*2;
        end;
        Alg := AlgBest;
        ConvC1DX(A, M, B, N, Circular, Alg, Q, R);
        Exit;
    end;
    
    //
    // straightforward formula for
    // circular and non-circular convolutions.
    //
    // Very simple code, no further comments needed.
    //
    if Alg=0 then
    begin
        
        //
        // Special case: N=1
        //
        if N=1 then
        begin
            SetLength(R, M);
            V := B[0];
            for i_ := 0 to M-1 do
            begin
                R[i_] := C_Mul(V, A[i_]);
            end;
            Exit;
        end;
        
        //
        // use straightforward formula
        //
        if Circular then
        begin
            
            //
            // circular convolution
            //
            SetLength(R, M);
            V := B[0];
            for i_ := 0 to M-1 do
            begin
                R[i_] := C_Mul(V, A[i_]);
            end;
            I:=1;
            while I<=N-1 do
            begin
                V := B[I];
                I1 := 0;
                I2 := I-1;
                J1 := M-I;
                J2 := M-1;
                i1_ := (J1) - (I1);
                for i_ := I1 to I2 do
                begin
                    R[i_] := C_Add(R[i_], C_Mul(V, A[i_+i1_]));
                end;
                I1 := I;
                I2 := M-1;
                J1 := 0;
                J2 := M-I-1;
                i1_ := (J1) - (I1);
                for i_ := I1 to I2 do
                begin
                    R[i_] := C_Add(R[i_], C_Mul(V, A[i_+i1_]));
                end;
                Inc(I);
            end;
        end
        else
        begin
            
            //
            // non-circular convolution
            //
            SetLength(R, M+N-1);
            I:=0;
            while I<=M+N-2 do
            begin
                R[I] := C_Complex(0);
                Inc(I);
            end;
            I:=0;
            while I<=N-1 do
            begin
                V := B[I];
                i1_ := (0) - (I);
                for i_ := I to I+M-1 do
                begin
                    R[i_] := C_Add(R[i_], C_Mul(V, A[i_+i1_]));
                end;
                Inc(I);
            end;
        end;
        Exit;
    end;
    
    //
    // general FFT-based code for
    // circular and non-circular convolutions.
    //
    // First, if convolution is circular, we test whether M is smooth or not.
    // If it is smooth, we just use M-length FFT to calculate convolution.
    // If it is not, we calculate non-circular convolution and wrap it arount.
    //
    // IF convolution is non-circular, we use zero-padding + FFT.
    //
    if Alg=1 then
    begin
        if Circular and FTBaseIsSmooth(M) then
        begin
            
            //
            // special code for circular convolution with smooth M
            //
            FTBaseGenerateComplexFFTPlan(M, Plan);
            SetLength(Buf, 2*M);
            I:=0;
            while I<=M-1 do
            begin
                Buf[2*I+0] := A[I].X;
                Buf[2*I+1] := A[I].Y;
                Inc(I);
            end;
            SetLength(Buf2, 2*M);
            I:=0;
            while I<=N-1 do
            begin
                Buf2[2*I+0] := B[I].X;
                Buf2[2*I+1] := B[I].Y;
                Inc(I);
            end;
            I:=N;
            while I<=M-1 do
            begin
                Buf2[2*I+0] := 0;
                Buf2[2*I+1] := 0;
                Inc(I);
            end;
            FTBaseExecutePlan(Buf, 0, M, Plan);
            FTBaseExecutePlan(Buf2, 0, M, Plan);
            I:=0;
            while I<=M-1 do
            begin
                AX := Buf[2*I+0];
                AY := Buf[2*I+1];
                BX := Buf2[2*I+0];
                BY := Buf2[2*I+1];
                TX := AX*BX-AY*BY;
                TY := AX*BY+AY*BX;
                Buf[2*I+0] := TX;
                Buf[2*I+1] := -TY;
                Inc(I);
            end;
            FTBaseExecutePlan(Buf, 0, M, Plan);
            T := AP_Double(1)/M;
            SetLength(R, M);
            I:=0;
            while I<=M-1 do
            begin
                R[I].X := +T*Buf[2*I+0];
                R[I].Y := -T*Buf[2*I+1];
                Inc(I);
            end;
        end
        else
        begin
            
            //
            // M is non-smooth, general code (circular/non-circular):
            // * first part is the same for circular and non-circular
            //   convolutions. zero padding, FFTs, inverse FFTs
            // * second part differs:
            //   * for non-circular convolution we just copy array
            //   * for circular convolution we add array tail to its head
            //
            P := FTBaseFindSmooth(M+N-1);
            FTBaseGenerateComplexFFTPlan(P, Plan);
            SetLength(Buf, 2*P);
            I:=0;
            while I<=M-1 do
            begin
                Buf[2*I+0] := A[I].X;
                Buf[2*I+1] := A[I].Y;
                Inc(I);
            end;
            I:=M;
            while I<=P-1 do
            begin
                Buf[2*I+0] := 0;
                Buf[2*I+1] := 0;
                Inc(I);
            end;
            SetLength(Buf2, 2*P);
            I:=0;
            while I<=N-1 do
            begin
                Buf2[2*I+0] := B[I].X;
                Buf2[2*I+1] := B[I].Y;
                Inc(I);
            end;
            I:=N;
            while I<=P-1 do
            begin
                Buf2[2*I+0] := 0;
                Buf2[2*I+1] := 0;
                Inc(I);
            end;
            FTBaseExecutePlan(Buf, 0, P, Plan);
            FTBaseExecutePlan(Buf2, 0, P, Plan);
            I:=0;
            while I<=P-1 do
            begin
                AX := Buf[2*I+0];
                AY := Buf[2*I+1];
                BX := Buf2[2*I+0];
                BY := Buf2[2*I+1];
                TX := AX*BX-AY*BY;
                TY := AX*BY+AY*BX;
                Buf[2*I+0] := TX;
                Buf[2*I+1] := -TY;
                Inc(I);
            end;
            FTBaseExecutePlan(Buf, 0, P, Plan);
            T := AP_Double(1)/P;
            if Circular then
            begin
                
                //
                // circular, add tail to head
                //
                SetLength(R, M);
                I:=0;
                while I<=M-1 do
                begin
                    R[I].X := +T*Buf[2*I+0];
                    R[I].Y := -T*Buf[2*I+1];
                    Inc(I);
                end;
                I:=M;
                while I<=M+N-2 do
                begin
                    R[I-M].X := R[I-M].X+T*Buf[2*I+0];
                    R[I-M].Y := R[I-M].Y-T*Buf[2*I+1];
                    Inc(I);
                end;
            end
            else
            begin
                
                //
                // non-circular, just copy
                //
                SetLength(R, M+N-1);
                I:=0;
                while I<=M+N-2 do
                begin
                    R[I].X := +T*Buf[2*I+0];
                    R[I].Y := -T*Buf[2*I+1];
                    Inc(I);
                end;
            end;
        end;
        Exit;
    end;
    
    //
    // overlap-add method for
    // circular and non-circular convolutions.
    //
    // First part of code (separate FFTs of input blocks) is the same
    // for all types of convolution. Second part (overlapping outputs)
    // differs for different types of convolution. We just copy output
    // when convolution is non-circular. We wrap it around, if it is
    // circular.
    //
    if Alg=2 then
    begin
        SetLength(Buf, 2*(Q+N-1));
        
        //
        // prepare R
        //
        if Circular then
        begin
            SetLength(R, M);
            I:=0;
            while I<=M-1 do
            begin
                R[I] := C_Complex(0);
                Inc(I);
            end;
        end
        else
        begin
            SetLength(R, M+N-1);
            I:=0;
            while I<=M+N-2 do
            begin
                R[I] := C_Complex(0);
                Inc(I);
            end;
        end;
        
        //
        // pre-calculated FFT(B)
        //
        SetLength(BBuf, Q+N-1);
        for i_ := 0 to N-1 do
        begin
            BBuf[i_] := B[i_];
        end;
        J:=N;
        while J<=Q+N-2 do
        begin
            BBuf[J] := C_Complex(0);
            Inc(J);
        end;
        FFTC1D(BBuf, Q+N-1);
        
        //
        // prepare FFT plan for chunks of A
        //
        FTBaseGenerateComplexFFTPlan(Q+N-1, Plan);
        
        //
        // main overlap-add cycle
        //
        I := 0;
        while I<=M-1 do
        begin
            P := Min(Q, M-I);
            J:=0;
            while J<=P-1 do
            begin
                Buf[2*J+0] := A[I+J].X;
                Buf[2*J+1] := A[I+J].Y;
                Inc(J);
            end;
            J:=P;
            while J<=Q+N-2 do
            begin
                Buf[2*J+0] := 0;
                Buf[2*J+1] := 0;
                Inc(J);
            end;
            FTBaseExecutePlan(Buf, 0, Q+N-1, Plan);
            J:=0;
            while J<=Q+N-2 do
            begin
                AX := Buf[2*J+0];
                AY := Buf[2*J+1];
                BX := BBuf[J].X;
                BY := BBuf[J].Y;
                TX := AX*BX-AY*BY;
                TY := AX*BY+AY*BX;
                Buf[2*J+0] := TX;
                Buf[2*J+1] := -TY;
                Inc(J);
            end;
            FTBaseExecutePlan(Buf, 0, Q+N-1, Plan);
            T := AP_Double(1)/(Q+N-1);
            if Circular then
            begin
                J1 := Min(I+P+N-2, M-1)-I;
                J2 := J1+1;
            end
            else
            begin
                J1 := P+N-2;
                J2 := J1+1;
            end;
            J:=0;
            while J<=J1 do
            begin
                R[I+J].X := R[I+J].X+Buf[2*J+0]*T;
                R[I+J].Y := R[I+J].Y-Buf[2*J+1]*T;
                Inc(J);
            end;
            J:=J2;
            while J<=P+N-2 do
            begin
                R[J-J2].X := R[J-J2].X+Buf[2*J+0]*T;
                R[J-J2].Y := R[J-J2].Y-Buf[2*J+1]*T;
                Inc(J);
            end;
            I := I+P;
        end;
        Exit;
    end;
end;


(*************************************************************************
1-dimensional real convolution.

Extended subroutine which allows to choose convolution algorithm.
Intended for internal use, ALGLIB users should call ConvR1D().

INPUT PARAMETERS
    A   -   array[0..M-1] - complex function to be transformed
    M   -   problem size
    B   -   array[0..N-1] - complex function to be transformed
    N   -   problem size, N<=M
    Alg -   algorithm type:
            *-2     auto-select Q for overlap-add
            *-1     auto-select algorithm and parameters
            * 0     straightforward formula for small N's
            * 1     general FFT-based code
            * 2     overlap-add with length Q
    Q   -   length for overlap-add

OUTPUT PARAMETERS
    R   -   convolution: A*B. array[0..N+M-1].

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure ConvR1DX(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     Circular : Boolean;
     Alg : Integer;
     Q : Integer;
     var R : TReal1DArray);
var
    V : Double;
    I : Integer;
    J : Integer;
    P : Integer;
    PTotal : Integer;
    I1 : Integer;
    I2 : Integer;
    J1 : Integer;
    J2 : Integer;
    AX : Double;
    AY : Double;
    BX : Double;
    BY : Double;
    T : Double;
    TX : Double;
    TY : Double;
    FlopCand : Double;
    FlopBest : Double;
    AlgBest : Integer;
    Plan : FTPlan;
    Buf : TReal1DArray;
    Buf2 : TReal1DArray;
    Buf3 : TReal1DArray;
begin
    Assert((N>0) and (M>0), 'ConvC1DX: incorrect N or M!');
    Assert(N<=M, 'ConvC1DX: N<M assumption is false!');
    
    //
    // handle special cases
    //
    if Min(M, N)<=2 then
    begin
        Alg := 0;
    end;
    
    //
    // Auto-select
    //
    if Alg<0 then
    begin
        
        //
        // Initial candidate: straightforward implementation.
        //
        // If we want to use auto-fitted overlap-add,
        // flop count is initialized by large real number - to force
        // another algorithm selection
        //
        AlgBest := 0;
        if Alg=-1 then
        begin
            FlopBest := Double(0.15)*M*N;
        end
        else
        begin
            FlopBest := MaxRealNumber;
        end;
        
        //
        // Another candidate - generic FFT code
        //
        if Alg=-1 then
        begin
            if Circular and FTBaseIsSmooth(M) and (M mod 2=0) then
            begin
                
                //
                // special code for circular convolution of a sequence with a smooth length
                //
                FlopCand := 3*FTBaseGetFLOPEstimate(M div 2)+AP_Double(6*M)/2;
                if AP_FP_Less(FlopCand,FlopBest) then
                begin
                    AlgBest := 1;
                    FlopBest := FlopCand;
                end;
            end
            else
            begin
                
                //
                // general cyclic/non-cyclic convolution
                //
                P := FTBaseFindSmoothEven(M+N-1);
                FlopCand := 3*FTBaseGetFLOPEstimate(P div 2)+AP_Double(6*P)/2;
                if AP_FP_Less(FlopCand,FlopBest) then
                begin
                    AlgBest := 1;
                    FlopBest := FlopCand;
                end;
            end;
        end;
        
        //
        // Another candidate - overlap-add
        //
        Q := 1;
        PTotal := 1;
        while PTotal<N do
        begin
            PTotal := PTotal*2;
        end;
        while PTotal<=M+N-1 do
        begin
            P := PTotal-N+1;
            FlopCand := Ceil(AP_Double(M)/P)*(2*FTBaseGetFLOPEstimate(PTotal div 2)+1*(PTotal div 2));
            if AP_FP_Less(FlopCand,FlopBest) then
            begin
                FlopBest := FlopCand;
                AlgBest := 2;
                Q := P;
            end;
            PTotal := PTotal*2;
        end;
        Alg := AlgBest;
        ConvR1DX(A, M, B, N, Circular, Alg, Q, R);
        Exit;
    end;
    
    //
    // straightforward formula for
    // circular and non-circular convolutions.
    //
    // Very simple code, no further comments needed.
    //
    if Alg=0 then
    begin
        
        //
        // Special case: N=1
        //
        if N=1 then
        begin
            SetLength(R, M);
            V := B[0];
            APVMove(@R[0], 0, M-1, @A[0], 0, M-1, V);
            Exit;
        end;
        
        //
        // use straightforward formula
        //
        if Circular then
        begin
            
            //
            // circular convolution
            //
            SetLength(R, M);
            V := B[0];
            APVMove(@R[0], 0, M-1, @A[0], 0, M-1, V);
            I:=1;
            while I<=N-1 do
            begin
                V := B[I];
                I1 := 0;
                I2 := I-1;
                J1 := M-I;
                J2 := M-1;
                APVAdd(@R[0], I1, I2, @A[0], J1, J2, V);
                I1 := I;
                I2 := M-1;
                J1 := 0;
                J2 := M-I-1;
                APVAdd(@R[0], I1, I2, @A[0], J1, J2, V);
                Inc(I);
            end;
        end
        else
        begin
            
            //
            // non-circular convolution
            //
            SetLength(R, M+N-1);
            I:=0;
            while I<=M+N-2 do
            begin
                R[I] := 0;
                Inc(I);
            end;
            I:=0;
            while I<=N-1 do
            begin
                V := B[I];
                APVAdd(@R[0], I, I+M-1, @A[0], 0, M-1, V);
                Inc(I);
            end;
        end;
        Exit;
    end;
    
    //
    // general FFT-based code for
    // circular and non-circular convolutions.
    //
    // First, if convolution is circular, we test whether M is smooth or not.
    // If it is smooth, we just use M-length FFT to calculate convolution.
    // If it is not, we calculate non-circular convolution and wrap it arount.
    //
    // If convolution is non-circular, we use zero-padding + FFT.
    //
    // We assume that M+N-1>2 - we should call small case code otherwise
    //
    if Alg=1 then
    begin
        Assert(M+N-1>2, 'ConvR1DX: internal error!');
        if Circular and FTBaseIsSmooth(M) and (M mod 2=0) then
        begin
            
            //
            // special code for circular convolution with smooth even M
            //
            SetLength(Buf, M);
            APVMove(@Buf[0], 0, M-1, @A[0], 0, M-1);
            SetLength(Buf2, M);
            APVMove(@Buf2[0], 0, N-1, @B[0], 0, N-1);
            I:=N;
            while I<=M-1 do
            begin
                Buf2[I] := 0;
                Inc(I);
            end;
            SetLength(Buf3, M);
            FTBaseGenerateComplexFFTPlan(M div 2, Plan);
            FFTR1DInternalEven(Buf, M, Buf3, Plan);
            FFTR1DInternalEven(Buf2, M, Buf3, Plan);
            Buf[0] := Buf[0]*Buf2[0];
            Buf[1] := Buf[1]*Buf2[1];
            I:=1;
            while I<=M div 2-1 do
            begin
                AX := Buf[2*I+0];
                AY := Buf[2*I+1];
                BX := Buf2[2*I+0];
                BY := Buf2[2*I+1];
                TX := AX*BX-AY*BY;
                TY := AX*BY+AY*BX;
                Buf[2*I+0] := TX;
                Buf[2*I+1] := TY;
                Inc(I);
            end;
            FFTR1DInvInternalEven(Buf, M, Buf3, Plan);
            SetLength(R, M);
            APVMove(@R[0], 0, M-1, @Buf[0], 0, M-1);
        end
        else
        begin
            
            //
            // M is non-smooth or non-even, general code (circular/non-circular):
            // * first part is the same for circular and non-circular
            //   convolutions. zero padding, FFTs, inverse FFTs
            // * second part differs:
            //   * for non-circular convolution we just copy array
            //   * for circular convolution we add array tail to its head
            //
            P := FTBaseFindSmoothEven(M+N-1);
            SetLength(Buf, P);
            APVMove(@Buf[0], 0, M-1, @A[0], 0, M-1);
            I:=M;
            while I<=P-1 do
            begin
                Buf[I] := 0;
                Inc(I);
            end;
            SetLength(Buf2, P);
            APVMove(@Buf2[0], 0, N-1, @B[0], 0, N-1);
            I:=N;
            while I<=P-1 do
            begin
                Buf2[I] := 0;
                Inc(I);
            end;
            SetLength(Buf3, P);
            FTBaseGenerateComplexFFTPlan(P div 2, Plan);
            FFTR1DInternalEven(Buf, P, Buf3, Plan);
            FFTR1DInternalEven(Buf2, P, Buf3, Plan);
            Buf[0] := Buf[0]*Buf2[0];
            Buf[1] := Buf[1]*Buf2[1];
            I:=1;
            while I<=P div 2-1 do
            begin
                AX := Buf[2*I+0];
                AY := Buf[2*I+1];
                BX := Buf2[2*I+0];
                BY := Buf2[2*I+1];
                TX := AX*BX-AY*BY;
                TY := AX*BY+AY*BX;
                Buf[2*I+0] := TX;
                Buf[2*I+1] := TY;
                Inc(I);
            end;
            FFTR1DInvInternalEven(Buf, P, Buf3, Plan);
            if Circular then
            begin
                
                //
                // circular, add tail to head
                //
                SetLength(R, M);
                APVMove(@R[0], 0, M-1, @Buf[0], 0, M-1);
                if N>=2 then
                begin
                    APVAdd(@R[0], 0, N-2, @Buf[0], M, M+N-2);
                end;
            end
            else
            begin
                
                //
                // non-circular, just copy
                //
                SetLength(R, M+N-1);
                APVMove(@R[0], 0, M+N-2, @Buf[0], 0, M+N-2);
            end;
        end;
        Exit;
    end;
    
    //
    // overlap-add method
    //
    if Alg=2 then
    begin
        Assert((Q+N-1) mod 2=0, 'ConvR1DX: internal error!');
        SetLength(Buf, Q+N-1);
        SetLength(Buf2, Q+N-1);
        SetLength(Buf3, Q+N-1);
        FTBaseGenerateComplexFFTPlan((Q+N-1) div 2, Plan);
        
        //
        // prepare R
        //
        if Circular then
        begin
            SetLength(R, M);
            I:=0;
            while I<=M-1 do
            begin
                R[I] := 0;
                Inc(I);
            end;
        end
        else
        begin
            SetLength(R, M+N-1);
            I:=0;
            while I<=M+N-2 do
            begin
                R[I] := 0;
                Inc(I);
            end;
        end;
        
        //
        // pre-calculated FFT(B)
        //
        APVMove(@Buf2[0], 0, N-1, @B[0], 0, N-1);
        J:=N;
        while J<=Q+N-2 do
        begin
            Buf2[J] := 0;
            Inc(J);
        end;
        FFTR1DInternalEven(Buf2, Q+N-1, Buf3, Plan);
        
        //
        // main overlap-add cycle
        //
        I := 0;
        while I<=M-1 do
        begin
            P := Min(Q, M-I);
            APVMove(@Buf[0], 0, P-1, @A[0], I, I+P-1);
            J:=P;
            while J<=Q+N-2 do
            begin
                Buf[J] := 0;
                Inc(J);
            end;
            FFTR1DInternalEven(Buf, Q+N-1, Buf3, Plan);
            Buf[0] := Buf[0]*Buf2[0];
            Buf[1] := Buf[1]*Buf2[1];
            J:=1;
            while J<=(Q+N-1) div 2-1 do
            begin
                AX := Buf[2*J+0];
                AY := Buf[2*J+1];
                BX := Buf2[2*J+0];
                BY := Buf2[2*J+1];
                TX := AX*BX-AY*BY;
                TY := AX*BY+AY*BX;
                Buf[2*J+0] := TX;
                Buf[2*J+1] := TY;
                Inc(J);
            end;
            FFTR1DInvInternalEven(Buf, Q+N-1, Buf3, Plan);
            if Circular then
            begin
                J1 := Min(I+P+N-2, M-1)-I;
                J2 := J1+1;
            end
            else
            begin
                J1 := P+N-2;
                J2 := J1+1;
            end;
            APVAdd(@R[0], I, I+J1, @Buf[0], 0, J1);
            if P+N-2>=J2 then
            begin
                APVAdd(@R[0], 0, P+N-2-J2, @Buf[0], J2, P+N-2);
            end;
            I := I+P;
        end;
        Exit;
    end;
end;


end.