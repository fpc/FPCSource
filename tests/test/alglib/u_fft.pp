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
unit u_fft;
interface
uses Math, Sysutils, u_ap, u_ftbase;

procedure FFTC1D(var A : TComplex1DArray; N : Integer);
procedure FFTC1DInv(var A : TComplex1DArray; N : Integer);
procedure FFTR1D(const A : TReal1DArray; N : Integer; var F : TComplex1DArray);
procedure FFTR1DInv(const F : TComplex1DArray;
     N : Integer;
     var A : TReal1DArray);
procedure FFTR1DInternalEven(var A : TReal1DArray;
     N : Integer;
     var Buf : TReal1DArray;
     var Plan : FTPlan);
procedure FFTR1DInvInternalEven(var A : TReal1DArray;
     N : Integer;
     var Buf : TReal1DArray;
     var Plan : FTPlan);

implementation

(*************************************************************************
1-dimensional complex FFT.

Array size N may be arbitrary number (composite or prime).  Composite  N's
are handled with cache-oblivious variation of  a  Cooley-Tukey  algorithm.
Small prime-factors are transformed using hard coded  codelets (similar to
FFTW codelets, but without low-level  optimization),  large  prime-factors
are handled with Bluestein's algorithm.

Fastests transforms are for smooth N's (prime factors are 2, 3,  5  only),
most fast for powers of 2. When N have prime factors  larger  than  these,
but orders of magnitude smaller than N, computations will be about 4 times
slower than for nearby highly composite N's. When N itself is prime, speed
will be 6 times lower.

Algorithm has O(N*logN) complexity for any N (composite or prime).

INPUT PARAMETERS
    A   -   array[0..N-1] - complex function to be transformed
    N   -   problem size
    
OUTPUT PARAMETERS
    A   -   DFT of a input array, array[0..N-1]
            A_out[j] = SUM(A_in[k]*exp(-2*pi*sqrt(-1)*j*k/N), k = 0..N-1)


  -- ALGLIB --
     Copyright 29.05.2009 by Bochkanov Sergey
*************************************************************************)
procedure FFTC1D(var A : TComplex1DArray; N : Integer);
var
    Plan : FTPlan;
    I : Integer;
    Buf : TReal1DArray;
begin
    Assert(N>0, 'FFTC1D: incorrect N!');
    
    //
    // Special case: N=1, FFT is just identity transform.
    // After this block we assume that N is strictly greater than 1.
    //
    if N=1 then
    begin
        Exit;
    end;
    
    //
    // convert input array to the more convinient format
    //
    SetLength(Buf, 2*N);
    I:=0;
    while I<=N-1 do
    begin
        Buf[2*I+0] := A[I].X;
        Buf[2*I+1] := A[I].Y;
        Inc(I);
    end;
    
    //
    // Generate plan and execute it.
    //
    // Plan is a combination of a successive factorizations of N and
    // precomputed data. It is much like a FFTW plan, but is not stored
    // between subroutine calls and is much simpler.
    //
    FTBaseGenerateComplexFFTPlan(N, Plan);
    FTBaseExecutePlan(Buf, 0, N, Plan);
    
    //
    // result
    //
    I:=0;
    while I<=N-1 do
    begin
        A[I].X := Buf[2*I+0];
        A[I].Y := Buf[2*I+1];
        Inc(I);
    end;
end;


(*************************************************************************
1-dimensional complex inverse FFT.

Array size N may be arbitrary number (composite or prime).  Algorithm  has
O(N*logN) complexity for any N (composite or prime).

See FFTC1D() description for more information about algorithm performance.

INPUT PARAMETERS
    A   -   array[0..N-1] - complex array to be transformed
    N   -   problem size

OUTPUT PARAMETERS
    A   -   inverse DFT of a input array, array[0..N-1]
            A_out[j] = SUM(A_in[k]/N*exp(+2*pi*sqrt(-1)*j*k/N), k = 0..N-1)


  -- ALGLIB --
     Copyright 29.05.2009 by Bochkanov Sergey
*************************************************************************)
procedure FFTC1DInv(var A : TComplex1DArray; N : Integer);
var
    I : Integer;
begin
    Assert(N>0, 'FFTC1DInv: incorrect N!');
    
    //
    // Inverse DFT can be expressed in terms of the DFT as
    //
    //     invfft(x) = fft(x')'/N
    //
    // here x' means conj(x).
    //
    I:=0;
    while I<=N-1 do
    begin
        A[I].Y := -A[I].Y;
        Inc(I);
    end;
    FFTC1D(A, N);
    I:=0;
    while I<=N-1 do
    begin
        A[I].X := A[I].X/N;
        A[I].Y := -A[I].Y/N;
        Inc(I);
    end;
end;


(*************************************************************************
1-dimensional real FFT.

Algorithm has O(N*logN) complexity for any N (composite or prime).

INPUT PARAMETERS
    A   -   array[0..N-1] - real function to be transformed
    N   -   problem size

OUTPUT PARAMETERS
    F   -   DFT of a input array, array[0..N-1]
            F[j] = SUM(A[k]*exp(-2*pi*sqrt(-1)*j*k/N), k = 0..N-1)

NOTE:
    F[] satisfies symmetry property F[k] = conj(F[N-k]),  so just one half
of  array  is  usually needed. But for convinience subroutine returns full
complex array (with frequencies above N/2), so its result may be  used  by
other FFT-related subroutines.


  -- ALGLIB --
     Copyright 01.06.2009 by Bochkanov Sergey
*************************************************************************)
procedure FFTR1D(const A : TReal1DArray; N : Integer; var F : TComplex1DArray);
var
    I : Integer;
    N2 : Integer;
    Idx : Integer;
    Hn : Complex;
    HmnC : Complex;
    V : Complex;
    Buf : TReal1DArray;
    Plan : FTPlan;
begin
    Assert(N>0, 'FFTR1D: incorrect N!');
    
    //
    // Special cases:
    // * N=1, FFT is just identity transform.
    // * N=2, FFT is simple too
    //
    // After this block we assume that N is strictly greater than 2
    //
    if N=1 then
    begin
        SetLength(F, 1);
        F[0] := C_Complex(A[0]);
        Exit;
    end;
    if N=2 then
    begin
        SetLength(F, 2);
        F[0].X := A[0]+A[1];
        F[0].Y := 0;
        F[1].X := A[0]-A[1];
        F[1].Y := 0;
        Exit;
    end;
    
    //
    // Choose between odd-size and even-size FFTs
    //
    if N mod 2=0 then
    begin
        
        //
        // even-size real FFT, use reduction to the complex task
        //
        N2 := N div 2;
        SetLength(Buf, N);
        APVMove(@Buf[0], 0, N-1, @A[0], 0, N-1);
        FTBaseGenerateComplexFFTPlan(N2, Plan);
        FTBaseExecutePlan(Buf, 0, N2, Plan);
        SetLength(F, N);
        I:=0;
        while I<=N2 do
        begin
            Idx := 2*(I mod N2);
            Hn.X := Buf[Idx+0];
            Hn.Y := Buf[Idx+1];
            Idx := 2*((N2-I) mod N2);
            HmnC.X := Buf[Idx+0];
            HmnC.Y := -Buf[Idx+1];
            V.X := -Sin(-2*Pi*I/N);
            V.Y := Cos(-2*Pi*I/N);
            F[I] := C_Sub(C_Add(Hn,HmnC),C_Mul(V,C_Sub(Hn,HmnC)));
            F[I].X := Double(0.5)*F[I].X;
            F[I].Y := Double(0.5)*F[I].Y;
            Inc(I);
        end;
        I:=N2+1;
        while I<=N-1 do
        begin
            F[I] := Conj(F[N-I]);
            Inc(I);
        end;
        Exit;
    end
    else
    begin
        
        //
        // use complex FFT
        //
        SetLength(F, N);
        I:=0;
        while I<=N-1 do
        begin
            F[I] := C_Complex(A[I]);
            Inc(I);
        end;
        FFTC1D(F, N);
        Exit;
    end;
end;


(*************************************************************************
1-dimensional real inverse FFT.

Algorithm has O(N*logN) complexity for any N (composite or prime).

INPUT PARAMETERS
    F   -   array[0..floor(N/2)] - frequencies from forward real FFT
    N   -   problem size

OUTPUT PARAMETERS
    A   -   inverse DFT of a input array, array[0..N-1]

NOTE:
    F[] should satisfy symmetry property F[k] = conj(F[N-k]), so just  one
half of frequencies array is needed - elements from 0 to floor(N/2).  F[0]
is ALWAYS real. If N is even F[floor(N/2)] is real too. If N is odd,  then
F[floor(N/2)] has no special properties.

Relying on properties noted above, FFTR1DInv subroutine uses only elements
from 0th to floor(N/2)-th. It ignores imaginary part of F[0],  and in case
N is even it ignores imaginary part of F[floor(N/2)] too.  So you can pass
either frequencies array with N elements or reduced array with roughly N/2
elements - subroutine will successfully transform both.


  -- ALGLIB --
     Copyright 01.06.2009 by Bochkanov Sergey
*************************************************************************)
procedure FFTR1DInv(const F : TComplex1DArray;
     N : Integer;
     var A : TReal1DArray);
var
    I : Integer;
    H : TReal1DArray;
    FH : TComplex1DArray;
begin
    Assert(N>0, 'FFTR1DInv: incorrect N!');
    
    //
    // Special case: N=1, FFT is just identity transform.
    // After this block we assume that N is strictly greater than 1.
    //
    if N=1 then
    begin
        SetLength(A, 1);
        A[0] := F[0].X;
        Exit;
    end;
    
    //
    // inverse real FFT is reduced to the inverse real FHT,
    // which is reduced to the forward real FHT,
    // which is reduced to the forward real FFT.
    //
    // Don't worry, it is really compact and efficient reduction :)
    //
    SetLength(H, N);
    SetLength(A, N);
    H[0] := F[0].X;
    I:=1;
    while I<=Floor(AP_Double(N)/2)-1 do
    begin
        H[I] := F[I].X-F[I].Y;
        H[N-I] := F[I].X+F[I].Y;
        Inc(I);
    end;
    if N mod 2=0 then
    begin
        H[Floor(AP_Double(N)/2)] := F[Floor(AP_Double(N)/2)].X;
    end
    else
    begin
        H[Floor(AP_Double(N)/2)] := F[Floor(AP_Double(N)/2)].X-F[Floor(AP_Double(N)/2)].Y;
        H[Floor(AP_Double(N)/2)+1] := F[Floor(AP_Double(N)/2)].X+F[Floor(AP_Double(N)/2)].Y;
    end;
    FFTR1D(H, N, FH);
    I:=0;
    while I<=N-1 do
    begin
        A[I] := (FH[I].X-FH[I].Y)/N;
        Inc(I);
    end;
end;


(*************************************************************************
Internal subroutine. Never call it directly!


  -- ALGLIB --
     Copyright 01.06.2009 by Bochkanov Sergey
*************************************************************************)
procedure FFTR1DInternalEven(var A : TReal1DArray;
     N : Integer;
     var Buf : TReal1DArray;
     var Plan : FTPlan);
var
    X : Double;
    Y : Double;
    I : Integer;
    N2 : Integer;
    Idx : Integer;
    Hn : Complex;
    HmnC : Complex;
    V : Complex;
begin
    Assert((N>0) and (N mod 2=0), 'FFTR1DEvenInplace: incorrect N!');
    
    //
    // Special cases:
    // * N=2
    //
    // After this block we assume that N is strictly greater than 2
    //
    if N=2 then
    begin
        X := A[0]+A[1];
        Y := A[0]-A[1];
        A[0] := X;
        A[1] := Y;
        Exit;
    end;
    
    //
    // even-size real FFT, use reduction to the complex task
    //
    N2 := N div 2;
    APVMove(@Buf[0], 0, N-1, @A[0], 0, N-1);
    FTBaseExecutePlan(Buf, 0, N2, Plan);
    A[0] := Buf[0]+Buf[1];
    I:=1;
    while I<=N2-1 do
    begin
        Idx := 2*(I mod N2);
        Hn.X := Buf[Idx+0];
        Hn.Y := Buf[Idx+1];
        Idx := 2*(N2-I);
        HmnC.X := Buf[Idx+0];
        HmnC.Y := -Buf[Idx+1];
        V.X := -Sin(-2*Pi*I/N);
        V.Y := Cos(-2*Pi*I/N);
        V := C_Sub(C_Add(Hn,HmnC),C_Mul(V,C_Sub(Hn,HmnC)));
        A[2*I+0] := Double(0.5)*V.X;
        A[2*I+1] := Double(0.5)*V.Y;
        Inc(I);
    end;
    A[1] := Buf[0]-Buf[1];
end;


(*************************************************************************
Internal subroutine. Never call it directly!


  -- ALGLIB --
     Copyright 01.06.2009 by Bochkanov Sergey
*************************************************************************)
procedure FFTR1DInvInternalEven(var A : TReal1DArray;
     N : Integer;
     var Buf : TReal1DArray;
     var Plan : FTPlan);
var
    X : Double;
    Y : Double;
    T : Double;
    I : Integer;
    N2 : Integer;
begin
    Assert((N>0) and (N mod 2=0), 'FFTR1DInvInternalEven: incorrect N!');
    
    //
    // Special cases:
    // * N=2
    //
    // After this block we assume that N is strictly greater than 2
    //
    if N=2 then
    begin
        X := Double(0.5)*(A[0]+A[1]);
        Y := Double(0.5)*(A[0]-A[1]);
        A[0] := X;
        A[1] := Y;
        Exit;
    end;
    
    //
    // inverse real FFT is reduced to the inverse real FHT,
    // which is reduced to the forward real FHT,
    // which is reduced to the forward real FFT.
    //
    // Don't worry, it is really compact and efficient reduction :)
    //
    N2 := N div 2;
    Buf[0] := A[0];
    I:=1;
    while I<=N2-1 do
    begin
        X := A[2*I+0];
        Y := A[2*I+1];
        Buf[I] := X-Y;
        Buf[N-I] := X+Y;
        Inc(I);
    end;
    Buf[N2] := A[1];
    FFTR1DInternalEven(Buf, N, A, Plan);
    A[0] := Buf[0]/N;
    T := AP_Double(1)/N;
    I:=1;
    while I<=N2-1 do
    begin
        X := Buf[2*I+0];
        Y := Buf[2*I+1];
        A[I] := T*(X-Y);
        A[N-I] := T*(X+Y);
        Inc(I);
    end;
    A[N2] := Buf[1]/N;
end;


end.