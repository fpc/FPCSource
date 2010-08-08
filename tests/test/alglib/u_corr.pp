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
unit u_corr;
interface
uses Math, Sysutils, u_ap, u_ftbase, u_fft, u_conv;

procedure CorrC1D(const Signal : TComplex1DArray;
     N : Integer;
     const Pattern : TComplex1DArray;
     M : Integer;
     var R : TComplex1DArray);
procedure CorrC1DCircular(const Signal : TComplex1DArray;
     M : Integer;
     const Pattern : TComplex1DArray;
     N : Integer;
     var C : TComplex1DArray);
procedure CorrR1D(const Signal : TReal1DArray;
     N : Integer;
     const Pattern : TReal1DArray;
     M : Integer;
     var R : TReal1DArray);
procedure CorrR1DCircular(const Signal : TReal1DArray;
     M : Integer;
     const Pattern : TReal1DArray;
     N : Integer;
     var C : TReal1DArray);

implementation

(*************************************************************************
1-dimensional complex cross-correlation.

For given Pattern/Signal returns corr(Pattern,Signal) (non-circular).

Correlation is calculated using reduction to  convolution.  Algorithm with
max(N,N)*log(max(N,N)) complexity is used (see  ConvC1D()  for  more  info
about performance).

IMPORTANT:
    for  historical reasons subroutine accepts its parameters in  reversed
    order: CorrC1D(Signal, Pattern) = Pattern x Signal (using  traditional
    definition of cross-correlation, denoting cross-correlation as "x").

INPUT PARAMETERS
    Signal  -   array[0..N-1] - complex function to be transformed,
                signal containing pattern
    N       -   problem size
    Pattern -   array[0..M-1] - complex function to be transformed,
                pattern to search withing signal
    M       -   problem size

OUTPUT PARAMETERS
    R       -   cross-correlation, array[0..N+M-2]:
                * positive lags are stored in R[0..N-1],
                  R[i] = sum(conj(pattern[j])*signal[i+j]
                * negative lags are stored in R[N..N+M-2],
                  R[N+M-1-i] = sum(conj(pattern[j])*signal[-i+j]

NOTE:
    It is assumed that pattern domain is [0..M-1].  If Pattern is non-zero
on [-K..M-1],  you can still use this subroutine, just shift result by K.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure CorrC1D(const Signal : TComplex1DArray;
     N : Integer;
     const Pattern : TComplex1DArray;
     M : Integer;
     var R : TComplex1DArray);
var
    P : TComplex1DArray;
    B : TComplex1DArray;
    I : Integer;
    i_ : Integer;
    i1_ : Integer;
begin
    Assert((N>0) and (M>0), 'CorrC1D: incorrect N or M!');
    SetLength(P, M);
    I:=0;
    while I<=M-1 do
    begin
        P[M-1-I] := Conj(Pattern[I]);
        Inc(I);
    end;
    ConvC1D(P, M, Signal, N, B);
    SetLength(R, M+N-1);
    i1_ := (M-1) - (0);
    for i_ := 0 to N-1 do
    begin
        R[i_] := B[i_+i1_];
    end;
    if M+N-2>=N then
    begin
        i1_ := (0) - (N);
        for i_ := N to M+N-2 do
        begin
            R[i_] := B[i_+i1_];
        end;
    end;
end;


(*************************************************************************
1-dimensional circular complex cross-correlation.

For given Pattern/Signal returns corr(Pattern,Signal) (circular).
Algorithm has linearithmic complexity for any M/N.

IMPORTANT:
    for  historical reasons subroutine accepts its parameters in  reversed
    order:   CorrC1DCircular(Signal, Pattern) = Pattern x Signal    (using
    traditional definition of cross-correlation, denoting cross-correlation
    as "x").

INPUT PARAMETERS
    Signal  -   array[0..N-1] - complex function to be transformed,
                periodic signal containing pattern
    N       -   problem size
    Pattern -   array[0..M-1] - complex function to be transformed,
                non-periodic pattern to search withing signal
    M       -   problem size

OUTPUT PARAMETERS
    R   -   convolution: A*B. array[0..M-1].


  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure CorrC1DCircular(const Signal : TComplex1DArray;
     M : Integer;
     const Pattern : TComplex1DArray;
     N : Integer;
     var C : TComplex1DArray);
var
    P : TComplex1DArray;
    B : TComplex1DArray;
    I1 : Integer;
    I2 : Integer;
    I : Integer;
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
        SetLength(B, M);
        I1:=0;
        while I1<=M-1 do
        begin
            B[I1] := C_Complex(0);
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
                B[i_] := C_Add(B[i_], Pattern[i_+i1_]);
            end;
            I1 := I1+M;
        end;
        CorrC1DCircular(Signal, M, B, M, C);
        Exit;
    end;
    
    //
    // Task is normalized
    //
    SetLength(P, N);
    I:=0;
    while I<=N-1 do
    begin
        P[N-1-I] := Conj(Pattern[I]);
        Inc(I);
    end;
    ConvC1DCircular(Signal, M, P, N, B);
    SetLength(C, M);
    i1_ := (N-1) - (0);
    for i_ := 0 to M-N do
    begin
        C[i_] := B[i_+i1_];
    end;
    if M-N+1<=M-1 then
    begin
        i1_ := (0) - (M-N+1);
        for i_ := M-N+1 to M-1 do
        begin
            C[i_] := B[i_+i1_];
        end;
    end;
end;


(*************************************************************************
1-dimensional real cross-correlation.

For given Pattern/Signal returns corr(Pattern,Signal) (non-circular).

Correlation is calculated using reduction to  convolution.  Algorithm with
max(N,N)*log(max(N,N)) complexity is used (see  ConvC1D()  for  more  info
about performance).

IMPORTANT:
    for  historical reasons subroutine accepts its parameters in  reversed
    order: CorrR1D(Signal, Pattern) = Pattern x Signal (using  traditional
    definition of cross-correlation, denoting cross-correlation as "x").

INPUT PARAMETERS
    Signal  -   array[0..N-1] - real function to be transformed,
                signal containing pattern
    N       -   problem size
    Pattern -   array[0..M-1] - real function to be transformed,
                pattern to search withing signal
    M       -   problem size

OUTPUT PARAMETERS
    R       -   cross-correlation, array[0..N+M-2]:
                * positive lags are stored in R[0..N-1],
                  R[i] = sum(pattern[j]*signal[i+j]
                * negative lags are stored in R[N..N+M-2],
                  R[N+M-1-i] = sum(pattern[j]*signal[-i+j]

NOTE:
    It is assumed that pattern domain is [0..M-1].  If Pattern is non-zero
on [-K..M-1],  you can still use this subroutine, just shift result by K.

  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure CorrR1D(const Signal : TReal1DArray;
     N : Integer;
     const Pattern : TReal1DArray;
     M : Integer;
     var R : TReal1DArray);
var
    P : TReal1DArray;
    B : TReal1DArray;
    I : Integer;
begin
    Assert((N>0) and (M>0), 'CorrR1D: incorrect N or M!');
    SetLength(P, M);
    I:=0;
    while I<=M-1 do
    begin
        P[M-1-I] := Pattern[I];
        Inc(I);
    end;
    ConvR1D(P, M, Signal, N, B);
    SetLength(R, M+N-1);
    APVMove(@R[0], 0, N-1, @B[0], M-1, M+N-2);
    if M+N-2>=N then
    begin
        APVMove(@R[0], N, M+N-2, @B[0], 0, M-2);
    end;
end;


(*************************************************************************
1-dimensional circular real cross-correlation.

For given Pattern/Signal returns corr(Pattern,Signal) (circular).
Algorithm has linearithmic complexity for any M/N.

IMPORTANT:
    for  historical reasons subroutine accepts its parameters in  reversed
    order:   CorrR1DCircular(Signal, Pattern) = Pattern x Signal    (using
    traditional definition of cross-correlation, denoting cross-correlation
    as "x").

INPUT PARAMETERS
    Signal  -   array[0..N-1] - real function to be transformed,
                periodic signal containing pattern
    N       -   problem size
    Pattern -   array[0..M-1] - real function to be transformed,
                non-periodic pattern to search withing signal
    M       -   problem size

OUTPUT PARAMETERS
    R   -   convolution: A*B. array[0..M-1].


  -- ALGLIB --
     Copyright 21.07.2009 by Bochkanov Sergey
*************************************************************************)
procedure CorrR1DCircular(const Signal : TReal1DArray;
     M : Integer;
     const Pattern : TReal1DArray;
     N : Integer;
     var C : TReal1DArray);
var
    P : TReal1DArray;
    B : TReal1DArray;
    I1 : Integer;
    I2 : Integer;
    I : Integer;
    J2 : Integer;
begin
    Assert((N>0) and (M>0), 'ConvC1DCircular: incorrect N or M!');
    
    //
    // normalize task: make M>=N,
    // so A will be longer (at least - not shorter) that B.
    //
    if M<N then
    begin
        SetLength(B, M);
        I1:=0;
        while I1<=M-1 do
        begin
            B[I1] := 0;
            Inc(I1);
        end;
        I1 := 0;
        while I1<N do
        begin
            I2 := Min(I1+M-1, N-1);
            J2 := I2-I1;
            APVAdd(@B[0], 0, J2, @Pattern[0], I1, I2);
            I1 := I1+M;
        end;
        CorrR1DCircular(Signal, M, B, M, C);
        Exit;
    end;
    
    //
    // Task is normalized
    //
    SetLength(P, N);
    I:=0;
    while I<=N-1 do
    begin
        P[N-1-I] := Pattern[I];
        Inc(I);
    end;
    ConvR1DCircular(Signal, M, P, N, B);
    SetLength(C, M);
    APVMove(@C[0], 0, M-N, @B[0], N-1, M-1);
    if M-N+1<=M-1 then
    begin
        APVMove(@C[0], M-N+1, M-1, @B[0], 0, N-2);
    end;
end;


end.