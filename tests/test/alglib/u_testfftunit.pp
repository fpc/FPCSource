unit u_testfftunit;
interface
uses Math, Sysutils, u_ap, u_ftbase, u_fft;

function TestFFT(Silent : Boolean):Boolean;
function testfftunit_test_silent():Boolean;
function testfftunit_test():Boolean;

implementation

procedure RefFFTC1D(var A : TComplex1DArray; N : Integer);forward;
procedure RefFFTC1DInv(var A : TComplex1DArray; N : Integer);forward;
procedure RefInternalCFFT(var a : TReal1DArray;
     nn : Integer;
     InverseFFT : Boolean);forward;
procedure RefInternalRFFT(const a : TReal1DArray;
     nn : Integer;
     var F : TComplex1DArray);forward;


(*************************************************************************
Test
*************************************************************************)
function TestFFT(Silent : Boolean):Boolean;
var
    N : Integer;
    I : Integer;
    K : Integer;
    A1 : TComplex1DArray;
    A2 : TComplex1DArray;
    A3 : TComplex1DArray;
    R1 : TReal1DArray;
    R2 : TReal1DArray;
    Buf : TReal1DArray;
    Plan : FTPlan;
    MaxN : Integer;
    BiDiErr : Double;
    BiDiRErr : Double;
    RefErr : Double;
    RefRErr : Double;
    REIntErr : Double;
    ErrTol : Double;
    RefErrors : Boolean;
    BiDiErrors : Boolean;
    RefRErrors : Boolean;
    BiDiRErrors : Boolean;
    REIntErrors : Boolean;
    WasErrors : Boolean;
begin
    MaxN := 128;
    ErrTol := 100000*Power(MaxN, AP_Double(3)/2)*MachineEpsilon;
    BiDiErrors := False;
    RefErrors := False;
    BiDiRErrors := False;
    RefRErrors := False;
    REIntErrors := False;
    WasErrors := False;
    
    //
    // Test bi-directional error: norm(x-invFFT(FFT(x)))
    //
    BiDiErr := 0;
    BiDiRErr := 0;
    N:=1;
    while N<=MaxN do
    begin
        
        //
        // Complex FFT/invFFT
        //
        SetLength(A1, N);
        SetLength(A2, N);
        SetLength(A3, N);
        I:=0;
        while I<=N-1 do
        begin
            A1[I].X := 2*RandomReal-1;
            A1[I].Y := 2*RandomReal-1;
            A2[I] := A1[I];
            A3[I] := A1[I];
            Inc(I);
        end;
        FFTC1D(A2, N);
        FFTC1DInv(A2, N);
        FFTC1DInv(A3, N);
        FFTC1D(A3, N);
        I:=0;
        while I<=N-1 do
        begin
            BiDiErr := Max(BiDiErr, AbsComplex(C_Sub(A1[I],A2[I])));
            BiDiErr := Max(BiDiErr, AbsComplex(C_Sub(A1[I],A3[I])));
            Inc(I);
        end;
        
        //
        // Real
        //
        SetLength(R1, N);
        SetLength(R2, N);
        I:=0;
        while I<=N-1 do
        begin
            R1[I] := 2*RandomReal-1;
            R2[I] := R1[I];
            Inc(I);
        end;
        FFTR1D(R2, N, A1);
        APVMul(@R2[0], 0, N-1, 0);
        FFTR1DInv(A1, N, R2);
        I:=0;
        while I<=N-1 do
        begin
            BiDiRErr := Max(BiDiRErr, AbsComplex(C_Complex(R1[I]-R2[I])));
            Inc(I);
        end;
        Inc(N);
    end;
    BiDiErrors := BiDiErrors or AP_FP_Greater(BiDiErr,ErrTol);
    BiDiRErrors := BiDiRErrors or AP_FP_Greater(BiDiRErr,ErrTol);
    
    //
    // Test against reference O(N^2) implementation
    //
    RefErr := 0;
    RefRErr := 0;
    N:=1;
    while N<=MaxN do
    begin
        
        //
        // Complex FFT
        //
        SetLength(A1, N);
        SetLength(A2, N);
        I:=0;
        while I<=N-1 do
        begin
            A1[I].X := 2*RandomReal-1;
            A1[I].Y := 2*RandomReal-1;
            A2[I] := A1[I];
            Inc(I);
        end;
        FFTC1D(A1, N);
        RefFFTC1D(A2, N);
        I:=0;
        while I<=N-1 do
        begin
            RefErr := Max(RefErr, AbsComplex(C_Sub(A1[I],A2[I])));
            Inc(I);
        end;
        
        //
        // Complex inverse FFT
        //
        SetLength(A1, N);
        SetLength(A2, N);
        I:=0;
        while I<=N-1 do
        begin
            A1[I].X := 2*RandomReal-1;
            A1[I].Y := 2*RandomReal-1;
            A2[I] := A1[I];
            Inc(I);
        end;
        FFTC1DInv(A1, N);
        RefFFTC1DInv(A2, N);
        I:=0;
        while I<=N-1 do
        begin
            RefErr := Max(RefErr, AbsComplex(C_Sub(A1[I],A2[I])));
            Inc(I);
        end;
        
        //
        // Real forward/inverse FFT:
        // * calculate and check forward FFT
        // * use precalculated FFT to check backward FFT
        //   fill unused parts of frequencies array with random numbers
        //   to ensure that they are not really used
        //
        SetLength(R1, N);
        SetLength(R2, N);
        I:=0;
        while I<=N-1 do
        begin
            R1[I] := 2*RandomReal-1;
            R2[I] := R1[I];
            Inc(I);
        end;
        FFTR1D(R1, N, A1);
        RefInternalRFFT(R2, N, A2);
        I:=0;
        while I<=N-1 do
        begin
            RefRErr := Max(RefRErr, AbsComplex(C_Sub(A1[I],A2[I])));
            Inc(I);
        end;
        SetLength(A3, Floor(AP_Double(N)/2)+1);
        I:=0;
        while I<=Floor(AP_Double(N)/2) do
        begin
            A3[I] := A2[I];
            Inc(I);
        end;
        A3[0].Y := 2*RandomReal-1;
        if N mod 2=0 then
        begin
            A3[Floor(AP_Double(N)/2)].Y := 2*RandomReal-1;
        end;
        I:=0;
        while I<=N-1 do
        begin
            R1[I] := 0;
            Inc(I);
        end;
        FFTR1DInv(A3, N, R1);
        I:=0;
        while I<=N-1 do
        begin
            RefRErr := Max(RefRErr, AbsReal(R2[I]-R1[I]));
            Inc(I);
        end;
        Inc(N);
    end;
    RefErrors := RefErrors or AP_FP_Greater(RefErr,ErrTol);
    RefRErrors := RefRErrors or AP_FP_Greater(RefRErr,ErrTol);
    
    //
    // test internal real even FFT
    //
    REIntErr := 0;
    K:=1;
    while K<=MaxN div 2 do
    begin
        N := 2*K;
        
        //
        // Real forward FFT
        //
        SetLength(R1, N);
        SetLength(R2, N);
        I:=0;
        while I<=N-1 do
        begin
            R1[I] := 2*RandomReal-1;
            R2[I] := R1[I];
            Inc(I);
        end;
        FTBaseGenerateComplexFFTPlan(N div 2, Plan);
        SetLength(Buf, N);
        FFTR1DInternalEven(R1, N, Buf, Plan);
        RefInternalRFFT(R2, N, A2);
        REIntErr := Max(REIntErr, AbsReal(R1[0]-A2[0].X));
        REIntErr := Max(REIntErr, AbsReal(R1[1]-A2[N div 2].X));
        I:=1;
        while I<=N div 2-1 do
        begin
            REIntErr := Max(REIntErr, AbsReal(R1[2*I+0]-A2[I].X));
            REIntErr := Max(REIntErr, AbsReal(R1[2*I+1]-A2[I].Y));
            Inc(I);
        end;
        
        //
        // Real backward FFT
        //
        SetLength(R1, N);
        I:=0;
        while I<=N-1 do
        begin
            R1[I] := 2*RandomReal-1;
            Inc(I);
        end;
        SetLength(A2, Floor(AP_Double(N)/2)+1);
        A2[0] := C_Complex(R1[0]);
        I:=1;
        while I<=Floor(AP_Double(N)/2)-1 do
        begin
            A2[I].X := R1[2*I+0];
            A2[I].Y := R1[2*I+1];
            Inc(I);
        end;
        A2[Floor(AP_Double(N)/2)] := C_Complex(R1[1]);
        FTBaseGenerateComplexFFTPlan(N div 2, Plan);
        SetLength(Buf, N);
        FFTR1DInvInternalEven(R1, N, Buf, Plan);
        FFTR1DInv(A2, N, R2);
        I:=0;
        while I<=N-1 do
        begin
            REIntErr := Max(REIntErr, AbsReal(R1[I]-R2[I]));
            Inc(I);
        end;
        Inc(K);
    end;
    REIntErrors := REIntErrors or AP_FP_Greater(REIntErr,ErrTol);
    
    //
    // end
    //
    WasErrors := BiDiErrors or BiDiRErrors or RefErrors or RefRErrors or REIntErrors;
    if  not Silent then
    begin
        Write(Format('TESTING FFT'#13#10'',[]));
        Write(Format('FINAL RESULT:                             ',[]));
        if WasErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* BI-DIRECTIONAL COMPLEX TEST:            ',[]));
        if BiDiErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* AGAINST REFERENCE COMPLEX FFT:          ',[]));
        if RefErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* BI-DIRECTIONAL REAL TEST:               ',[]));
        if BiDiRErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* AGAINST REFERENCE REAL FFT:             ',[]));
        if RefRErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* INTERNAL EVEN FFT:                      ',[]));
        if REIntErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        if WasErrors then
        begin
            Write(Format('TEST FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('TEST PASSED'#13#10'',[]));
        end;
    end;
    Result :=  not WasErrors;
end;


(*************************************************************************
Reference FFT
*************************************************************************)
procedure RefFFTC1D(var A : TComplex1DArray; N : Integer);
var
    Buf : TReal1DArray;
    I : Integer;
begin
    Assert(N>0, 'FFTC1D: incorrect N!');
    SetLength(Buf, 2*N);
    I:=0;
    while I<=N-1 do
    begin
        Buf[2*I+0] := A[I].X;
        Buf[2*I+1] := A[I].Y;
        Inc(I);
    end;
    RefInternalCFFT(Buf, N, False);
    I:=0;
    while I<=N-1 do
    begin
        A[I].X := Buf[2*I+0];
        A[I].Y := Buf[2*I+1];
        Inc(I);
    end;
end;


(*************************************************************************
Reference inverse FFT
*************************************************************************)
procedure RefFFTC1DInv(var A : TComplex1DArray; N : Integer);
var
    Buf : TReal1DArray;
    I : Integer;
begin
    Assert(N>0, 'FFTC1DInv: incorrect N!');
    SetLength(Buf, 2*N);
    I:=0;
    while I<=N-1 do
    begin
        Buf[2*I+0] := A[I].X;
        Buf[2*I+1] := A[I].Y;
        Inc(I);
    end;
    RefInternalCFFT(Buf, N, True);
    I:=0;
    while I<=N-1 do
    begin
        A[I].X := Buf[2*I+0];
        A[I].Y := Buf[2*I+1];
        Inc(I);
    end;
end;


(*************************************************************************
Internal complex FFT stub.
Uses straightforward formula with O(N^2) complexity.
*************************************************************************)
procedure RefInternalCFFT(var a : TReal1DArray;
     nn : Integer;
     InverseFFT : Boolean);
var
    Tmp : TReal1DArray;
    I : Integer;
    J : Integer;
    K : Integer;
    Hre : Double;
    Him : Double;
    C : Double;
    S : Double;
    re : Double;
    im : Double;
begin
    SetLength(Tmp, 2*nn-1+1);
    if  not InverseFFT then
    begin
        i:=0;
        while i<=nn-1 do
        begin
            HRe := 0;
            Him := 0;
            K:=0;
            while K<=nn-1 do
            begin
                re := A[2*k];
                im := A[2*k+1];
                C := Cos(-2*Pi*k*i/nn);
                S := Sin(-2*Pi*k*i/nn);
                HRe := HRe+C*Re-S*im;
                HIm := HIm+C*im+S*re;
                Inc(K);
            end;
            Tmp[2*i] := Hre;
            Tmp[2*i+1] := Him;
            Inc(i);
        end;
        i:=0;
        while i<=2*nn-1 do
        begin
            A[I] := Tmp[I];
            Inc(i);
        end;
    end
    else
    begin
        k:=0;
        while k<=nn-1 do
        begin
            HRe := 0;
            Him := 0;
            i:=0;
            while i<=nn-1 do
            begin
                re := A[2*i];
                im := A[2*i+1];
                C := Cos(2*Pi*k*i/nn);
                S := Sin(2*Pi*k*i/nn);
                HRe := HRe+C*Re-S*im;
                HIm := HIm+C*im+S*re;
                Inc(i);
            end;
            Tmp[2*k] := Hre/nn;
            Tmp[2*k+1] := Him/nn;
            Inc(k);
        end;
        i:=0;
        while i<=2*nn-1 do
        begin
            A[I] := Tmp[I];
            Inc(i);
        end;
    end;
end;


(*************************************************************************
Internal real FFT stub.
Uses straightforward formula with O(N^2) complexity.
*************************************************************************)
procedure RefInternalRFFT(const a : TReal1DArray;
     nn : Integer;
     var F : TComplex1DArray);
var
    Tmp : TReal1DArray;
    I : Integer;
begin
    SetLength(Tmp, 2*nn-1+1);
    I:=0;
    while I<=nn-1 do
    begin
        Tmp[2*I] := A[I];
        Tmp[2*I+1] := 0;
        Inc(I);
    end;
    RefInternalCFFT(Tmp, nn, False);
    SetLength(F, nn);
    I:=0;
    while I<=nn-1 do
    begin
        F[I].X := Tmp[2*I+0];
        F[I].Y := Tmp[2*I+1];
        Inc(I);
    end;
end;


(*************************************************************************
Silent unit test
*************************************************************************)
function testfftunit_test_silent():Boolean;
begin
    Result := TestFFT(True);
end;


(*************************************************************************
Unit test
*************************************************************************)
function testfftunit_test():Boolean;
begin
    Result := TestFFT(False);
end;


end.