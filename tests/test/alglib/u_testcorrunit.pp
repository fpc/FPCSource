unit u_testcorrunit;
interface
uses Math, Sysutils, u_ap, u_ftbase, u_fft, u_conv, u_corr;

function TestCorr(Silent : Boolean):Boolean;
function testcorrunit_test_silent():Boolean;
function testcorrunit_test():Boolean;

implementation

procedure RefCorrC1D(const Signal : TComplex1DArray;
     N : Integer;
     const Pattern : TComplex1DArray;
     M : Integer;
     var R : TComplex1DArray);forward;
procedure RefCorrC1DCircular(const Signal : TComplex1DArray;
     N : Integer;
     const Pattern : TComplex1DArray;
     M : Integer;
     var R : TComplex1DArray);forward;
procedure RefCorrR1D(const Signal : TReal1DArray;
     N : Integer;
     const Pattern : TReal1DArray;
     M : Integer;
     var R : TReal1DArray);forward;
procedure RefCorrR1DCircular(const Signal : TReal1DArray;
     N : Integer;
     const Pattern : TReal1DArray;
     M : Integer;
     var R : TReal1DArray);forward;
procedure RefConvC1D(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);forward;
procedure RefConvC1DCircular(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);forward;
procedure RefConvR1D(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);forward;
procedure RefConvR1DCircular(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);forward;


(*************************************************************************
Test
*************************************************************************)
function TestCorr(Silent : Boolean):Boolean;
var
    M : Integer;
    N : Integer;
    I : Integer;
    RA : TReal1DArray;
    RB : TReal1DArray;
    RR1 : TReal1DArray;
    RR2 : TReal1DArray;
    CA : TComplex1DArray;
    CB : TComplex1DArray;
    CR1 : TComplex1DArray;
    CR2 : TComplex1DArray;
    MaxN : Integer;
    RefErr : Double;
    RefRErr : Double;
    InvErr : Double;
    InvRErr : Double;
    ErrTol : Double;
    RefErrors : Boolean;
    RefRErrors : Boolean;
    InvErrors : Boolean;
    InvRErrors : Boolean;
    WasErrors : Boolean;
begin
    MaxN := 32;
    ErrTol := 100000*Power(MaxN, AP_Double(3)/2)*MachineEpsilon;
    RefErrors := False;
    RefRErrors := False;
    InvErrors := False;
    InvRErrors := False;
    WasErrors := False;
    
    //
    // Test against reference O(N^2) implementation.
    //
    RefErr := 0;
    RefRErr := 0;
    M:=1;
    while M<=MaxN do
    begin
        N:=1;
        while N<=MaxN do
        begin
            
            //
            // Complex correlation
            //
            SetLength(CA, M);
            I:=0;
            while I<=M-1 do
            begin
                CA[I].X := 2*RandomReal-1;
                CA[I].Y := 2*RandomReal-1;
                Inc(I);
            end;
            SetLength(CB, N);
            I:=0;
            while I<=N-1 do
            begin
                CB[I].X := 2*RandomReal-1;
                CB[I].Y := 2*RandomReal-1;
                Inc(I);
            end;
            SetLength(CR1, 1);
            CorrC1D(CA, M, CB, N, CR1);
            RefCorrC1D(CA, M, CB, N, CR2);
            I:=0;
            while I<=M+N-2 do
            begin
                RefErr := Max(RefErr, AbsComplex(C_Sub(CR1[I],CR2[I])));
                Inc(I);
            end;
            SetLength(CR1, 1);
            CorrC1DCircular(CA, M, CB, N, CR1);
            RefCorrC1DCircular(CA, M, CB, N, CR2);
            I:=0;
            while I<=M-1 do
            begin
                RefErr := Max(RefErr, AbsComplex(C_Sub(CR1[I],CR2[I])));
                Inc(I);
            end;
            
            //
            // Real correlation
            //
            SetLength(RA, M);
            I:=0;
            while I<=M-1 do
            begin
                RA[I] := 2*RandomReal-1;
                Inc(I);
            end;
            SetLength(RB, N);
            I:=0;
            while I<=N-1 do
            begin
                RB[I] := 2*RandomReal-1;
                Inc(I);
            end;
            SetLength(RR1, 1);
            CorrR1D(RA, M, RB, N, RR1);
            RefCorrR1D(RA, M, RB, N, RR2);
            I:=0;
            while I<=M+N-2 do
            begin
                RefRErr := Max(RefRErr, AbsReal(RR1[I]-RR2[I]));
                Inc(I);
            end;
            SetLength(RR1, 1);
            CorrR1DCircular(RA, M, RB, N, RR1);
            RefCorrR1DCircular(RA, M, RB, N, RR2);
            I:=0;
            while I<=M-1 do
            begin
                RefRErr := Max(RefRErr, AbsReal(RR1[I]-RR2[I]));
                Inc(I);
            end;
            Inc(N);
        end;
        Inc(M);
    end;
    RefErrors := RefErrors or AP_FP_Greater(RefErr,ErrTol);
    RefRErrors := RefRErrors or AP_FP_Greater(RefRErr,ErrTol);
    
    //
    // end
    //
    WasErrors := RefErrors or RefRErrors;
    if  not Silent then
    begin
        Write(Format('TESTING CORRELATION'#13#10'',[]));
        Write(Format('FINAL RESULT:                             ',[]));
        if WasErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* AGAINST REFERENCE COMPLEX CORR:         ',[]));
        if RefErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* AGAINST REFERENCE REAL CORR:            ',[]));
        if RefRErrors then
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
Reference implementation
*************************************************************************)
procedure RefCorrC1D(const Signal : TComplex1DArray;
     N : Integer;
     const Pattern : TComplex1DArray;
     M : Integer;
     var R : TComplex1DArray);
var
    I : Integer;
    J : Integer;
    V : Complex;
    S : TComplex1DArray;
    i_ : Integer;
begin
    SetLength(S, M+N-1);
    for i_ := 0 to N-1 do
    begin
        S[i_] := Signal[i_];
    end;
    I:=N;
    while I<=M+N-2 do
    begin
        S[I] := C_Complex(0);
        Inc(I);
    end;
    SetLength(R, M+N-1);
    I:=0;
    while I<=N-1 do
    begin
        V := C_Complex(0);
        J:=0;
        while J<=M-1 do
        begin
            if I+J>=N then
            begin
                Break;
            end;
            V := C_Add(V,C_Mul(Conj(Pattern[J]),S[I+J]));
            Inc(J);
        end;
        R[I] := V;
        Inc(I);
    end;
    I:=1;
    while I<=M-1 do
    begin
        V := C_Complex(0);
        J:=I;
        while J<=M-1 do
        begin
            V := C_Add(V,C_Mul(Conj(Pattern[J]),S[J-I]));
            Inc(J);
        end;
        R[M+N-1-I] := V;
        Inc(I);
    end;
end;


(*************************************************************************
Reference implementation
*************************************************************************)
procedure RefCorrC1DCircular(const Signal : TComplex1DArray;
     N : Integer;
     const Pattern : TComplex1DArray;
     M : Integer;
     var R : TComplex1DArray);
var
    I : Integer;
    J : Integer;
    V : Complex;
begin
    SetLength(R, N);
    I:=0;
    while I<=N-1 do
    begin
        V := C_Complex(0);
        J:=0;
        while J<=M-1 do
        begin
            V := C_Add(V,C_Mul(Conj(Pattern[J]),Signal[(I+J) mod N]));
            Inc(J);
        end;
        R[I] := V;
        Inc(I);
    end;
end;


(*************************************************************************
Reference implementation
*************************************************************************)
procedure RefCorrR1D(const Signal : TReal1DArray;
     N : Integer;
     const Pattern : TReal1DArray;
     M : Integer;
     var R : TReal1DArray);
var
    I : Integer;
    J : Integer;
    V : Double;
    S : TReal1DArray;
begin
    SetLength(S, M+N-1);
    APVMove(@S[0], 0, N-1, @Signal[0], 0, N-1);
    I:=N;
    while I<=M+N-2 do
    begin
        S[I] := 0;
        Inc(I);
    end;
    SetLength(R, M+N-1);
    I:=0;
    while I<=N-1 do
    begin
        V := 0;
        J:=0;
        while J<=M-1 do
        begin
            if I+J>=N then
            begin
                Break;
            end;
            V := V+Pattern[J]*S[I+J];
            Inc(J);
        end;
        R[I] := V;
        Inc(I);
    end;
    I:=1;
    while I<=M-1 do
    begin
        V := 0;
        J:=I;
        while J<=M-1 do
        begin
            V := V+Pattern[J]*S[-I+J];
            Inc(J);
        end;
        R[M+N-1-I] := V;
        Inc(I);
    end;
end;


(*************************************************************************
Reference implementation
*************************************************************************)
procedure RefCorrR1DCircular(const Signal : TReal1DArray;
     N : Integer;
     const Pattern : TReal1DArray;
     M : Integer;
     var R : TReal1DArray);
var
    I : Integer;
    J : Integer;
    V : Double;
begin
    SetLength(R, N);
    I:=0;
    while I<=N-1 do
    begin
        V := 0;
        J:=0;
        while J<=M-1 do
        begin
            V := V+Pattern[J]*Signal[(I+J) mod N];
            Inc(J);
        end;
        R[I] := V;
        Inc(I);
    end;
end;


(*************************************************************************
Reference implementation
*************************************************************************)
procedure RefConvC1D(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);
var
    I : Integer;
    V : Complex;
    i_ : Integer;
    i1_ : Integer;
begin
    SetLength(R, M+N-1);
    I:=0;
    while I<=M+N-2 do
    begin
        R[I] := C_Complex(0);
        Inc(I);
    end;
    I:=0;
    while I<=M-1 do
    begin
        V := A[I];
        i1_ := (0) - (I);
        for i_ := I to I+N-1 do
        begin
            R[i_] := C_Add(R[i_], C_Mul(V, B[i_+i1_]));
        end;
        Inc(I);
    end;
end;


(*************************************************************************
Reference implementation
*************************************************************************)
procedure RefConvC1DCircular(const A : TComplex1DArray;
     M : Integer;
     const B : TComplex1DArray;
     N : Integer;
     var R : TComplex1DArray);
var
    I1 : Integer;
    I2 : Integer;
    J2 : Integer;
    Buf : TComplex1DArray;
    i_ : Integer;
    i1_ : Integer;
begin
    RefConvC1D(A, M, B, N, Buf);
    SetLength(R, M);
    for i_ := 0 to M-1 do
    begin
        R[i_] := Buf[i_];
    end;
    I1 := M;
    while I1<=M+N-2 do
    begin
        I2 := Min(I1+M-1, M+N-2);
        J2 := I2-I1;
        i1_ := (I1) - (0);
        for i_ := 0 to J2 do
        begin
            R[i_] := C_Add(R[i_], Buf[i_+i1_]);
        end;
        I1 := I1+M;
    end;
end;


(*************************************************************************
Reference FFT
*************************************************************************)
procedure RefConvR1D(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);
var
    I : Integer;
    V : Double;
begin
    SetLength(R, M+N-1);
    I:=0;
    while I<=M+N-2 do
    begin
        R[I] := 0;
        Inc(I);
    end;
    I:=0;
    while I<=M-1 do
    begin
        V := A[I];
        APVAdd(@R[0], I, I+N-1, @B[0], 0, N-1, V);
        Inc(I);
    end;
end;


(*************************************************************************
Reference implementation
*************************************************************************)
procedure RefConvR1DCircular(const A : TReal1DArray;
     M : Integer;
     const B : TReal1DArray;
     N : Integer;
     var R : TReal1DArray);
var
    I1 : Integer;
    I2 : Integer;
    J2 : Integer;
    Buf : TReal1DArray;
begin
    RefConvR1D(A, M, B, N, Buf);
    SetLength(R, M);
    APVMove(@R[0], 0, M-1, @Buf[0], 0, M-1);
    I1 := M;
    while I1<=M+N-2 do
    begin
        I2 := Min(I1+M-1, M+N-2);
        J2 := I2-I1;
        APVAdd(@R[0], 0, J2, @Buf[0], I1, I2);
        I1 := I1+M;
    end;
end;


(*************************************************************************
Silent unit test
*************************************************************************)
function testcorrunit_test_silent():Boolean;
begin
    Result := TestCorr(True);
end;


(*************************************************************************
Unit test
*************************************************************************)
function testcorrunit_test():Boolean;
begin
    Result := TestCorr(False);
end;


end.