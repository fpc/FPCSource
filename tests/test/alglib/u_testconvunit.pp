unit u_testconvunit;
interface
uses Math, Sysutils, u_ap, u_ftbase, u_fft, u_conv;

function TestConv(Silent : Boolean):Boolean;
function testconvunit_test_silent():Boolean;
function testconvunit_test():Boolean;

implementation

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
function TestConv(Silent : Boolean):Boolean;
var
    M : Integer;
    N : Integer;
    I : Integer;
    RKind : Integer;
    CircKind : Integer;
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
    // Automatic ConvC1D() and different algorithms of ConvC1DX() are tested.
    //
    RefErr := 0;
    RefRErr := 0;
    M:=1;
    while M<=MaxN do
    begin
        N:=1;
        while N<=MaxN do
        begin
            CircKind:=0;
            while CircKind<=1 do
            begin
                RKind:=-3;
                while RKind<=1 do
                begin
                    
                    //
                    // skip impossible combinations of parameters:
                    // * circular convolution, M<N, RKind<>-3 - internal subroutine does not support M<N.
                    //
                    if (CircKind<>0) and (M<N) and (RKind<>-3) then
                    begin
                        Inc(RKind);
                        Continue;
                    end;
                    
                    //
                    // Complex convolution
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
                    if RKind=-3 then
                    begin
                        
                        //
                        // test wrapper subroutine:
                        // * circular/non-circular
                        //
                        if CircKind=0 then
                        begin
                            ConvC1D(CA, M, CB, N, CR1);
                        end
                        else
                        begin
                            ConvC1DCircular(CA, M, CB, N, CR1);
                        end;
                    end
                    else
                    begin
                        
                        //
                        // test internal subroutine
                        //
                        if M>=N then
                        begin
                            
                            //
                            // test internal subroutine:
                            // * circular/non-circular mode
                            //
                            ConvC1DX(CA, M, CB, N, CircKind<>0, RKind, 0, CR1);
                        end
                        else
                        begin
                            
                            //
                            // test internal subroutine - circular mode only
                            //
                            Assert(CircKind=0, 'Convolution test: internal error!');
                            ConvC1DX(CB, N, CA, M, False, RKind, 0, CR1);
                        end;
                    end;
                    if CircKind=0 then
                    begin
                        RefConvC1D(CA, M, CB, N, CR2);
                    end
                    else
                    begin
                        RefConvC1DCircular(CA, M, CB, N, CR2);
                    end;
                    if CircKind=0 then
                    begin
                        I:=0;
                        while I<=M+N-2 do
                        begin
                            RefErr := Max(RefErr, AbsComplex(C_Sub(CR1[I],CR2[I])));
                            Inc(I);
                        end;
                    end
                    else
                    begin
                        I:=0;
                        while I<=M-1 do
                        begin
                            RefErr := Max(RefErr, AbsComplex(C_Sub(CR1[I],CR2[I])));
                            Inc(I);
                        end;
                    end;
                    
                    //
                    // Real convolution
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
                    if RKind=-3 then
                    begin
                        
                        //
                        // test wrapper subroutine:
                        // * circular/non-circular
                        //
                        if CircKind=0 then
                        begin
                            ConvR1D(RA, M, RB, N, RR1);
                        end
                        else
                        begin
                            ConvR1DCircular(RA, M, RB, N, RR1);
                        end;
                    end
                    else
                    begin
                        if M>=N then
                        begin
                            
                            //
                            // test internal subroutine:
                            // * circular/non-circular mode
                            //
                            ConvR1DX(RA, M, RB, N, CircKind<>0, RKind, 0, RR1);
                        end
                        else
                        begin
                            
                            //
                            // test internal subroutine - non-circular mode only
                            //
                            ConvR1DX(RB, N, RA, M, CircKind<>0, RKind, 0, RR1);
                        end;
                    end;
                    if CircKind=0 then
                    begin
                        RefConvR1D(RA, M, RB, N, RR2);
                    end
                    else
                    begin
                        RefConvR1DCircular(RA, M, RB, N, RR2);
                    end;
                    if CircKind=0 then
                    begin
                        I:=0;
                        while I<=M+N-2 do
                        begin
                            RefRErr := Max(RefRErr, AbsReal(RR1[I]-RR2[I]));
                            Inc(I);
                        end;
                    end
                    else
                    begin
                        I:=0;
                        while I<=M-1 do
                        begin
                            RefRErr := Max(RefRErr, AbsReal(RR1[I]-RR2[I]));
                            Inc(I);
                        end;
                    end;
                    Inc(RKind);
                end;
                Inc(CircKind);
            end;
            Inc(N);
        end;
        Inc(M);
    end;
    RefErrors := RefErrors or AP_FP_Greater(RefErr,ErrTol);
    RefRErrors := RefRErrors or AP_FP_Greater(RefRErr,ErrTol);
    
    //
    // Test inverse convolution
    //
    InvErr := 0;
    InvRErr := 0;
    M:=1;
    while M<=MaxN do
    begin
        N:=1;
        while N<=MaxN do
        begin
            
            //
            // Complex circilar and non-circular
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
            SetLength(CR2, 1);
            ConvC1D(CA, M, CB, N, CR2);
            ConvC1DInv(CR2, M+N-1, CB, N, CR1);
            I:=0;
            while I<=M-1 do
            begin
                InvErr := Max(InvErr, AbsComplex(C_Sub(CR1[I],CA[I])));
                Inc(I);
            end;
            SetLength(CR1, 1);
            SetLength(CR2, 1);
            ConvC1DCircular(CA, M, CB, N, CR2);
            ConvC1DCircularInv(CR2, M, CB, N, CR1);
            I:=0;
            while I<=M-1 do
            begin
                InvErr := Max(InvErr, AbsComplex(C_Sub(CR1[I],CA[I])));
                Inc(I);
            end;
            
            //
            // Real circilar and non-circular
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
            SetLength(RR2, 1);
            ConvR1D(RA, M, RB, N, RR2);
            ConvR1DInv(RR2, M+N-1, RB, N, RR1);
            I:=0;
            while I<=M-1 do
            begin
                InvRErr := Max(InvRErr, AbsReal(RR1[I]-RA[I]));
                Inc(I);
            end;
            SetLength(RR1, 1);
            SetLength(RR2, 1);
            ConvR1DCircular(RA, M, RB, N, RR2);
            ConvR1DCircularInv(RR2, M, RB, N, RR1);
            I:=0;
            while I<=M-1 do
            begin
                InvRErr := Max(InvRErr, AbsReal(RR1[I]-RA[I]));
                Inc(I);
            end;
            Inc(N);
        end;
        Inc(M);
    end;
    InvErrors := InvErrors or AP_FP_Greater(InvErr,ErrTol);
    InvRErrors := InvRErrors or AP_FP_Greater(InvRErr,ErrTol);
    
    //
    // end
    //
    WasErrors := RefErrors or RefRErrors or InvErrors or InvRErrors;
    if  not Silent then
    begin
        Write(Format('TESTING CONVOLUTION'#13#10'',[]));
        Write(Format('FINAL RESULT:                             ',[]));
        if WasErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* AGAINST REFERENCE COMPLEX CONV:         ',[]));
        if RefErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* AGAINST REFERENCE REAL CONV:            ',[]));
        if RefRErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* COMPLEX INVERSE:                        ',[]));
        if InvErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* REAL INVERSE:                           ',[]));
        if InvRErrors then
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
function testconvunit_test_silent():Boolean;
begin
    Result := TestConv(True);
end;


(*************************************************************************
Unit test
*************************************************************************)
function testconvunit_test():Boolean;
begin
    Result := TestConv(False);
end;


end.