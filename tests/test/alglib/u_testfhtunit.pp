unit u_testfhtunit;
interface
uses Math, Sysutils, u_ap, u_ftbase, u_fft, u_fht;

function TestFHT(Silent : Boolean):Boolean;
function testfhtunit_test_silent():Boolean;
function testfhtunit_test():Boolean;

implementation

procedure RefFHTR1D(var A : TReal1DArray; N : Integer);forward;
procedure RefFHTR1DInv(var A : TReal1DArray; N : Integer);forward;


(*************************************************************************
Test
*************************************************************************)
function TestFHT(Silent : Boolean):Boolean;
var
    N : Integer;
    I : Integer;
    R1 : TReal1DArray;
    R2 : TReal1DArray;
    R3 : TReal1DArray;
    MaxN : Integer;
    BiDiErr : Double;
    RefErr : Double;
    ErrTol : Double;
    RefErrors : Boolean;
    BiDiErrors : Boolean;
    WasErrors : Boolean;
begin
    MaxN := 128;
    ErrTol := 100000*Power(MaxN, AP_Double(3)/2)*MachineEpsilon;
    BiDiErrors := False;
    RefErrors := False;
    WasErrors := False;
    
    //
    // Test bi-directional error: norm(x-invFHT(FHT(x)))
    //
    BiDiErr := 0;
    N:=1;
    while N<=MaxN do
    begin
        
        //
        // FHT/invFHT
        //
        SetLength(R1, N);
        SetLength(R2, N);
        SetLength(R3, N);
        I:=0;
        while I<=N-1 do
        begin
            R1[I] := 2*RandomReal-1;
            R2[I] := R1[I];
            R3[I] := R1[I];
            Inc(I);
        end;
        FHTR1D(R2, N);
        FHTR1DInv(R2, N);
        FHTR1DInv(R3, N);
        FHTR1D(R3, N);
        I:=0;
        while I<=N-1 do
        begin
            BiDiErr := Max(BiDiErr, AbsReal(R1[I]-R2[I]));
            BiDiErr := Max(BiDiErr, AbsReal(R1[I]-R3[I]));
            Inc(I);
        end;
        Inc(N);
    end;
    BiDiErrors := BiDiErrors or AP_FP_Greater(BiDiErr,ErrTol);
    
    //
    // Test against reference O(N^2) implementation
    //
    RefErr := 0;
    N:=1;
    while N<=MaxN do
    begin
        
        //
        // FHT
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
        FHTR1D(R1, N);
        RefFHTR1D(R2, N);
        I:=0;
        while I<=N-1 do
        begin
            RefErr := Max(RefErr, AbsReal(R1[I]-R2[I]));
            Inc(I);
        end;
        
        //
        // inverse FHT
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
        FHTR1DInv(R1, N);
        RefFHTR1DInv(R2, N);
        I:=0;
        while I<=N-1 do
        begin
            RefErr := Max(RefErr, AbsReal(R1[I]-R2[I]));
            Inc(I);
        end;
        Inc(N);
    end;
    RefErrors := RefErrors or AP_FP_Greater(RefErr,ErrTol);
    
    //
    // end
    //
    WasErrors := BiDiErrors or RefErrors;
    if  not Silent then
    begin
        Write(Format('TESTING FHT'#13#10'',[]));
        Write(Format('FINAL RESULT:                             ',[]));
        if WasErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* BI-DIRECTIONAL TEST:                    ',[]));
        if BiDiErrors then
        begin
            Write(Format('FAILED'#13#10'',[]));
        end
        else
        begin
            Write(Format('OK'#13#10'',[]));
        end;
        Write(Format('* AGAINST REFERENCE FHT:                  ',[]));
        if RefErrors then
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
Reference FHT
*************************************************************************)
procedure RefFHTR1D(var A : TReal1DArray; N : Integer);
var
    Buf : TReal1DArray;
    I : Integer;
    J : Integer;
    V : Double;
begin
    Assert(N>0, 'RefFHTR1D: incorrect N!');
    SetLength(Buf, N);
    I:=0;
    while I<=N-1 do
    begin
        V := 0;
        J:=0;
        while J<=N-1 do
        begin
            V := V+A[J]*(Cos(2*Pi*I*J/N)+Sin(2*Pi*I*J/N));
            Inc(J);
        end;
        Buf[I] := V;
        Inc(I);
    end;
    I:=0;
    while I<=N-1 do
    begin
        A[I] := Buf[I];
        Inc(I);
    end;
end;


(*************************************************************************
Reference inverse FHT
*************************************************************************)
procedure RefFHTR1DInv(var A : TReal1DArray; N : Integer);
var
    I : Integer;
begin
    Assert(N>0, 'RefFHTR1DInv: incorrect N!');
    RefFHTR1D(A, N);
    I:=0;
    while I<=N-1 do
    begin
        A[I] := A[I]/N;
        Inc(I);
    end;
end;


(*************************************************************************
Silent unit test
*************************************************************************)
function testfhtunit_test_silent():Boolean;
begin
    Result := TestFHT(True);
end;


(*************************************************************************
Unit test
*************************************************************************)
function testfhtunit_test():Boolean;
begin
    Result := TestFHT(False);
end;


end.