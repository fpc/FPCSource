{ %CPU=x86_64 }
program vectorcall_hva_test1;

{$IFNDEF CPUX86_64}
  {$FATAL This test program can only be compiled on Windows or Linux 64-bit with an Intel processor }
{$ENDIF}

{$ASMMODE Intel}
{$PUSH}
{$CODEALIGN RECORDMIN=16}
{$PACKRECORDS C}
type
  TM128 = record
    case Byte of
      0: (M128_F32: array[0..3] of Single);
      1: (M128_F64: array[0..1] of Double);
  end;
{$POP}

{ HFA test: field style. }

{ NOTE: if the record falls on a 16-byte boundary, the 4-component entries will
  turned into vectors rather than HFAs. }

  THFA1_SF = packed record
    F1: Single;
  end;

{$IFDEF WIN64}
  THFA2_SF = packed record
    F1, F2: Single;
  end;

  THFA3_SF = packed record
    F1, F2, F3: Single;
  end;

  THFA4_SF = packed record
    F1, F2, F3, F4: Single;
  end;
{$ENDIF}

  THFA1_DF = packed record
    F1: Double;
  end;

{$IFDEF WIN64}
  THFA2_DF = packed record
    F1, F2: Double;
  end;

  THFA3_DF = packed record
    F1, F2, F3: Double;
  end;

  THFA4_DF = packed record
    F1, F2, F3, F4: Double;
  end;
{$ENDIF}

{ HFA test - array style }

{ NOTE: if the record falls on a 16-byte boundary, the 4-component entries will
  turned into vectors rather than HFAs. }

  THFA1_SA = packed record
    F: array[0..0] of Single;
  end;

{$IFDEF WIN64}
  THFA2_SA = packed record
    F: array[0..1] of Single;
  end;

  THFA3_SA = packed record
    F: array[0..2] of Single;
  end;

  THFA4_SA = packed record
    F: array[0..3] of Single;
  end;
{$ENDIF}

  THFA1_DA = packed record
    F: array[0..0] of Double;
  end;

{$IFDEF WIN64}
  THFA2_DA = packed record
    F: array[0..1] of Double;
  end;

  THFA3_DA = packed record
    F: array[0..2] of Double;
  end;

  THFA4_DA = packed record
    F: array[0..3] of Double;
  end;
{$ENDIF}

{ Single-type vector }

function HorizontalAddSingle(V: TM128): Single; vectorcall;
begin
  HorizontalAddSingle := V.M128_F32[0] + V.M128_F32[1] + V.M128_F32[2] + V.M128_F32[3];
end;

function HorizontalAddSingle_ASM(V: TM128): Single; vectorcall; assembler; nostackframe;
asm
  HADDPS XMM0, XMM0
  HADDPS XMM0, XMM0
end;

{ Double-type vector }

function HorizontalAddDouble(V: TM128): Double; vectorcall;
begin
  HorizontalAddDouble := V.M128_F64[0] + V.M128_F64[1];
end;

function HorizontalAddDouble_ASM(V: TM128): Double; vectorcall; assembler; nostackframe;
asm
  HADDPD XMM0, XMM0
end;

{ 3-element aggregate }

function AddSingles1F(HFA: THFA1_SF): Single; vectorcall;
begin
  AddSingles1F := HFA.F1;
end;

function AddSingles1F_ASM(HFA: THFA1_SF): Single; vectorcall; assembler; nostackframe;
asm
  { Do absolutely nothing! }
end;

function AddDoubles1F(HFA: THFA1_DF): Double; vectorcall;
begin
  AddDoubles1F := HFA.F1;
end;

function AddDoubles1F_ASM(HFA: THFA1_DF): Double; vectorcall; assembler; nostackframe;
asm
  { Do absolutely nothing! }
end;

function AddSingles1A(HFA: THFA1_SA): Single; vectorcall;
begin
  AddSingles1A := HFA.F[0];
end;

function AddSingles1A_ASM(HFA: THFA1_SA): Single; vectorcall; assembler; nostackframe;
asm
  { Do absolutely nothing! }
end;

function AddDoubles1A(HFA: THFA1_DA): Double; vectorcall;
begin
  AddDoubles1A := HFA.F[0];
end;

function AddDoubles1A_ASM(HFA: THFA1_DA): Double; vectorcall; assembler; nostackframe;
asm
  { Do absolutely nothing! }
end;

{$IFDEF WIN64}
{ 2-element aggregate }

function AddSingles2F(HFA: THFA2_SF): Single; vectorcall;
begin
  AddSingles2F := HFA.F1 + HFA.F2;
end;

function AddSingles2F_ASM(HFA: THFA2_SF): Single; vectorcall; assembler; nostackframe;
asm
  ADDSS XMM0, XMM1
end;

function AddDoubles2F(HFA: THFA2_DF): Double; vectorcall;
begin
  AddDoubles2F := HFA.F1 + HFA.F2;
end;

function AddDoubles2F_ASM(HFA: THFA2_DF): Double; vectorcall; assembler; nostackframe;
asm
  ADDSD XMM0, XMM1
end;

function AddSingles2A(HFA: THFA2_SA): Single; vectorcall;
begin
  AddSingles2A := HFA.F[0] + HFA.F[1];
end;

function AddSingles2A_ASM(HFA: THFA2_SA): Single; vectorcall; assembler; nostackframe;
asm
  ADDSS XMM0, XMM1
end;

function AddDoubles2A(HFA: THFA2_DA): Double; vectorcall;
begin
  AddDoubles2A := HFA.F[0] + HFA.F[1];
end;

function AddDoubles2A_ASM(HFA: THFA2_DA): Double; vectorcall; assembler; nostackframe;
asm
  ADDSD XMM0, XMM1
end;

{ 3-element aggregate }

function AddSingles3F(HFA: THFA3_SF): Single; vectorcall;
begin
  AddSingles3F := HFA.F1 + HFA.F2 + HFA.F3;
end;

function AddSingles3F_ASM(HFA: THFA3_SF): Single; vectorcall; assembler; nostackframe;
asm
  ADDSS XMM0, XMM1
  ADDSS XMM0, XMM2
end;

function AddDoubles3F(HFA: THFA3_DF): Double; vectorcall;
begin
  AddDoubles3F := HFA.F1 + HFA.F2 + HFA.F3;
end;

function AddDoubles3F_ASM(HFA: THFA3_DF): Double; vectorcall; assembler; nostackframe;
asm
  ADDSD XMM0, XMM1
  ADDSD XMM0, XMM2
end;

function AddSingles3A(HFA: THFA3_SA): Single; vectorcall;
begin
  AddSingles3A := HFA.F[0] + HFA.F[1] + HFA.F[2];
end;

function AddSingles3A_ASM(HFA: THFA3_SA): Single; vectorcall; assembler; nostackframe;
asm
  ADDSS XMM0, XMM1
  ADDSS XMM0, XMM2
end;

function AddDoubles3A(HFA: THFA3_DA): Double; vectorcall;
begin
  AddDoubles3A := HFA.F[0] + HFA.F[1] + HFA.F[2];
end;

function AddDoubles3A_ASM(HFA: THFA3_DA): Double; vectorcall; assembler; nostackframe;
asm
  ADDSD XMM0, XMM1
  ADDSD XMM0, XMM2
end;

{ 4-element aggregate }

function AddSingles4F(HFA: THFA4_SF): Single; vectorcall;
begin
  AddSingles4F := HFA.F1 + HFA.F2 + HFA.F3 + HFA.F4;
end;

function AddSingles4F_ASM(HFA: THFA4_SF): Single; vectorcall; assembler; nostackframe;
asm
  ADDSS XMM0, XMM1
  ADDSS XMM0, XMM2
  ADDSS XMM0, XMM3
end;

function AddDoubles4F(HFA: THFA4_DF): Double; vectorcall;
begin
  AddDoubles4F := HFA.F1 + HFA.F2 + HFA.F3 + HFA.F4;
end;

function AddDoubles4F_ASM(HFA: THFA4_DF): Double; vectorcall; assembler; nostackframe;
asm
  ADDSD XMM0, XMM1
  ADDSD XMM0, XMM2
  ADDSD XMM0, XMM3
end;

function AddSingles4A(HFA: THFA4_SA): Single; vectorcall;
begin
  AddSingles4A := HFA.F[0] + HFA.F[1] + HFA.F[2] + HFA.F[3];
end;

function AddSingles4A_ASM(HFA: THFA4_SA): Single; vectorcall; assembler; nostackframe;
asm
  ADDSS XMM0, XMM1
  ADDSS XMM0, XMM2
  ADDSS XMM0, XMM3
end;

function AddDoubles4A(HFA: THFA4_DA): Double; vectorcall;
begin
  AddDoubles4A := HFA.F[0] + HFA.F[1] + HFA.F[2] + HFA.F[3];
end;

function AddDoubles4A_ASM(HFA: THFA4_DA): Double; vectorcall; assembler; nostackframe;
asm
  ADDSD XMM0, XMM1
  ADDSD XMM0, XMM2
  ADDSD XMM0, XMM3
end;
{$ENDIF}

var
  HVA: TM128;
  HFA1_SF: THFA1_SF;
  HFA1_DF: THFA1_DF;
  HFA1_SA: THFA1_SA;
  HFA1_DA: THFA1_DA;
{$IFDEF WIN64}
  HFA2_SF: THFA2_SF;
  HFA2_DF: THFA2_DF;
  HFA2_SA: THFA2_SA;
  HFA2_DA: THFA2_DA;
  HFA3_SF: THFA3_SF;
  HFA3_DF: THFA3_DF;
  HFA3_SA: THFA3_SA;
  HFA3_DA: THFA3_DA;
  HFA4_SF: THFA4_SF;
  HFA4_DF: THFA4_DF;
  HFA4_SA: THFA4_SA;
  HFA4_DA: THFA4_DA;
{$ENDIF}
  TestPointer: PtrUInt;
  I, J: Integer;
  ResS, ResSA: Single;
  ResD, ResDA: Double;
  Addresses: array[0..3] of Pointer;
  FieldAddresses: array[0..3, 0..3] of Pointer;
const
  AddressNames1: array[0..3] of ShortString = ('HFA1_SF', 'HFA1_DF', 'HFA1_SA', 'HFA1_DA');
{$IFDEF WIN64}
  AddressNames2: array[0..3] of ShortString = ('HFA2_SF', 'HFA2_DF', 'HFA2_SA', 'HFA2_DA');
  AddressNames3: array[0..3] of ShortString = ('HFA3_SF', 'HFA3_DF', 'HFA3_SA', 'HFA3_DA');
  AddressNames4: array[0..3] of ShortString = ('HFA4_SF', 'HFA4_DF', 'HFA4_SA', 'HFA4_DA');
{$ENDIF}
  FieldAddressNames: array[0..3] of ShortString = ('F1', 'F2', 'F3', 'F4');

  ExpS1: Single = 5.0;
{$IFDEF WIN64}
  ExpS2: Single = -5.0;
  ExpS3: Single = 10.0;
{$ENDIF}
  ExpS4: Single = -10.0;
  ExpD1: Double = 5.0;
  ExpD2: Double = -5.0;
{$IFDEF WIN64}
  ExpD3: Double = 10.0;
  ExpD4: Double = -10.0;
{$ENDIF}
begin

  if (PtrUInt(@HVA) and $F) <> 0 then
  begin
    WriteLn('FAIL: HVA is not correctly aligned.');
    Halt(1);
  end;

  { array of singles }
  WriteLn('- horizontal add (4 singles)');
  HVA.M128_F32[0] := 5.0;
  HVA.M128_F32[1] := -10.0;
  HVA.M128_F32[2] := 15.0;
  HVA.M128_F32[3] := -20.0;
  ResS := HorizontalAddSingle(HVA);
  ResSA := HorizontalAddSingle_ASM(HVA);
  if (ResS <> ResSA) then
  begin
    WriteLn('FAIL: HorizontalAddSingle(HVA) has the vector in the wrong register.');
    Halt(1);
  end else
  begin
    if ResS <> ExpS4 then
    begin
      WriteLn('FAIL: HorizontalAddSingle(HVA) returned ', ResS, ' instead of ', ExpS4);
      Halt(1);
    end;
  end;

  { array of doubles }
  WriteLn('- horizontal add (2 doubles)');
  HVA.M128_F64[0] := 5.0;
  HVA.M128_F64[1] := -10.0;
  ResD := HorizontalAddDouble(HVA);
  ResDA := HorizontalAddDouble_ASM(HVA);
  if (ResD <> ResDA) then
  begin
    WriteLn('FAIL: HorizontalAddDouble(HVA) has the vector in the wrong register.');
    Halt(1);
  end else
  begin
    if ResD <> ExpD2 then
    begin
      WriteLn('FAIL: HorizontalAddDouble(HVA) returned ', ResD, ' instead of ', ExpD2);
      Halt(1);
    end;
  end;

  { 1-field aggregates }
  WriteLn('- 1-field aggregates');

  Addresses[0] := @HFA1_SF;
  Addresses[1] := @HFA1_SA;
  Addresses[2] := @HFA1_DF;
  Addresses[3] := @HFA1_DA;
  FieldAddresses[0][0] := @(HFA1_SF.F1);
  FieldAddresses[1][0] := @(HFA1_SA.F[0]);
  FieldAddresses[2][0] := @(HFA1_DF.F1);
  FieldAddresses[3][0] := @(HFA1_DA.F[0]);

  { Check alignment }
  for I := 0 to 1 do
  begin
    TestPointer := PtrUInt(Addresses[I]);
    if Pointer(TestPointer) <> FieldAddresses[I][0] then
    begin
      WriteLn('FAIL: ', AddressNames1[I], ' is not correctly packed; field F1 is not in the expected place.');
      Halt(1);
    end;
  end;

  HFA1_SF.F1 := 5.0;
  ResS := AddSingles1F(HFA1_SF);
  ResSA := AddSingles1F_ASM(HFA1_SF);
  if (ResS <> ResSA) then
  begin
    WriteLn('FAIL: AddSingles1F(', AddressNames1[I], ') is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResS <> ExpS1 then
    begin
      WriteLn('FAIL: AddSingles1F(', AddressNames1[I], ') returned ', ResS, ' instead of ', ExpS1);
      Halt(1);
    end;
  end;

  HFA1_DF.F1 := 5.0;
  ResD := AddDoubles1F(HFA1_DF);
  ResDA := AddDoubles1F_ASM(HFA1_DF);
  if (ResD <> ResDA) then
  begin
    WriteLn('FAIL: AddDoubles1F(', AddressNames1[I], ') is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResD <> ExpD1 then
    begin
      WriteLn('FAIL: AddDoubles1F(', AddressNames1[I], ') returned ', ResD, ' instead of ', ExpD1);
      Halt(1);
    end;
  end;

  HFA1_SA.F[0] := 5.0;
  ResS := AddSingles1A(HFA1_SA);
  ResSA := AddSingles1A_ASM(HFA1_SA);
  if (ResS <> ResSA) then
  begin
    WriteLn('FAIL: AddSingles1A(', AddressNames1[I], ') is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResS <> ExpS1 then
    begin
      WriteLn('FAIL: AddSingles1A(', AddressNames1[I], ') returned ', ResS, ' instead of ', ExpS1);
      Halt(1);
    end;
  end;

  HFA1_DA.F[0] := 5.0;
  ResD := AddDoubles1A(HFA1_DA);
  ResDA := AddDoubles1A_ASM(HFA1_DA);
  if (ResD <> ResDA) then
  begin
    WriteLn('FAIL: AddDoubles1A(', AddressNames1[I], ') is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResD <> ExpD1 then
    begin
      WriteLn('FAIL: AddDoubles1A(', AddressNames1[I], ') returned ', ResD, ' instead of ', ExpD1);
      Halt(1);
    end;
  end;

{$IFDEF WIN64}
  { 2-field aggregates }
  WriteLn('- 2-field aggregates');

  Addresses[0] := @HFA2_SF;
  Addresses[1] := @HFA2_SA;
  FieldAddresses[0][0] := @(HFA2_SF.F1);
  FieldAddresses[0][1] := @(HFA2_SF.F2);
  FieldAddresses[1][0] := @(HFA2_SA.F[0]);
  FieldAddresses[1][1] := @(HFA2_SA.F[1]);

  { Check alignment of Singles }
  for I := 0 to 1 do
  begin
    TestPointer := PtrUInt(Addresses[I]);
    for J := 0 to 1 do
    begin
      if Pointer(TestPointer) <> FieldAddresses[I][J] then
      begin
        WriteLn('FAIL: ', AddressNames2[I], ' is not correctly packed; field ', FieldAddressNames[J], ' is not in the expected place.');
        Halt(1);
      end;

      Inc(TestPointer, $4);
    end;
  end;

  Addresses[2] := @HFA2_DF;
  Addresses[3] := @HFA2_DA;
  FieldAddresses[2][0] := @(HFA2_DF.F1);
  FieldAddresses[2][1] := @(HFA2_DF.F2);
  FieldAddresses[3][0] := @(HFA2_DA.F[0]);
  FieldAddresses[3][1] := @(HFA2_DA.F[1]);

  { Check alignment of Doubles }
  for I := 2 to 3 do
  begin
    TestPointer := PtrUInt(Addresses[I]);
    for J := 0 to 1 do
    begin
      if Pointer(TestPointer) <> FieldAddresses[I][J] then
      begin
        WriteLn('FAIL: ', AddressNames2[I], ' is not correctly packed; field ', FieldAddressNames[J], ' is not in the expected place.');
        Halt(1);
      end;

      Inc(TestPointer, $8);
    end;
  end;

  HFA2_SF.F1 := 5.0;
  HFA2_SF.F2 := -10.0;
  ResS := AddSingles2F(HFA2_SF);
  ResSA := AddSingles2F_ASM(HFA2_SF);
  if (ResS <> ResSA) then
  begin
    WriteLn('FAIL: AddSingles2F(HFA2_SF) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResS <> ExpS2 then
    begin
      WriteLn('FAIL: AddSingles2F(HFA2_SF) returned ', ResS, ' instead of ', ExpS2);
      Halt(1);
    end;
  end;

  HFA2_DF.F1 := 5.0;
  HFA2_DF.F2 := -10.0;
  ResD := AddDoubles2F(HFA2_DF);
  ResDA := AddDoubles2F_ASM(HFA2_DF);
  if (ResD <> ResDA) then
  begin
    WriteLn('FAIL: AddDoubles2F(HFA2_DF) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResD <> ExpD2 then
    begin
      WriteLn('FAIL: AddDoubles2F(HFA2_DF) returned ', ResD, ' instead of ', ExpD2);
      Halt(1);
    end;
  end;

  HFA2_SA.F[0] := 5.0;
  HFA2_SA.F[1] := -10.0;
  ResS := AddSingles2A(HFA2_SA);
  ResSA := AddSingles2A_ASM(HFA2_SA);
  if (ResS <> ResSA) then
  begin
    WriteLn('FAIL: AddSingles2A(HFA2_SA) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResS <> ExpS2 then
    begin
      WriteLn('FAIL: AddSingles2A(HFA2_SA) returned ', ResS, ' instead of ', ExpS2);
      Halt(1);
    end;
  end;

  HFA2_DA.F[0] := 5.0;
  HFA2_DA.F[1] := -10.0;
  ResD := AddDoubles2A(HFA2_DA);
  ResDA := AddDoubles2A_ASM(HFA2_DA);
  if (ResD <> ResDA) then
  begin
    WriteLn('FAIL: AddDoubles2A(HFA2_DA) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResD <> ExpD2 then
    begin
      WriteLn('FAIL: AddDoubles2A(HFA2_DA) returned ', ResD, ' instead of ', ExpD2);
      Halt(1);
    end;
  end;

  { 3-field aggregates }
  WriteLn('- 3-field aggregates');

  Addresses[0] := @HFA3_SF;
  Addresses[1] := @HFA3_SA;
  FieldAddresses[0][0] := @(HFA3_SF.F1);
  FieldAddresses[0][1] := @(HFA3_SF.F2);
  FieldAddresses[0][2] := @(HFA3_SF.F3);
  FieldAddresses[1][0] := @(HFA3_SA.F[0]);
  FieldAddresses[1][1] := @(HFA3_SA.F[1]);
  FieldAddresses[1][2] := @(HFA3_SA.F[2]);

  { Check alignment of Singles }
  for I := 0 to 1 do
  begin
    TestPointer := PtrUInt(Addresses[I]);
    for J := 0 to 2 do
    begin
      if Pointer(TestPointer) <> FieldAddresses[I][J] then
      begin
        WriteLn('FAIL: ', AddressNames3[I], ' is not correctly packed; field ', FieldAddressNames[J], ' is not in the expected place.');
        Halt(1);
      end;

      Inc(TestPointer, $4);
    end;
  end;

  Addresses[2] := @HFA3_DF;
  Addresses[3] := @HFA3_DA;
  FieldAddresses[2][0] := @(HFA3_DF.F1);
  FieldAddresses[2][1] := @(HFA3_DF.F2);
  FieldAddresses[2][2] := @(HFA3_DF.F3);
  FieldAddresses[3][0] := @(HFA3_DA.F[0]);
  FieldAddresses[3][1] := @(HFA3_DA.F[1]);
  FieldAddresses[3][2] := @(HFA3_DA.F[2]);

  { Check alignment of Doubles }
  for I := 2 to 3 do
  begin
    TestPointer := PtrUInt(Addresses[I]);
    for J := 0 to 2 do
    begin
      if Pointer(TestPointer) <> FieldAddresses[I][J] then
      begin
        WriteLn('FAIL: ', AddressNames3[I], ' is not correctly packed; field ', FieldAddressNames[J], ' is not in the expected place.');
        Halt(1);
      end;

      Inc(TestPointer, $8);
    end;
  end;

  HFA3_SF.F1 := 5.0;
  HFA3_SF.F2 := -10.0;
  HFA3_SF.F3 := 15.0;
  ResS := AddSingles3F(HFA3_SF);
  ResSA := AddSingles3F_ASM(HFA3_SF);
  if (ResS <> ResSA) then
  begin
    WriteLn('FAIL: AddSingles3F(HFA3_SF) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResS <> ExpS3 then
    begin
      WriteLn('FAIL: AddSingles3F(HFA3_SF) returned ', ResS, ' instead of ', ExpS3);
      Halt(1);
    end;
  end;

  HFA3_DF.F1 := 5.0;
  HFA3_DF.F2 := -10.0;
  HFA3_DF.F3 := 15.0;
  ResD := AddDoubles3F(HFA3_DF);
  ResDA := AddDoubles3F_ASM(HFA3_DF);
  if (ResD <> ResDA) then
  begin
    WriteLn('FAIL: AddDoubles3F(HFA3_DF) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResD <> ExpD3 then
    begin
      WriteLn('FAIL: AddDoubles3F(HFA3_DF) returned ', ResD, ' instead of ', ExpD3);
      Halt(1);
    end;
  end;

  HFA3_SA.F[0] := 5.0;
  HFA3_SA.F[1] := -10.0;
  HFA3_SA.F[2] := 15.0;
  ResS := AddSingles3A(HFA3_SA);
  ResSA := AddSingles3A_ASM(HFA3_SA);
  if (ResS <> ResSA) then
  begin
    WriteLn('FAIL: AddSingles3A(HFA3_SA) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResS <> ExpS3 then
    begin
      WriteLn('FAIL: AddSingles3A(HFA3_SA) returned ', ResS, ' instead of ', ExpS3);
      Halt(1);
    end;
  end;

  HFA3_DA.F[0] := 5.0;
  HFA3_DA.F[1] := -10.0;
  HFA3_DA.F[2] := 15.0;
  ResD := AddDoubles3A(HFA3_DA);
  ResDA := AddDoubles3A_ASM(HFA3_DA);
  if (ResD <> ResDA) then
  begin
    WriteLn('FAIL: AddDoubles3A(HFA3_DA) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResD <> ExpD3 then
    begin
      WriteLn('FAIL: AddDoubles3A(HFA3_DA) returned ', ResD, ' instead of ', ExpD3);
      Halt(1);
    end;
  end;

  { 4-field aggregates }
  WriteLn('- 4-field aggregates');

  Addresses[0] := @HFA4_SF;
  Addresses[1] := @HFA4_SA;
  FieldAddresses[0][0] := @(HFA4_SF.F1);
  FieldAddresses[0][1] := @(HFA4_SF.F2);
  FieldAddresses[0][2] := @(HFA4_SF.F3);
  FieldAddresses[0][3] := @(HFA4_SF.F4);
  FieldAddresses[1][0] := @(HFA4_SA.F[0]);
  FieldAddresses[1][1] := @(HFA4_SA.F[1]);
  FieldAddresses[1][2] := @(HFA4_SA.F[2]);
  FieldAddresses[1][3] := @(HFA4_SA.F[3]);

  { Check alignment of Singles }
  for I := 0 to 1 do
  begin
    TestPointer := PtrUInt(Addresses[I]);
    for J := 0 to 3 do
    begin
      if Pointer(TestPointer) <> FieldAddresses[I][J] then
      begin
        WriteLn('FAIL: ', AddressNames4[I], ' is not correctly packed; field ', FieldAddressNames[J], ' is not in the expected place.');
        Halt(1);
      end;

      Inc(TestPointer, $4);
    end;
  end;

  Addresses[2] := @HFA4_DF;
  Addresses[3] := @HFA4_DA;
  FieldAddresses[2][0] := @(HFA4_DF.F1);
  FieldAddresses[2][1] := @(HFA4_DF.F2);
  FieldAddresses[2][2] := @(HFA4_DF.F3);
  FieldAddresses[2][3] := @(HFA4_DF.F4);
  FieldAddresses[3][0] := @(HFA4_DA.F[0]);
  FieldAddresses[3][1] := @(HFA4_DA.F[1]);
  FieldAddresses[3][2] := @(HFA4_DA.F[2]);
  FieldAddresses[3][3] := @(HFA4_DA.F[3]);

  { Check alignment of Doubles }
  for I := 2 to 3 do
  begin
    TestPointer := PtrUInt(Addresses[I]);
    for J := 0 to 3 do
    begin
      if Pointer(TestPointer) <> FieldAddresses[I][J] then
      begin
        WriteLn('FAIL: ', AddressNames4[I], ' is not correctly packed; field ', FieldAddressNames[J], ' is not in the expected place.');
        Halt(1);
      end;

      Inc(TestPointer, $8);
    end;
  end;

  HFA4_SF.F1 := 5.0;
  HFA4_SF.F2 := -10.0;
  HFA4_SF.F3 := 15.0;
  HFA4_SF.F4 := -20.0;
  ResS := AddSingles4F(HFA4_SF);
  ResSA := AddSingles4F_ASM(HFA4_SF);
  if (ResS <> ResSA) then
  begin
    WriteLn('FAIL: AddSingles4F(HFA4_SF) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResS <> ExpS4 then
    begin
      WriteLn('FAIL: AddSingles4F(HFA4_SF) returned ', ResS, ' instead of ', ExpS4);
      Halt(1);
    end;
  end;

  HFA4_DF.F1 := 5.0;
  HFA4_DF.F2 := -10.0;
  HFA4_DF.F3 := 15.0;
  HFA4_DF.F4 := -20.0;
  ResD := AddDoubles4F(HFA4_DF);
  ResDA := AddDoubles4F_ASM(HFA4_DF);
  if (ResD <> ResDA) then
  begin
    WriteLn('FAIL: AddDoubles4F(HFA4_DF) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResD <> ExpD4 then
    begin
      WriteLn('FAIL: AddDoubles4F(HFA4_DF) returned ', ResD, ' instead of ', ExpD4);
      Halt(1);
    end;
  end;

  HFA4_SA.F[0] := 5.0;
  HFA4_SA.F[1] := -10.0;
  HFA4_SA.F[2] := 15.0;
  HFA4_SA.F[3] := -20.0;
  ResS := AddSingles4A(HFA4_SA);
  ResSA := AddSingles4A_ASM(HFA4_SA);
  if (ResS <> ResSA) then
  begin
    WriteLn('FAIL: AddSingles4A(HFA4_SA) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResS <> ExpS4 then
    begin
      WriteLn('FAIL: AddSingles4A(HFA4_SA) returned ', ResS, ' instead of ', ExpS4);
      Halt(1);
    end;
  end;

  HFA4_DA.F[0] := 5.0;
  HFA4_DA.F[1] := -10.0;
  HFA4_DA.F[2] := 15.0;
  HFA4_DA.F[3] := -20.0;
  ResD := AddDoubles4A(HFA4_DA);
  ResDA := AddDoubles4A_ASM(HFA4_DA);
  if (ResD <> ResDA) then
  begin
    WriteLn('FAIL: AddDoubles4A(HFA4_DF) is not passing the aggregate correctly.');
    Halt(1);
  end else
  begin
    if ResD <> ExpD4 then
    begin
      WriteLn('FAIL: AddDoubles4A(HFA4_DF) returned ', ResD, ' instead of ', ExpD4);
      Halt(1);
    end;
  end;
{$ENDIF}
  WriteLn('ok');
end.

