{ %CPU=x86_64 }
program vectorcall_stack_test;

{$IFNDEF CPUX86_64}
  {$FATAL This test program can only be compiled on Windows or Linux 64-bit with an Intel processor }
{$ENDIF}

{ This program can be compiled on Linux, and all the vectorcall
  routines should work the same, including the assembler routine.
  'vectorcall' should be ignored by the compiler on this platform. }

{$push}
{$CODEALIGN RECORDMIN=16}
{$PACKRECORDS C}
type
  TM128 = record
    case Byte of
      0: (M128_F32: array[0..3] of Single);
      1: (M128_F64: array[0..1] of Double);
  end;

{$CODEALIGN RECORDMIN=32}
{$PACKRECORDS C}
type
  TM256 = record
    case Byte of
      0: (M256_F32: array[0..7] of Single);
      1: (M256_F64: array[0..3] of Double);
      2: (M256_M128: array[0..1] of TM128);
  end;
{$pop}

  TVector4f = record
    case Byte of
      0: (M128: TM128);
      1: (X, Y, Z, W: Single);
  end;

  TVectorPair4f = record
    case Byte of
      0: (M256: TM256);
      1: (V: array[0..1] of TVector4f);
      2: (X1, Y1, Z1, W1, X2, Y2, Z2, W2: Single);
  end;

function TestFloat(TP: Single): Single; vectorcall; { vectorcall should have no effect on how this function behaves }
begin
  TestFloat := TP * 1.5;
end;

function AddVectors(V1, V2: TVector4f): TVector4f; vectorcall;
begin
  AddVectors.X := V1.X + V2.X;
  AddVectors.Y := V1.Y + V2.Y;
  AddVectors.Z := V1.Z + V2.Z;
  AddVectors.W := V1.W + V2.W;
end;

{$ASMMODE Intel}
function AddVectorsAsm(V1, V2: TVector4f): TVector4f; vectorcall; assembler; nostackframe; inline; { The inline is for a future test }
asm
  ADDPS XMM0, XMM1
end;

{ Note: V1, V2 and the result will go on the stack until FPC fully supports 256-bit vectors }
function AddVectors(V1, V2: TVectorPair4f): TVectorPair4f; vectorcall;
var
  C: Integer;
begin
  for C := 0 to 1 do
  begin
    AddVectors.V[C].X := V1.V[C].X + V2.V[C].X;
    AddVectors.V[C].Y := V1.V[C].Y + V2.V[C].Y;
    AddVectors.V[C].Z := V1.V[C].Z + V2.V[C].Z;
    AddVectors.V[C].W := V1.V[C].W + V2.V[C].W;
  end;
end;

var
  Vecs: array[0..1] of TVector4f; Res, ResAsm, Exp: TVector4f;
  Pairs: array[0..1] of TVectorPair4f; ResPair, ExpPair: TVectorPair4f;
  I: Integer;
begin
  FillDWord(Vecs[0], 0, 8);
  Vecs[0].X := TestFloat(2.0);
  Vecs[0].Y := 1.0;
  Vecs[0].Z := -4.0;
  Vecs[0].W := 1.0;

  Vecs[1].X := 0.0;
  Vecs[1].Y := -2.0;
  Vecs[1].Z := TestFloat(4.0);
  Vecs[1].W := 0.0;

  Exp.X := 3.0;
  Exp.Y := -1.0;
  Exp.Z := 2.0;
  Exp.W := 1.0;

  Pairs[0].V[0].X := 1.0;     Pairs[0].V[1].X := 5.0;
  Pairs[0].V[0].Y := 2.0;     Pairs[0].V[1].Y := 6.0;
  Pairs[0].V[0].Z := 3.0;     Pairs[0].V[1].Z := 7.0;
  Pairs[0].V[0].W := 4.0;     Pairs[0].V[1].W := 8.0;

  Pairs[1].V[0].X := 9.0;     Pairs[1].V[1].X := 13.0;
  Pairs[1].V[0].Y := 10.0;    Pairs[1].V[1].Y := 14.0;
  Pairs[1].V[0].Z := 11.0;    Pairs[1].V[1].Z := 15.0;
  Pairs[1].V[0].W := 12.0;    Pairs[1].V[1].W := 16.0;

  ExpPair.V[0].X := 10.0;     ExpPair.V[1].X := 18.0;
  ExpPair.V[0].Y := 12.0;     ExpPair.V[1].Y := 20.0;
  ExpPair.V[0].Z := 14.0;     ExpPair.V[1].Z := 22.0;
  ExpPair.V[0].W := 16.0;     ExpPair.V[1].W := 24.0;

  WriteLn('Vecs[0]  = (', Vecs[0].X, ', ', Vecs[0].Y, ', ', Vecs[0].Z, ', ', Vecs[0].W, ')');
  WriteLn('Vecs[1]  = (', Vecs[1].X, ', ', Vecs[1].Y, ', ', Vecs[1].Z, ', ', Vecs[1].W, ')');

  Res := AddVectors(Vecs[0], Vecs[1]);
  ResAsm := AddVectorsAsm(Vecs[0], Vecs[1]);

  WriteLn('Result   = (', Res.X, ', ', Res.Y, ', ', Res.Z, ', ', Res.W, ')');
  WriteLn('ResAsm   = (', ResAsm.X, ', ', ResAsm.Y, ', ', ResAsm.Z, ', ', ResAsm.W, ')');
  WriteLn('Expected = (', Exp.X, ', ', Exp.Y, ', ', Exp.Z, ', ', Exp.W, ')');

  WriteLn('Pairs[0] = (', Pairs[0].V[0].X, ', ', Pairs[0].V[0].Y, ', ', Pairs[0].V[0].Z, ', ', Pairs[0].V[0].W, ', ', Pairs[0].V[1].X, ', ', Pairs[0].V[1].Y, ', ', Pairs[0].V[1].Z, ', ', Pairs[0].V[1].W, ')');
  WriteLn('Pairs[1] = (', Pairs[1].V[0].X, ', ', Pairs[1].V[0].Y, ', ', Pairs[1].V[0].Z, ', ', Pairs[1].V[0].W, ', ', Pairs[1].V[1].X, ', ', Pairs[1].V[1].Y, ', ', Pairs[1].V[1].Z, ', ', Pairs[1].V[1].W, ')');

  ResPair := AddVectors(Pairs[0], Pairs[1]);

  WriteLn('ResPair  = (', ResPair.V[0].X, ', ', ResPair.V[0].Y, ', ', ResPair.V[0].Z, ', ', ResPair.V[0].W, ', ', ResPair.V[1].X, ', ', ResPair.V[1].Y, ', ', ResPair.V[1].Z, ', ', ResPair.V[1].W, ')');
  WriteLn('Expected = (', ExpPair.V[0].X, ', ', ExpPair.V[0].Y, ', ', ExpPair.V[0].Z, ', ', ExpPair.V[0].W, ', ', ExpPair.V[1].X, ', ', ExpPair.V[1].Y, ', ', ExpPair.V[1].Z, ', ', ExpPair.V[1].W, ')');

  for I := 0 to 3 do
  begin
    if Res.M128.M128_F32[I] <> Exp.M128.M128_F32[I] then
    begin
      WriteLn('FAILURE on Res.M128.M128_F32[', I, ']');
      Halt(1);
    end;

    if ResAsm.M128.M128_F32[I] <> Exp.M128.M128_F32[I] then
    begin
      WriteLn('FAILURE on ResAsm.M128.M128_F32[', I, ']');
      Halt(1);
    end;
  end;

  for I := 0 to 7 do
  begin
    if ResPair.M256.M256_F32[I] <> ExpPair.M256.M256_F32[I] then
    begin
      WriteLn('FAILURE on ResPair.M256.M256_F32[', I, ']');
      Halt(1);
    end;
  end;

  WriteLn('ok');
end.
