{ %CPU=x86_64 }
program vectorcall_hva_test2;

{$IFNDEF CPUX86_64}
  {$FATAL This test program can only be compiled on Windows or Linux 64-bit with an Intel processor }
{$ENDIF}

{$push}
{$CODEALIGN RECORDMIN=16}
{$PACKRECORDS C}
type
  TM128 = record
    case Byte of
      0: (M128_F32: array[0..3] of Single);
      1: (M128_F64: array[0..1] of Double);
  end;
{$pop}

{ HVA test }
  THVA = record
    V1, V2, V3, V4: TM128;
  end;

operator +(X, Y: TM128)Z: TM128; vectorcall;
  var
    I: Integer;
  begin
    for I := 0 to 3 do
      Z.M128_F32[I] := X.M128_F32[I] + Y.M128_F32[I];
  end;

operator -(X, Y: TM128)Z: TM128; vectorcall;
  var
    I: Integer;
  begin
    for I := 0 to 3 do
      Z.M128_F32[I] := X.M128_F32[I] - Y.M128_F32[I];
  end;

{ - InputHVA goes on the stack because there are not enough free XMM registers to contain the entire argument
  - A4 does NOT go on the stack and goes into an XMM register.
}
function HVATest(A1, A2, A3: TM128; InputHVA: THVA; A4: TM128; Op: Integer): THVA; vectorcall;
  begin
    { FIXME: There is an internal stack misalignment for A4, necessitating the
      use of (V)MOVDQU instead of (V)MOVDQA in the compiled code. }
    case Op of
      1:
        begin
          HVATest.V1 := InputHVA.V1 + A1;
          HVATest.V2 := InputHVA.V2 + A2;
          HVATest.V3 := InputHVA.V3 + A3;
          HVATest.V4 := InputHVA.V4 + A4;
        end;
      2:
        begin
          HVATest.V1 := InputHVA.V1 - A1;
          HVATest.V2 := InputHVA.V2 - A2;
          HVATest.V3 := InputHVA.V3 - A3;
          HVATest.V4 := InputHVA.V4 - A4;
        end;
      else
        begin
          HVATest.V1 := InputHVA.V1 + A1;
          HVATest.V2 := InputHVA.V2 - A2;
          HVATest.V3 := InputHVA.V3 + A3;
          HVATest.V4 := InputHVA.V4 - A4;
        end;
    end;
  end;

var
  B1, B2, B3, B4: TM128; HVA, AddRes, SubRes, MixRes, AddExp, SubExp, MixExp: THVA; I: Integer;
begin
  B1.M128_F32[0] := 1.0;        B1.M128_F32[1] := 2.0;        B1.M128_F32[2] := 3.0;        B1.M128_F32[3] := 4.0;
  B2.M128_F32[0] := 5.0;        B2.M128_F32[1] := 6.0;        B2.M128_F32[2] := 7.0;        B2.M128_F32[3] := 8.0;
  B3.M128_F32[0] := 9.0;        B3.M128_F32[1] := 10.0;       B3.M128_F32[2] := 11.0;       B3.M128_F32[3] := 12.0;
  B4.M128_F32[0] := 13.0;       B4.M128_F32[1] := 14.0;       B4.M128_F32[2] := 15.0;       B4.M128_F32[3] := 16.0;

  HVA.V1.M128_F32[0] := 10.0;   HVA.V1.M128_F32[1] := 20.0;   HVA.V1.M128_F32[2] := 30.0;   HVA.V1.M128_F32[3] := 40.0;
  HVA.V2.M128_F32[0] := 50.0;   HVA.V2.M128_F32[1] := 60.0;   HVA.V2.M128_F32[2] := 70.0;   HVA.V2.M128_F32[3] := 80.0;
  HVA.V3.M128_F32[0] := 90.0;   HVA.V3.M128_F32[1] := 100.0;  HVA.V3.M128_F32[2] := 110.0;  HVA.V3.M128_F32[3] := 120.0;
  HVA.V4.M128_F32[0] := 130.0;  HVA.V4.M128_F32[1] := 140.0;  HVA.V4.M128_F32[2] := 150.0;  HVA.V4.M128_F32[3] := 160.0;

  AddExp.V1.M128_F32[0] := 11.0;   AddExp.V1.M128_F32[1] := 22.0;   AddExp.V1.M128_F32[2] := 33.0;   AddExp.V1.M128_F32[3] := 44.0;
  AddExp.V2.M128_F32[0] := 55.0;   AddExp.V2.M128_F32[1] := 66.0;   AddExp.V2.M128_F32[2] := 77.0;   AddExp.V2.M128_F32[3] := 88.0;
  AddExp.V3.M128_F32[0] := 99.0;   AddExp.V3.M128_F32[1] := 110.0;  AddExp.V3.M128_F32[2] := 121.0;  AddExp.V3.M128_F32[3] := 132.0;
  AddExp.V4.M128_F32[0] := 143.0;  AddExp.V4.M128_F32[1] := 154.0;  AddExp.V4.M128_F32[2] := 165.0;  AddExp.V4.M128_F32[3] := 176.0;

  SubExp.V1.M128_F32[0] := 9.0;    SubExp.V1.M128_F32[1] := 18.0;   SubExp.V1.M128_F32[2] := 27.0;   SubExp.V1.M128_F32[3] := 36.0;
  SubExp.V2.M128_F32[0] := 45.0;   SubExp.V2.M128_F32[1] := 54.0;   SubExp.V2.M128_F32[2] := 63.0;   SubExp.V2.M128_F32[3] := 72.0;
  SubExp.V3.M128_F32[0] := 81.0;   SubExp.V3.M128_F32[1] := 90.0;   SubExp.V3.M128_F32[2] := 99.0;   SubExp.V3.M128_F32[3] := 108.0;
  SubExp.V4.M128_F32[0] := 117.0;  SubExp.V4.M128_F32[1] := 126.0;  SubExp.V4.M128_F32[2] := 135.0;  SubExp.V4.M128_F32[3] := 144.0;

  MixExp.V1.M128_F32[0] := 11.0;   MixExp.V1.M128_F32[1] := 22.0;   MixExp.V1.M128_F32[2] := 33.0;   MixExp.V1.M128_F32[3] := 44.0;
  MixExp.V2.M128_F32[0] := 45.0;   MixExp.V2.M128_F32[1] := 54.0;   MixExp.V2.M128_F32[2] := 63.0;   MixExp.V2.M128_F32[3] := 72.0;
  MixExp.V3.M128_F32[0] := 99.0;   MixExp.V3.M128_F32[1] := 110.0;  MixExp.V3.M128_F32[2] := 121.0;  MixExp.V3.M128_F32[3] := 132.0;
  MixExp.V4.M128_F32[0] := 117.0;  MixExp.V4.M128_F32[1] := 126.0;  MixExp.V4.M128_F32[2] := 135.0;  MixExp.V4.M128_F32[3] := 144.0;

  WriteLn('    B1: ', B1.M128_F32[0], ',', B1.M128_F32[1], ',', B1.M128_F32[2], ',', B1.M128_F32[3]);
  WriteLn('    B2: ', B2.M128_F32[0], ',', B2.M128_F32[1], ',', B2.M128_F32[2], ',', B2.M128_F32[3]);
  WriteLn('    B3: ', B3.M128_F32[0], ',', B3.M128_F32[1], ',', B3.M128_F32[2], ',', B3.M128_F32[3]);
  WriteLn('    B4: ', B4.M128_F32[0], ',', B4.M128_F32[1], ',', B4.M128_F32[2], ',', B4.M128_F32[3]);
  WriteLn('HVA.V1: ', HVA.V1.M128_F32[0], ',', HVA.V1.M128_F32[1], ',', HVA.V1.M128_F32[2], ',', HVA.V1.M128_F32[3]);
  WriteLn('HVA.V2: ', HVA.V2.M128_F32[0], ',', HVA.V2.M128_F32[1], ',', HVA.V2.M128_F32[2], ',', HVA.V2.M128_F32[3]);
  WriteLn('HVA.V3: ', HVA.V3.M128_F32[0], ',', HVA.V3.M128_F32[1], ',', HVA.V3.M128_F32[2], ',', HVA.V3.M128_F32[3]);
  WriteLn('HVA.V4: ', HVA.V4.M128_F32[0], ',', HVA.V4.M128_F32[1], ',', HVA.V4.M128_F32[2], ',', HVA.V4.M128_F32[3]);
  AddRes := HVATest(B1, B2, B3, HVA, B4, 1);
  SubRes := HVATest(B1, B2, B3, HVA, B4, 2);
  MixRes := HVATest(B1, B2, B3, HVA, B4, 0);
  WriteLn('----');
  WriteLn('AddRes.V1: ', AddRes.V1.M128_F32[0], ',', AddRes.V1.M128_F32[1], ',', AddRes.V1.M128_F32[2], ',', AddRes.V1.M128_F32[3]);
  WriteLn('AddRes.V2: ', AddRes.V2.M128_F32[0], ',', AddRes.V2.M128_F32[1], ',', AddRes.V2.M128_F32[2], ',', AddRes.V2.M128_F32[3]);
  WriteLn('AddRes.V3: ', AddRes.V3.M128_F32[0], ',', AddRes.V3.M128_F32[1], ',', AddRes.V3.M128_F32[2], ',', AddRes.V3.M128_F32[3]);
  WriteLn('AddRes.V4: ', AddRes.V4.M128_F32[0], ',', AddRes.V4.M128_F32[1], ',', AddRes.V4.M128_F32[2], ',', AddRes.V4.M128_F32[3]);
  WriteLn();
  WriteLn('AddExp.V1: ', AddExp.V1.M128_F32[0], ',', AddExp.V1.M128_F32[1], ',', AddExp.V1.M128_F32[2], ',', AddExp.V1.M128_F32[3]);
  WriteLn('AddExp.V2: ', AddExp.V2.M128_F32[0], ',', AddExp.V2.M128_F32[1], ',', AddExp.V2.M128_F32[2], ',', AddExp.V2.M128_F32[3]);
  WriteLn('AddExp.V3: ', AddExp.V3.M128_F32[0], ',', AddExp.V3.M128_F32[1], ',', AddExp.V3.M128_F32[2], ',', AddExp.V3.M128_F32[3]);
  WriteLn('AddExp.V4: ', AddExp.V4.M128_F32[0], ',', AddExp.V4.M128_F32[1], ',', AddExp.V4.M128_F32[2], ',', AddExp.V4.M128_F32[3]);
  WriteLn('----');
  WriteLn('SubRes.V1: ', SubRes.V1.M128_F32[0], ',', SubRes.V1.M128_F32[1], ',', SubRes.V1.M128_F32[2], ',', SubRes.V1.M128_F32[3]);
  WriteLn('SubRes.V2: ', SubRes.V2.M128_F32[0], ',', SubRes.V2.M128_F32[1], ',', SubRes.V2.M128_F32[2], ',', SubRes.V2.M128_F32[3]);
  WriteLn('SubRes.V3: ', SubRes.V3.M128_F32[0], ',', SubRes.V3.M128_F32[1], ',', SubRes.V3.M128_F32[2], ',', SubRes.V3.M128_F32[3]);
  WriteLn('SubRes.V4: ', SubRes.V4.M128_F32[0], ',', SubRes.V4.M128_F32[1], ',', SubRes.V4.M128_F32[2], ',', SubRes.V4.M128_F32[3]);
  WriteLn();
  WriteLn('SubExp.V1: ', SubExp.V1.M128_F32[0], ',', SubExp.V1.M128_F32[1], ',', SubExp.V1.M128_F32[2], ',', SubExp.V1.M128_F32[3]);
  WriteLn('SubExp.V2: ', SubExp.V2.M128_F32[0], ',', SubExp.V2.M128_F32[1], ',', SubExp.V2.M128_F32[2], ',', SubExp.V2.M128_F32[3]);
  WriteLn('SubExp.V3: ', SubExp.V3.M128_F32[0], ',', SubExp.V3.M128_F32[1], ',', SubExp.V3.M128_F32[2], ',', SubExp.V3.M128_F32[3]);
  WriteLn('SubExp.V4: ', SubExp.V4.M128_F32[0], ',', SubExp.V4.M128_F32[1], ',', SubExp.V4.M128_F32[2], ',', SubExp.V4.M128_F32[3]);
  WriteLn('----');
  WriteLn('MixRes.V1: ', MixRes.V1.M128_F32[0], ',', MixRes.V1.M128_F32[1], ',', MixRes.V1.M128_F32[2], ',', MixRes.V1.M128_F32[3]);
  WriteLn('MixRes.V2: ', MixRes.V2.M128_F32[0], ',', MixRes.V2.M128_F32[1], ',', MixRes.V2.M128_F32[2], ',', MixRes.V2.M128_F32[3]);
  WriteLn('MixRes.V3: ', MixRes.V3.M128_F32[0], ',', MixRes.V3.M128_F32[1], ',', MixRes.V3.M128_F32[2], ',', MixRes.V3.M128_F32[3]);
  WriteLn('MixRes.V4: ', MixRes.V4.M128_F32[0], ',', MixRes.V4.M128_F32[1], ',', MixRes.V4.M128_F32[2], ',', MixRes.V4.M128_F32[3]);
  WriteLn();
  WriteLn('MixExp.V1: ', MixExp.V1.M128_F32[0], ',', MixExp.V1.M128_F32[1], ',', MixExp.V1.M128_F32[2], ',', MixExp.V1.M128_F32[3]);
  WriteLn('MixExp.V2: ', MixExp.V2.M128_F32[0], ',', MixExp.V2.M128_F32[1], ',', MixExp.V2.M128_F32[2], ',', MixExp.V2.M128_F32[3]);
  WriteLn('MixExp.V3: ', MixExp.V3.M128_F32[0], ',', MixExp.V3.M128_F32[1], ',', MixExp.V3.M128_F32[2], ',', MixExp.V3.M128_F32[3]);
  WriteLn('MixExp.V4: ', MixExp.V4.M128_F32[0], ',', MixExp.V4.M128_F32[1], ',', MixExp.V4.M128_F32[2], ',', MixExp.V4.M128_F32[3]);

  for I := 0 to 3 do
    begin
      if AddRes.V1.M128_F32[I] <> AddExp.V1.M128_F32[I] then
        begin
          WriteLn('FAILURE on AddRes.V1.M128_F32[', I, ']');
          Halt(1);
        end;
      if SubRes.V1.M128_F32[I] <> SubExp.V1.M128_F32[I] then
        begin
          WriteLn('FAILURE on SubRes.V1.M128_F32[', I, ']');
          Halt(1);
        end;
      if MixRes.V1.M128_F32[I] <> MixExp.V1.M128_F32[I] then
        begin
          WriteLn('FAILURE on MixRes.V1.M128_F32[', I, ']');
          Halt(1);
        end;
    end;

  WriteLn('ok');
end.
