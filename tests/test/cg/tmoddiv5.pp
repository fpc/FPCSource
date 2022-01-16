program tmoddiv5;

{$MACRO ON}

const
  DivModConst_NumeratorCount = 6;

type
  TExpectedSet = record
    Divisor: QWord;
    ExpectedQ: array[0..DivModConst_NumeratorCount - 1] of QWord;
    ExpectedR: array[0..DivModConst_NumeratorCount - 1] of QWord;
  end;

{ NOTES:
  -  $DE0B6B3A7640000 =  1,000,000,000,000,000,000
  - $4563918244F40000 =  5,000,000,000,000,000,000
  - $8AC7230489E80000 = 10,000,000,000,000,000,000
  - $D02AB486CEDC0000 = 15,000,000,000,000,000,000
  - 18446744073709551615 = $FFFFFFFFFFFFFFFF - this tests how well the compiler can manage large decimal immediates
}

const
  Inputs: array[0..DivModConst_NumeratorCount - 1] of QWord = (0, 500, $4563918244F40000, QWord($8AC7230489E80000), QWord($D02AB486CEDC0000), 18446744073709551615);

  ExpectedSets: array[0..12] of TExpectedSet = (
    (Divisor:                       1; ExpectedQ: (0, 500, $4563918244F40000, QWord($8AC7230489E80000),QWord($D02AB486CEDC0000),   QWord($FFFFFFFFFFFFFFFF));ExpectedR: (0,   0,                 0,                 0,                 0,                  0)),
    (Divisor:                       3; ExpectedQ: (0, 166, $17213080C1A6AAAA,        $2E426101834D5555,       $4563918244F40000,       6148914691236517205); ExpectedR: (0,   2,                 2,                 1,                 0,                  0)),
    (Divisor:                     $10; ExpectedQ: (0, 31,   $4563918244F4000,         $8AC7230489E8000,        $D02AB486CEDC000,          $FFFFFFFFFFFFFFF); ExpectedR: (0,   4,                 0,                 0,                 0,                 $F)),
    (Divisor:                    $100; ExpectedQ: (0, 1,     $4563918244F400,   $8AC7230489E800,   $D02AB486CEDC00,     $FFFFFFFFFFFFFF); ExpectedR: (0, 244,                 0,                 0,                 0,                $FF)),
    (Divisor:                  $10000; ExpectedQ: (0, 0,       $4563918244F4,     $8AC7230489E8,     $D02AB486CEDC,       $FFFFFFFFFFFF); ExpectedR: (0, 500,                 0,                 0,                 0,              $FFFF)),
    (Divisor:                 1000000; ExpectedQ: (0, 0,       5000000000000,    10000000000000,    15000000000000,      18446744073709); ExpectedR: (0, 500,                 0,                 0,                 0,             551615)),
    (Divisor:              $100000000; ExpectedQ: (0, 0,           $45639182,         $8AC72304,         $D02AB486,           $FFFFFFFF); ExpectedR: (0, 500,         $44F40000,         $89E80000,         $CEDC0000,          $FFFFFFFF)),
    (Divisor:        $DE0B6B3A7640000; ExpectedQ: (0, 0,                   5,                10,                15,                  18); ExpectedR: (0, 500,                 0,                 0,                 0, 446744073709551615)),
    (Divisor:       $1000000000000000; ExpectedQ: (0, 0,                  $4,                $8,                $D,                  $F); ExpectedR: (0, 500,  $563918244F40000,  $AC7230489E80000,   $2AB486CEDC0000,   $FFFFFFFFFFFFFFF)),
    (Divisor:       $7FFFFFFFFFFFFFFF; ExpectedQ: (0, 0,                   0,                 1,                 1,                   2); ExpectedR: (0, 500, $4563918244F40000,  $AC7230489E80001, $502AB486CEDC0001,                  1)),
    (Divisor: QWord($8000000000000000); ExpectedQ: (0, 0,                   0,                 1,                 1,                   1); ExpectedR: (0, 500, $4563918244F40000,  $AC7230489E80000, $502AB486CEDC0000,  $7FFFFFFFFFFFFFFF)),
    (Divisor: QWord($8AC7230489E80000); ExpectedQ: (0, 0,                   0,                 1,                 1,                   1); ExpectedR: (0, 500, $4563918244F40000,                 0, $4563918244F40000,  $7538DCFB7617FFFF)),
    (Divisor: QWord($FFFFFFFFFFFFFFFF); ExpectedQ: (0, 0,                   0,                 0,                 0,                   1); ExpectedR: (0, 500, $4563918244F40000, QWord($8AC7230489E80000),QWord($D02AB486CEDC0000),                  0))
  );

var
  TestCount, PassCount, SkipCount, FailCount: Cardinal;

{ It must be inline for reasons of code expansion, so div and mod contain constant denominators }
procedure DivModConstTest(D: QWord); inline;
var
  X, Y, Z: QWord; A, C: Integer; FoundSet: Boolean;
begin
  WriteLn('Denominator: ', D);
  WriteLn('---------------------------------');
  FoundSet := False;

  for A := Low(ExpectedSets) to High(ExpectedSets) do
    if ExpectedSets[A].Divisor = D then
    begin
      FoundSet := True;
      Break;
    end;

  if not FoundSet then
    WriteLn('WARNING: Expected values missing');

  for C := Low(Inputs) to High(Inputs) do
  begin
    Inc(TestCount, 2);
    X := Inputs[C];
    Y := X div D;
    Z := X mod D;

    if not FoundSet then
    begin
      WriteLn('  ', X, ' div ', D, ' = ', Y, #10'  ', X, ' mod ', D, ' = ', Z);
      Inc(SkipCount, 2);
    end
    else
    begin

      { Compare quotient values }
      if Y = ExpectedSets[A].ExpectedQ[C] then
      begin
        Write('Pass');
        Inc(PassCount);
      end
      else
      begin
        Write('FAIL');
        Inc(FailCount);
      end;

      WriteLn(' - ', X, ' div ', D, ' = ', Y, '; Expected: ', ExpectedSets[A].ExpectedQ[C]);

      { Compare remainder values }
      if Z = ExpectedSets[A].ExpectedR[C] then
      begin
        Write('Pass');
        Inc(PassCount);
      end
      else
      begin
        Write('FAIL');
        Inc(FailCount);
      end;

      WriteLn(' - ', X, ' mod ', D, ' = ', Z, '; Expected: ', ExpectedSets[A].ExpectedR[C]);

    end;

  end;
  WriteLn();
end;

begin
  { Initialisation }
  TestCount := 0;
  PassCount := 0;
  FailCount := 0;
  SkipCount := 0;

  { Insert tests here }
  DivModConstTest(1);
  DivModConstTest(3);
  DivModConstTest($10);
  DivModConstTest($100);
  DivModConstTest($10000);
  DivModConstTest(1000000);
  DivModConstTest($100000000);
  DivModConstTest(1000000000000000000);
  DivModConstTest($1000000000000000);
  DivModConstTest($7FFFFFFFFFFFFFFF);
  DivModConstTest(QWord($8000000000000000));

  { Comment out these two tests to remove "Internal error 200706094" }
  DivModConstTest(QWord($8AC7230489E80000));
  DivModConstTest(QWord($FFFFFFFFFFFFFFFF));

  { Final tally }
  WriteLn('Total tests: ', TestCount);
  WriteLn('----------------');
  WriteLn('     PASSED: ', PassCount);
  WriteLn('     FAILED: ', FailCount);
  WriteLn('    SKIPPED: ', SkipCount);
  if FailCount<>0 then
    halt(1);
  writeln('ok');
end.

