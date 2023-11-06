{ %CPU=AARCH64 }
{ %OPT--O2 }
program teontest4;

{ This test evaluates the AArch64 node parsing code that converts an
  "xor not" operation into an EON instruction from Word-sized operands
  into a LongWord result }

function TestEON(const Input, InvertedInput: SmallInt): LongInt; noinline;
begin
  TestEON := Input xor not InvertedInput;
end;

const
  InputsList: array[0..15] of SmallInt = (            $0000,          $0000,             $0000,          $0000, SmallInt($FFFF),   SmallInt($FFFF), SmallInt($FFFF),   SmallInt($FFFF),             $5A5A,          $5A5A,             $5A5A,          $5A5A, SmallInt($A5A5),   SmallInt($A5A5), SmallInt($A5A5),   SmallInt($A5A5));
  InvertList: array[0..15] of SmallInt = (            $0000, SmallInt($FFFF),            $5A5A, SmallInt($A5A5),         $0000,    SmallInt($FFFF),          $5A5A,    SmallInt($A5A5),             $0000, SmallInt($FFFF),            $5A5A, SmallInt($A5A5),         $0000,    SmallInt($FFFF),          $5A5A,    SmallInt($A5A5));
  ExpectList: array[0..15] of LongInt  = (LongInt($FFFFFFFF),         $0000, LongInt($FFFFA5A5),         $5A5A,          $0000, LongInt($FFFFFFFF),          $5A5A, LongInt($FFFFA5A5), LongInt($FFFFA5A5),         $5A5A, LongInt($FFFFFFFF),         $0000,          $5A5A, LongInt($FFFFA5A5),          $0000, LongInt($FFFFFFFF));
  
var
  X: Integer; Output: LongInt;
  
begin
  for X := Low(InputsList) to High(InputsList) do
    begin
      Output := TestEON(InputsList[X], InvertList[X]);
      if Output <> ExpectList[X] then
        begin
          WriteLn('FAIL: Test index ', X, ', EON($', hexstr(InputsList[X], 4), ', $', hexstr(InputsList[X], 4), ')  expected $', hexstr(ExpectList[X], 8), ' but got $', hexstr(Output, 8));
          Halt(1);
        end; 
    end;
    
  WriteLn('ok');
end.
