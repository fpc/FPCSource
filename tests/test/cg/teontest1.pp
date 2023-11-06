{ %CPU=AARCH64 }
{ %OPT--O2 }
program teontest1;

{ This test evaluates the AArch64 node parsing code that converts an
  "xor not" operation into an EON instruction from Word-sized operands
  into a LongWord result }

const
  InputsList: array[0..15] of Word     = ($0000, $0000, $0000, $0000, $FFFF, $FFFF, $FFFF, $FFFF, $5A5A, $5A5A, $5A5A, $5A5A, $A5A5, $A5A5, $A5A5, $A5A5);
  InvertList: array[0..15] of Word     = ($0000, $FFFF, $5A5A, $A5A5, $0000, $FFFF, $5A5A, $A5A5, $0000, $FFFF, $5A5A, $A5A5, $0000, $FFFF, $5A5A, $A5A5);
  ExpectList: array[0..15] of LongWord = ($FFFF, $0000, $A5A5, $5A5A, $0000, $FFFF, $5A5A, $A5A5, $A5A5, $5A5A, $FFFF, $0000, $5A5A, $A5A5, $0000, $FFFF);
  
var
  X: Integer; Output: LongWord;
  
begin
  for X := Low(InputsList) to High(InputsList) do
    begin
      Output := InputsList[X] xor not InvertList[X];
      if Output <> ExpectList[X] then
        begin
          WriteLn('FAIL: Test index ', X, ', EON($', hexstr(InputsList[X], 4), ', $', hexstr(InputsList[X], 4), ')  expected $', hexstr(ExpectList[X], 8), ' but got $', hexstr(Output, 8));
          Halt(1);
        end; 
    end;
    
  WriteLn('ok');
end.
