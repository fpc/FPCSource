{$mode objfpc}
{$modeswitch advancedrecords}
{
  test binary operators with generic constant params
}
program tgenconst14;

type
  generic TBinaryOp<const I: Integer> = record
    const
    	d0 = I + I;
    	d1 = I - I; 
    	d2 = I * I; 
    	d3 = I / I; 
    	d4 = I div I; 
    	d5 = I mod I; 
    	d6 = I and I;
    	d7 = I or I;
      d8 = I shl 2;
      d9 = I shr 2;
  end;

procedure Check(aExpected, aActual: Integer; aErrorCode: LongInt);
begin
  if aExpected <> aActual then
    Halt(aErrorCode);
end;

var
	op: specialize TBinaryOp<100>;
begin
  Check(op.d0, 100 + 100, 1);
  Check(op.d1, 100 - 100, 2);
  Check(op.d2, 100 * 100, 3);
  Check(Trunc(op.d3), Trunc(100 / 100), 4);
  Check(op.d4, 100 div 100, 5);
  Check(op.d5, 100 mod 100, 6);
  Check(op.d6, 100 and 100, 7);
  Check(op.d7, 100 or 100, 8);
  Check(op.d8, 100 shl 2, 9);
  Check(op.d9, 100 shr 2, 10);
end.
