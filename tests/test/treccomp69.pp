{ %FAIL }
{ Record composition: inside a generic, a member access on a composed type
  parameter (R.B) is an error if the specialized type has no such member }
program treccomp;

{$mode objfpc}
{$modeswitch recordcomposition}
{$modeswitch advancedrecords}

type
  generic TTest<T> = record
    A: LongInt;
    contains T;
    C: LongInt;
  end;

  generic TTest2<T> = record
    R: specialize TTest<T>;
    procedure Test;
  end;

  TNested = record
    D: LongInt;
  end;

procedure TTest2.Test;
begin
  if R.B<>42 then Halt(1);
end;

var
  t: specialize TTest2<TNested>;
begin
  t.Test;
  WriteLn('Ok');
end. 
