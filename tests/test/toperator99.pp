{ as toperator98, for the class operators of a record: the declaration of each
  operator and its implementation end with the same hidden parameters }
program toperator99;

{$mode delphi}

type
  TCheckedInt = record
    Value: Int64;
    class operator Add(const A, B: TCheckedInt; RangeCheck, OverflowCheck: Boolean): TCheckedInt;
    class operator Implicit(const A: Int64; RangeCheck: Boolean): TCheckedInt;
  end;

var
  Calls: longint;
  LastRange, LastOverflow: Boolean;

class operator TCheckedInt.Add(const A, B: TCheckedInt; RangeCheck, OverflowCheck: Boolean): TCheckedInt;
begin
  Inc(Calls);
  LastRange:=RangeCheck;
  LastOverflow:=OverflowCheck;
  Result.Value:=A.Value+B.Value;
end;

class operator TCheckedInt.Implicit(const A: Int64; RangeCheck: Boolean): TCheckedInt;
begin
  Inc(Calls);
  LastRange:=RangeCheck;
  LastOverflow:=False;
  Result.Value:=A;
end;

procedure Check(Row, WantCalls: longint; WantRange, WantOverflow: Boolean);
begin
  if (Calls<>WantCalls) or (LastRange<>WantRange) or (LastOverflow<>WantOverflow) then
    halt(Row);
end;

var
  A, B, C: TCheckedInt;
begin
  A.Value:=1;
  B.Value:=2;
{$R+}{$Q+}
  C:=A+B;
  Check(1,1,True,True);
{$R-}{$Q-}
  C:=A+B;
  Check(2,2,False,False);
{$R+}{$Q-}
  C:=A+B;
  Check(3,3,True,False);
{$R-}{$Q+}
  C:=A+B;
  Check(4,4,False,True);
{$Q-}
  if C.Value<>3 then
    halt(5);
{$R+}
  C:=Int64(5);
  Check(6,5,True,False);
{$R-}
  C:=Int64(7);
  Check(7,6,False,False);
  if C.Value<>7 then
    halt(8);
  writeln('ok');
end.
