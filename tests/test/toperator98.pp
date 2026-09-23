{ the Boolean parameters named RangeCheck and OverflowCheck that end the
  parameter list of an operator are hidden: each call passes the range and
  overflow checking state in effect where the call is }
program toperator98;

{$mode objfpc}

type
  TCheckedInt = record
    Value: Int64;
  end;

var
  Calls: longint;
  LastRange, LastOverflow: Boolean;

operator + (const A, B: TCheckedInt; RangeCheck, OverflowCheck: Boolean) Res: TCheckedInt;
begin
  Inc(Calls);
  LastRange:=RangeCheck;
  LastOverflow:=OverflowCheck;
  Res.Value:=A.Value+B.Value;
end;

operator := (const A: Int64; RangeCheck: Boolean) Res: TCheckedInt;
begin
  Inc(Calls);
  LastRange:=RangeCheck;
  LastOverflow:=False;
  Res.Value:=A;
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
