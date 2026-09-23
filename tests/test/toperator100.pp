{ as toperator99 in objfpc mode, with either hidden parameter on its own, and
  for the class operator of a generic record }
program toperator100;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TCheckedInt = record
    Value: Int64;
    class operator -(const A, B: TCheckedInt; OverflowCheck: Boolean): TCheckedInt;
  end;

  generic TBox<T> = record
    Value: T;
    class operator *(const A, B: TBox; RangeCheck: Boolean): TBox;
  end;

  TIntBox = specialize TBox<longint>;

var
  Calls: longint;
  LastRange, LastOverflow: Boolean;

class operator TCheckedInt.-(const A, B: TCheckedInt; OverflowCheck: Boolean): TCheckedInt;
begin
  Inc(Calls);
  LastRange:=False;
  LastOverflow:=OverflowCheck;
  Result.Value:=A.Value-B.Value;
end;

class operator TBox.*(const A, B: TBox; RangeCheck: Boolean): TBox;
begin
  Inc(Calls);
  LastRange:=RangeCheck;
  LastOverflow:=False;
  Result.Value:=A.Value*B.Value;
end;

procedure Check(Row, WantCalls: longint; WantRange, WantOverflow: Boolean);
begin
  if (Calls<>WantCalls) or (LastRange<>WantRange) or (LastOverflow<>WantOverflow) then
    halt(Row);
end;

var
  A, B, C: TCheckedInt;
  X, Y, Z: TIntBox;
begin
  A.Value:=5;
  B.Value:=2;
{$Q+}
  C:=A-B;
  Check(1,1,False,True);
{$Q-}
  C:=A-B;
  Check(2,2,False,False);
  if C.Value<>3 then
    halt(3);
  X.Value:=6;
  Y.Value:=7;
{$R+}
  Z:=X*Y;
  Check(4,3,True,False);
{$R-}
  Z:=X*Y;
  Check(5,4,False,False);
  if Z.Value<>42 then
    halt(6);
  writeln('ok');
end.
