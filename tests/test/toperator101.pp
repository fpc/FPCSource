{ which parameters of an operator are hidden: the Boolean ones named RangeCheck
  and OverflowCheck, in either order, while nothing but such parameters follows
  them; any other parameter so named is an ordinary one }
program toperator101;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TCheckedInt = record
    Value: Int64;
    class operator +(const A, B: TCheckedInt; OverflowCheck, RangeCheck: Boolean): TCheckedInt;
  end;

var
  Calls: longint;
  LastRange, LastOverflow, LastFlag: Boolean;
  LastCount: longint;

class operator TCheckedInt.+(const A, B: TCheckedInt; OverflowCheck, RangeCheck: Boolean): TCheckedInt;
begin
  Inc(Calls);
  LastRange:=RangeCheck;
  LastOverflow:=OverflowCheck;
  Result.Value:=A.Value+B.Value;
end;

{ a Boolean with another name ends the hidden parameters: Flag is an operand }
operator - (const A: TCheckedInt; Flag: Boolean; RangeCheck: Boolean) Res: TCheckedInt;
begin
  Inc(Calls);
  LastFlag:=Flag;
  LastRange:=RangeCheck;
  Res.Value:=A.Value-Ord(Flag);
end;

{ a parameter so named that is not a Boolean is an operand }
operator * (const A: TCheckedInt; RangeCheck: longint) Res: TCheckedInt;
begin
  Inc(Calls);
  LastCount:=RangeCheck;
  Res.Value:=A.Value*RangeCheck;
end;

{ one that is not at the end is an operand }
operator / (RangeCheck: Boolean; const A: TCheckedInt) Res: TCheckedInt;
begin
  Inc(Calls);
  LastFlag:=RangeCheck;
  Res.Value:=A.Value;
end;

var
  A, B, C: TCheckedInt;
begin
  A.Value:=1;
  B.Value:=2;
{$R+}{$Q-}
  C:=A+B;
  if (Calls<>1) or not LastRange or LastOverflow or (C.Value<>3) then
    halt(1);
{$R-}{$Q+}
  C:=A+B;
  if (Calls<>2) or LastRange or not LastOverflow or (C.Value<>3) then
    halt(2);
{$Q-}
{$R+}
  C:=A-False;
  if (Calls<>3) or LastFlag or not LastRange or (C.Value<>1) then
    halt(3);
{$R-}
  C:=A-True;
  if (Calls<>4) or not LastFlag or LastRange or (C.Value<>0) then
    halt(4);
{$R+}
  C:=B*3;
  if (Calls<>5) or (LastCount<>3) or (C.Value<>6) then
    halt(5);
  C:=False/B;
  if (Calls<>6) or LastFlag or (C.Value<>2) then
    halt(6);
{$R-}
  C:=True/B;
  if (Calls<>7) or not LastFlag or (C.Value<>2) then
    halt(7);
  writeln('ok');
end.
