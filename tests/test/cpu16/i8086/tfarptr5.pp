{ %cpu=i8086 }

{ far pointer comparison tests (>, <, >= and <=) }

{ >, <, >= and <= should compare only the offset }
{ = and <> (not tested here) should compare *both* the segment and the offset }

var
  ErrorCode: Integer;

procedure Error(Code: Integer);
begin
  Writeln('Error: ', code);
  ErrorCode := Code;
end;

type
  TFarPtrRec = packed record
    offset: Word;
    segment: Word;
  end;

var
  FarPtr: FarPointer;
  FarPtr2: FarPointer;
  FarPtrRec: TFarPtrRec absolute FarPtr;
  lt, gt, lteq, gteq: Boolean;
begin
  ErrorCode := 0;

  Writeln('var, var');
  FarPtr := Ptr($4321, $5678);
  FarPtr2 := Ptr($1234, $89AB);
  lt := FarPtr < FarPtr2;
  lteq := FarPtr <= FarPtr2;
  gt := FarPtr > FarPtr2;
  gteq := FarPtr >= FarPtr2;
  if not lt or not lteq or gt or gteq then
    Error(1);

  FarPtr := Ptr($1234, $89AB);
  FarPtr2 := Ptr($4321, $5678);
  lt := FarPtr < FarPtr2;
  lteq := FarPtr <= FarPtr2;
  gt := FarPtr > FarPtr2;
  gteq := FarPtr >= FarPtr2;
  if lt or lteq or not gt or not gteq then
    Error(2);

  Writeln('var, ptr(const)');
  FarPtr := Ptr($4321, $5678);
  lt := FarPtr < Ptr($1234, $89AB);
  lteq := FarPtr <= Ptr($1234, $89AB);
  gt := FarPtr > Ptr($1234, $89AB);
  gteq := FarPtr >= Ptr($1234, $89AB);
  if not lt or not lteq or gt or gteq then
    Error(1);

  FarPtr := Ptr($1234, $89AB);
  lt := FarPtr < Ptr($4321, $5678);
  lteq := FarPtr <= Ptr($4321, $5678);
  gt := FarPtr > Ptr($4321, $5678);
  gteq := FarPtr >= Ptr($4321, $5678);
  if lt or lteq or not gt or not gteq then
    Error(2);

  Writeln('ptr(const), ptr(const)');
  lt := Ptr($4321, $5678) < Ptr($1234, $89AB);
  lteq := Ptr($4321, $5678) <= Ptr($1234, $89AB);
  gt := Ptr($4321, $5678) > Ptr($1234, $89AB);
  gteq := Ptr($4321, $5678) >= Ptr($1234, $89AB);
  if not lt or not lteq or gt or gteq then
    Error(1);

  lt := Ptr($1234, $89AB) < Ptr($4321, $5678);
  lteq := Ptr($1234, $89AB) <= Ptr($4321, $5678);
  gt := Ptr($1234, $89AB) > Ptr($4321, $5678);
  gteq := Ptr($1234, $89AB) >= Ptr($4321, $5678);
  if lt or lteq or not gt or not gteq then
    Error(2);

  if ErrorCode = 0 then
    Writeln('Success!')
  else
    Halt(ErrorCode);
end.
