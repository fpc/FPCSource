{ %cpu=i8086 }

{ far pointer equality (=, <>) comparison tests }

{ = and <> should compare *both* the segment and the offset }
{ >, <, >= and <= should compare only the offset }
{ note: >, <, >= and <= are tested only with equal pointers in this test }

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
  eq, neq: Boolean;
  lt, gt, lteq, gteq: Boolean;
begin
  ErrorCode := 0;

  Writeln('var, var');
  FarPtr := Ptr($1234, $5678);
  FarPtr2 := Ptr($1234, $5678);
  eq := FarPtr = FarPtr2;
  neq := FarPtr <> FarPtr2;
  lt := FarPtr < FarPtr2;
  lteq := FarPtr <= FarPtr2;
  gt := FarPtr > FarPtr2;
  gteq := FarPtr >= FarPtr2;
  if not eq or neq or lt or not lteq or gt or not gteq then
    Error(1);

  FarPtr := Ptr($1234, $5678);
  FarPtr2 := Ptr($4321, $5678);
  eq := FarPtr = FarPtr2;
  neq := FarPtr <> FarPtr2;
  if eq or not neq then
    Error(2);

  FarPtr := Ptr($1234, $5678);
  FarPtr2 := Ptr($1234, $8765);
  eq := FarPtr = FarPtr2;
  neq := FarPtr <> FarPtr2;
  if eq or not neq then
    Error(3);

  FarPtr := Ptr($1234, $5678);
  FarPtr2 := Ptr($4321, $8765);
  eq := FarPtr = FarPtr2;
  neq := FarPtr <> FarPtr2;
  if eq or not neq then
    Error(4);

  FarPtr := Ptr($1234, $5678);
  FarPtr2 := Ptr($1235, $5668);
  eq := FarPtr = FarPtr2;
  neq := FarPtr <> FarPtr2;
  if eq or not neq then
    Error(5);

  Writeln('var, ptr(const)');
  FarPtr := Ptr($1234, $5678);
  eq := FarPtr = Ptr($1234, $5678);
  neq := FarPtr <> Ptr($1234, $5678);
  lt := FarPtr < Ptr($1234, $5678);
  lteq := FarPtr <= Ptr($1234, $5678);
  gt := FarPtr > Ptr($1234, $5678);
  gteq := FarPtr >= Ptr($1234, $5678);
  if not eq or neq or lt or not lteq or gt or not gteq then
    Error(1);

  FarPtr := Ptr($1234, $5678);
  eq := FarPtr = Ptr($4321, $5678);
  neq := FarPtr <> Ptr($4321, $5678);
  if eq or not neq then
    Error(2);

  FarPtr := Ptr($1234, $5678);
  eq := FarPtr = Ptr($1234, $8765);
  neq := FarPtr <> Ptr($1234, $8765);
  if eq or not neq then
    Error(3);

  FarPtr := Ptr($1234, $5678);
  eq := FarPtr = Ptr($4321, $8765);
  neq := FarPtr <> Ptr($4321, $8765);
  if eq or not neq then
    Error(4);

  FarPtr := Ptr($1234, $5678);
  eq := FarPtr = Ptr($1235, $5668);
  neq := FarPtr <> Ptr($1235, $5668);
  if eq or not neq then
    Error(5);

  Writeln('ptr(const), ptr(const)');
  eq := Ptr($1234, $5678) = Ptr($1234, $5678);
  neq := Ptr($1234, $5678) <> Ptr($1234, $5678);
  lt := Ptr($1234, $5678) < Ptr($1234, $5678);
  lteq := Ptr($1234, $5678) <= Ptr($1234, $5678);
  gt := Ptr($1234, $5678) > Ptr($1234, $5678);
  gteq := Ptr($1234, $5678) >= Ptr($1234, $5678);
  if not eq or neq or lt or not lteq or gt or not gteq then
    Error(1);

  eq := Ptr($1234, $5678) = Ptr($4321, $5678);
  neq := Ptr($1234, $5678) <> Ptr($4321, $5678);
  if eq or not neq then
    Error(2);

  eq := Ptr($1234, $5678) = Ptr($1234, $8765);
  neq := Ptr($1234, $5678) <> Ptr($1234, $8765);
  if eq or not neq then
    Error(3);

  eq := Ptr($1234, $5678) = Ptr($4321, $8765);
  neq := Ptr($1234, $5678) <> Ptr($4321, $8765);
  if eq or not neq then
    Error(4);

  eq := Ptr($1234, $5678) = Ptr($1235, $5668);
  neq := Ptr($1234, $5678) <> Ptr($1235, $5668);
  if eq or not neq then
    Error(5);

  Writeln('var, nil');
  FarPtr := Ptr(0, 0);
  eq := FarPtr = nil;
  neq := FarPtr <> nil;
  lt := FarPtr < nil;
  lteq := FarPtr <= nil;
  gt := FarPtr > nil;
  gteq := FarPtr >= nil;
  if not eq or neq or lt or not lteq or gt or not gteq then
    Error(1);

  FarPtr := Ptr(0, 1);
  eq := FarPtr = nil;
  neq := FarPtr <> nil;
  if eq or not neq then
    Error(2);

  FarPtr := Ptr(1, 0);
  eq := FarPtr = nil;
  neq := FarPtr <> nil;
  if eq or not neq then
    Error(3);

  FarPtr := Ptr(1, 1);
  eq := FarPtr = nil;
  neq := FarPtr <> nil;
  if eq or not neq then
    Error(4);

  if ErrorCode = 0 then
    Writeln('Success!')
  else
    Halt(ErrorCode);
end.
