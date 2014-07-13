{ %cpu=i8086 }

{ huge pointer normalized comparison tests (>, <, >= and <=) }

{$HugePointerComparisonNormalization On}

{ when huge pointer comparison normalization is on:

  = and <> (not tested here) should compare
  the linear address of the huge pointers. In other words, different pairs of
  segment:offset that point to the same linear address are treated as equal.

  >, <, >= and <= should compare based on their linear address as well }

var
  ErrorCode: Integer;

procedure Error(Code: Integer);
begin
  Writeln('Error: ', code);
  ErrorCode := Code;
end;

function HPtr(sel, off: Word): HugePointer; inline;
begin
  HPtr := HugePointer(Ptr(sel, off));
end;

type
  THugePtrRec = packed record
    offset: Word;
    segment: Word;
  end;

var
  HugePtr: HugePointer;
  HugePtr2: HugePointer;
  HugePtrRec: THugePtrRec absolute HugePtr;
  lt, gt, lteq, gteq: Boolean;
begin
  ErrorCode := 0;

  Writeln('var, var');
  HugePtr := HPtr($1230, $6876);
  HugePtr2 := HPtr($1234, $5678);
  lt := HugePtr < HugePtr2;
  lteq := HugePtr <= HugePtr2;
  gt := HugePtr > HugePtr2;
  gteq := HugePtr >= HugePtr2;
  if lt or lteq or not gt or not gteq then
    Error(1);

  HugePtr := HPtr($1234, $5678);
  HugePtr2 := HPtr($1230, $6876);
  lt := HugePtr < HugePtr2;
  lteq := HugePtr <= HugePtr2;
  gt := HugePtr > HugePtr2;
  gteq := HugePtr >= HugePtr2;
  if not lt or not lteq or gt or gteq then
    Error(2);

  HugePtr := HPtr($1234, $5678);
  HugePtr2 := HPtr($7230, $3876);
  lt := HugePtr < HugePtr2;
  lteq := HugePtr <= HugePtr2;
  gt := HugePtr > HugePtr2;
  gteq := HugePtr >= HugePtr2;
  if not lt or not lteq or gt or gteq then
    Error(3);

  HugePtr := HPtr($7230, $3876);
  HugePtr2 := HPtr($1234, $5678);
  lt := HugePtr < HugePtr2;
  lteq := HugePtr <= HugePtr2;
  gt := HugePtr > HugePtr2;
  gteq := HugePtr >= HugePtr2;
  if lt or lteq or not gt or not gteq then
    Error(4);

  HugePtr := HPtr($7230, $6876);
  HugePtr2 := HPtr($8234, $5678);
  lt := HugePtr < HugePtr2;
  lteq := HugePtr <= HugePtr2;
  gt := HugePtr > HugePtr2;
  gteq := HugePtr >= HugePtr2;
  if not lt or not lteq or gt or gteq then
    Error(5);

  HugePtr := HPtr($8234, $5678);
  HugePtr2 := HPtr($7230, $6876);
  lt := HugePtr < HugePtr2;
  lteq := HugePtr <= HugePtr2;
  gt := HugePtr > HugePtr2;
  gteq := HugePtr >= HugePtr2;
  if lt or lteq or not gt or not gteq then
    Error(6);

  Writeln('var, ptr(const)');
  HugePtr := HPtr($1230, $6876);
  lt := HugePtr < HPtr($1234, $5678);
  lteq := HugePtr <= HPtr($1234, $5678);
  gt := HugePtr > HPtr($1234, $5678);
  gteq := HugePtr >= HPtr($1234, $5678);
  if lt or lteq or not gt or not gteq then
    Error(1);

  HugePtr := HPtr($1234, $5678);
  lt := HugePtr < HPtr($1230, $6876);
  lteq := HugePtr <= HPtr($1230, $6876);
  gt := HugePtr > HPtr($1230, $6876);
  gteq := HugePtr >= HPtr($1230, $6876);
  if not lt or not lteq or gt or gteq then
    Error(2);

  HugePtr := HPtr($1234, $5678);
  lt := HugePtr < HPtr($7230, $3876);
  lteq := HugePtr <= HPtr($7230, $3876);
  gt := HugePtr > HPtr($7230, $3876);
  gteq := HugePtr >= HPtr($7230, $3876);
  if not lt or not lteq or gt or gteq then
    Error(3);

  HugePtr := HPtr($7230, $3876);
  lt := HugePtr < HPtr($1234, $5678);
  lteq := HugePtr <= HPtr($1234, $5678);
  gt := HugePtr > HPtr($1234, $5678);
  gteq := HugePtr >= HPtr($1234, $5678);
  if lt or lteq or not gt or not gteq then
    Error(4);

  HugePtr := HPtr($7230, $6876);
  lt := HugePtr < HPtr($8234, $5678);
  lteq := HugePtr <= HPtr($8234, $5678);
  gt := HugePtr > HPtr($8234, $5678);
  gteq := HugePtr >= HPtr($8234, $5678);
  if not lt or not lteq or gt or gteq then
    Error(5);

  HugePtr := HPtr($8234, $5678);
  lt := HugePtr < HPtr($7230, $6876);
  lteq := HugePtr <= HPtr($7230, $6876);
  gt := HugePtr > HPtr($7230, $6876);
  gteq := HugePtr >= HPtr($7230, $6876);
  if lt or lteq or not gt or not gteq then
    Error(6);

  Writeln('ptr(const), ptr(const)');
  lt := HPtr($1230, $6876) < HPtr($1234, $5678);
  lteq := HPtr($1230, $6876) <= HPtr($1234, $5678);
  gt := HPtr($1230, $6876) > HPtr($1234, $5678);
  gteq := HPtr($1230, $6876) >= HPtr($1234, $5678);
  if lt or lteq or not gt or not gteq then
    Error(1);

  lt := HPtr($1234, $5678) < HPtr($1230, $6876);
  lteq := HPtr($1234, $5678) <= HPtr($1230, $6876);
  gt := HPtr($1234, $5678) > HPtr($1230, $6876);
  gteq := HPtr($1234, $5678) >= HPtr($1230, $6876);
  if not lt or not lteq or gt or gteq then
    Error(2);

  lt := HPtr($1234, $5678) < HPtr($7230, $3876);
  lteq := HPtr($1234, $5678) <= HPtr($7230, $3876);
  gt := HPtr($1234, $5678) > HPtr($7230, $3876);
  gteq := HPtr($1234, $5678) >= HPtr($7230, $3876);
  if not lt or not lteq or gt or gteq then
    Error(3);

  lt := HPtr($7230, $3876) < HPtr($1234, $5678);
  lteq := HPtr($7230, $3876) <= HPtr($1234, $5678);
  gt := HPtr($7230, $3876) > HPtr($1234, $5678);
  gteq := HPtr($7230, $3876) >= HPtr($1234, $5678);
  if lt or lteq or not gt or not gteq then
    Error(4);

  lt := HPtr($7230, $6876) < HPtr($8234, $5678);
  lteq := HPtr($7230, $6876) <= HPtr($8234, $5678);
  gt := HPtr($7230, $6876) > HPtr($8234, $5678);
  gteq := HPtr($7230, $6876) >= HPtr($8234, $5678);
  if not lt or not lteq or gt or gteq then
    Error(5);

  lt := HPtr($8234, $5678) < HPtr($7230, $6876);
  lteq := HPtr($8234, $5678) <= HPtr($7230, $6876);
  gt := HPtr($8234, $5678) > HPtr($7230, $6876);
  gteq := HPtr($8234, $5678) >= HPtr($7230, $6876);
  if lt or lteq or not gt or not gteq then
    Error(6);

  if ErrorCode = 0 then
    Writeln('Success!')
  else
    Halt(ErrorCode);
end.
