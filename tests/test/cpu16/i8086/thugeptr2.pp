{ %cpu=i8086 }

{ huge pointer equality (=, <>) non-normalized comparison tests }

{$HugePointerComparisonNormalization Off}

{ when huge pointer comparison normalization is off, = and <> should compare
  the segment and the offset pair, just like far pointers. In other words,
  different pairs of segment:offset that point to the same linear address are
  treated as different. }

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
  eq, neq: Boolean;
begin
  ErrorCode := 0;

  Writeln('var, var');
  HugePtr := HPtr($1234, $5678);
  HugePtr2 := HPtr($1234, $5678);
  eq := HugePtr = HugePtr2;
  neq := HugePtr <> HugePtr2;
  if not eq or neq then
    Error(1);

  HugePtr := HPtr($1234, $5678);
  HugePtr2 := HPtr($4321, $5678);
  eq := HugePtr = HugePtr2;
  neq := HugePtr <> HugePtr2;
  if eq or not neq then
    Error(2);

  HugePtr := HPtr($1234, $5678);
  HugePtr2 := HPtr($1234, $8765);
  eq := HugePtr = HugePtr2;
  neq := HugePtr <> HugePtr2;
  if eq or not neq then
    Error(3);

  HugePtr := HPtr($1234, $5678);
  HugePtr2 := HPtr($4321, $8765);
  eq := HugePtr = HugePtr2;
  neq := HugePtr <> HugePtr2;
  if eq or not neq then
    Error(4);

  HugePtr := HPtr($1234, $5678);
  HugePtr2 := HPtr($1235, $5668);
  eq := HugePtr = HugePtr2;
  neq := HugePtr <> HugePtr2;
  if eq or not neq then
    Error(5);

  Writeln('var, HPtr(const)');
  HugePtr := HPtr($1234, $5678);
  eq := HugePtr = HPtr($1234, $5678);
  neq := HugePtr <> HPtr($1234, $5678);
  if not eq or neq then
    Error(1);

  HugePtr := HPtr($1234, $5678);
  eq := HugePtr = HPtr($4321, $5678);
  neq := HugePtr <> HPtr($4321, $5678);
  if eq or not neq then
    Error(2);

  HugePtr := HPtr($1234, $5678);
  eq := HugePtr = HPtr($1234, $8765);
  neq := HugePtr <> HPtr($1234, $8765);
  if eq or not neq then
    Error(3);

  HugePtr := HPtr($1234, $5678);
  eq := HugePtr = HPtr($4321, $8765);
  neq := HugePtr <> HPtr($4321, $8765);
  if eq or not neq then
    Error(4);

  HugePtr := HPtr($1234, $5678);
  eq := HugePtr = HPtr($1235, $5668);
  neq := HugePtr <> HPtr($1235, $5668);
  if eq or not neq then
    Error(5);

  Writeln('HPtr(const), HPtr(const)');
  eq := HPtr($1234, $5678) = HPtr($1234, $5678);
  neq := HPtr($1234, $5678) <> HPtr($1234, $5678);
  if not eq or neq then
    Error(1);

  eq := HPtr($1234, $5678) = HPtr($4321, $5678);
  neq := HPtr($1234, $5678) <> HPtr($4321, $5678);
  if eq or not neq then
    Error(2);

  eq := HPtr($1234, $5678) = HPtr($1234, $8765);
  neq := HPtr($1234, $5678) <> HPtr($1234, $8765);
  if eq or not neq then
    Error(3);

  eq := HPtr($1234, $5678) = HPtr($4321, $8765);
  neq := HPtr($1234, $5678) <> HPtr($4321, $8765);
  if eq or not neq then
    Error(4);

  eq := HPtr($1234, $5678) = HPtr($1235, $5668);
  neq := HPtr($1234, $5678) <> HPtr($1235, $5668);
  if eq or not neq then
    Error(5);

  Writeln('var, nil');
  HugePtr := HPtr(0, 0);
  eq := HugePtr = nil;
  neq := HugePtr <> nil;
  if not eq or neq then
    Error(1);

  HugePtr := HPtr(0, 1);
  eq := HugePtr = nil;
  neq := HugePtr <> nil;
  if eq or not neq then
    Error(2);

  HugePtr := HPtr(1, 0);
  eq := HugePtr = nil;
  neq := HugePtr <> nil;
  if eq or not neq then
    Error(3);

  HugePtr := HPtr(1, 1);
  eq := HugePtr = nil;
  neq := HugePtr <> nil;
  if eq or not neq then
    Error(4);

  if ErrorCode = 0 then
    Writeln('Success!')
  else
    Halt(ErrorCode);
end.
