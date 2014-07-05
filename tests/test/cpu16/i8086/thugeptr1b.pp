{ %cpu=i8086 }

program thugeptr1b;

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
  I: Integer;
  W1, W2: Word;
begin
  ErrorCode := 0;

  Writeln('Ptr(const, const)');
  HugePtr := HPtr($1234, $5678);
  if HugePtrRec.offset <> $5678 then
    Error(1);
  if HugePtrRec.segment <> $1234 then
    Error(2);

  Writeln('Ptr(const, var)');
  for I := 1 to 1000 do
  begin
    HugePtr := HPtr($1234, $5678);
    W2 := Random($10000);
    HugePtr := HPtr($4321, W2);
    if HugePtrRec.offset <> W2 then
      Error(3);
    if HugePtrRec.segment <> $4321 then
      Error(4);
  end;

  Writeln('Ptr(var, const)');
  for I := 1 to 1000 do
  begin
    HugePtr := HPtr($1234, $5678);
    W1 := Random($10000);
    HugePtr := HPtr(W1, $8765);
    if HugePtrRec.segment <> W1 then
      Error(5);
    if HugePtrRec.offset <> $8765 then
      Error(6);
  end;

  Writeln('Ptr(var, var)');
  for I := 1 to 1000 do
  begin
    HugePtr := HPtr($1234, $5678);
    W1 := Random($10000);
    W2 := Random($10000);
    HugePtr := HPtr(W1, W2);
    if HugePtrRec.segment <> W1 then
      Error(7);
    if HugePtrRec.offset <> W2 then
      Error(8);
  end;

  Writeln('nil');
  HugePtr := HPtr($1234, $5678);
  HugePtr := nil;
  if HugePtrRec.segment <> 0 then
    Error(9);
  if HugePtrRec.offset <> 0 then
    Error(10);

  Writeln('assignment');
  HugePtr := nil;
  HugePtr2 := HPtr($2143, $6587);
  HugePtr := HugePtr2;
  if HugePtrRec.segment <> $2143 then
    Error(11);
  if HugePtrRec.offset <> $6587 then
    Error(12);

  if ErrorCode = 0 then
    Writeln('Success!')
  else
    Halt(ErrorCode);
end.
