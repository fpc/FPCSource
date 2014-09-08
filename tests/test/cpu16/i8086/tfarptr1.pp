{ %cpu=i8086 }

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
  I: Integer;
  W1, W2: Word;
begin
  ErrorCode := 0;

  Writeln('Ptr(const, const)');
  FarPtr := Ptr($1234, $5678);
  if FarPtrRec.offset <> $5678 then
    Error(1);
  if FarPtrRec.segment <> $1234 then
    Error(2);

  Writeln('Ptr(const, var)');
  for I := 1 to 1000 do
  begin
    FarPtr := Ptr($1234, $5678);
    W2 := Random($10000);
    FarPtr := Ptr($4321, W2);
    if FarPtrRec.offset <> W2 then
      Error(3);
    if FarPtrRec.segment <> $4321 then
      Error(4);
  end;

  Writeln('Ptr(var, const)');
  for I := 1 to 1000 do
  begin
    FarPtr := Ptr($1234, $5678);
    W1 := Random($10000);
    FarPtr := Ptr(W1, $8765);
    if FarPtrRec.segment <> W1 then
      Error(5);
    if FarPtrRec.offset <> $8765 then
      Error(6);
  end;

  Writeln('Ptr(var, var)');
  for I := 1 to 1000 do
  begin
    FarPtr := Ptr($1234, $5678);
    W1 := Random($10000);
    W2 := Random($10000);
    FarPtr := Ptr(W1, W2);
    if FarPtrRec.segment <> W1 then
      Error(7);
    if FarPtrRec.offset <> W2 then
      Error(8);
  end;

  Writeln('nil');
  FarPtr := Ptr($1234, $5678);
  FarPtr := nil;
  if FarPtrRec.segment <> 0 then
    Error(9);
  if FarPtrRec.offset <> 0 then
    Error(10);

  Writeln('assignment');
  FarPtr := nil;
  FarPtr2 := Ptr($2143, $6587);
  FarPtr := FarPtr2;
  if FarPtrRec.segment <> $2143 then
    Error(11);
  if FarPtrRec.offset <> $6587 then
    Error(12);

  if ErrorCode = 0 then
    Writeln('Success!')
  else
    Halt(ErrorCode);
end.
