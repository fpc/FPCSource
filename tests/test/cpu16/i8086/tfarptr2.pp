{ %cpu=i8086 }

{ far pointer equality comparison tests }

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
begin
  ErrorCode := 0;

  FarPtr := Ptr($1234, $5678);
  FarPtr2 := Ptr($1234, $5678);
  eq := FarPtr = FarPtr2;
  neq := FarPtr <> FarPtr2;
  if not eq or neq then
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

  if ErrorCode = 0 then
    Writeln('Success!')
  else
    Halt(ErrorCode);
end.
