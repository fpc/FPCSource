{ in code, ">>" closes two nested lists of type arguments, as in Delphi: a generic
  method or type specialized with a specialization, then a member of the result }
program tgeneric136;

{$mode delphi}

type
  TBox<T> = class
    Value: Integer;
    constructor Create(aValue: Integer);
  end;

  TPair<A, B> = record
    X: A;
    Y: B;
  end;

  TMaker = class
    function Alloc<T: class>: T;
    class function Size<T>: Integer; static;
    { the same inside a generic body, which is recorded and replayed }
    class function Nested<T>: Integer; static;
  end;

  { and inside a generic class's method, three lists closed at once }
  TWrap<T> = class
    class function Size: Integer; static;
  end;

constructor TBox<T>.Create(aValue: Integer);
begin
  Value := aValue;
end;

function TMaker.Alloc<T>: T;
begin
  Result := T(TBox<Integer>.Create(5));
end;

class function TMaker.Size<T>: Integer;
begin
  Result := SizeOf(T);
end;

class function TMaker.Nested<T>: Integer;
begin
  Result := TMaker.Size<TBox<T>>;
end;

class function TWrap<T>.Size: Integer;
begin
  Result := TMaker.Size<TPair<Integer, TPair<Byte, T>>>;
end;

var
  m: TMaker;
  b: TBox<Integer>;
  bb: TBox<TBox<Integer>>;
  i: Integer;
  s: Integer;
begin
  m := TMaker.Create;
  b := m.Alloc<TBox<Integer>>;
  if b.Value <> 5 then
    Halt(1);
  b.Free;
  i := m.Alloc<TBox<Integer>>.Value;
  if i <> 5 then
    Halt(2);
  bb := TBox<TBox<Integer>>.Create(7);
  if bb.Value <> 7 then
    Halt(3);
  bb.Free;
  s := TMaker.Size<TPair<Integer, TPair<Byte, Byte>>>;
  if s <> SizeOf(TPair<Integer, TPair<Byte, Byte>>) then
    Halt(4);
  i := 1 + m.Alloc<TBox<Integer>>.Value;
  if i <> 6 then
    Halt(5);
  { the shift operator is still one }
  i := 64 >> 2;
  if i <> 16 then
    Halt(6);
  i := 64 shr 3;
  if i <> 8 then
    Halt(7);
  if TMaker.Nested<Integer> <> SizeOf(TBox<Integer>) then
    Halt(8);
  if TWrap<Word>.Size <> SizeOf(TPair<Integer, TPair<Byte, Word>>) then
    Halt(9);
  m.Free;
  WriteLn('ok');
end.
