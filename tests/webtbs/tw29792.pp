unit tw29792;

{$mode delphi}

interface

type
  { TMyRecord }

  TMyRecord<T> = record
    class operator Add(A,B: TMyRecord<T>): TMyRecord<T>;
  end;

implementation

{ TMyRecord }

class operator TMyRecord<T>.Add(A, B: TMyRecord<T>): TMyRecord<T>;
begin
  // add implementation
end;

procedure TestIfCompiles;
type
  TInteger = TMyRecord<Integer>;
var
  N1, N2, N3: TInteger;
begin
  N1 := N2 + N3;
end;

end.

