unit uw29859b;

{$mode delphi}

interface

type
  TMyRecord<T> = record
  public
    FValue: T;
    class operator LogicalAnd(A: TMyRecord<T>; B: Boolean): TMyRecord<T>;
  end;

implementation

class operator TMyRecord<T>.LogicalAnd(A: TMyRecord<T>; B: Boolean): TMyRecord<T>;
begin
  Result.FValue := A.FValue and B;

end;

end.
