unit uw29859a;

{$mode delphi}

interface

type
  TMyRecord<T> = record
  public
    FValue: T;
    class operator Add(A,B: TMyRecord<T>): TMyRecord<T>;
  end;

implementation

class operator TMyRecord<T>.Add(A,B: TMyRecord<T>): TMyRecord<T>;
begin
  Result.FValue := A.FValue + B.FValue;
end;

end.

