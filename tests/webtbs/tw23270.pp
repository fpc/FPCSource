{$MODE DELPHI}

type
  TSmallWrapper<TValue> = record
    Value: TValue;
  end;

  TWrapper<T> = class
  strict private
    class var FSmallWrapper: TSmallWrapper<PInteger>;
  public
    class procedure Z; static;
  end;

class procedure TWrapper<T>.Z;
begin
  FSmallWrapper.Value := New(PInteger);
  Dispose(FSmallWrapper.Value);  { Error: pointer type expected, but ... }
end;

begin
  TWrapper<Byte>.Z;
end.
