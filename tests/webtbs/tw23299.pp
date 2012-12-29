{$MODE DELPHI}

type
  TSmallWrapper<TValue> = record
    Value: TValue;
  end;

  TWrapper<T> = class
  strict private
    class var FSmallWrapper: TSmallWrapper<Integer>;
  public
    class procedure Z; static;
  end;

class procedure TWrapper<T>.Z;
begin
  FSmallWrapper.Value := 0;
  Inc(FSmallWrapper.Value);
  Dec(FSmallWrapper.Value);
  FSmallWrapper.Value := Succ(FSmallWrapper.Value);
  FSmallWrapper.Value := Pred(FSmallWrapper.Value);
end;

begin
  TWrapper<Byte>.Z;
end.
