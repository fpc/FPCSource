{ %NORUN }

program tw40712b;
{$mode delphi}
type
    TSomeGeneric<T> = object
    type
      TMine = T;
    var
      data: record
        value: T;
      end;
  end;
  TSomeGeneric2 = object(TSomeGeneric<Integer>)
  end;
begin
end.
