{ %NORUN }

program tw40712a;
{$mode objfpc}
type
  generic TSomeGeneric<T> = object
  type
      TMine = T;
  var
      data: record
        value: TMine;
      end;
  end;
  TSomeGeneric2 = object(specialize TSomeGeneric<Integer>)
  end;
begin
end.

