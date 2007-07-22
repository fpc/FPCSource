{ %fail }

{$mode objfpc}

type
  generic TG1<T> = class
  end;

  generic TG2<T> = class(TG1)
  end;

begin
end.
