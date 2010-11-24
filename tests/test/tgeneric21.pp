{ %fail }
{$mode objfpc}{$H+}

type
  generic TOuter<T> = class(TObject)
  public type
    generic TInner<U> = class(TObject)
    end;
  end;

begin
end.
