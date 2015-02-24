unit uw27320.Defaults;

{$MODE DELPHI}

interface

type
  IEqualityComparer<T> = interface
  end;

  TEqualityComparer<T> = class
  public
    class function Default: IEqualityComparer<T>; static;
  end;

implementation

class function TEqualityComparer<T>.Default: IEqualityComparer<T>;
begin
end;

end.

