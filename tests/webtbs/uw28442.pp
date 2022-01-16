unit uw28442;

{$MODE DELPHI}{$H+}

interface

type
  TEqualityComparer<T> = class
  public
    class function Construct: TEqualityComparer<T>;

    function Test: Boolean; virtual; abstract;
  end;

  TDelegatedEqualityComparerEvents<T> = class(TEqualityComparer<T>)
  public
    function Test: Boolean; override;
  end;

implementation

class function TEqualityComparer<T>.Construct: TEqualityComparer<T>;
begin
  Result := TDelegatedEqualityComparerEvents<T>.Create;
end;

function TDelegatedEqualityComparerEvents<T>.Test: Boolean;
begin
  Result := False;
end;

end.

