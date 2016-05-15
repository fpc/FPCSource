unit tw26482;

{$mode delphi}

interface

type
  TEnumerator<T> = class
  end;

  TList<T> = class
  public
    type
      TEnumerator = class(TObject);
  protected
    function DoGetEnumerator: TEnumerator<T>;
  end;

implementation

function TList<T>.DoGetEnumerator: TEnumerator<T>; // Error: Identifier not found "TEnumerator$1"
begin
end;

end.
