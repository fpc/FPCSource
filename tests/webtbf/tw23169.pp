{ %fail }
{$mode objfpc}
uses gvector;

type
  generic TObjectVector<T> = class(specialize TVector<T>)
    procedure X; //Destroy; override;
  end;

  TMyVector = specialize TObjectVector<TObject>;

//destructor TMyVector.Destroy;
procedure TMyVector.X;
begin
end;

begin
end.
