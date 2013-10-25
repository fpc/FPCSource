unit uw25132;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

type
  TIterator<TElement> = class(TObject)
  public
    function    GetValue(): Integer; virtual; abstract;
  end;

  TCollectionIterator = class(TIterator<TObject>)
  public  
    function    GetValue(): Integer; override;
  end;

implementation

{ TCollectionIterator }

function TCollectionIterator.GetValue(): Integer; 
begin
  Result := 1;
end;

end.
