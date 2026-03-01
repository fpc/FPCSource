unit generic_changec_cat;

{$mode objfpc}

interface

type

  { TCat }

  generic TCat<T> = class
    function Jump(w: T): word;
  end;

implementation

{ TCat }

function TCat.Jump(w: T): word;
begin
  Result:=sizeof(T) * w;
end;

end.
