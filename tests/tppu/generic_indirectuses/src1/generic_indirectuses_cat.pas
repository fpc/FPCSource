unit generic_indirectuses_cat;

{$mode objfpc}

interface

type

  { TCat }

  TCat = class
  public
    generic function Jump<T>(w: word): word;
  end;

implementation

{ TCat }

generic function TCat.Jump<T>(w: word): word;
begin
  Result:=SizeOf(T)+w;
end;

end.
