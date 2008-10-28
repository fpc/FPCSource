unit tw11435b;
{$MODE ObjFPC}

interface

type
  generic gCBla<_T> = class
    function add( item: _T) : integer;
  end;

  CBla = specialize gCBla<Pointer>;

implementation

function gCBla.add( item: _T) : integer;
begin
  result := 0;
end;

end.
