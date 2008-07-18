unit tw11436;
{$MODE ObjFPC}

interface

type
  generic gIBla<_T> = interface
    function add( item: _T) : integer;
  end;

  IBla = specialize gIBla<byte>;

implementation

end.
