unit uw25598;

{$mode delphi}

interface

type
  TR = record
    class function Foo: Integer; static; inline;
  end;

implementation

const
  C: array[0..0] of byte = (0);

class function TR.Foo: Integer; inline;
begin
  Result := C[0];
end;

end.
