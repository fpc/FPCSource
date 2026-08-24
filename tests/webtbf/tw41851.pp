{ %FAIL }

program tw41851;

{$mode objfpc}

type
  generic tfoo<T: longint> = class
  end;

var
  f: specialize tfoo<longint>;

begin
end.
