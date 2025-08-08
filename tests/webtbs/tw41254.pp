{ %NORUN }

program tw41254;

{$mode objfpc}

type
  generic tt<T> = record
    zz: ^specialize tt<T>; // Error: Internal error 2019112401
  end;

  ttint = specialize tt<longint>;

begin
end.

