{ %NORUN }

program tw28674;

{$mode objfpc}

type
  generic node<T> = object
    data: T;
    link: ^node;
  end;

  tintnode = specialize node<int32>;

begin

end.
