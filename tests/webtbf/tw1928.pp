{ %fail }

type
 tmethod = procedure (x: byte);

begin
 tmethod(get_static_method)(0);
end.
