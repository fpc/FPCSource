{ %fail }

type
  dummyrec = record
    i : int64;
  end;

var
   d: double;
begin
   d := double(dummyrec($ffffffff80000000));
end.
