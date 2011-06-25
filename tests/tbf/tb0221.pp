{ %fail }

{ should not crash the compiler }

type
  tr = record
    a,b: longint;
  end;

var
  r: tr;
begin
  r[0].a:=1;
end.
