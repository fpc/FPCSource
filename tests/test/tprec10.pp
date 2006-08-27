{ %fail }

{$mode tp}

type
  tr = bitpacked record
    a,b: 0..7;
  end;

var
  r: tr;
begin
  for r.a := 0 to 4 do
    writeln; 
end.
