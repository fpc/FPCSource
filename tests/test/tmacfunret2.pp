{ %fail }

{$mode macpas}

procedure Example;

begin
return false;  // compiler catches this, but crashes instead of reporting an error
end;

begin
end.
