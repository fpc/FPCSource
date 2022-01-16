uses gcmem;  // use new Boehm GC collector and create leaks

var p:^integer;
    i:integer;
    
begin
for i:=0 to 99 do
  begin
  new(p); //pointers created, never tidied up
  end;
end.
