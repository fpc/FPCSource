{ %norun }

{$optimization regvar on}
procedure test;
var m,n: integer;
begin
  for m := 100 downto 0 do begin
    prefetch (m);
  end;
end;

begin
end.
