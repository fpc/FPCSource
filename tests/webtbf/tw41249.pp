{ %FAIL }
program tw41249;
var
  c: Char;
  s: string;

begin
  Str(c, s); // Ensure a compiler error occurs and not Internal error 2013032603
end.
