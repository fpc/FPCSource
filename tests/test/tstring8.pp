uses strings;

var
  a, b: pchar;

begin
  a := nil;
  b := 'abc';
  if (strpos(a,b) <> nil) or
     (strpos(b,a) <> nil) or
     (strpos(a,a) <> nil) then
    halt(1);
end.
