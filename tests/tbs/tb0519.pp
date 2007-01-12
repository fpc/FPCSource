var
  e: extended;
  d: double;
  s: single;
begin
  e := pi;
  d := pi;
  s := pi;
  if (sizeof(double) < sizeof(extended)) and
     (d = e) then
    halt(1);
  if (sizeof(double) < sizeof(extended)) and
     (s = e) then
    halt(2);
  if (sizeof(double) < sizeof(extended)) and
     (double(e) = e) then
    halt(3);
  if (sizeof(single) < sizeof(extended)) and
     (single(e) = e) then
    halt(4);

  d := 1.1;
  s := 1.1;
  if (sizeof(single) < sizeof(double)) and
     (d = s) then
   halt(5);
  if (sizeof(single) < sizeof(double)) and
     (d = single(d)) then
   halt(6);
end.
