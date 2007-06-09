type
  tstr = string[2];
  tarr = bitpacked array[0..20] of tstr;

procedure p(var a: tstr);
begin
  a := 'ab';
end;

var
  a: tarr;
begin
  a[0]:='gh';
  p(a[0]);
  if (a[0]<>'ab') then
    halt(1);
end.
