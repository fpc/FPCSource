program WideStringFault;

procedure Test;
var
  w: widestring;
  v: variant;
begin
  w := '';
  v := w;
  w := v;
  if w <> '' then
    halt(1);
end;

begin
  Test;
end.
