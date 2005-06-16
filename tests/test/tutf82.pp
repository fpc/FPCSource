{$codepage utf8}
var
  w : widestring;
begin
  w:='äüö';
  if (ord(w[1])<>$e4) or
    (ord(w[2])<>$fc) or
    (ord(w[3])<>$f6) then
    halt(1);
  writeln('ok');
end.
