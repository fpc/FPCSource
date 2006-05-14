{ tests if '' is optimized properly in string concatenations }
var
  s1 : string;
  s2 : string;

begin
  s1:='asdf';
  if s1+''<>s1 then
    halt(1);
  s1:='asdf';
  if ''+s1<>s1 then
    halt(1);

  if ''+s2+''+s1+''<>s2+s1 then
    halt(1);

  writeln('ok');
end.