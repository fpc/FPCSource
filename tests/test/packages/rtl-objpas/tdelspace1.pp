uses
  strutils;

begin
  if DelSpace1('asdf')<>'asdf' then
    halt(1);
  if DelSpace1(' asdf')<>' asdf' then
    halt(2);
  if DelSpace1('asdf ')<>'asdf ' then
    halt(3);
  if DelSpace1('  asdf')<>' asdf' then
    halt(4);
  if DelSpace1('asdf  ')<>'asdf ' then
    halt(5);
  if DelSpace1('as df')<>'as df' then
    halt(6);
  if DelSpace1('as  df')<>'as df' then
    halt(7);
  if DelSpace1(' a  s   d f')<>' a s d f' then
    halt(8);
  if DelSpace1('  a        b     c   ')<>' a b c ' then
    halt(9);
  writeln('ok')
end.
