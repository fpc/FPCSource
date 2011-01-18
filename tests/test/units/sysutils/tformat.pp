uses sysutils;

begin
  if format('>%1:*s<',[0, 12,'def',-15]) <> '>         def<' then
    Halt(1);
  if format('>%1:*s< >%*s<', [0, 12, 'abc', 10, 'def']) <> '>         abc< >       def<' then
    Halt(2);
  if format('>%1:*.*s< >%*.*s<', [0, 10,10,'abc', 6,6,'def']) <> '>       abc< >   def<' then
    Halt(3);
end.