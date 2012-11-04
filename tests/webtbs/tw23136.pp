var
 ws:widestring;
 s:string;
begin
  s:='Aajsljaklsja'[1];
  if s<>'A' then
    halt(1);

  ws:=#1234#134#312[1];
  if ws<>#1234 then
    halt(1);

  writeln('ok');
end.
