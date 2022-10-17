uses
  strutils;
Var
  matches: SizeIntArray;
  i : Longint;

begin
  FindMatchesBoyerMooreCaseSensitive('a x b x c', 'x', matches, {matchAll}true); // never returns
  if matches[0]<>3 then
    halt(1);
  if matches[1]<>7 then
    halt(1);  
  FindMatchesBoyerMooreCaseSensitive('a xx b xx c', 'xx', matches, {matchAll}true); // never returns
  if matches[0]<>3 then
    halt(1);
  if matches[1]<>8 then
    halt(1);  
  FindMatchesBoyerMooreCaseSensitive('a xy b xy c', 'xy', matches, {matchAll}true); // ok
  if matches[0]<>3 then
    halt(1);
  if matches[1]<>8 then
    halt(1);  
  writeln('ok');
end.

