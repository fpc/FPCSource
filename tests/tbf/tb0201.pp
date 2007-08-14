{ %fail }

{$mode delphi}
const
  WideNull = widechar(#0);
  WideSpace = widechar(#32);

var
  w : widechar;
  w2,w3 : widechar;
begin
  w:=WideSpace;
  w3:=WideSpace;
  w2:=WideNull;
  if not(w in [WideSpace]) then
    begin
      writeln('error 1');
      halt(1);
    end;
  if not(w in [WideNull..WideSpace]) then
    begin
      writeln('error 2');
      halt(1);
    end;
  if not(w in [WideNull..WideSpace,w3]) then
    begin
      writeln('error 3');
      halt(1);
    end;
  if not(w in [WideNull..WideSpace,w2..w3]) then
    begin
      writeln('error 4');
      halt(1);
    end;
end.
