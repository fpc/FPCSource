{$ifdef fpc}
{$mode objfpc}
{$inline on}
{$endif}

function fa: ansistring;
begin
  fa:='b';
  fa:=result+'a';
end;

function fb: ansistring;
begin
  fb:='c';
  fb:=result+'d';
end;

procedure test(const a,b: pointer); inline;
begin
  if (ansistring(a)<>'ba') or
     (ansistring(b)<>'cd') then
    halt(1);
end;

begin
  test(pointer(fa()),pointer(fb()));
end.
