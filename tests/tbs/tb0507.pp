{$goto on}
{$inline on}

var
  j : longint;

procedure p1;inline;
  label l;
  var
    i:longint;
  begin
    i:=0;
  l:
    inc(i);
    while i<2 do
      begin
        goto l;
        goto l;
        goto l;
      end;
  end;


procedure p2;inline;
  label l;
  begin
    goto l;
    goto l;
    goto l;
  l:
  end;


procedure p3;inline;
  begin
    j:=j+1;
  end;


begin
  j:=0;
  p1;
  p1;
  p1;
  p1;
  p2;
  p2;
  p2;
  p2;
  p3;
end.

