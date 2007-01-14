{ %fail }
{$goto on}
label
  l;

procedure p;
  var
    a : longint;
  begin
    writeln(longint(@l));
  end;

begin
  l:
end.
