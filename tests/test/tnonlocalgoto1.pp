{ %fail }
{$mode iso}
label 1;

procedure p1;
  var
    s : ansistring;
  procedure p2;
    begin
      goto 1;
    end;

  begin
    s:='asdf'
  end;

begin
  1:
end.
