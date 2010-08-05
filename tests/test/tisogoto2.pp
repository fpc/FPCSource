{ %fail }
{$mode iso}
program tisogoto2(output);
label
  1;

procedure p1;
  begin
  1:
  end;
  
procedure p2;
  begin
    goto 1;
  end;
  
begin
  p2;
end.
