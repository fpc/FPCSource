{ %fail }
{$mode objfpc}
{$goto on}
{$modeswitch nonlocalgoto }

procedure p1;
  label 1;
  procedure p2;
    begin
      goto 1;
    end;

  begin
    try
      p1;
    finally
    end;
    1:
  end;

begin
end.
