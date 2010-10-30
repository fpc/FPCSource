{ %fail }
{$mode objfpc}
{$goto on}
{$modeswitch nonlocalgoto }
label 1;

procedure p1;
  procedure p2;
    begin
      goto 1;
    end;

  begin
    try
      p2;
    finally
    end;
  end;

begin
  1:
end.
