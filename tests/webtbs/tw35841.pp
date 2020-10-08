{ %norun }
{$mode objfpc}
procedure p;
  begin
    try
      writeln
    except
      try
        writeln;
        Exit;
      finally
        writeln;
      end;
    end;
  end;

begin
end.
