{ %norun }
{$mode objfpc}
procedure p1;
  begin
    try
      writeln;
    except
      try
        writeln;
        Exit;
      finally
        writeln;
      end;
    end;
  end;


procedure p2;
  var
    i : longint;
  begin
    for i:=1 to 10 do
      try
        writeln;
      except
        try
          writeln;
          break;
        finally
          writeln;
        end;
      end;
  end;

procedure p3;
  var
    i : longint;
  begin
    for i:=1 to 10 do
      try
        writeln;
      except
        try
          writeln;
          continue;
        finally
          writeln;
        end;
      end;
  end;


begin
end.
