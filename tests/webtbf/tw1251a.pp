{ %FAIL }

{$mode objfpc}

begin
  try
    writeln ('Start: create game object');
  except
    on e : exception do
      writeln ('Exception: ', e.message);
  end;
end.
