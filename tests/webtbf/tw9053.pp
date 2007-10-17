{ %fail }

program UnknownLocalSymbol;

  procedure p;
  label
    10;
  begin
    if False then
      begin
        10: Exit
      end;
    goto 10
  end;

begin
end.

