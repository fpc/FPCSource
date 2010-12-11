{ %fail}
{ %norun}
program terecs5;

{$mode delphi}

type
  TFoo = record
    class procedure Test; // not allowed without static
  end;

class procedure TFoo.Test;
begin
end;

begin
end.
