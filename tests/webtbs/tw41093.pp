{ %OPT=-Sg }
{$mode objfpc}
{$H+}

procedure Test;
var
  A: String;

  function TestInline: Char; inline;
  begin
  end;

label
  TestLabel;

begin
  A := TestInline;
  goto TestLabel;
TestLabel:
end;

begin
  Test;
end.
