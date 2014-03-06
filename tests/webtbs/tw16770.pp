{ %result=201 }

{$mode delphi}
{$r+}
var
  Count: Word;

procedure Test;
var
  I: Integer;

begin
  for I := 0 to Pred(Count) do
    begin
      WriteLn(I);
      break;
    end;
end;

begin
  Count := 0;

  test;
end.

