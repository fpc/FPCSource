{ %result=201 }

{$mode delphi}
{$r+}
procedure Test;
var
  Count: Word;
  I: Integer;

begin
  Count := 0;

  for I := 0 to Pred(Count) do
    begin
      WriteLn(I);
      break;
    end;
end;

begin
  test;
end.

