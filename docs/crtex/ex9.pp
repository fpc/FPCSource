Program Example9;
uses Crt;

{ Program to demonstrate the ClrEol function. }

begin
  Write('This line will be cleared from the',
        ' cursor postion until the right of the screen');
  GotoXY(27,WhereY);
  ReadKey;
  ClrEol;
  WriteLn;
end.
