Program Example10;
uses Crt;

{ Program to demonstrate the InsLine function. }

begin
  ClrScr;
  WriteLn;
  WriteLn('Line 1');
  WriteLn('Line 2');
  WriteLn('Line 2');
  WriteLn('Line 3');
  WriteLn;
  WriteLn('Oops, Line 2 is listed twice,',
          ' let''s delete the line at the cursor postion');
  GotoXY(1,3);
  ReadKey;
  DelLine;
  GotoXY(1,10);
end.
