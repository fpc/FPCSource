Program Example4;
uses Crt;

{ Program to demonstrate the TextMode function. }

begin
  TextMode(CO40); {Only clears screen under linux}
  WriteLn('Mode 40x25 Color');
  ReadKey;
  TextMode(CO80);
  WriteLn('Back in Mode 80x25 Color');
  ReadKey;
end.
