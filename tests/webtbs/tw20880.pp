{ %interactive }

program CrtBug;

uses Crt;

begin
  ClrScr;
  Window(windmaxx - 25, 5, windmaxx, 20);
  TextColor(LightRed);
  TextBackground(Cyan); 
  ClrScr;  
  
  while not KeyPressed do
  begin
    Write('R=', Random(256), ' ');
    Delay(100);
  end;
  
  ReadKey;
end.
