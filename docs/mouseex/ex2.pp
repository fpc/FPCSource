Program Example2;

{ Program to demonstrate the GetMouseButtons function. }

Uses mouse;

begin
  InitMouse;
  Writeln('Press right mouse button to exit program');
  While (GetMouseButtons<>MouseRightButton) do ;
  DoneMouse;
end.
