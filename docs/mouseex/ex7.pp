Program Example7;

{ Program to demonstrate the SetMouseXY function. }

Uses mouse;

begin
  InitMouse;
  Writeln('Click right mouse button to quit.');
  SetMouseXY(40,12);
  Repeat
    Writeln(GetMouseX,',',GetMouseY);
    If (GetMouseX>70) then
      SetMouseXY(10,GetMouseY);
    If (GetMouseY>20) then
      SetMouseXY(GetMouseX,5);
  Until (GetMouseButtons=MouseRightButton);
  DoneMouse;
end.
