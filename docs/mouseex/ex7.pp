Program Example7;

{ Program to demonstrate the SetMouseXY function. }

Uses mouse;

Var
  Event : TMouseEvent;

begin
  InitMouse;
  Writeln('Click right mouse button to quit.');
  SetMouseXY(40,12);
  Repeat 
    If (GetMouseX>70) then
      SetMouseXY(10,GetMouseY);
    If (GetMouseY>20) then
      SetMouseXY(GetMouseX,5);
    GetMouseEvent(Event);
  Until (Event.Buttons=MouseRightButton) and
        (Event.Action=MouseActionDown);
  DoneMouse;
end.
