Program Example5;

{ Program to demonstrate the HideMouse function. }

Uses mouse;

Var
  Event : TMouseEvent;
  Visible: Boolean;

begin
  InitMouse;
  ShowMouse;
  Visible:=True;
  Writeln('Press left mouse button to hide/show, right button quits');
  Repeat
   GetMouseEvent(Event);
   With Event do
     If (Buttons=MouseLeftbutton) and
        (Action=MouseActionDown) then
       begin
       If Visible then
         HideMouse
       else
         ShowMouse;
       Visible:=Not Visible;
       end;
  Until (Event.Buttons=MouseRightButton) and
        (Event.Action=MouseActionDown);
  DoneMouse;
end.
