Program Example3;

{ Program to demonstrate the GetMouseEvent function. }

Uses mouse;


Var
  Event : TMouseEvent;

begin
  InitMouse;
  Writeln('Play with mouse. Press right mouse button to end.');
  Repeat
    GetMouseEvent(Event);
    With event do
      begin
      Write('Action:');
      Case Action of
        MouseActionDown : Write('Mouse down.');
        MouseActionUp   : Write('Mouse up.');
        MouseActionMove : Write('Mouse move.');
      end;
      Writeln('Button state : ');
      If (Buttons and MouseLeftbutton)<>0 then
        Write('Left ');
      If (Buttons and MouseRightbutton)<>0 then
        Write('Right ');
      If (Buttons and MouseMiddlebutton)<>0 then
        Write('Middle ');
      Write('Button(s)');
      Writeln(' at ',X,',',Y,'.');
      end;
  Until (Event.Buttons=MouseRightButton) and
        (Event.Action=MouseActionDown);
  DoneMouse;
end.
