Program Example6;

{ Program to demonstrate the PollMouseEvent function. }

Uses mouse;

Procedure DumpEvent(Const Event : TMouseEvent);

begin
  With event do
    begin
    Write('Action:');
    Case Action of
      MouseActionDown : Write('Mouse down.');
      MouseActionUp   : Write('Mouse up.');
      MouseActionMove : Write('Mouse move.');
    end;
    Write('Button state : ');
    If (Buttons and MouseLeftbutton)<>0 then
      Write('Left ');
    If (Buttons and MouseRightbutton)<>0 then
      Write('Right ');
    If (Buttons and MouseMiddlebutton)<>0 then
      Write('Middle ');
    Write('Button(s)');
    Writeln(' at ',X,',',Y,'.');
    end;
end;

Var
  Event : TMouseEvent;

begin
  InitMouse;
  Writeln('Play with mouse. Press right mouse button to end.');
  Repeat
    If PollMouseEvent(Event)Then
      begin
      DumpEvent(Event);
      GetMouseEvent(Event);
      end
    else
      Write('.');
  Until (Event.Buttons=MouseRightButton) and
        (Event.Action=MouseActionDown);
  DoneMouse;
end.
