{example for ShowMouse and HideMouse}

Uses MsMouse;

Begin
  ClrScr;
  If MouseFound Then
    Begin
      Writeln('Now you can see the mouse... (press enter to continue)');
      ShowMouse;
      Readln;
      HideMouse;
      Writeln('And now you can''t... (press enter to quit)');
      Readln
    End
End.

