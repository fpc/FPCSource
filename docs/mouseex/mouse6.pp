{example for SetMouseXRange, SetMouseYRange and SetMouseWindow}

Uses MsMouse, Crt;

Begin
  If MouseFound Then
    Begin
      SetMouseXRange(20*8,50*8);  {charracter width and height = 8 pixels}
      SetMouseYRange(10*8,15*8);

{the two lines of code have exactly the same effect as
 SetMouseWindow(20*8,10*8,50*8,15*8)}

      Writeln('Press any key to quit.');
      ShowMouse;
      While KeyPressed Do ReadKey;
      Readkey;
      While KeyPressed Do ReadKey;
      HideMouse
    End
End.

