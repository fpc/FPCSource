{example for SetMousePos}

Uses MsMouse, Crt;

Begin
  If MouseFound Then
    Begin
      ShowMouse;
      While KeyPressed do ReadKey;
      Repeat
        SetMousePos(Random(80*8), Random(25*8));
        delay(100);
      Until Keypressed;
      HideMouse;
      While KeyPressed do ReadKey;
    End;
End.

