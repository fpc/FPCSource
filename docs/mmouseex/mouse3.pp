{example for GetMouseState, IsLPressed, IsRPressed and IsMPressed}

Uses MsMouse, Crt;

Var X, Y, State: Longint;

Begin
  If MouseFound Then
    Begin
      ClrScr;
      ShowMouse;
      GotoXY(5,24);
      Write('Left button:');
      GotoXY(30,24);
      Write('Right button:');
      GotoXY(55,24);
      Write('Middle button:');
      While KeyPressed do Readkey; {clear keyboard buffer}
      Repeat
         GetMouseState(x, y, State);
         GotoXY(20, 22);
         Write('X: ',x:5,' (column: ',(x div 8):2,')  Y: ',y:5, ' (row: ',(y div 8):2,')');
         GotoXY(18, 24); {left button}
         If (State and LButton) = LButton Then
{or: "If LPressed Then". If you use this function, no call to GetMouseState
 is necessary}
           Write('Down')
         Else
           Write('Up  ');
         GotoXY(44, 24); {right button}
         If (State and RButton) = RButton Then
{or: "If RPressed Then"}
           Write('Down')
         Else
           Write('Up  ');
         GotoXY(70, 24); {middle button}
         If (State and MButton) = MButton Then
{or: "If MPressed Then"}
           Write('Down')
         Else
           Write('Up  ')
      Until KeyPressed;
      HideMouse;
      While KeyPressed Do Readkey
    End
End.

