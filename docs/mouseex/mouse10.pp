Uses MsMouse, Crt;

Var hor, vert: Longint;
    x, y: Longint;

Begin
  If MouseFound Then
    Begin
      ClrScr;
      Writeln('Click any button to quit after you''ve entered a sequence of numbers.');
      Writeln;
      Writeln('Horizontal mickey''s per pixel:');
      Writeln('Vertical mickey''s per pixel:');
      ShowMouse;
      Repeat
        GotoXY(32,3);
        ClrEol;
        Readln(hor);
        GotoXY(30,4);
        ClrEol;
        Readln(vert);
        SetMouseSpeed(hor, vert);
      Until (GetLastButtonPress(LButton,x,y) <> 0) Or
            (GetLastButtonPress(RButton,x,y) <> 0) Or
            (GetLastButtonPress(MButton,x,y) <> 0);
    End
End.

