{example for SetMouseAscii}

{warning: no error checking is performed on the input}

Uses MsMouse, Crt;

Var ascii: Byte;
    x,y: Longint;

Begin
  If MouseFound Then
    Begin
      ClrScr;
      Writeln('Press any mouse button to quit after you''ve entered an Ascii value.');
      Writeln;
      Writeln('ASCII value of mouse cursor:');
      ShowMouse;
      Repeat
        GotoXY(30,3);
        ClrEol;
        Readln(ascii);
        SetMouseAscii(ascii)
      Until (GetLastButtonPress(LButton,x,y) <> 0) Or
            (GetLastButtonPress(RButton,x,y) <> 0) Or
            (GetLastButtonPress(MButton,x,y) <> 0);
      HideMouse
    End;
End.

