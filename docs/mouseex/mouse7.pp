{example for SetMouseShape}

{warning: no error checking is performed on the input}

{the Ascii value you enter is XOR'ed with the Ascii value of the character
 on the screen over which you move the cursor. To get a "transparent" cursor,
 use the Ascii value 0}

Uses MsMouse, Crt;

Var ascii, fc, bc: Byte;
    x,y: Longint;

Begin
  If MouseFound Then
    Begin
      ClrScr;
      Writeln('Press any mouse button to quit after you''ve entered a sequence of numbers.');
      Writeln;
      Writeln('ASCII value of mouse cursor:');
      Writeln('Forground color:');
      Writeln('Background color:');
      ShowMouse;
      Repeat
        GotoXY(30,3);
        ClrEol;
        Readln(ascii);
        GotoXY(18,4);
        ClrEol;
        Readln(fc);
        GotoXY(19,5);
        ClrEol;
        Readln(bc);
        SetMouseShape(fc, bc, ascii)
      Until (GetLastButtonPress(LButton,x,y) <> 0) Or
            (GetLastButtonPress(RButton,x,y) <> 0) Or
            (GetLastButtonPress(MButton,x,y) <> 0);
      HideMouse
    End;
End.

