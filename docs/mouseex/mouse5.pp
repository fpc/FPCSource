{example for GetLastButtonPress and GetLastButtonRelease}

Uses MsMouse, Crt;

Var x, y, times: Longint;
    c: Char;

Begin
  If MouseFound Then
    Begin
      ClrScr;
      ShowMouse;
      Writeln('Move the mouse and click the buttons (press escape to quit).');
      Writeln('Press the L-key to see the stats for the left button.');
      Writeln('Press the R-key to see the stats for the right button.');
      Writeln('Press the M-key to see the stats for the middle button.');
      GotoXY(1,19);
      Write('Since the last call to GetLastButtonPress with this button as parameter, the');
      GotoXY(1,22);
      Write('Since the last call to GetLastButtonRelease with this button as parameter, the');
      Repeat
        If Keypressed Then
          Begin
            c := UpCase(Readkey);
            Case c Of
              'L':
                Begin
                  GotoXY(1, 20);
                  ClrEol;
                  times := GetLastButtonPress(LButton, x, y);
                  Write('left button has been pressed ',times,
                          ' times, the last time at (',x,',',y,')');
                  times := GetLastButtonRelease(LButton, x, y);
                  GotoXY(1,23);
                  ClrEol;
                  Write('left button has been released ',times,
                          ' times, the last time at (',x,',',y,')')
                End;
              'R':
                Begin
                  GotoXY(1, 20);
                  ClrEol;
                  times := GetLastButtonPress(RButton, x, y);
                  Writeln('right button has been pressed ',times,
                          ' times, the last time at (',x,',',y,')');
                  times := GetLastButtonRelease(RButton, x, y);
                  GotoXY(1,23);
                  ClrEol;
                  Write('right button has been released ',times,
                          ' times, the last time at (',x,',',y,')')
                End;
              'M':
                Begin
                  GotoXY(1, 20);
                  ClrEol;
                  times := GetLastButtonPress(MButton, x, y);
                  Writeln('middle button has been pressed ',times,
                          ' times, the last time at (',x,',',y,')');
                  times := GetLastButtonRelease(MButton, x, y);
                  GotoXY(1,23);
                  ClrEol;
                  Write('middle button has been released ',times,
                          ' times, the last time at (',x,',',y,')')
                End
            End
          End;
      Until (c = #27); {escape}
      While KeyPressed do ReadKey;
      GotoXY(1,24);
      HideMouse
    End
End.

