{example for SetMouseHideWindow}

{warning: when the mouse is moved into the specified region, it is turned off
 until you call ShowMouse again. However, when you've called ShowMouse,
 you'll have to call SetMouseHideWindow again to redefine the hide window...
 It's not our fault, that's the way it's implemented in the mouse driver.

 Below you can find an example of how to define a "permanent" hide region
 with the cursor showing up again when you move it out of the region

 Note: the mouse functions are zero-based, GotoXY is 1-based}

Uses MsMouse, Crt;

Var x, y, buttons: Longint;
    MouseOn: Boolean;

Begin
  If MouseFound Then
    Begin
      ClrScr;
      For y := 1 to 25 Do
        Begin
          GotoXY(20,y);
          Write('|');
          GotoXY(60,y);
          Write('|');
        End;
      MouseOn := true;
      GotoXY(30, 24);
      Writeln('Press any key to quit');
      ShowMouse;
      SetMousePos(1,1);
      While KeyPressed Do Readkey;
      Repeat
        GetMouseState(x,y,buttons);
        If Not(MouseOn) And
          ((x <= 19*8) or (x >= 59*8)) Then
          Begin
            ShowMouse;
            MouseOn := true
          End;
        If MouseOn And (x > 19*8) And (x<59*8) Then
          Begin
            SetMouseHideWindow(20*8,0,60*8,25*8);
            MouseOn := false
          End;
      Until KeyPressed;
      While KeyPressed Do Readkey;
      HideMouse
    End
End.


