Program Example4;

{ Program to demonstrate the GetMouseX,GetMouseY functions. }

Uses mouse;

Var
  X,Y : Word;

begin
  InitMouse;
  Writeln('Move mouse cursor to square 10,10 to end');
  Repeat
    X:=GetMouseX;
    Y:=GetMouseY;
    Writeln('X,Y= (',X,',',Y,')');
  Until (X=9) and (Y=9);
  DoneMouse;
end.
