Program Example1;

{ Program to demonstrate the DetectMouse function. }

Uses mouse;

Var
  Buttons : Byte;

begin
  InitMouse;
  Buttons:=DetectMouse;
  If Buttons=0 then
    Writeln('No mouse present.')
  else
    Writeln('Found mouse with ',Buttons,' buttons.');
  DoneMouse;
end.
