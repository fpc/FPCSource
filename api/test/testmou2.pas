program MouseTest;

uses
 Crt, Mouse;

var
 Event: TMouseEvent;

begin
 while KeyPressed do ReadKey;
 WriteLn ('Mouse will be shown after any key');
 ReadKey;
 while KeyPressed do ReadKey;
 WriteLn ('Now generate mouse events or press any key to continue');
 InitMouse;
 while not (KeyPressed) do
 begin
  repeat until (KeyPressed) or PollMouseEvent (Event);
  if not (KeyPressed) then
  begin
   GetMouseEvent (Event);
   HideMouse;
   Write ('Buttons: ', Event.Buttons, ', X: ', Event.X, ', Y: ', Event.Y,
                                                                 ', action: ');
   case Event.Action of
    0: WriteLn ('nothing');
    MouseActionDown: WriteLn ('down');
    MouseActionUp: WriteLn ('up');
    MouseActionMove: WriteLn ('move');
   else
    begin
     WriteLn ('undefined!!!');
     if ReadKey = #0 then ReadKey;
    end;
   end;
   ShowMouse;
  end;
 end;
 HideMouse;
 WriteLn ('Mouse will be hidden after any key');
 while KeyPressed do ReadKey;
 ShowMouse;
 if ReadKey = #0 then ReadKey;
 HideMouse;
 WriteLn ('Program ends after any key');
 if ReadKey = #0 then ReadKey;
 DoneMouse;
end.
