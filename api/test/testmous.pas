program testmouse;
uses mouse;

var
  quit : boolean;
  event : TMouseEvent;
begin
  initmouse;
  repeat
    GetMouseEvent(event);
    writeln('action : ',event.action,' (',event.x,',',event.y,') [',event.buttons,']');
    if event.buttons and MouseRightButton<>0 then
      quit:=true;
  until quit;
  donemouse;
end.
