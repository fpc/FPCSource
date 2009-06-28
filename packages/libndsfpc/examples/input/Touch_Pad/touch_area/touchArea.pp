program touchArea;

uses
  nds9;

const
	//my experimental value for pen vs finger (higher value == lower area)
  threshold = 400;

var
	touch: touchPosition;
	area: integer = 0;

begin
	consoleDemoInit(); 

  while true do
  begin
    scanKeys();
    
    touchRead(touch);
    
    area := (touch.px * touch.z2) div (touch.z1 - touch.px);
    
    iprintf(#27'[10;0H' + 'Touch x = %04i, %04i'#10, touch.rawx, touch.px);
    
    iprintf('Touch y = %04i, %04i'#10, touch.rawy, touch.py);
    
    iprintf('Touch Area (pressure) %04i'#10, area);
    
    if (keysHeld() and KEY_TOUCH) <> 0 then
      if area > threshold then
        iprintf('Last touched by: pen')
      else
        iprintf('Last touched by: finger');

    swiWaitForVBlank();
  end;

end.
