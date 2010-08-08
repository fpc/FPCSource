program hello_world;

{$mode objfpc}

uses
  ctypes, nds9;

var
  frame: integer;
	touchXY: touchPosition;

procedure Vblank();
begin
  inc(frame);
end;

	
begin
  irqSet(IRQ_VBLANK, @Vblank);

	consoleDemoInit();

	iprintf('      Hello DS dev''rs'#10);
	iprintf(#27'[32m' + 'www.devkitpro.org'#10);
	iprintf(#27'[32;1m' + 'www.drunkencoders.com'#27'[39m');
 
	while true do
	begin
    swiWaitForVBlank();
    touchRead(touchXY);
    
    // print at using ansi escape sequence \x1b[line;columnH 
    iprintf(#27'[10;0H' + 'Frame = %d', frame);
    iprintf(#27'[16;0H' + 'Touch x = %04X, %04X'#10, touchXY.rawx, touchXY.px);
    iprintf('Touch y = %04X, %04X'#10, touchXY.rawy, touchXY.py);		
	end;

end.
