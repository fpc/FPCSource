program hello_world;

{$mode objfpc}

uses
  ctypes, nds9;

var
  frame: integer;
	touchXY: touchPosition;

  REG_DIVPCNT       : pcuint8 = pointer($4000280);
  DIV_NUMER   : pcuint32 = pointer($4000290);
  DIV_DENOM : pcuint32 = pointer($4000298);
  DIV_RESULT : pcuint32 = pointer($40002A0);
  DIVREM_RESULT : pcuint32 = pointer($40002A8);

procedure Vblank();
begin
  inc(frame);
end;

	
begin
  irqSet(IRQ_VBLANK, @Vblank);

	consoleDemoInit();
  iprintf('%i div %i'#10, DIV_NUMER^, DIV_DENOM^ );
  iprintf('%i, %i'#10, DIV_RESULT^, DIVREM_RESULT^);
  
  DIV_NUMER^ := 5;
  DIV_DENOM^ := 0;  
  iprintf('%i div %i'#10, DIV_NUMER^, DIV_DENOM^ );

  iprintf('%i'#10,DIV_RESULT^ );
  iprintf('%i'#10,DIVREM_RESULT^ );
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
