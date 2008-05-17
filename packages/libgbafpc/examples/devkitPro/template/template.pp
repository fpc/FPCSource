program template;

uses
  ctypes, gba;
  


var
  frame: integer = 0;
  zbuffer: array [0..239, 0..159] of u8; cvar; external;  //EWRAM_BSS;


procedure Vblank();
begin
	frame := frame + 1;
end;

begin

	// the vblank interrupt must be enabled for VBlankIntrWait() to work
	// since the default dispatcher handles the bios flags no vblank handler
	// is required
	irqInit();
	irqSet(IRQ_VBLANK, @Vblank);
	irqEnable(IRQ_VBLANK);

	consoleInit(0, 4, 0, nil, 0, 15);

	BG_COLORS[0] := RGB8(58,110,165);
	BG_COLORS[241] := RGB5(31,31,31);

	SetMode(MODE_0 or BG0_ON);

	// ansi escape sequence to set print co-ordinates
	// /x1b[line;columnH
	iprintf(#27'[10;10H' + 'Hello World!'#10);
	iprintf('%x', getmem(200));
	while true do
	begin
		VBlankIntrWait();
		scanKeys();
  end;
end.


