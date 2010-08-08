program printBothScreens;

{$mode objfpc}

uses
  ctypes, nds9;

var
	touch: touchPosition;
	topScreen, bottomScreen: PrintConsole;

begin
	videoSetMode(MODE_0_2D);
	videoSetModeSub(MODE_0_2D);

	vramSetBankA(VRAM_A_MAIN_BG);
	vramSetBankC(VRAM_C_SUB_BG);

	consoleInit(@topScreen, 3,BgType_Text4bpp, BgSize_T_256x256, 31, 0, true, true);
	consoleInit(@bottomScreen, 3,BgType_Text4bpp, BgSize_T_256x256, 31, 0, false, true);

	consoleSelect(@topScreen);
	
	iprintf(#10#10#9'Hello DS dev''rs'#10);
	iprintf(#9'www.drunkencoders.com'#10);
	iprintf(#9'www.devkitpro.org');

  consoleSelect(@bottomScreen);

	while true do
	begin
		touchRead(touch);
		iprintf(#27'[10;0H' + 'Touch x = %04i, %04i'#10, touch.rawx, touch.px);
		iprintf('Touch y = %04i, %04i'#10, touch.rawy, touch.py);

		swiWaitForVBlank();
	end;

end.

