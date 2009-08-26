program RotscaleText;
{$L build/font.o}

{$mode objfpc}

uses
  ctypes, nds9;

const
  fontPalLen = 32;
  fontTilesLen = 3072;
  tile_base = 0;
	map_base = 20;

var
  fontTiles: array [0..767] of cushort; cvar; external;
  fontPal: array [0..255] of cushort; cvar; external;

  scaleX, scaleY: cint16;
  scrollX, scrollY: cint16;
  angle: cuint = 0;
  keys: cuint32;

  console: pPrintConsole;
	font: ConsoleFont;
	bg3: cint;

begin
	videoSetMode(0);	

	videoSetModeSub(MODE_5_2D);	
	vramSetBankC(VRAM_C_SUB_BG); 

	console := consoleInit(nil, 3, BgType_ExRotation, BgSize_ER_256x256, map_base, tile_base, false, false);

	font.gfx := pcuint16(fontTiles);
	font.pal := pcuint16(fontPal);
	font.numChars := 95;
	font.numColors :=  fontPalLen div 2;
	font.bpp := 8;
	font.asciiOffset := 32;
	font.convertSingleColor := false;

	consoleSetFont(console, @font);

	bg3 := console^.bgId;

	printf('Custom Font Demo'#10);
	printf('   by Poffy'#10);
	printf('modified by WinterMute and Dovoto'#10);
	printf('for libnds examples'#10);

	
	angle := 0;
  scrollX := 0;
  scrollY := 0;
  scaleX := intToFixed(1,8);
  scaleY := intToFixed(1,8);

	while true do
	begin
		scanKeys();
		keys := keysHeld();

		if ( keys and KEY_L ) <> 0 then angle := angle + 64;
		if ( keys and KEY_R )  <> 0 then angle := angle - 64;

		if ( keys and KEY_LEFT )  <> 0 then scrollX := scrollX + 1;
		if ( keys and KEY_RIGHT )  <> 0 then scrollX := scrollX - 1;
		if ( keys and KEY_UP )  <> 0 then scrollY := scrollY + 1;
		if ( keys and KEY_DOWN )  <> 0 then scrollY := scrollY - 1;

		if ( keys and KEY_A )  <> 0 then scaleX := scaleX + 1;
		if ( keys and KEY_B )  <> 0 then scaleX := scaleX - 1;

		if( keys and KEY_X )  <> 0 then scaleY := scaleY + 1;
		if( keys and KEY_Y )  <> 0 then scaleY := scaleY - 1;

		swiWaitForVBlank();


		bgSetRotateScale(bg3, angle, scaleX, scaleY);
		bgSetScroll(bg3, scrollX, scrollY);
		bgUpdate();
	end;

end.
