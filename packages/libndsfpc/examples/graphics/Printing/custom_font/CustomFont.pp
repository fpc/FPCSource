program custom_font;
{$L build/font.o}

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

  console: pPrintConsole;
	font: ConsoleFont;


begin
	videoSetModeSub(MODE_0_2D);	
	vramSetBankC(VRAM_C_SUB_BG); 

	console := consoleInit(nil, 0, BgType_Text4bpp, BgSize_T_256x256, map_base, tile_base, false, false);

	font.gfx := pcuint16(fontTiles);
	font.pal := pcuint16(fontPal);
	font.numChars := 95;
	font.numColors :=  fontPalLen div 2;
	font.bpp := 4;
	font.asciiOffset := 32;
	font.convertSingleColor := false;

	consoleSetFont(console, @font);
	
	printf('Custom Font Demo'#10);
	printf('   by Poffy'#10);
	printf('modified by WinterMute'#10);
	printf('for libnds examples'#10);

	while true do
		swiWaitForVBlank();
end.
