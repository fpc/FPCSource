program DoubleBuffer;

{$mode objfpc}

uses
  ctypes, nds9;

var
  backBuffer: pcuint16;
  colorMask: cuint16;
  iy, ix: integer;
	bg: cuint16;

begin
  randomize;
	//set the mode for 2 text layers and two extended background layers
	videoSetMode(MODE_5_2D); 

	//set the first two banks as background memory and the third as sub background memory
	//D is not used..if you need a bigger background then you will need to map
	//more vram banks consecutivly (VRAM A-D are all 0x20000 bytes in size)
	vramSetPrimaryBanks(VRAM_A_MAIN_BG_0x06000000, VRAM_B_MAIN_BG_0x06020000, 
		VRAM_C_SUB_BG, VRAM_D_LCD);

	consoleDemoInit();

  iprintf(#10#10#9 + 'Hello DS devers' + #10);
  iprintf(#9 + 'www.drunkencoders.com' + #10);
  iprintf(#9 + 'double buffer demo');


	bg := bgInit(3, BgType_Bmp16, BgSize_B16_256x256, 0,0);

	colorMask := $1F;

  backBuffer := pcuint16(bgGetGfxPtr(bg)) + 256*256;

	while true do
	begin
		//draw a box (60,60,196,136)
		for iy := 60 to 196 - 60 - 1 do
			for ix := 60 to 256 - 60 - 1 do
				backBuffer[iy * 256 + ix] := random(colorMask) or BIT(15);

		swiWaitForVBlank();
		scanKeys();
		if (keysDown() and KEY_START) <> 0 then 
      exit;
		//swap the back buffer to the current buffer
		backBuffer := pcuint16(bgGetGfxPtr(bg));

		//swap the current buffer by changing the base. Each base
		//represents 16KB of offset and each screen is 256x256x2 (128KB)
		//this requires a map base seperation of 8 (could get away with smaller
		//as the screen is really only showing 256x192 (96KB or map base 6)
		if (bgGetMapBase(bg) = 8) then
		  bgSetMapBase(bg, 0)
		else
			bgSetMapBase(bg, 8);

		colorMask := colorMask xor $3FF;
	end;
end.
