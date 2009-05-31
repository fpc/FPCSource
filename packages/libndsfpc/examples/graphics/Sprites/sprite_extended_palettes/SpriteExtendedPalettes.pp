program SpriteExtendedPalettes;

uses
  ctypes, nds9;

var
	i: integer;
	touch: touchPosition;
	gfx1, gfx2: pcuint16;

begin
	videoSetMode(MODE_0_2D);

	vramSetBankA(VRAM_A_MAIN_SPRITE);

	oamInit(oamMain, SpriteMapping_1D_32, true);

	gfx1 := oamAllocateGfx(oamMain, SpriteSize_16x16, SpriteColorFormat_256Color);
	gfx2 := oamAllocateGfx(oamMain, SpriteSize_16x16, SpriteColorFormat_256Color);

	//------------------------------------------------------------------
	// notice both sprites are filled with color 1
	//------------------------------------------------------------------
	for i := 0 to (16 * 16 div 2) - 1 do
	begin
		gfx1[i] := 1 or (1 shl 8);
		gfx2[i] := 1 or (1 shl 8);
	end;

	//------------------------------------------------------------------
	// unlock vram (cannot write to vram while mapped as palette memory)
	//------------------------------------------------------------------
	vramSetBankF(VRAM_F_LCD);

	VRAM_F_EXT_PALETTE[0][1] := RGB15(31,0,0);
	VRAM_F_EXT_PALETTE[1][1] := RGB15(0,31,0);

	// set vram to ex palette
	vramSetBankF(VRAM_F_SPRITE_EXT_PALETTE);

	while true do
	begin
		scanKeys();

		if (keysHeld() and KEY_TOUCH) <> 0 then
			touchRead(touch);

		oamSet(oamMain, //main graphics engine context
			0,           //oam index (0 to 127)  
			touch.px, touch.py,   //x and y pixle location of the sprite
			0,                    //priority, lower renders last (on top)
			0,					  //this is the palette index if multiple palettes or the alpha value if bmp sprite	
			SpriteSize_16x16,     
			SpriteColorFormat_256Color, 
			gfx1,                  //pointer to the loaded graphics
			-1,                  //sprite rotation data  
			false,               //double the size when rotating?
			false,			//hide the sprite?
			false, false, //vflip, hflip
			false	//apply mosaic
			);              
		
		
		oamSet(oamMain,
			1, 
			SCREEN_WIDTH - touch.px, 
			SCREEN_HEIGHT - touch.py, 
			0, 
			1,  //use second palette
			SpriteSize_16x16, 
			SpriteColorFormat_256Color, 
			gfx2, 
			-1, 
			false, 
			false,			
			false, false, 
			false	
			);              
	
		swiWaitForVBlank();

		
		oamUpdate(oamMain);
  end;
end.
