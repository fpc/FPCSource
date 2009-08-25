program Simple;

{$mode objfpc}

uses
  ctypes, nds9;

var
	i: integer;
	touch: touchPosition;
	gfx, gfxSub: pcuint16;

begin
	videoSetMode(MODE_0_2D);
	videoSetModeSub(MODE_0_2D);

	vramSetBankA(VRAM_A_MAIN_SPRITE);
	vramSetBankD(VRAM_D_SUB_SPRITE);

	oamInit(oamMain, SpriteMapping_1D_32, false);
	oamInit(oamSub, SpriteMapping_1D_32, false);

	gfx := oamAllocateGfx(oamMain, SpriteSize_16x16, SpriteColorFormat_256Color);
	gfxSub := oamAllocateGfx(oamSub, SpriteSize_16x16, SpriteColorFormat_256Color);

	for i := 0 to (16 * 16 div 2) - 1 do
	begin
		gfx[i] := 1 or (1 shl 8);
		gfxSub[i] := 1 or (1 shl 8);
	end;

	SPRITE_PALETTE[1] := RGB15(31,0,0);
	SPRITE_PALETTE_SUB[1] := RGB15(0,31,0);

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
			gfx,                  //pointer to the loaded graphics
			-1,                  //sprite rotation data
			false,               //double the size when rotating?
			false,			//hide the sprite?
			false, false, //vflip, hflip
			false	//apply mosaic
			);


		oamSet(oamSub,
			0,
			touch.px,
			touch.py,
			0,
			0,
			SpriteSize_16x16,
			SpriteColorFormat_256Color,
			gfxSub,
			-1,
			false,
			false,
			false, false,
			false
			);

		swiWaitForVBlank();


		oamUpdate(oamMain);
		oamUpdate(oamSub);
	end;

end.