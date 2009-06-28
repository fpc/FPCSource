unit advanced;

interface

uses
  ctypes, nds9, scrolling, RotBackgrounds, TextBackgrounds, Multilayer;

procedure advMosaic();
procedure advRotating();
procedure advScaling();
procedure advExtendedPalette();
procedure advMultipleLayers();


implementation

procedure advMosaic();
var
  bg: cint;
	keys: integer;
	mosaic_x: integer;
	mosaic_y: integer;
begin
	videoSetMode(MODE_0_2D);
	vramSetBankA(VRAM_A_MAIN_BG);
	
  bg := bgInit(0, BgType_Text8bpp, BgSize_T_256x256, 0,1);
	
	dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
	dmaCopy(@Layer256x256Map, bgGetMapPtr(bg),  Layer256x256MapLen);
	dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));
	
	bgMosaicEnable(bg);
	
	
	while ((keys and KEY_B) = 0) do
  begin
		scanKeys();
		keys := keysDown();
		
		if (keys and KEY_UP) <> 0 then mosaic_y := mosaic_y - 1;
		if (keys and KEY_DOWN) <> 0 then mosaic_y := mosaic_y + 1;
	
		if (keys and KEY_LEFT) <> 0 then mosaic_x := mosaic_x - 1;
		if (keys and KEY_RIGHT) <> 0 then mosaic_x := mosaic_x + 1;

		if (mosaic_x > 15) then mosaic_x := 15;
		if (mosaic_x < 0) then mosaic_x := 0;
		
		if (mosaic_y > 15) then mosaic_y := 15;
		if (mosaic_y < 0) then mosaic_y := 0;

		
		swiWaitForVBlank();
		
		bgSetMosaic(mosaic_x, mosaic_y);
		
		consoleClear();
		iprintf('Press B to exit'#10);
		iprintf('DX: %d  DY: %d', mosaic_x, mosaic_y);
		
	end;
end;

procedure advRotating();
var
  bg: cint;
  keys: integer;
  angle: integer;
  mosaic_x: integer;
  mosaic_y: integer;
  center_x: integer;
  center_y: integer;
begin
	videoSetMode(MODE_5_2D);
	vramSetBankA(VRAM_A_MAIN_BG);
	
  bg := bgInit(3, BgType_ExRotation, BgSize_ER_256x256, 0,1);
	
	dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
	dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));
	dmaCopy(@Layer256x256Map, bgGetMapPtr(bg),  Layer256x256MapLen);
	
	bgMosaicEnable(bg);
	
	while ((keys and KEY_B) = 0) do
	begin
		scanKeys();
		keys := keysHeld();
		
		if (keys and KEY_UP) <> 0 then center_y := center_y - 1;
		if (keys and KEY_DOWN) <> 0 then center_y := center_y + 1;
	
		if (keys and KEY_LEFT) <> 0 then center_x := center_x - 1;
		if (keys and KEY_RIGHT) <> 0 then center_x := center_x + 1;

		if (keys and KEY_L) <> 0 then angle := angle - 1;
		if (keys and KEY_R) <> 0 then angle := angle + 1;

		if (center_x > 256) then center_x := 256;
		if (center_x < 0) then center_x := 0;
		
		if (center_y > 192) then center_y := 192;
		if (center_y < 0) then center_y := 0;

		
		swiWaitForVBlank();
		
		bgSetRotate(bg, angle);
		bgSetScroll(bg, center_x, center_y);
		bgSetCenter(bg, center_x, center_y);
		bgUpdate();

		consoleClear();
		iprintf('Press B to exit.'#10);
		iprintf('Angle: %d '#10'center X: %d  center Y: %d', angle, center_x, center_y);
		
	end;
end;

procedure advScaling();
var
  bg: cint;
	keys, scale_x, scale_y: integer;
  
begin
	videoSetMode(MODE_5_2D);
	vramSetBankA(VRAM_A_MAIN_BG);
	
	bg := bgInit(3, BgType_ExRotation, BgSize_ER_256x256, 0,1);
	
	dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
	dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));
	dmaCopy(@Layer256x256Map, bgGetMapPtr(bg),  Layer256x256MapLen);
	
	bgMosaicEnable(bg);
	
	scale_x := 1 shl 8;
	scale_y := 1 shl 8;
	
	while ((keys and KEY_B) = 0) do
	begin
		scanKeys();
		keys := keysHeld();
		
		if (keys and KEY_UP) <> 0 then scale_y := scale_y - 1;
		if (keys and KEY_DOWN) <> 0 then scale_y := scale_y + 1;
	
		if (keys and KEY_LEFT) <> 0 then scale_x := scale_x + 1;
		if (keys and KEY_RIGHT) <> 0 then scale_x := scale_x - 1;

    swiWaitForVBlank();
		
		bgSetScale(bg, scale_x , scale_y );
		
		bgUpdate();

		consoleClear();
		iprintf('Press B to exit.'#10);
		iprintf('scale X: %d  scale Y: %d', scale_x, scale_y);
		
	end;
end;

procedure advExtendedPalette();
begin

end;

procedure advMultipleLayers();
var
  bg1, bg2, bg3: cint;
	keys: integer;
	bg1_hidden, bg2_hidden, bg3_hidden: boolean;

begin
	videoSetMode(MODE_5_2D);
	vramSetBankA(VRAM_A_MAIN_BG);
	
	//initialize the backgrounds
	bg1 := bgInit(0, BgType_Text8bpp, BgSize_ER_256x256, 0,1);
	bg2 := bgInit(1, BgType_Text8bpp, BgSize_ER_256x256, 1,1);
	bg3 := bgInit(2, BgType_ExRotation, BgSize_ER_256x256, 2,1);
	
	//make sure the floor is on the bottom (by default hardware layer 0 will be rendered last)
	bgSetPriority(bg1, 3);
	bgSetPriority(bg2, 2);
	bgSetPriority(bg3, 1);
	
	//they all share tiles and a palette
	dmaCopy(@MultilayerTiles, bgGetGfxPtr(bg1), sizeof(MultilayerTiles));
	dmaCopy(@MultilayerPal, BG_PALETTE, sizeof(MultilayerPal));

	//all we need to do is copy in the maps
	dmaCopy(@Layer_1Map, bgGetMapPtr(bg1),  Layer_1MapLen);
	dmaCopy(@Layer_2Map, bgGetMapPtr(bg2),  Layer_2MapLen);
	dmaCopy(@Layer_3Map, bgGetMapPtr(bg3),  Layer_3MapLen);
	
  keys := 0;
	bg1_hidden := false;
	bg2_hidden := false;
	bg3_hidden := false;

	while ((keys and KEY_B) = 0) do
	begin
		scanKeys();
		
		keys := keysDown();
		
		if (keys and KEY_UP) <> 0 then bg1_hidden := not bg1_hidden;
		if (keys and KEY_DOWN) <> 0 then bg2_hidden := not bg2_hidden;
		if (keys and KEY_LEFT) <> 0 then bg3_hidden :=  not bg3_hidden;
		
		swiWaitForVBlank();
		
		if bg1_hidden then bgHide(bg1) else bgShow(bg1);
		if bg2_hidden then bgHide(bg2) else bgShow(bg2);
		if bg3_hidden then bgHide(bg3) else bgShow(bg3);
		
		consoleClear();

		iprintf('Press UP DOWN LEFT to toggle the layers'#10#10);
		if bg1_hidden then
		  iprintf('Floor (UP): %s'#10, 'hidden')
    else 
      iprintf('Floor (UP): %s'#10, 'displayed');
      
		if bg2_hidden then
		  iprintf('Walls (DOWN): %s'#10, 'hidden') 
    else 
      iprintf('Walls (DOWN): %s'#10, 'displayed');
		
    if bg3_hidden then
      iprintf('Decorations (LEFT): %s'#10, 'hidden') 
    else 
      iprintf('Decorations (LEFT): %s'#10, 'displayed');
	end;
end;

end.
