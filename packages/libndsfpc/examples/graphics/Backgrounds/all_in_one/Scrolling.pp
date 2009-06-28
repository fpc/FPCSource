unit scrolling;

interface

uses
  ctypes, nds9, RotBackgrounds, TextBackgrounds, Multilayer;

procedure scroll(id, width, height: cint);
procedure scrollText();
procedure scrollRotation();
procedure scrollVertical();
procedure scrollHorizontalText();
procedure scrollHorizontalExRotation();
procedure scroll4wayText();
procedure scroll4wayExRotation();


implementation

//reusable scroll function to allow the user to explore 
//the maps somewhat
procedure scroll(id, width, height: cint);
var
  keys, sx, sy: integer;
begin
   keys := 0;
   sx := 0;
   sy := 0;

   while (keys and KEY_B) = 0 do
   begin
      scanKeys();

      keys := keysHeld();

      if (keys and KEY_UP) <> 0 then dec(sy);
      if (keys and KEY_DOWN) <> 0 then  inc(sy);
      if (keys and KEY_LEFT) <> 0 then  dec(sx);
      if (keys and KEY_RIGHT) <> 0 then  inc(sx);

      if (sx < 0) then sx := 0;
      if (sx >= width - 256) then sx := width - 1 - 256;
      if (sy < 0) then sy := 0;
      if (sy >= height - 192) then sy := height - 1 - 192;

      swiWaitForVBlank();

      bgSetScroll(id, sx, sy);

	  bgUpdate();

      consoleClear();
      iprintf('Scroll x: %d Scroll y: %d'#10, sx, sy);
      iprintf('Press ''B'' to exit');
   end;
end;


procedure scrollText();
var
  keys, sx, sy, width, height: integer;
  bg: cint;
begin
  keys := 0;
  sx := 0;
  sy := 0;
  width := 256;
  height := 512;

  //set up a simple text background
  videoSetMode(MODE_0_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(0, BgType_Text8bpp, BgSize_T_256x512, 0,1);

  dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
  dmaCopy(@Layer256x512Map, bgGetMapPtr(bg),  Layer256x512MapLen);
  dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

   while (keys and KEY_B) = 0 do
   begin
      scanKeys();

      keys := keysHeld();

      if (keys and KEY_UP) <> 0 then dec(sy);
      if (keys and KEY_DOWN) <> 0 then  inc(sy);
      if (keys and KEY_LEFT) <> 0 then  dec(sx);
      if (keys and KEY_RIGHT) <> 0 then  inc(sx);

      //clamp the scroll value to the map width and height
      if (sx < 0) then sx := 0;
      if (sx >= width - 256) then sx := width - 1 - 256;
      if (sy < 0) then sy := 0;
      if (sy >= height - 192) then sy := height - 1 - 192;

      swiWaitForVBlank();

      //normally would call bgSetScroll(id, sx, sy) here
      //but to demonstrate the hardware difference between 
      //scrolling rotation and text backgrounds we will use
      //direct register access

      REG_BG0HOFS^ := sx;
      REG_BG0VOFS^ := sy;

      consoleClear();
      iprintf('Scroll x: %d Scroll y: %d'#10, sx, sy);
      iprintf('Press ''B'' to exit');
   end;
end;

procedure scrollRotation();
var
  keys, sx, sy, width, height: integer;
  bg: cint;
begin
  keys := 0;
  sx := 0;
  sy := 0;
  width := 512;
  height := 512;

   videoSetMode(MODE_5_2D);
   vramSetBankA(VRAM_A_MAIN_BG);

   bg := bgInit(3, BgType_ExRotation, BgSize_ER_512x512, 0,1);

   dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
   dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));
   dmaCopy(@Layer512x512Map, bgGetMapPtr(bg),  Layer512x512MapLen);

   while (keys and KEY_B)=0 do
   begin
      scanKeys();

      keys := keysHeld();

      if (keys and KEY_UP) <> 0 then dec(sy);
      if (keys and KEY_DOWN) <> 0 then  inc(sy);
      if (keys and KEY_LEFT) <> 0 then  dec(sx);
      if (keys and KEY_RIGHT) <> 0 then  inc(sx);

      //clamp the scroll value to the map width and height
      if (sx < 0) then sx := 0;
      if (sx >= width - 256) then sx := width - 1 - 256;
      if (sy < 0) then sy := 0;
      if (sy >= height - 192) then sy := height - 1 - 192;

      swiWaitForVBlank();

      //normally would call bgSetScroll(id, sx, sy) here
      //but to demonstrate the hardware difference between 
      //scrolling rotation and text backgrounds we will use
      //direct register access

      REG_BG3X^ := sx shl 8;
      REG_BG3Y^ := sy shl 8;

      consoleClear();
      iprintf('Scroll x: %d Scroll y: %d'#10, sx, sy);
      iprintf('Press ''B'' to exit');
   end;
end;



procedure scrollVertical();
var
  scroll_y: integer = 0;
  map: pcuint16;
  bg: cint;
  keys: integer = 0;
  offset: integer = 0;
begin
   videoSetMode(MODE_0_2D);
   vramSetBankA(VRAM_A_MAIN_BG);

   scroll_y := 0;

   bg := bgInit(0, BgType_Text8bpp, BgSize_T_256x256, 0,1);

   dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
   dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

   map := pcuint16(bgGetMapPtr(bg));

   dmaCopy(@Layer256x512Map, map, 32*32*2);

   keys := 0;
   offset := 0;

   while (keys and KEY_B)= 0 do
   begin
      scanKeys();

      keys := keysHeld();

      swiWaitForVBlank();


      if (keys and KEY_UP) <> 0 then
      begin
         offset := scroll_y div 8 - 1;

         dmaCopy(@Layer256x512Map[(offset and 63) * 32], @map[(offset and 31) * 32], 32 * 2);

         dec(scroll_y);
      end;
      
      if (keys and KEY_DOWN) <> 0 then
      begin
         offset := scroll_y div 8 + 24;

         dmaCopy(@Layer256x512Map[(offset and 63) * 32], @map[(offset and 31) * 32], 32 * 2);

         inc(scroll_y);
      end;

      bgSetScroll(bg, 0, scroll_y);
	  bgUpdate();
   end;

end;


procedure scrollHorizontalText();
var
  scroll_x: integer = 0;
  bg: cint;
  map: pcuint16;
  iy: integer;
  keys: integer = 0;
  layerOffset: integer = 0;
  mapOffset: integer = 0;
begin
   videoSetMode(MODE_5_2D);
   vramSetBankA(VRAM_A_MAIN_BG);

   bg := bgInit(0, BgType_Text8bpp, BgSize_T_512x256, 0,1);

   dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
   dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

   map := pcuint16(bgGetMapPtr(bg));


   for iy := 0 to 23 do
      dmaCopy(@Layer512x256Map[iy * 64], @map[iy * 32], 32*2);

   while (keys and KEY_B) = 0 do
   begin
      scanKeys();

      keys := keysHeld();

      swiWaitForVBlank();

      if (keys and KEY_LEFT) <> 0 then
      begin
         mapOffset := scroll_x div 8 - 1;
         layerOffset := (mapOffset and 63);

         if(layerOffset >= 32) then layerOffset := layerOffset + (32 * 32 - 32);

         for iy := 0 to 23 do
            map[layerOffset + (iy * 32)] := Layer512x256Map[(mapOffset and 63) + (iy * 64)] ;

         dec(scroll_x);
      end;
      
      if (keys and KEY_RIGHT) <> 0 then
      begin
         mapOffset := scroll_x div 8 + 32;
         layerOffset := (mapOffset and 63);

         if(layerOffset >= 32) then layerOffset := layerOffset + (32 * 32 - 32);

         for iy := 0 to 23 do
            map[layerOffset + (iy * 32)] := Layer512x256Map[(mapOffset and 63) + (iy * 64)] ;

         inc(scroll_x);
      end;


      bgSetScroll(bg, scroll_x, 0);
	  bgUpdate();
   end;

end;

procedure scrollHorizontalExRotation();
var
  scroll_x: integer = 0;
  bg: cint;
  map: pcuint16;
  iy: integer;
  keys: integer = 0;
  offset: integer = 0;
begin
   videoSetMode(MODE_5_2D);
   vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_ExRotation, BgSize_ER_512x512, 0,1);

   dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
   dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

   map := pcuint16(bgGetMapPtr(bg));

   bgSetControlBits(bg, BG_WRAP_ON);

   for iy := 0 to 23 do
      dmaCopy(@Layer512x256Map[iy * 64], @map[iy * 64], 32*2);

   while (keys and KEY_B) = 0 do
   begin
      scanKeys();

      keys := keysHeld();

      swiWaitForVBlank();

      if (keys and KEY_LEFT) <> 0 then
      begin
         offset := scroll_x div 8 - 1;

         for iy := 0 to 23 do
            map[(offset and 63) + (iy * 64)] := Layer512x256Map[(offset and 63) + (iy * 64)] ;

         dec(scroll_x);
      end;
      
      if (keys and KEY_RIGHT) <> 0 then
      begin
         offset := scroll_x div 8 + 32;

         for iy := 0 to 23 do
            map[(offset and 63) + (iy * 64)] := Layer512x256Map[(offset and 63) + (iy * 64)] ;

         inc(scroll_x);
      end;

      bgSetScroll(bg, scroll_x, 0);
	  bgUpdate();
   end;

end;


//scrolls a 1024 by 1024 map on a 512x512 layer
procedure scroll4wayText();
var
  scroll_x: integer = 0;
  scroll_y: integer = 0;
  bg: cint;
  bgTileMap, bgLeftHalf, bgRightHalf: pcuint16;
  ix, iy: integer;
  keys: integer = 0;
  offset_x: integer = 0;
  offset_y: integer = 0;
  movingHorizontal: boolean = false;
  movingVertical: boolean = false;
  bgTemp: pcuint16;
const
  tileWidth   : integer = 8;          //width of a tile in pixels
  mapWidth    : integer = 1024 div 8; //width of the big map in tiles
  mapHeight   : integer = 1024 div 8; //heigh of the big map in tiles
  bgWidth     : integer = 256 div 8;  //width of the hardware map in tiles
  bgHeight    : integer = 256 div 8;  //height of the hardware map in tiles
  screenWidth : integer = 256 div 8;  //screen width in tiles
  screenHeight: integer = 192 div 8;  //screen height in tiles
begin
   videoSetMode(MODE_0_2D);
   vramSetBankA(VRAM_A_MAIN_BG);

   bg := bgInit(3, BgType_Text8bpp, BgSize_T_512x256, 0,1);

   dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
   dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

   bgTileMap := pcuint16(bgGetMapPtr(bg));
   bgLeftHalf := bgTileMap;
   bgRightHalf := bgTileMap + 32 * 32;

   for iy := 0 to screenHeight - 1 do
      dmaCopy(@Layer1024x1024Map[iy * mapWidth], @bgTileMap[iy * bgWidth], screenWidth * 2);

   while (keys and KEY_B) = 0 do
   begin
      movingHorizontal := false;
      movingVertical := false;

      scanKeys();

      keys := keysHeld();

      swiWaitForVBlank();

      if (keys and KEY_LEFT) <> 0 then
      begin
         offset_x := scroll_x div 8 - 1;
         dec(scroll_x);

         if(scroll_x < 0) then
            scroll_x := 0
         else 
            movingHorizontal := true;
      end else
      if (keys and KEY_RIGHT) <> 0 then
      begin
         offset_x := scroll_x div 8 + screenWidth;
         inc(scroll_x);

         if (scroll_x >= (mapWidth - screenWidth) * tileWidth) then
            scroll_x := (mapWidth - screenWidth) * tileWidth - 1
         else 
            movingHorizontal := true;
      end;

      if (keys and KEY_UP) <> 0 then
      begin
         offset_y := scroll_y div 8 - 1;
         dec(scroll_y);

         if(scroll_y < 0) then
            scroll_y := 0
         else 
            movingVertical := true;

      end else
      if (keys and KEY_DOWN) <> 0 then
      begin
         offset_y := scroll_y div 8 + screenHeight;
         inc(scroll_y);

         if(scroll_y >= (mapHeight - screenHeight) * tileWidth) then
            scroll_y := (mapHeight - screenHeight) * tileWidth - 1
         else 
            movingVertical := true;
      end;

      if (movingHorizontal) then
      begin
        if (offset_x and 63) >= bgWidth then
          bgTemp := bgRightHalf
        else
          bgTemp := bgLeftHalf;

         for iy := scroll_y div 8 - 1 to scroll_y div 8 + screenHeight do
            bgTemp[(offset_x and (bgWidth - 1)) + (iy and (bgHeight - 1)) * 32] := Layer1024x1024Map[offset_x + iy * mapWidth];

      end;
      
      if (movingVertical) then
      begin
        for ix := scroll_x div 8 - 1 to scroll_x div 8 + screenWidth do
        begin
          if ((ix and 63) >= bgWidth) then
            bgTemp := bgRightHalf
          else
            bgTemp := bgLeftHalf;

            bgTemp[(ix and (bgWidth - 1)) + (offset_y and (bgHeight - 1))* 32] := Layer1024x1024Map[ix + offset_y * mapWidth];
         end;
      end;

      bgSetScroll(bg, scroll_x, scroll_y);
	  bgUpdate();
   end;
end;

procedure scroll4wayExRotation();
var
  scroll_x: integer = 0;
  scroll_y: integer = 0;
  bgTileMap: pcuint16;
  iy, ix: integer;
  keys: integer = 0;
  offset_x: integer = 0;
  offset_y: integer = 0;
  movingHorizontal: boolean = false;
  movingVertical: boolean = false;
  bg: cint;
const
  tileWidth   : integer = 8;
  mapWidth    : integer = 1024 div 8;
  mapHeight   : integer = 1024 div 8;
  bgWidth     : integer = 512 div 8;
  bgHeight    : integer = 512 div 8;
  screenWidth : integer = 256 div 8;
  screenHeight: integer = 192 div 8;
begin
   videoSetMode(MODE_5_2D);
   vramSetBankA(VRAM_A_MAIN_BG);

   bg := bgInit(3, BgType_ExRotation, BgSize_ER_512x512, 0,1);

   dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
   dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

  bgTileMap := pcuint16(bgGetMapPtr(bg));

   bgSetControlBits(bg, BG_WRAP_ON);

   for iy := 0 to screenHeight - 1 do
      dmaCopy(@Layer1024x1024Map[iy * mapWidth], @bgTileMap[iy * bgWidth], screenWidth * 2);


   while (keys and KEY_B) = 0 do
   begin
      movingHorizontal := false;
      movingVertical := false;

      scanKeys();

      keys := keysHeld();

      swiWaitForVBlank();

      if (keys and KEY_LEFT) <> 0 then
      begin
         offset_x := scroll_x div 8 - 1;
         dec(scroll_x);

         if(scroll_x < 0) then
            scroll_x := 0
         else 
            movingHorizontal := true;
      end else
      if (keys and KEY_RIGHT) <> 0 then
      begin
         offset_x := scroll_x div 8 + screenWidth;
         inc(scroll_x);

         if (scroll_x >= (mapWidth - screenWidth) * tileWidth) then
            scroll_x := (mapWidth - screenWidth) * tileWidth - 1
         else 
            movingHorizontal := true;
      end;

      if (keys and KEY_UP) <> 0 then
      begin
         offset_y := scroll_y div 8 - 1;
         dec(scroll_y);

         if(scroll_y < 0) then
            scroll_y := 0
         else 
            movingVertical := true;

      end else
      if(keys and KEY_DOWN) <> 0 then
      begin
         offset_y := scroll_y div 8 + screenHeight;
         inc(scroll_y);

         if(scroll_y >= (mapHeight - screenHeight) * tileWidth) then
            scroll_y := (mapHeight - screenHeight) * tileWidth - 1
         else 
            movingVertical := true;
      end;

      if (movingHorizontal) then
      begin
         for iy := scroll_y div 8 - 1  to scroll_y div 8 + screenHeight do
            bgTileMap[(offset_x and (bgWidth - 1)) + (iy and (bgHeight - 1)) * bgWidth] := Layer1024x1024Map[offset_x  + iy * mapWidth];
      end;
      if (movingVertical) then
      begin
         for ix := scroll_x div 8 - 1 to scroll_x div 8 + screenWidth do
            bgTileMap[(ix and (bgWidth - 1)) + (offset_y and (bgHeight - 1))* bgWidth] := Layer1024x1024Map[ix + offset_y * mapWidth];
      end;

      bgSetScroll(bg, scroll_x, scroll_y);
	  bgUpdate();
   end;
end;


end.
