program BitmapSprites;

uses
  ctypes, nds9;

//a simple sprite structure
//it is generally preferred to separate your game object
//from OAM

type
  TMySprite = record
    gfx: pcuint16;
    size: SpriteSize;
    format: SpriteColorFormat;
    rotationIndex: integer;
    paletteAlpha: integer;
    x: integer;
    y: integer;
  end;

var
  sprites: array [0..2] of TMySprite;
  i, angle: integer;

begin
  with Sprites[0] do
  begin
    gfx := nil;
    size := SpriteSize_32x32;
    format := SpriteColorFormat_Bmp;
    rotationIndex := 0;
    paletteAlpha := 15;
    x := 20;
    y := 15;
  end;

  with Sprites[1] do
  begin
    gfx := nil;
    size := SpriteSize_32x32;
    format := SpriteColorFormat_256Color;
    rotationIndex := -1;
    paletteAlpha := 0;
    x := 20;
    y := 80;
  end;

  with Sprites[2] do
  begin
    gfx := nil;
    size := SpriteSize_32x32;
    format := SpriteColorFormat_16Color;
    rotationIndex := -1;
    paletteAlpha := 1;
    x := 20;
    y := 136;
  end;

   videoSetModeSub(MODE_0_2D);

   consoleDemoInit();

   //initialize the sub sprite engine with 1D mapping 128 byte boundary
   //and no external palette support
   oamInit(oamSub, SpriteMapping_Bmp_1D_128, false);


   vramSetBankD(VRAM_D_SUB_SPRITE);

   //allocate some space for the sprite graphics
   for i := 0 to 2 do
      sprites[i].gfx := oamAllocateGfx(oamSub, sprites[i].size, sprites[i].format);

   //ugly positional printf
  iprintf(#27 + '[1;1H' + 'Direct Bitmap:');
  iprintf(#27 + '[9;1H' + '256 color:');
  iprintf(#27 + '[16;1H' + '16 color:');

  //fill bmp sprite with the color red
  dmaFillHalfWords(ARGB16(1,31,0,0), sprites[0].gfx, 32*32*2);
  //fill the 256 color sprite with index 1 (2 pixels at a time)
  dmaFillHalfWords((1 shl 8) or 1, sprites[1].gfx, 32*32);
  //fill the 16 color sprite with index 1 (4 pixels at a time)
  dmaFillHalfWords((1 shl 12) or (1 shl 8) or (1 shl 4) or 1, sprites[2].gfx, 32*32 div 2);

   //set index 1 to blue...this will be the 256 color sprite
   SPRITE_PALETTE_SUB[1] := RGB15(0,31,0);
   //set index 17 to green...this will be the 16 color sprite
   SPRITE_PALETTE_SUB[16 + 1] := RGB15(0,0,31);

   angle := 0;

  while true do
  begin
    for i := 0 to 2 do
    begin
      oamSet(
              oamSub, //sub display
              i,       //oam entry to set
              sprites[i].x, sprites[i].y, //position
              0, //priority
              sprites[i].paletteAlpha, //palette for 16 color sprite or alpha for bmp sprite
              sprites[i].size,
              sprites[i].format,
              sprites[i].gfx,
              sprites[i].rotationIndex,
              true, //double the size of rotated sprites
              false, //don't hide the sprite
              false, false, //vflip, hflip
              false //apply mosaic
            );
    end;

    oamRotateScale(oamSub, 0, angle, (1 shl 8), (1 shl 8));

    angle := angle + 64;

    swiWaitForVBlank();

    //send the updates to the hardware
    oamUpdate(oamSub);
  end;

end.
