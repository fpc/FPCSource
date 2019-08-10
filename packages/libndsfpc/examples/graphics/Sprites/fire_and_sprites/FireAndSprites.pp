program FireAndSprites;
{$L build/ball.pcx.o}

{$mode objfpc}

uses
  ctypes, nds9;
  
{$include inc/ball.pcx.inc}

const
  NUM_SPRITES = 128;

var
  OAMCopySub: array [0..127] of SpriteEntry;

type
  TSprite = record
    x, y: cint;       //location
    dx, dy: cint;     //speed
    oam: PSpriteEntry;
    gfxID: integer;         //graphics location
  end;
  PSprite = ^TSprite;


procedure MoveSprite(var sp: TSprite);
var
  x, y: integer;
begin
  x := sp.x shr 8;
  y := sp.y shr 8;
  sp.oam^.y := y;
  sp.oam^.x := x;
end;

procedure initOAM();
var
  i: integer;
begin
  for i := 0 to 127 do
    OAMCopySub[i].attribute[0] := ATTR0_DISABLED;
end;

procedure updateOAM();
begin
  memcpy(OAM_SUB, @OAMCopySub, 128 * sizeof(SpriteEntry));
  //dmaCopy(@OAMCopySub, OAM_SUB, 128 * sizeof(SpriteEntry));
end;

// HSV to RGB conversion function with only integer math
function hsl2rgb(hue, sat, lum: cuint8): cuint16;
var
  v: cint;
  m: cint;
  sextant: cint;
  fract, vsf, mid1, mid2: cint;
begin
  if lum < 128 then
    v := lum * (256 + sat) shr 8
  else
    v := (((lum + sat) shl 8) - lum * sat) shr 8;
  
  if (v <= 0) then
    hsl2rgb := RGB8(0,0,0)
  else 
 begin

    m := lum + lum - v;
    hue := hue * 6;
    sextant := hue shr 8;
    fract := hue - (sextant shl 8);
    vsf := v * fract * (v - m) div v shr 8;
    mid1 := m + vsf;
    mid2 := v - vsf;
    case sextant of
      0: hsl2rgb := RGB8(v,mid1,m); 
      1: hsl2rgb := RGB8(mid2,v,m);
      2: hsl2rgb := RGB8(m,v,mid1); 
      3: hsl2rgb := RGB8(m,mid2,v); 
      4: hsl2rgb := RGB8(mid1,m,v); 
    else hsl2rgb := RGB8(v,m,mid2);
    end; 
  end;
end;

const
  WIDTH = 256;   
  HEIGHT = 196;

var
  fire: array [0..HEIGHT, 0..WIDTH - 1] of cuint8;

  x, y: integer;
  w: integer = WIDTH; 
  h: integer = HEIGHT;
  sprites: array [0..NUM_SPRITES - 1] of TSprite;
  i, delta: integer;
  ix, iy: integer;
  
  temp: cint;
  pressed: integer;
  
  map0, map1: pcuint16;
  red: cuint16;
  ball: sImage;

begin
  randomize;

  i := 0;
  delta := 0;
  ix := 0;
  iy := 0;

  map0 := pcuint16(SCREEN_BASE_BLOCK_SUB(1));
  map1 := pcuint16(SCREEN_BASE_BLOCK_SUB(2));

  //set main display to render 256 color bitmap
  videoSetMode(MODE_5_2D or DISPLAY_BG3_ACTIVE);

  //set up the sub display
  videoSetModeSub(MODE_0_2D or
          DISPLAY_SPR_1D_LAYOUT or
          DISPLAY_SPR_ACTIVE or
          DISPLAY_BG0_ACTIVE or
          DISPLAY_BG1_ACTIVE );

  //vram banks are somewhat complex
  vramSetPrimaryBanks(VRAM_A_MAIN_BG_0x06000000, VRAM_B_MAIN_SPRITE, VRAM_C_SUB_BG, VRAM_D_SUB_SPRITE);

  //load our ball pcx file into an image
  loadPCX(pcuint8(ball_pcx), @ball);

  //tile it so it is useful as sprite data
  imageTileData(@ball);

  // Sprite initialisation
  for i := 0 to 255 do
    SPRITE_PALETTE_SUB[i] := cuint32(ball.palette[i]);

  for i := 0 to 32*16 - 1 do
    SPRITE_GFX_SUB[i] := cuint32(ball.image.data16[i]);

  //turn off sprites
  initOAM();

  for i := 0 to NUM_SPRITES - 1 do
  begin
    //random place and speed
    sprites[i].x := random($FFFF);
    sprites[i].y := random($7FFF);
    sprites[i].dx := (random($FF)) + $100;
    sprites[i].dy := (random($FF)) + $100;

    if random(2) = 1 then
      sprites[i].dx := -sprites[i].dx;
    if random(2) = 1 then
      sprites[i].dy := -sprites[i].dy;

    sprites[i].oam := @OAMCopySub[i];
    sprites[i].gfxID := 0;

    //set up our sprites OAM entry attributes
    sprites[i].oam^.attribute[0] := ATTR0_COLOR_256 or ATTR0_SQUARE;
    sprites[i].oam^.attribute[1] := ATTR1_SIZE_32;
    sprites[i].oam^.attribute[2] := sprites[i].gfxID;
  end;

  //set up two backgrounds to scroll around
  REG_BG0CNT_SUB^ := BG_COLOR_256 or (1 shl MAP_BASE_SHIFT);
  REG_BG1CNT_SUB^ := BG_COLOR_256 or (2 shl MAP_BASE_SHIFT);

  BG_PALETTE_SUB[0] := RGB15(10,10,10);
  BG_PALETTE_SUB[1] := RGB15(0,16,0);
  BG_PALETTE_SUB[2] := RGB15(0,0,31);

  //load the maps with alternating tiles (0,1 for bg0 and 0,2 for bg1)
  for iy := 0 to 31 do
    for ix := 0 to 31 do
    begin
      map0[iy * 32 + ix] := (ix xor iy) and 1;
      map1[iy * 32 + ix] := ((ix xor iy) and 1) shl 1;
    end;

  //fill 2 tiles with different colors
  for i := 0 to (64 div 2) - 1 do
  begin
    BG_GFX_SUB[i+32] := $0101;
    BG_GFX_SUB[i+32+32] := $0202;
  end;

	FillChar(fire, 256*192, 0);

	// Set up palette for fire effect
	for x := 0 to 256 do
  begin 
    if x * 2 > 255 then
      BG_PALETTE[x] := hsl2rgb(x div 3,255, 255)
    else
      BG_PALETTE[x] := hsl2rgb(x div 3, 255, x * 2);
  end;

	// Set up background for 8bit bitmap
	REG_BG3CNT^ := BG_BMP8_256x256;

	// and 1:1 scaling
	REG_BG3PA^ := 1 shl 8;
	REG_BG3PB^ := 0; // BG SCALING X
	REG_BG3PC^ := 0; // BG SCALING Y
	REG_BG3PD^ := 1 shl 8;
	REG_BG3X^ := 0;
	REG_BG3Y^ := 0;

  while (true) do
  begin
    //scroll the background
    REG_BG0HOFS_SUB^ := delta;
    inc(delta);
    REG_BG0VOFS_SUB^ := delta;

    //move the sprites
    for i := 0 to NUM_SPRITES - 1 do
    begin
      sprites[i].x := sprites[i].x + sprites[i].dx;
      sprites[i].y := sprites[i].y + sprites[i].dy;

      //check for collision with the screen boundries
      if (sprites[i].x < (1 shl 8)) or (sprites[i].x > (247 shl 8)) then
        sprites[i].dx := -sprites[i].dx;

      if (sprites[i].y < (1 shl 8)) or (sprites[i].y > (182 shl 8)) then
        sprites[i].dy := -sprites[i].dy;

      //reposition the sprites
      MoveSprite(sprites[i]);
    end;

 

    //do the plasma/fire
		//randomize the bottom row of the fire buffer
		for x := 0 to w - 1 do
      //fire[h-1, x] := abs(32768 + random(high(cint))) mod 256;
      fire[h-1, x] := abs(32768 + random(32767)) mod 256;
    
		//do the fire calculations for every pixel, from top to bottom
		for y := 0 to h - 2 do
			for x := 0 to w - 1 do
      begin
        temp := fire[y + 1, (x - 1) mod w] + fire[y + 2, (x) mod w] + fire[y + 1, (x + 1) mod w] + fire[y + 3, (x) mod w];
			end;

		dmaCopy(@fire, pointer($06000000), 256 * 192);
  
    swiWaitForVBlank();

   
    scanKeys();
		pressed := keysDown();
		if (pressed and KEY_START) <> 0 then break;

    updateOAM();

  end;
end.

