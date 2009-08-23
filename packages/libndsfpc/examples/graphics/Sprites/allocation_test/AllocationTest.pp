program AllocationTest;

{$mode objfpc}

uses
  ctypes, nds9;

const
  SPRITE_MAX = 128;

var
  sizes: array [0..11] of SpriteSize;

//this is our game entity. Notice it has a bit more info than
//would fit into OAM.  This method is a lot more flexible than trying
//to treat oam as a game object directly.
type
  TMySprite = record
    x, y, z: integer;
    dx, dy: integer;
    alive: boolean;
    gfx: pcuint16;
    format: SpriteColorFormat;
    size: SpriteSize;
  end;
  PMySprite = ^TMySprite;

var
  sprites: array [0..SPRITE_MAX - 1] of TMySprite;
  spriteMemoryUsage: cuint32 = 0;
  oomCount: cuint32 = 0;
  allocationCount: cuint32 = 0;
  spriteMemSize: cuint32 = 128 * 1024;

  oom: boolean = false;
  oam: POamState = @oamMain;

  i: integer;

procedure createSprite(var s: TmySprite; x, y, z: integer; size: SpriteSize; format: SpriteColorFormat; dx, dy: integer);
begin
  s.alive := true;
  s.x := x;
  s.y := y;
  s.z := z;
  s.dx := dx;
  s.dy := dy;
  s.size := size;
  s.format := format;

  //api: allocate a chunk of sprite graphics memory
  s.gfx := oamAllocateGfx(oam^, size, format);

  allocationCount := allocationCount + 1;
  if (s.gfx <> nil) then
  begin
    spriteMemoryUsage := spriteMemoryUsage + ((size and $FFF) shl 5);
    oom := false;
  end else
  begin
    oom := true;
    //only a failure of the allocator if there was enough room
    if (spriteMemoryUsage + ((size and $FFF) shl 5) < spriteMemSize) then
      oomCount := oomCount + 1;
  end;
end;

procedure killSprite(var s: TMySprite);
begin
  s.alive := false;

  //api: free the graphics
  if (s.gfx <> nil) then
  begin
    oamFreeGfx(oam^, s.gfx);
    spriteMemoryUsage := spriteMemoryUsage - ((s.size and $FFF) shl 5);
  end;

  s.gfx := nil;
end;

function zsort(const a, b: pointer): integer;
var
	first, second: PMySprite;
begin
	first := PMySprite(a);
	second := PMySprite(b);

	//the trivial case
	if (first = second) then
    result := 0;

	//handle nulls
	if (first = nil) and (second <> nil) then
    result := -1;
	if (first <> nil) and (second = nil) then
    result := 1;

	//a dead sprite is always after a living one in the sort
	if (not first^.alive) and (second^.alive) then
    result := -1;
  if (first^.alive) and (not second^.alive) then
    result := 1;
	if (not first^.alive) and (not second^.alive) then
    result := 0;

	//finally if both are alive and not null sort them by depth
	if (first^.z = second^.z) then
    result := 0;
	if(first^.z < second^.z ) then
    result := -1;
	if(first^.z > second^.z) then
    result := 1;

	result := 0;
end;

procedure updateSprites();
var
  i: integer;
begin
  //sort our sprites on z
  //a more efficient way would be to keep a sorted list of sprites
  qsort(@sprites, SPRITE_MAX, sizeof(TMySprite), TSort(@zsort));

  //set oam to values required by my sprite
  for i := 0 to SPRITE_MAX - 1 do
  begin
    //an api function: void oamSet(int id, SpriteSize size, int x, int y, SpriteColorFormat format, const void* gfxOffset, bool hide);
    oamSet(oam^, i, sprites[i].x, sprites[i].y, 0, 0, sprites[i].size, sprites[i].format, sprites[i].gfx, -1, false, not sprites[i].alive, false, false, false);
  end;
end;

procedure randomSprite(var s: TMySprite);
var
  c: cuint8;
  color: cuint16;
begin

  //pick a random color index
  c := random(256);

  //two pixels at a time
  color := c or (c shl 8);

  //create a randomly oriented sprite going off in a random direction
  createSprite(s, random(256), random(192), 0, integer(sizes[random(12)]), SpriteColorFormat_256Color, random(4) - 2, random(4) - 2);

  //dont let sprites get stuck with 0 velocity
  if (s.dx = 0) and (s.dy = 0) then
  begin
    s.dx := random(3) + 1;
    s.dy := random(3) + 1;
  end;

  //the size (in pixels) is encoded in the low 12 bits of the Size attribute (shifted left by 5)
  //we load new graphics each time as this is as much a test of my allocator as an example of api usage
  if (s.gfx <> nil) then
    swiCopy(@color, s.gfx, ((s.size and $FFF) shl 4) or COPY_MODE_FILL)
  else
    s.alive := false;
end;

procedure moveSprites();
var
  i: integer;
begin
	for i := 0 to SPRITE_MAX - 1 do
	begin
		sprites[i].x := sprites[i].x + sprites[i].dx;
		sprites[i].y := sprites[i].y + sprites[i].dy;

		if (sprites[i].x >= 256) or (sprites[i].x < 0) or (sprites[i].y >= 192) or (sprites[i].y < 0) then
		begin
			killSprite(sprites[i]);
			randomSprite(sprites[i]);
		end;
	end;
end;

var
  memUsageTemp: longint;// = $FFFFFFFF;

begin
  randomize;
  sizes[0]  := SpriteSize_8x8;
  sizes[1]  := SpriteSize_8x16;
  sizes[2]  := SpriteSize_16x8;
  sizes[3]  := SpriteSize_8x32;
  sizes[4]  := SpriteSize_16x16;
  sizes[5]  := SpriteSize_32x8;
  sizes[6]  := SpriteSize_16x32;
  sizes[7]  := SpriteSize_32x16;
  sizes[8]  := SpriteSize_32x32;
  sizes[9]  := SpriteSize_32x64;
  sizes[10] := SpriteSize_64x32;
  sizes[11] := SpriteSize_64x64;

	videoSetMode(MODE_0_2D);
	videoSetModeSub(MODE_0_2D);
	vramSetBankA(VRAM_A_MAIN_SPRITE);
	vramSetBankB(VRAM_B_MAIN_SPRITE);
	vramSetBankD(VRAM_D_SUB_SPRITE);

	consoleDemoInit();
//	consoleDebugInit(DebugDevice_NOCASH); //send stderr to no$gba debug window

	//api: initialize OAM to 1D mapping with XX byte offsets and no external palette
	oamInit(oam^, SpriteMapping_1D_128, false);

  //create some sprites
  for i := 0 to SPRITE_MAX - 1 do
    randomSprite(sprites[i]);

  //load a randomly colored palette
  for i := 0 to 255 do
  begin
    SPRITE_PALETTE[i] := random((1 shl 15) - 1);
    SPRITE_PALETTE_SUB[i] := random((1 shl 15) - 1);
  end;

  while true do
  begin
		moveSprites();

		updateSprites();

		swiWaitForVBlank();
		
		//api: updates real oam memory 
		oamUpdate(oam^);

		if oom then
      if memUsageTemp > spriteMemoryUsage then
        memUsageTemp := spriteMemoryUsage;

		consoleClear();

    printf('Memory usage: %i %i%% '#10,  spriteMemoryUsage, 100 * spriteMemoryUsage div (spriteMemSize));
    printf('Percentage fail: %i%% '#10, oomCount * 100 div allocationCount);
    printf('Lowest Usage at fail %i %i%% '#10, memUsageTemp, 100 * memUsageTemp div (spriteMemSize));
  end;
end.
		
