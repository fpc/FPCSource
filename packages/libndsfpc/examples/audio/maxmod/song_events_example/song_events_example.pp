program song_events_example;
{$L build/ball.o}
{$L build/mmsolution.bin.o}

{$mode objfpc}

uses
  ctypes, nds9, maxmod9;

const 
  ballTilesLen = 1024;
  ballPalLen = 512;
var
  ballTiles: array [0..255] of cuint; cvar; external;
  ballPal: array [0..255] of cushort; cvar; external;


const
  MOD_EXAMPLE	= 0;
  MSL_NSONGS	= 1;
  MSL_NSAMPS	= 3;
  MSL_BANKSIZE	= 4;

var
  mmsolution_bin_end: array [0..0] of cuint8; cvar; external;
  mmsolution_bin: array [0..0] of cuint8; cvar; external;
  mmsolution_bin_size: cuint32; cvar; external;


  spriteDy: cint = 0;
  spriteY: cint = 140;

//---------------------------------------------------------------------------------
// callback function to handle song events
//---------------------------------------------------------------------------------
function myEventHandler(msg, param: mm_word): mm_callback;//pmm_word;
begin
  case msg of

    MMCB_SONGMESSAGE:	// process song messages
  		// if song event 1 is triggered, set sprite's y velocity to make it jump
  		if (param = 1) then spriteDy := -16;
		
    MMCB_SONGFINISHED:	// process song finish message (only triggered in songs played with MM_PLAY_ONCE)
      exit;
  end;
end;

var
  gfx: pcuint16;

begin	
	videoSetMode(MODE_0_2D);
	
	//initialize the sprite engine with 1D mapping 32 byte boundary
	//and no external palette support
	oamInit(oamMain, SpriteMapping_1D_32, false);

	vramSetBankA(VRAM_A_MAIN_SPRITE);

	//allocate some space for the sprite graphics	
	gfx := oamAllocateGfx(oamMain, SpriteSize_32x32, SpriteColorFormat_256Color);

	//copy in our ball graphics
	dmaCopy(@ballTiles, gfx, ballTilesLen);
	dmaCopy(@ballPal, SPRITE_PALETTE, ballPalLen);
	
	//--------------------------------------------
	// initialise maxmod using default settings, and 
	// enable interface for a soundbank that is loaded 
	// into memory
	//--------------------------------------------
	mmInitDefaultMem( mm_addr(@mmsolution_bin));

	// setup maxmod to use the song event handler
	mmSetEventHandler(mm_callback(@myEventHandler));
	
	// load song
	// values for this function are in the solution header
	mmLoad( MOD_EXAMPLE );

	// start the music playing
	mmStart( MOD_EXAMPLE, MM_PLAY_LOOP );

	while true do
	begin
		// Sprite accelerates down
		spriteDy := spriteDy + 2;
		
		// sprite falls
		spriteY := spriteY + spriteDy;
		
		// Floor is arbitrarily set to 140
		if ( spriteY > 140 ) then spriteY := 140;
		
		
		oamSet(oamMain, 			//main graphics engine context
			0,           			//oam index (0 to 127)  
			(256 div 2)-16,				//x and y pixel location of the sprite
			spriteY, 			
			0,						//priority, lower renders last (on top)
			0,	//palette index if multiple palettes or the alpha value if bmp sprite	
			SpriteSize_32x32,
			SpriteColorFormat_256Color,
			gfx,				//pointer to the loaded graphics
			-1,	//sprite rotation data  
			false,					//double the size when rotating?
			false,			//hide the sprite?
			false, false, //vflip, hflip
			false	//apply mosaic
			);              
	
		swiWaitForVBlank();
		
		//send the updates to the hardware
		oamUpdate(oamMain);
	end;

end.

