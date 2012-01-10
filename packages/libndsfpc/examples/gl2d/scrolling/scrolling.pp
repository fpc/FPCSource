(*
  Easy GL2D
  Relminator 2011 
  Richard Eric M. Lope BSN RN
  Http://Rel.Phatcode.Net
  A very small, simple, yet very fast DS 2D rendering lib using the DS' 3D core.
  --
  Translated in Object Pascal by Francesco Lombardi - 2012
  http://itaprogaming.free.fr
*)

program scrolling;

{$mode objfpc}
{$H+}

{$L build/crono.o}
{$L build/tiles.o}

uses
  ctypes, nds9, gl2d, uvcoord_crono;


const
  cronoBitmapLen = 32768;
  cronoPalLen = 512;
  tilesBitmapLen = 65536;
  tilesPalLen = 512;

var
  cronoBitmap: array [0..0] of cuint; cvar; external;
  cronoPal: array [0..0] of cushort; cvar; external;
  tilesBitmap: array [0..0] of cuint; cvar; external;
  tilesPal: array [0..0] of cushort; cvar; external;

const
  MAP_WIDTH = 32;
  MAP_HEIGHT = 32;


(*
	I'm using the struct of player from the
	Animate simple man/woman exmple in the 
	"nds/examples" folder
	You might want to read up on that too to
	see the differnce in handling sprites via OAM
	and Easy GL2D.
*)

const
  P_RIGHT = 0;
  P_UP = 1;
  P_DOWN = 2;
  P_LEFT = 3;

type
  TPlayer = record
    x, y: integer;
    gfx_frame: integer;
    state: integer;
    anim_frame: integer;
    is_walking: boolean;       // an animation flag whether crono is walking or not
  end;
  PPlayer = ^TPlayer;
  
  // Our level struct
  TLevel = record
    width: integer;			// dimensions of the map
    height: integer;
    
    camera_x: integer;		// top-left cooordinates of our virtual camera
    camera_y: integer;		// Works almost the same the 2d BG scroller
    
    tile_x: integer;			// current tile the top-left coordinate of our
    tile_y: integer;			// camera occupies
    
    pixel_x: integer;		// scrolling tile offsets
    pixel_y: integer;
  end;
  PLevel = ^TLevel;

  TMapArray = array [0..MAP_WIDTH-1, 0..MAP_HEIGHT-1] of cushort;



// Animates crono
procedure AnimatePlayer(p: PPlayer);
const 
  FRAMES_PER_ANIMATION = 6;		// 6 crono animations 
	frame: integer = 0;      // a static frame counter
begin	
	// Only animate if crono is walking
	if (p^.is_walking) then
	begin
		inc(frame);
		
		// Animate only every 8th frame
		// I used an if() block instead of % since % is slow
		// on the DS (not that it would matter in this demo)
		if ((frame and 7) = 0) then
		begin 
			inc(p^.anim_frame);
			if (p^.anim_frame >= (FRAMES_PER_ANIMATION)) then
				p^.anim_frame := 0;
		end;
	end;
	
	// P_RIGHT, P_UP and P_DOWN is calculated normally.
	// P_LEFT is P_RIGHT flipped.
	case (p^.state) of
	  P_RIGHT: p^.gfx_frame := p^.anim_frame + p^.state * FRAMES_PER_ANIMATION;
	  P_UP: p^.gfx_frame := p^.anim_frame + p^.state * FRAMES_PER_ANIMATION;
		P_DOWN: p^.gfx_frame := p^.anim_frame + p^.state * FRAMES_PER_ANIMATION;
		P_LEFT: p^.gfx_frame := p^.anim_frame + P_RIGHT * FRAMES_PER_ANIMATION;
	 else
     p^.gfx_frame := p^.anim_frame + p^.state * FRAMES_PER_ANIMATION;
	end;
		
end;


// Draws a full screen map
procedure DrawMap(lvl: PLevel; map: TMapArray; tiles: pglImage);
const
	// tiles are 16x16 pixels
  TILE_SIZE = 16;
	// calculate number of tiles per row and column
	SCREEN_TILE_X = SCREEN_WIDTH div TILE_SIZE;
	SCREEN_TILE_Y = SCREEN_HEIGHT div TILE_SIZE;
var	
	x, y: integer;				// counters
	tile_x, tile_y: integer;		// current tile to draw
	screen_x, screen_y: integer;	// actual screen position (in pixel)
	i: integer;					// tile index to draw
begin	
	// we need to draw an extra tile at the bottom and right 
	// since we are scrolling
	for y := 0 to SCREEN_TILE_Y do
	begin
		for x := 0 to SCREEN_TILE_X do 
		begin
			tile_x := lvl^.tile_x + x;		// get relative tile positions
			tile_y := lvl^.tile_y + y;
			i := map[tile_x, tile_y];		// get map index
			screen_x := (x * TILE_SIZE) - lvl^.pixel_x;      //Calculate where to put a
      screen_y := (y * TILE_SIZE) - lvl^.pixel_y;      //particular tile
                
			glSprite(screen_x, screen_y, GL_FLIP_NONE , @tiles[i]);
		end;
	end;
end;


// Update's the camera's position relative to the player
procedure CameraUpdate(lvl: PLevel; p: PPlayer);
const
	// set constants for middle of screen
	SCREEN_MID_WIDTH = SCREEN_WIDTH div 2;
	SCREEN_MID_HEIGHT = SCREEN_HEIGHT div 2;
  TILE_SIZE = 16;
begin 
	// update the camera
	lvl^.camera_x := p^.x - SCREEN_MID_WIDTH;
	lvl^.camera_y := p^.y - SCREEN_MID_HEIGHT;
	
	// limit camera X values
	if ( lvl^.camera_x < 0 ) then lvl^.camera_x := 0;
	if ( lvl^.camera_x > ((lvl^.width-2) * TILE_SIZE ) - SCREEN_WIDTH ) then
		lvl^.camera_x := ((lvl^.width-2) * TILE_SIZE ) - SCREEN_WIDTH;
	
	// limit camera Y values
	if ( lvl^.camera_y < 0 ) then lvl^.camera_y := 0;
	if ( lvl^.camera_y > ((lvl^.height-2) * TILE_SIZE ) - SCREEN_HEIGHT ) then
		lvl^.camera_y := ((lvl^.height-2) * TILE_SIZE ) - SCREEN_HEIGHT;
	
	// calculate level starting tiles
	lvl^.tile_x := lvl^.camera_x div TILE_SIZE; 
	lvl^.tile_y := lvl^.camera_y div TILE_SIZE; 
	
	// calculate tile pixel offsets
	// Only works with power of 2 tilesize
	// use "%" for non-power of 2 sizes
	lvl^.pixel_x := lvl^.camera_x and (TILE_SIZE - 1);
	lvl^.pixel_y := lvl^.camera_y and (TILE_SIZE - 1);
	
end;


// Just a simple map
// A real engine should use a map editor
procedure InitMap(var map: TMapArray);
var
	x, y: integer;
begin
	for y := 0 to MAP_HEIGHT - 1 do
		for x := 0 to MAP_WIDTH - 1 do 
			map[x, y] := ((y and 15)*16 + (x and 15)) and 255;
end;

var
// This imageset would use our texture packer generated coords so it's kinda
// safe and easy to use 
// CRONO_NUM_IMAGES is a value from "uvcoord_crono.h"
  crono_images: array [0..CRONO_NUM_IMAGES-1] of glImage;

// This tileset won't make use of our texture packer generated coords
// messy, manual and prone to errors
// BMP is 256x256 and tiles are 16x16 so.. (256/16) * (256 /16) = 16 * 16
  tiles_images: array [0..((256 div 16) * (256 div 16)) - 1] of glImage;  

// Our level map
// I used shorts since we would be able to reference 65535
// uinique tiles with shorts.
// You should use malloc() or new[] to dimension
// your maps for a real game though.
  level_map: TMapArray;

	// Our crono guy
  crono: TPlayer;
	// the level
	lvl: TLevel;
  
  crono_textureID: cint;
  tiles_textureID: cint;
  
  TextureSize: integer;
	frame: integer = 0;	// ever present frame counter
	key: integer;		// for key input
  
  i: integer;
  
begin
	// crono starting positions
	crono.x := 16 * 5;		// 5th tile 
	crono.y := 16 * 5;
	crono.state := P_RIGHT;	// facing right
	crono.anim_frame := 0;	// starting frame
	lvl.width := MAP_WIDTH;		// init map dimesions
	lvl.height := MAP_HEIGHT;
	
	InitMap(level_map);			// load a randomized map (too lazy to make a proper one)
	
	videoSetMode(MODE_5_3D);	// favorite mode
	
	
	consoleDemoInit();
	
	
	// Initialize GL in 3d mode
	glScreen2D();
	
	
	// set  Bank A to texture (128 kb)
	vramSetBankA( VRAM_A_TEXTURE );
	
	vramSetBankE(VRAM_E_TEX_PALETTE);  // Allocate VRAM bank for all the palettes
	
	// Our texture handle for crono
	// I used glLoadSpriteSet since the texture was made
	// with my texture packer.
	crono_textureID :=  glLoadSpriteSet(crono_images,			// pointer to glImage array
						 CRONO_NUM_IMAGES, 		// Texture packer auto-generated #define
						 @crono_texcoords,		// Texture packer auto-generated array
						 GL_RGB256,				// texture type for glTexImage2D() in videoGL.h 
						 TEXTURE_SIZE_256,		// sizeX for glTexImage2D() in videoGL.h
						 TEXTURE_SIZE_128,		// sizeY for glTexImage2D() in videoGL.h
						 GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T or TEXGEN_OFF or GL_TEXTURE_COLOR0_TRANSPARENT, // param for glTexImage2D() in videoGL.h
						 256,					// Length of the palette to use (256 colors)
						 @cronoPal,		// Load our 256 color crono palette
						 @cronoBitmap		// image data generated by GRIT
					   );

	// Our texture handle for our tiles
	// I used glLoadTileSet since the texture 
	// is just a bunch of 16x16 tiles in a 256x256
	// tileset so we don't need a texture packer for this.
	tiles_textureID :=  glLoadTileSet(tiles_images,		// pointer to glImage array
					   16,					// sprite width
					   16,					// sprite height
					   256,					// bitmap width
					   256,					// bitmap height
					   GL_RGB256,			// texture type for glTexImage2D() in videoGL.h 
					   TEXTURE_SIZE_256,	// sizeX for glTexImage2D() in videoGL.h
					   TEXTURE_SIZE_256,	// sizeY for glTexImage2D() in videoGL.h
					   GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T or TEXGEN_OFF or GL_TEXTURE_COLOR0_TRANSPARENT, // param for glTexImage2D() in videoGL.h
					   256,					// Length of the palette to use (256 colors)
					   @tilesPal,		// Load our 256 color tiles palette
					   @tilesBitmap		// image data generated by GRIT
					 );
	
	
	

	iprintf(#$1b'[1;1HSCROLLING TEST');
	iprintf(#$1b'[3;1HArrow Keys to move');
	
	iprintf(#$1b'[6;1HRelminator');
	iprintf(#$1b'[7;1HHttp://Rel.Phatcode.Net');
	
	iprintf(#$1b'[9;1HCrono = %i', crono_textureID);
	iprintf(#$1b'[10;1HTiles = %i', tiles_textureID);

	iprintf(#$1b'[13;1HTiles by unknown');
	iprintf(#$1b'[14;1HCrono by Square Enix');
	
		// calculate the amount of 
	// memory uploaded to VRAM in KB
	TextureSize := cronoBitmapLen + tilesBitmapLen;
					  
					  
	iprintf(#$1b'[17;1HTotal Texture size= %i kb', TextureSize div 1024);


	while true do
	begin
		// increment frame counter
		inc(frame);
	
		crono.is_walking := false;  // crono is lazily standing to the right
		scanKeys();
		key := keysHeld();
		
		// process input and move crono
		if (key and KEY_RIGHT) <> 0 then
		begin
			inc(crono.x);
			crono.state := P_RIGHT;
			crono.is_walking := true;
		end;
		
		if (key and KEY_LEFT)<>0 then
		begin
			dec(crono.x);
			crono.state := P_LEFT;
			crono.is_walking := true;
		end;
		
		if (key and KEY_UP) <> 0 then
		begin
			dec(crono.y);
			crono.state := P_UP;
			crono.is_walking := true;
		end;
		
		if (key and KEY_DOWN) <> 0 then
		begin
			inc(crono.y);
			crono.state := P_DOWN;
			crono.is_walking := true;
		end;
		
		// Update player animations 	
		AnimatePlayer(@crono);
		
		
		// Update level camera relative to crono's position
		CameraUpdate(@lvl, @crono);
		
		glBegin2D();
		
			// Draw our map layer
			DrawMap( @lvl, level_map, @tiles_images );
			
			// Process crono
			// Left and right share the same frames
			// I just flipped the sprite depending on where crono faces.
			if (crono.state < P_LEFT) then
				glSpriteRotate(crono.x - lvl.camera_x, crono.y - lvl.camera_y, 0,GL_FLIP_NONE , @crono_images[crono.gfx_frame])
			else
				glSpriteRotate(crono.x - lvl.camera_x, crono.y - lvl.camera_y, 0,GL_FLIP_H , @crono_images[crono.gfx_frame]);
			
			
			// Draw a translucent gradient box to emulate dialogboxes
			// giving it a unique Polygon ID
			glPolyFmt(POLY_ALPHA(16) or POLY_CULL_NONE or POLY_ID(1));
			glBoxFilledGradient( 0, 150, 255, 191,
								 RGB15( 31,  0,  0 ),
								 RGB15(  0, 31,  0 ),
								 RGB15( 31,  0, 31 ),
								 RGB15(  0, 31, 31 )
                               );
							   
			//back to opaque mode
			// and draw the border of the "dialog box"
			glPolyFmt(POLY_ALPHA(31) or POLY_CULL_NONE );
			for i := 0 to 4 do
			  glBox(i, 150 + i, 255 - i , 191 - i,
					     RGB15( 31-i*5,  i*5,  31 - i * 3 )
					   );
			
		glEnd2D();
		
		glFlush(0);

		swiWaitForVBlank();
		
	
	end;

end.
