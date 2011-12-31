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
program sprites;

{$L build/enemies.o}
{$L build/zero.o}
{$L build/tiles.o}
{$L build/shuttle.o}
{$L build/anya.o}
{$L build/font.o}
{$L build/fontbubble.o}

{$mode objfpc}
{$H+}



uses 
 ctypes, nds9, gl2d, 
 cearn_atan, 
 uvcoord_enemies, uvcoord_zero;

// GRIT auto-generated  files
const
  enemiesBitmapLen = 65536;
  enemiesPalLen = 512;
  zeroBitmapLen = 32768;
  tilesBitmapLen = 65536;
  tilesPalLen = 512;
  shuttleBitmapLen = 2048;
  shuttlePalLen = 32;
  anyaBitmapLen = 32768;
  fontTilesLen = 3072;
  fontPalLen = 512;
  fontbubbleTilesLen = 2048;
  fontbubblePalLen = 512;

var
  enemiesBitmap: array [0..0] of cuint; cvar; external;
  enemiesPal: array [0..0] of cushort; cvar; external;
  zeroBitmap: array [0..0] of cuint; cvar; external;
  tilesBitmap: array [0..0] of cuint; cvar; external;

  tilesPal: array [0..0] of cushort; cvar; external;
  shuttleBitmap: array [0..0] of cuint; cvar; external;

  shuttlePal: array [0..0] of cushort; cvar; external;
  anyaBitmap: array [0..0] of cuint; cvar; external;
  fontTiles: array [0..0] of cuint; cvar; external;
  fontPal: array [0..0] of cushort; cvar; external;
  fontbubbleTiles: array [0..0] of cuint; cvar; external;
  fontbubblePal: array [0..0] of cushort; cvar; external;

// Declare our BG drawing routine
procedure DrawBG(images:  pglImage);
var
  x, y, i: integer;
begin 
  for y := 0 to (256 div 16) - 1 do
    for x := 0 to (256 div 16) - 1 do
    begin
      i := y * 16 + x;
      glSprite(x * 16, y * 16, GL_FLIP_NONE, @images[i and 255]);
    end;
end;



var
  BRAD_PI: cint = 1 shl 14;

  // This imagesets would use our texture packer generated coords so it's kinda
  // safe and easy to use 
  // ENEMIES_NUM_IMAGES is a value from "uvcoord_crono.h"
  // ZERO_NUM_IMAGES is a value from "uvcoord_zero.h"
  Enemies: array [0..ENEMIES_NUM_IMAGES  -1] of glImage;  // spriteset
  Zero: array [0..ZERO_NUM_IMAGES - 1] of glImage;        // spriteset

  // This tileset won't make use of our texture packer generated coords.
  // Messy, manual and prone to errors.
  // BMP is 256x256 and tiles are 16x16 so.. (256/16) * (256 /16) = 16 * 16
  Tiles: array [0..(256 div 16) * (256 div 16) - 1] of glImage;  

  // These sprites are single texture only so no need to
  // do anything special
  Shuttle: array [0..0] of glImage;
  Anya: array [0..0] of glImage;
  
  topScreen, bottomScreen: TPrintConsole;
  font, fontbubble: consoleFont;
  
  hitPal: array of cushort;
  
  i: cint;
  
  PaletteID: cint;           // the all-white pal
  OriginalPaletteID: cint;   // Original palette
  EnemiesTextureID: cint; 
  ZeroTextureID: cint;
  TilesTextureID: cint;
  ShuttleTextureID: cint;
  AnyaTextureID: cint;

  TextureSize: cint;
  
  Frame: cint;          // just the standard frame counter
  PhoenixFrame: cint;   // animation frame for our firebird
  BeeFrame: cint;       // animation frame for the bee
  ZeroFrame: cint;      // animation frame for zero
  Rotation: cint;       // rotation value of the rotating sprites
  
  x, y: integer;
  color: cint;

begin
  // set mode 5, enable BG0 and set it to 3D
  videoSetMode(MODE_5_3D);
  videoSetModeSub(MODE_0_2D);
  
  // initialize gl2d
  glScreen2D();
  
  // Set up enough texture memory for our textures
  // Bank A is just 128kb and we are using 194 kb of
  // sprites
  vramSetBankA(VRAM_A_TEXTURE);
  vramSetBankB(VRAM_B_TEXTURE);

  vramSetBankF(VRAM_F_TEX_PALETTE);  // Allocate VRAM bank for all the palettes
  
  vramSetBankE(VRAM_E_MAIN_BG);  // Main bg text/8bit bg. Bank E size == 64kb, exactly enough for 8bit * 256 * 192 + text layer
  
  // Load our custom font for the top screen
  consoleInit(@topScreen, 1, BgType_Text4bpp, BgSize_T_256x256, 31, 0, true, false);
  //put bg 0 at a lower priority than the text background
  bgSetPriority(0, 1);

  // Bottom screeen
  vramSetBankI(VRAM_I_SUB_BG_0x06208000);
  consoleInit(@bottomScreen, 0, BgType_Text4bpp, BgSize_T_256x256, 20, 0, false, false);
  
  font.gfx := pcuint16(@fontTiles);
  font.pal := pcuint16(@fontPal);
  font.numChars := 95;
  font.numColors :=  fontPalLen div 2;
  font.bpp := 4;
  font.asciiOffset := 32;
  font.convertSingleColor := false;
  
  
  // Top Screen 
  fontbubble.gfx := pcuint16(@fontbubbleTiles);
  fontbubble.pal := pcuint16(@fontbubblePal);
  fontbubble.numChars := 64;
  fontbubble.numColors :=  fontbubblePalLen div 2;
  fontbubble.bpp := 4;
  fontbubble.asciiOffset := 32;
  fontbubble.convertSingleColor := false;
  
  consoleSetFont(@bottomScreen, @font);
  consoleSetFont(@topScreen, @fontbubble);
  
  
  setlength(hitPal, 256);
  for i := 0 to 255 do
    hitPal[i] := ($FF shl 8) or $FF;
  
  
  // Generate the texture and load the all-white palette
  glGenTextures(1, @PaletteID);
  glBindTexture(0, PaletteID);
  glColorTableEXT(0, 0, 256, 0, 0, @hitPal[0]);
  
  // Generate another palette for our original image palette
  glGenTextures(1, @OriginalPaletteID);
  glBindTexture(0, OriginalPaletteID);
  glColorTableEXT(0, 0, 256, 0, 0, enemiesPal);
  
  // free some memory
  setLength(hitPal, 0);

  // Load our Enemies texture
  // We used glLoadSpriteSet since the texture was made
  // with my texture packer.
  EnemiesTextureID := glLoadSpriteSet(
              Enemies,             // pointer to glImage array
              ENEMIES_NUM_IMAGES,   // Texture packer auto-generated #define
              @enemies_texcoords,   // Texture packer auto-generated array
              GL_RGB256,            // texture type for glTexImage2D() in videoGL.h 
              TEXTURE_SIZE_256,     // sizeX for glTexImage2D() in videoGL.h
              TEXTURE_SIZE_256,     // sizeY for glTexImage2D() in videoGL.h
              GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T or TEXGEN_OFF or 
                GL_TEXTURE_COLOR0_TRANSPARENT, // param for glTexImage2D() in videoGL.h
              256,                  // Length of the palette to use (256 colors)
              @enemiesPal,          // Load our 256 color enemies palette
              @enemiesBitmap        // image data generated by GRIT
            );
  
  // Load our Zero texture
  // We used glLoadSpriteSet since the texture was made
  // with my texture packer.
  // No need for another palette since enemies and zero
  // share the same palette.
  ZeroTextureID := glLoadSpriteSet(
              Zero,              // pointer to glImage array
              ZERO_NUM_IMAGES,    // Texture packer auto-generated #define
              @zero_texcoords,    // Texture packer auto-generated array
              GL_RGB256,          // texture type for glTexImage2D() in videoGL.h 
              TEXTURE_SIZE_128,   // sizeX for glTexImage2D() in videoGL.h
              TEXTURE_SIZE_256,   // sizeY for glTexImage2D() in videoGL.h
              GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T or TEXGEN_OFF or 
                GL_TEXTURE_COLOR0_TRANSPARENT, // param for glTexImage2D() in videoGL.h
              256,                // Length of the palette to use (256 colors)
              @enemiesPal,        // Zero and Enemies share the same palette
              @zeroBitmap         // image data generated by GRIT
            );
  
  
  // Our texture handle for our tiles
  // I used glLoadTileSet since the texture 
  // is just a bunch of 16x16 tiles in a 256x256
  // tileset so we don't need a texture packer for this.
  TilesTextureID := glLoadTileSet(
              Tiles,           // pointer to glImage array
              16,               // sprite width
              16,               // sprite height
              256,              // bitmap width
              256,              // bitmap height
              GL_RGB256,        // texture type for glTexImage2D() in videoGL.h 
              TEXTURE_SIZE_256, // sizeX for glTexImage2D() in videoGL.h
              TEXTURE_SIZE_256, // sizeY for glTexImage2D() in videoGL.h
              GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T or TEXGEN_OFF or 
                GL_TEXTURE_COLOR0_TRANSPARENT, // param for glTexImage2D() in videoGL.h
              256,              // Length of the palette to use (256 colors)
              @tilesPal,        // Load our 256 color tiles palette
              @tilesBitmap      // image data generated by GRIT
            );
  
  
  
  // Shuttle
  // Since the shuttle is just a single 64x64 image,
  // We use glLoadTileSet() giving the right dimensions.
  ShuttleTextureID := glLoadTileSet(
              Shuttle,         // pointer to glImage array
              64,               // sprite width
              64,               // sprite height
              64,               // bitmap image width
              64,               // bitmap image height
              GL_RGB16,         // texture type for glTexImage2D() in videoGL.h
              TEXTURE_SIZE_64,  // sizeX for glTexImage2D() in videoGL.h
              TEXTURE_SIZE_64,  // sizeY for glTexImage2D() in videoGL.h
              GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T or TEXGEN_OFF or 
                GL_TEXTURE_COLOR0_TRANSPARENT,
              16,               // Length of the palette to use (16 colors)
              @shuttlePal,      // Load our 256 color tiles palette
              @shuttleBitmap    // image data generated by GRIT
            );

  

  // Anya
  // This is a 16 bit image
  AnyaTextureID := glLoadTileSet(
              Anya,
              128,
              128,
              128,
              128,
              GL_RGB,
              TEXTURE_SIZE_128,
              TEXTURE_SIZE_128,
              GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T or TEXGEN_OFF,
              0,   // Just use 0 if palette is not in use
              nil,   // Just use nil if palette is not in use
              @anyaBitmap
            );

  // Print some console stuff

  // Top
  consoleSelect(@topScreen);
  iprintf(#10#10#10#10#9'WOOT!'#10);
  iprintf(#9'TOPSCREEN 3D+TEXT'#10);
    
  
  // Bottom
  consoleSelect(@bottomScreen);
  iprintf(#$1b'[1;1HEasy GL2D Sprites Demo');
  iprintf(#$1b'[2;1HRelminator');
  iprintf(#$1b'[4;1HHttp://Rel.Phatcode.Net');
  
  iprintf(#$1b'[6;1HA demo showing some sprite');
  iprintf(#$1b'[7;1Hcapabilities of Easy GL2D');
  
  iprintf(#$1b'[ 9;1HSprites by:');
  iprintf(#$1b'[10;1HAdigun A. Polack, Patater,');
  iprintf(#$1b'[11;1HCapcom and Anya Therese Lope');
  
  iprintf(#$1b'[13;1HTextureIDs = %i, %i, %i, %i, %i', 
        EnemiesTextureID,
        ZeroTextureID,
        TilesTextureID,
        ShuttleTextureID,
        AnyaTextureID );
  
  
  // calculate the amount of 
  // memory uploaded to VRAM in KB
  TextureSize := enemiesBitmapLen + zeroBitmapLen + tilesBitmapLen + shuttleBitmapLen + anyaBitmapLen;
            
            
  iprintf(#$1b'[15;1HTotal Texture size= %i kb', TextureSize div 1024);
  
  
  iprintf(#$1b'[17;1HEnemies use a 256 color pal');
  iprintf(#$1b'[18;1HZero uses a 256 color pal');
  iprintf(#$1b'[19;1HTiles use a 256 color pal');
  iprintf(#$1b'[20;1HShuttle uses a 16 color pal');
  iprintf(#$1b'[21;1HAnya is a 16 bit image');
  
  
  // some variables for our demo
  Frame := 0;       // just the standard frame counter
  PhoenixFrame := 0;    // animation frame for our firebird
  BeeFrame := 0;      // animation frame for the bee
  ZeroFrame := 0;     // animation frame for zero
  Rotation := 0;      // rotation value of the rotating sprites
  
  
  while true do 
  begin
    inc(Frame);
    
    Rotation := Frame * 240;    // speed up our rotation
    
    // animate some of our animated sprites
    // every 8th frame
    if (Frame and 7) = 0 then
    begin
      BeeFrame := (BeeFrame + 1) and 1;
      inc(PhoenixFrame);
      if (PhoenixFrame > 2) then 
        PhoenixFrame := 0;
      
    end;

    // Faster zero animation
    if (Frame and 3) = 0 then
    begin
      inc(ZeroFrame);
      if (ZeroFrame > 9) then 
        ZeroFrame := 0;
    end;
    
    
    // calculate positions for our rotating sprites
    x := 128 + SarLongInt((cosLerp(Frame) + sinLerp(BRAD_PI + Rotation) * 100), 12);
    y := 96 + SarLongInt((cosLerp(Frame) + cosLerp(-Rotation) * 80), 12);
      
    
    
    // Start 2D mode
    glBegin2D();

      // Set our palette to our tiles (256 colors)
      // and draw our background
      DrawBG(@Tiles);
      
      // Make the Anya's rotoscaled sprite translucent just for kicks
      // Use glSpriteRotateScaleXY() for some effect
      // Give it a polygon ID so that transluceny would work
      glPolyFmt(POLY_ALPHA(20) or POLY_CULL_NONE or POLY_ID(1));
      glSpriteRotateScaleXY(SCREEN_WIDTH div 2, SCREEN_HEIGHT div 2, 
          Frame * 140, sinLerp(Frame * 120) * 3, sinLerp(Frame * 210) * 2, 
          GL_FLIP_NONE, 
          @Anya);
      
      
      // Draw our enemies
      // draw some rotated and/or animated sprites
      // Give  the other sprites different polygon IDs
      // so that translucency works
      glPolyFmt(POLY_ALPHA(20) or POLY_CULL_NONE or POLY_ID(2));
      glSpriteRotate(      x,       y,      Rotation,           GL_FLIP_NONE, @Enemies[30 + BeeFrame]);
      glSpriteRotate(255 - x, 191 - y,  Rotation * 4,              GL_FLIP_H, @Enemies[84]);
      glSpriteRotate(255 - x,       y,     -Rotation,              GL_FLIP_V, @Enemies[32]);
      glSpriteRotate(      x, 191 - y, -Rotation * 3, GL_FLIP_H or GL_FLIP_V, @Enemies[81]);
      
      
      // Some phoenix enemies on the right
      // Note the flipmodes 
      // Also shows how we can draw in "color mode" and shadow mode
      glPolyFmt(POLY_ALPHA(20) or POLY_CULL_NONE or POLY_ID(3));
      glSprite(200,  0, GL_FLIP_NONE, @Enemies[87 + PhoenixFrame]);
      glColor(RGB15(31, 0, 0));
      glSprite(200, 30,    GL_FLIP_H, @Enemies[87 + PhoenixFrame]);
      
      // Make the last two sprites translucent
      glPolyFmt(POLY_ALPHA(20) or POLY_CULL_NONE or POLY_ID(4));
      glColor(RGB15(0, 31, 20) );
      glSprite(200, 60, GL_FLIP_V,              @Enemies[87 + PhoenixFrame]);
      glColor(RGB15(0, 0, 0));
      glSprite(200, 90, GL_FLIP_V or GL_FLIP_H, @Enemies[87 + PhoenixFrame]);
      
      //Restore color and translucency to normal 
      glColor(RGB15(31, 31, 31));
      glPolyFmt(POLY_ALPHA(31) or POLY_CULL_NONE or POLY_ID(5));
      
      
      // "Clean Stretch" the sprite
      // Useful for lasers and some effects
      glSpriteStretchHorizontal(0, 135, 64 + (abs(sinLerp(Frame * 100) * 200) shr 12), @Shuttle);
      
      
      // USING DIFFERENT PALETTES
      // Set the active texture to Zero
      // and use our special all white palette
      glSetActiveTexture(ZeroTextureID);
      glAssignColorTable(0, PaletteID);
      
      // Zero Sprite is drawn all white
      glSprite(0, 42 * 0, GL_FLIP_NONE, @Zero[ZeroFrame]);

      // Draw a horizontally  flipped "disco" zero
      // Disco fx is done with glColor
      color := (Frame * 4) and 31;
      glColor(RGB15(color, 31 - color, 16 + color * 2));
      glSprite(0, 42 * 1, GL_FLIP_H, @Zero[ZeroFrame]);
      
      // Restore our palette
      glAssignColorTable(0, OriginalPaletteID);
      
      // restore pal to enemies
      glColor(RGB15(31 - color, 16 + color * 2, color));
      glSprite(0, 42 * 2, GL_FLIP_V, @Zero[ZeroFrame]);

      
      // Normalize color
      glColor(RGB15(31, 31, 31));
      glSprite(0, 42 * 3, GL_FLIP_V or GL_FLIP_H, @Zero[ZeroFrame]);

    glEnd2D();

    glFlush(0);

    swiWaitForVBlank();
  
    
  end;

end.
