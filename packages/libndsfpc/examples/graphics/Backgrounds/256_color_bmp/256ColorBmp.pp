program bmp_256_color;
{$L build/drunkenlogo.o}

{$apptype arm9}

{$mode objfpc}

uses
  ctypes, nds9;

const
  drunkenlogoPalLen = 512;
  drunkenlogoBitmapLen = 65536;

var
  drunkenlogoPal: array [0..255] of cushort; cvar; external;
  drunkenlogoBitmap: array [0..16383] of cuint; cvar; external;
  bg3: integer;

begin
  // set the mode for 2 text layers and two extended background layers
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG_0x06000000);

  consoleDemoInit();

  iprintf(#10#10#9 + 'Hello DS devers' + #10);
  iprintf(#9 + 'www.drunkencoders.com' + #10);
  iprintf(#9 + '256 color bitmap demo');

  bg3 := bgInit(3, BgType_Bmp8, BgSize_B8_256x256, 0,0);

  dmaCopy(@drunkenlogoBitmap, bgGetGfxPtr(bg3), 256*256);
  dmaCopy(@drunkenlogoPal, BG_PALETTE, 256*2);


  while true do
    swiWaitForVBlank();

end.
