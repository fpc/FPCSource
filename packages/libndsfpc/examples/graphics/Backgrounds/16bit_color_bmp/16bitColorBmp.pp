program bmp_16bit_color;
{$L build/drunkenlogo.o}

uses
  ctypes, nds9;

const
  drunkenlogoBitmapLen = 26988;

var
  drunkenlogoBitmap: array [0..6746] of cuint; cvar; external;

begin
    // set the mode for 2 text layers and two extended background layers
  videoSetMode(MODE_5_2D);

  // set the sub background up for text display (we could just print to one
  // of the main display text backgrounds just as easily
  videoSetModeSub(MODE_0_2D); //sub bg 0 will be used to print text

  vramSetBankA(VRAM_A_MAIN_BG);

  consoleDemoInit();

  iprintf(#10#10#9'Hello DS devers'#10);
  iprintf(#9'www.drunkencoders.com'#10);
  iprintf(#9'16 bit bitmap demo');

  // set up our bitmap background
  bgInit(3, BgType_Bmp16, BgSize_B16_256x256, 0,0);

  decompress(@drunkenlogoBitmap, BG_GFX,  LZ77Vram);

  while true do
    swiWaitForVBlank();

end.
