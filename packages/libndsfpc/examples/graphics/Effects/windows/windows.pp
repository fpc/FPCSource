program windows;
{$L build/drunkenlogo.o}

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
  x, y, size: integer;



begin    
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);
  
  //enable a background
  bg3 := bgInit(3, BgType_Bmp8, BgSize_B8_256x256, 0, 0);
    
  //use the standard drunken logo
  dmaCopy(@drunkenlogoBitmap, bgGetGfxPtr(bg3), drunkenlogoBitmapLen);
  dmaCopy(@drunkenlogoPal, BG_PALETTE, drunkenlogoPalLen);
  
  //enable window 0 
  windowEnable(WINDOW_0);
  
  //enable window 0 on our new background
  bgWindowEnable(bg3, WINDOW_0);
  
  x := 60;
  y := 60;
  size := 100;

  while true do 
  begin
    scanKeys();
    //the code below just moves the window around 
    if (keysHeld() and KEY_UP) <> 0 then dec(y);
    if (keysHeld() and KEY_DOWN) <> 0 then  inc(y);
    if (keysHeld() and KEY_LEFT) <> 0 then  dec(x);
    if (keysHeld() and KEY_RIGHT) <> 0 then  inc(x);
    
    if (keysHeld() and KEY_A) <> 0 then  dec(size);
    if (keysHeld() and KEY_B) <> 0 then  inc(size);
    
    if (keysHeld() and KEY_X) <> 0 then  
    begin
    	bgWindowDisable(bg3, WINDOW_OUT);
    	bgWindowEnable(bg3, WINDOW_0);
    end;
    if (keysHeld() and KEY_Y) <> 0 then 
    begin
    	bgWindowDisable(bg3, WINDOW_0);
    	bgWindowEnable(bg3, WINDOW_OUT);
    end;
    
    if (x < 0) then x := 0;
    if (x > SCREEN_WIDTH - 1) then x := SCREEN_WIDTH - 1;
    if (y < 0) then y := 0;
    if (y > SCREEN_HEIGHT - 1) then y := SCREEN_HEIGHT - 1;
    
    swiWaitForVBlank();
    
    //set up the boundaries on our window
    windowSetBounds(WINDOW_0, x, y, x + size, y + size);
  end;

end.