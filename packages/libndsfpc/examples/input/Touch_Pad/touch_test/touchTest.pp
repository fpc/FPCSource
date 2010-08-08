program touchTest;
{$L build/ballpalette.bin.o}
{$L build/balldata.bin.o}

uses
  nds9, ctypes;

var
  OAMCopy: array [0..127] of SpriteEntry;

{$include inc/ballpalette.bin.inc}
{$include inc/balldata.bin.inc}

procedure initOAM(); 
var
  i: integer;
begin
  for i := 0 to 127 do
    OAMCopy[i].attribute[0] := ATTR0_DISABLED;
end;

procedure updateOAM();
begin
  Move(OAMCopy, OAM^, 128 * sizeof(SpriteEntry)); 
end;


type
  TTouchType = (ttContinuous, ttSingle);

var
  frame: integer;
  TouchType: TTouchType = ttContinuous;


procedure Vblank();
begin
  inc(frame);
end;
 
var
  min_x, min_y, max_x, max_y: integer;
  min_px, min_py, max_px, max_py: integer;
	touch: touchPosition;
  i: integer;
	pressed, held: integer;

begin
	min_x  := 4096;
  min_y  := 4096; 
  max_x  := 0;
  max_y  := 0;
	min_px := 4096;
  min_py := 4096;
  max_px := 0;
  max_py := 0;
  
  // put the main screen on the bottom lcd
  lcdMainOnBottom();
  
  initOAM();
  //enable vram and map it to the right places
  vramSetMainBanks(   VRAM_A_MAIN_SPRITE,         //A and B maped consecutively as sprite memory
                      VRAM_B_MAIN_SPRITE,         //this gives us 256KB which is the max
                      VRAM_C_MAIN_BG_0x06000000,  //map C to background memory
                      VRAM_D_LCD                  //not using D
                      ); 
   
  //set the video mode
  videoSetMode(  MODE_0_2D or 
                 DISPLAY_SPR_ACTIVE or		//turn on sprites
                 DISPLAY_BG0_ACTIVE or		//turn on background 0
                 DISPLAY_SPR_1D			      //this is used when in tile mode
                  );

  // Sprite initialisation
  Move(ballpalette_bin, SPRITE_PALETTE^, ballpalette_bin_size);
  Move(balldata_bin, SPRITE_GFX^, balldata_bin_size);
 
  consoleInit(nil, 0, BgType_Text4bpp, BgSize_T_256x256, 31, 0, true, true); 
  
  iprintf(#27'[4;8H' + 'Touch Screen Test');
  iprintf(#27'[15;4H' + 'Right Shoulder toggles');
 
  while true do
  begin
    swiWaitForVBlank();
    updateOAM();
  
    // read the button states
    scanKeys();
  
    // read the touchscreen coordinates
    touchRead(touch);
  
    pressed := keysDown();  // buttons pressed this loop
    held := keysHeld();     // buttons currently held
  
    // Right Shoulder button toggles the mode
		if ( pressed and KEY_R) <> 0 then Inc(TouchType);

		if TouchType = ttContinuous then 
      printf(#27 + '[14;4H' + 'Touch mode: CONTINUOUS ')
    else
      printf(#27 + '[14;4H' + 'Touch mode: SINGLE SHOT');

    iprintf(#27'[6;5H' + 'Touch x = %04X, %04X'#10, touch.rawx, touch.px);
    iprintf(#27'[7;5H' + 'Touch y = %04X, %04X'#10, touch.rawy, touch.py);
    
    
    iprintf(#27'[0;18H' + 'keys: %08X'#10, keysHeld());
    iprintf(#27'[9;10H' + 'Frame %d'#10, frame);
  
		if (TouchType = ttSingle) and not ( (pressed and KEY_TOUCH) <> 0) then continue;

		if ((held and KEY_TOUCH) = 0) or (touch.rawx = 0) or (touch.rawy = 0) then continue;

    
    iprintf(#27'[12;12H' + '(%d,%d)      ', touch.px, touch.py);

    if ( touch.rawx > max_x)	then	max_x := touch.rawx;
    if ( touch.rawy > max_y)		then	max_y := touch.rawy;
    if ( touch.px > max_px)	then	max_px := touch.px;
    if ( touch.py > max_py)	then	max_py := touch.py;
    
    if ( touch.rawx < min_x)		then	min_x := touch.rawx;
    if ( touch.rawy < min_y)		then	min_y := touch.rawy;
    if ( touch.px < min_px)	then	min_px := touch.px;
    if ( touch.py < min_py)	then	min_py := touch.py;

    iprintf(#27'[0;0H' + '(%d,%d)      ', min_px, min_py);
    iprintf(#27'[1;0H' + '(%d,%d)      ', min_x, min_y);
    iprintf(#27'[22;21H' + '(%d,%d)', max_x, max_y);
    iprintf(#27'[23;23H' + '(%d,%d)', max_px, max_py);
   
    OAMCopy[0].attribute[2] := 0;
    OAMCopy[0].attribute[1] := ATTR1_SIZE_32 or ((touch.px - 16) and $01FF);
    OAMCopy[0].attribute[0] := ATTR0_COLOR_256 or ATTR0_SQUARE or ((touch.py - 16) and $00FF);
  
  end;
  
end.

