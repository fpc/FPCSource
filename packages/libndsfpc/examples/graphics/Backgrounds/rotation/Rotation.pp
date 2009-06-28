program BG_Rotation;
{$L build/drunkenlogo.bin.o}
{$L build/palette.bin.o}

uses
  ctypes, nds9;

{$include inc/drunkenlogo.bin.inc}
{$include inc/palette.bin.inc}

var
  angle: u32;
  scrollX, scrollY: s16;
  scaleX, scaleY: s16;
  rcX, rcY: s16;
  keys: u32;
  s, c: s16;
  bg3: integer;


begin
  videoSetMode(MODE_5_2D );

  vramSetBankA(VRAM_A_MAIN_BG);

  consoleDemoInit();


  bg3 := bgInit(3, BgType_Bmp8, BgSize_B8_256x256, 0,0);


  dmaCopy(@drunkenlogo_bin, bgGetGfxPtr(bg3), 256*256);
  dmaCopy(@palette_bin, BG_PALETTE, 256*2);

  angle := 0;

  // the screen origin is at the rotation center...so scroll to the rotation
  // center + a small 32 pixle offset so our image is centered
  scrollX := 128;
  scrollY := 128 ;

  //scale is fixed point
  scaleX := 1 shl 8;
  scaleY := 1 shl 8;

  //this is the screen pixel that the image will rotate about
  rcX := 128;
  rcY := 96;

  while true do
  begin
    printf(#10#10#9 + 'Hello DS devers' + #10);
    printf(#9 + 'www.drunkencoders.com' + #10);
    printf(#9 + 'BG Rotation demo' + #10);

    iprintf('Angle %3d(actual) %3d(degrees)' + #10, angle, (angle * 360) div (1 shl 15));
    iprintf('Scroll  X: %4d Y: %4d' + #10, scrollX, scrollY);
    iprintf('Rot center X: %4d Y: %4d' + #10, rcX, rcY);
    iprintf('Scale X: %4d Y: %4d' + #10, scaleX, scaleY);

    scanKeys();
    keys := keysHeld();

    if ( keys and KEY_L ) <> 0 then angle := angle + 20;
    if ( keys and KEY_R ) <> 0 then angle := angle - 20;
    if ( keys and KEY_LEFT ) <> 0 then scrollX := scrollX + 1;
    if ( keys and KEY_RIGHT ) <> 0 then  scrollX := scrollX - 1;
    if ( keys and KEY_UP ) <> 0 then scrollY := scrollY + 1;
    if ( keys and KEY_DOWN ) <> 0 then scrollY := scrollY - 1;
    if ( keys and KEY_A ) <> 0 then scaleX := scaleX + 1;
    if ( keys and KEY_B ) <> 0 then scaleX := scaleX - 1;
    if ( keys and KEY_START ) <> 0 then rcX := rcX + 1;
    if ( keys and KEY_SELECT ) <> 0 then rcY := rcY + 1;
    if ( keys and KEY_X ) <> 0 then scaleY := scaleY + 1;
    if ( keys and KEY_Y ) <> 0 then scaleY := scaleY - 1;


    swiWaitForVBlank();

    bgSetCenter(bg3, rcX, rcY);
    bgSetRotateScale(bg3, angle, scaleX, scaleY);
    bgSetScroll(bg3, scrollX, scrollY);
    bgUpdate();

    // clear the console screen (ansi escape sequence)
    iprintf(#$1b'[2J');

  end;
end.
