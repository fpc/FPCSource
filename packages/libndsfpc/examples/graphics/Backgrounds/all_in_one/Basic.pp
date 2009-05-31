unit basic;

interface

uses
  ctypes, nds9, scrolling, TextBackgrounds, RotBackgrounds, Multilayer;


procedure Text256x256();
procedure Text256x512();
procedure Text512x256();
procedure Text512x512();
procedure ExRot128x128();
procedure ExRot256x256();
procedure ExRot512x512();
procedure ExRot1024x1024();
procedure Rot128x128();
procedure Rot256x256();
procedure Rot512x512();
procedure Rot1024x1024();
procedure Bmp8_128x128();
procedure Bmp8_256x256();
procedure Bmp8_512x256();
procedure Bmp8_512x512();
procedure Bmp8_512x1024();
procedure Bmp8_1024x512();
procedure Bmp16_128x128();
procedure Bmp16_512x256();
procedure Bmp16_256x256();
procedure Bmp16_512x512();



implementation

procedure Text256x256();
var
  bg: cint;
begin
  videoSetMode(MODE_0_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(0, BgType_Text8bpp, BgSize_T_256x256, 0,1);

  dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
  dmaCopy(@Layer256x256Map, bgGetMapPtr(bg),  Layer256x256MapLen);
  dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

  scroll(bg, 256, 256);
end;

procedure Text256x512();
var
  bg: cint;
begin
  videoSetMode(MODE_0_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(0, BgType_Text8bpp, BgSize_T_256x512, 0,1);

  dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
  dmaCopy(@Layer256x512Map, bgGetMapPtr(bg),  Layer256x512MapLen);
  dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

  scroll(bg, 256, 512);
end;

procedure Text512x256();
var
  bg: cint;
  map: pcuint16;
  iy, ix: integer;
begin
  videoSetMode(MODE_0_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(0, BgType_Text8bpp, BgSize_T_512x256, 0, 1);

  dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
  dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

  map := pcuint16(bgGetMapPtr(bg));

  for iy := 0 to 31 do
  begin
    //first the left half
		dmaCopy(@Layer512x256Map + iy * 64 * 2, @map[iy * 32],  32 * 2);

		//then the right half
		dmaCopy(@Layer512x256Map + iy * 64 * 2 + 64, @map[(32 * 32) + (iy * 32)],  32 * 2);
  end;

  scroll(bg, 512, 256);
end;

procedure Text512x512();
var
  bg: cint;
  map: pcuint16;
  iy, ix: integer;
begin

  videoSetMode(MODE_0_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(0, BgType_Text8bpp, BgSize_T_512x512, 0,1);

  dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
  dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

  map := pcuint16(bgGetMapPtr(bg));

  //draw top half
  for iy := 0 to 31 do
  begin
    //first the left half
    dmaCopy(@Layer512x512Map + iy * 64 * 2, map + iy * 32, 32 * 2);

    //then the right half
    dmaCopy(@Layer512x512Map + iy * 64 * 2 + 64, map + (32 * 32) + iy * 32, 32 * 2);
  end;

  map := map + (32 * 32 * 2);

  //draw bottom half
  for iy := 0 to 31 do
  begin
    //copy one line at a time
    //first the left half
    dmaCopy(@Layer512x512Map + (iy + 32) * 64 * 2, map + iy * 32,  32 * 2);

    //then the right half
    dmaCopy(@Layer512x512Map + (iy + 32) * 64 * 2 + 64, map + (32 * 32) + iy * 32,  32 * 2);
  end;

  scroll(bg, 512, 512);
end;

procedure ExRot128x128();
var
  bg: cint;
begin

  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_ExRotation, BgSize_ER_128x128, 0, 1);

  dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
  dmaCopy(@Layer128x128Map, bgGetMapPtr(bg),  Layer128x128MapLen);
  dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

  scroll(bg, 128, 128);
end;

procedure ExRot256x256();
var
  bg: cint;
begin

  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_ExRotation, BgSize_ER_256x256, 0,1);

  dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
  dmaCopy(@Layer256x256Map, bgGetMapPtr(bg),  Layer256x256MapLen);
  dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));

  scroll(bg, 256, 256);
end;


procedure ExRot512x512();
var
  bg: cint;
begin

  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_ExRotation, BgSize_ER_512x512, 0,1);

  dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
  dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));
  dmaCopy(@Layer512x512Map, bgGetMapPtr(bg),  Layer512x512MapLen);

  scroll(bg, 512, 512);
end;

procedure ExRot1024x1024();
var
  bg: cint;
begin

  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_ExRotation, BgSize_ER_1024x1024, 0,2);

  dmaCopy(@TextBackgroundsTiles, bgGetGfxPtr(bg), sizeof(TextBackgroundsTiles));
  dmaCopy(@TextBackgroundsPal, BG_PALETTE, sizeof(TextBackgroundsPal));
  dmaCopy(@Layer1024x1024Map, bgGetMapPtr(bg),  Layer1024x1024MapLen);

  scroll(bg, 1024, 1024);
end;

procedure Rot128x128();
var
  bg: cint;
begin

  videoSetMode(MODE_2_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_Rotation, BgSize_R_128x128, 0, 1);

  dmaCopy(@RotBackgroundsTiles, bgGetGfxPtr(bg), sizeof(RotBackgroundsTiles));
  dmaCopy(@RotBackgroundsPal, BG_PALETTE, sizeof(RotBackgroundsPal));
  dmaCopy(@Layer128x128rMap, bgGetMapPtr(bg),  Layer128x128rMapLen);

  scroll(bg, 128, 128);
end;

procedure Rot256x256();
var
  bg: cint;
begin

  videoSetMode(MODE_2_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_Rotation, BgSize_R_256x256, 0,2);

  dmaCopy(@RotBackgroundsTiles, bgGetGfxPtr(bg), sizeof(RotBackgroundsTiles));
  dmaCopy(@RotBackgroundsPal, BG_PALETTE, sizeof(RotBackgroundsPal));
  dmaCopy(@Layer256x256rMap, bgGetMapPtr(bg),  Layer256x256rMapLen);

  scroll(bg, 256, 256);
end;


procedure Rot512x512();
var
  bg: cint;
begin

  videoSetMode(MODE_2_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_Rotation, BgSize_R_512x512, 0,2);

  dmaCopy(@RotBackgroundsTiles, bgGetGfxPtr(bg), sizeof(RotBackgroundsTiles));
  dmaCopy(@RotBackgroundsPal, BG_PALETTE, sizeof(RotBackgroundsPal));
  dmaCopy(@Layer512x512rMap, bgGetMapPtr(bg),  Layer512x512rMapLen);

  scroll(bg, 512, 512);
end;

procedure Rot1024x1024();
var
  bg: cint;
begin

  videoSetMode(MODE_2_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_Rotation, BgSize_R_1024x1024, 0,3);

  dmaCopy(@RotBackgroundsTiles, bgGetGfxPtr(bg), sizeof(RotBackgroundsTiles));
  dmaCopy(@RotBackgroundsPal, BG_PALETTE, sizeof(RotBackgroundsPal));
  dmaCopy(@Layer1024x1024rMap, bgGetMapPtr(bg),  Layer1024x1024rMapLen);

  scroll(bg, 1024, 1024);
end;



procedure Bmp8_128x128();
var
  bg: cint;
  buffer: pcuint16;
  i, iy, ix: integer;
begin
  randomize;
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_Bmp8, BgSize_B8_128x128,0,0);

  for i := 0 to 255 do
    BG_PALETTE[i] := random(high(cuint16));

  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 127 do
    for ix := 0 to (128 div 2) - 1 do
      buffer[ix + iy * 64] := random(high(cuint16));

  scroll(bg, 128, 128);
end;

procedure Bmp8_256x256();
var
  bg: cint;
  buffer: pcuint16;
  i, iy, ix: integer;
begin
  randomize;
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_Bmp8, BgSize_B8_256x256,0,0);

  for i := 0 to 255 do
    BG_PALETTE[i] := random(high(cuint16));

  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 255 do
    for ix := 0 to (256 div 2) - 1 do
      buffer[ix + iy * 128] := random(high(cuint16));

  scroll(bg, 256, 256);

end;

procedure Bmp8_512x256();
var
  bg: cint;
  buffer: pcuint16;
  iy, ix: integer;
  i: integer;
begin
  randomize;
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(3, BgType_Bmp8, BgSize_B8_512x256,0,0);

  for i := 0 to 255 do
    BG_PALETTE[i] := random(high(cuint16));

  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 255 do
    for ix := 0 to (512 div 2) - 1 do
      buffer[ix + iy * 256] := random(high(cuint16));

  scroll(bg, 512, 256);
end;

procedure Bmp8_512x512();
var
  bg: cint;
  buffer: pcuint16;
  iy, ix: integer;
  i: integer;
begin
  randomize;
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);
  vramSetBankB(VRAM_B_MAIN_BG);

  bg := bgInit(3, BgType_Bmp8, BgSize_B8_512x512,0,0);

  for i := 0 to 255 do
    BG_PALETTE[i] := random(high(cuint16));

  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 511 do
    for ix := 0 to (512 div 2) -1 do
      buffer[ix + iy * 256] := random(high(cuint16));

  scroll(bg, 512, 512);
end;

procedure Bmp8_512x1024();
var
  bg: cint;
  buffer: pcuint16;
  iy, ix: integer;
  i: integer;
begin
  randomize;
  videoSetMode(MODE_6_2D);
  vramSetBankA(VRAM_A_MAIN_BG);
  vramSetBankB(VRAM_B_MAIN_BG);
  vramSetBankC(VRAM_C_MAIN_BG);
  vramSetBankD(VRAM_D_MAIN_BG);

  bg := bgInit(2, BgType_Bmp8, BgSize_B8_512x1024,0,0);

  for i := 0 to 255 do
    BG_PALETTE[i] := random(high(cuint16));

  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 1023 do
    for ix := 0  to (512 div 2) -1 do
      buffer[ix + iy * 256] := random(high(cuint16));

  scroll(bg, 512, 1024);
end;

procedure Bmp8_1024x512();
var
  bg: cint;
  buffer: pcuint16;
  iy, ix: integer;
  i: integer;
begin
  randomize;
  videoSetMode(MODE_6_2D);
  vramSetBankA(VRAM_A_MAIN_BG);
  vramSetBankB(VRAM_B_MAIN_BG);
  vramSetBankC(VRAM_C_MAIN_BG);
  vramSetBankD(VRAM_D_MAIN_BG);

  bg := bgInit(2, BgType_Bmp8, BgSize_B8_1024x512,0,0);

  for i := 0 to 255 do
    BG_PALETTE[i] := random(high(cuint16));

  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 511 do
    for ix := 0 to (1024 div 2) -1 do
      buffer[ix + iy * 512] := random(high(cuint16));

  scroll(bg, 1024, 512);
end;

procedure Bmp16_128x128();
var
  bg: cint;
  buffer: pcuint16;
  iy, ix: integer;
begin
  randomize;
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(2, BgType_Bmp16, BgSize_B16_128x128,0,0);

  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 127 do
    for ix := 0 to 127 do
      buffer[ix + iy * 128] := random(high(cuint16));

  scroll(bg, 128, 128);
end;

procedure Bmp16_256x256();
var
  bg: cint;
  buffer: pcuint16;
  iy, ix: integer;
begin
  randomize;
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);

  bg := bgInit(2, BgType_Bmp16, BgSize_B16_256x256,0,0);


  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 255 do
    for ix := 0 to 255 do
      buffer[ix + iy * 256] := random(high(cuint16));

  scroll(bg, 256, 256);
end;

procedure Bmp16_512x256();
var
  bg: cint;
  buffer: pcuint16;
  iy, ix: integer;
begin
  randomize;
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);
  vramSetBankB(VRAM_B_MAIN_BG);

  bg := bgInit(2, BgType_Bmp16, BgSize_B16_512x256,0,0);


  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 255 do
    for ix := 0 to 512 do
      buffer[ix + iy * 512] := random(high(cuint16));

  scroll(bg, 512, 256);
end;

procedure Bmp16_512x512();
var
  bg: cint;
  buffer: pcuint16;
  iy, ix: integer;
begin
  randomize;
  videoSetMode(MODE_5_2D);
  vramSetBankA(VRAM_A_MAIN_BG);
  vramSetBankB(VRAM_B_MAIN_BG);
  vramSetBankC(VRAM_C_MAIN_BG);
  vramSetBankD(VRAM_D_MAIN_BG);

  bg := bgInit(2, BgType_Bmp16, BgSize_B16_512x512,0,0);


  buffer := pcuint16(bgGetGfxPtr(bg));

  for iy := 0 to 511 do
    for ix := 0 to 511 do
      buffer[ix + iy * 512] := random(high(cuint16));

  scroll(bg, 512, 512);
end;


end.
