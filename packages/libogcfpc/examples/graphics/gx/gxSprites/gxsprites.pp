program gxsprites;

{$J+}
{$macro on}
{$mode objfpc}

uses
  cmem, ctypes, gctypes, gccore;

{$include inc/textures.tpl.inc}
{$link build/textures.tpl.o}

const
  ballsprites = 0;

const
  DEFAULT_FIFO_SIZE = (256 * 1024);
const
  RAND_MAX = 2147483647;
var
  frameBuffer: array [0..1] of pcuint32 = (nil, nil);
  rmode: PGXRModeObj = nil;

const
  NUM_SPRITES = 1024;

  //simple sprite struct
type
  TSprite = record
    x, y: cint;     // screen co-ordinates
    dx, dy: cint;     // velocity
    image: cint;
  end;

var
  sprites: array [0..NUM_SPRITES - 1] of TSprite;
  texObj: GXTexObj;

  fb: cuint32;  // initial framebuffer index
  first_frame: cuint32;
  yscale: f32;
  xfbHeight: cuint32;
  perspective: Mtx44;
  GXmodelView2D: Mtx;
  gp_fifo: pointer = nil;

  background: GXColor = (r: 0; g: 0; b: 0; a: $ff; );

  i: cint;
  spriteTPL: TPLFile;

  texCoords: array[0..31] of f32 = (
    0.0, 0.0, 0.5, 0.0, 0.5, 0.5, 0.0, 0.5,
    0.5, 0.0, 1.0, 0.0, 1.0, 0.5, 0.5, 0.5,
    0.0, 0.5, 0.5, 0.5, 0.5, 1.0, 0.0, 1.0,
    0.5, 0.5, 1.0, 0.5, 1.0, 1.0, 0.5, 1.0);


  procedure drawSpriteTex(x, y, Width, Height, image: cint);
  var
    texIndex: cint;
  begin
    texIndex := image * 8;

    GX_Begin(GX_QUADS, GX_VTXFMT0, 4);      // Draw A Quad
    GX_Position2f32(x, y);          // Top Left
    GX_TexCoord2f32(texCoords[texIndex], texCoords[texIndex + 1]);
    texIndex := texIndex + 2;
    GX_Position2f32(x + Width - 1, y);      // Top Right
    GX_TexCoord2f32(texCoords[texIndex], texCoords[texIndex + 1]);
    texIndex := texIndex + 2;
    GX_Position2f32(x + Width - 1, y + Height - 1);  // Bottom Right
    GX_TexCoord2f32(texCoords[texIndex], texCoords[texIndex + 1]);
    texIndex := texIndex + 2;
    GX_Position2f32(x, y + Height - 1);      // Bottom Left
    GX_TexCoord2f32(texCoords[texIndex], texCoords[texIndex + 1]);
    GX_End();                 // Done Drawing The Quad
  end;



begin
  VIDEO_Init();

  rmode := VIDEO_GetPreferredMode(nil);

  fb := 0;
  first_frame := 1;
  // allocate 2 framebuffers for double buffering
  frameBuffer[0] := (SYS_AllocateFramebuffer(rmode));
  frameBuffer[1] := (SYS_AllocateFramebuffer(rmode));


  VIDEO_Configure(rmode);
  VIDEO_SetNextFramebuffer(frameBuffer[fb]);
  VIDEO_SetBlack(False);
  VIDEO_Flush();
  VIDEO_WaitVSync();
  if (rmode^.viTVMode and VI_NON_INTERLACE) <> 0 then
    VIDEO_WaitVSync();

  // setup the fifo and then init the flipper
  gp_fifo := memalign(32, DEFAULT_FIFO_SIZE);
  memset(gp_fifo, 0, DEFAULT_FIFO_SIZE);

  fb := fb xor 1;

  GX_Init(gp_fifo, DEFAULT_FIFO_SIZE);

  // clears the bg to color and clears the z buffer
  GX_SetCopyClear(background, $00ffffff);

  // other gx setup
  GX_SetViewport(0, 0, rmode^.fbWidth, rmode^.efbHeight, 0, 1);
  yscale := GX_GetYScaleFactor(rmode^.efbHeight, rmode^.xfbHeight);
  xfbHeight := GX_SetDispCopyYScale(yscale);
  GX_SetScissor(0, 0, rmode^.fbWidth, rmode^.efbHeight);
  GX_SetDispCopySrc(0, 0, rmode^.fbWidth, rmode^.efbHeight);
  GX_SetDispCopyDst(rmode^.fbWidth, xfbHeight);
  GX_SetCopyFilter(rmode^.aa, rmode^.sample_pattern, GX_TRUE, rmode^.vfilter);

  if rmode^.viHeight = 2 * rmode^.xfbHeight then
    GX_SetFieldMode(rmode^.field_rendering, GX_ENABLE)
  else
    GX_SetFieldMode(rmode^.field_rendering, GX_DISABLE);

  if (rmode^.aa) <> 0 then
    GX_SetPixelFmt(GX_PF_RGB565_Z16, GX_ZC_LINEAR)
  else
    GX_SetPixelFmt(GX_PF_RGB8_Z24, GX_ZC_LINEAR);

  GX_SetCullMode(GX_CULL_NONE);
  GX_CopyDisp(frameBuffer[fb], GX_TRUE);
  GX_SetDispCopyGamma(GX_GM_1_0);

  // setup the vertex descriptor
  // tells the flipper to expect direct data
  GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_POS, GX_POS_XY, GX_F32, 0);
  GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_TEX0, GX_TEX_ST, GX_F32, 0);


  GX_SetNumChans(1);
  GX_SetNumTexGens(1);
  GX_SetTevOp(GX_TEVSTAGE0, GX_REPLACE);
  GX_SetTevOrder(GX_TEVSTAGE0, GX_TEXCOORD0, GX_TEXMAP0, GX_COLOR0A0);
  GX_SetTexCoordGen(GX_TEXCOORD0, GX_TG_MTX2x4, GX_TG_TEX0, GX_IDENTITY);

  GX_InvalidateTexAll();

  TPL_OpenTPLFromMemory(@spriteTPL, @textures_tpl, textures_tpl_size);
  TPL_GetTexture(@spriteTPL, ballsprites, @texObj);
  GX_LoadTexObj(@texObj, GX_TEXMAP0);

  guOrtho(perspective, 0, 479, 0, 639, 0, 300);
  GX_LoadProjectionMtx(perspective, GX_ORTHOGRAPHIC);

  WPAD_Init();

  randomize();

  for i := 0 to NUM_SPRITES - 1 do
  begin
    //random place and speed
    sprites[i].x := random(640 - 32 + 1) shl 8;
    sprites[i].y := random(480 - 32 + 1) shl 8;
    sprites[i].dx := random($FF + 1) + $100;
    sprites[i].dy := random($FF + 1) + $100;
    sprites[i].image := random(4);

    if random(2) <> 0 then
      sprites[i].dx := -sprites[i].dx;
    if random(2) <> 0 then
      sprites[i].dy := -sprites[i].dy;
  end;

  while True do
  begin

    WPAD_ScanPads();

    if (WPAD_ButtonsDown(0) and WPAD_BUTTON_HOME) <> 0 then
      exit;

    GX_SetViewport(0, 0, rmode^.fbWidth, rmode^.efbHeight, 0, 1);
    GX_InvVtxCache();
    GX_InvalidateTexAll();

    GX_ClearVtxDesc();
    GX_SetVtxDesc(GX_VA_POS, GX_DIRECT);
    GX_SetVtxDesc(GX_VA_TEX0, GX_DIRECT);

    guMtxIdentity(GXmodelView2D);
    guMtxTransApply(GXmodelView2D, GXmodelView2D, 0.0, 0.0, -5.0);
    GX_LoadPosMtxImm(GXmodelView2D, GX_PNMTX0);

    for i := 0 to NUM_SPRITES - 1 do
    begin
      sprites[i].x := sprites[i].x + sprites[i].dx;
      sprites[i].y := sprites[i].y + sprites[i].dy;

      //check for collision with the screen boundaries
      if (sprites[i].x < (1 shl 8)) or (sprites[i].x > ((640 - 32) shl 8)) then
        sprites[i].dx := -sprites[i].dx;

      if (sprites[i].y < (1 shl 8)) or (sprites[i].y > ((480 - 32) shl 8)) then
        sprites[i].dy := -sprites[i].dy;

      drawSpriteTex(sprites[i].x shr 8, sprites[i].y shr 8, 32, 32, sprites[i].image);
    end;

    GX_DrawDone();

    GX_SetZMode(GX_TRUE, GX_LEQUAL, GX_TRUE);
    GX_SetBlendMode(GX_BM_BLEND, GX_BL_SRCALPHA, GX_BL_INVSRCALPHA, GX_LO_CLEAR);
    GX_SetAlphaUpdate(GX_TRUE);
    GX_SetColorUpdate(GX_TRUE);
    GX_CopyDisp(frameBuffer[fb], GX_TRUE);

    VIDEO_SetNextFramebuffer(frameBuffer[fb]);
    if (first_frame) <> 0 then
    begin
      VIDEO_SetBlack(False);
      first_frame := 0;
    end;
    VIDEO_Flush();
    VIDEO_WaitVSync();
    fb := fb xor 1;   // flip framebuffer
  end;

end.

