program lesson9;

{$J+}
{$macro on}
{$mode objfpc}

uses
  cmem, ctypes, gctypes, gccore;

{$include inc/startex.tpl.inc}
{$link build/startex.tpl.o}

const startex = 0;

const 
  DEFAULT_FIFO_SIZE = (256 * 1024);
  NUM_STARS = 50;

type
  tagtexdef  = record
    pal_data: pointer;
    tex_data: pointer;
    sz_x: cuint32;
    sz_y: cuint32;
    fmt: cuint32;
    min_lod: cuint32;
    max_lod: cuint32;
    min: cuint32;
    mag: cuint32;
    wrap_s: cuint32;
    wrap_t: cuint32;
    nextdef: pointer;
  end;
  texdef = tagtexdef;
  ptexdef = ^texdef;

  TStar = record
    r,g,b: cuint8;
    dist: f32;
    ang: f32;
  end;


var
  stars: array [0..NUM_STARS-1] of TStar;
  frameBuffer: array [0..1] of pcuint32 = (nil, nil);
  rmode: PGXRModeObj = nil;

	yscale: f32 = 0.0; 
  spin: f32 = 0.0;
	xfbHeight, i: cuint32;
	fb: cuint32 = 0;
	first_frame: cuint32 = 1;
	zoom: f32 = -15.0;
	texture: GXTexObj;
	view: Mtx; // view and perspective matrices
	model, modelview: Mtx;
	perspective: Mtx44;
	gpfifo: pointer = nil;
	background: GXColor = (r:0; g:0; b:0; a:$ff;);
	cam: guVector = (x:0.0; y:0.0; z:0.0;);
	up: guVector = (x:0.0; y:1.0; z:0.0;);
	look: guVector = (x:0.0; y:0.0; z:-1.0;);
	
	starAxis1: guVector  = (x:0; y:1; z:0;);
	starAxis2: guVector  = (x:0; y:0; z:1;);
	
	starTPL: TPLFile;
  w, h: f32;
  
	mry, mrz: Mtx;
  
begin
	VIDEO_Init();
	WPAD_Init();

	rmode := VIDEO_GetPreferredMode(nil);

	// allocate 2 framebuffers for double buffering
	frameBuffer[0] := SYS_AllocateFramebuffer(rmode);
	frameBuffer[1] := SYS_AllocateFramebuffer(rmode);

	// configure video
	VIDEO_Configure(rmode);
	VIDEO_SetNextFramebuffer(frameBuffer[fb]);
	VIDEO_Flush();
	VIDEO_WaitVSync();
	if (rmode^.viTVMode and VI_NON_INTERLACE) <> 0 then VIDEO_WaitVSync();

	// allocate the fifo buffer
	gpfifo := memalign(32,DEFAULT_FIFO_SIZE);
	memset(gpfifo,0,DEFAULT_FIFO_SIZE);

	fb := fb xor 1;

	// init the flipper
	GX_Init(gpfifo,DEFAULT_FIFO_SIZE);
 
	// clears the bg to color and clears the z buffer
	GX_SetCopyClear(background, $00ffffff);
 
	// other gx setup
	GX_SetViewport(0,0,rmode^.fbWidth,rmode^.efbHeight,0,1);
	yscale := GX_GetYScaleFactor(rmode^.efbHeight,rmode^.xfbHeight);
	xfbHeight := GX_SetDispCopyYScale(yscale);
	GX_SetScissor(0,0,rmode^.fbWidth,rmode^.efbHeight);
	GX_SetDispCopySrc(0,0,rmode^.fbWidth,rmode^.efbHeight);
	GX_SetDispCopyDst(rmode^.fbWidth,xfbHeight);
	GX_SetCopyFilter(rmode^.aa,rmode^.sample_pattern,GX_TRUE,rmode^.vfilter);
  if rmode^.viHeight = 2 * rmode^.xfbHeight then
    GX_SetFieldMode(rmode^.field_rendering, GX_ENABLE)
  else
    GX_SetFieldMode(rmode^.field_rendering, GX_DISABLE);
 
	if (rmode^.aa) <> 0 then
        GX_SetPixelFmt(GX_PF_RGB565_Z16, GX_ZC_LINEAR)
  else
        GX_SetPixelFmt(GX_PF_RGB8_Z24, GX_ZC_LINEAR);
 
	GX_SetCullMode(GX_CULL_NONE);
	GX_CopyDisp(frameBuffer[fb],GX_TRUE);
	GX_SetDispCopyGamma(GX_GM_1_0);

	// setup the vertex attribute table
	// describes the data
	// args: vat location 0-7, type of data, data format, size, scale
	// so for ex. in the first call we are sending position data with
	// 3 values X,Y,Z of size F32. scale sets the number of fractional
	// bits for non float data.
	GX_ClearVtxDesc();
	GX_SetVtxDesc(GX_VA_POS, GX_DIRECT);
	GX_SetVtxDesc(GX_VA_CLR0, GX_DIRECT);
	GX_SetVtxDesc(GX_VA_TEX0, GX_DIRECT);

	GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_POS, GX_POS_XYZ, GX_F32, 0);
	GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_TEX0, GX_TEX_ST, GX_F32, 0);
	GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_CLR0, GX_CLR_RGBA, GX_RGB8, 0);

	// set number of rasterized color channels
	GX_SetNumChans(1);

	//set number of textures to generate
	GX_SetNumTexGens(1);

	// setup texture coordinate generation
	// args: texcoord slot 0-7, matrix type, source to generate texture coordinates from, matrix to use
	GX_SetTexCoordGen(GX_TEXCOORD0, GX_TG_MTX2x4, GX_TG_TEX0, GX_IDENTITY);


    GX_InvVtxCache();
	GX_InvalidateTexAll();
	TPL_OpenTPLFromMemory(@starTPL, @startex_tpl[0],startex_tpl_size);
	TPL_GetTexture(@starTPL,startex,@texture);
	// setup our camera at the origin
	// looking down the -z axis with y up
	guLookAt(view, @cam, @up, @look);
 
	// setup our projection matrix
	// this creates a perspective matrix with a view angle of 90,
	// and aspect ratio based on the display resolution
  w := rmode^.viWidth;
  h := rmode^.viHeight;
	guPerspective(perspective, 45, f32(w/h), 0.1, 300.0);
	GX_LoadProjectionMtx(perspective, GX_PERSPECTIVE);

	randomize();
	
  for i:=0 to NUM_STARS - 1 do
  begin
		stars[i].ang := 0.0;
		stars[i].dist := f32(i)/f32(NUM_STARS);
		stars[i].r := random(256);			// Give star[loop] A Random Red Intensity
		stars[i].g := random(256);			// Give star[loop] A Random Green Intensity
		stars[i].b := random(256);			// Give star[loop] A Random Blue Intensity
	end;

	while true do
	begin

		WPAD_ScanPads();
		if (WPAD_ButtonsDown(0) and WPAD_BUTTON_HOME) <> 0 then exit;

		GX_SetTevOp(GX_TEVSTAGE0,GX_MODULATE);
		GX_SetTevOrder(GX_TEVSTAGE0, GX_TEXCOORD0, GX_TEXMAP0, GX_COLOR0A0);

		GX_LoadTexObj(@texture, GX_TEXMAP0);

		for i:=0 to NUM_STARS - 1 do
		begin

			guMtxIdentity(model);
			guMtxRotAxisDeg(mry, @starAxis1, stars[i].ang);
			guMtxRotAxisDeg(mrz, @starAxis2, spin);
			guMtxConcat(mry,mrz,model);
			guMtxTransApply(model, model, stars[i].dist,0.0,zoom);
			guMtxConcat(view,model,modelview);

			GX_LoadPosMtxImm(modelview, GX_PNMTX0);

			GX_Begin(GX_QUADS, GX_VTXFMT0, 4);					// Draw a quad
				GX_Position3f32(-1.0, -1.0, 0.0);			// Top Left of the quad (top)
				GX_Color3u8(stars[i].r,stars[i].g,stars[i].b);	// Set The Color To Green
				GX_TexCoord2f32(0.0,0.0);
				GX_Position3f32(1.0, -1.0, 0.0);				// Top Right of the quad (top)
				GX_Color3u8(stars[i].r,stars[i].g,stars[i].b);	// Set The Color To Green
				GX_TexCoord2f32(1.0,0.0);
				GX_Position3f32(1.0, 1.0, 0.0);				// Bottom Right of the quad (top)
				GX_Color3u8(stars[i].r,stars[i].g,stars[i].b);	// Set The Color To Green
				GX_TexCoord2f32(1.0,1.0);
				GX_Position3f32(-1.0, 1.0, 0.0);				// Bottom Left of the quad (top)
				GX_Color3u8(stars[i].r,stars[i].g,stars[i].b);	// Set The Color To Green
				GX_TexCoord2f32(0.0,1.0);
			GX_End();											// Done Drawing The Quad 

			spin := spin + 0.01;
			stars[i].ang := stars[i].ang + f32(i)/f32(NUM_STARS);
			stars[i].dist := stars[i].dist - 0.01;

			if (stars[i].dist < 0.0) then			// Is The Star In The Middle Yet
			begin
				stars[i].dist := stars[i].dist + 5.0;			// Move The Star 5 Units From The Center
				stars[i].r := random(256);		// Give It A New Red Value
				stars[i].g := random(256);		// Give It A New Green Value
				stars[i].b := random(256);		// Give It A New Blue Value
			end;
		end;

		GX_SetZMode(GX_TRUE, GX_LEQUAL, GX_TRUE);
		GX_SetBlendMode(GX_BM_BLEND, GX_BL_SRCALPHA, GX_BL_ONE, GX_LO_SET);
		GX_SetColorUpdate(GX_TRUE);
		GX_SetAlphaUpdate(GX_TRUE);
		GX_CopyDisp(frameBuffer[fb],GX_TRUE);

		GX_DrawDone();

		VIDEO_SetNextFramebuffer(frameBuffer[fb]);
		if (first_frame) <> 0 then
		begin
			first_frame := 0;
			VIDEO_SetBlack(FALSE);
		end;
		VIDEO_Flush();
 		VIDEO_WaitVSync();
		fb := fb xor 1;
	end;
end.
