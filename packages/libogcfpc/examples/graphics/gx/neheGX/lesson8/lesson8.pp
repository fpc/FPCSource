program lesson8;

{$J+}
{$macro on}
{$mode objfpc}

uses
  cmem, ctypes, gctypes, gccore, math;

{$include inc/glass.tpl.inc}
{$link build/glass.tpl.o}

const glass = 0;

const 
  DEFAULT_FIFO_SIZE = (256 * 1024);

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

var
  litcolors: array [0..2] of GXColor = (
        (r:$D0; g:$D0; b:$D0; a:$FF;), // Light color 1
        (r:$40; g:$40; b:$40; a:$FF;), // Ambient 1
        (r:$80; g:$80; b:$80; a:$FF;));  // Material 1

  frameBuffer: array [0..1] of pcuint32 = (nil, nil);
  rmode: PGXRModeObj = nil;

procedure setlight(view: Mtx; theta, phi: cuint32; litcol, ambcol, matcol: GXColor);
var
	lpos: guVector;
	_theta, _phi: f32;
	lobj: GXLightObj;
begin
	_theta := f32(theta * M_PI / 180.0);
	_phi := f32(phi * M_PI / 180.0);
	lpos.x := 1000.0 * cos(_phi) * sin(_theta);
	lpos.y := 1000.0 * sin(_phi);
	lpos.z := 1000.0 * cos(_phi) * cos(_theta);

	guVecMultiply(view,@lpos,@lpos);

	GX_InitLightPos(@lobj,lpos.x,lpos.y,lpos.z);
	GX_InitLightColor(@lobj,litcol);
	GX_LoadLightObj(@lobj,GX_LIGHT0);
	
	// set number of rasterized color channels
	GX_SetNumChans(1);
    GX_SetChanCtrl(GX_COLOR0A0,GX_ENABLE,GX_SRC_REG,GX_SRC_REG,GX_LIGHT0,GX_DF_CLAMP,GX_AF_NONE);
    GX_SetChanAmbColor(GX_COLOR0A0,ambcol);
    GX_SetChanMatColor(GX_COLOR0A0,matcol);
end;

var
	yscale: f32 = 0; 
  zt: f32 = 0;
	xfbHeight: cuint32;
	fb: cuint32 = 0;
	rquad: f32 = 0.0;
	first_frame: cuint32 = 1;
	texture: GXTexObj;
	view, mv, mr, mvi: Mtx; // view and perspective matrices
	model, modelview: Mtx;
	perspective: Mtx44;
	gpfifo: pointer = nil;
	background: GXColor = (r:0; g:0; b:0; a:$ff;);
	cam: guVector = (x:0.0; y:0.0; z:0.0;);
	up: guVector = (x:0.0; y:1.0; z:0.0;);
	look: guVector = (x:0.0; y:0.0; z:-1.0;);
  cubeAxis: guVector  = (x: 1.0; y: 1.0; z: 1.0;);
	glassTPL: TPLFile;

  w, h: f32;

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
    GX_InvVtxCache();
	GX_ClearVtxDesc();
	GX_SetVtxDesc(GX_VA_POS, GX_DIRECT);
	GX_SetVtxDesc(GX_VA_NRM, GX_DIRECT);
	GX_SetVtxDesc(GX_VA_TEX0, GX_DIRECT);

	GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_POS, GX_POS_XYZ, GX_F32, 0);
	GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_NRM, GX_NRM_XYZ, GX_F32, 0);
	GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_TEX0, GX_TEX_ST, GX_F32, 0);

	// setup texture coordinate generation
	// args: texcoord slot 0-7, matrix type, source to generate texture coordinates from, matrix to use
	GX_SetTexCoordGen(GX_TEXCOORD0, GX_TG_MTX3x4, GX_TG_TEX0, GX_TEXMTX0);

	w := rmode^.viWidth;
	h := rmode^.viHeight;
	guLightPerspective(mv,45, f32(w/h), 1.05, 1.0, 0.0, 0.0);
    guMtxTrans(mr, 0.0, 0.0, -1.0);
    guMtxConcat(mv, mr, mv);
    GX_LoadTexMtxImm(mv, GX_TEXMTX0, GX_MTX3x4);

	GX_InvalidateTexAll();
	TPL_OpenTPLFromMemory(@glassTPL, @glass_tpl[0], glass_tpl_size);
	TPL_GetTexture(@glassTPL,glass,@texture);
	
	// setup our camera at the origin
	// looking down the -z axis with y up
	guLookAt(view, @cam, @up, @look);
 
	// setup our projection matrix
	// this creates a perspective matrix with a view angle of 90,
	// and aspect ratio based on the display resolution
	guPerspective(perspective, 45, f32(w/h), 0.1, 300.0);
	GX_LoadProjectionMtx(perspective, GX_PERSPECTIVE);

	while true do
	begin
		WPAD_ScanPads();
		if (WPAD_ButtonsDown(0) and WPAD_BUTTON_HOME) <> 0 then exit
		else 
    if (WPAD_ButtonsHeld(0) and WPAD_BUTTON_UP) <> 0 then zt := zt - 0.25
		else 
    if (WPAD_ButtonsHeld(0) and WPAD_BUTTON_DOWN) <> 0 then zt := zt + 0.25;

		// set number of rasterized color channels
		//GX_SetNumChans(1);
		setlight(view,8,20,litcolors[0],litcolors[1],litcolors[2]);

		//set number of textures to generate
		GX_SetNumTexGens(1);

		GX_SetTevOp(GX_TEVSTAGE0,GX_BLEND);
		GX_SetTevOrder(GX_TEVSTAGE0, GX_TEXCOORD0, GX_TEXMAP0, GX_COLOR0A0);

		GX_LoadTexObj(@texture, GX_TEXMAP0);

		guMtxIdentity(model);
		guMtxRotAxisDeg(model, @cubeAxis, rquad);
		guMtxTransApply(model, model, 0.0,0.0,zt-7.0);
		guMtxConcat(view,model,modelview);

		// load the modelview matrix into matrix memory
		GX_LoadPosMtxImm(modelview, GX_PNMTX0);
		
		guMtxInverse(modelview,mvi);
		guMtxTranspose(mvi,modelview);
	    GX_LoadNrmMtxImm(modelview, GX_PNMTX0);

		GX_Begin(GX_QUADS, GX_VTXFMT0, 24);			// Draw a Cube

			GX_Position3f32(-1.0,1.0,1.0);	// Top Left of the quad (top)
			GX_Normal3f32(-1.0,1.0,1.0);
			GX_TexCoord2f32(1.0,0.0);
			GX_Position3f32(1.0,1.0,1.0);	// Top Right of the quad (top)
			GX_Normal3f32(1.0,1.0,1.0);	// Top Right of the quad (top)
			GX_TexCoord2f32(1.0,1.0);
			GX_Position3f32(1.0,1.0,-1.0);	// Bottom Right of the quad (top)
			GX_Normal3f32(1.0,1.0,-1.0);	// Bottom Right of the quad (top)
			GX_TexCoord2f32(0.0,1.0);
			GX_Position3f32(-1.0,1.0,-1.0);		// Bottom Left of the quad (top)
			GX_Normal3f32(-1.0,1.0,-1.0);		// Bottom Left of the quad (top)
			GX_TexCoord2f32(0.0,0.0);

			GX_Position3f32(-1.0,-1.0,1.0);	// Top Left of the quad (bottom)
			GX_Normal3f32(-1.0,-1.0,1.0);	// Top Left of the quad (bottom)
			GX_TexCoord2f32(1.0,0.0);
			GX_Position3f32(1.0,-1.0,1.0);	// Top Right of the quad (bottom)
			GX_Normal3f32(1.0,-1.0,1.0);	// Top Right of the quad (bottom)
			GX_TexCoord2f32(1.0,1.0);
			GX_Position3f32(1.0,-1.0,-1.0);	// Bottom Right of the quad (bottom)
			GX_Normal3f32(1.0,-1.0,-1.0);	// Bottom Right of the quad (bottom)
			GX_TexCoord2f32(0.0,1.0);
			GX_Position3f32(-1.0,-1.0,-1.0);		// Bottom Left of the quad (bottom)
			GX_Normal3f32(-1.0,-1.0,-1.0);		// Bottom Left of the quad (bottom)
			GX_TexCoord2f32(0.0,0.0);

			GX_Position3f32(-1.0,1.0,1.0);	// Top Left of the quad (front)
			GX_Normal3f32(-1.0,1.0,1.0);	// Top Left of the quad (front)
			GX_TexCoord2f32(1.0,0.0);
			GX_Position3f32(-1.0,-1.0,1.0);	// Top Right of the quad (front)
			GX_Normal3f32(-1.0,-1.0,1.0);	// Top Right of the quad (front)
			GX_TexCoord2f32(1.0,1.0);
			GX_Position3f32(1.0,-1.0,1.0);	// Bottom Right of the quad (front)
			GX_Normal3f32(1.0,-1.0,1.0);	// Bottom Right of the quad (front)
			GX_TexCoord2f32(0.0,1.0);
			GX_Position3f32(1.0,1.0,1.0);		// Bottom Left of the quad (front)
			GX_Normal3f32(1.0,1.0,1.0);		// Bottom Left of the quad (front)
			GX_TexCoord2f32(0.0,0.0);

			GX_Position3f32(-1.0,1.0,-1.0);	// Top Left of the quad (back)
			GX_Normal3f32(-1.0,1.0,-1.0);	// Top Left of the quad (back)
			GX_TexCoord2f32(1.0,0.0);
			GX_Position3f32(-1.0,-1.0,-1.0);	// Top Right of the quad (back)
			GX_Normal3f32(-1.0,-1.0,-1.0);	// Top Right of the quad (back)
			GX_TexCoord2f32(1.0,1.0);
			GX_Position3f32(1.0,-1.0,-1.0);	// Bottom Right of the quad (back)
			GX_Normal3f32(1.0,-1.0,-1.0);	// Bottom Right of the quad (back)
			GX_TexCoord2f32(0.0,1.0);
			GX_Position3f32(1.0,1.0,-1.0);		// Bottom Left of the quad (back)
			GX_Normal3f32(1.0,1.0,-1.0);		// Bottom Left of the quad (back)
			GX_TexCoord2f32(0.0,0.0);

			GX_Position3f32(-1.0,1.0,1.0);	// Top Left of the quad (left)
			GX_Normal3f32(-1.0,1.0,1.0);	// Top Left of the quad (left)
			GX_TexCoord2f32(1.0,0.0);
			GX_Position3f32(-1.0,1.0,-1.0);	// Top Right of the quad (back)
			GX_Normal3f32(-1.0,1.0,-1.0);	// Top Right of the quad (back)
			GX_TexCoord2f32(1.0,1.0);
			GX_Position3f32(-1.0,-1.0,-1.0);	// Bottom Right of the quad (back)
			GX_Normal3f32(-1.0,-1.0,-1.0);	// Bottom Right of the quad (back)
			GX_TexCoord2f32(0.0,1.0);
			GX_Position3f32(-1.0,-1.0,1.0);		// Bottom Left of the quad (back)
			GX_Normal3f32(-1.0,-1.0,1.0);		// Bottom Left of the quad (back)
			GX_TexCoord2f32(0.0,0.0);

			GX_Position3f32(1.0,1.0,1.0);	// Top Left of the quad (right)
			GX_Normal3f32(1.0,1.0,1.0);	// Top Left of the quad (right)
			GX_TexCoord2f32(1.0,0.0);
			GX_Position3f32(1.0,1.0,-1.0);	// Top Right of the quad (right)
			GX_Normal3f32(1.0,1.0,-1.0);	// Top Right of the quad (right)
			GX_TexCoord2f32(1.0,1.0);
			GX_Position3f32(1.0,-1.0,-1.0);	// Bottom Right of the quad (right)
			GX_Normal3f32(1.0,-1.0,-1.0);	// Bottom Right of the quad (right)
			GX_TexCoord2f32(0.0,1.0);
			GX_Position3f32(1.0,-1.0,1.0);		// Bottom Left of the quad (right)
			GX_Normal3f32(1.0,-1.0,1.0);		// Bottom Left of the quad (right)
			GX_TexCoord2f32(0.0,0.0);

		GX_End();									// Done Drawing The Quad 

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

		rquad := rquad - 0.15;				// Decrease The Rotation Variable For The Quad     ( NEW )
	end;
end.
