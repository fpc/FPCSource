program lesson1;
{$J+}
{$macro on}
{$mode objfpc}

uses
  cmem, ctypes, gctypes, gccore;
 
const
  DEFAULT_FIFO_SIZE = (256*1024);
  
var
  frameBuffer: array [0..1] of pcuint32 = (nil, nil);
  rmode: PGXRModeObj = nil;
	yscale: f32;

	xfbHeight: cuint32;

	view: Mtx;
	perspective: Mtx44;
  fb: cuint32 = 0;
  background: GXColor = (r:0; g:0; b:0; a:$ff;);
  gp_fifo: pointer = nil;
  
	cam: guVector  = (x:0.0; y:0.0; z:0.0;);
	up: guVector  = (x:0.0; y:1.0; z:0.0;);
	look: guVector  = (x:0.0; y:0.0; z:-1.0;);
  
  w,h: f32;
begin

	VIDEO_Init();
	WPAD_Init();
 
	rmode := VIDEO_GetPreferredMode(nil);
	
	// allocate 2 framebuffers for double buffering
	frameBuffer[0] := MEM_K0_TO_K1(integer(SYS_AllocateFramebuffer(rmode)));
	frameBuffer[1] := MEM_K0_TO_K1(integer(SYS_AllocateFramebuffer(rmode)));

	VIDEO_Configure(rmode);
	VIDEO_SetNextFramebuffer(frameBuffer[fb]);
	VIDEO_SetBlack(false);
	VIDEO_Flush();
	VIDEO_WaitVSync();
	if (rmode^.viTVMode and VI_NON_INTERLACE) <> 0 then 
    VIDEO_WaitVSync();

	// setup the fifo and then init the flipper
	gp_fifo := memalign(32, DEFAULT_FIFO_SIZE);
	memset(gp_fifo, 0, DEFAULT_FIFO_SIZE);
 
	GX_Init(gp_fifo, DEFAULT_FIFO_SIZE);
 
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
	
	if rmode^.viHeight  = 2*rmode^.xfbHeight then
	  GX_SetFieldMode(rmode^.field_rendering,GX_ENABLE)
	else
    GX_SetFieldMode(rmode^.field_rendering,GX_DISABLE);
 
	GX_SetCullMode(GX_CULL_NONE);
	GX_CopyDisp(frameBuffer[fb],GX_TRUE);
	GX_SetDispCopyGamma(GX_GM_1_0);
 
	// setup our camera at the origin
	// looking down the -z axis with y up
	guLookAt(view, @cam, @up, @look);
 

	// setup our projection matrix
	// this creates a perspective matrix with a view angle of 90,
	// and aspect ratio based on the display resolution
  w := rmode^.viWidth;
  h := rmode^.viHeight;
	guPerspective(perspective, 45, f32(w / h), 0.1, 300.0);
	GX_LoadProjectionMtx(perspective, GX_PERSPECTIVE);
 
	while true do
	begin

		WPAD_ScanPads();

		if (WPAD_ButtonsDown(0) and WPAD_BUTTON_HOME) <> 0 then 
      exit;

		// do this before drawing
		GX_SetViewport(0,0,rmode^.fbWidth,rmode^.efbHeight,0,1);


		// do this stuff after drawing
		GX_DrawDone();
		
		fb := fb xor 1;		// flip framebuffer
    GX_SetZMode(GX_TRUE, GX_LEQUAL, GX_TRUE);
		GX_SetColorUpdate(GX_TRUE);
		GX_CopyDisp(frameBuffer[fb],GX_TRUE);

		VIDEO_SetNextFramebuffer(frameBuffer[fb]);
 
		VIDEO_Flush();
 
		VIDEO_WaitVSync();


	end;

end. 
