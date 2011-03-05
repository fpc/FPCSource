program triangle;

{$J+}
{$macro on}
{$mode objfpc}
{$inline on}

uses
  cmem, ctypes, gctypes, gccore;

var
  framebuffer: pcuint32;
  screenMode: PGXRModeObj = nil;
  readyForCopy: cuint8;

const
  FIFO_SIZE = (256*1024);

var
  vertices: array[0..2, 0..2] of cint16 =((0, 15, 0),(-15, -15, 0),(15,	-15, 0));
  colors: array[0..2, 0..3] of cuint8 = ((255, 0,	0, 255),(0, 255,	0, 255),(0, 0, 255, 255));

procedure update_screen(viewMatrix: Mtx);
var
	modelView: Mtx;
begin
	guMtxIdentity(modelView);
	guMtxTransApply(modelView, modelView, 0.0,	0.0, -50.0);
	guMtxConcat(viewMatrix,modelView,modelView);

	GX_LoadPosMtxImm(modelView,	GX_PNMTX0);

	GX_Begin(GX_TRIANGLES, GX_VTXFMT0, 3);

	GX_Position1x8(0);
	GX_Color1x8(0);
	GX_Position1x8(1);
	GX_Color1x8(1);
	GX_Position1x8(2);
	GX_Color1x8(2);

	GX_End();

	GX_DrawDone();
	readyForCopy := GX_TRUE;

	VIDEO_WaitVSync();
end;


procedure copy_buffers(count: cuint32); cdecl;
begin
	if (readyForCopy = GX_TRUE) then
  begin
		GX_SetZMode(GX_TRUE, GX_LEQUAL,	GX_TRUE);
		GX_SetColorUpdate(GX_TRUE);
		GX_CopyDisp(frameBuffer,GX_TRUE);
		GX_Flush();
		readyForCopy := GX_FALSE;
	end;
end;

var
  view: Mtx;
	projection: Mtx44;
	backgroundColor: GXColor = (r:0; g:0; b:0; a:255;);
	fifoBuffer: pcuint32 = nil;

  camera, up, look: guVector;


begin
	VIDEO_Init();		
	WPAD_Init();
	
	screenMode := VIDEO_GetPreferredMode(nil);

	frameBuffer	:= MEM_K0_TO_K1(integer(SYS_AllocateFramebuffer(screenMode)));

	VIDEO_Configure(screenMode);
	VIDEO_SetNextFramebuffer(frameBuffer);
	VIDEO_SetPostRetraceCallback(@copy_buffers);
	VIDEO_SetBlack(FALSE);
	VIDEO_Flush();



	fifoBuffer := MEM_K0_TO_K1(integer(memalign(32,FIFO_SIZE)));
	memset(fifoBuffer,	0, FIFO_SIZE);
	
	GX_Init(fifoBuffer, FIFO_SIZE);
	GX_SetCopyClear(backgroundColor, $00ffffff);
	GX_SetViewport(0,0,screenMode^.fbWidth,screenMode^.efbHeight,0,1);
	GX_SetDispCopyYScale(f32(screenMode^.xfbHeight) / f32(screenMode^.efbHeight));
	GX_SetScissor(0,0,screenMode^.fbWidth,screenMode^.efbHeight);
	GX_SetDispCopySrc(0,0,screenMode^.fbWidth,screenMode^.efbHeight);
	GX_SetDispCopyDst(screenMode^.fbWidth,screenMode^.xfbHeight);
	GX_SetCopyFilter(screenMode^.aa, screenMode^.sample_pattern,
					 GX_TRUE, screenMode^.vfilter);

  if screenMode^.viHeight = 2*screenMode^.xfbHeight then
	  GX_SetFieldMode(screenMode^.field_rendering,GX_ENABLE)
	else
	  GX_SetFieldMode(screenMode^.field_rendering,GX_DISABLE);
					 
	GX_SetCullMode(GX_CULL_NONE);
	GX_CopyDisp(frameBuffer,GX_TRUE);
	GX_SetDispCopyGamma(GX_GM_1_0);

  camera.x :=	0.0;
  camera.y := 0.0;
  camera.z := 0.0;
  up.x :=	0.0;
  up.y :=	1.0;
  up.z :=	0.0;
  look.x	:= 0.0;
  look.y	:= 0.0;
  look.z	:= -1.0;

	guPerspective(projection, 60, 1.33, 10.0,	300.0);
	GX_LoadProjectionMtx(projection, GX_PERSPECTIVE);

	GX_ClearVtxDesc();
	GX_SetVtxDesc(GX_VA_POS, GX_INDEX8);
	GX_SetVtxDesc(GX_VA_CLR0, GX_INDEX8);
	GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_POS,	GX_POS_XYZ,	GX_S16,	0);
	GX_SetVtxAttrFmt(GX_VTXFMT0, GX_VA_CLR0, GX_CLR_RGBA, GX_RGBA8,	0);
	GX_SetArray(GX_VA_POS, @vertices, 3*sizeof(cint16));
	GX_SetArray(GX_VA_CLR0,	@colors,	4*sizeof(cuint8));
	GX_SetNumChans(1);
	GX_SetNumTexGens(0);
	GX_SetTevOrder(GX_TEVSTAGE0, GX_TEXCOORDNULL, GX_TEXMAP_NULL, GX_COLOR0A0);
	GX_SetTevOp(GX_TEVSTAGE0, GX_PASSCLR);

	while true do
	begin
		guLookAt(view, @camera,	@up, @look);
		GX_SetViewport(0,0,screenMode^.fbWidth,screenMode^.efbHeight,0,1);
		GX_InvVtxCache();
		GX_InvalidateTexAll();
		update_screen(view);

		WPAD_ScanPads();
		if (WPAD_ButtonsDown(0) and WPAD_BUTTON_HOME) <> 0 then 
      exit;
	end;
end.
