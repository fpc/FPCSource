program gdbstub;

{$J+}
{$macro on}
{$mode objfpc}

uses
  cmem, ctypes, gctypes, debug, gccore;

var
  xfb: pcuint32 = nil;
  rmode: PGXRModeObj = nil;

begin
	VIDEO_Init();
	WPAD_Init();
	
	rmode := VIDEO_GetPreferredMode(nil);

	xfb := SYS_AllocateFramebuffer(rmode);
	console_init(xfb,20,20,rmode^.fbWidth,rmode^.xfbHeight,rmode^.fbWidth*VI_DISPLAY_PIX_SZ);
		
	VIDEO_Configure(rmode);
	VIDEO_SetNextFramebuffer(xfb);
	VIDEO_SetBlack(FALSE);
	VIDEO_Flush();
	VIDEO_WaitVSync();
	if(rmode^.viTVMode and VI_NON_INTERLACE) <> 0 then VIDEO_WaitVSync();

	// Configure for use with USB on EXI channel 1 (memcard slot B) 
	// Other option: GDBSTUB_DEVICE_TCP. Note: second parameter acts as port for this type of device 
	DEBUG_Init(GDBSTUB_DEVICE_USB,1);


	printf('Waiting for debugger ...'#10);
	// This function call enters the debug stub for the first time 
  // It's needed to call this if one wants to start debugging. 
	_break();

	printf('debugger connected ...'#10);
     
	while true do
	begin

		VIDEO_WaitVSync();
		WPAD_ScanPads();

		if (WPAD_ButtonsDown(0) and WPAD_BUTTON_A) <> 0 then 
			printf('Button A pressed.'#10);

		if (WPAD_ButtonsDown(0) and WPAD_BUTTON_HOME) <> 0 then break;
	end;

end.