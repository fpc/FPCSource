program template;
{$J+}
{$macro on}
{$mode objfpc}

uses
  ctypes, gccore;

type
  _gx_rmodeobj2 = packed record
    viTVMode : cuint32;
    fbWidth : cuint16;
    efbHeight : cuint16;
    xfbHeight : cuint16;
    viXOrigin : cuint16;
    viYOrigin : cuint16;
    viWidth : cuint16;
    viHeight : cuint16;
    xfbMode : cuint32;
    field_rendering : cuint8;
    aa : cuint8;
    sample_pattern : array [0..11, 0..1] of cuint8;
    vfilter : array [0..6] of cuint8;
  end;

var
  xfb: pcuint32;
  rmode: PGXRModeObj = nil;
  pressed: cuint32;
  
begin
	// Initialise the video system
	VIDEO_Init();
	
	// This function initialises the attached controllers
	WPAD_Init();
	
	// Obtain the preferred video mode from the system
	// This will correspond to the settings in the Wii menu
	rmode := VIDEO_GetPreferredMode(nil);

	// Allocate memory for the display in the uncached region
	xfb := MEM_K0_TO_K1(integer(SYS_AllocateFramebuffer(rmode)));

	// Initialise the console, required for printf
	//console_init(xfb,
  console_init(xfb,
               20, 20,
               rmode^.fbWidth, rmode^.xfbHeight,
               rmode^.fbWidth * VI_DISPLAY_PIX_SZ);
	
	// Set up the video registers with the chosen mode
	VIDEO_Configure(rmode);
	
	// Tell the video hardware where our display memory is
	VIDEO_SetNextFramebuffer(xfb);
	
	// Make the display visible
	VIDEO_SetBlack(FALSE);

	// Flush the video register changes to the hardware
	VIDEO_Flush();

	// Wait for Video setup to complete
	VIDEO_WaitVSync();
	if (rmode^.viTVMode and VI_NON_INTERLACE)<> 0 then
    VIDEO_WaitVSync();


	// The console understands VT terminal escape codes
	// This positions the cursor on row 2, column 0
	// we can use variables for this with format codes too
	// e.g. printf ("\x1b[%d;%dH", row, column );
	printf(#27'[2;0H');
	

	printf('Hello World!'#10);
	iprintf('%d', sizeof(twgpipe));

	while true do
  begin
		// Call WPAD_ScanPads each loop, this reads the latest controller states
		WPAD_ScanPads();

		// WPAD_ButtonsDown tells us which buttons were pressed in this loop
		// this is a "one shot" state which will not fire again until the button has been released
		pressed := WPAD_ButtonsDown(0);

		// We return to the launcher application via exit
		if ( pressed and WPAD_BUTTON_HOME ) <> 0 then
      exit;

		// Wait for the next frame
		VIDEO_WaitVSync();
	end;

end.
