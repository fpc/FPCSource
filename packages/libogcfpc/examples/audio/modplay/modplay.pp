program modplay;

{$J+}
{$macro on}
{$mode objfpc}
{$goto on}

uses
  cmem, ctypes, aesndlib, gcmodplay, gccore;

// include generated header
{$include inc/technique.mod.inc}
{$L build/technique.mod.o}

var
  xfb: pcuint32 = nil;
  rmode: PGXRModeObj = nil;
  mplay: TMODPlay;

begin
	// Initialise the video system
	VIDEO_Init();
	
	// Initialise the attached controllers
	WPAD_Init();
	
	// Initialise the audio subsystem
	AESND_Init();

	// Obtain the preferred video mode from the system
	// This will correspond to the settings in the Wii menu
	rmode := VIDEO_GetPreferredMode(nil);

	// Allocate memory for the display in the uncached region
	xfb := SYS_AllocateFramebuffer(rmode);
	
	// Initialise the console, required for printf
	console_init(xfb,20,20,rmode^.fbWidth,rmode^.xfbHeight,rmode^.fbWidth*VI_DISPLAY_PIX_SZ);
	
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
	if (rmode^.viTVMode and VI_NON_INTERLACE) <> 0 then VIDEO_WaitVSync();


	// The console understands VT terminal escape codes
	// This positions the cursor on row 2, column 0
	// we can use variables for this with format codes too
	// e.g. printf ("\x1b[%d;%dH", row, column );
	printf(#$1b'[2;0H');
	

	printf('Hello World!');
	
	MODPlay_Init(@mplay);
	MODPlay_SetMOD(@mplay,@technique_mod);
	MODPlay_SetVolume(@mplay,63,63);
	MODPlay_Start(@mplay);

	while true do
	begin

		// Call WPAD_ScanPads each loop, this reads the latest controller states
		WPAD_ScanPads();

		// WPAD_ButtonsDown tells us which buttons were pressed in this loop
		// this is a "one shot" state which will not fire again until the button has been released
	

		// We return to the launcher application via exit
		if ( WPAD_ButtonsDown(0) and WPAD_BUTTON_HOME ) <> 0 then exit;

		// Wait for the next frame
		VIDEO_WaitVSync();
	end;

end.
