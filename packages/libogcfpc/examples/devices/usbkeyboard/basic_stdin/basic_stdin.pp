program basic_stdin;

{$J+}
{$macro on}
{$mode objfpc}

uses
  cmem, ctypes, gccore;

var
  xfb: pcuint32 = nil;
  rmode: PGXRModeObj = nil;


  quitapp: boolean = false;
  key : cchar;

function putchar(__c: longint): longint; cdecl; external;
function getchar:longint;cdecl;external;

procedure keyPress_cb(sym: cchar); cdecl;
begin

	if (sym > 31 ) then putchar(sym);
	if (sym = 13) then putchar(Ord($10));
	
	if (Ord(sym) = $1b) then quitapp := true;
end;


begin
	// Initialise the video system
	VIDEO_Init();
	
	// This function initialises the attached controllers
	WPAD_Init();
	
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
	if(rmode^.viTVMode and VI_NON_INTERLACE) <> 0 then VIDEO_WaitVSync();


	// The console understands VT terminal escape codes
	// This positions the cursor on row 2, column 0
	// we can use variables for this with format codes too
	// e.g. printf ("\x1b[%d;%dH", row, column );
	printf(#$1b'[2;0HHello World!'#10);
	
	if (KEYBOARD_Init(@keyPress_cb) = 0) then 
    printf('keyboard initialised'#10);

	repeat
		// Call WPAD_ScanPads each loop, this reads the latest controller states
		WPAD_ScanPads();

		// WPAD_ButtonsDown tells us which buttons were pressed in this loop
		// this is a "one shot" state which will not fire again until the button has been released

		key := getchar();

		// We return to the launcher application via exit
		if ( WPAD_ButtonsDown(0) and WPAD_BUTTON_HOME ) <> 0 then quitapp := true;

		// Wait for the next frame
		VIDEO_WaitVSync();
	until quitapp;

end.
