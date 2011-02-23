program directory;

{$J+}
{$macro on}
{$mode objfpc}
{$goto on}

uses
  cmem, ctypes, fat, gccore;

label err;

var
  xfb: pcuint32 = nil;
  rmode: PGXRModeObj = nil;
  MyDir: PDIR;
	pent: pdirent;
	statbuf: stat;
           
begin
	// Initialise the video system
	VIDEO_Init();
	
	// This function initialises the attached controllers
	WPAD_Init();
	
	// Obtain the preferred video mode from the system
	// This will correspond to the settings in the Wii menu
	rmode := VIDEO_GetPreferredMode(nil);

	// Allocate memory for the display in the uncached region
	xfb := (SYS_AllocateFramebuffer(rmode));
	
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
	
	if not fatInitDefault() then
  begin
		printf('fatInitDefault failure: terminating'#10);
		goto err;
	end;
	

	MyDir := opendir('/');

	if MyDir = nil then
	begin
	  printf('opendir() failure; terminating'#10);
		goto err;
	end;
  pent := readdir(MyDir);

	while pent <> nil do
	begin
	    _stat(pent^.d_name, statbuf);
      if (strcmp('.', pent^.d_name) = 0) or (strcmp('..', pent^.d_name) = 0) then
	        continue;
	    if (S_ISDIR(statbuf.st_mode)) then
	        printf('%s <dir>'#10, pent^.d_name);
	    if (not (S_ISDIR(statbuf.st_mode))) then
	        printf('%s %lld'#10, pent^.d_name, statbuf.st_size);
	end;
	closedir(MyDir);

err:
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