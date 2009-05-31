program AudioModes;
{$L build/soundbank.bin.o}

uses
  ctypes, nds9, maxmod9;
  
(***********************************************
 * this example demonstrates the 3 audio modes
 *
 * functions used:
 *   mmInitDefaultMem(soundbank)
 *     Initialize with default settings
 *
 *   mmLoad( module )
 *     Loads a module to be played
 *
 *   mmStart( module )
 *     Starts playback of a module
 *
 *   mmStop()
 *     Stops module playback
 *
 *   mmSelectMode( mode )
 *     Selects the audio mode
 *     modes:
 *       0: MM_MODE_A, hardware audio mode
 *       1: MM_MODE_B, interpolated audio mode
 *       2: MM_MODE_C, extended audio mode
 *********************************************************)
 

var
  soundbank_bin_end: array [0..0] of cuint8; cvar; external;
  soundbank_bin: array [0..0] of cuint8; cvar; external;
  soundbank_bin_size: cuint32; cvar; external;

const
  MOD_KEYG_SUBTONAL	= 0;
  MOD_PURPLE_MOTION_INSPIRATION	= 1;
  MOD_REZ_MONDAY	= 2;
  MSL_NSONGS	= 3;
  MSL_NSAMPS	= 65;
  MSL_BANKSIZE	= 68;



//---------------------------------------------
  SHOW_TEXT = #10 +

'   Maxmod Audio Modes Example'#10#10 +

' Song: '#10 +
' Mode: '#10#10 +

' Left/Right: Select Song'#10 +
' Up/Down: Change Audio Mode'#10 +
' A: Start Playback'#10 +
' B: Stop Playback'#10#10 +

' Tip: Play subtonal with the'#10 +
' extended mode or else it won''t'#10 +
' sound right.'#10#10 +

' Another Tip: The interpolated'#10 +
' mode doesn''t work in current'#10 +
' emulators.';
//---------------------------------------------




var
  // song order
  song_order: array [0..2] of cshort = ( MOD_KEYG_SUBTONAL, MOD_REZ_MONDAY, MOD_PURPLE_MOTION_INSPIRATION );

  // rez-monday.mod is a bit loud, so we will lower the volume to 500 (normal volume is 1024)
  song_volumes: array [0..2] of cshort = ( 1024, 500, 1024 );

  // strings for the "Song: " display
  song_titles: array [0..2] of pchar = ( 
        'subtonal (30ch)  ',
        'monday (14ch)    ',
        'inspiration (4ch)');

// strings for the "Mode: " display
  audiomode_names: array [0..2] of pchar = (
        'A - Hardware (16ch)    ',
        'B - Interpolated (16ch)',
        'C - Extended (30ch)    ');

  song: integer = 0;
	mode: integer = 0;
  keys: integer;



procedure print_song(song: cint);
begin
	iprintf(#27'[3;7H%s', song_titles[song] );
end;

procedure print_mode(mode: cint);
begin
	iprintf(#27'[4;7H%s', audiomode_names[mode] );
end;


begin	
	//---------------------------------------------------------
	// setup console
	//---------------------------------------------------------
	consoleDemoInit();
	
	// set a dark blue backdrop
	BG_PALETTE_SUB[0] := RGB15( 0, 0, 10 );
	
	//---------------------------------------------------------
	// init maxmod with default settings
	//---------------------------------------------------------
	mmInitDefaultMem(mm_addr(@soundbank_bin));
	
	//---------------------------------------------------------
	// load songs (must be loaded before using with mmStart)
	//---------------------------------------------------------
	mmLoad( MOD_KEYG_SUBTONAL );
	mmLoad( MOD_REZ_MONDAY );
	mmLoad( MOD_PURPLE_MOTION_INSPIRATION );
	
	
	//---------------------------------------------------------
	// display screen info
	//---------------------------------------------------------
	iprintf( SHOW_TEXT );
	print_song( song );
	print_mode( mode );
	
	//---------------------------------------------------------
	// main loop
	//---------------------------------------------------------
	while true do
	begin
		//-----------------------------------------------------
		// get new keypad input
		//-----------------------------------------------------
		scanKeys();
		keys := keysDown();
		
		//-----------------------------------------------------
		// LEFT: select previous song
		//-----------------------------------------------------
		if( keys and KEY_LEFT ) <> 0 then
		begin
			dec(song);
			if( song < 0 ) then song := 2;
			print_song( song );
		end;
		
		//-----------------------------------------------------
		// RIGHT: select next song
		//-----------------------------------------------------
		if( keys and KEY_RIGHT ) <> 0 then
		begin
			inc(song);
			if( song > 2 ) then song := 0;
			print_song( song );
		end;
		
		//-----------------------------------------------------
		// A: start song
		//-----------------------------------------------------
		if( keys and KEY_A ) <> 0 then
		begin
			mmSetModuleVolume( song_volumes[song] );
			
			// loop module until stopped with B keypress
			mmStart( song_order[song], MM_PLAY_LOOP );
		end;
		
		//-----------------------------------------------------
		// B: stop song
		//-----------------------------------------------------
		if( keys and KEY_B ) <> 0 then
		begin
			mmStop();
		end;
	
		//-----------------------------------------------------
		// UP: next audio mode
		//-----------------------------------------------------
		if( keys and KEY_UP ) <> 0 then
		begin
			inc(mode);
			if( mode > 2 ) then mode := 0;
			print_mode( mode );
			
			// switch audio mode
			mmSelectMode( mode );
		end;
		
		//-----------------------------------------------------
		// DOWN: previous audio mode
		//-----------------------------------------------------
		if( keys and KEY_DOWN )<> 0 then
		begin
			dec(mode);
			if( mode < 0 ) then mode := 2;
			print_mode( mode );
			
			// switch audio mode
			mmSelectMode( mode );
		end;
		
		//-----------------------------------------------------
		// wait until next frame
		//-----------------------------------------------------
		swiWaitForVBlank();
	end;
	
end.
